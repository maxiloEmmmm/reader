use core::slice;
use std::{
    borrow::Cow, env::JoinPathsError, fmt::Display, io::{self, ErrorKind, Read, Seek}, ops::Range, string::FromUtf8Error
};

use strum_macros::FromRepr;
use thiserror::Error;

#[repr(u8)]
#[derive(FromRepr, Debug, PartialEq)]
pub enum Delimiter {
    LeftParen = b'(',
    RightParen = b')',
    LeftAngle = b'<',
    RightAngle = b'>',
    LeftBracket = b'[',
    RightBracket = b']',
    LeftBrace = b'{',
    RightBrace = b'}',
    Slash = b'/',
    Percent = b'%',
}

#[repr(u8)]
#[derive(FromRepr, Debug, PartialEq)]
pub enum WhiteSpace {
    NULL = 0,
    Tab = 9,
    LF = 10,
    FF = 12,
    CR = 13,
    Space = 32,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Object {
    Boolean(bool),
    Integer(i64),
    Real(f64),
    String(Vec<u8>),
    Invalid(String),
    Name(Vec<u8>),
    Null,
    Array(Vec<Object>),
    Dictionary(Vec<(Vec<u8>, Object)>), // key 是 Name 的内容，value 是 Object
    Stream(Stream),
    /// 间接对象引用 (object_number, generation_number)
    IndirectRef(i64, i64),
    Object(i64, i64, Box<Object>),
}

impl From<io::Error> for Object {
    fn from(value: io::Error) -> Self {
        Object::io_err(value)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Stream {
    /// 流的原始字节数据
    pub data: Vec<u8>,
    /// (Required) stream 关键字后到 endstream 关键字前的字节数
    pub length: i64,
    /// (Optional) 过滤器名称或过滤器名称数组
    pub filter: Option<Box<Object>>,
    /// (Optional) 过滤器参数字典或字典数组
    pub decode_parms: Option<Box<Object>>,
    /// (Optional; PDF 1.2) 包含流数据的外部文件
    pub f: Option<Box<Object>>,
    /// (Optional; PDF 1.2) 外部文件的过滤器
    pub f_filter: Option<Box<Object>>,
    /// (Optional; PDF 1.2) 外部文件过滤器的参数
    pub f_decode_parms: Option<Box<Object>>,
    /// (Optional; PDF 1.5) 解码后的流字节数（提示值）
    pub dl: Option<i64>,
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("invalid resource {0}")]
    Invalid(String),
    #[error("not found")]
    NotFound,
    #[error("not type {0}, is {1:?}")]
    Type(String, Object),
}


impl Object {
    pub fn unexpected(what: &str, pos: usize) -> Self {
        Self::Invalid(format!("unexpected to see {} at {}", what, pos))
    }

    pub fn incomplete(what: &str) -> Self {
        Self::Invalid(format!("{} incomplete", what))
    }

    pub fn expected(what: &str, pos: usize) -> Self {
        Self::Invalid(format!("expected to see {} at {}", what, pos))
    }

    pub fn expected_obj(what: &str, raw: &Object) -> Self {
        Self::Invalid(format!("waht {}, find {:?}", what, raw))
    }

    pub fn io_err(err: io::Error) -> Self {
        Self::Invalid(format!("io::error {}", err.to_string()))
    }

    pub fn dic_key<'a>(obj: &'a Object, key: &[u8]) -> Result<&'a Object, Error> {
        for (k, v) in Self::must_dic(obj)? {
            if k == key {
                return Ok(v)
            }
        }
        Err(Error::NotFound)
    }
    pub fn must_array(obj: &Object) -> Result<&[Object], Error> {
        match obj {
            Object::Array(obj) => Ok(obj),
            _ => Err(Error::Type("array".to_owned(), obj.to_owned()))
        }
    }
    pub fn must_i64(obj: &Object) -> Result<i64, Error> {
        match obj {
            Object::Integer(obj) => Ok(*obj),
            _ => Err(Error::Type("i64".to_owned(), obj.to_owned()))
        }
    }

    pub fn must_dic(obj: &Object) -> Result<&[(Vec<u8>, Object)], Error> {
        match obj {
            Object::Dictionary(obj) => Ok(obj),
            _ => Err(Error::Type("dic".to_owned(), obj.to_owned()))
        }
    }
    
    pub fn must_obj(obj: &Object) -> Result<&Box<Object>, Error> {
        match obj {
            Object::Object(_, _, obj) => Ok(obj),
            _ => Err(Error::Type("obj".to_owned(), obj.to_owned()))
        }
    }

    pub fn must_ref(obj: &Object) -> Result<i64, Error> {
        match obj {
            Object::IndirectRef(pos, _) => Ok(*pos),
            _ => Err(Error::Type("ref".to_owned(), obj.to_owned()))
        }
    }
    
    pub fn eq_name(obj: &Object, n: &[u8]) -> Result<bool, Error> {
        match obj {
            Object::Name(v) => Ok(v.as_slice() == n),
            _ => Err(Error::Type("name".to_owned(), obj.to_owned()))
        }
    }

    pub fn must_eq_name(obj: &Object, n: &[u8]) -> Result<(), Error> {
        if !Self::eq_name(obj, n)? {
            return Err(Error::Invalid("no eq".to_owned()))
        }
        Ok(())
    }
}

pub trait Source: Read + Seek {}

impl<T: Read + Seek> Source for T {}

pub struct Tokenizer<T: Source> {
    src: T,
    buf: Vec<u8>,
    pos: usize,
    len: usize,
    _seek: u64,
    rev: bool,
    total_len: u64,
    debug: bool
}


impl<T: Source> Seek for Tokenizer<T> {
    fn seek(&mut self, mut pos: std::io::SeekFrom) -> std::io::Result<u64> {
        if self.rev {
            pos = match pos {
                io::SeekFrom::Start(n) => io::SeekFrom::End(-1 * n as i64),
                io::SeekFrom::End(n) => io::SeekFrom::Start((-1 * n) as u64),
                io::SeekFrom::Current(n) => io::SeekFrom::Current(-n),
            };
        }
        match pos {
            io::SeekFrom::Start(n) => {
                if n >= self.start_pos() && n <= self.end_pos() {
                    self.pos = (n as u64 - self.start_pos()) as usize;
                    // 这里不进行底层的 seek 因为这里要服用 所以只要从 正确的位置开始读即可
                    return Ok(self._seek);
                }
            }
            io::SeekFrom::End(_) => {}
            io::SeekFrom::Current(n) => {
                if n > 0 && n < (self.len - self.pos) as i64 {
                    self.pos += n as usize;
                    return Ok(self._seek);
                }
            }
        }
        self.pos = 0;
        self.len = 0;
        self._seek = self.src.seek(pos)?;
        if self.rev {
            self._seek = self.total_len - self._seek;
        }
        Ok(self._seek)
    }
}

// 设计基础操作时，一切输入输出均已文件偏移面相调用者
// 内部pos不对外暴露, 所以对外没有peek这种操作，只有相对于某个偏移的读取
impl<T: Source> Tokenizer<T> {
    pub fn ensure(&mut self, mut n: usize) -> io::Result<bool> {
        let mut remaining = self.len - self.pos;
        if remaining >= n {
            return Ok(true);
        }

        if self.pos > 0 {
            self.buf.copy_within(self.pos..self.len, 0);
            self.len = 0;
            n -= remaining;
            remaining = 0;
            self.pos = 0;
        }

        let mut end = self.buf.capacity();
        if self.rev {
            end = remaining + (self.total_len - self._seek) as usize;
            if end > self.buf.capacity() {
                end = self.buf.capacity();
            }
            // 电表倒转！！
            let left = (self.total_len - self._seek) as usize;
            self._seek = self.total_len
                - self.src.seek(io::SeekFrom::Current(
                    -1 * (if self.buf.capacity() <= left {
                        self.buf.capacity()
                    } else {
                        left
                    }) as i64,
                ))?;
        }
        let num = self.src.read(&mut self.buf[remaining..end])?;
        if !self.rev {
            self._seek += num as u64;
        }
        self.len += num;
        if self.rev {
            self.buf[remaining..self.len].reverse();
        }
        Ok(remaining + num >= n)
    }

    pub fn current_pos(&self) -> usize {
        self.pos
    }

    pub fn current(&self) -> u8 {
        self.buf[self.pos]
    }

    pub fn consume(&mut self) -> io::Result<Option<u8>> {
        if !self.ensure(1)? {
            return Ok(None);
        }
        let n = self.current();
        self.pos += 1;
        Ok(Some(n))
    }

    pub fn back(&mut self, n: usize) -> io::Result<()> {
        if self.pos >= n {
            self.pos -= n;
        } else {
            if self.start_pos() >= n as u64 {
                self.seek(io::SeekFrom::Start(self.start_pos() - n as u64))?;
            } else {
                return Err(io::Error::new(ErrorKind::NotSeekable, "back too more!"));
            }
        }
        Ok(())
    }

    pub fn left_pos(&self) -> u64 {
        self._seek + self.pos as u64 - self.len as u64
    }

    pub fn end_pos(&self) -> u64 {
        self._seek as u64
    }

    pub fn start_pos(&self) -> u64 {
        self._seek - self.len as u64
    }

    pub fn index<F>(&mut self, f: F) -> Result<Range<u64>, io::Error>
    where
        F: Fn(u8) -> bool,
    {
        let start = self.left_pos();
        loop {
            let Some(next) = self.consume()? else {
                return Err(io::Error::new(ErrorKind::UnexpectedEof, "index"));
            };

            if !f(next) {
                self.back(1)?;
                return Ok(start..self.left_pos());
            }
        }
    }

    pub fn read_range(&mut self, r: Range<u64>) -> io::Result<Option<Cow<[u8]>>> {
        let len = r.end - r.start;
        if len <= 0 {
            return Ok(None);
        }
        let start = self.start_pos();
        if r.start >= start {
            self.pos = (r.start - start) as usize;
            return self.read_bytes(len as usize);
        } else {
            self.seek(io::SeekFrom::Current(r.start as i64))?;
        }
        self.read_bytes(len as usize)
    }

    fn in_seek(&self, pos: usize) -> bool {
        pos >= self.start_pos() as usize && pos <= self._seek as usize
    }

    pub fn read_bytes_at(&mut self, pos: usize, n: usize) -> io::Result<Option<Cow<[u8]>>> {
        self.seek(io::SeekFrom::Start(pos as u64))?;
        self.read_bytes(n)
    }

    /// pos: seek
    pub fn read_byte_at(&mut self, pos: usize) -> Result<u8, io::Error> {
        self.seek(io::SeekFrom::Start(pos as u64))?;
        self.ensure(1)?;
        Ok(self.buf[0])
    }

    pub fn read_back(&mut self, n: usize) -> io::Result<&[u8]> {
        let back = self.pos + 1 - n;
        if back >= 0 {
            return Ok(&self.buf[self.pos - n..self.pos]);
        }

        let prev = self.pos;
        self.seek(io::SeekFrom::Current(back as i64))?;
        self.ensure(n)?;
        self.pos = prev;
        Ok(&self.buf[..n])
    }

    pub fn read_byte(&mut self) -> io::Result<Option<u8>> {
        let Some(data) = self.read_bytes(1)? else {
            return Ok(None);
        };
        Ok(Some(data[0]))
    }

    pub fn peek_byte(&mut self) -> io::Result<Option<u8>> {
        let n = self.read_byte()?;
        self.back(1)?;
        Ok(n)
    }

    pub fn read_bytes(&mut self, mut n: usize) -> io::Result<Option<Cow<[u8]>>> {
        let remaining_size = self.len - self.pos;
        if n > self.buf.capacity() {
            let mut ret = Vec::with_capacity(n);
            ret.extend(self.buf[self.pos..self.len].iter());
            n -= remaining_size;
            self.pos = self.len;
            loop {
                let mut tmp = n;
                if tmp > self.buf.capacity() {
                    tmp = self.buf.capacity();
                }
                if !self.ensure(tmp)? {
                    return Ok(None);
                }
                ret.extend(self.buf[..tmp].iter());
                self.pos = tmp;
                n -= tmp;
                if n == 0 {
                    break;
                }
            }
            Ok(Some(Cow::Owned(ret)))
        } else {
            if remaining_size < n {
                if self.pos > 0 {
                    self.buf.copy_within(self.pos..self.len, 0);
                }
                self.len = remaining_size;
                self.pos = 0;
            }
            if !self.ensure(n)? {
                return Ok(None);
            }
            self.pos += n;

            let mut ret = Cow::Borrowed(&self.buf[self.pos - n..self.pos]);
            if self.rev {
                let mut tmp = ret.into_owned();
                tmp.reverse();
                ret = Cow::Owned(tmp);
            }
            Ok(Some(ret))
        }
    }

    pub fn set_rev(&mut self, rev: bool) -> io::Result<()> {
        if self.rev != rev {
            if rev {
                self.total_len = self.src.seek(io::SeekFrom::End(0))?;
            } else {
                self.len = 0;
                self.pos = 0;
                self._seek = self.src.seek(io::SeekFrom::Start(0))?;
            }
        }
        self.rev = rev;
        Ok(())
    }

    pub fn left(&self) -> &[u8] {
        &self.buf[self.pos..self.len]
    }

    pub fn set_debug(&mut self, debug: bool) {
        self.debug = debug;
    }
}

impl<T: Source> Tokenizer<T> {
    pub fn new(src: T) -> Self {
        Self {
            src: src,
            len: 0,
            pos: 0,
            buf: vec![0; 1024 * 8],
            rev: false,
            _seek: 0,
            total_len: 0,
            debug: false,
        }
    }

    pub fn skip_not_whitespace_and_comments(&mut self) -> io::Result<()> {
        loop {
            let Some(next) = self.consume()? else {
                return Ok(());
            };

            if WhiteSpace::from_repr(next).is_some() {
                self.back(1)?;
                return Ok(());
            }

            if next == b'%' {
                self.back(1)?;
                return Ok(());
            }
        }
    }

    pub fn skip_whitespace(&mut self) -> io::Result<()> {
        loop {
            let Some(next) = self.consume()? else {
                return Ok(());
            };
            if !WhiteSpace::from_repr(next).is_some() {
                return self.back(1);
            }
        }
    }

    pub fn skip_whitespace_and_comments(&mut self) -> io::Result<()> {
        let mut in_comment = false;
        loop {
            let Some(next) = self.consume()? else {
                return Ok(());
            };
            match WhiteSpace::from_repr(next) {
                Some(ws) => {
                    if in_comment {
                        match ws {
                            WhiteSpace::CR | WhiteSpace::LF => {
                                in_comment = false;
                            }
                            _ => {}
                        }
                    }
                }
                None => {
                    if let Some(d) = Delimiter::from_repr(next) {
                        if d.eq(&Delimiter::Percent) {
                            in_comment = true
                        }
                    }
                    if !in_comment {
                        self.back(1)?;
                        return Ok(());
                    }
                }
            }
        }
    }

    pub fn parse_boolean(&mut self) -> Object {
        let p = match self.peek_byte() {
            Ok(Some(n)) => n,
            Ok(None) => return Object::incomplete("boolean"),
            Err(e) => return Object::io_err(e),
        };

        match p {
            b'f' => {
                if let Some(v) = match self.read_bytes(5) {
                    Ok(n) => n,
                    Err(e) => return Object::io_err(e),
                } {
                    if v.as_ref() == b"false" {
                        return Object::Boolean(false);
                    }
                    return Object::expected("false", self.pos);
                }
                return Object::incomplete("bool");
            }
            b't' => {
                if let Some(v) = match self.read_bytes(4) {
                    Ok(n) => n,
                    Err(e) => return Object::io_err(e),
                } {
                    if v.as_ref() == b"true" {
                        return Object::Boolean(true);
                    }
                    return Object::expected("true", self.pos);
                }
                return Object::incomplete("true");
            }
            _ => return Object::unexpected(&(p as char).to_string(), self.pos),
        }
    }

    // parse_stream 尝试解析 stream 否则退化为 Dictionary
    pub fn parse_stream(&mut self) -> Object {
        let info = match self.parse_dictionary() {
            Object::Dictionary(v) => v,
            other => return other,
        };
        if let Err(e) = self.skip_whitespace_and_comments() {
            return Object::io_err(e);
        }

        let start = match self.read_bytes(6) {
            Ok(Some(n)) => n,
            Ok(None) => return Object::Dictionary(info),
            Err(e) => return Object::io_err(e),
        };

        if start.as_ref() != b"stream" {
            if let Err(e) = self.back(6) {
                return Object::io_err(e);
            }
            return Object::Dictionary(info);
        }
        let mut length = -1;

        let mut filter = None;
        let mut decode_params = None;
        let mut dl = None;
        let mut f = None;
        let mut ff = None;
        let mut fdp = None;
        for (k, v) in info {
            match k.as_slice() {
                b"Length" => {
                    let Object::Integer(vv) = v else {
                        return Object::expected_obj("int", &v);
                    };
                    length = vv;
                }
                b"Filter" => {
                    filter = Some(Box::new(v.clone()));
                }
                b"DecodeParms" => {
                    decode_params = Some(Box::new(v.clone()));
                }
                b"F" => {
                    f = Some(Box::new(v.clone()));
                }
                b"FFilter" => {
                    ff = Some(Box::new(v.clone()));
                }
                b"FDecodeParms" => {
                    fdp = Some(Box::new(v.clone()));
                }
                b"DL" => match v {
                    Object::Integer(vv) => {
                        dl = Some(vv);
                    }
                    _ => return Object::expected_obj("int", &v),
                },
                _ => {}
            }
        }

        if length < 0 {
            return Object::Invalid(format!("length gt 0 {}", length));
        }

        if let Some(next) = match self.read_byte() {
            Ok(n) => n,
            Err(e) => return Object::io_err(e),
        } {
            match next {
                b'\r' => {
                    if let Some(v) = match self.read_byte() {
                        Ok(n) => n,
                        Err(e) => return Object::io_err(e),
                    } {
                        if v != b'\n' {
                            return Object::Invalid(format!(
                                "after stream not cr+lf is cr+{:x}",
                                v
                            ));
                        }
                    } else {
                        return Object::incomplete("after stream cr+?");
                    }
                }
                b'\n' => {}
                _ => return Object::Invalid(format!("after stream not br {:x}", next)),
            }
        } else {
            return Object::incomplete("stream first br");
        }

        let stream_data = match self.read_bytes(length as usize) {
            Ok(n) => n,
            Err(e) => return Object::io_err(e),
        };
        if let Some(stream_data) = stream_data.map(|v| v.to_vec()) {
            // bad to
            if let Some(vv) = match self.read_byte() {
                Ok(n) => n,
                Err(e) => return Object::io_err(e),
            } {
                match vv {
                    b'\r' => {
                        if let Some(vx) = match self.read_byte() {
                            Ok(n) => n,
                            Err(e) => return Object::io_err(e),
                        } {
                            if vx != b'\n' {
                                return Object::Invalid(format!(
                                    "before endstream not cr+lf is cr+{:x}",
                                    vx
                                ));
                            }
                        }
                    }
                    b'\n' => {}
                    b'e' => {
                        if let Err(e) = self.back(1) {
                            return Object::io_err(e);
                        }
                    }
                    _ => return Object::Invalid(format!("before endstream not br {:x}", vv)),
                }

                if let Some(end_stream) = match self.read_bytes(9) {
                    Ok(n) => n,
                    Err(e) => return Object::io_err(e),
                } {
                    if end_stream.as_ref() == b"endstream" {
                        return Object::Stream(Stream {
                            data: stream_data.to_vec(), // bad to
                            length: length,
                            filter: filter,
                            decode_parms: decode_params,
                            f: f,
                            f_filter: ff,
                            f_decode_parms: fdp,
                            dl: dl,
                        });
                    }
                }
            }
            return Object::incomplete("endstream");
        }

        return Object::incomplete("stream body too small");
    }
    pub fn parse_direct_obj(&mut self) -> Object {
        self.parse_indirect_ref()
    }

    fn direct_parse_indirect_ref(&mut self) -> Object {
        if let Ok(index) = self.index(|v| matches!(v, b'0'..=b'9')) {
            let v1 = match self.read_range(index) {
                Ok(Some(n)) => n.to_vec(),
                Ok(None) => return Object::incomplete("ref-num"),
                Err(e) => return Object::io_err(e),
            };
            if let Some(v) = match self.read_byte() {
                Ok(n) => n,
                Err(e) => return Object::io_err(e),
            } {
                if WhiteSpace::from_repr(v).is_some() {
                    if let Err(e) = self.skip_whitespace_and_comments() {
                        return Object::io_err(e);
                    }
                    if let Ok(two_index) = self.index(|v| matches!(v, b'0'..=b'9')) {
                        let v2 = match self.read_range(two_index) {
                            Ok(Some(n)) => n.to_vec(),
                            Ok(None) => return Object::incomplete("ref-version-num"),
                            Err(e) => return Object::io_err(e),
                        };
                        if let Some(vv) = match self.read_byte() {
                            Ok(n) => n,
                            Err(e) => return Object::io_err(e),
                        } {
                            if WhiteSpace::from_repr(vv).is_some() {
                                if let Err(e) = self.skip_whitespace_and_comments() {
                                    return Object::io_err(e);
                                }
                                if let Some(vv) = match self.read_byte() {
                                    Ok(n) => n,
                                    Err(e) => return Object::io_err(e),
                                } {
                                    match vv {
                                        b'R' => {
                                            if let Ok(v1) =
                                                String::from_utf8_lossy(v1.as_ref()).parse::<i64>()
                                            {
                                                if let Ok(v2) = String::from_utf8_lossy(v2.as_ref())
                                                    .parse::<i64>()
                                                {
                                                    return Object::IndirectRef(v1, v2);
                                                }
                                            }
                                        }

                                        b'o' => {
                                            if let Some(bj) = match self.read_bytes(2) {
                                                Ok(n) => n,
                                                Err(e) => return Object::io_err(e),
                                            } {
                                                if bj.as_ref() == b"bj" {
                                                    if let Some(inner) = self.parse_object() {
                                                        match inner {
                                                            Object::Invalid(n) => {
                                                                if self.debug {
                                                                    println!("obj inner parse failed {}", n);
                                                                }
                                                            }
                                                            _ => {
                                                                if let Err(e) = self
                                                                    .skip_whitespace_and_comments()
                                                                {
                                                                    return Object::io_err(e);
                                                                }
                                                                if let Some(end) = match self
                                                                    .read_bytes(6)
                                                                {
                                                                    Ok(n) => n,
                                                                    Err(e) => {
                                                                        return Object::io_err(e);
                                                                    }
                                                                } {
                                                                    if end.as_ref() == b"endobj" {
                                                                        if let Ok(v1) =
                                                                            String::from_utf8_lossy(
                                                                                v1.as_ref(),
                                                                            )
                                                                            .parse::<i64>()
                                                                        {
                                                                            if let Ok(v2) = String::from_utf8_lossy(v2.as_ref()).parse::<i64>() {
                                                                                return Object::Object(v1, v2, Box::new(inner))
                                                                            }
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        _ => {}
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        Object::incomplete("ref")
    }

    pub fn parse_indirect_ref(&mut self) -> Object {
        let raw = self.left_pos();

        let obj = self.direct_parse_indirect_ref();

        match obj {
            Object::IndirectRef(_, _) => return obj,
            Object::Object(_, _, _) => return obj,
            other => {
                if self.debug {
                    println!("not obj & ref is {:?}", other)
                }
            }
        }
        if let Err(e) = self.seek(io::SeekFrom::Start(raw)) {
            return Object::io_err(e);
        }

        self.inner_parse_number()
    }

    pub fn parse_object(&mut self) -> Option<Object> {
        if let Err(e) = self.skip_whitespace_and_comments() {
            return Some(Object::from(e));
        }
        let next = match self.peek_byte() {
            Ok(Some(n)) => n,
            Ok(None) => return None,
            Err(e) => return Some(Object::io_err(e)),
        };
        match next {
            b'+' | b'-' | b'.' => {
                return Some(self.parse_number());
            }
            b'0'..=b'9' => {
                return Some(self.parse_indirect_ref());
            }
            b'(' => {
                return Some(self.parse_string());
            }
            b'<' => {
                let mut is_stream = false;
                if let Some(n) = match self.read_bytes(2) {
                    Ok(n) => n,
                    Err(e) => return Some(Object::io_err(e)),
                } {
                    if n[1] == b'<' {
                        is_stream = true
                    }
                }
                if let Err(e) = self.back(2) {
                    return Some(Object::io_err(e));
                }
                if is_stream {
                    return Some(self.parse_stream());
                }
                return Some(self.parse_hexadecimal_string());
            }
            b'/' => {
                return Some(self.parse_name());
            }
            b'n' => {
                return Some(self.parse_null());
            }
            b'[' => {
                return Some(self.parse_array());
            }
            b't' | b'f' => {
                return Some(self.parse_boolean());
            }
            _ => {
                return None;
            }
        }
    }

    pub fn parse_number(&mut self) -> Object {
        self.parse_indirect_ref()
    }

    fn inner_parse_number(&mut self) -> Object {
        let mut has_point = false;
        let mut pos = 0;
        let start = self.left_pos();
        loop {
            let b = match self.read_byte() {
                Err(e) => return Object::io_err(e),
                Ok(Some(b)) => b,
                Ok(None) => break,
            };
            match b {
                b'.' => {
                    if has_point {
                        return Object::unexpected(".", self.pos);
                    }
                    has_point = true;
                }
                b'0'..=b'9' => {}
                b'+' => {
                    if pos != 0 {
                        return Object::unexpected("+", self.pos);
                    }
                }
                b'-' => {
                    if pos != 0 {
                        return Object::unexpected("-", self.pos);
                    }
                }
                _ => {
                    if let Err(e) = self.back(1) {
                        return Object::io_err(e);
                    }
                    break;
                }
            }
            pos += 1;
        }
        let data = match self.read_bytes_at(start as usize, pos) {
            Ok(Some(data)) => data,
            Ok(None) => return Object::incomplete("number"),
            Err(e) => return Object::io_err(e),
        };
        let s = match str::from_utf8(data.as_ref()) {
            Ok(s) => s,
            Err(e) => return Object::Invalid(e.to_string()),
        };
        if has_point {
            match s.parse::<f64>() {
                Ok(v) => Object::Real(v),
                Err(e) => Object::Invalid(e.to_string()),
            }
        } else {
            match s.parse::<i64>() {
                Ok(v) => Object::Integer(v),
                Err(e) => Object::Invalid(e.to_string()),
            }
        }
    }

    pub fn parse_16(&mut self) -> io::Result<Option<u8>> {
        let mut ret = 0;
        let mut next = false;
        loop {
            let Some(n) = self.read_byte()? else {
                return Ok(None);
            };
            ret = (ret << 4)
                | match n {
                    b'0'..=b'9' => n - b'0',
                    b'a'..=b'f' => 10 + n - b'a',
                    b'A'..=b'F' => 10 + n - b'A',
                    _ => return Ok(None),
                };

            if next {
                return Ok(Some(ret));
            }
            next = true;
        }
    }

    pub fn parse_string_eight(&mut self) -> io::Result<Option<u8>> {
        let mut pos = 0;
        let mut ret = 0_u16;
        loop {
            let Some(next) = self.read_byte()? else {
                break;
            };
            match next {
                b'0'..=b'7' => {
                    pos += 1;
                    ret = (ret << 3) | (next - '0' as u8) as u16;
                    if pos == 3 {
                        break;
                    }
                }
                _ => {
                    self.back(1)?;
                    break;
                }
            }
        }

        if ret > 255 {
            ret = ret & 0xFF;
        }

        Ok(Some(ret as u8))
    }

    pub fn parse_hexadecimal_string(&mut self) -> Object {
        let mut double = false;
        let mut base = 0_u8;
        let mut ret = Vec::new();
        let mut pos = 0;
        loop {
            let next = match self.read_byte() {
                Ok(Some(n)) => n,
                Ok(None) => return Object::incomplete("hexadecimal string"),
                Err(e) => return Object::io_err(e),
            };

            if pos == 0 && next != b'<' {
                return Object::unexpected("<", self.pos);
            }

            match next {
                b'<' => {
                    if pos != 0 {
                        return Object::unexpected("<", self.pos);
                    }
                }
                b'>' => {
                    if double {
                        ret.push(base << 4);
                    }
                    break;
                }
                b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F' => {
                    let x = match next {
                        b'0'..=b'9' => next - '0' as u8,
                        b'a'..=b'f' => 10 + next - b'a',
                        b'A'..=b'F' => 10 + next - b'A',
                        _ => 0,
                    };

                    base = (base << 4) | x;

                    double = !double;
                    if !double {
                        ret.push(base);
                        base = 0;
                    }
                }
                b' ' | b'\t' | b'\r' | b'\n' | 0x0c => {}
                other => return Object::unexpected(&format!("{}", other), self.pos),
            }
            pos += 1;
        }

        return Object::String(ret);
    }

    pub fn parse_string(&mut self) -> Object {
        let mut pos = 0;
        let mut ret = Vec::new();
        let mut depth = 1;
        loop {
            let next = match self.read_byte() {
                Ok(Some(n)) => n,
                Ok(None) => return Object::incomplete("string"),
                Err(e) => return Object::io_err(e),
            };

            if pos == 0 && next != b'(' {
                return Object::unexpected("(", self.pos);
            }

            match next {
                b'(' => {
                    depth += 1;

                    if pos != 0 {
                        ret.push(next);
                    }
                }
                b')' => {
                    depth -= 1;
                    if depth == 1 {
                        return Object::String(ret);
                    }
                    ret.push(next);
                }
                b'\\' => {
                    if let Some(n) = match self.read_byte() {
                        Ok(n) => n,
                        Err(e) => return Object::io_err(e),
                    } {
                        match n {
                            b'\r' => {
                                if let Some(n) = match self.read_byte() {
                                    Ok(n) => n,
                                    Err(e) => return Object::io_err(e),
                                } {
                                    if n != b'\n' {
                                        if let Err(e) = self.back(1) {
                                            return Object::io_err(e);
                                        }
                                    }
                                }
                            }
                            b'\n' => {}
                            b'n' => ret.push(b'\n'),
                            b'r' => ret.push(b'\r'),
                            b't' => ret.push(b'\t'),
                            b'b' => ret.push(0x08),
                            b'f' => ret.push(0x0c),
                            b'(' => ret.push(b'('),
                            b')' => ret.push(b')'),
                            b'\\' => ret.push(b'\\'),
                            b'0'..=b'7' => {
                                if let Err(e) = self.back(1) {
                                    return Object::io_err(e);
                                }
                                if let Some(v) = match self.parse_string_eight() {
                                    Ok(n) => n,
                                    Err(e) => return Object::io_err(e),
                                } {
                                    ret.push(v);
                                }
                                continue; // 别继续加了 eight 里已经步进了
                            }
                            _ => {
                                if let Err(e) = self.back(1) {
                                    return Object::io_err(e);
                                }
                            }
                        }
                    }
                }
                _ => {
                    ret.push(next);
                }
            }

            pos += 1;
        }
    }

    pub fn parse_name(&mut self) -> Object {
        let mut pos = 0;
        let mut ret = Vec::new();
        loop {
            let next = match self.read_byte() {
                Ok(Some(n)) => n,
                Ok(None) => break,
                Err(e) => return Object::io_err(e),
            };

            if pos == 0 && next != b'/' {
                return Object::unexpected(&(next as char).to_string(), self.pos);
            }

            match next {
                b'/' => {
                    if pos != 0 {
                        if let Err(e) = self.back(1) {
                            return Object::io_err(e);
                        }
                        break;
                    }
                }
                b'#' => {
                    if let Some(v) = match self.parse_16() {
                        Ok(n) => n,
                        Err(e) => return Object::io_err(e),
                    } {
                        ret.push(v);
                        continue;
                    }
                }
                _ => {
                    if Delimiter::from_repr(next).is_some() || WhiteSpace::from_repr(next).is_some()
                    {
                        if let Err(e) = self.back(1) {
                            return Object::io_err(e);
                        }
                        break;
                    }
                    ret.push(next);
                }
            }

            pos += 1;
        }

        return Object::Name(ret);
    }

    pub fn parse_null(&mut self) -> Object {
        if let Some(v) = match self.read_bytes(4) {
            Ok(n) => n,
            Err(e) => return Object::io_err(e),
        } {
            if v.as_ref() == b"null" {
                return Object::Null;
            }
        }
        return Object::Invalid("not null".to_owned());
    }

    pub fn parse_array(&mut self) -> Object {
        let mut ret = Vec::new();
        let mut pos = 0;
        loop {
            let next = match self.read_byte() {
                Ok(Some(n)) => n,
                Ok(None) => return Object::incomplete("array"),
                Err(e) => return Object::io_err(e),
            };

            if pos == 0 {
                if next != b'[' {
                    return Object::unexpected(&(next as char).to_string(), self.pos);
                }
            } else {
                match next {
                    b']' => {
                        return Object::Array(ret);
                    }
                    _ => {
                        if let Err(e) = self.back(1) {
                            return Object::io_err(e);
                        }
                        if let Some(v) = self.parse_object() {
                            if matches!(v, Object::Invalid(_)) {
                                return v;
                            }
                            ret.push(v);
                        }
                        continue;
                    }
                }
            }
            pos += 1;
        }
    }

    pub fn parse_dictionary(&mut self) -> Object {
        let mut ret = Vec::new();

        let pos = self.left_pos();
        let nb = match self.read_bytes(2) {
            Ok(Some(n)) => n,
            Ok(None) => return Object::incomplete("dictionary"),
            Err(e) => return Object::io_err(e),
        };

        if nb.as_ref() != b"<<" {
            return Object::unexpected(&String::from_utf8_lossy(nb.as_ref()), pos as usize);
        }
        loop {
            let next = match self.peek_byte() {
                Ok(Some(n)) => n,
                Ok(None) => return Object::incomplete("dictionary"),
                Err(e) => return Object::io_err(e),
            };

            match next {
                b'>' => {
                    if let Some(end) = match self.read_bytes(2) {
                        Ok(n) => n,
                        Err(e) => return Object::io_err(e),
                    } {
                        if end[1] == b'>' {
                            return Object::Dictionary(ret);
                        }
                        return Object::unexpected(&(end[1] as char).to_string(), self.pos);
                    }
                    return Object::incomplete("dictionary last >");
                }
                _ => {
                    let raw = self.pos;
                    if let Some(obj) = self.parse_object() {
                        match obj {
                            Object::Name(name) => {
                                if let Some(value) = self.parse_object() {
                                    match value {
                                        Object::Invalid(v) => return Object::Invalid(v),
                                        Object::Null => {}
                                        _ => {
                                            ret.push((name, value));
                                        }
                                    }
                                    continue;
                                } else {
                                    return Object::incomplete("no value");
                                }
                            }
                            other => return Object::unexpected(&format!("{:?}", other), raw),
                        }
                    }
                }
            }
        }
    }
}
