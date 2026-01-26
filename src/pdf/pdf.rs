use std::{env::JoinPathsError, string::FromUtf8Error};

use strum_macros::FromRepr;

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
}

pub struct Tokenizer {
    data: Vec<u8>,
    pos: usize,
}

impl Tokenizer {
    pub fn new(data: &[u8]) -> Self {
        Self {
            data: data.to_vec(),
            pos: 0,
        }
    }

    pub fn skip_whitespace_and_comments(&mut self) {
        let mut in_comment = false;
        loop {
            if self.peek_byte().is_none() {
                break;
            }

            match WhiteSpace::from_repr(self.data[self.pos]) {
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
                    if let Some(d) = Delimiter::from_repr(self.data[self.pos]) {
                        if d.eq(&Delimiter::Percent) {
                            in_comment = true
                        }
                    }
                    if !in_comment {
                        return;
                    }
                }
            }
            self.pos += 1;
        }
    }

    pub fn peek_byte(&self) -> Option<u8> {
        self.index_byte(self.pos)
    }

    pub fn peek_next_byte(&self) -> Option<u8> {
        self.index_byte(self.pos + 1)
    }

    pub fn index<F>(&self, f: F) -> Option<usize>
    where
        F: Fn(u8) -> bool {
        let mut raw = self.pos;
        loop {
            if raw >= self.data.len() {
                return None
            }
            if !f(self.data[raw]) {
                if self.pos == raw {
                    return None
                }
                return Some(raw-1)
            }
            raw += 1;
        }
    }

    fn index_byte(&self, pos: usize) -> Option<u8> {
        if pos >= self.data.len() {
            return None;
        }
        Some(self.data[pos])
    }

    pub fn parse_boolean(&mut self) -> Object {
        let Some(p) = self.peek_byte() else {
            return Object::incomplete("boolean");
        };

        match p {
            b'f' => {
                if let Some(v) = self.peek_bytes(5) {
                    if v == b"false" {
                        self.pos += 5;
                        return Object::Boolean(false);
                    }
                    return Object::expected("false", self.pos);
                }
                return Object::incomplete("bool");
            }
            b't' => {
                if let Some(v) = self.peek_bytes(4) {
                    if v == b"true" {
                        self.pos += 4;
                        return Object::Boolean(true);
                    }
                    return Object::expected("true", self.pos);
                }
                return Object::incomplete("true");
            }
            other => return Object::unexpected(&(p as char).to_string(), self.pos),
        }
    }

    // parse_stream 尝试解析 stream 否则退化为 Dictionary
    pub fn parse_stream(&mut self) -> Object {
        let info = match self.parse_dictionary() {
            Object::Dictionary(v) => v,
            other => return other,
        };
        self.skip_whitespace_and_comments();

        let Some(start) = self.peek_bytes(6) else {
            return Object::Dictionary(info);
        };

        if start != b"stream" {
            return Object::Dictionary(info);
        }

        self.pos += 6;

        let mut length = -1;

        let mut filter = None;
        let mut decodeParams = None;
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
                    decodeParams = Some(Box::new(v.clone()));
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
                b"DL" => {
                    match v {
                        Object::Integer(vv) => {
                            dl = Some(vv);
                        }
                        _ => return Object::expected_obj("int", &v)
                    }
                }
                _ => {}
            }
        }

        if length < 0 {
            return Object::Invalid(format!("length gt 0 {}", length));
        }

        if let Some(next) = self.peek_byte() {
            match next {
                b'\r' => {
                    if let Some(v) = self.peek_next_byte() {
                        if v != b'\n' {
                            return Object::Invalid(format!("after stream not cr+lf is cr+{:x}", v));
                        }
                        self.pos += 2;
                    } else {
                        return Object::incomplete("after stream cr+?");
                    }
                }
                b'\n' => {
                    self.pos += 1;
                }
                _ => return Object::Invalid(format!("after stream not br {:x}", next)),
            }
        } else {
            return Object::incomplete("stream first br");
        }

        let raw = self.pos;
        let end_index = self.pos + length as usize - 1;
        if self.index_byte(end_index).is_some() {
            self.pos += length as usize;
            if let Some(vv) = self.peek_byte() {
                match vv {
                    b'\r' => {
                        if let Some(vx) = self.peek_next_byte() {
                            if vx != b'\n' {
                                return Object::Invalid(format!(
                                    "before endstream not cr+lf is cr+{:x}",
                                    vx
                                ));
                            }
                            self.pos += 2;
                        }
                    }
                    b'\n' => {
                        self.pos += 1;
                    }
                    b'e' => {}
                    _ => return Object::Invalid(format!("before endstream not br {:x}", vv)),
                }

                if self.index_byte(self.pos + 9 - 1).is_some() {
                    if &self.data[self.pos..self.pos + 9] == b"endstream" {
                        self.pos += 9;
                        return Object::Stream(Stream {
                            data: self.data[raw..=end_index].to_vec(),
                            length: length,
                            filter: filter,
                            decode_parms: decodeParams,
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

    pub fn parse_indirect_ref(&mut self) -> Object {
        let raw = self.pos;
        if let Some(index) = self.index(|v| matches!(v, b'0'..=b'9')) {
            self.pos = index+1;
            if let Some(v) = self.peek_byte() {
                if WhiteSpace::from_repr(v).is_some() {
                   self.skip_whitespace_and_comments();
                   if let Some(two_index) = self.index(|v| matches!(v, b'0'..=b'9')) {
                       let two_raw = self.pos;
                       self.pos = two_index+1;
                       self.skip_whitespace_and_comments();
                       if let Some(vv) = self.peek_byte() {
                           match vv {
                               b'R' => {
                                   if let Ok(v1) = String::from_utf8_lossy(&self.data[raw..=index]).parse::<i64>() {
                                       if let Ok(v2) = String::from_utf8_lossy(&self.data[two_raw..=two_index]).parse::<i64>() {
                                           self.pos += 1;
                                           return Object::IndirectRef(v1, v2)
                                       }
                                   }
                               }

                               b'o' => {
                                   self.pos += 1;
                                   if let Some(bj) = self.peek_bytes(2) {
                                       if bj == b"bj" {
                                           self.pos += 2;
                                           if let Some(inner) = self.parse_object() {
                                               self.skip_whitespace_and_comments();
                                               if let Some(end) = self.peek_bytes(6) {
                                                   if end == b"endobj" {
                                                        if let Ok(v1) = String::from_utf8_lossy(&self.data[raw..=index]).parse::<i64>() {
                                                            if let Ok(v2) = String::from_utf8_lossy(&self.data[two_raw..=two_index]).parse::<i64>() {
                                                                self.pos += 6;
                                                                return Object::Object(v1, v2, Box::new(inner))
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
        self.pos = raw;
        self.inner_parse_number()       
    }
    
    pub fn parse_object(&mut self) -> Option<Object> {
        self.skip_whitespace_and_comments();
        let Some(next) = self.peek_byte() else {
            return None;
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
                if let Some(n) = self.peek_next_byte() {
                    if n == b'<' {
                        return Some(self.parse_stream());
                    }
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
        loop {
            let Some(b) = self.peek_byte() else {
                break;
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
                    break;
                }
            }
            self.pos += 1;
            pos += 1;
        }

        let s = match str::from_utf8(self.peek_back(pos)) {
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

    pub fn parse_16(&mut self) -> Option<u8> {
        let mut ret = 0;
        let mut next = false;
        loop {
            let Some(n) = self.peek_byte() else {
                return None;
            };
            ret = (ret << 4)
                | match n {
                    b'0'..=b'9' => n - b'0',
                    b'a'..=b'f' => 10 + n - b'a',
                    b'A'..=b'F' => 10 + n - b'A',
                    _ => return None,
                };
            self.pos += 1;

            if next {
                return Some(ret);
            }
            next = true;
        }
    }

    pub fn parse_string_eight(&mut self) -> Option<u8> {
        let mut pos = 0;
        let mut ret = 0_u16;
        loop {
            let Some(next) = self.peek_byte() else {
                break;
            };
            match next {
                b'0'..=b'7' => {
                    pos += 1;
                    ret = (ret << 3) | (next - '0' as u8) as u16;
                    if pos == 3 {
                        self.pos += 1;
                        break;
                    }
                }
                _ => {
                    break;
                }
            }
            self.pos += 1;
        }

        if ret > 255 {
            ret = ret & 0xFF;
        }

        Some(ret as u8)
    }

    pub fn parse_hexadecimal_string(&mut self) -> Object {
        let mut double = false;
        let mut base = 0_u8;
        let mut ret = Vec::new();
        let mut pos = 0;
        loop {
            let Some(next) = self.peek_byte() else {
                return Object::incomplete("hexadecimal string");
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
                    self.pos += 1;
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
            self.pos += 1;
        }

        return Object::String(ret);
    }

    pub fn parse_string(&mut self) -> Object {
        let mut pos = 0;
        let mut ret = Vec::new();
        let mut depth = 1;
        loop {
            let Some(next) = self.peek_byte() else {
                return Object::incomplete("string");
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
                        self.pos += 1;
                        return Object::String(ret);
                    }
                    ret.push(next);
                }
                b'\\' => {
                    self.pos += 1;
                    if let Some(n) = self.peek_byte() {
                        match n {
                            b'\r' => {
                                if let Some(n) = self.peek_next_byte() {
                                    if n == b'\n' {
                                        self.pos += 1;
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
                                if let Some(v) = self.parse_string_eight() {
                                    ret.push(v);
                                }
                                continue; // 别继续加了 eight 里已经步进了
                            }
                            _ => {
                                self.pos -= 1; //其他情况\没意义 忽略
                            }
                        }
                    }
                }
                _ => {
                    ret.push(next);
                }
            }

            pos += 1;
            self.pos += 1;
        }
    }

    pub fn current(&self) -> u8 {
        self.data[self.pos]
    }

    pub fn peek_back(&self, n: usize) -> &[u8] {
        &self.data[self.pos - n..self.pos]
    }

    pub fn peek_range(&self, start: usize, end: usize) -> &[u8] {
        &self.data[start..end]
    }

    pub fn peek_bytes(&self, n: usize) -> Option<&[u8]> {
        if self.index_byte(self.pos + n - 1).is_some() {
            return Some(&self.data[self.pos..self.pos + n]);
        }
        None
    }

    pub fn parse_name(&mut self) -> Object {
        let mut pos = 0;
        let mut ret = Vec::new();
        loop {
            let Some(next) = self.peek_byte() else {
                break;
            };

            if pos == 0 && next != b'/' {
                return Object::unexpected(&(next as char).to_string(), self.pos);
            }

            match next {
                b'/' => {
                    if pos != 0 {
                        break;
                    }
                }
                b'#' => {
                    self.pos += 1;
                    if let Some(v) = self.parse_16() {
                        ret.push(v);
                        continue;
                    }
                }
                _ => {
                    if Delimiter::from_repr(next).is_some() || WhiteSpace::from_repr(next).is_some()
                    {
                        break;
                    }
                    ret.push(next);
                }
            }

            pos += 1;
            self.pos += 1;
        }

        return Object::Name(ret);
    }

    pub fn parse_null(&mut self) -> Object {
        if let Some(v) = self.peek_bytes(4) {
            if v.eq("null".as_bytes()) {
                self.pos += 4;
                return Object::Null;
            }
        }
        return Object::Invalid("not null".to_owned());
    }

    pub fn parse_array(&mut self) -> Object {
        let mut ret = Vec::new();
        let mut pos = 0;
        loop {
            let Some(next) = self.peek_byte() else {
                return Object::incomplete("array");
            };

            if pos == 0 {
                if next != b'[' {
                    return Object::unexpected(&(next as char).to_string(), self.pos);
                }
            } else {
                match next {
                    b']' => {
                        self.pos += 1;
                        return Object::Array(ret);
                    }
                    _ => {
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
            self.pos += 1;
        }
    }

    pub fn parse_dictionary(&mut self) -> Object {
        let mut ret = Vec::new();

        let Some(nb) = self.peek_bytes(2) else {
            return Object::incomplete("dictionary");
        };

        if nb != b"<<" {
            return Object::unexpected(&String::from_utf8_lossy(nb), self.pos);
        }
        self.pos += 2;
        loop {
            let Some(next) = self.peek_byte() else {
                return Object::incomplete("dictionary");
            };

            match next {
                b'>' => {
                    if let Some(end) = self.peek_next_byte() {
                        if end == b'>' {
                            self.pos += 2;
                            return Object::Dictionary(ret);
                        }
                        return Object::unexpected(&(end as char).to_string(), self.pos);
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn after_comment() {
        let mut tokenizer = Tokenizer::new(b"% comment\nabc");
        tokenizer.skip_whitespace_and_comments();
        assert_eq!(tokenizer.peek_byte(), Some(b'a'));
    }

    #[test]
    fn comment_at_eof_without_newline() {
        let mut tokenizer = Tokenizer::new(b"% comment");
        tokenizer.skip_whitespace_and_comments();
        assert_eq!(tokenizer.peek_byte(), None);
    }

    #[test]
    fn mixed_whitespace_and_comment() {
        let mut tokenizer = Tokenizer::new(b"  \t % skip me\n \r\nabc");
        tokenizer.skip_whitespace_and_comments();
        assert_eq!(tokenizer.peek_byte(), Some(b'a'));
    }

    #[test]
    fn comment_ends_with_cr() {
        let mut tokenizer = Tokenizer::new(b"% comment\rX");
        tokenizer.skip_whitespace_and_comments();
        assert_eq!(tokenizer.peek_byte(), Some(b'X'));
    }

    #[test]
    fn comment_ends_with_crlf() {
        let mut tokenizer = Tokenizer::new(b"% comment\r\nY");
        tokenizer.skip_whitespace_and_comments();
        assert_eq!(tokenizer.peek_byte(), Some(b'Y'));
    }

    #[test]
    fn parse_valid_integers() {
        let cases: &[(&[u8], Object)] = &[
            (b"123", Object::Integer(123)),
            (b"0", Object::Integer(0)),
            (b"-98", Object::Integer(-98)),
            (b"+17", Object::Integer(17)),
            (b"2147483647", Object::Integer(2147483647)),
            (b"-2147483648", Object::Integer(-2147483648)),
            (b"9223372036854775807", Object::Integer(9223372036854775807)),
        ];

        for (input, expected) in cases {
            let mut tokenizer = Tokenizer::new(input);
            let result = tokenizer.parse_number();

            assert_eq!(&result, expected);
            println!("{}", str::from_utf8(input).unwrap_or(""));
        }
    }

    #[test]
    fn parse_valid_reals() {
        use std::f64;

        let cases: &[(&[u8], f64)] = &[
            (b"34.5", 34.5f64),
            (b"-3.62", -3.62),
            (b"+123.6", 123.6),
            (b"4.", 4.0),
            (b".002", 0.002),
            (b"-.002", -0.002),
            (b".5", 0.5),
            (b"0.0", 0.0),
            (b"1.0", 1.0),
            (b"-0.0", -0.0),
        ];

        for (input, expected_val) in cases {
            let mut tokenizer = Tokenizer::new(input);
            let result = tokenizer.parse_number();
            if let Object::Real(val) = result {
                assert!((val - expected_val).abs() < f64::EPSILON * 10.0);
            } else {
                panic!("Expected Real");
            }
        }
    }

    #[test]
    fn parse_edge_cases() {
        assert_eq!(Tokenizer::new(b"5").parse_number(), Object::Integer(5));
        assert_eq!(Tokenizer::new(b"0").parse_number(), Object::Integer(0));
        assert_eq!(Tokenizer::new(b"0.").parse_number(), Object::Real(0.0));
        assert_eq!(Tokenizer::new(b".0").parse_number(), Object::Real(0.0));
    }

    #[test]
    fn parse_invalid_numbers() {
        // 真正无效的情况：无法解析出任何数字
        let truly_invalid: &[&[u8]] = &[
            b"",    // 空输入
            b".",   // 只有点，没有数字
            b"+",   // 只有符号，没有数字
            b"-",   // 只有符号，没有数字
            b"a12", // 开头是字母，不是数字
            b"abc", // 纯字母
        ];

        for &input in truly_invalid {
            let mut tokenizer = Tokenizer::new(input);
            let result = tokenizer.parse_number();
            match result {
                Object::Invalid(_) => {}
                other => {
                    panic!(
                        "Expected invalid for {:?}, got {:?}",
                        String::from_utf8_lossy(input),
                        other
                    )
                }
            }
        }
    }

    #[test]
    fn parse_numbers_with_trailing_chars() {
        // 只识别 0-9 . + - 这些字符，遇到其他字符时停止解析
        let cases: &[(&[u8], Object)] = &[
            // 数字后跟字母 -> 停止
            (b"12a", Object::Integer(12)),
            (b"3.14abc", Object::Real(3.14)),
            (b"1e5", Object::Integer(1)),     // e 不是数字字符，停止
            (b"6.02E23", Object::Real(6.02)), // E 不是数字字符，停止
            // 数字后跟空白 -> 停止
            (b"42 abc", Object::Integer(42)),
            (b"3.14 xyz", Object::Real(3.14)),
            // 数字后跟 delimiter -> 停止
            (b"123[", Object::Integer(123)),
            (b"456(", Object::Integer(456)),
            (b"7.89<", Object::Real(7.89)),
            // 数字后跟下划线 -> 停止
            (b"1_000", Object::Integer(1)),
        ];

        for &(input, ref expected) in cases {
            let mut tokenizer = Tokenizer::new(input);
            let result = tokenizer.parse_number();
            assert_eq!(
                &result,
                expected,
                "Input: {:?} Expected: {:?} Got: {:?}",
                String::from_utf8_lossy(input),
                expected,
                result
            );
        }
    }

    #[test]
    fn parse_invalid_number_sequences() {
        // 只识别 0-9 . + - 但组合不合法的情况
        let invalid: &[&[u8]] = &[
            b"++1",   // 两个加号
            b"--5",   // 两个减号
            b"1-2",   // 中间有减号（- 只能在开头）
            b"1+2",   // 中间有加号（+ 只能在开头）
            b"1.2.3", // 多个小数点
            b"1..2",  // 连续小数点
            b".1.2",  // 多个小数点
        ];

        for &input in invalid {
            let mut tokenizer = Tokenizer::new(input);
            let result = tokenizer.parse_number();
            match result {
                Object::Invalid(_) => {}
                other => {
                    panic!(
                        "Expected invalid for {:?}, got {:?}",
                        String::from_utf8_lossy(input),
                        other
                    )
                }
            }
        }
    }

    #[test]
    fn parse_literal_strings_pdf17() {
        let cases: &[(&[u8], &[u8])] = &[
            // ========== 基础格式 ==========
            (b"(Hello)", b"Hello"),
            (b"()", b""),
            (b"(simple text)", b"simple text"),
            (b"(123)", b"123"),
            (b"( This is a string )", b" This is a string "),
            // ========== 嵌套括号（平衡的括号对） ==========
            (b"(a(b)c)", b"a(b)c"),
            (b"( ( nested ) )", b" ( nested ) "),
            (b"(a(b(c)d)e)", b"a(b(c)d)e"),
            (
                b"(level1(level2(level3)level2)level1)",
                b"level1(level2(level3)level2)level1",
            ),
            (b"(1(2(3(4)3)2)1)", b"1(2(3(4)3)2)1"),
            (b"(a(b(c(d(e)e)d)c)b)a)", b"a(b(c(d(e)e)d)c)b"),
            (
                b"( Strings may contain balanced parentheses ( ) and special characters )",
                b" Strings may contain balanced parentheses ( ) and special characters ",
            ),
            // ========== 转义序列（Table 3.2） ==========
            // \n → Line feed (LF)
            (b"(a\\nb)", b"a\nb"),
            // \r → Carriage return (CR) - 保持为 \r，不转为 \n
            (b"(a\\rb)", b"a\rb"),
            // \t → Horizontal tab (HT)
            (b"(a\\tb)", b"a\tb"),
            // \b → Backspace (BS)
            (b"(a\\bb)", b"a\x08b"),
            // \f → Form feed (FF)
            (b"(a\\fb)", b"a\x0Cb"),
            // \( → Left parenthesis
            (b"(a\\(b)", b"a(b"),
            // \) → Right parenthesis
            (b"(a\\)b)", b"a)b"),
            // \\ → Backslash
            (b"(a\\\\b)", b"a\\b"),
            // 组合使用
            (b"(a\\(b\\))", b"a(b)"),
            (b"(\\n\\r\\t\\b\\f)", b"\n\r\t\x08\x0C"),
            (b"(text\\nmore\\ttext)", b"text\nmore\ttext"),
            (b"(start\\n\\rmiddle\\tend)", b"start\n\rmiddle\tend"),
            // \r\n 转义序列 - 保持为 \r\n，不转为 \n\n
            (b"(text\\r\\nmore)", b"text\r\nmore"),
            // ========== 八进制转义 \ddd ==========
            // 3位八进制
            (b"(\\053)", b"+"),
            (b"(\\101)", b"A"),
            // 2位八进制
            (b"(\\53)", b"+"),
            (b"(\\101)", b"A"),
            // 1位八进制
            (b"(\\5)", b"\x05"),
            (b"(\\7)", b"\x07"),
            // 八进制边界值
            (b"(\\000)", b"\x00"),
            (b"(\\377)", b"\xFF"),
            (b"(\\256)", b"\xAE"),
            (b"(\\300)", b"\xC0"),
            // 八进制超过 377（高位溢出被忽略）
            (b"(\\400)", b"\x00"), // 400 (256) mod 256 = 0
            (b"(\\777)", b"\xFF"), // 777 (511) mod 256 = 255
            (b"(\\500)", b"\x40"), // 500 (320) mod 256 = 64
            (b"(\\600)", b"\x80"), // 600 (384) mod 256 = 128
            (b"(\\700)", b"\xC0"), // 700 (448) mod 256 = 192
            // 八进制后跟数字字符（必须使用3位，前导0）
            (b"(\\0053)", b"\x05\x33"), // \005 + '3'
            (b"(\\0534)", b"+4"),       // \053 + '4'
            (b"(\\1012)", b"A2"),       // \101 + '2'
            // 多个八进制
            (b"(\\053\\101)", b"+A"),
            (b"(\\053\\101\\102)", b"+AB"),
            // 示例：包含八进制字符的字符串
            (
                b"( This string contains \\245two octal characters\\307 . )",
                b" This string contains \xA5two octal characters\xC7 . ",
            ),
            // ========== 行尾折行（\ 后跟换行符，两者都被忽略） ==========
            // 注意：在 Rust 字节字面量中，\\ 是反斜杠字节(0x5C)，\n 是换行符字节(0x0A)，\r 是回车符字节(0x0D)
            (b"(These \\\ntwo)", b"These two"),     // \ 后跟 LF
            (b"(Start\\\n   end)", b"Start   end"), // \ 后跟 LF，保留后续空白
            (b"(A\\\rB)", b"AB"),                   // \ 后跟 CR
            (b"(X\\\r\nY)", b"XY"),                 // \ 后跟 CRLF
            (b"(line1\\\n   \tline2)", b"line1   \tline2"), // \ 后跟 LF，保留后续空白
            (b"(text\\\n\t  \r\nmore)", b"text\t  \r\nmore"), // \ 后跟 LF，保留后续空白
            // 示例：多行字符串
            (
                b"( These \\\ntwo strings \\\nare the same . )",
                b" These two strings are the same . ",
            ),
            // ========== 未转义换行符处理 ==========
            // 未转义的换行符保持原样，不统一转为 \n
            (b"(line1\nline2)", b"line1\nline2"), // LF 保持为 LF
            (b"(line1\rline2)", b"line1\rline2"), // CR 保持为 CR，不转为 \n
            (b"(line1\r\nline2)", b"line1\r\nline2"), // CRLF 保持为 CRLF，不转为 \n
            (b"(multi\nline\rtext\r\nhere)", b"multi\nline\rtext\r\nhere"), // 保持原样
            // 示例
            (
                b"( This string has an end-of-line at the end of it .\n)",
                b" This string has an end-of-line at the end of it .\n",
            ),
            // ========== 无效转义（\ 后跟不在表中的字符，\ 被忽略） ==========
            (b"(a\\ b)", b"a b"), // \ 后跟空格，\ 被忽略
            (b"(a\\x)", b"ax"),   // \ 后跟 x，\ 被忽略
            (b"(a\\z)", b"az"),   // \ 后跟 z，\ 被忽略
            (b"(a\\%)", b"a%"),   // \ 后跟 %，\ 被忽略
            // ========== 特殊字符和边界情况 ==========
            // 包含 null 字节
            (b"(text\\000null)", b"text\x00null"),
            // 包含各种 ASCII 控制字符
            (b"(\\001\\002\\003)", b"\x01\x02\x03"),
            (b"(\\007\\010\\011)", b"\x07\x08\x09"), // BEL, BS, TAB
            // 包含特殊字符的普通文本
            (b"(text with spaces)", b"text with spaces"),
            (
                b"( Strings may contain newlines\nand such . )",
                b" Strings may contain newlines\nand such . ",
            ),
        ];

        for &(input, expected) in cases {
            let mut tokenizer = Tokenizer::new(input);
            match tokenizer.parse_string() {
                Object::String(v) => {
                    assert_eq!(
                        v.as_slice(),
                        expected,
                        "Input: {:?} {:?}",
                        v,
                        String::from_utf8_lossy(input)
                    );
                }
                other => {
                    panic!("other {} {:?}", String::from_utf8_lossy(input), other)
                }
            }
        }
    }

    #[test]
    fn parse_hex_strings_pdf17() {
        let cases: &[(&[u8], &[u8])] = &[
            // ========== 基础格式 ==========
            (b"<48656C6C6F>", b"Hello"),
            (b"<>", b""),
            (b"<41>", b"A"),
            (b"<414243>", b"ABC"),
            // ========== 大小写混合 ==========
            (b"<68656c6c6f>", b"hello"),
            (b"<48656c6C6F>", b"Hello"),
            (b"<48AbCdEf>", b"H\xAB\xCD\xEF"),
            (b"<aAbBcCdD>", b"\xAA\xBB\xCC\xDD"),
            // ========== 空白字符处理（各种空白字符、位置） ==========
            // 基本空白字符忽略
            (b"<48 65\t6C\n6C\r6F\x0C>", b"Hello"),
            // 空白字符在开头和结尾
            (b"< 48656C6C6F >", b"Hello"),
            (b"<  48656C6C6F  >", b"Hello"),
            // 大量空白字符混合
            (b"<48\t\t65\n\n6C\r\r6F>", b"Helo"),
            (b"<48 \t\n\r\x0C 65>", b"He"),
            // 只有空白字符的字符串
            (b"<   >", b""),
            (b"<\t\n\r\x0C >", b""),
            // 空白字符在中间
            (b"<48 65 6C 6C 6F>", b"Hello"),
            (b"<48\t65\n6C\r6C\x0C6F>", b"Hello"),
            // ========== 奇数位补0（各种情况） ==========
            // 单个字符后跟空白
            (b"<A >", b"\xA0"),
            (b"<A\t>", b"\xA0"),
            (b"<A\n>", b"\xA0"),
            // 基本奇数位
            (b"<A>", b"\xA0"),
            (b"<901FA>", b"\x90\x1F\xA0"),
            (b"<901FA3>", b"\x90\x1F\xA3"),
            // 多个奇数位情况
            (b"<123>", b"\x12\x30"),
            (b"<12345>", b"\x12\x34\x50"),
            (b"<1234567>", b"\x12\x34\x56\x70"),
            // 奇数位与空白字符混合
            (b"<A B>", b"\xAB"),
            (b"<A\tB\nC>", b"\xAB\xC0"),
            (b"<901 FA>", b"\x90\x1F\xA0"),
            (b"<90 1F A>", b"\x90\x1F\xA0"),
            // ========== 边界情况 ==========
            // 最小有效字符串
            (b"<00>", b"\x00"),
            (b"<FF>", b"\xFF"),
            // 最大有效字符
            (b"<00FF>", b"\x00\xFF"),
            // 混合大小写和空白
            (b"<48Ab\tCd\nEf>", b"H\xAB\xCD\xEF"),
        ];

        for &(input, expected) in cases {
            let mut tokenizer = Tokenizer::new(input);
            match tokenizer.parse_hexadecimal_string() {
                Object::String(v) => {
                    assert_eq!(v.as_slice(), expected, "{}", String::from_utf8_lossy(input))
                }
                other => {
                    panic!("other {} {:?}", String::from_utf8_lossy(input), other)
                }
            }
        }
    }

    #[test]
    fn parse_invalid_strings() {
        // 注意：parse_string 是尝试获取一个字符串即可，后续有没有不关心
        // 但如果匹配不到闭合括号，仍然应该返回错误

        let invalid_inputs: &[&[u8]] = &[
            // ========== 未闭合括号（匹配不到闭合括号） ==========
            // 只有左括号
            b"(",
            b"(unmatched",
            b"(a(b)c",
            b"(level1(level2(level3)",
            b"(text",
            // 括号不匹配（无法正确闭合）
            b"(a(b(c)d)e",
            // ========== 未闭合的转义序列（\ 在字符串末尾，匹配不到后续字符） ==========
            // 未闭合的转义序列
            b"(a\\",
            b"(text\\",
            b"(\\",
            // 转义序列在字符串末尾（但未闭合）
            b"(text\\n",
            b"(\\053",
            // 八进制转义未完成
            b"(\\05",
            b"(\\5",
            b"(\\",
            // ========== 十六进制字符串无效字符（真正的语法错误） ==========
            // 未闭合的十六进制字符串（匹配不到闭合的 >）
            b"<",
            b"<48656C6C6F",
            b"<48",
            // 无效字符（非十六进制）
            b"<G>",
            b"<123G>",
            b"<12X34>",
            b"<12@34>",
            b"<12#34>",
            // 无效字符与空白混合
            b"<48 G5>",
            b"<48\tG\n65>",
            // 只有无效字符
            b"<XYZ>",
            b"<!!!>",
            // 无效字符在开头
            b"<G48656C6C6F>",
            // 无效字符在中间
            b"<486G656C6C6F>",
            // 无效字符在结尾
            b"<48656C6C6FG>",
            // 十六进制字符串中无效字符的更多组合
            b"<12G34H56>",
            b"<A B C D E F G>",
            // 混合错误情况
            b"<48656C6C6F", // 未闭合且有效内容
        ];

        for &input in invalid_inputs {
            let mut tokenizer = Tokenizer::new(input);
            // 根据输入类型选择解析方法
            match tokenizer.parse_string() {
                Object::Invalid(_) => {}
                other => {
                    panic!(
                        "should invalid {} {:?}",
                        String::from_utf8_lossy(input),
                        other
                    )
                }
            }
        }
    }

    #[test]
    fn parse_name_objects_pdf17() {
        let cases: &[(&[u8], &[u8])] = &[
            // ========== 基础格式 ==========
            // 注意：Name object 以 / 开头，但 / 不是名称的一部分
            (b"/Name1", b"Name1"),
            (b"/ASomewhatLongerName", b"ASomewhatLongerName"),
            (b"/", b""), // 空名称，有效
            // ========== 包含特殊字符的名称 ==========
            (
                b"/A;Name_With-Various***Characters?",
                b"A;Name_With-Various***Characters?",
            ),
            (b"/1 . 2", b"1"), // 空格终止名称解析
            (b"/$$", b"$$"),
            (b"/@pattern", b"@pattern"),
            (b"/. notdef", b"."), // 空格终止名称解析
            // ========== 使用 # 转义序列（PDF 1.2+） ==========
            // # 后跟2位十六进制代码表示字符
            (b"/Adobe#20Green", b"Adobe Green"), // #20 = 空格 (0x20)
            (b"/PANTONE#205757#20CV", b"PANTONE 5757 CV"), // #20 = 空格
            (b"/paired#28#29parentheses", b"paired()parentheses"), // #28 = '(', #29 = ')'
            (b"/The_Key_of_F#23_Minor", b"The_Key_of_F#_Minor"), // #23 = '#' 本身
            (b"/A#42", b"AB"),                   // #42 = 'B' (0x42)
            // ========== # 转义边界情况 ==========
            (b"/Name#00", b"Name\x00"), // #00 = null (虽然不推荐，但技术上允许)
            (b"/Name#FF", b"Name\xFF"), // #FF = 255
            (b"/Name#0A", b"Name\n"),   // #0A = LF
            (b"/Name#0D", b"Name\r"),   // #0D = CR
            (b"/Name#09", b"Name\t"),   // #09 = TAB
            // ========== 大小写敏感 ==========
            (b"/A", b"A"),
            (b"/a", b"a"), // 与 /A 不同
            (b"/Name", b"Name"),
            (b"/NAME", b"NAME"), // 与 /Name 不同
            // ========== 多个 # 转义 ==========
            (b"/Test#20#20#20", b"Test   "), // 三个空格
            (b"/A#42#43#44", b"ABCD"),       // ABC
            (b"/#20#21#22", b" !\""),        // 空格、!、"
            // ========== 混合普通字符和 # 转义 ==========
            (b"/Name#20With#20Space", b"Name With Space"),
            (b"/Font#2DName", b"Font-Name"), // #2D = '-'
            (b"/Key#3DValue", b"Key=Value"), // #3D = '='
            // ========== Delimiter 终止名称解析 ==========
            (b"/Name(rest", b"Name"), // '(' 终止名称
            (b"/Name)rest", b"Name"), // ')' 终止名称
            (b"/Name<rest", b"Name"), // '<' 终止名称
            (b"/Name>rest", b"Name"), // '>' 终止名称
            (b"/Name[rest", b"Name"), // '[' 终止名称
            (b"/Name]rest", b"Name"), // ']' 终止名称
            (b"/Name{rest", b"Name"), // '{' 终止名称
            (b"/Name}rest", b"Name"), // '}' 终止名称
            (b"/Name/rest", b"Name"), // '/' 终止名称（第二个斜杠）
            (b"/Name%rest", b"Name"), // '%' 终止名称
        ];

        for &(input, expected) in cases {
            let mut tokenizer = Tokenizer::new(input);
            match tokenizer.parse_name() {
                Object::Name(v) => {
                    assert_eq!(
                        v.as_slice(),
                        expected,
                        "Input: {:?} Expected: {:?} Actual: {:?}",
                        String::from_utf8_lossy(input),
                        String::from_utf8_lossy(expected),
                        String::from_utf8_lossy(&v)
                    );
                }
                Object::Invalid(e) => {
                    panic!(
                        "Failed to parse name {:?}: {}",
                        String::from_utf8_lossy(input),
                        e
                    );
                }
                other => {
                    panic!(
                        "Unexpected result for {:?}: {:?}",
                        String::from_utf8_lossy(input),
                        other
                    );
                }
            }
        }
    }

    #[test]
    fn parse_boolean_objects() {
        // ========== Boolean Objects (PDF 3.2.1) ==========
        // PDF provides boolean objects identified by the keywords true and false.
        // 匹配到 true/false 就行，后续字符由其他环节负责

        // true 的测试用例
        let true_cases: &[&[u8]] = &[
            b"true",
            b"true ",
            b"true\t",
            b"true\n",
            b"true\r",
            b"true%comment\n",
            b"true(",
            b"true[",
            b"true<",
            b"true/",
            b"trueabc", // 后续字符由其他环节负责
            b"true123",
            b"truetrue",
        ];

        for &input in true_cases {
            let mut tokenizer = Tokenizer::new(input);
            match tokenizer.parse_boolean() {
                Object::Boolean(true) => {
                    // 正确：解析为 true
                }
                other => {
                    panic!(
                        "Expected Boolean(true), got {:?} for input {:?}",
                        other,
                        String::from_utf8_lossy(input)
                    );
                }
            }
        }

        // false 的测试用例
        let false_cases: &[&[u8]] = &[
            b"false",
            b"false ",
            b"false\t",
            b"false\n",
            b"false\r",
            b"false%comment\n",
            b"false(",
            b"false[",
            b"false<",
            b"false/",
            b"falseabc", // 后续字符由其他环节负责
            b"false123",
            b"falsefalse",
        ];

        for &input in false_cases {
            let mut tokenizer = Tokenizer::new(input);
            match tokenizer.parse_boolean() {
                Object::Boolean(false) => {
                    // 正确：解析为 false
                }
                other => {
                    panic!(
                        "Expected Boolean(false), got {:?} for input {:?}",
                        other,
                        String::from_utf8_lossy(input)
                    );
                }
            }
        }
    }

    #[test]
    fn parse_invalid_booleans() {
        // 无效的布尔值测试：无法匹配 true 或 false
        let invalid_cases: &[&[u8]] = &[
            b"True",  // 大写 T（PDF 区分大小写）
            b"TRUE",  // 全大写
            b"False", // 大写 F
            b"FALSE", // 全大写
            b"tru",   // 不完整
            b"fals",  // 不完整
            b"t",     // 只有首字母
            b"f",     // 只有首字母
            b"",      // 空输入
            b"abc",   // 不以 t 或 f 开头
        ];

        for &input in invalid_cases {
            let mut tokenizer = Tokenizer::new(input);
            match tokenizer.parse_boolean() {
                Object::Invalid(_) => {
                    // 正确：应返回 Invalid
                }
                other => {
                    panic!(
                        "Expected Invalid for {:?}, got {:?}",
                        String::from_utf8_lossy(input),
                        other
                    );
                }
            }
        }
    }

    #[test]
    fn parse_null_object() {
        // ========== Null Object (PDF 3.2.8) ==========
        // The null object has a type and value that are unequal to those of any other object.
        // There is only one object of type null, denoted by the keyword null.

        let cases: &[&[u8]] = &[
            b"null",
            b"null ",
            b"null\t",
            b"null\n",
            b"null\r",
            b"null%comment\n",
            b"null(",
            b"null[",
            b"null<",
        ];

        for &input in cases {
            let mut tokenizer = Tokenizer::new(input);
            match tokenizer.parse_null() {
                Object::Null => {
                    // 正确：解析为 null object
                }
                other => {
                    panic!(
                        "Expected null object, got {:?} for input {:?}",
                        other,
                        String::from_utf8_lossy(input)
                    );
                }
            }
        }
    }

    // 辅助函数：验证数组结构（内部递归用）
    fn assert_array_structure_inner(obj: &Object, expected: &[Object], input: &[u8]) {
        match obj {
            Object::Array(elements) => {
                assert_eq!(
                    elements.len(),
                    expected.len(),
                    "Input: {:?} Array length mismatch: expected {}, got {}",
                    String::from_utf8_lossy(input),
                    expected.len(),
                    elements.len()
                );
                for (i, (actual, exp)) in elements.iter().zip(expected.iter()).enumerate() {
                    match (actual, exp) {
                        (Object::Integer(a), Object::Integer(b)) => {
                            assert_eq!(
                                a,
                                b,
                                "Input: {:?} Integer mismatch at index {}",
                                String::from_utf8_lossy(input),
                                i
                            );
                        }
                        (Object::Real(a), Object::Real(b)) => {
                            assert!(
                                (a - b).abs() < f64::EPSILON,
                                "Input: {:?} Real mismatch at index {}",
                                String::from_utf8_lossy(input),
                                i
                            );
                        }
                        (Object::Boolean(a), Object::Boolean(b)) => {
                            assert_eq!(
                                a,
                                b,
                                "Input: {:?} Boolean mismatch at index {}",
                                String::from_utf8_lossy(input),
                                i
                            );
                        }
                        (Object::Null, Object::Null) => {}
                        (Object::String(a), Object::String(b)) => {
                            assert_eq!(
                                a,
                                b,
                                "Input: {:?} String mismatch at index {}",
                                String::from_utf8_lossy(input),
                                i
                            );
                        }
                        (Object::Name(a), Object::Name(b)) => {
                            assert_eq!(
                                a,
                                b,
                                "Input: {:?} Name mismatch at index {}",
                                String::from_utf8_lossy(input),
                                i
                            );
                        }
                        (Object::Array(_), Object::Array(exp_inner)) => {
                            assert_array_structure_inner(actual, exp_inner, input);
                        }
                        (Object::Dictionary(a), Object::Dictionary(b)) => {
                            assert_eq!(
                                a,
                                b,
                                "Input: {:?} Dictionary mismatch at index {}",
                                String::from_utf8_lossy(input),
                                i
                            );
                        }
                        (Object::Stream(a), Object::Stream(b)) => {
                            assert_eq!(
                                a.length,
                                b.length,
                                "Input: {:?} Stream length mismatch at index {}",
                                String::from_utf8_lossy(input),
                                i
                            );
                            assert_eq!(
                                a.data,
                                b.data,
                                "Input: {:?} Stream data mismatch at index {}",
                                String::from_utf8_lossy(input),
                                i
                            );
                        }
                        _ => panic!(
                            "Input: {:?} Type mismatch at index {}: expected {:?}, got {:?}",
                            String::from_utf8_lossy(input),
                            i,
                            exp,
                            actual
                        ),
                    }
                }
            }
            _ => panic!(
                "Input: {:?} Expected Array, for {:?} got {:?}",
                String::from_utf8_lossy(input),
                expected,
                obj
            ),
        }
    }

    // 辅助函数：验证数组结构
    fn assert_array_structure(input: &[u8], obj: &Object, expected: &[Object]) {
        assert_array_structure_inner(obj, expected, input);
    }

    #[test]
    fn parse_array_objects_pdf17() {
        // ========== Array Objects (PDF 3.2.5) ==========
        // An array is written as a sequence of objects enclosed in square brackets ([ and ])
        // Array elements may be any combination of numbers, strings, names, booleans, null, or other arrays

        // ========== 空数组 ==========
        let empty_cases: &[&[u8]] = &[b"[]", b"[ ]", b"[\t]", b"[\n]", b"[  \t\n  ]"];
        for &input in empty_cases {
            let mut tokenizer = Tokenizer::new(input);
            match tokenizer.parse_array() {
                Object::Array(elements) => {
                    assert_eq!(
                        elements.len(),
                        0,
                        "Expected empty array for {:?}",
                        String::from_utf8_lossy(input)
                    );
                }
                other => panic!(
                    "Expected empty array for {:?}, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }

        // ========== 单元素数组：验证元素类型和值 ==========
        {
            // 整数
            let input = b"[42]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(input, &t.parse_array(), &[Object::Integer(42)]);
        }
        {
            // 负整数
            let input = b"[-100]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(input, &t.parse_array(), &[Object::Integer(-100)]);
        }
        {
            // 实数
            let input = b"[3.14]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(input, &t.parse_array(), &[Object::Real(3.14)]);
        }
        {
            // 布尔值 true
            let input = b"[true]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(input, &t.parse_array(), &[Object::Boolean(true)]);
        }
        {
            // 布尔值 false
            let input = b"[false]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(input, &t.parse_array(), &[Object::Boolean(false)]);
        }
        {
            // null
            let input = b"[null]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(input, &t.parse_array(), &[Object::Null]);
        }
        {
            // 字符串
            let input = b"[(Hello)]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(
                input,
                &t.parse_array(),
                &[Object::String(b"Hello".to_vec())],
            );
        }
        {
            // 十六进制字符串
            let input = b"[<48656C6C6F>]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(
                input,
                &t.parse_array(),
                &[Object::String(b"Hello".to_vec())],
            );
        }
        {
            // 名称
            let input = b"[/Name]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(input, &t.parse_array(), &[Object::Name(b"Name".to_vec())]);
        }

        // ========== 多元素数组：验证每个元素 ==========
        {
            // PDF 规范示例: [ 549 3.14 false ( Ralph ) /SomeName ]
            let input = b"[ 549 3.14 false ( Ralph ) /SomeName ]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(
                input,
                &t.parse_array(),
                &[
                    Object::Integer(549),
                    Object::Real(3.14),
                    Object::Boolean(false),
                    Object::String(b" Ralph ".to_vec()),
                    Object::Name(b"SomeName".to_vec()),
                ],
            );
        }
        {
            // 混合类型
            let input = b"[1 3.14 true false null (string) <48> /Name]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(
                input,
                &t.parse_array(),
                &[
                    Object::Integer(1),
                    Object::Real(3.14),
                    Object::Boolean(true),
                    Object::Boolean(false),
                    Object::Null,
                    Object::String(b"string".to_vec()),
                    Object::String(b"H".to_vec()), // <48> = 'H'
                    Object::Name(b"Name".to_vec()),
                ],
            );
        }

        // ========== 嵌套数组：验证结构和深度 ==========
        {
            // [[]] - 包含一个空数组
            let input = b"[[]]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(input, &t.parse_array(), &[Object::Array(vec![])]);
        }
        {
            // [[1]] - 包含一个单元素数组
            let input = b"[[1]]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(
                input,
                &t.parse_array(),
                &[Object::Array(vec![Object::Integer(1)])],
            );
        }
        {
            // [[1 2] [3 4]] - 包含两个数组
            let input = b"[[1 2] [3 4]]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(
                input,
                &t.parse_array(),
                &[
                    Object::Array(vec![Object::Integer(1), Object::Integer(2)]),
                    Object::Array(vec![Object::Integer(3), Object::Integer(4)]),
                ],
            );
        }
        {
            // [1 [2 3] 4] - 混合元素和嵌套数组
            let input = b"[1 [2 3] 4]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(
                input,
                &t.parse_array(),
                &[
                    Object::Integer(1),
                    Object::Array(vec![Object::Integer(2), Object::Integer(3)]),
                    Object::Integer(4),
                ],
            );
        }
        {
            // [[[1]]] - 三层嵌套
            let input = b"[[[1]]]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(
                input,
                &t.parse_array(),
                &[Object::Array(vec![Object::Array(vec![Object::Integer(1)])])],
            );
        }
        {
            // [[1 [2 [3]]]] - 深层嵌套
            let input = b"[[1 [2 [3]]]]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(
                input,
                &t.parse_array(),
                &[Object::Array(vec![
                    Object::Integer(1),
                    Object::Array(vec![
                        Object::Integer(2),
                        Object::Array(vec![Object::Integer(3)]),
                    ]),
                ])],
            );
        }
        {
            // 复杂嵌套：混合类型和多层
            let input = b"[/A [1 (x) [true /B]]]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(
                input,
                &t.parse_array(),
                &[
                    Object::Name(b"A".to_vec()),
                    Object::Array(vec![
                        Object::Integer(1),
                        Object::String(b"x".to_vec()),
                        Object::Array(vec![Object::Boolean(true), Object::Name(b"B".to_vec())]),
                    ]),
                ],
            );
        }

        // ========== 紧凑格式（无空白） ==========
        {
            // Name 之间无需空白
            let input = b"[/A/B/C]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(
                input,
                &t.parse_array(),
                &[
                    Object::Name(b"A".to_vec()),
                    Object::Name(b"B".to_vec()),
                    Object::Name(b"C".to_vec()),
                ],
            );
        }
        {
            // String 之间无需空白
            let input = b"[(a)(b)(c)]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(
                input,
                &t.parse_array(),
                &[
                    Object::String(b"a".to_vec()),
                    Object::String(b"b".to_vec()),
                    Object::String(b"c".to_vec()),
                ],
            );
        }
        {
            // 混合无空白
            let input = b"[1(a)/B]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(
                input,
                &t.parse_array(),
                &[
                    Object::Integer(1),
                    Object::String(b"a".to_vec()),
                    Object::Name(b"B".to_vec()),
                ],
            );
        }

        // ========== 数组中包含字典 ==========
        {
            // 单个字典元素
            let input = b"[<</Key 1>>]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(
                input,
                &t.parse_array(),
                &[Object::Dictionary(vec![(
                    b"Key".to_vec(),
                    Object::Integer(1),
                )])],
            );
        }
        {
            // 多个字典元素
            let input = b"[<</A 1>> <</B 2>>]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(
                input,
                &t.parse_array(),
                &[
                    Object::Dictionary(vec![(b"A".to_vec(), Object::Integer(1))]),
                    Object::Dictionary(vec![(b"B".to_vec(), Object::Integer(2))]),
                ],
            );
        }
        {
            // 混合字典和其他元素
            let input = b"[1 <</Type /Page>> (hello)]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(
                input,
                &t.parse_array(),
                &[
                    Object::Integer(1),
                    Object::Dictionary(vec![(b"Type".to_vec(), Object::Name(b"Page".to_vec()))]),
                    Object::String(b"hello".to_vec()),
                ],
            );
        }
        {
            // 嵌套字典
            let input = b"[<</Outer <</Inner 42>>>>]";
            let mut t = Tokenizer::new(input);
            assert_array_structure(
                input,
                &t.parse_array(),
                &[Object::Dictionary(vec![(
                    b"Outer".to_vec(),
                    Object::Dictionary(vec![(b"Inner".to_vec(), Object::Integer(42))]),
                )])],
            );
        }

        // ========== 数组中包含流 ==========
        {
            // 单个流元素
            let input = b"[<</Length 5>>\nstream\nHelloendstream]";
            let mut t = Tokenizer::new(input);
            match t.parse_array() {
                Object::Array(elements) => {
                    assert_eq!(
                        elements.len(),
                        1,
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                    match &elements[0] {
                        Object::Stream(s) => {
                            assert_eq!(s.length, 5, "Input: {:?}", String::from_utf8_lossy(input));
                            assert_eq!(
                                s.data,
                                b"Hello",
                                "Input: {:?}",
                                String::from_utf8_lossy(input)
                            );
                        }
                        other => panic!(
                            "Input: {:?} Expected Stream, got {:?}",
                            String::from_utf8_lossy(input),
                            other
                        ),
                    }
                }
                other => panic!(
                    "Input: {:?} Expected Array, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
        {
            // 混合流和其他元素
            let input = b"[/Type <</Length 3>>\nstream\nabcendstream 123]";
            let mut t = Tokenizer::new(input);
            match t.parse_array() {
                Object::Array(elements) => {
                    assert_eq!(
                        elements.len(),
                        3,
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                    assert_eq!(
                        elements[0],
                        Object::Name(b"Type".to_vec()),
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                    match &elements[1] {
                        Object::Stream(s) => {
                            assert_eq!(s.length, 3, "Input: {:?}", String::from_utf8_lossy(input));
                            assert_eq!(
                                s.data,
                                b"abc",
                                "Input: {:?}",
                                String::from_utf8_lossy(input)
                            );
                        }
                        other => panic!(
                            "Input: {:?} Expected Stream at index 1, got {:?}",
                            String::from_utf8_lossy(input),
                            other
                        ),
                    }
                    assert_eq!(
                        elements[2],
                        Object::Integer(123),
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                }
                other => panic!(
                    "Input: {:?} Expected Array, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
    }

    #[test]
    fn parse_invalid_arrays() {
        // ========== 无效数组测试 ==========
        let invalid_inputs: &[&[u8]] = &[
            // 未闭合
            b"[", b"[1 2 3", b"[[1]", // 缺少开头
            b"]", b"1 2 3]",
        ];

        for &input in invalid_inputs {
            let mut tokenizer = Tokenizer::new(input);
            match tokenizer.parse_array() {
                Object::Invalid(_) => {
                    // 正确：应返回 Invalid
                }
                other => {
                    panic!(
                        "Should be invalid for {:?}, got {:?}",
                        String::from_utf8_lossy(input),
                        other
                    );
                }
            }
        }
    }

    // 辅助函数：验证字典结构
    fn assert_dict_structure(input: &[u8], obj: &Object, expected: &[(Vec<u8>, Object)]) {
        match obj {
            Object::Dictionary(entries) => {
                assert_eq!(
                    entries.len(),
                    expected.len(),
                    "Input: {:?} Dictionary length mismatch: expected {}, got {}",
                    String::from_utf8_lossy(input),
                    expected.len(),
                    entries.len()
                );
                for (i, ((actual_key, actual_val), (exp_key, exp_val))) in
                    entries.iter().zip(expected.iter()).enumerate()
                {
                    assert_eq!(
                        actual_key,
                        exp_key,
                        "Input: {:?} Key mismatch at index {}: expected {:?}, got {:?}",
                        String::from_utf8_lossy(input),
                        i,
                        String::from_utf8_lossy(exp_key),
                        String::from_utf8_lossy(actual_key)
                    );
                    assert_eq!(
                        actual_val,
                        exp_val,
                        "Input: {:?} Value mismatch for key {:?} at index {}",
                        String::from_utf8_lossy(input),
                        String::from_utf8_lossy(actual_key),
                        i
                    );
                }
            }
            _ => panic!(
                "Input: {:?} Expected Dictionary, got {:?}",
                String::from_utf8_lossy(input),
                obj
            ),
        }
    }

    #[test]
    fn parse_dictionary_objects_pdf17() {
        // ========== Dictionary Objects (PDF 3.2.6) ==========
        // A dictionary is written as a sequence of key-value pairs enclosed in double angle brackets (<< … >>)
        // The key must be a name, the value can be any kind of object

        // ========== 空字典 ==========
        {
            let input = b"<<>>";
            let mut t = Tokenizer::new(input);
            assert_dict_structure(input, &t.parse_dictionary(), &[]);
        }
        {
            let input = b"<< >>";
            let mut t = Tokenizer::new(input);
            assert_dict_structure(input, &t.parse_dictionary(), &[]);
        }
        {
            let input = b"<<\n>>";
            let mut t = Tokenizer::new(input);
            assert_dict_structure(input, &t.parse_dictionary(), &[]);
        }

        // ========== 单个键值对 ==========
        {
            // 整数值
            let input = b"<</Key 123>>";
            let mut t = Tokenizer::new(input);
            assert_dict_structure(
                input,
                &t.parse_dictionary(),
                &[(b"Key".to_vec(), Object::Integer(123))],
            );
        }
        {
            // 实数值
            let input = b"<</Version 0.01>>";
            let mut t = Tokenizer::new(input);
            assert_dict_structure(
                input,
                &t.parse_dictionary(),
                &[(b"Version".to_vec(), Object::Real(0.01))],
            );
        }
        {
            // 布尔值
            let input = b"<</Flag true>>";
            let mut t = Tokenizer::new(input);
            assert_dict_structure(
                input,
                &t.parse_dictionary(),
                &[(b"Flag".to_vec(), Object::Boolean(true))],
            );
        }
        {
            // null 值 - 根据 PDF 规范，null 值的条目等价于不存在，应被忽略
            let input = b"<</Empty null>>";
            let mut t = Tokenizer::new(input);
            assert_dict_structure(input, &t.parse_dictionary(), &[]); // 空字典
        }
        {
            // 字符串值
            let input = b"<</Title (Hello)>>";
            let mut t = Tokenizer::new(input);
            assert_dict_structure(
                input,
                &t.parse_dictionary(),
                &[(b"Title".to_vec(), Object::String(b"Hello".to_vec()))],
            );
        }
        {
            // 十六进制字符串值
            let input = b"<</Data <48656C6C6F>>>";
            let mut t = Tokenizer::new(input);
            assert_dict_structure(
                input,
                &t.parse_dictionary(),
                &[(b"Data".to_vec(), Object::String(b"Hello".to_vec()))],
            );
        }
        {
            // 名称值
            let input = b"<</Type /Page>>";
            let mut t = Tokenizer::new(input);
            assert_dict_structure(
                input,
                &t.parse_dictionary(),
                &[(b"Type".to_vec(), Object::Name(b"Page".to_vec()))],
            );
        }
        {
            // 数组值
            let input = b"<</Kids [1 2 3]>>";
            let mut t = Tokenizer::new(input);
            assert_dict_structure(
                input,
                &t.parse_dictionary(),
                &[(
                    b"Kids".to_vec(),
                    Object::Array(vec![
                        Object::Integer(1),
                        Object::Integer(2),
                        Object::Integer(3),
                    ]),
                )],
            );
        }

        // ========== 多个键值对 ==========
        {
            let input = b"<</Type /Page /Count 5>>";
            let mut t = Tokenizer::new(input);
            assert_dict_structure(
                input,
                &t.parse_dictionary(),
                &[
                    (b"Type".to_vec(), Object::Name(b"Page".to_vec())),
                    (b"Count".to_vec(), Object::Integer(5)),
                ],
            );
        }
        {
            // PDF 规范示例
            let input = b"<</Type /Example /Subtype /DictionaryExample /Version 0.01 /IntegerItem 12 /StringItem (a string)>>";
            let mut t = Tokenizer::new(input);
            assert_dict_structure(
                input,
                &t.parse_dictionary(),
                &[
                    (b"Type".to_vec(), Object::Name(b"Example".to_vec())),
                    (
                        b"Subtype".to_vec(),
                        Object::Name(b"DictionaryExample".to_vec()),
                    ),
                    (b"Version".to_vec(), Object::Real(0.01)),
                    (b"IntegerItem".to_vec(), Object::Integer(12)),
                    (b"StringItem".to_vec(), Object::String(b"a string".to_vec())),
                ],
            );
        }

        // ========== 嵌套字典 ==========
        {
            let input = b"<</Outer <</Inner 42>>>>";
            let mut t = Tokenizer::new(input);
            assert_dict_structure(
                input,
                &t.parse_dictionary(),
                &[(
                    b"Outer".to_vec(),
                    Object::Dictionary(vec![(b"Inner".to_vec(), Object::Integer(42))]),
                )],
            );
        }
        {
            // PDF 规范示例中的嵌套字典
            let input = b"<</Subdictionary <</Item1 0.4 /Item2 true /LastItem (not!) /VeryLastItem (OK)>>>>";
            let mut t = Tokenizer::new(input);
            assert_dict_structure(
                input,
                &t.parse_dictionary(),
                &[(
                    b"Subdictionary".to_vec(),
                    Object::Dictionary(vec![
                        (b"Item1".to_vec(), Object::Real(0.4)),
                        (b"Item2".to_vec(), Object::Boolean(true)),
                        (b"LastItem".to_vec(), Object::String(b"not!".to_vec())),
                        (b"VeryLastItem".to_vec(), Object::String(b"OK".to_vec())),
                    ]),
                )],
            );
        }

        // ========== 多行格式 ==========
        {
            let input = b"<<\n/Type /Page\n/Count 0\n>>";
            let mut t = Tokenizer::new(input);
            assert_dict_structure(
                input,
                &t.parse_dictionary(),
                &[
                    (b"Type".to_vec(), Object::Name(b"Page".to_vec())),
                    (b"Count".to_vec(), Object::Integer(0)),
                ],
            );
        }

        // ========== 紧凑格式（名称之间无空白） ==========
        {
            let input = b"<</A/B/C/D>>";
            let mut t = Tokenizer::new(input);
            assert_dict_structure(
                input,
                &t.parse_dictionary(),
                &[
                    (b"A".to_vec(), Object::Name(b"B".to_vec())),
                    (b"C".to_vec(), Object::Name(b"D".to_vec())),
                ],
            );
        }

        // ========== 混合类型值 ==========
        {
            let input = b"<</Int 1 /Real 2.5 /Bool true /Str (x) /Name /Y /Arr [1]>>";
            let mut t = Tokenizer::new(input);
            assert_dict_structure(
                input,
                &t.parse_dictionary(),
                &[
                    (b"Int".to_vec(), Object::Integer(1)),
                    (b"Real".to_vec(), Object::Real(2.5)),
                    (b"Bool".to_vec(), Object::Boolean(true)),
                    (b"Str".to_vec(), Object::String(b"x".to_vec())),
                    (b"Name".to_vec(), Object::Name(b"Y".to_vec())),
                    (b"Arr".to_vec(), Object::Array(vec![Object::Integer(1)])),
                ],
            );
        }

        // ========== null 值忽略测试 ==========
        {
            // null 值的条目等价于不存在（PDF 3.2.6）
            let input = b"<</Key null>>";
            let mut t = Tokenizer::new(input);
            assert_dict_structure(input, &t.parse_dictionary(), &[]);
        }
        {
            // 多个条目中有 null 值，应被忽略
            let input = b"<</A 1 /B null /C 2>>";
            let mut t = Tokenizer::new(input);
            assert_dict_structure(
                input,
                &t.parse_dictionary(),
                &[
                    (b"A".to_vec(), Object::Integer(1)),
                    (b"C".to_vec(), Object::Integer(2)),
                ],
            );
        }
        {
            // 多个 null 值
            let input = b"<</A null /B null /C 3>>";
            let mut t = Tokenizer::new(input);
            assert_dict_structure(
                input,
                &t.parse_dictionary(),
                &[(b"C".to_vec(), Object::Integer(3))],
            );
        }
        {
            // 全部是 null 值
            let input = b"<</A null /B null>>";
            let mut t = Tokenizer::new(input);
            assert_dict_structure(input, &t.parse_dictionary(), &[]);
        }

        // ========== 字典中包含流作为值 ==========
        {
            // 流作为字典值
            let input = b"<</Content <</Length 5>>\nstream\nHelloendstream>>";
            let mut t = Tokenizer::new(input);
            match t.parse_dictionary() {
                Object::Dictionary(entries) => {
                    assert_eq!(
                        entries.len(),
                        1,
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                    assert_eq!(
                        entries[0].0,
                        b"Content",
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                    match &entries[0].1 {
                        Object::Stream(s) => {
                            assert_eq!(s.length, 5, "Input: {:?}", String::from_utf8_lossy(input));
                            assert_eq!(
                                s.data,
                                b"Hello",
                                "Input: {:?}",
                                String::from_utf8_lossy(input)
                            );
                        }
                        other => panic!(
                            "Input: {:?} Expected Stream value, got {:?}",
                            String::from_utf8_lossy(input),
                            other
                        ),
                    }
                }
                other => panic!(
                    "Input: {:?} Expected Dictionary, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
    }

    #[test]
    fn parse_invalid_dictionaries() {
        // ========== 无效字典测试 ==========
        let invalid_inputs: &[&[u8]] = &[
            // 未闭合
            b"<<",
            b"<</Key 1",
            b"<</Key 1>", // 只有一个 >
            // 缺少开头
            b">>",
            b"/Key 1>>",
            // key 不是 name
            b"<<1 2>>",     // 数字作为 key
            b"<<(str) 1>>", // 字符串作为 key
            b"<<true 1>>",  // 布尔值作为 key
            // 只有 key 没有 value
            b"<</Key>>",
            b"<</A 1 /B>>",
        ];

        for &input in invalid_inputs {
            let mut tokenizer = Tokenizer::new(input);
            match tokenizer.parse_dictionary() {
                Object::Invalid(_) => {
                    // 正确：应返回 Invalid
                }
                other => {
                    panic!(
                        "Should be invalid for {:?}, got {:?}",
                        String::from_utf8_lossy(input),
                        other
                    );
                }
            }
        }
    }

    #[test]
    fn parse_stream_objects_pdf17() {
        // ========== Stream Objects (PDF 3.2.7) ==========
        // A stream consists of a dictionary followed by zero or more bytes bracketed
        // between the keywords stream and endstream.
        // The keyword stream should be followed by an end-of-line marker (CRLF or LF, not CR alone)

        // ========== 基本流（LF 作为行尾） ==========
        {
            // 空流
            let input = b"<</Length 0>>\nstream\nendstream";
            let mut t = Tokenizer::new(input);
            match t.parse_stream() {
                Object::Stream(s) => {
                    assert_eq!(s.length, 0, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(s.data, b"", "Input: {:?}", String::from_utf8_lossy(input));
                }
                other => panic!(
                    "Input: {:?} Expected Stream, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
        {
            // 简单流数据
            let input = b"<</Length 5>>\nstream\nHelloendstream";
            let mut t = Tokenizer::new(input);
            match t.parse_stream() {
                Object::Stream(s) => {
                    assert_eq!(s.length, 5, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(
                        s.data,
                        b"Hello",
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                }
                other => panic!(
                    "Input: {:?} Expected Stream, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
        {
            // 流数据包含换行符
            let input = b"<</Length 6>>\nstream\nHello\nendstream";
            let mut t = Tokenizer::new(input);
            match t.parse_stream() {
                Object::Stream(s) => {
                    assert_eq!(s.length, 6, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(
                        s.data,
                        b"Hello\n",
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                }
                other => panic!(
                    "Input: {:?} Expected Stream, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }

        // ========== CRLF 作为行尾 ==========
        {
            let input = b"<</Length 5>>\r\nstream\r\nHelloendstream";
            let mut t = Tokenizer::new(input);
            match t.parse_stream() {
                Object::Stream(s) => {
                    assert_eq!(s.length, 5, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(
                        s.data,
                        b"Hello",
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                }
                other => panic!(
                    "Input: {:?} Expected Stream, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }

        // ========== endstream 前的可选 EOL ==========
        {
            // endstream 前有 LF（不计入 Length）
            let input = b"<</Length 5>>\nstream\nHello\nendstream";
            let mut t = Tokenizer::new(input);
            match t.parse_stream() {
                Object::Stream(s) => {
                    assert_eq!(s.length, 5, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(
                        s.data,
                        b"Hello",
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                }
                other => panic!(
                    "Input: {:?} Expected Stream, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
        {
            // endstream 前有 CRLF（不计入 Length）
            let input = b"<</Length 5>>\nstream\nHello\r\nendstream";
            let mut t = Tokenizer::new(input);
            match t.parse_stream() {
                Object::Stream(s) => {
                    assert_eq!(s.length, 5, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(
                        s.data,
                        b"Hello",
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                }
                other => panic!(
                    "Input: {:?} Expected Stream, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }

        // ========== 带 Filter 的流 ==========
        {
            let input = b"<</Length 10 /Filter /FlateDecode>>\nstream\n0123456789endstream";
            let mut t = Tokenizer::new(input);
            match t.parse_stream() {
                Object::Stream(s) => {
                    assert_eq!(s.length, 10, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(
                        s.filter,
                        Some(Box::new(Object::Name(b"FlateDecode".to_vec()))),
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                    assert_eq!(
                        s.data,
                        b"0123456789",
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                }
                other => panic!(
                    "Input: {:?} Expected Stream, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
        {
            // 多个 Filter（数组）
            let input = b"<</Length 10 /Filter [/ASCII85Decode /FlateDecode]>>\nstream\n0123456789endstream";
            let mut t = Tokenizer::new(input);
            match t.parse_stream() {
                Object::Stream(s) => {
                    assert_eq!(s.length, 10, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(
                        s.filter,
                        Some(Box::new(Object::Array(vec![
                            Object::Name(b"ASCII85Decode".to_vec()),
                            Object::Name(b"FlateDecode".to_vec()),
                        ]))),
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                }
                other => panic!(
                    "Input: {:?} Expected Stream, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }

        // ========== 带 DecodeParms 的流 ==========
        {
            let input = b"<</Length 10 /Filter /FlateDecode /DecodeParms <</Columns 4>>>>\nstream\n0123456789endstream";
            let mut t = Tokenizer::new(input);
            match t.parse_stream() {
                Object::Stream(s) => {
                    assert_eq!(s.length, 10, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(
                        s.decode_parms,
                        Some(Box::new(Object::Dictionary(vec![(
                            b"Columns".to_vec(),
                            Object::Integer(4)
                        ),]))),
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                }
                other => panic!(
                    "Input: {:?} Expected Stream, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }

        // ========== 带 DL 的流 (PDF 1.5) ==========
        {
            let input = b"<</Length 10 /DL 100>>\nstream\n0123456789endstream";
            let mut t = Tokenizer::new(input);
            match t.parse_stream() {
                Object::Stream(s) => {
                    assert_eq!(s.length, 10, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(
                        s.dl,
                        Some(100),
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                }
                other => panic!(
                    "Input: {:?} Expected Stream, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }

        // ========== 二进制数据流 ==========
        {
            let input = b"<</Length 4>>\nstream\n\x00\x01\x02\x03endstream";
            let mut t = Tokenizer::new(input);
            match t.parse_stream() {
                Object::Stream(s) => {
                    assert_eq!(s.length, 4, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(
                        s.data,
                        &[0x00, 0x01, 0x02, 0x03],
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                }
                other => panic!(
                    "Input: {:?} Expected Stream, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }

        // ========== 流数据中包含 "endstream" 子串（通过 Length 界定） ==========
        {
            // 如果数据中包含 "endstream"，应通过 Length 正确界定
            let data = b"endstream"; // 9 bytes
            let input = b"<</Length 9>>\nstream\nendstreamendstream";
            let mut t = Tokenizer::new(input);
            match t.parse_stream() {
                Object::Stream(s) => {
                    assert_eq!(s.length, 9, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(s.data, data, "Input: {:?}", String::from_utf8_lossy(input));
                }
                other => panic!(
                    "Input: {:?} Expected Stream, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }

        // ========== 带 F（外部文件）的流 (PDF 1.2) ==========
        {
            // F 指定外部文件，stream 和 endstream 之间的数据被忽略
            let input = b"<</Length 0 /F (external.dat)>>\nstream\nendstream";
            let mut t = Tokenizer::new(input);
            match t.parse_stream() {
                Object::Stream(s) => {
                    assert_eq!(s.length, 0, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(
                        s.f,
                        Some(Box::new(Object::String(b"external.dat".to_vec()))),
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                }
                other => panic!(
                    "Input: {:?} Expected Stream, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }

        // ========== 带 FFilter（外部文件过滤器）的流 (PDF 1.2) ==========
        {
            let input = b"<</Length 0 /F (data.gz) /FFilter /FlateDecode>>\nstream\nendstream";
            let mut t = Tokenizer::new(input);
            match t.parse_stream() {
                Object::Stream(s) => {
                    assert_eq!(s.length, 0, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(
                        s.f,
                        Some(Box::new(Object::String(b"data.gz".to_vec()))),
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                    assert_eq!(
                        s.f_filter,
                        Some(Box::new(Object::Name(b"FlateDecode".to_vec()))),
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                }
                other => panic!(
                    "Input: {:?} Expected Stream, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
        {
            // 多个 FFilter（数组）
            let input = b"<</Length 0 /F (data.bin) /FFilter [/ASCII85Decode /LZWDecode]>>\nstream\nendstream";
            let mut t = Tokenizer::new(input);
            match t.parse_stream() {
                Object::Stream(s) => {
                    assert_eq!(s.length, 0, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(
                        s.f_filter,
                        Some(Box::new(Object::Array(vec![
                            Object::Name(b"ASCII85Decode".to_vec()),
                            Object::Name(b"LZWDecode".to_vec()),
                        ]))),
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                }
                other => panic!(
                    "Input: {:?} Expected Stream, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }

        // ========== 带 FDecodeParms（外部文件过滤器参数）的流 (PDF 1.2) ==========
        {
            let input = b"<</Length 0 /F (data.bin) /FFilter /LZWDecode /FDecodeParms <</EarlyChange 0>>>>\nstream\nendstream";
            let mut t = Tokenizer::new(input);
            match t.parse_stream() {
                Object::Stream(s) => {
                    assert_eq!(s.length, 0, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(
                        s.f_decode_parms,
                        Some(Box::new(Object::Dictionary(vec![(
                            b"EarlyChange".to_vec(),
                            Object::Integer(0)
                        )]))),
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                }
                other => panic!(
                    "Input: {:?} Expected Stream, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }

        // ========== 完整的外部文件流（F + FFilter + FDecodeParms） ==========
        {
            let input = b"<</Length 0 /F (image.raw) /FFilter /DCTDecode /FDecodeParms <</ColorTransform 1>>>>\nstream\nendstream";
            let mut t = Tokenizer::new(input);
            match t.parse_stream() {
                Object::Stream(s) => {
                    assert_eq!(s.length, 0, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(
                        s.f,
                        Some(Box::new(Object::String(b"image.raw".to_vec()))),
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                    assert_eq!(
                        s.f_filter,
                        Some(Box::new(Object::Name(b"DCTDecode".to_vec()))),
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                    assert_eq!(
                        s.f_decode_parms,
                        Some(Box::new(Object::Dictionary(vec![(
                            b"ColorTransform".to_vec(),
                            Object::Integer(1)
                        )]))),
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                }
                other => panic!(
                    "Input: {:?} Expected Stream, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
    }

    #[test]
    fn parse_invalid_streams() {
        // ========== 无效流测试（看到 stream 关键字但解析失败） ==========
        let invalid_inputs: &[&[u8]] = &[
            // 缺少 Length（有 stream 关键字）
            b"<<>>\nstream\nendstream",
            // stream 关键字后只有 CR（不允许，规范要求 CRLF 或 LF）
            b"<</Length 5>>stream\rHelloendstream",
            // 缺少 endstream 关键字
            b"<</Length 5>>\nstream\nHello",
            // Length 与实际数据不符（数据不足）
            b"<</Length 100>>\nstream\nHelloendstream",
        ];

        for &input in invalid_inputs {
            let mut tokenizer = Tokenizer::new(input);
            match tokenizer.parse_stream() {
                Object::Invalid(_) => {
                    // 正确：应返回 Invalid
                }
                other => {
                    panic!(
                        "Should be invalid for {:?}, got {:?}",
                        String::from_utf8_lossy(input),
                        other
                    );
                }
            }
        }
    }

    #[test]
    fn parse_stream_fallback_to_dictionary() {
        // ========== 没看到 stream 关键字，退化为字典 ==========
        let fallback_cases: &[&[u8]] = &[
            // 没有 stream 关键字，只有字典
            b"<</Length 5>>",
            b"<</Type /Page /Count 0>>",
            // 字典后跟其他内容但不是 stream
            b"<</Length 5>>\nHelloendstream",
            b"<</Key 1>>abc",
        ];

        for &input in fallback_cases {
            let mut tokenizer = Tokenizer::new(input);
            match tokenizer.parse_stream() {
                Object::Dictionary(_) => {
                    // 正确：应返回 Dictionary
                }
                other => {
                    panic!(
                        "Should fallback to Dictionary for {:?}, got {:?}",
                        String::from_utf8_lossy(input),
                        other
                    );
                }
            }
        }
    }

    #[test]
    fn parse_indirect_reference() {
        // ========== Indirect Objects (PDF 3.2.9) ==========
        // 间接引用格式: object_number generation_number R
        // object_number: 正整数
        // generation_number: 非负整数
        
        // ========== 基本间接引用 ==========
        let cases: &[(&[u8], i64, i64)] = &[
            // 基本格式
            (b"1 0 R", 1, 0),
            (b"12 0 R", 12, 0),
            (b"123 0 R", 123, 0),
            // 不同的 generation number
            (b"1 1 R", 1, 1),
            (b"5 2 R", 5, 2),
            (b"10 99 R", 10, 99),
            // 大对象号
            (b"999999 0 R", 999999, 0),
            // 带空白字符
            (b"1  0  R", 1, 0),
            (b"1\t0\tR", 1, 0),
            (b"1\n0\nR", 1, 0),
        ];

        for &(input, expected_obj_num, expected_gen_num) in cases {
            let mut tokenizer = Tokenizer::new(input);
            match tokenizer.parse_indirect_ref() {
                Object::IndirectRef(obj_num, gen_num) => {
                    assert_eq!(
                        obj_num, expected_obj_num,
                        "Input: {:?} Object number mismatch",
                        String::from_utf8_lossy(input)
                    );
                    assert_eq!(
                        gen_num, expected_gen_num,
                        "Input: {:?} Generation number mismatch",
                        String::from_utf8_lossy(input)
                    );
                }
                other => panic!(
                    "Input: {:?} Expected IndirectRef, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
    }

    #[test]
    fn parse_indirect_reference_in_array() {
        // 数组中包含间接引用
        {
            let input = b"[1 0 R]";
            let mut t = Tokenizer::new(input);
            match t.parse_array() {
                Object::Array(elements) => {
                    assert_eq!(elements.len(), 1, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(
                        elements[0],
                        Object::IndirectRef(1, 0),
                        "Input: {:?}",
                        String::from_utf8_lossy(input)
                    );
                }
                other => panic!(
                    "Input: {:?} Expected Array, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
        {
            // 多个间接引用
            let input = b"[1 0 R 2 0 R 3 0 R]";
            let mut t = Tokenizer::new(input);
            match t.parse_array() {
                Object::Array(elements) => {
                    assert_eq!(elements.len(), 3, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(elements[0], Object::IndirectRef(1, 0));
                    assert_eq!(elements[1], Object::IndirectRef(2, 0));
                    assert_eq!(elements[2], Object::IndirectRef(3, 0));
                }
                other => panic!(
                    "Input: {:?} Expected Array, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
        {
            // 混合间接引用和其他类型
            let input = b"[/Type 1 0 R (hello) 2 0 R]";
            let mut t = Tokenizer::new(input);
            match t.parse_array() {
                Object::Array(elements) => {
                    assert_eq!(elements.len(), 4, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(elements[0], Object::Name(b"Type".to_vec()));
                    assert_eq!(elements[1], Object::IndirectRef(1, 0));
                    assert_eq!(elements[2], Object::String(b"hello".to_vec()));
                    assert_eq!(elements[3], Object::IndirectRef(2, 0));
                }
                other => panic!(
                    "Input: {:?} Expected Array, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
    }

    #[test]
    fn parse_indirect_reference_in_dictionary() {
        // 字典中包含间接引用作为值
        {
            let input = b"<</Parent 1 0 R>>";
            let mut t = Tokenizer::new(input);
            match t.parse_dictionary() {
                Object::Dictionary(entries) => {
                    assert_eq!(entries.len(), 1, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(entries[0].0, b"Parent");
                    assert_eq!(entries[0].1, Object::IndirectRef(1, 0));
                }
                other => panic!(
                    "Input: {:?} Expected Dictionary, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
        {
            // 多个间接引用
            let input = b"<</Parent 1 0 R /Kids 2 0 R /Resources 3 0 R>>";
            let mut t = Tokenizer::new(input);
            match t.parse_dictionary() {
                Object::Dictionary(entries) => {
                    assert_eq!(entries.len(), 3, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(entries[0], (b"Parent".to_vec(), Object::IndirectRef(1, 0)));
                    assert_eq!(entries[1], (b"Kids".to_vec(), Object::IndirectRef(2, 0)));
                    assert_eq!(entries[2], (b"Resources".to_vec(), Object::IndirectRef(3, 0)));
                }
                other => panic!(
                    "Input: {:?} Expected Dictionary, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
        {
            // 混合间接引用和直接对象
            let input = b"<</Type /Page /Parent 1 0 R /Count 5>>";
            let mut t = Tokenizer::new(input);
            match t.parse_dictionary() {
                Object::Dictionary(entries) => {
                    assert_eq!(entries.len(), 3, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(entries[0], (b"Type".to_vec(), Object::Name(b"Page".to_vec())));
                    assert_eq!(entries[1], (b"Parent".to_vec(), Object::IndirectRef(1, 0)));
                    assert_eq!(entries[2], (b"Count".to_vec(), Object::Integer(5)));
                }
                other => panic!(
                    "Input: {:?} Expected Dictionary, got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
    }

    #[test]
    fn parse_indirect_object_definition() {
        // ========== 间接对象定义 ==========
        // 格式: object_number generation_number obj ... endobj
        // 返回 Object::Object(obj_num, gen_num, Box<Object>)
        // 使用 parse_direct_obj 明确表示要解析 obj
        
        {
            // 简单字符串对象
            let input = b"12 0 obj\n( Brillig )\nendobj";
            let mut t = Tokenizer::new(input);
            match t.parse_direct_obj() {
                Object::Object(obj_num, gen_num, inner) => {
                    assert_eq!(obj_num, 12, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(gen_num, 0, "Input: {:?}", String::from_utf8_lossy(input));
                    assert_eq!(*inner, Object::String(b" Brillig ".to_vec()), "Input: {:?}", String::from_utf8_lossy(input));
                }
                other => panic!(
                    "Input: {:?} Expected Object::Object(12, 0, String), got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
        {
            // 整数对象
            let input = b"8 0 obj\n77\nendobj";
            let mut t = Tokenizer::new(input);
            match t.parse_direct_obj() {
                Object::Object(obj_num, gen_num, inner) => {
                    assert_eq!(obj_num, 8);
                    assert_eq!(gen_num, 0);
                    assert_eq!(*inner, Object::Integer(77));
                }
                other => panic!(
                    "Input: {:?} Expected Object::Object(8, 0, Integer(77)), got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
        {
            // 字典对象
            let input = b"1 0 obj\n<</Type /Catalog /Pages 2 0 R>>\nendobj";
            let mut t = Tokenizer::new(input);
            match t.parse_direct_obj() {
                Object::Object(obj_num, gen_num, inner) => {
                    assert_eq!(obj_num, 1);
                    assert_eq!(gen_num, 0);
                    match *inner {
                        Object::Dictionary(entries) => {
                            assert_eq!(entries.len(), 2);
                            assert_eq!(entries[0], (b"Type".to_vec(), Object::Name(b"Catalog".to_vec())));
                            assert_eq!(entries[1], (b"Pages".to_vec(), Object::IndirectRef(2, 0)));
                        }
                        other => panic!("Expected Dictionary, got {:?}", other),
                    }
                }
                other => panic!(
                    "Input: {:?} Expected Object::Object(1, 0, Dictionary), got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
        {
            // 数组对象
            let input = b"3 0 obj\n[1 2 3]\nendobj";
            let mut t = Tokenizer::new(input);
            match t.parse_direct_obj() {
                Object::Object(obj_num, gen_num, inner) => {
                    assert_eq!(obj_num, 3);
                    assert_eq!(gen_num, 0);
                    match *inner {
                        Object::Array(elements) => {
                            assert_eq!(elements.len(), 3);
                            assert_eq!(elements[0], Object::Integer(1));
                            assert_eq!(elements[1], Object::Integer(2));
                            assert_eq!(elements[2], Object::Integer(3));
                        }
                        other => panic!("Expected Array, got {:?}", other),
                    }
                }
                other => panic!(
                    "Input: {:?} Expected Object::Object(3, 0, Array), got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
        {
            // 流对象
            let input = b"7 0 obj\n<</Length 5>>\nstream\nHelloendstream\nendobj";
            let mut t = Tokenizer::new(input);
            match t.parse_direct_obj() {
                Object::Object(obj_num, gen_num, inner) => {
                    assert_eq!(obj_num, 7);
                    assert_eq!(gen_num, 0);
                    match *inner {
                        Object::Stream(s) => {
                            assert_eq!(s.length, 5);
                            assert_eq!(s.data, b"Hello");
                        }
                        other => panic!("Expected Stream, got {:?}", other),
                    }
                }
                other => panic!(
                    "Input: {:?} Expected Object::Object(7, 0, Stream), got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
        {
            // 非零 generation number
            let input = b"5 2 obj\nnull\nendobj";
            let mut t = Tokenizer::new(input);
            match t.parse_direct_obj() {
                Object::Object(obj_num, gen_num, inner) => {
                    assert_eq!(obj_num, 5);
                    assert_eq!(gen_num, 2);
                    assert_eq!(*inner, Object::Null);
                }
                other => panic!(
                    "Input: {:?} Expected Object::Object(5, 2, Null), got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
        {
            // Boolean 对象
            let input = b"10 0 obj\ntrue\nendobj";
            let mut t = Tokenizer::new(input);
            match t.parse_direct_obj() {
                Object::Object(obj_num, gen_num, inner) => {
                    assert_eq!(obj_num, 10);
                    assert_eq!(gen_num, 0);
                    assert_eq!(*inner, Object::Boolean(true));
                }
                other => panic!(
                    "Input: {:?} Expected Object::Object(10, 0, Boolean(true)), got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
        {
            // Real 对象
            let input = b"15 0 obj\n3.14\nendobj";
            let mut t = Tokenizer::new(input);
            match t.parse_direct_obj() {
                Object::Object(obj_num, gen_num, inner) => {
                    assert_eq!(obj_num, 15);
                    assert_eq!(gen_num, 0);
                    match *inner {
                        Object::Real(v) => {
                            assert!((v - 3.14).abs() < 0.001, "Expected 3.14, got {}", v);
                        }
                        other => panic!("Expected Real, got {:?}", other),
                    }
                }
                other => panic!(
                    "Input: {:?} Expected Object::Object(15, 0, Real), got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
        {
            // Name 对象
            let input = b"20 0 obj\n/SomeName\nendobj";
            let mut t = Tokenizer::new(input);
            match t.parse_direct_obj() {
                Object::Object(obj_num, gen_num, inner) => {
                    assert_eq!(obj_num, 20);
                    assert_eq!(gen_num, 0);
                    assert_eq!(*inner, Object::Name(b"SomeName".to_vec()));
                }
                other => panic!(
                    "Input: {:?} Expected Object::Object(20, 0, Name), got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
        {
            // 十六进制字符串对象
            let input = b"25 0 obj\n<48656C6C6F>\nendobj";
            let mut t = Tokenizer::new(input);
            match t.parse_direct_obj() {
                Object::Object(obj_num, gen_num, inner) => {
                    assert_eq!(obj_num, 25);
                    assert_eq!(gen_num, 0);
                    assert_eq!(*inner, Object::String(b"Hello".to_vec()));
                }
                other => panic!(
                    "Input: {:?} Expected Object::Object(25, 0, String(Hello)), got {:?}",
                    String::from_utf8_lossy(input),
                    other
                ),
            }
        }
    }

    #[test]
    fn parse_indirect_ref_fallback_to_number() {
        // ========== 无法解析为间接引用时，回退到数字解析 ==========
        // 这些输入以数字开头，但不是有效的间接引用，应回退解析为数字
        
        {
            // 单个数字
            let input = b"123";
            let mut t = Tokenizer::new(input);
            match t.parse_indirect_ref() {
                Object::Integer(123) => {}
                other => panic!("Input: {:?} Expected Integer(123), got {:?}", String::from_utf8_lossy(input), other),
            }
        }
        {
            // 两个数字后没有 R 或 obj
            let input = b"1 0";
            let mut t = Tokenizer::new(input);
            match t.parse_indirect_ref() {
                Object::Integer(1) => {}
                other => panic!("Input: {:?} Expected Integer(1), got {:?}", String::from_utf8_lossy(input), other),
            }
        }
        {
            // 实数
            let input = b"3.14";
            let mut t = Tokenizer::new(input);
            match t.parse_indirect_ref() {
                Object::Real(v) => {
                    assert!((v - 3.14).abs() < 0.001);
                }
                other => panic!("Input: {:?} Expected Real, got {:?}", String::from_utf8_lossy(input), other),
            }
        }
        {
            // 两个数字后跟其他字符
            let input = b"1 0 X";
            let mut t = Tokenizer::new(input);
            match t.parse_indirect_ref() {
                Object::Integer(1) => {}
                other => panic!("Input: {:?} Expected Integer(1), got {:?}", String::from_utf8_lossy(input), other),
            }
        }
        {
            // 负数（不可能是间接引用，直接解析为数字）
            let input = b"-5";
            let mut t = Tokenizer::new(input);
            match t.parse_indirect_ref() {
                Object::Integer(-5) => {}
                other => panic!("Input: {:?} Expected Integer(-5), got {:?}", String::from_utf8_lossy(input), other),
            }
        }
    }

    #[test]
    fn parse_indirect_ref_edge_cases() {
        // ========== 间接引用边界情况 ==========
        
        {
            // 间接引用后有其他内容
            let input = b"1 0 R /Name";
            let mut t = Tokenizer::new(input);
            match t.parse_indirect_ref() {
                Object::IndirectRef(1, 0) => {}
                other => panic!("Input: {:?} Expected IndirectRef(1, 0), got {:?}", String::from_utf8_lossy(input), other),
            }
        }
        {
            // 大的对象号和 generation number
            let input = b"65535 65535 R";
            let mut t = Tokenizer::new(input);
            match t.parse_indirect_ref() {
                Object::IndirectRef(65535, 65535) => {}
                other => panic!("Input: {:?} Expected IndirectRef(65535, 65535), got {:?}", String::from_utf8_lossy(input), other),
            }
        }
        {
            // obj 后有空白字符
            let input = b"1 0 obj\n123\nendobj";
            let mut t = Tokenizer::new(input);
            match t.parse_direct_obj() {
                Object::Object(1, 0, inner) => {
                    assert_eq!(*inner, Object::Integer(123));
                }
                other => panic!("Input: {:?} Expected Object::Object, got {:?}", String::from_utf8_lossy(input), other),
            }
        }
        {
            // obj 和对象内容紧邻（无空白）
            let input = b"1 0 obj(hello)endobj";
            let mut t = Tokenizer::new(input);
            match t.parse_direct_obj() {
                Object::Object(1, 0, inner) => {
                    assert_eq!(*inner, Object::String(b"hello".to_vec()));
                }
                other => panic!("Input: {:?} Expected Object::Object, got {:?}", String::from_utf8_lossy(input), other),
            }
        }
    }
}
