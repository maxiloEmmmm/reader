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

#[derive(PartialEq, Debug)]
pub enum Object {
    Boolean(bool),
    Integer(i64),
    Real(f64),
    String(Vec<u8>),
    Invalid(String),
}

impl Object {
    pub fn unexpected(what: &str, pos: usize) -> Self {
        Self::Invalid(format!("unexpected to see {} at {}", what, pos))
    }

    pub fn incomplete(what: &str) -> Self {
        Self::Invalid(format!("{} incomplete", what))
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

    fn index_byte(&self, pos: usize) -> Option<u8> {
        if pos >= self.data.len() {
            return None;
        }
        Some(self.data[pos])
    }

    pub fn parse_object(&mut self) -> Option<Object> {
        let Some(next) = self.peek_byte() else {
            return None;
        };
        match next {
            b'+' | b'-' | b'0'..=b'9' | b'.' => {
                return Some(self.parse_number());
            }
            b'(' => {
                return Some(self.parse_string());
            }
            b'<' => {
                return Some(self.parse_hexadecimal_string());
            }
            _ => {
                return None;
            }
        }
    }

    pub fn parse_number(&mut self) -> Object {
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
                other => {
                    if WhiteSpace::from_repr(other).is_none() {
                        return Object::unexpected(&format!("{:x}", other), self.pos);
                    }
                    self.pos += 1;
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

            match next {
                b'<' => {
                    if pos != 0 {
                        return Object::unexpected("<", self.pos);
                    }
                }
                b'>' => {
                    if double {
                        println!("??? {}", base);
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
        let truly_invalid: &[&[u8]] = &[
            b"", b".", b"+", b"-", b"++1", b"--5", b"1-2", b"1.2.3", b"1e5", b"6.02E23", b"12a",
            b"a12",
        ];

        for &input in truly_invalid {
            let mut tokenizer = Tokenizer::new(input);
            let result = tokenizer.parse_number();
            match result {
                Object::Invalid(_) => {}
                other => {
                    panic!("not invalid {:?}", other)
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
            (b"(level1(level2(level3)level2)level1)", b"level1(level2(level3)level2)level1"),
            (b"(1(2(3(4)3)2)1)", b"1(2(3(4)3)2)1"),
            (b"(a(b(c(d(e)e)d)c)b)a)", b"a(b(c(d(e)e)d)c)b"),
            (b"( Strings may contain balanced parentheses ( ) and special characters )", 
             b" Strings may contain balanced parentheses ( ) and special characters "),
            
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
            (b"(\\400)", b"\x00"),  // 400 (256) mod 256 = 0
            (b"(\\777)", b"\xFF"),  // 777 (511) mod 256 = 255
            (b"(\\500)", b"\x40"),  // 500 (320) mod 256 = 64
            (b"(\\600)", b"\x80"),  // 600 (384) mod 256 = 128
            (b"(\\700)", b"\xC0"),  // 700 (448) mod 256 = 192
            // 八进制后跟数字字符（必须使用3位，前导0）
            (b"(\\0053)", b"\x05\x33"),  // \005 + '3'
            (b"(\\0534)", b"+4"),        // \053 + '4'
            (b"(\\1012)", b"A2"),        // \101 + '2'
            // 多个八进制
            (b"(\\053\\101)", b"+A"),
            (b"(\\053\\101\\102)", b"+AB"),
            // 示例：包含八进制字符的字符串
            (b"( This string contains \\245two octal characters\\307 . )", 
             b" This string contains \xA5two octal characters\xC7 . "),
            
            // ========== 行尾折行（\ 后跟换行符，两者都被忽略） ==========
            // 注意：在 Rust 字节字面量中，\\ 是反斜杠字节(0x5C)，\n 是换行符字节(0x0A)，\r 是回车符字节(0x0D)
            (b"(These \\\ntwo)", b"These two"),  // \ 后跟 LF
            (b"(Start\\\n   end)", b"Start   end"),  // \ 后跟 LF，保留后续空白
            (b"(A\\\rB)", b"AB"),  // \ 后跟 CR
            (b"(X\\\r\nY)", b"XY"),  // \ 后跟 CRLF
            (b"(line1\\\n   \tline2)", b"line1   \tline2"),  // \ 后跟 LF，保留后续空白
            (b"(text\\\n\t  \r\nmore)", b"text\t  \r\nmore"),  // \ 后跟 LF，保留后续空白
            // 示例：多行字符串
            (b"( These \\\ntwo strings \\\nare the same . )", 
             b" These two strings are the same . "),
            
            // ========== 未转义换行符处理 ==========
            // 未转义的换行符保持原样，不统一转为 \n
            (b"(line1\nline2)", b"line1\nline2"),  // LF 保持为 LF
            (b"(line1\rline2)", b"line1\rline2"),  // CR 保持为 CR，不转为 \n
            (b"(line1\r\nline2)", b"line1\r\nline2"),  // CRLF 保持为 CRLF，不转为 \n
            (b"(multi\nline\rtext\r\nhere)", b"multi\nline\rtext\r\nhere"),  // 保持原样
            // 示例
            (b"( This string has an end-of-line at the end of it .\n)", b" This string has an end-of-line at the end of it .\n"),
            
            // ========== 无效转义（\ 后跟不在表中的字符，\ 被忽略） ==========
            (b"(a\\ b)", b"a b"),  // \ 后跟空格，\ 被忽略
            (b"(a\\x)", b"ax"),    // \ 后跟 x，\ 被忽略
            (b"(a\\z)", b"az"),   // \ 后跟 z，\ 被忽略
            (b"(a\\%)", b"a%"),   // \ 后跟 %，\ 被忽略
            
            // ========== 特殊字符和边界情况 ==========
            // 包含 null 字节
            (b"(text\\000null)", b"text\x00null"),
            // 包含各种 ASCII 控制字符
            (b"(\\001\\002\\003)", b"\x01\x02\x03"),
            (b"(\\007\\010\\011)", b"\x07\x08\x09"),  // BEL, BS, TAB
            // 包含特殊字符的普通文本
            (b"(text with spaces)", b"text with spaces"),
            (b"( Strings may contain newlines\nand such . )", 
             b" Strings may contain newlines\nand such . "),
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
}
