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

#[derive(PartialEq)]
#[derive(Debug)]
pub enum Object {
    Boolean(bool),
    Integer(i64),
    Real(f64),
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
                },
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
        if self.pos >= self.data.len() {
            return None;
        }
        Some(self.data[self.pos])
    }

    pub fn parse_object(&mut self) -> Option<Object> {
        loop {
            match self.data[self.pos] {
                b'+' | b'-' | b'0'..=b'9' | b'.' => {
                    return self.parse_number();
                }
                _ => {
                    return None;
                }
            }
        }
    }

    pub fn parse_number(&mut self) -> Option<Object> {
        let mut has_point = false;
        let mut pos = 0;
        loop {
            let Some(b) = self.peek_byte() else {
                break;
            };
            match b {
                b'.' => {
                    if has_point {
                        return None;
                    }
                    has_point = true;
                }
                b'0'..=b'9' => {}
                b'+' => {
                    if pos != 0 {
                        return None;
                    }
                }
                b'-' => {
                    if pos != 0 {
                        return None;
                    }
                }
                other => {
                    if WhiteSpace::from_repr(other).is_none() {
                        return None;
                    }
                    self.pos += 1;
                    break;
                }
            }
            self.pos += 1;
            pos += 1;
        }

            println!("{:?}", str::from_utf8(self.peek_back(pos)));
        let Ok(s) = str::from_utf8(self.peek_back(pos)) else {
            return None;
        };
        if has_point {
            s.parse::<f64>().ok().map(|v| Object::Real(v)).or(None)
        }else {
            s.parse::<i64>().ok().map(|v| Object::Integer(v)).or(None)
        }
    }

    pub fn current(&self) -> u8 {
        self.data[self.pos]
    }

    pub fn peek_back(&self, n: usize) -> &[u8] {
        &self.data[self.pos-n..self.pos]
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
        println!("{}", (tokenizer.peek_byte().unwrap() as char).to_string());
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
            assert_eq!(&result.unwrap(), expected);
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
            let result = tokenizer.parse_number().unwrap();
            if let Object::Real(val) = result {
                assert!((val - expected_val).abs() < f64::EPSILON * 10.0);
            } else {
                panic!("Expected Real");
            }
        }
    }

    #[test]
    fn parse_edge_cases() {
        assert_eq!(Tokenizer::new(b"5").parse_number().unwrap(), Object::Integer(5));
        assert_eq!(Tokenizer::new(b"0").parse_number().unwrap(), Object::Integer(0));
        assert_eq!(Tokenizer::new(b"0.").parse_number().unwrap(), Object::Real(0.0));
        assert_eq!(Tokenizer::new(b".0").parse_number().unwrap(), Object::Real(0.0));
    }

    #[test]
    fn parse_invalid_numbers() {
        let truly_invalid: &[&[u8]] = &[
            b"",
            b".",
            b"+",
            b"-",
            b"++1",
            b"--5",
            b"1-2",
            b"1.2.3",
            b"1e5",
            b"6.02E23",
            b"12a",
            b"a12",
        ];

        for &input in truly_invalid {
            let mut tokenizer = Tokenizer::new(input);
            let result = tokenizer.parse_number();
            assert!(result.is_none());
        }
    }

    #[test]
    fn parse_literal_strings_pdf17() {
        let cases: &[(&[u8], &[u8])] = &[
            // 基础
            (b"(Hello)", b"Hello"),
            (b"()", b""),

            // 配对括号
            (b"(a(b)c)", b"a(b)c"),
            (b"( ( nested ) )", b" ( nested ) "),

            // 行尾无 \ → 转为 \n
            (b"(line1\nline2)", b"line1\nline2"),
            (b"(line1\rline2)", b"line1\nline2"),
            (b"(line1\r\nline2)", b"line1\nline2"),

            // 转义序列
            (b"(a\\nb)", b"a\nb"),
            (b"(a\\rb)", b"a\rb"),
            (b"(a\\tb)", b"a\tb"),
            (b"(a\\bb)", b"a\x08b"),   // \b → 0x08
            (b"(a\\fb)", b"a\x0Cb"),   // \f → 0x0C
            (b"(a\\\\b)", b"a\\b"),
            (b"(a\\(b\\))", b"a(b)"),

            // 八进制
            (b"(\\053)", b"+"),
            (b"(\\53)", b"+"),
            (b"(\\0053)", b"\x05\x33"),

            // 折行（保留空格）
            (b"(These \\ntwo)", b"These two"),
            (b"(Start\\n   end)", b"Start   end"),
            (b"(A\\rB)", b"AB"),
            (b"(X\\r\nY)", b"XY"),

            // 无效转义 → 忽略 \
            (b"(\\x)", b"x"),
            (b"(\\%)", b"%"),
            (b"(\\z)", b"z"),
        ];

        for &(input, expected) in cases {
            let mut tokenizer = Tokenizer::new(input);
            let obj = tokenizer.parse_string().unwrap_or_else(|e| {
                panic!("Failed to parse {:?}: {}", String::from_utf8_lossy(input), e);
            });
            assert_eq!(obj.as_bytes(), expected, "Input: {:?}", String::from_utf8_lossy(input));
        }
    }

    #[test]
    fn parse_hex_strings_pdf17() {
        let cases: &[(&[u8], &[u8])] = &[
            (b"<48656C6C6F>", b"Hello"),
            (b"<68656c6c6f>", b"hello"),
            // 忽略空白：space, \t, \n, \r, \x0C
            (b"<48 65\t6C\n6C\r6F\x0C>", b"Hello"),
            // 奇数位补 0
            (b"<A>", b"\xA0"),
            (b"<901FA>", b"\x90\x1F\xA0"),
            (b"<901FA3>", b"\x90\x1F\xA3"),
            (b"<>", b""),
        ];

        for &(input, expected) in cases {
            let mut tokenizer = Tokenizer::new(input);
            let obj = tokenizer.parse_string().unwrap_or_else(|e| {
                panic!("Failed to parse hex {:?}: {}", String::from_utf8_lossy(input), e);
            });
            assert_eq!(obj.as_bytes(), expected, "Hex input: {:?}", String::from_utf8_lossy(input));
        }
    }

    #[test]
    fn parse_invalid_strings() {
        let invalid_inputs: &[&[u8]] = &[
            b"(",
            b"(unmatched",
            b"(a(b)c",
            b")",
            b"(a\\",
            b"<",
            b"<G>",
            b"<123G>",
            b"<12 3>",
        ];

        for &input in invalid_inputs {
            let mut tokenizer = Tokenizer::new(input);
            assert!(
                tokenizer.parse_string().is_err(),
                "Should fail on input: {:?}",
                String::from_utf8_lossy(input)
            );
        }
    }
}
