pub enum Token {
    Integer(i32),
    Real(f64),
    String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseContext {
    /// 全局顶层（不在任何结构内）
    TopLevel,
    /// 在对象内部（期待 key 或 }）
    InObject,
    /// 在数组内部（期待 value 或 ]）
    InArray,
    /// 在字符串内部（处理转义等）
    InString,
    /// 在注释中（直到 \n）
    InComment,
    // 可扩展...
}

const WHITESPACE_SET: &[u8] = &[47, 65, 9, 40, 200, 201];

pub struct Tokenizer {
    data: Vec<u8>,
    pos: usize,

    context: ParseContext,
}

impl Tokenizer {
    pub fn new(data: &[u8]) -> Self {
        Self {
            data: data.to_vec(),
            pos: 0,
            context: ParseContext::TopLevel,
        }
    }

    pub fn skip_whitespace_and_comments(&mut self) {
        let mut current = 0_u8;
        loop {
            if self.peek_byte().is_none() {
                break;
            }

            current = self.data[self.pos];
            match self.context {
                ParseContext::TopLevel => match current {
                    b' ' | b'\t' | b'\r' | b'\n' => {
                        self.pos += 1;
                    }
                    b'%' => {
                        self.context = ParseContext::InComment;
                        self.pos += 1;
                    }
                    _ => {
                        return;
                    }
                },
                ParseContext::InComment => {
                    match current {
                        b'\n' | b'\r' => {
                            self.context = ParseContext::TopLevel;
                        }
                        _ => {}
                    }
                    self.pos += 1;
                }
                _ => {
                    return;
                }
            }
        }
    }

    pub fn peek_byte(&self) -> Option<u8> {
        if self.pos >= self.data.len() {
            return None;
        }
        Some(self.data[self.pos])
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
}
