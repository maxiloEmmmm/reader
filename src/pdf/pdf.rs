use std::{
    fs::{File, OpenOptions},
    io::{self, Cursor, Error as stdIOErr, Read, Seek},
};

use crate::pdf::tokenizer::{Object, Source, Tokenizer};

use thiserror::Error;

const TRAILER_TOKEN: &[u8] = b"trailer";
const START_XREF: &[u8] = b"startxref";

#[derive(Error, Debug)]
pub enum Error {
    #[error("invalid resource {0}")]
    Invalid(String),
    #[error("handle io")]
    IO(#[from] stdIOErr),
}

pub struct Pdf<T: Source> {
    token: Tokenizer<T>,
    start_xref: usize,
    size: i64,
    root: Object,
}

impl<T: Source> Pdf<T> {
    fn new(r: T) -> Result<Self, Error> {
        let mut token = Tokenizer::new(r);

        let mut root = None;
        let mut size = None;
        let mut start_xref;
        token.seek(std::io::SeekFrom::End(0))?;
        token.seek(std::io::SeekFrom::End(if token.end_pos() >= 1024 { -1024 } else { -(token.end_pos() as i64) } ))?;
        loop {
            token.skip_whitespace_and_comments()?;
            let Some(v) = token.read_bytes(TRAILER_TOKEN.len())? else {
                return Err(Error::Invalid("too short to find trailer".to_owned()))
            };
            if v == TRAILER_TOKEN {
                token.skip_whitespace_and_comments()?;
                match token.parse_dictionary() {
                    Object::Dictionary(dic) => {
                        for (key, item) in dic {
                            match key.as_slice() {
                                b"Size" => {
                                    if let Object::Integer(n) = item {
                                        size = Some(n);
                                    }
                                }
                                b"Root" => {
                                    if let Object::Dictionary(_) = item {
                                        root = Some(item);
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    _ => {}
                }
                if root.is_some() && size.is_some() {
                    token.skip_whitespace_and_comments()?;
                    if let Some(v) = token.read_bytes(9)? {
                        if v == START_XREF {
                            token.skip_whitespace_and_comments()?;
                            match token.parse_number() {
                                Object::Integer(n) => {
                                    start_xref = n as usize;
                                    token.skip_whitespace()?;
                                    if let Some(v) = token.read_bytes(5)? {
                                        if v.as_ref() == b"%%EOF" {
                                            token.skip_whitespace_and_comments()?;
                                            if !token.ensure(1)? {
                                                break
                                            }
                                        }
                                    }
                                },
                                _ => {}
                            }
                        }
                    }
                }
            }
            token.skip_whitespace_and_comments()?;
        }

        Ok(Self {
            token: token,
            size: size.unwrap(),
            root: root.unwrap(),
            start_xref,
        })
    }
}

impl TryFrom<&str> for Pdf<File> {
    type Error = Error;
    fn try_from(src: &str) -> Result<Self, Error> {
        let fd = OpenOptions::new().read(true).open(src)?;
        Ok(Self::new(fd)?)
    }
}

impl TryFrom<Vec<u8>> for Pdf<Cursor<Vec<u8>>> {
    type Error = Error;
    fn try_from(src: Vec<u8>) -> Result<Self, Error> {
        Self::new(Cursor::new(src))
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use super::Pdf;

    #[test]
    fn new_valid_minimal_tail() {
        let v = b"trailer << /Size 1 /Root << >> >> startxref 0 %%EOF".to_vec();
        let p = Pdf::<Cursor<Vec<u8>>>::try_from(v).unwrap();
        assert_eq!(p.size, 1);
        assert_eq!(p.start_xref, 0);
    }

    #[test]
    fn new_valid_tail_with_whitespace_and_comments() {
        let v = b"  % comment\n trailer  << /Size 2 /Root << /Type /Catalog >> >>  \r\n startxref 100 \n %%EOF\n".to_vec();
        let p = Pdf::<Cursor<Vec<u8>>>::try_from(v).unwrap();
        assert_eq!(p.size, 2);
        assert_eq!(p.start_xref, 100);
    }

    #[test]
    fn new_valid_tail_startxref_large() {
        let v = b"trailer << /Size 3 /Root << >> >> startxref 200 %%EOF".to_vec();
        let p = Pdf::<Cursor<Vec<u8>>>::try_from(v).unwrap();
        assert_eq!(p.size, 3);
        assert_eq!(p.start_xref, 200);
    }

    #[test]
    fn new_invalid_too_short() {
        assert!(Pdf::<Cursor<Vec<u8>>>::try_from(b"trailer".to_vec()).is_err());
    }

    #[test]
    fn new_invalid_no_trailer() {
        assert!(Pdf::<Cursor<Vec<u8>>>::try_from(b"startxref 0 %%EOF".to_vec()).is_err());
    }

    #[test]
    fn new_invalid_trailer_dict_no_size() {
        assert!(Pdf::<Cursor<Vec<u8>>>::try_from(b"trailer << /Root << >> >> startxref 0 %%EOF".to_vec()).is_err());
    }

    #[test]
    fn new_invalid_trailer_dict_no_root() {
        assert!(Pdf::<Cursor<Vec<u8>>>::try_from(b"trailer << /Size 1 >> startxref 0 %%EOF".to_vec()).is_err());
    }

    #[test]
    fn new_invalid_no_startxref_after_trailer() {
        assert!(Pdf::<Cursor<Vec<u8>>>::try_from(b"trailer << /Size 1 /Root << >> >> %%EOF".to_vec()).is_err());
    }

    #[test]
    fn new_invalid_no_eof_after_startxref() {
        assert!(Pdf::<Cursor<Vec<u8>>>::try_from(b"trailer << /Size 1 /Root << >> >> startxref 0".to_vec()).is_err());
    }
}
