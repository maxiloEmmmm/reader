use std::{
    collections::HashMap, fs::{File, OpenOptions}, io::{self, Cursor, Error as stdIOErr, Read, Seek}
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

pub struct Catalog {
    pub page: Pages,
}


pub struct Pages {
    pub count: usize,
    pub pages: Vec<i64>
}


pub struct Pdf<T: Source> {
    token: Tokenizer<T>,
    root: i64,
    // size: i64,
    // root: Object,
    xref: HashMap<i64, u64>,
    catalog: Catalog,
}

impl<T: Source> Pdf<T> {
    fn new(r: T) -> Result<Self, Error> {
        let mut token = Tokenizer::new(r);

        let mut start_xref;
        token.set_rev(true)?;
        token.skip_whitespace()?;
        let Some(eof) = token.read_bytes(5)? else {
            return Err(Error::Invalid("too short for end".to_owned()))
        };

        if eof.as_ref() != b"%%EOF" {
            return Err(Error::Invalid("invalid pdf for end".to_owned()))
        }
        token.skip_whitespace_and_comments()?;

        start_xref = match token.parse_number() {
            Object::Integer(n) => n,
            other => return Err(Error::Invalid(format!("invalid {:?} for start_xref", other)))
        };
        token.skip_whitespace_and_comments()?;
        let Some(v) = token.read_bytes(START_XREF.len())? else {
            return Err(Error::Invalid("too short to find trailer".to_owned()))
        };
        if v.as_ref() != START_XREF {
            return Err(Error::Invalid(format!("invalid start xref tag {:?}", String::from_utf8(v.to_vec()))))
        }
        token.set_rev(false)?;
        let mut ref_hash = HashMap::<i64, u64>::new();
        let mut root = None;
        loop {
            token.seek(io::SeekFrom::Start(start_xref as u64))?;
            let Some(xref) = token.read_bytes(4)? else {
                return Err(Error::Invalid(format!("incomplete find xref on {}", start_xref)));
            };            
            if xref.as_ref() != b"xref" {
                return Err(Error::Invalid(format!("pos not xref, is {:?}", String::from_utf8(xref.to_vec()))))
            }
            token.skip_whitespace_and_comments()?;
            
            let mut index = match token.parse_number() {
                Object::Integer(n) => n,
                other => return Err(Error::Invalid(format!("base index not number is {:?}", other))),
            };
            token.skip_whitespace_and_comments()?;
            
            let mut sum = match token.parse_number() {
                Object::Integer(n) => n,
                other => return Err(Error::Invalid(format!("sum not number is {:?}", other))),
            };

            loop {
                token.skip_whitespace_and_comments()?;
                let pos = match token.parse_number() {
                    Object::Integer(n) => n,
                    other => return Err(Error::Invalid(format!("pos not number is {:?}", other))),
                };
                token.skip_whitespace_and_comments()?;
                match token.parse_number() {
                    Object::Integer(_) => {},
                    other => return Err(Error::Invalid(format!("pos not number is {:?}", other))),
                }
                token.skip_whitespace_and_comments()?;
                let Some(typ) = token.read_byte()? else {
                    return Err(Error::Invalid(format!("incomplete find {} ref type {}", index+1, token.left_pos())))
                };

                if typ == b'n' {
                    if !ref_hash.contains_key(&index) {
                        ref_hash.insert(index, pos as u64);
                    }
                }
                index += 1;
                sum -= 1;
                if sum == 0 {
                    break
                }
            }
            token.skip_whitespace_and_comments()?;
            let Some(trailer) = token.read_bytes(TRAILER_TOKEN.len())? else {
                return Err(Error::Invalid(format!("incomplete find trailer on {}", start_xref)))
            };
            if trailer.as_ref() != TRAILER_TOKEN {
                return Err(Error::Invalid(format!("pos not trailer, is {:?}", String::from_utf8(trailer.to_vec()))))
            }
            token.skip_whitespace_and_comments()?;
            let dic = match token.parse_dictionary() {
                Object::Dictionary(n) => n,
                other => return Err(Error::Invalid(format!("pos not dic is {:?}", other))),
            };
            let mut prev = None;
            for (k, v) in dic {
                match k.as_slice() {
                    b"Root" => {
                        match v {
                            Object::IndirectRef(pos, _) => {
                                if root.is_none() {
                                    root.replace(pos);
                                }
                            },
                            _ => {
                                return Err(Error::Invalid(format!("root want ref not {:?}", v)))
                            }
                        }

                    }
                    b"Prev" => {
                        match v {
                            Object::Integer(n) => {
                                prev.replace(n);
                            }
                            _ => {
                                return Err(Error::Invalid(format!("prev want i64 not {:?}", v)))
                            }
                        }
                    }
                    _ => {}
                }
            }

            match prev {
                Some(n) => {
                    start_xref = n;
                }
                None => break
            }
        }

        let Some(root) = root else {
            return Err(Error::Invalid("no root".to_owned()))
        };

        token.seek(io::SeekFrom::Start(*ref_hash.get(&root).unwrap()))?;
        let mut catalog = Catalog{
            page: Pages{
                count: 0,
                pages: vec![],
            }
        };
        match token.parse_direct_obj() {
            Object::Object(_, _, n) => {
                match n.as_ref() {
                    Object::Dictionary(dic) => {
                        for (k, v) in dic {
                            match k.as_slice() {
                                b"Type" => {
                                    match v {
                                        Object::Name(n) => {
                                            if n != b"Catalog" {
                                                return Err(Error::Invalid(format!("root type not catalog is {:?}", String::from_utf8(n.to_vec()))))
                                            }
                                        },
                                        _ => return Err(Error::Invalid(format!("root want obj not {:?}", v)))
                                    }
                                }
                                b"Pages" => {
                                    match v {
                                        Object::IndirectRef(pos, _) => {
                                            token.seek(io::SeekFrom::Start(*ref_hash.get(&pos).unwrap()))?;
                                            match token.parse_direct_obj() {
                                                Object::Object(_, _, obj) => {
                                                    match obj.as_ref() {
                                                        Object::Dictionary(dic) => {
                                                            for (k, v) in dic {
                                                                match k.as_slice() {
                                                                    b"Count" => {
                                                                        match v {
                                                                            Object::Integer(n) => catalog.page.count = *n as usize,
                                                                            other => return Err(Error::Invalid(format!("page.count want i64 not {:?}", other)))
                                                                        }
                                                                    },
                                                                    b"Kids" => {
                                                                        match v {
                                                                            Object::Array(vs) => {
                                                                                for vv in vs {
                                                                                    match vv {
                                                                                        Object::IndirectRef(pos, _) => {
                                                                                            catalog.page.pages.push(*pos);
                                                                                        },
                                                                                        other => return Err(Error::Invalid(format!("page.kid item want refnot {:?}", other)))
                                                                                    }
                                                                                }
                                                                            },
                                                                            other => return Err(Error::Invalid(format!("page.kids want array not {:?}", other)))
                                                                        }
                                                                    },
                                                                    _ => {}
                                                                }
                                                            }
                                                        },
                                                        other => return Err(Error::Invalid(format!("page obj want dic not {:?}", other)))
                                                    }
                                                },
                                                
                                                other => return Err(Error::Invalid(format!("pages want obj not {:?}", other)))
                                            }
                                        },
                                        _ => return Err(Error::Invalid(format!("pages want ref not {:?}", v)))
                                    }
                                }
                                _ => {}
                            }
                        }   
                    },
                    _ => {
                        return Err(Error::Invalid(format!("root obj want dic not {:?}", n)))
                    }
                }
            },
            other => {
                return Err(Error::Invalid(format!("root want obj not {:?}", other)))
            }
        };

        Ok(Self {
            token: token,
            xref: ref_hash,
            root: root,
            catalog: catalog,
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


}
