use std::{
    collections::HashMap, f32::MIN_POSITIVE, fs::{File, OpenOptions}, io::{self, Cursor, Error as stdIOErr, Read, Seek}
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
    pub pages: Vec<i64>,
}

pub struct Pdf<T: Source> {
    token: Tokenizer<T>,
    pub root: i64,
    // size: i64,
    // root: Object,
    pub xref: HashMap<i64, u64>,
    pub catalog: Catalog,
}

impl<T: Source> Pdf<T> {
    fn new(r: T) -> Result<Self, Error> {
        let mut token = Tokenizer::new(r);

        let mut start_xref;
        token.set_rev(true)?;
        token.skip_whitespace()?;
        let Some(eof) = token.read_bytes(5)? else {
            return Err(Error::Invalid("too short for end".to_owned()));
        };

        if eof.as_ref() != b"%%EOF" {
            return Err(Error::Invalid("invalid pdf for end".to_owned()));
        }
        token.skip_whitespace_and_comments()?;

        start_xref = match token.parse_number() {
            Object::Integer(n) => n,
            other => {
                return Err(Error::Invalid(format!(
                    "invalid {:?} for start_xref",
                    other
                )));
            }
        };
        token.skip_whitespace_and_comments()?;
        let Some(v) = token.read_bytes(START_XREF.len())? else {
            return Err(Error::Invalid("too short to find trailer".to_owned()));
        };
        if v.as_ref() != START_XREF {
            return Err(Error::Invalid(format!(
                "invalid start xref tag {:?}",
                String::from_utf8(v.to_vec())
            )));
        }
        token.set_rev(false)?;
        let mut ref_hash = HashMap::<i64, u64>::new();
        let mut root = None;
        loop {
            token.seek(io::SeekFrom::Start(start_xref as u64))?;
            let Some(xref) = token.read_bytes(4)? else {
                return Err(Error::Invalid(format!(
                    "incomplete find xref on {}",
                    start_xref
                )));
            };
            if xref.as_ref() != b"xref" {
                return Err(Error::Invalid(format!(
                    "pos not xref, is {:?}",
                    String::from_utf8(xref.to_vec())
                )));
            }
            token.skip_whitespace_and_comments()?;

            let mut index = Self::must_i64(&token.parse_number())?;
            token.skip_whitespace_and_comments()?;

            let mut sum= Self::must_i64(&token.parse_number())?;

            loop {
                token.skip_whitespace_and_comments()?;
                let mut pos = Self::must_i64(&token.parse_number())?;
                token.skip_whitespace_and_comments()?;
                Self::must_i64(&token.parse_number())?;
                token.skip_whitespace_and_comments()?;
                let Some(typ) = token.read_byte()? else {
                    return Err(Error::Invalid(format!(
                        "incomplete find {} ref type {}",
                        index + 1,
                        token.left_pos()
                    )));
                };

                if typ == b'n' {
                    if !ref_hash.contains_key(&index) {
                        ref_hash.insert(index, pos as u64);
                    }
                }
                index += 1;
                sum -= 1;
                if sum == 0 {
                    break;
                }
            }
            token.skip_whitespace_and_comments()?;
            let Some(trailer) = token.read_bytes(TRAILER_TOKEN.len())? else {
                return Err(Error::Invalid(format!(
                    "incomplete find trailer on {}",
                    start_xref
                )));
            };
            if trailer.as_ref() != TRAILER_TOKEN {
                return Err(Error::Invalid(format!(
                    "pos not trailer, is {:?}",
                    String::from_utf8(trailer.to_vec())
                )));
            }
            token.skip_whitespace_and_comments()?;
            let dic = token.parse_dictionary();
            let dic = Self::must_dic(&dic)?;
            let mut prev = None;
            for (k, v) in dic {
                match k.as_slice() {
                    b"Root" => {
                        let pos = Self::must_ref(v)?;    
                        if root.is_none() {
                            root.replace(pos);
                        }
                    }
                    b"Prev" => {
                        prev.replace(Self::must_i64(v)?);
                    }
                    _ => {}
                }
            }

            match prev {
                Some(n) => {
                    start_xref = n;
                }
                None => break,
            }
        }

        let Some(root) = root else {
            return Err(Error::Invalid("no root".to_owned()));
        };

        token.seek(io::SeekFrom::Start(*ref_hash.get(&root).unwrap()))?;
        let mut catalog = Catalog {
            page: Pages {
                count: 0,
                pages: vec![],
            },
        };
        let obj = token.parse_direct_obj();
        let dic = Self::must_dic(Self::must_obj(&obj)?)?;
        for (k, v) in dic {
            match k.as_slice() {
                b"Type" => {
                    Self::must_eq_name(v, b"Catalog")?;
                }
                b"Pages" => {
                    let pos = Self::must_ref(v)?;
                    token.seek(io::SeekFrom::Start(*ref_hash.get(&pos).unwrap()))?;
                    let obj = token.parse_direct_obj();
                    let dic = Self::must_dic(Self::must_obj(&obj)?)?;
                    for (k, v) in dic {
                        match k.as_slice() {
                            b"Count" => {
                                catalog.page.count = Self::must_i64(v)? as usize;
                            }
                            b"Kids" => {
                                for vv in Self::must_array(v)? {
                                    catalog.page.pages.push(Self::must_ref(vv)?);
                                }
                            }
                            _ => {}
                        }
                    }
                },
                _ => {}
            }
        }

        Ok(Self {
            token: token,
            xref: ref_hash,
            root: root,
            catalog: catalog,
        })
    }
    pub fn must_array(obj: &Object) -> Result<&[Object], Error> {
        match obj {
            Object::Array(obj) => Ok(obj),
            other => Err(Error::Invalid(format!("want Array, is {:?}", other)))
        }
        
    }
    pub fn must_i64(obj: &Object) -> Result<i64, Error> {
        match obj {
            Object::Integer(obj) => Ok(*obj),
            other => Err(Error::Invalid(format!("want i64, is {:?}", other)))
        }
    }

    pub fn must_dic(obj: &Object) -> Result<&[(Vec<u8>, Object)], Error> {
        match obj {
            Object::Dictionary(obj) => Ok(obj),
            other => Err(Error::Invalid(format!("want Dic, is {:?}", other)))
        }
    }
    
    pub fn must_obj(obj: &Object) -> Result<&Box<Object>, Error> {
        match obj {
            Object::Object(_, _, obj) => Ok(obj),
            other => Err(Error::Invalid(format!("want Obj, is {:?}", other)))
        }
    }

    pub fn must_ref(obj: &Object) -> Result<i64, Error> {
        match obj {
            Object::IndirectRef(pos, _) => Ok(*pos),
            other => Err(Error::Invalid(format!("want Ref, is {:?}", other)))
        }
    }
    
    pub fn eq_name(obj: &Object, n: &[u8]) -> Result<bool, Error> {
        match obj {
            Object::Name(v) => Ok(v.as_slice() == n),
            other => Err(Error::Invalid(format!("want Name, is {:?}", other)))
        }
    }

    pub fn must_eq_name(obj: &Object, n: &[u8]) -> Result<(), Error> {
        if !Self::eq_name(obj, n)? {
            return Err(Error::Invalid("no eq".to_owned()))
        }
        Ok(())
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
