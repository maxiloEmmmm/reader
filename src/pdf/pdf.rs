use std::{
    collections::{HashMap, HashSet}, f32::MIN_POSITIVE, fs::{File, OpenOptions}, io::{self, Cursor, Error as stdIOErr, Read, Seek}
};

use crate::pdf::tokenizer::{Error as TokenError, Object, Source, Tokenizer};

use thiserror::Error;

const TRAILER_TOKEN: &[u8] = b"trailer";
const START_XREF: &[u8] = b"startxref";
const CONTENTS_KEY: &[u8] = b"contents";

#[derive(Error, Debug)]
pub enum Error {
    #[error("invalid resource {0}")]
    Invalid(String),
    #[error("handle io")]
    IO(#[from] stdIOErr),
    #[error("object {0:?}")]
    Object(#[from] TokenError)
}

pub struct Catalog {
    pub page: Pages,
}

pub struct Pages {
    pub count: usize,
    pub index: Vec<u64>
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

            let mut index = Object::must_i64(&token.parse_number())?;
            token.skip_whitespace_and_comments()?;

            let mut sum= Object::must_i64(&token.parse_number())?;

            loop {
                token.skip_whitespace_and_comments()?;
                let mut pos = Object::must_i64(&token.parse_number())?;
                token.skip_whitespace_and_comments()?;
                Object::must_i64(&token.parse_number())?;
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
            let dic = Object::must_dic(&dic)?;
            let mut prev = None;
            for (k, v) in dic {
                match k.as_slice() {
                    b"Root" => {
                        let pos = Object::must_ref(v)?;    
                        if root.is_none() {
                            root.replace(pos);
                        }
                    }
                    b"Prev" => {
                        prev.replace(Object::must_i64(v)?);
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
                index: vec![],
            },
        };
        let obj = token.parse_direct_obj();
        let dic = Object::must_dic(Object::must_obj(&obj)?)?;
        for (k, v) in dic {
            match k.as_slice() {
                b"Type" => {
                    Object::must_eq_name(v, b"Catalog")?;
                }
                b"Pages" => {
                    catalog.page.count = Self::deep_page(&mut catalog.page.index, &mut ref_hash, &mut token, v)?;
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

    fn deep_page(index: &mut Vec<u64>, ref_hash: &mut HashMap<i64, u64>, token: &mut Tokenizer<T>, obj: &Object) -> Result<usize, Error> {
        let dic;
        let mut pos = 0;
        let mut inner_obj = None;
        match obj {
            Object::IndirectRef(v, _) => {
                token.seek(io::SeekFrom::Start(*ref_hash.get(&v).unwrap()))?;
                let obj = token.parse_direct_obj();
                inner_obj.replace(obj);
                let obj = Object::must_obj(inner_obj.as_ref().unwrap())?;
                dic = Object::must_dic(obj)?;
                pos = *v;
            },
            Object::Dictionary(v) => {
                dic = v;
            },
            _ => return Err(Error::Invalid(format!("not dic or obj is {:?}", obj)))
        }

        let mut count = 0;
        for (k, v) in dic {
            match k.as_slice() {
                b"Count" => {
                    count = Object::must_i64(v)? as usize;
                }
                b"Kids" => {
                    for vv in Object::must_array(v)? {
                        Self::deep_page(index, ref_hash, token, vv)?;
                    }
                }
                b"Type" => {
                    if Object::eq_name(v, b"Page")? {
                       index.push(pos as u64);
                    }
                }
                _ => {}
            }
        }
        Ok(count)
    }
    

    pub fn dump_ref(&mut self, xref: u64) -> Result<String, Error> {
        let pos = self.xref.get(&(xref as i64)).ok_or(Error::Invalid("unknown ref".to_owned()))?;
        self.token.seek(io::SeekFrom::Start(*pos))?;
        let object = self.token.parse_direct_obj();
        let object = Object::must_obj(&object)?;
        self.dump_obj(object, "")
    }


    pub fn dump_obj(&mut self, obj: &Object, prefix: &str) -> Result<String, Error> {
        self._dump_obj(&mut HashSet::new(), obj, prefix)
    }

    fn _dump_obj(&mut self, ref_set: &mut HashSet<i64>, obj: &Object, prefix: &str) -> Result<String, Error> {
        let mut ret = String::new();
        ret.push_str(prefix);
        match obj {
            Object::Boolean(n) => {
                if *n {
                    ret.push_str("true");
                }else {
                    ret.push_str("false");
                }
            },
            Object::Integer(n) => {
                ret.push_str(&n.to_string());
            },
            Object::Real(n) => {
                ret.push_str(&n.to_string());
            },
            Object::String(items) => {
                ret.push_str(str::from_utf8(items.as_slice()).unwrap_or(&format!("{:?}", items)));
            },
            Object::Invalid(n) => {
                ret.push_str(n.as_str());
            },
            Object::Name(items) => {
                ret.push_str("/");
                ret.push_str(str::from_utf8(items.as_slice()).unwrap_or(&format!("{:?}", items)));
            },
            Object::Null => {
                ret.push_str("null");
            },
            Object::Array(objects) => {
                ret.push_str("[\n");
                let sub_prefix = format!("  {}", prefix);
                for item in objects {
                    ret.push_str(self._dump_obj(ref_set, item, sub_prefix.as_str())?.as_str());
                    ret.push_str("\n");
                }
                ret.push_str(prefix);
                ret.push_str("]\n");
            },
            Object::Dictionary(items) => {
                ret.push_str("<<\n");
                let sub_prefix = format!("  {}", prefix);
                for (k, item) in items {
                    ret.push_str(sub_prefix.as_str());
                    ret.push_str(str::from_utf8(k.as_slice()).unwrap_or(&format!("{:?}", k)));
                    ret.push_str(": ");
                    ret.push_str(self._dump_obj(ref_set, item, sub_prefix.as_str())?.as_str());
                    ret.push_str("\n");
                }
                ret.push_str(prefix);
                ret.push_str(">>\n");
            },
            Object::Stream(stream) => {
                ret.push_str(&format!("stream[{}]", stream.length));
                // ret.push_str("  ");
                // ret.push_str(prefix);
                // ret.push_str(str::from_utf8(stream.data.as_slice()).unwrap_or(&format!("{:?}", stream.data)));
            },
            Object::IndirectRef(pos, _) => {
                if ref_set.contains(pos) {
                    ret.push_str("ref ");
                    ret.push_str(pos.to_string().as_str());
                    return Ok(ret)
                }
                ref_set.insert(*pos);
                self.token.seek(io::SeekFrom::Start(*self.xref.get(pos).ok_or(Error::Invalid("not found ref".to_owned()))?))?;
                let object = self.token.parse_direct_obj();
                let object = Object::must_obj(&object)?;
                ret.push_str(self._dump_obj(ref_set, object, &format!("  {}", prefix))?.as_str());
            },
            Object::Object(_, _, object) => {
                ret.push_str(self._dump_obj(ref_set, object, &format!("  {}", prefix))?.as_str());
            },
        }
        Ok(ret)
    }

    pub fn page(&mut self, n: u64) -> Result<Page, Error> {
        let xref = self.catalog.page.index.get(n as usize - 1).ok_or(Error::Invalid("out of max page".to_owned()))?;
        let obj = self.ref_object(*xref as i64)?;
        let cs = Object::dic_key(&obj, CONTENTS_KEY)?;
        let cs = Object::must_array(&cs)?;
        Ok(Page{
            content: cs.to_vec(),
        })
    }

    fn ref_object(&mut self, n: i64) -> Result<Object, Error> {
        self.seek_ref(n)?;
        self.token.parse_object().ok_or(Error::Invalid("no more".to_owned()))
    }

    fn resolve_ref_pos(&mut self, n: i64) -> Result<u64, Error> {
        self.xref.get(&n).map(|v| *v).ok_or(Error::Invalid("unknown ref".to_owned()))        
    }

    fn seek_ref(&mut self, n: i64) -> Result<(), Error> {
        let pos = self.resolve_ref_pos(n)?;
        self.token.seek(io::SeekFrom::Start(pos))?;
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


#[derive(Debug)]
pub struct Page {
    pub content: Vec<Object>
}
