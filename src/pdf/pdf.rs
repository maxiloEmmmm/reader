use std::{fs::{File, OpenOptions}, io::{Cursor, Error as stdIOErr, Read, Seek}};

use crate::pdf::tokenizer::{Source, Tokenizer};

use thiserror::Error;

const TRAILER_TOKEN: &str = "trailer";

#[derive(Error, Debug)]
pub enum Error {
    #[error("invalid resource {0}")]
    Invalid(String),
    #[error("handle io")]
    IO(#[from] stdIOErr)
}


pub struct Pdf<T: Source> {
    token: Tokenizer<T>,
    start_xref: usize,
}

impl<T: Source> Pdf<T> {
    fn new(r: T) -> Result<Self, Error> {
        let mut token = Tokenizer::new(r);

        token.seek(std::io::SeekFrom::End(1024))?;
        loop {
            token.skip_whitespace_and_comments();
            if token.peek_bytes(n)? != TRAILER_TOKEN {
                
            }
        }
        
        
        Ok(Self {
             token: token,
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



