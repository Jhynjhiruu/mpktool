use std::collections::HashMap;

use anyhow::{anyhow, Result};
pub trait Decoder<T> {
    fn consume(&mut self, char: &T) -> Result<Vec<char>>;
    fn flush(&self) -> Vec<char>;
}

pub trait Encoder<T> {
    fn consume(&mut self, c: char) -> Result<Vec<T>>;
    fn flush(&self) -> Vec<T>;
}

pub trait DecoderTable {
    const TABLE: [char; 256];
}

pub fn decode<T: Decoder<U>, U>(string: &[U], mut decoder: T) -> Result<String> {
    let mut rv = String::new();
    for i in string {
        match decoder.consume(i) {
            Ok(x) => rv.extend(x),
            Err(e) => return Err(e),
        }
    }
    rv += &String::from_iter(decoder.flush());

    Ok(rv)
}

pub fn encode<T: Encoder<U>, U>(string: &str, mut encoder: T) -> Result<Vec<U>> {
    let mut rv = vec![];

    for i in string.chars() {
        match encoder.consume(i) {
            Ok(x) => rv.extend(x),
            Err(e) => return Err(e),
        }
    }

    rv.extend(encoder.flush());

    Ok(rv)
}

impl<T: DecoderTable, U: Copy + std::convert::Into<usize>> Decoder<U> for T {
    fn consume(&mut self, char: &U) -> Result<Vec<char>> {
        Ok(match Self::TABLE.get::<usize>((*char).into()) {
            Some(&c) => vec![c],
            None => vec![],
        })
    }

    fn flush(&self) -> Vec<char> {
        vec![]
    }
}

impl<T: DecoderTable> Encoder<u8> for T {
    fn consume(&mut self, c: char) -> Result<Vec<u8>> {
        Self::TABLE
            .iter()
            .enumerate()
            .find_map(|(index, &ch)| {
                if ch == c {
                    Some(vec![index as u8])
                } else {
                    None
                }
            })
            .ok_or(anyhow!("failed to encode char {c}"))
    }

    fn flush(&self) -> Vec<u8> {
        vec![]
    }
}
