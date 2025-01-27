use std::array;
use std::io::{Seek, SeekFrom, Write};
use std::num::Wrapping;
use std::str::FromStr;
use std::{fmt::Display, io::Cursor};

use anyhow::{anyhow, Result};
use binrw::{binrw, BinRead, BinResult, BinWrite, Error, VecArgs};
use encoding::{decode, encode, DecoderTable};
use hex::{FromHex, ToHex};
use rand::random;
use unfmt::unformat;

mod encoding;

const MAX_BANKS: u8 = (128 - 3) >> 1;

#[binrw]
#[derive(Debug, Clone, Copy)]
pub struct IDBlock {
    pub repaired: u32,
    pub random: u32,
    pub serial_mid: u64,
    pub serial_low: u64,
    pub deviceid: u16,
    pub banks: u8,
    pub version: u8,
    pub checksum: u16,
    pub inverted_checksum: u16,
}

impl IDBlock {
    pub fn sum(&self) -> u16 {
        let mut buf = [0; 32];
        let mut cursor = Cursor::new(buf.as_mut_slice());
        self.write_be(&mut cursor)
            .expect("should never fail to write IDBlock into buffer");

        let sum: Wrapping<_> = buf[..28]
            .chunks_exact(2)
            .map(|e| u16::from_be_bytes(e.try_into().unwrap()))
            .map(Wrapping)
            .sum();

        sum.0
    }

    pub fn check(&self) -> bool {
        !(self.sum() != self.checksum
            || 0xFFF2u16.wrapping_sub(self.sum()) != self.inverted_checksum)
    }
}

#[binrw]
#[derive(Debug)]
pub struct IDSector {
    pub label_area: [u8; 0x20],
    #[brw(pad_after(0x20))]
    pub id_block: IDBlock,
    pub id_block_backup: IDBlock,
    #[brw(pad_after(0x20))]
    pub id_block_backup_2: IDBlock,
    #[brw(pad_after(0x20))]
    pub id_block_backup_3: IDBlock,
}

impl IDSector {
    pub fn blocks(&self) -> [&IDBlock; 4] {
        [
            &self.id_block,
            &self.id_block_backup,
            &self.id_block_backup_2,
            &self.id_block_backup_3,
        ]
    }
}

#[binrw]
#[derive(Debug, Clone, Copy, Default)]
pub struct Inode {
    pub bank: u8,
    pub page: u8,
}

impl Inode {
    pub fn address(&self) -> usize {
        (self.bank as usize * 0x8000) | (self.page as usize * 0x100)
    }
}

impl From<&Inode> for u16 {
    fn from(val: &Inode) -> Self {
        ((val.bank as u16) << 8) | (val.page as u16)
    }
}

impl From<Inode> for u16 {
    fn from(val: Inode) -> Self {
        (&val).into()
    }
}

impl From<u16> for Inode {
    fn from(value: u16) -> Self {
        Self {
            bank: (value >> 8) as _,
            page: (value) as _,
        }
    }
}

#[binrw]
#[derive(Debug, Clone, Copy)]
pub struct InodeTable {
    #[brw(pad_before(1))]
    pub checksum: u8,
    pub inodes: [Inode; Self::NUM_INODES],
}

impl InodeTable {
    pub const NUM_INODES: usize = 128 - 1;

    pub fn sum(&self) -> u8 {
        let mut buf = [0; 256];
        let mut cursor = Cursor::new(buf.as_mut_slice());
        self.write_be(&mut cursor)
            .expect("should never fail to write InodeTable into buffer");

        let sum: Wrapping<_> = buf[2..].iter().copied().map(Wrapping).sum();

        sum.0
    }

    pub fn check(&self) -> bool {
        self.sum() == self.checksum
    }
}

#[binrw]
#[derive(Debug, Clone, Copy, Default)]
pub struct Note {
    pub game_code: u32,
    pub company_code: u16,
    pub start_page: Inode,
    pub status: u8,
    pub reserved: i8,
    pub data_sum: u16,
    pub ext_name: [u8; 4],
    pub game_name: [u8; 16],
}

impl Display for Note {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let game_code = {
            let bytes = self.game_code.to_be_bytes();
            if bytes.is_ascii() {
                String::from_utf8_lossy(&bytes).to_string()
            } else {
                format!("{:08X}", self.game_code)
            }
        };

        let company_code = {
            let bytes = self.company_code.to_be_bytes();
            if bytes.is_ascii() {
                String::from_utf8_lossy(&bytes).to_string()
            } else {
                format!("{:04X}", self.company_code)
            }
        };

        let name = match decode(&self.game_name, N64FontCode) {
            Ok(s) => s.trim_end_matches('\0').to_string(),
            Err(_) => format!("&{}", self.game_name.encode_hex_upper::<String>()),
        };

        let extension = match decode(&self.ext_name, N64FontCode) {
            Ok(s) => s.trim_end_matches('\0').to_string(),
            Err(_) => format!("&{}", self.ext_name.encode_hex_upper::<String>()),
        };

        write!(f, "£{}£{}£{}£{}£", game_code, company_code, name, extension)
    }
}

impl Note {
    pub fn ext(&self) -> String {
        decode(&self.ext_name, N64FontCode).unwrap()
    }

    pub fn name(&self) -> String {
        decode(&self.game_name, N64FontCode).unwrap()
    }
}

impl FromStr for Note {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let (game_code, company_code, name, extension) =
            unformat!("£{}£{}£{}£{}£", s).ok_or(anyhow!("failed to decode note from string"))?;

        let game_code = u32::from_be_bytes(match game_code.len() {
            4 => game_code.as_bytes().try_into()?,
            8 => <[u8; 4]>::from_hex(game_code)?,
            _ => return Err(anyhow!("invalid game code length")),
        });

        let company_code = u16::from_be_bytes(match company_code.len() {
            2 => company_code.as_bytes().try_into()?,
            4 => <[u8; 2]>::from_hex(company_code)?,
            _ => return Err(anyhow!("invalid company code length")),
        });

        let name = match name.strip_prefix('&') {
            Some(s) => <[u8; 16]>::from_hex(s)?,
            None => encode(&format!("{:\0<16}", name), N64FontCode)?
                .try_into()
                .map_err(|_| anyhow!("failed to encode note name"))?,
        };

        let ext = match extension.strip_prefix('&') {
            Some(s) => <[u8; 4]>::from_hex(s)?,
            None => encode(&format!("{:\0<4}", extension), N64FontCode)?
                .try_into()
                .map_err(|_| anyhow!("failed to encode note extension"))?,
        };

        Ok(Self {
            game_code,
            company_code,
            start_page: Inode { bank: 0, page: 0 },
            status: 0x02,
            reserved: 0,
            data_sum: 0,
            ext_name: ext,
            game_name: name,
        })
    }
}

#[binrw]
#[derive(Debug)]
pub struct NoteTable {
    pub notes: [Note; Self::NUM_NOTES],
}

impl NoteTable {
    pub const NUM_NOTES: usize = 16;
}

pub struct N64FontCode;

impl DecoderTable for N64FontCode {
    const TABLE: [char; 256] = [
        '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', // 0x00
        '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', ' ', // 0x08
        '0', '1', '2', '3', '4', '5', '6', '7', // 0x10
        '8', '9', 'A', 'B', 'C', 'D', 'E', 'F', // 0x18
        'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', // 0x20
        'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', // 0x28
        'W', 'X', 'Y', 'Z', '!', '"', '#', '\'', // 0x30
        '*', '+', ',', '-', '.', '/', ':', '=', // 0x38
        '?', '@', '。', '゛', '゜', 'ァ', 'ィ', 'ゥ', // 0x40
        'ェ', 'ォ', 'ッ', 'ャ', 'ュ', 'ョ', 'ヲ', 'ン', // 0x48
        'ア', 'イ', 'ウ', 'エ', 'オ', 'カ', 'キ', 'ク', // 0x50
        'ケ', 'コ', 'サ', 'シ', 'ス', 'セ', 'ソ', 'タ', // 0x58
        'チ', 'ツ', 'テ', 'ト', 'ナ', 'ニ', 'ヌ', 'ネ', // 0x60
        'ノ', 'ハ', 'ヒ', 'フ', 'ヘ', 'ホ', 'マ', 'ミ', // 0x68
        'ム', 'メ', 'モ', 'ヤ', 'ユ', 'ヨ', 'ラ', 'リ', // 0x70
        'ル', 'レ', 'ロ', 'ワ', 'ガ', 'ギ', 'グ', 'ゲ', // 0x78
        'ゴ', 'ザ', 'ジ', 'ズ', 'ゼ', 'ゾ', 'ダ', 'ヂ', // 0x80
        'ヅ', 'デ', 'ド', 'バ', 'ビ', 'ブ', 'ベ', 'ボ', // 0x88
        'パ', 'ピ', 'プ', 'ペ', 'ポ', '\x00', '\x00', '\x00', //0x90
        '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', // 0x98
        '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', // 0xA0
        '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', // 0xA8
        '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', // 0xB0
        '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', // 0xB8
        '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', // 0xC0
        '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', // 0xC8
        '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', // 0xD0
        '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', // 0xD8
        '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', // 0xE0
        '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', // 0xE8
        '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', // 0xF0
        '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', '\x00', // 0xF8
    ];
}

pub fn extract(file: &[u8]) -> BinResult<Vec<(Note, Vec<&[u8]>)>> {
    let mut rv = vec![];

    let mut cursor = Cursor::new(&file);

    let id = IDSector::read_be(&mut cursor)?;

    let blocks = id.blocks();
    let block = blocks
        .iter()
        .find(|id| id.check())
        .ok_or(Error::AssertFail {
            pos: 0,
            message: "No ID block has a valid checksum".into(),
        })?;

    if block.banks > MAX_BANKS {
        return Err(Error::AssertFail {
            pos: 0,
            message: format!(
                "specified number of banks ({}) exceeds maximum ({MAX_BANKS})",
                block.banks
            ),
        });
    }

    let inodes = <[Vec<InodeTable>; 2]>::read_be_args(
        &mut cursor,
        VecArgs::builder()
            .count(block.banks.into())
            .inner(())
            .finalize(),
    )?;

    let inode_table = inodes
        .iter()
        .find(|tab| tab.iter().all(|inode| inode.check()))
        .map(|tab| tab.as_slice())
        .ok_or(Error::AssertFail {
            pos: 0,
            message: "No inode table has a complete set of valid checksums".into(),
        })?;

    let tab = NoteTable::read_be(&mut cursor)?;

    let notes = tab
        .notes
        .into_iter()
        .filter(|n| n.status == 0x02)
        .collect::<Vec<_>>();

    for note in notes {
        let mut pages = vec![];

        let mut node = &note.start_page;
        while u16::from(node) != 0x0001 {
            let address = node.address();
            pages.push(&file[address..address + 0x100]);
            node = &inode_table[node.bank as usize].inodes[node.page as usize - 1];
        }

        rv.push((note, pages));
    }

    Ok(rv)
}

pub fn build(notes: &[(Note, Vec<u8>)]) -> BinResult<Vec<u8>> {
    if notes.len() > NoteTable::NUM_NOTES {
        return Err(Error::AssertFail {
            pos: 0,
            message: format!(
                "provided number of notes ({}) exceeds maximum ({})",
                notes.len(),
                NoteTable::NUM_NOTES
            ),
        });
    }

    let total_pages = notes
        .iter()
        .map(|(_, d)| d.len().next_multiple_of(0x100))
        .sum::<usize>()
        / 0x100;

    // banks to pages = 125 * b - 2
    // i.e. max pages <= 125 * b - 2
    // therefore
    // b = ceil((pages + 2) / 125)
    let banks = (total_pages + 2).div_ceil(125);

    if banks > MAX_BANKS.into() {
        return Err(Error::AssertFail {
            pos: 0,
            message: format!("required number of banks ({banks}) exceeds maximum ({MAX_BANKS})"),
        });
    }

    let mut inode_table = vec![
        InodeTable {
            checksum: 0,
            inodes: [Inode::from(0x0003); InodeTable::NUM_INODES],
        };
        banks
    ];

    for i in 0..banks {
        inode_table[0].inodes[i] = Inode::from(0x0000); // inode table
        inode_table[0].inodes[banks + i] = Inode::from(0x0000);
    }

    inode_table[0].inodes[2 * banks] = Inode::from(0x0000); // note table
    inode_table[0].inodes[2 * banks + 1] = Inode::from(0x0000);

    let id = {
        let mut id = IDBlock {
            repaired: 0xFFFFFFFF,
            random: random(),
            serial_mid: 0,
            serial_low: 0,
            deviceid: 1,
            banks: banks as _,
            version: 0,
            checksum: 0,
            inverted_checksum: 0,
        };
        let sum = id.sum();
        let inv_sum = 0xFFF2u16.wrapping_sub(sum);
        id.checksum = sum;
        id.inverted_checksum = inv_sum;
        id
    };

    let sector = IDSector {
        label_area: [0; 32],
        id_block: id,
        id_block_backup: id,
        id_block_backup_2: id,
        id_block_backup_3: id,
    };

    let mut page_mappings = vec![];

    let mut cur_page = Inode::from((banks * 2 + 3) as u16);

    let mut note_table = NoteTable {
        notes: [Default::default(); NoteTable::NUM_NOTES],
    };

    for (note_index, (note, data)) in notes.iter().enumerate() {
        note_table.notes[note_index] = *note;
        note_table.notes[note_index].start_page = cur_page;

        let next_page = |page| match page {
            Inode { bank: x, page: 127 } => Inode {
                bank: x + 1,
                page: 1, // always skip the first page of every bank
            },
            Inode { bank: x, page: y } => Inode {
                bank: x,
                page: y + 1,
            },
        };

        let find_next_page = |page, table: &[InodeTable]| {
            let mut next = next_page(page);
            while u16::from(table[next.bank as usize].inodes[next.page as usize - 1]) != 0x0003 {
                next = next_page(next);
            }
            next
        };

        let mut next = find_next_page(cur_page, &inode_table);

        let mut prev = cur_page;

        for i in data.chunks(0x100) {
            page_mappings.push((cur_page, i));
            inode_table[cur_page.bank as usize].inodes[cur_page.page as usize - 1] = next;
            prev = cur_page;
            cur_page = next;
            next = find_next_page(cur_page, &inode_table);
        }

        inode_table[prev.bank as usize].inodes[prev.page as usize - 1] = Inode::from(0x0001);
    }

    for tab in &mut inode_table {
        tab.checksum = tab.sum();
    }

    let inode_table = array::from_fn::<_, 2, _>(|_| inode_table.clone());

    let mut rv = vec![0; banks * 0x8000];

    let mut cursor = Cursor::new(&mut rv);

    sector.write_be(&mut cursor)?;

    inode_table.write_be(&mut cursor)?;

    note_table.write_be(&mut cursor)?;

    for (inode, data) in page_mappings {
        cursor.seek(SeekFrom::Start(inode.address() as u64))?;
        cursor.write_all(data)?;
    }

    Ok(rv)
}
