use std::{
    collections::HashMap,
    fs::{File, OpenOptions},
    io::{self, Cursor, Error as stdIOErr, Read, Seek},
};

use crate::pdf::tokenizer::{Object, Source, Tokenizer};

use thiserror::Error;

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use crate::pdf::pdf::*;

    // ---------- trailer (PDF 1.7: trailer 字典紧跟 xref 后，含 /Size /Root 等) ----------

    #[test]
    fn parse_trailer_valid_minimal() {
        let v = minimal_pdf_with_trailer(b"trailer << /Size 4 /Root 1 0 R >>");
        let p = Pdf::<Cursor<Vec<u8>>>::try_from(v).unwrap();
        assert_eq!(p.catalog.page.count, 0);
    }

    #[test]
    fn parse_trailer_with_prev() {
        let v = minimal_pdf_with_two_xref_and_prev();
        let p = Pdf::<Cursor<Vec<u8>>>::try_from(v).unwrap();
        assert_eq!(p.catalog.page.count, 0);
    }

    #[test]
    fn parse_trailer_invalid_no_trailer_keyword() {
        let v = minimal_pdf_with_trailer(b"<< /Size 4 /Root 1 0 R >>");
        assert!(Pdf::<Cursor<Vec<u8>>>::try_from(v).is_err());
    }

    #[test]
    fn parse_trailer_invalid_root_not_ref() {
        let v = minimal_pdf_with_trailer(b"trailer << /Size 4 /Root (notref) >>");
        assert!(Pdf::<Cursor<Vec<u8>>>::try_from(v).is_err());
    }

    // ---------- startxref (PDF 1.7: startxref 后跟数字，再 %%EOF) ----------

    #[test]
    fn parse_startxref_valid() {
        let v = full_minimal_pdf();
        let p = Pdf::<Cursor<Vec<u8>>>::try_from(v).unwrap();
        assert!(p.xref.contains_key(&1));
    }

    #[test]
    fn parse_startxref_invalid_not_number() {
        let v = minimal_pdf_append_tail(b"startxref abc\n%%EOF");
        assert!(Pdf::<Cursor<Vec<u8>>>::try_from(v).is_err());
    }

    #[test]
    fn parse_startxref_invalid_no_eof() {
        let v = minimal_pdf_append_tail(b"startxref 0 ");
        assert!(Pdf::<Cursor<Vec<u8>>>::try_from(v).is_err());
    }

    // ---------- xref (PDF 1.7: xref 关键字 + 子节 起始编号 数量 + 行) ----------

    #[test]
    fn parse_xref_valid_one_subsection() {
        let v = full_minimal_pdf();
        let p = Pdf::<Cursor<Vec<u8>>>::try_from(v).unwrap();
        assert!(!p.xref.is_empty());
        assert!(p.root > 0);
    }

    #[test]
    fn parse_xref_entry_count_and_offsets() {
        let v = full_minimal_pdf();
        let p = Pdf::<Cursor<Vec<u8>>>::try_from(v).unwrap();
        assert_eq!(
            p.xref.len(),
            3,
            "xref 子节 0 4：仅记录 n 条目 obj1 obj2 obj3"
        );
        assert_eq!(p.xref.get(&1), Some(&9_u64), "obj1 Catalog 在偏移 9");
        assert_eq!(p.xref.get(&2), Some(&58_u64), "obj2 Pages 在偏移 58");
        assert_eq!(p.xref.get(&3), Some(&115_u64), "obj3 在偏移 115");
    }

    #[test]
    fn parse_xref_invalid_wrong_keyword_at_startxref() {
        let v = minimal_pdf_with_xref_keyword(b"xxxx");
        assert!(Pdf::<Cursor<Vec<u8>>>::try_from(v).is_err());
    }

    #[test]
    fn parse_xref_invalid_incomplete_subsection() {
        let v = minimal_pdf_with_xref_incomplete();
        assert!(Pdf::<Cursor<Vec<u8>>>::try_from(v).is_err());
    }

    // ---------- catalog (PDF 1.7: Root 为 Catalog，/Type /Catalog, /Pages ref) ----------

    #[test]
    fn parse_catalog_valid() {
        let v = full_minimal_pdf();
        let p = Pdf::<Cursor<Vec<u8>>>::try_from(v).unwrap();
        assert_eq!(p.root, 1);
    }

    #[test]
    fn parse_catalog_invalid_type_not_catalog() {
        let v = minimal_pdf_with_catalog_type(b"/Type /Xatalog");
        assert!(Pdf::<Cursor<Vec<u8>>>::try_from(v).is_err());
    }

    #[test]
    fn parse_catalog_invalid_pages_not_ref() {
        let v = minimal_pdf_with_pages_value(b"(notref)   ");
        assert!(Pdf::<Cursor<Vec<u8>>>::try_from(v).is_err());
    }

    // ---------- pages (PDF 1.7: /Type /Pages, /Count n, /Kids [ refs ]) ----------

    #[test]
    fn parse_pages_valid_zero_kids() {
        let v = full_minimal_pdf();
        let p = Pdf::<Cursor<Vec<u8>>>::try_from(v).unwrap();
        assert_eq!(p.catalog.page.count, 0);
        assert!(p.catalog.page.pages.is_empty());
    }

    #[test]
    fn parse_pages_valid_with_kids() {
        let v = minimal_pdf_with_pages_kids();
        let p = Pdf::<Cursor<Vec<u8>>>::try_from(v).unwrap();
        assert!(p.catalog.page.count >= 1);
        assert!(!p.catalog.page.pages.is_empty());
    }

    #[test]
    fn parse_pages_invalid_count_not_integer() {
        let v = minimal_pdf_with_pages_count(b"/Count a");
        assert!(Pdf::<Cursor<Vec<u8>>>::try_from(v).is_err());
    }

    #[test]
    fn parse_pages_invalid_kids_not_array() {
        let v = minimal_pdf_with_pages_kids_not_array();
        assert!(Pdf::<Cursor<Vec<u8>>>::try_from(v).is_err());
    }

    // ---------- 辅助：构造符合 PDF 1.7 的最小字节流 ----------

    fn minimal_pdf_body() -> Vec<u8> {
        let mut v = Vec::new();
        v.extend_from_slice(b"%PDF-1.7\n");
        v.extend_from_slice(b"1 0 obj << /Type /Catalog /Pages 2 0 R >> endobj\n");
        v.extend_from_slice(b"2 0 obj << /Type /Pages /Count 0 /Kids [] >> endobj\n");
        v
    }

    fn minimal_pdf_with_trailer(trailer_part: &[u8]) -> Vec<u8> {
        let mut v = minimal_pdf_body();
        let xref_start = v.len();
        v.extend_from_slice(b"xref\n0 4\n0000000000 65535 f \n0000000009 00000 n \n0000000058 00000 n \n0000000115 00000 n \n");
        v.extend_from_slice(trailer_part);
        v.extend_from_slice(b"\nstartxref ");
        v.extend_from_slice(format!("{}\n", xref_start).as_bytes());
        v.extend_from_slice(b"%%EOF");
        v
    }

    fn minimal_pdf_append_tail(tail: &[u8]) -> Vec<u8> {
        let mut v = minimal_pdf_body();
        let xref_start = v.len();
        v.extend_from_slice(b"xref\n0 4\n0000000000 65535 f \n0000000009 00000 n \n0000000058 00000 n \n0000000115 00000 n \n");
        v.extend_from_slice(b"trailer << /Size 4 /Root 1 0 R >> \n");
        v.extend_from_slice(tail);
        v
    }

    fn full_minimal_pdf() -> Vec<u8> {
        minimal_pdf_with_trailer(b"trailer << /Size 4 /Root 1 0 R >>")
    }

    fn minimal_pdf_with_two_xref_and_prev() -> Vec<u8> {
        let mut v = minimal_pdf_body();
        let xref1_start = v.len();
        v.extend_from_slice(
            b"xref\n0 3\n0000000000 65535 f \n0000000009 00000 n \n0000000058 00000 n \n",
        );
        v.extend_from_slice(b"trailer << /Size 3 /Root 1 0 R >> \n");
        let obj3_start = v.len();
        v.extend_from_slice(b"3 0 obj << >> endobj\n");
        let xref2_start = v.len();
        v.extend_from_slice(b"xref\n3 1\n");
        v.extend_from_slice(format!("{:010} 00000 n \n", obj3_start).as_bytes());
        v.extend_from_slice(b"trailer << /Size 4 /Root 1 0 R /Prev ");
        v.extend_from_slice(format!("{}", xref1_start).as_bytes());
        v.extend_from_slice(b" >> \nstartxref ");
        v.extend_from_slice(format!("{}\n", xref2_start).as_bytes());
        v.extend_from_slice(b"%%EOF");
        v
    }

    fn minimal_pdf_with_xref_keyword(keyword: &[u8]) -> Vec<u8> {
        let mut v = minimal_pdf_body();
        let xref_start = v.len();
        v.extend_from_slice(keyword);
        v.extend_from_slice(b"\n0 4\n0000000000 65535 f \n0000000009 00000 n \n0000000058 00000 n \n0000000115 00000 n \n");
        v.extend_from_slice(b"trailer << /Size 4 /Root 1 0 R >> \nstartxref ");
        v.extend_from_slice(format!("{}\n", xref_start).as_bytes());
        v.extend_from_slice(b"%%EOF");
        v
    }

    fn minimal_pdf_with_xref_incomplete() -> Vec<u8> {
        let mut v = minimal_pdf_body();
        let xref_start = v.len();
        v.extend_from_slice(b"xref\n0 4\n0000000000 65535 f \n0000000009 00000 n \n");
        v.extend_from_slice(b"trailer << /Size 4 /Root 1 0 R >> \nstartxref ");
        v.extend_from_slice(format!("{}\n", xref_start).as_bytes());
        v.extend_from_slice(b"%%EOF");
        v
    }

    fn minimal_pdf_with_catalog_type(type_val: &[u8]) -> Vec<u8> {
        let mut v = full_minimal_pdf();
        let needle = b"/Type /Catalog";
        if let Some(i) = v.windows(needle.len()).position(|w| w == needle) {
            let n = type_val.len().min(needle.len());
            v[i..i + n].copy_from_slice(&type_val[..n]);
            if n < needle.len() {
                v[i + n..i + needle.len()].fill(b' ');
            }
        }
        v
    }

    fn minimal_pdf_with_pages_value(pages_val: &[u8]) -> Vec<u8> {
        let mut v = full_minimal_pdf();
        let needle = b"/Pages 2 0 R";
        if let Some(i) = v.windows(needle.len()).position(|w| w == needle) {
            let n = pages_val.len().min(needle.len());
            v[i..i + n].copy_from_slice(&pages_val[..n]);
            if n < needle.len() {
                v[i + n..i + needle.len()].fill(b' ');
            }
        }
        v
    }

    fn minimal_pdf_with_pages_count(count_entry: &[u8]) -> Vec<u8> {
        let mut v = full_minimal_pdf();
        let needle = b"/Count 0";
        if let Some(i) = v.windows(needle.len()).position(|w| w == needle) {
            let n = count_entry.len().min(needle.len());
            v[i..i + n].copy_from_slice(&count_entry[..n]);
            if n < needle.len() {
                v[i + n..i + needle.len()].fill(b' ');
            }
        }
        v
    }

    fn minimal_pdf_with_pages_kids_not_array() -> Vec<u8> {
        let mut v = Vec::new();
        v.extend_from_slice(b"%PDF-1.7\n");
        v.extend_from_slice(b"1 0 obj << /Type /Catalog /Pages 2 0 R >> endobj\n");
        v.extend_from_slice(b"2 0 obj << /Type /Pages /Count 0 /Kids 1 0 R >> endobj\n");
        let xref_start = v.len();
        v.extend_from_slice(
            b"xref\n0 3\n0000000000 65535 f \n0000000009 00000 n \n0000000058 00000 n \n",
        );
        v.extend_from_slice(b"trailer << /Size 3 /Root 1 0 R >> \nstartxref ");
        v.extend_from_slice(format!("{}\n", xref_start).as_bytes());
        v.extend_from_slice(b"%%EOF");
        v
    }

    fn minimal_pdf_with_pages_kids() -> Vec<u8> {
        let mut v = Vec::new();
        v.extend_from_slice(b"%PDF-1.7\n");
        v.extend_from_slice(b"1 0 obj << /Type /Catalog /Pages 2 0 R >> endobj\n");
        v.extend_from_slice(b"2 0 obj << /Type /Pages /Count 1 /Kids [ 3 0 R ] >> endobj\n");
        v.extend_from_slice(b"3 0 obj << /Type /Page /Parent 2 0 R >> endobj\n");
        let xref_start = v.len();
        v.extend_from_slice(b"xref\n0 5\n0000000000 65535 f \n0000000009 00000 n \n0000000058 00000 n \n0000000115 00000 n \n0000000172 00000 n \n");
        v.extend_from_slice(b"trailer << /Size 5 /Root 1 0 R >> \nstartxref ");
        let num_pos = v.len();
        v.extend_from_slice(b"0000000000\n%%EOF");
        let num_str = format!("{:010}", xref_start);
        v[num_pos..num_pos + 10].copy_from_slice(num_str.as_bytes());
        v
    }
}
