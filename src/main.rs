use std::fs;

mod pdf;
mod util;

fn main() {
    let mut inst = pdf::pdf::Pdf::try_from(r#"C:\Users\hw\Downloads\demo.pdf"#).expect("what?");
    println!("Hello, world! {} {}", inst.catalog.page.count, inst.catalog.page.index.len());
    for page in 0..inst.catalog.page.count {
        let page_obj = inst.page((page+1) as u64).unwrap();
        println!("page {} {:?}", page, page_obj);
    }
}
