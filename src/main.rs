use std::fs;

mod pdf;
mod util;

fn main() {
    let inst = pdf::pdf::Pdf::try_from(r#"C:\Users\hw\Downloads\demo.pdf"#).expect("what?");
    println!("Hello, world! {} {}", inst.catalog.page.count, inst.catalog.page.index.len());
    for page in 0..inst.catalog.page.count {
        println!("page {} at {}", page, inst.catalog.page.index.get(page).unwrap());
    }
}
