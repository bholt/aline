use comfy_table::{Table, presets};

fn main() {
    let mut table = Table::new();
    table
        .load_preset(presets::UTF8_FULL)
        .set_header(vec!["Header1", "Header2", "Header3"])
        .add_row(vec![
            "This is a text",
            "This is another text",
            "This is the third text",
        ])
        .add_row(vec![
            "This is another text",
            "Now\n add some\n multi line stuff",
            "This is awesome",
        ]);

    println!("{table}");
}
