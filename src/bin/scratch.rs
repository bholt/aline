use comfy_table::{modifiers, presets, Table};

fn main() {
    let mut table = Table::new();
    // let mystyle = "││──╞═╪╡│─┼├┤┬┴┌┐└┘";
    table
        .load_preset(presets::UTF8_FULL)
        .apply_modifier(modifiers::UTF8_ROUND_CORNERS)
        .apply_modifier(modifiers::UTF8_SOLID_INNER_BORDERS)
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
