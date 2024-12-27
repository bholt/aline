use comfy_table::{modifiers, presets, Table};
use polars::prelude::*;

pub fn create_dataframe() -> PolarsResult<DataFrame> {
    let s0 = Series::new("days".into(), &[0, 1, 2, 3, 4]);
    let s1 = Series::new("temp".into(), &[22.1, 19.9, 7.0, 2.0, 3.0]);
    DataFrame::new(vec![s0.into(), s1.into()]).map_err(|e| e.into())
}

fn main() {
    let mut table = Table::new();
    // let mystyle = "││──╞═╪╡│─┼├┤┬┴┌┐└┘";
    table
        .load_preset(presets::UTF8_FULL)
        .apply_modifier(modifiers::UTF8_ROUND_CORNERS)
        .apply_modifier(modifiers::UTF8_SOLID_INNER_BORDERS);
    //     .set_header(vec!["Header1", "Header2", "Header3"])
    //     .add_row(vec![
    //         "This is a text",
    //         "This is another text",
    //         "This is the third text",
    //     ]);
    //     .add_row(vec![
    //         "This is another text",
    //         "Now\n add some\n multi line stuff",
    //         "This is awesome",
    //     ]);
    // println!("{table}");

    let df = create_dataframe().unwrap();

    table.set_header(df.get_column_names().iter().map(|s| s.to_string()).collect::<Vec<String>>());

    let mut row = df.get_row(0).expect("unable to get first reference row");
    for i in 0..df.height() {
        df.get_row_amortized(i, &mut row).expect("unable to get row {i}");
        let row_values: Vec<String> = row.0.iter().map(|s| s.to_string()).collect();
        table.add_row(row_values);
    }

    println!("{table}");
}
