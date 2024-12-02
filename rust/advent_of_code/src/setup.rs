use std::{fs::{self, File}, io::{self, Write}, path::Path};

pub fn setup_year() {
    let mut buffer = String::new();

    buffer = "2024".to_string();

    let template = fs::read_to_string("src/day0x.rs").unwrap();
    
    let mut mods = String::new();
    let mut maps = String::new();


    for i in 1 ..=25 {
        create_file(&buffer, i, &template);

        mods.push_str(&format!("pub mod day{:02};\n", i));
        maps.push_str(&format!("({}, {:02}) => Some(y{}::day{:02}::execute),\n", &buffer, i, &buffer, i));
    }

    let mod_path = &format!("src/y{}/mod.rs", &buffer);
    if !Path::new(mod_path).exists() {
        let mut file = File::create(mod_path).unwrap();
        file.write_all(mods.as_bytes()).unwrap();
    } else {
        println!("mod.rs already exists. Here is a complete list of day imports that should be present");
        println!("{}", mods);
    }

    println!("Paste following into the mapping:");
    println!("{}", maps);


}

fn create_file(year: &str, day: i32, template: &str) {
    let mut content = template.replace("yyyy", year);
    content = content.replace("dd", &day.to_string());

    let path = format!("src/y{}/day{:02}.rs", year, day);

    if Path::new(&path).exists() {
        println!("{}-{} already exists.. Skipping!", year, day);
    } else {
        let mut file = File::create(&path).unwrap();
        file.write_all(content.as_bytes()).unwrap();
    }
}

