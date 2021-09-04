fn main() {
    let s = String::from("book");
    let pluralized_s = pluralize(&s);

    println!("I have one {}, you have two {}", s, pluralized_s);
}

fn pluralize(st: &str) -> String {
    st.to_owned() + "s"
}
