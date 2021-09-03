fn main() {
    let s = String::from("book");
    let pluralized_s = pluralize(s.clone());

    println!("I have one {}, you have two {}", s, pluralized_s);
}

fn pluralize(mut st: String) -> String {
    st.push_str("s");
    st
}
