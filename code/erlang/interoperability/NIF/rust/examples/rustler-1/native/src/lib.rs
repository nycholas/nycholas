extern crate rustler;

mod calc;

#[rustler::nif]
pub fn sum(a: i64, b: i64) -> i64 {
    calc::sum(a, b)
}

#[rustler::nif]
pub fn subtract(a: i64, b: i64) -> i64 {
    calc::subtract(a, b)
}

#[rustler::nif]
pub fn multiplies(a: i64, b: i64) -> i64 {
    calc::multiplies(a, b)
}

#[rustler::nif]
pub fn divides(a: i64, b: i64) -> i64 {
    calc::divides(a, b)
}

rustler::init! {
    "calc",
    [
        sum,
        subtract,
        multiplies,
        divides,
    ]
}
