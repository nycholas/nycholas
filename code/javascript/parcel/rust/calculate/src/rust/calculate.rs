#[no_mangle]
pub fn add(a: i32, b: i32) -> i32 {
    return a + b
}

#[no_mangle]
pub fn subtract(a: i32, b: i32) -> i32 {
    return a - b
}

#[no_mangle]
pub fn multiply(a: i32, b: i32) -> i32 {
    return a * b
}

#[no_mangle]
pub fn divide(a: i32, b: i32) -> i32 {
    return a / b
}
