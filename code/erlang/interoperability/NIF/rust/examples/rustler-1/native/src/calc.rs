pub fn sum(a: i64, b: i64) -> i64 {
    a + b
}

pub fn subtract(a: i64, b: i64) -> i64 {
    a - b
}

pub fn multiplies(a: i64, b: i64) -> i64 {
    a * b
}

pub fn divides(a: i64, b: i64) -> i64 {
    a / b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_it_works() {
        assert_eq!(2, sum(1, 1));
        assert_eq!(2, subtract(4, 2));
        assert_eq!(2, multiplies(2, 1));
        assert_eq!(2, divides(10, 5));
    }
}
