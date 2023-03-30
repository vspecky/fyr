use crate::ssa::value::Value;

pub struct Add {
    pub rn: Value,
    pub rs: Value,
    pub rd: Value,
}

pub struct Sub {
    pub rn: Value,
    pub rs: Value,
    pub rd: Value,
}
