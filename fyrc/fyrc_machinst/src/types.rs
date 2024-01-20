use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Register(pub u8);

impl Register {
    pub const R0: Self = Self(0);
    pub const R1: Self = Self(1);
    pub const R2: Self = Self(2);
    pub const R3: Self = Self(3);
    pub const R4: Self = Self(4);
    pub const R5: Self = Self(5);
    pub const R6: Self = Self(6);
    pub const R7: Self = Self(7);

    pub fn thumb(&self) -> u16 {
        u16::from(self.0) & 0b111
    }

    #[inline]
    pub fn is_lo(&self) -> bool {
        self.0 < 8
    }

    #[inline]
    pub fn is_hi(&self) -> bool {
        self.0 >= 8
    }

    pub fn reg_id(&self) -> u8 {
        self.0
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "r{}", self.0)
    }
}

pub trait MachineCode {
    type Output;

    fn to_machinst_bits(&self) -> Option<Vec<Self::Output>>;
    fn len(&self) -> usize {
        1
    }
}

macro_rules! make_thumb_imm {
    ($(($name:ident $bits:literal $($shift:literal)?))+) => {
        $(
            #[doc = concat!(
                stringify!($bits $(+ $shift)?),
                "-bit immediate value",
                $(" shifted right by ", $shift, " bits")?
            )]
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub struct $name(u16);

            impl $name {
                pub fn new(data: u16) -> Self {
                    Self(data)
                }

                pub fn bits(&self) -> u16 {
                    (self.0 $(>> $shift)?) & (2u16.pow($bits) - 1)
                }

                pub fn mag(&self) -> u16 {
                    self.0
                }
            }

            impl std::fmt::Display for $name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "#{}", self.0)
                }
            }

            impl From<u16> for $name {
                fn from(value: u16) -> Self {
                    Self(value)
                }
            }
        )+
    };
}

pub(crate) use make_thumb_imm;

macro_rules! make_thumb_simm {
    ($(($name:ident $bits:literal $($shift:literal)?))+) => {
        $(
            #[doc = concat!(
                stringify!($bits $(+ $shift)?),
                "-bit signed immediate value",
                $(" shifted right by ", $shift, " bits")?
            )]
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub struct $name(i16);

            impl $name {
                pub fn new(data: i16) -> Self {
                    Self(data)
                }

                pub fn bits(&self) -> u16 {
                    let res = if self.0 < 0 {
                        0b1 << ($bits - 1)
                    } else {
                        0b0
                    };
                    let casted = u16::from_ne_bytes(self.0.to_ne_bytes());
                    let rest_bits = (casted $(>> $shift)?) & ((0b1 << ($bits - 1)) - 1);
                    res | rest_bits
                }

                pub fn mag(&self) -> i16 {
                    self.0
                }
            }

            impl std::fmt::Display for $name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "#{}", self.0)
                }
            }

            impl From<i16> for $name {
                fn from(value: i16) -> Self {
                    Self(value)
                }
            }
        )+
    };
}

pub(crate) use make_thumb_simm;
