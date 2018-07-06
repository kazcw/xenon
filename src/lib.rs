#![recursion_limit = "128"]

extern crate failure;
extern crate typenum;

use std::cmp::min;
use std::fmt::{self, Debug, Display, Formatter};
use std::ops::*;
use std::str::FromStr;

use typenum::consts::*;
use typenum::{Pow, Unsigned};

#[derive(Copy, Clone, Default, PartialEq, Eq, Ord, PartialOrd)]
pub struct Xe<T, B = U10, V = u64>(V, T, B);

pub type Xe0<V = u64> = Xe<U0, U10, V>;
pub type Xe1<V = u64> = Xe<U1, U10, V>;
pub type Xe2<V = u64> = Xe<U2, U10, V>;
pub type Xe3<V = u64> = Xe<U3, U10, V>;
pub type Xe4<V = u64> = Xe<U4, U10, V>;
pub type Xe5<V = u64> = Xe<U5, U10, V>;
pub type Xe6<V = u64> = Xe<U6, U10, V>;
pub type Xe7<V = u64> = Xe<U7, U10, V>;
pub type Xe8<V = u64> = Xe<U8, U10, V>;

pub type XeN = Xe<u8, U10, u64>;

impl<T, B> Xe<T, B> {
    #[inline]
    pub fn from_parts(n: u64, t: T) -> Self
    where
        B: Default,
    {
        Xe(n, t, B::default())
    }

    #[inline]
    pub fn from_quanta(n: u64) -> Self
    where
        T: Default,
        B: Default,
    {
        Xe(n, T::default(), B::default())
    }

    #[inline]
    pub fn from_u64(n: u64) -> Option<Self>
    where
        T: Default + Unsigned,
        B: Default + Unsigned + Pow<T>,
        <B as Pow<T>>::Output: Unsigned,
    {
        n.checked_mul(<B as Pow<T>>::Output::to_u64())
            .map(Self::from_quanta)
    }

    /*
    #[inline]
    fn rounded_mul<T2>(self, other: Xe<T1, B>) -> <T2>
where B: Unsigned + Pow<T>,
<B as Pow<T>>::Output: Unsigned, {
        // round to 0 can be done as a single int math hardware op, unless the quanta product sets
        // the hi qword
        let n = u128::from(self.0) * u128::from(other.0);
        let (hi, lo) = hilo(n);
        let n = if hi == 0 {
            lo / <B as Pow<T>>::Output::to_u64()
        } else {
            let n = n / u128::from(<B as Pow<T>>::Output::to_u64());
            let (hi, lo) = hilo(n);
            assert!(hi == 0, "overflow");
            lo
        };
        Xe(n, PhantomData)
    }
    */
}

impl<T, B> Add for Xe<T, B> {
    type Output = Self;

    #[inline]
    fn add(self, other: Self) -> Self::Output {
        let n = self.0.checked_add(other.0).expect("overflow");
        Xe(n, self.1, self.2)
    }
}

impl<T, B> Sub for Xe<T, B> {
    type Output = Self;
    #[inline]
    fn sub(self, other: Self) -> Self::Output {
        let n = self.0.checked_sub(other.0).expect("underflow");
        Xe(n, self.1, self.2)
    }
}

fn hilo(n: u128) -> (u64, u64) {
    ((n >> 64) as u64, n as u64)
}

impl<T0, T1, B> Mul<Xe<T1, B>> for Xe<T0, B> where T0: Add<T1>
{
    type Output = Xe<<T0 as Add<T1>>::Output, B>;

    #[inline]
    fn mul(self, other: Xe<T1, B>) -> Self::Output {
        let n = u128::from(self.0) * u128::from(other.0);
        let (hi, lo) = hilo(n);
        assert!(hi == 0, "overflow");
        Xe(lo, self.1 + other.1, self.2)
    }
}

impl<T, U10> Display for Xe<T, U10>
where
    T: Unsigned,
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let mut s = [b'0'; 20];
        let mut x = self.0;
        let start = {
            let mut inserter = s.iter_mut().rev();
            for c in inserter.by_ref() {
                *c = ((x % 10) as u32 + '0' as u32) as u8;
                x /= 10;
                if x == 0 {
                    break;
                }
            }
            inserter.count()
        };
        let start = min(start, 19 - T::to_usize());
        let s = unsafe { std::str::from_utf8_unchecked(&s) };
        let (ipart, fpart) = s.split_at(20 - T::to_usize());
        let ipart = &ipart[start..];
        write!(f, "{}.{}", ipart, fpart)
    }
}

impl<U10> Display for Xe<u8, U10> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let mut s = [b'0'; 20];
        let mut x = self.0;
        let start = {
            let mut inserter = s.iter_mut().rev();
            for c in inserter.by_ref() {
                *c = ((x % 10) as u32 + '0' as u32) as u8;
                x /= 10;
                if x == 0 {
                    break;
                }
            }
            inserter.count()
        };
        let start = min(start, 19 - self.1);
        let s = unsafe { std::str::from_utf8_unchecked(&s) };
        let (ipart, fpart) = s.split_at(20 - self.1);
        let ipart = &ipart[start..];
        write!(f, "{}.{}", ipart, fpart)
    }
}

impl<T, B> Debug for Xe<T, B>
where
    T: Debug,
    B: Debug,
    Xe<T, B>: Display,
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}\t=\tXe<{:?}, {:?}>({})",
            self,
            self.1,
            self.2,
            self.0
        )
    }
}

impl<T> FromStr for Xe<T, U10> where T: Default + Unsigned {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        let s = s.as_bytes();
        if s.len() < T::to_usize() + 2 {
            return Err(());
        }
        let (ipart, fpart) = s.split_at(s.len() - T::to_usize() - 1);
        let (dot, fpart) = fpart.split_first().unwrap();
        if *dot != b'.' {
            return Err(());
        }
        let mut x = 0u64;
        for c in ipart.into_iter().cloned().chain(fpart.into_iter().cloned()) {
            if c < b'0' || c > b'9' {
                return Err(());
            }
            x *= 10;
            x += u64::from(c - b'0');
        }
        Ok(Xe::from_quanta(x))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        let a = Xe2::from_u64(2).unwrap();
        let b = Xe::from_quanta(0_02);
        let c = "2.02".parse().unwrap();
        //eprintln!("test_add: a: {:?}", a);
        //eprintln!("test_add: b: {:?}", b);
        //eprintln!("test_add: c: {:?}", c);
        assert_eq!(a + b, c);
    }

    #[test]
    fn test_mul() {
        let a = Xe2::from_u64(2).unwrap();
        let b: Xe1 = "0.2".parse().unwrap();
        let c = Xe::from_quanta(0_400);
        //eprintln!("test_mul: a: {:?}", a);
        //eprintln!("test_mul: b: {:?}", b);
        //eprintln!("test_mul: c: {:?}", c);
        assert_eq!(a * b, c);
    }

    #[test]
    fn test_mul2() {
        let a = XeN::from_parts(2, 2);
        let b = XeN::from_parts(2, 1);
        let c = XeN::from_parts(4, 3);
        //eprintln!("test_mul: a: {:?}", a);
        //eprintln!("test_mul: b: {:?}", b);
        //eprintln!("test_mul: c: {:?}", c);
        assert_eq!(a * b, c);
    }

    #[test]
    fn test_bad_parses() {
        let a: Result<Xe2, _> = "0.2".parse();
        assert!(a.is_err());

        let a: Result<Xe1, _> = "0.20".parse();
        assert!(a.is_err());

        let a: Result<Xe0, _> = ".1".parse();
        assert!(a.is_err());

        let a: Result<Xe1, _> = ".1".parse();
        assert!(a.is_err());

        let a: Result<Xe2, _> = ".1".parse();
        assert!(a.is_err());

        let a: Result<Xe0, _> = "1".parse();
        assert!(a.is_err());

        let a: Result<Xe1, _> = "1".parse();
        assert!(a.is_err());
    }
}
