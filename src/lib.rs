//! # Fixed-point math, for any static or dynamic precision in any base.
//!
//! Differences from floating-point:
//! - exact results, except where explicitly rounded
//! - supports rounding in any base, such as decimal
//! - simpler interface (no NaN, infinities)
//! - better performance
//! - more compact storage for some value ranges
//! - unnormalized, so sigfigs of result can be determined by sigfigs of inputs
//!
//! Differences from bignums:
//! - if you know the "shape" of your inputs/calculations, the statically typed fixed-points can be
//! more efficient
//! - XeN is optimized for values that fit in a relatively small range
//!
//! ## Dynamic precision? Isn't that floating point?
//!
//! Technically yes, but the Xe API is much simpler than typical floating point; it's designed for
//! applications like working with a collection of values that are known to be fixed-precision
//! decimal of the same type, but the number of decimal places won't be known until runtime.

//#![recursion_limit = "4096"]

extern crate failure;
extern crate typenum;

use std::cmp::min;
use std::cmp::Ordering;
use std::fmt::{self, Debug, Display, Formatter};
use std::ops::*;
use std::str::FromStr;

use typenum::consts::*;
use typenum::{Bit, Diff, Pow, Sum, UInt, Unsigned};

//#[derive(Copy, Clone, Default, PartialEq, Eq, Ord, PartialOrd)]
#[derive(Copy, Clone, Default)]
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
pub type Xe9<V = u64> = Xe<U9, U10, V>;
pub type Xe10<V = u64> = Xe<U10, U10, V>;
pub type Xe11<V = u64> = Xe<U11, U10, V>;
pub type Xe12<V = u64> = Xe<U12, U10, V>;
pub type Xe13<V = u64> = Xe<U13, U10, V>;
pub type Xe14<V = u64> = Xe<U14, U10, V>;
pub type Xe15<V = u64> = Xe<U15, U10, V>;

pub type XeN = Xe<u8, U10, u64>;
pub type XeNS = Xe<i8, U10, u64>;

impl<T, B> Xe<T, B> {
    #[inline]
    pub fn from_parts(n: u64, t: T) -> Self
    where
        B: Default,
    {
        Xe(n, t, B::default())
    }

    #[inline]
    pub fn from_str_at_precision(s: &str, t: T) -> Result<Self, ()>
    where
        B: Default,
        isize: From<T>,
        T: Copy,
    {
        // this the XeN version of from_str()
        let sz = isize::from(t);
        if sz >= 0 {
            let x = u64::from_str_radix(s, 10).map_err(|_| ())?;
            let x = x.checked_div(10u64.pow(sz as u32)).unwrap();
            return Ok(Xe(x, t, B::default()));
        }
        let s = s.as_bytes();
        let sz = -sz as usize;
        if s.len() < sz + 2 {
            return Err(());
        }
        let (ipart, fpart) = s.split_at(s.len() - 1 - sz);
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
        Ok(Xe(x, t, B::default()))
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
        T: Default,
        B: Default + Pow<T>,
        <B as Pow<T>>::Output: Unsigned,
    {
        n.checked_mul(<B as Pow<T>>::Output::to_u64())
            .map(Self::from_quanta)
    }

    //see also `from_f64_rounded(n: f64) -> Option<Self>`
    #[inline]
    pub fn from_f64(f: f64) -> Option<Self>
    where
        T: Default,
        B: Default + Pow<T>,
        <B as Pow<T>>::Output: Unsigned,
    {
        let f = f * (<B as Pow<T>>::Output::to_u64() as f64);
        if f.fract() != 0.0 {
            return None;
        }
        let n = f.trunc() as u64;
        Some(Self::from_quanta(n))
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

    // Pow version. doesn't (ever?) finish compiling
    // TODO: if T + T2 < T1, we need to include the factor in the divisor instead
    /*
    #[inline]
    pub fn truncated_div<T1, T2>(self, other: Xe<T1, B>) -> Xe<T2, B>
    where
        T: Add<T2>,
        T2: Default,
        Sum<T, T2>: Sub<T1>,
        B: Pow<Diff<Sum<T, T2>, T1>>,
        typenum::Exp<B, Diff<Sum<T, T2>, T1>>: Unsigned,
    {
        let n = self
            .0
            .checked_mul(typenum::Exp::<B, Diff<Sum<T, T2>, T1>>::U64)
            .expect("overflow");
        let n = n / other.0;
        Xe(n, Default::default(), self.2)
    }
    */

    // TODO: handle extreme truncation where Diff<Sum<T1, T2>, T>: ?Unsigned
    #[inline]
    pub fn truncated_div<T1, T2>(self, other: Xe<T1, B>) -> Xe<T2, B>
    where
        T1: Add<T2>,
        T2: Default,
        Sum<T1, T2>: Sub<T>,
        Diff<Sum<T1, T2>, T>: Unsigned,
        B: Unsigned,
    {
        let mut n = self.0;
        let mut overflow = false;
        for _ in 0..Diff::<Sum<T1, T2>, T>::USIZE {
            // checked_mul doesn't get constant multiple optimization here, emits actual mul insn
            let prev = n;
            n *= B::U64;
            overflow |= n < prev;
        }
        assert!(!overflow);

        let n = n / other.0;
        Xe(n, Default::default(), self.2)
    }

    /// The output type must be no greater in precision than the type that would accomodate the
    /// result of the multiplication without truncation (statically checked).
    #[inline]
    pub fn truncated_mul<T1, T2>(self, other: Xe<T1, B>) -> Xe<T2, B>
    where
        T: Add<T1>,
        T2: Default,
        Sum<T, T1>: Sub<T2>,
        Diff<Sum<T, T1>, T2>: Unsigned,
        B: Unsigned,
    {
        let mut n = self.0.checked_mul(other.0).expect("overflow");
        for _ in 0..Diff::<Sum<T, T1>, T2>::USIZE {
            n /= B::U64;
        }
        Xe(n, Default::default(), self.2)
    }

    /// Return whether the value is the additive identity for its types.
    #[inline]
    pub fn is_zero(self) -> bool
    where
        T: Add<T>,
        T: PartialEq<<T as Add<T>>::Output>,
    {
        self.0 == self.0 + self.0
    }
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

/*
fn hilo(n: u128) -> (u64, u64) {
    ((n >> 64) as u64, n as u64)
}
*/

impl<T0, T1, B> Mul<Xe<T1, B>> for Xe<T0, B>
where
    T0: Add<T1>,
{
    type Output = Xe<<T0 as Add<T1>>::Output, B>;

    #[inline]
    fn mul(self, other: Xe<T1, B>) -> Self::Output {
        let n = self.0.checked_mul(other.0).expect("overflow");
        Xe(n, self.1 + other.1, self.2)
    }
}

pub trait ToIsize {
    fn to_isize(&self) -> isize;
}
impl<U, B> ToIsize for UInt<U, B>
where
    U: Unsigned,
    B: Bit,
{
    fn to_isize(&self) -> isize {
        <Self as Unsigned>::USIZE as isize
    }
}
impl ToIsize for u8 {
    fn to_isize(&self) -> isize {
        isize::from(*self)
    }
}
impl ToIsize for i8 {
    fn to_isize(&self) -> isize {
        isize::from(*self)
    }
}

impl<T, U10> Display for Xe<T, U10>
where
    T: ToIsize,
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
        let s = unsafe { std::str::from_utf8_unchecked(&s) };
        let sz = self.1.to_isize();
        if sz > 0 {
            return write!(f, "{}e{}", &s[start..], sz);
        }
        let sz = -sz as usize;
        let start = min(start, 19 - sz);
        let (ipart, fpart) = s.split_at(20 - sz);
        let ipart = &ipart[start..];
        write!(f, "{}.{}", ipart, fpart)
    }
}

#[derive(Copy, Clone)]
pub struct F64(pub f64);

impl Debug for F64 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use std::fmt::Write;
        let n = self.0.to_bits();
        let sgn = n & 0x8000_0000_0000_0000;
        let exp = (n & 0x7ff0_0000_0000_0000) >> 52;
        let mantis = n & 0x000f_ffff_ffff_ffff;
        if sgn != 0 {
            f.write_char('-')?;
        }
        let firstbit = if exp == 0x000 || exp == 0x7ff {
            if exp == 0x7ff {
                return if mantis == 0 {
                    f.write_str("Inf")
                } else {
                    write!(f, "NaN({:013x})", mantis)
                };
            }
            // Zero or Subnormal
            '0'
        } else {
            '1'
        };
        let exp = exp as i32 - 1023;
        write!(f, "0x{}.{:013x}p{}", firstbit, mantis, exp)
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
            "{}\t=\tXe({:?}, {:?}, {:?})",
            self, self.0, self.1, self.2,
        )
    }
}

impl<T> FromStr for Xe<T, U10>
where
    T: Default + Unsigned,
{
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        let s = s.as_bytes();
        if s.len() < T::USIZE + 2 {
            return Err(());
        }
        let (ipart, fpart) = s.split_at(s.len() - T::USIZE - 1);
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

/*
impl<T, B> Into<f64> for Xe<T, B> {
    fn into(self) -> f64 {
        unimplemented!()
    }
}
*/

/*
impl<T, B> PartialEq<f64> for Xe<T, B> {
    fn eq(&self, &other: &f64) -> bool {
        let f: f64 = self.into();
        f == other
    }
}
*/

/*
impl<T, B> PartialEq for Xe<T, B> where T: Default, B: Default + Pow<T>, <B as Pow<T>>::Output: Unsigned {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<T, B> Eq for Xe<T, B> where T: Default, B: Default + Pow<T>, <B as Pow<T>>::Output: Unsigned {}
*/

impl<T, B> PartialEq for Xe<T, B> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<T, B> Eq for Xe<T, B> {}

impl<T, B> PartialOrd for Xe<T, B> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}
impl<T, B> Ord for Xe<T, B> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}

impl<T, B> PartialEq<f64> for Xe<T, B>
where
    T: Default,
    B: Default + Pow<T>,
    <B as Pow<T>>::Output: Unsigned,
{
    #[inline]
    fn eq(&self, other: &f64) -> bool {
        if let Some(other) = Self::from_f64(*other) {
            self.eq(&other)
        } else {
            // the value can't be exactly represented as a Self, so it can't be equal
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        let a = Xe2::from_u64(2).unwrap();
        let b = Xe::from_quanta(0_02);
        let c: Xe2 = "2.02".parse().unwrap();
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
        //eprintln!("test_mul2: a: {:?}", a);
        //eprintln!("test_mul2: b: {:?}", b);
        //eprintln!("test_mul2: c: {:?}", c);
        assert_eq!(a * b, c);
    }

    #[test]
    fn test_fsap() {
        let _ = Xe::<i8>::from_str_at_precision("100", 2i8).unwrap();
        let _ = Xe::<i8>::from_str_at_precision("1", 0i8).unwrap();
        let z = Xe::<i8>::from_str_at_precision("0", 2i8).unwrap();
        let _ = Xe::<i8>::from_str_at_precision("0.1", -1i8).unwrap();
        let _ = Xe::<i8>::from_str_at_precision("0.01", -2i8).unwrap();
        assert!(z.is_zero());
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
