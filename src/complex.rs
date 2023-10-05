use std::fmt::Display;

use std::ops;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Complex {
    pub(crate) re: f64,
    pub(crate) im: f64,
}

impl Complex {
    pub fn complex(r: f64, i: f64) -> Self {
        Self { re: r, im: i }
    }

    pub fn zero() -> Self {
        Self { re: 0.0, im: 0.0 }
    }

    pub fn from_real(v: f64) -> Self {
        Self { re: v, im: 0.0 }
    }

    pub fn from_img(v: f64) -> Self {
        Self { re: 0.0, im: v }
    }

    pub fn real(self) -> f64 {
        self.re
    }

    pub fn img(self) -> f64 {
        self.im
    }

    pub fn sqrt(self) -> Self {
        todo!("write")
    }
}

impl ops::Add for Complex {
    fn add(self, rhs: Self) -> Self::Output {
        Self {
            re: self.re + rhs.re,
            im: self.im + rhs.im,
        }
    }

    type Output = Complex;
}

impl ops::Sub for Complex {
    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            re: self.re - rhs.re,
            im: self.im - rhs.im,
        }
    }

    type Output = Complex;
}

impl ops::Mul for Complex {
    fn mul(self, rhs: Self) -> Self::Output {
        Complex::complex(
            self.real() * rhs.real() - self.img() * rhs.img(),
            self.real() * rhs.img() + self.img() * rhs.real(),
        )
    }

    type Output = Complex;
}

impl ops::Div for Complex {
    type Output = Complex;

    fn div(self, rhs: Self) -> Self::Output {
        let factor = rhs.real() * rhs.real() + rhs.img() * rhs.img();
        let real = (self.real() * rhs.real() + self.img() * rhs.img()) / factor;
        let img = (self.img() * rhs.real() - self.real() * rhs.img()) / factor;
        Complex::complex(real, img)
    }
}

impl std::fmt::Display for Complex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.im == 0.0 {
            write!(f, "{}", self.re)
        } else if self.im < 0.0 {
            write!(f, "{}-{}", self.re, self.im)
        } else {
            write!(f, "{}+{}", self.re, self.im)
        }
    }
}
