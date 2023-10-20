use std::{borrow::Cow, iter::Peekable, ops::Index, str::Chars, vec};

use num_complex::ComplexFloat;

struct Tokenizer<'a> {
    offset: usize,
    program: Cow<'a, str>,
}

pub enum LexItem {
    Paren(char),
    Op(char),
    Num(i64),
}

impl<'a> Tokenizer<'a> {
    pub fn new(program: &'a str) -> Self {
        Tokenizer {
            offset: 0,
            program: Cow::Borrowed(program),
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        let punctuators = vec!['{', '}', '+', '-', '*', '/', '\n', ' '];
        let mut acc = String::from("");
        let mut index = self.offset;
        let mut c = self.program.index(index);
        match c {
            '{' | '}' | '+' | '-' | '*' | '/' | '\n' | ' ' | '(' | ')' | '=' => {
                self.offset = index + 1;
                Some(String::from(c))
            }
            _ => {
                while !punctuators.contains(&self.program.index(index))
                    && index < self.program.len()
                {
                    c = self.program.as_bytes()[index] as char;
                    acc.push(c);
                    index = index + 1;
                }
                self.offset = index;
                Some(acc)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Tokenizer;

    #[test]
    fn tokenizer_works() {
        let program = "let x = 5+10*(10+9)";
        let tokens = Tokenizer::new(program);
        for t in tokens {
            println!("{}", t);
        }
    }
}
