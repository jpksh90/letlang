use std::{borrow::Cow, iter::Peekable};

struct Tokenizer<'a> {
    offset: usize,
    program: &'a str,
    curr_token: Option<Cow<'a, str>>,
    next_token: Option<Cow<'a, str>>,
}

impl<'a> Tokenizer<'a> {
    fn new(program: &'a str) -> Self {
        Tokenizer {
            offset: 0,
            program: program,
            curr_token: None,
            next_token: None,
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Option<Cow<'a, str>>;

    fn next(&mut self) -> Option<Self::Item> {}
}

#[cfg(test)]
mod tests {

    #[test]
    fn tokenizer_works() {
        let program = "let x = 5+10*(10+9)";
        let tokens = Tokenizer::new(program);
        for t in tokens {
            println!("{}", t);
        }
    }
}
