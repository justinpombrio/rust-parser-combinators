use crate::types::*;
use std::ops::{BitAnd, BitOr, Shl, Shr};

pub fn tok(token: &'static str) -> Parser<Token> {
    Parser(Token(token))
}

pub fn regex<X>(re: &'static str, f: fn(&str) -> Option<X>) -> Parser<Regex<X>> {
    Parser(Regex::new(re, f))
}

pub fn eof() -> Parser<Eof> {
    Parser(Eof())
}

impl<P: Parse, Q: Parse> BitAnd<Parser<Q>> for Parser<P> {
    type Output = Parser<SeqBoth<P, Q>>;
    fn bitand(self, rhs: Parser<Q>) -> Parser<SeqBoth<P, Q>> {
        Parser(SeqBoth(self.0, rhs.0))
    }
}

impl<P: Parse, Q: Parse> Shl<Parser<Q>> for Parser<P> {
    type Output = Parser<SeqLeft<P, Q>>;
    fn shl(self, rhs: Parser<Q>) -> Parser<SeqLeft<P, Q>> {
        Parser(SeqLeft(self.0, rhs.0))
    }
}

impl<P: Parse, Q: Parse> Shr<Parser<Q>> for Parser<P> {
    type Output = Parser<SeqRight<P, Q>>;
    fn shr(self, rhs: Parser<Q>) -> Parser<SeqRight<P, Q>> {
        Parser(SeqRight(self.0, rhs.0))
    }
}

impl<X, P: Parse<Output = X>, Q: Parse<Output = X>> BitOr<Parser<Q>> for Parser<P> {
    type Output = Parser<Alt<P, Q>>;
    fn bitor(self, rhs: Parser<Q>) -> Parser<Alt<P, Q>> {
        Parser(Alt(self.0, rhs.0))
    }
}

pub fn opt<P: Parse>(parser: Parser<P>) -> Parser<Optional<P>> {
    Parser(Optional(parser.0))
}

pub fn rep<P: Parse>(parser: Parser<P>) -> Parser<Repeat<P>> {
    Parser(Repeat(parser.0))
}

impl<X, P: Parse<Output = X>> Parser<P> {
    pub fn map<Y>(self, f: fn(X) -> Y) -> Parser<Map<X, Y, P>> {
        Parser(Map(self.0, f))
    }
}

pub fn knot<X>() -> Parser<Knot<X>> {
    Parser(Knot::new())
}

impl<X> Knot<X> {
    pub fn tie<P: Parse<Output = X> + 'static>(&mut self, parser: Parser<P>) {
        let mut opt = self.0.borrow_mut();
        opt.replace(Box::new(parser.0));
    }
}

impl<X, P: Parse<Output = X> + 'static> Parser<P> {
    pub fn boxed(self) -> Parser<Boxed<X>> {
        Parser(Boxed(Box::new(self.0)))
    }
}
