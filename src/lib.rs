use std::cell::RefCell;
use std::marker::PhantomData;
use std::mem;
use std::ops::{BitAnd, BitOr, Deref, DerefMut, Shl, Shr};
use std::rc::{self, Rc, Weak};

/****** Parser ******/

type PResult<'a, X> = Result<(&'a str, X), ()>;

pub trait Parse {
    type Output;
    fn parse<'a>(&self, src: &'a str) -> PResult<'a, Self::Output>;
}

#[derive(Debug, Clone)]
pub struct Parser<P: Parse>(P);

impl<P: Parse> Parser<P> {
    pub fn parse<'a>(&self, src: &'a str) -> Option<P::Output> {
        match self.0.parse(src) {
            Err(()) => None,
            Ok((_, out)) => Some(out),
        }
    }
}

impl<P: Parse> Deref for Parser<P> {
    type Target = P;
    fn deref(&self) -> &P {
        &self.0
    }
}

impl<P: Parse> DerefMut for Parser<P> {
    fn deref_mut(&mut self) -> &mut P {
        &mut self.0
    }
}

/****** Combinators: Tokens ******/

#[derive(Debug, Clone)]
pub struct Token(&'static str);

#[derive(Debug, Clone)]
pub struct Eof();

pub fn tok(token: &'static str) -> Parser<Token> {
    Parser(Token(token))
}

pub fn eof() -> Parser<Eof> {
    Parser(Eof())
}

impl Parse for Token {
    type Output = ();
    fn parse<'a>(&self, src: &'a str) -> PResult<'a, ()> {
        if src.starts_with(self.0) {
            Ok((&src[self.0.len()..], ()))
        } else {
            Err(())
        }
    }
}

impl Parse for Eof {
    type Output = ();
    fn parse<'a>(&self, src: &'a str) -> PResult<'a, ()> {
        if src.is_empty() {
            Ok((src, ()))
        } else {
            Err(())
        }
    }
}

/****** Combinators: Sequencing ******/

#[derive(Debug, Clone)]
pub struct SeqBoth<P: Parse, Q: Parse>(P, Q);

#[derive(Debug, Clone)]
pub struct SeqLeft<P: Parse, Q: Parse>(P, Q);

#[derive(Debug, Clone)]
pub struct SeqRight<P: Parse, Q: Parse>(P, Q);

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

impl<P: Parse, Q: Parse> Parse for SeqBoth<P, Q> {
    type Output = (P::Output, Q::Output);
    fn parse<'a>(&self, src: &'a str) -> PResult<'a, (P::Output, Q::Output)> {
        let (rest, left_out) = self.0.parse(src)?;
        let (rest, right_out) = self.1.parse(rest)?;
        Ok((rest, (left_out, right_out)))
    }
}

impl<P: Parse, Q: Parse> Parse for SeqLeft<P, Q> {
    type Output = P::Output;
    fn parse<'a>(&self, src: &'a str) -> PResult<'a, P::Output> {
        let (rest, left_out) = self.0.parse(src)?;
        let (rest, _) = self.1.parse(rest)?;
        Ok((rest, left_out))
    }
}

impl<P: Parse, Q: Parse> Parse for SeqRight<P, Q> {
    type Output = Q::Output;
    fn parse<'a>(&self, src: &'a str) -> PResult<'a, Q::Output> {
        let (rest, _) = self.0.parse(src)?;
        let (rest, right_out) = self.1.parse(rest)?;
        Ok((rest, right_out))
    }
}

/****** Combinators: Choice ******/

#[derive(Debug, Clone)]
pub struct Alt<P: Parse, Q: Parse>(P, Q);

impl<X, P: Parse<Output = X>, Q: Parse<Output = X>> BitOr<Parser<Q>> for Parser<P> {
    type Output = Parser<Alt<P, Q>>;
    fn bitor(self, rhs: Parser<Q>) -> Parser<Alt<P, Q>> {
        Parser(Alt(self.0, rhs.0))
    }
}

impl<X, P: Parse<Output = X>, Q: Parse<Output = X>> Parse for Alt<P, Q> {
    type Output = X;
    fn parse<'a>(&self, src: &'a str) -> PResult<'a, X> {
        match self.0.parse(src) {
            Ok(ok) => Ok(ok),
            Err(()) => self.1.parse(src),
        }
    }
}

/****** Combinators: Recursion ******/

#[derive(Clone)]
pub struct Knot<X>(Rc<RefCell<Option<Box<dyn Parse<Output = X>>>>>);

pub fn knot<X>() -> Parser<Knot<X>> {
    Parser(Knot(Rc::new(RefCell::new(None))))
}

impl<X> Knot<X> {
    pub fn tie<P: Parse<Output = X> + 'static>(&mut self, parser: Parser<P>) {
        let mut opt = self.0.borrow_mut();
        opt.replace(Box::new(parser.0));
    }
}

impl<X> Parse for Knot<X> {
    type Output = X;
    fn parse<'a>(&self, src: &'a str) -> PResult<'a, X> {
        if let Some(parser) = &*self.0.borrow() {
            parser.parse(src)
        } else {
            panic!("Untied knot!");
        }
    }
}

/****** Combinators: Repetition ******/

#[derive(Debug, Clone)]
pub struct Optional<P: Parse>(P);

pub fn opt<P: Parse>(parser: Parser<P>) -> Parser<Optional<P>> {
    Parser(Optional(parser.0))
}

impl<P: Parse> Parse for Optional<P> {
    type Output = Option<P::Output>;
    fn parse<'a>(&self, src: &'a str) -> PResult<'a, Option<P::Output>> {
        println!("parse opt");
        match self.0.parse(src) {
            Ok((rest, out)) => Ok((rest, Some(out))),
            Err(()) => Ok((src, None)),
        }
    }
}

#[test]
fn test_parser() {
    let t = tok("foo") & tok("b");
    let mut o = opt(t);
    println!("{:#?}", o.parse("foobar"));
    let mut parens = knot();
    let contents =
        tok("O") | tok("(") >> parens.clone() << tok(")") | tok("[") >> parens.clone() << tok("]");
    parens.tie(contents);
    println!("{:#?}", parens.parse("(O)after"));
    println!("{:#?}", parens.parse("(((O)))"));
    println!("{:#?}", parens.parse("((O)O)"));
    println!("{:#?}", parens.parse("(())"));
    panic!("Boom!");
}
