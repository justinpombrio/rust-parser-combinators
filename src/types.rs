use regex::Regex as ActualRegex;
use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

/****** Parser ******/

type PResult<'a, X> = Result<(&'a str, X), ()>;

pub trait Parse {
    type Output;
    fn parse<'a>(&self, src: &'a str) -> PResult<'a, Self::Output>;
}

#[derive(Debug, Clone)]
pub struct Parser<P: Parse>(pub P);

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
pub struct Token(pub &'static str);

#[derive(Clone)]
pub struct Regex<X>(pub ActualRegex, pub fn(&str) -> Option<X>);

#[derive(Debug, Clone)]
pub struct Eof();

impl<X> Regex<X> {
    pub fn new(re: &'static str, f: fn(&str) -> Option<X>) -> Regex<X> {
        Regex(ActualRegex::new(re).unwrap(), f)
    }
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

impl<X> Parse for Regex<X> {
    type Output = X;
    fn parse<'a>(&self, src: &'a str) -> PResult<'a, X> {
        if let Some(span) = self.0.find(src) {
            if span.start() == 0 {
                let matched = &src[0..span.end()];
                let output = self.1(matched);
                if let Some(output) = output {
                    return Ok((&src[span.end()..], output));
                }
            }
        }
        Err(())
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
pub struct SeqBoth<P: Parse, Q: Parse>(pub P, pub Q);

#[derive(Debug, Clone)]
pub struct SeqLeft<P: Parse, Q: Parse>(pub P, pub Q);

#[derive(Debug, Clone)]
pub struct SeqRight<P: Parse, Q: Parse>(pub P, pub Q);

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
pub struct Alt<P: Parse, Q: Parse>(pub P, pub Q);

impl<X, P: Parse<Output = X>, Q: Parse<Output = X>> Parse for Alt<P, Q> {
    type Output = X;
    fn parse<'a>(&self, src: &'a str) -> PResult<'a, X> {
        match self.0.parse(src) {
            Ok(ok) => Ok(ok),
            Err(()) => self.1.parse(src),
        }
    }
}

/****** Combinators: Repetition ******/

#[derive(Debug, Clone)]
pub struct Optional<P: Parse>(pub P);

#[derive(Debug, Clone)]
pub struct Repeat<P: Parse>(pub P);

impl<P: Parse> Parse for Optional<P> {
    type Output = Option<P::Output>;
    fn parse<'a>(&self, src: &'a str) -> PResult<'a, Option<P::Output>> {
        match self.0.parse(src) {
            Ok((rest, out)) => Ok((rest, Some(out))),
            Err(()) => Ok((src, None)),
        }
    }
}

impl<P: Parse> Parse for Repeat<P> {
    type Output = Vec<P::Output>;
    fn parse<'a>(&self, src: &'a str) -> PResult<'a, Vec<P::Output>> {
        let mut src = src;
        let mut outputs = vec![];
        while let Ok((rest, out)) = self.0.parse(src) {
            src = rest;
            outputs.push(out);
        }
        Ok((src, outputs))
    }
}

/****** Combinators: Transformation ******/

#[derive(Debug, Clone)]
pub struct Map<X, Y, P: Parse<Output = X>>(pub P, pub fn(X) -> Y);

impl<X, Y, P: Parse<Output = X>> Parse for Map<X, Y, P> {
    type Output = Y;
    fn parse<'a>(&self, src: &'a str) -> PResult<'a, Y> {
        self.0.parse(src).map(|(rest, out)| (rest, self.1(out)))
    }
}

/****** Combinators: Recursion ******/

#[derive(Clone)]
pub struct Knot<X>(pub Rc<RefCell<Option<Box<dyn Parse<Output = X>>>>>);

impl<X> Knot<X> {
    pub fn new() -> Knot<X> {
        Knot(Rc::new(RefCell::new(None)))
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
