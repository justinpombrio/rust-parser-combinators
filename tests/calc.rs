use parser_combinators::combinators::*;
use std::str::FromStr;

#[test]
fn test_tokens() {
    let num = regex("0|[1-9][0-9]*", |n| u64::from_str(n).ok());
    assert_eq!(num.parse("18"), Some(18));
    assert_eq!(num.parse("018"), Some(0));
    assert_eq!(num.parse("-18"), None);
    assert_eq!(num.parse("18+"), Some(18));
    let or = tok("or");
    assert_eq!(or.parse("or"), Some(()));
    assert_eq!(or.parse("orb"), Some(()));
    assert_eq!(or.parse("nor"), None);
    assert_eq!(or.parse(""), None);
}

#[test]
fn test_seq() {
    let num = regex("0|[1-9][0-9]*", |n| u64::from_str(n).ok());
    let unit = tok("(") & tok(")");
    assert_eq!(unit.parse("()"), Some(((), ())));
    let negnum = tok("-") >> num.clone();
    assert_eq!(negnum.parse("-15"), Some(15));
    let parenum = tok("(") >> num.clone() << tok(")");
    assert_eq!(parenum.parse("(3)"), Some(3));
    let sum = num.clone() << tok("+") & num;
    assert_eq!(sum.parse("1+2"), Some((1, 2)));
}

#[test]
fn test_map() {
    let num = regex("0|[1-9][0-9]*", |n| u64::from_str(n).ok());
    let incr = num.clone().map(|x| x + 1);
    assert_eq!(incr.parse("3"), Some(4));
    let sum = num.clone() << tok("+") & num;
    assert_eq!(sum.parse("1+2"), Some((1, 2)));
    let add = sum.map(|(x, y)| x + y);
    assert_eq!(add.parse("1+2"), Some(3));
    assert_eq!(add.parse("1+2!"), Some(3));
    let just_add = add << eof();
    assert_eq!(just_add.parse("1+2"), Some(3));
    assert_eq!(just_add.parse("1+2!"), None);
}

#[test]
fn test_choice() {
    let num = tok("11").map(|()| 11) | tok("12").map(|()| 12);
    assert_eq!(num.parse("111"), Some(11));
    assert_eq!(num.parse("121"), Some(12));
    assert_eq!(num.parse("13112"), None);
    let just_num = num << eof();
    assert_eq!(just_num.parse("111"), None);
    assert_eq!(just_num.parse("121"), None);
    assert_eq!(just_num.parse("11"), Some(11));
    assert_eq!(just_num.parse("12"), Some(12));
}

#[test]
fn test_parser() {
    let mut expn = knot();
    let mut mult = knot();
    let mut add = knot();

    let num = regex("0|[1-9][0-9]*", |n| u64::from_str(n).ok());
    let atom = tok("(") >> add.clone() << tok(")") | num.clone();
    let parse_expn = (atom.clone() << tok("^") & expn.clone()).map(|(x, y)| x.pow(y as u32)) | atom;
    let parse_mult = (expn.clone() << tok("*") & mult.clone()).map(|(x, y)| x * y) | expn.clone();
    let parse_add = (mult.clone() << tok("+") & add.clone()).map(|(x, y)| x + y) | mult.clone();
    let expr = add.clone() << eof();

    expn.tie(parse_expn);
    mult.tie(parse_mult);
    add.tie(parse_add);

    assert_eq!(expr.parse("16"), Some(16));
    assert_eq!(expr.parse("1+2"), Some(3));
    assert_eq!(expr.parse("2^3"), Some(8));
    assert_eq!(expr.parse("(1+2)"), Some(3));
    assert_eq!(expr.parse("1+2^(1+2)"), Some(9));
    assert_eq!(expr.parse("1+2+3+4"), Some(10));
    assert_eq!(expr.parse("(1+1^2)*(1+2^2)*(1+3^2)"), Some(100));
}
