extern crate nom;

#[cfg(test)]
mod tests {

    use nom::{
        branch::alt,
        bytes::complete::{tag, take_while_m_n},
        character::complete::{
            alpha0, alpha1, alphanumeric0, char, digit1, one_of, space0, space1,
        },
        combinator::map,
        combinator::map_res,
        combinator::opt,
        multi::fold_many0,
        multi::separated_list,
        multi::separated_nonempty_list,
        sequence::delimited,
        sequence::pair,
        sequence::preceded,
        sequence::separated_pair,
        sequence::terminated,
        sequence::tuple,
        IResult,
    };

    #[derive(Debug, PartialEq)]
    pub struct Color {
        pub red: u8,
        pub green: u8,
        pub blue: u8,
    }
    /* fn ws(input : &str) -> IResult<&str, char> { one_of("\t\n\r ")(input) } */

    fn from_hex(input: &str) -> Result<u8, std::num::ParseIntError> {
        u8::from_str_radix(input, 16)
    }

    fn is_hex_digit(c: char) -> bool {
        c.is_digit(16)
    }

    fn hex_primary(input: &str) -> IResult<&str, u8> {
        map_res(take_while_m_n(2, 2, is_hex_digit), from_hex)(input)
    }

    fn hex_color(input: &str) -> IResult<&str, Color> {
        let (input, _) = tag("#")(input)?;
        let (input, (red, green, blue)) = tuple((hex_primary, hex_primary, hex_primary))(input)?;

        Ok((input, Color { red, green, blue }))
    }

    fn parse_parens(input: &str) -> IResult<&str, i64> {
        let lparen = tag("(");
        let rparen = tag(")");
        let (input, _) = lparen(input)?;
        let (input, n) = opt(parse_parens)(input)?;
        let n = match n {
            Some(n) => n + 1,
            None => 1,
        };
        let (input, _) = rparen(input)?;

        return Ok((input, n));
    }
    fn parse_parens2(input: &str) -> IResult<&str, i64> {
        return map(
            tuple((char('('), opt(parse_parens2), char(')'))),
            |(_, n, _)| match n {
                Some(n) => n + 1,
                None => 1,
            },
        )(input);
    }
    // Syntax (  ) is a new lambda context. side by side is application (left associative)
    //
    #[derive(Debug, Eq, PartialEq, Clone)]
    pub enum Lambo {
        Lam(Box<Lambo>),
        Var(i64),
        App(Box<Lambo>, Box<Lambo>),
    }
    pub use self::Lambo::*;

    fn parse_list(input: &str) -> IResult<&str, Vec<&str>> {
        delimited(char('('), separated_list(space1, digit1), char(')'))(input) //  alt((digit1, parse_lisp))
    }

    enum Term {
        Lam(Rc<dyn Fn(Term) -> Term>),
        Var(usize),
        App(Box<Term>, Box<Term>),
    }
    /*

    fn reduce(t : Term) -> Term {
        match t {
            App(f, x) => match reduce(*f) {
                Lam(f2) => *f2(reduce(*x)),
                Var(z) => App(Var(z0), reduce(*x),

            }
        }
    } */

    #[derive(Debug, PartialEq, Eq)]
    enum Type {
        TFree(Name),
        Fun(Box<Type>, Box<Type>),
    }
    #[derive(Debug, PartialEq, Eq)]
    enum TermI {
        Ann(Box<TermC>, Type),
        Bound(i64),
        Free(Name),
        App(Box<TermI>, Box<TermC>),
    }
    #[derive(Debug, PartialEq, Eq)]
    enum TermC {
        Inf(TermI),
        Lam(Box<TermC>),
    }
    // https://stackoverflow.com/questions/51182640/is-it-possible-to-represent-higher-order-abstract-syntax-in-rust
    use self::Name::*;
    use self::Neutral::*;
    use self::TermC::*;
    use self::TermI::*;
    use self::Type::*;
    use self::Value::*;
    use std::rc::Rc;

    #[derive(Debug, PartialEq, Eq)]
    enum Name {
        Global(String),
        Local(i64),
        Quote(i64),
    }

    enum Neutral {
        NFree(Name),
        NApp(Box<Neutral>, Box<Value>),
    }
    enum Value {
        VLam(Rc<dyn Fn(Value) -> Value>),
        VNeutral(Box<Neutral>),
    }
    fn vfree(n: Name) -> Value {
        VNeutral(Box::new(NFree(n)))
    }
    type Env = Vec<Value>;
    /*
    fn evalI(t : TermI, e : Env) -> Value {
        match t {


        }
    } */
    fn vapp(f: Value, v: Value) -> Value {
        match f {
            VLam(f) => f(v),
            VNeutral(n) => VNeutral(Box::new(NApp(n, Box::new(v)))),
        }
    }
    /*
    fn parse_lam(input : &str) {

    } */
    /*
    fn parse_lambo(input : &str) -> IResult<&str, Lambo> {
        let lam =  ;
        let var =  map(digit1, | n | Var(0));
        map(delimited( char('('), parse_lambo , char(')')), |x| Lam(Box::new(x)))(input)
        let app =  map(separated_pair( alt((lam,var)) , space0, parse_lambo), |(f,x)| App(Box::new(f), Box::new(x)) );
        alt((app, lam, var))(input)
    }


    #[test]
    fn parse_lambo_test() {
        assert_eq!(
            parse_lambo("(0)(0)"),
            Ok((
                "",
                App(Box::new(Lam(Box::new(Var(0)))), Box::new(Lam(Box::new(Var(0)))))
            ))
        );
    }
    */
    fn lam(t: Lambo) -> Lambo {
        Lambo::Lam(Box::new(t))
    }
    fn app(t: Lambo, v: Lambo) -> Lambo {
        Lambo::App(Box::new(t), Box::new(v))
    }
    fn parse_lam(input: &str) -> IResult<&str, Lambo> {
        let head = preceded(char('\\'), preceded(space0, alpha1));
        let tail = preceded(space0, preceded(tag("->"), parse_lambo));
        map(pair(head, tail), |(v, b)| lam(b))(input)
    }
    fn parse_lambo_parens(input: &str) -> IResult<&str, Lambo> {
        delimited(char('('), parse_lambo, char(')'))(input)
    }
    fn parse_nonapp(input: &str) -> IResult<&str, Lambo> {
        let parse_var = map(alpha1, |x| Var(0)); // placeholder
        alt((parse_lambo_parens, parse_lam, parse_var))(input)
    }

    fn parse_lambo(input: &str) -> IResult<&str, Lambo> {
        let (input, n) = terminated(preceded(space0, parse_nonapp), space0)(input)?;
        // let (input, n) = preceded(space0, parse_nonapp)(input)?; */
        fold_many0(terminated(parse_nonapp, space0), n, |t, e| app(t, e))(input)
    }

    #[test]
    fn parse_lambo_test() {
        assert_eq!(
            parse_lambo("  (\\ x -> x)  (\\ z -> z )  "),
            Ok(("", app(lam(Var(0)), lam(Var(0)))))
        );
    }
    #[derive(Eq, Debug, PartialEq)]
    enum AExp {
        // Arithemtic expression
        Plus(Vec<AExp>),
        Times(Vec<AExp>),
        Const(i64),
    }

    fn parse_const(input: &str) -> IResult<&str, AExp> {
        map_res(digit1, |n: &str| -> Result<AExp, _> {
            n.parse().map(AExp::Const)
        })(input)
        /* let (i, n) = digit1(input)?;
        let n2 = n.parse::<i64>();
        match n2 {
            Ok(n) => Ok((i, AExp::Const(n))),
            Err(err) => Ok((i, AExp::Const(0))) // Screwy. Why didn't map_res work?
        } */
    }
    fn parse_parens_aexp(input: &str) -> IResult<&str, AExp> {
        delimited(char('('), parse_aexp, char(')'))(input)
    }
    fn parse_times(input: &str) -> IResult<&str, AExp> {
        map(
            separated_nonempty_list(
                terminated(char('*'), space0),
                terminated(alt((parse_const, parse_parens_aexp)), space0),
            ),
            |mut v| {
                if v.len() == 1 {
                    v.remove(0)
                } else {
                    AExp::Times(v)
                }
            },
        )(input)
    }
    fn parse_plus(input: &str) -> IResult<&str, AExp> {
        map(
            separated_nonempty_list(
                terminated(char('+'), space0),
                terminated(alt((parse_parens_aexp, parse_times)), space0),
            ),
            |mut v| {
                if v.len() == 1 {
                    v.remove(0)
                } else {
                    AExp::Plus(v)
                }
            },
        )(input)
    }

    fn parse_aexp(input: &str) -> IResult<&str, AExp> {
        preceded(space0, alt((parse_parens_aexp, parse_plus)))(input)
    }

    fn test_aexp(s: &str, e: AExp) {
        assert_eq!(parse_aexp(s), Ok(("", e)));
    }

    fn eval_aexp(t: AExp) -> i64 {
        match t {
            AExp::Plus(v) => v.into_iter().map(eval_aexp).sum(),
            AExp::Times(v) => v.into_iter().map(eval_aexp).product(),
            AExp::Const(n) => n,
        }
    }
    #[test]

    #[test]
    fn test12() {
        assert_eq!(eval_aexp(parse_aexp("1 + 1 * 2 + (3 + (4))").unwrap().1 ), 10 );
    }

    #[test]
    fn test1() {
        test_aexp(
            "1 + 1 * 2 + (3 + (4))",
            AExp::Plus(vec![AExp::Const(0), AExp::Const(0)]),
        )
    }
    #[test]
    fn parse_list_test() {
        assert_eq!(parse_list("(1 23 3)"), Ok(("", vec!("1", "23", "3"))));
    }

    #[test]
    fn parse_paren_test2() {
        assert_eq!(parse_parens2("((()))"), Ok(("", 3)));
    }
    #[test]
    fn parse_paren_test() {
        assert_eq!(parse_parens("((()))"), Ok(("", 3)));
    }
    /*
    fn lambo_parse(input: &str) -> IResult<&str, ()> {
        /*let slash = tag("\\");
        let lparen = tag("(");
        let rparen = tag(")");
        let arr = tag("->"); */
        //let parens = many0( lparen rparen )
        return Ok((input, ()));
    } */

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
    /*#[test]
    fn it_fails(){
        assert!(false);
    } */
    #[test]
    fn parse_color() {
        assert_eq!(
            hex_color("#2F14DF"),
            Ok((
                "",
                Color {
                    red: 47,
                    green: 20,
                    blue: 223,
                }
            ))
        );
    }
}
