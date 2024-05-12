use std::iter::once;

use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::PrattParser;
use pest::Parser;

#[derive(Parser)]
#[grammar = "./grammar.pest"]
pub struct JsParser;

lazy_static::lazy_static! {
    static ref PRATT_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
            // Addition and subtract have equal precedence
            .op(Op::infix(add, Left) | Op::infix(subtract, Left))
            .op(Op::infix(multiply, Left) | Op::infix(divide, Left))
            .op(Op::infix(equal, Left) | Op::infix(not_equal, Left)
                | Op::infix(less_than, Left) | Op::infix(less_equal, Left)
                | Op::infix(greater_than, Left) | Op::infix(greater_equal, Left))
    };
}

pub fn parse_file<'a>(
    input: &'a str,
) -> Result<pest::iterators::Pairs<'a, Rule>, pest::error::Error<Rule>> {
    JsParser::parse(Rule::program, input)
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Addition,
    Substract,
    Divide,
    Multiply,

    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
}

#[derive(Debug, Clone)]
pub enum JsLiteral {
    Boolean(bool),
    Number(f64),
    String(Box<str>),
}

#[derive(Debug, Clone)]
pub enum JsExpr {
    Variable(Box<str>),

    Access(Box<JsExpr>, Box<JsExpr>),
    Call(Box<JsExpr>, Vec<JsExpr>),

    Literal(JsLiteral),
    Binary(BinaryOp, Box<JsExpr>, Box<JsExpr>),
}

#[derive(Debug, Clone)]
pub enum DeclarationKind {
    Const,
    Let,
}

#[derive(Debug, Clone)]
pub enum JsStatement {
    Assignment(Box<str>, JsExpr),
    Declaration(DeclarationKind, Box<str>, JsExpr),
    If(JsExpr, Vec<JsStatement>, Option<Vec<JsStatement>>),
    While(JsExpr, Vec<JsStatement>),
    Expr(JsExpr),
    Break,
}

pub fn generate_tree(tokens: Pairs<'_, Rule>) -> Vec<JsStatement> {
    let mut out = Vec::with_capacity(tokens.len());

    for token in tokens {
        let token = match token.as_rule() {
            Rule::statement => token.into_inner().next().unwrap(),
            _ => token,
        };

        out.push(match token.as_rule() {
            Rule::EOI
            | Rule::comment
            | Rule::doc_comment
            | Rule::line_comment
            | Rule::block_comment => continue,

            Rule::let_declaration => {
                let mut inner = token.into_inner();
                let ident = inner.next().expect("Variable");
                let expr = inner.next().expect("Expression");

                JsStatement::Declaration(
                    DeclarationKind::Let,
                    ident.as_str().into(),
                    parse_expr(expr.into_inner()),
                )
            }
            Rule::const_declaration => {
                let mut inner = token.into_inner();
                let ident = inner.next().expect("Variable");
                let expr = inner.next().expect("Expression");

                JsStatement::Declaration(
                    DeclarationKind::Const,
                    ident.as_str().into(),
                    parse_expr(expr.into_inner()),
                )
            }

            Rule::while_loop => {
                let mut inner = token.into_inner();
                let cond = inner.next().expect("Condition");
                let block = inner.next().expect("Body");

                JsStatement::While(
                    parse_expr(vec![cond].into_iter()),
                    generate_tree(block.into_inner()),
                )
            }

            Rule::if_stmt => {
                let mut inner = token.into_inner();
                let cond = inner.next().expect("Condition");
                let block = inner.next().expect("Body");
                let otherwise = inner.next();

                JsStatement::If(
                    parse_expr(vec![cond].into_iter()),
                    generate_tree(block.into_inner()),
                    otherwise.map(|x| generate_tree(x.into_inner())),
                )
            }

            Rule::assignment => {
                let mut inner = token.into_inner();
                let ident = inner.next().expect("Variable");
                let expr = inner.next().expect("Expression");

                JsStatement::Assignment(ident.as_str().into(), parse_expr(once(expr)))
            }

            Rule::expr => JsStatement::Expr(parse_expr(vec![token].into_iter())),
            Rule::break_ => JsStatement::Break,

            _ => unreachable!("{token:#?}"),
        })
    }

    out
}

pub fn parse_expr<'a>(tokens: impl Iterator<Item = Pair<'a, Rule>>) -> JsExpr {
    PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::literal_expr => JsExpr::Literal({
                let literal = primary.into_inner().next().unwrap();
                match literal.as_rule() {
                    Rule::number => JsLiteral::Number(literal.as_str().parse::<f64>().unwrap()),
                    Rule::string => JsLiteral::String(
                        literal
                            .into_inner()
                            .next()
                            .unwrap()
                            .as_span()
                            .as_str()
                            .into(),
                    ),
                    Rule::boolean => JsLiteral::Boolean(literal.as_str() == "true"),
                    rule => unreachable!("Expr::parse expected atom, found {:?}", rule),
                }
            }),
            Rule::ident => JsExpr::Variable(primary.as_str().into()),
            Rule::access_expr => {
                let mut inner = primary.into_inner();
                let obj = inner.next().expect("From");
                let prop = inner.next().expect("Prop");

                JsExpr::Access(
                    parse_expr(vec![obj].into_iter()).into(),
                    parse_expr(vec![prop].into_iter()).into(),
                )
            }
            Rule::access_ident => {
                let mut inner = primary.into_inner();
                let obj = inner.next().expect("From");
                let prop = inner.next().expect("Prop");

                JsExpr::Access(
                    parse_expr(vec![obj].into_iter()).into(),
                    JsExpr::Literal(JsLiteral::String(prop.as_str().into())).into(),
                )
            }
            Rule::call => {
                let mut inner = primary.into_inner();
                let obj = inner.next().expect("Caller");
                let mut args = Vec::with_capacity(inner.len());

                for arg in inner {
                    args.push(parse_expr(once(arg)));
                }

                JsExpr::Call(parse_expr(vec![obj].into_iter()).into(), args)
            }
            Rule::expr => parse_expr(primary.into_inner()),
            rule => unreachable!("Expr::parse expected atom, found {:?}", rule),
        })
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_rule() {
                Rule::add => BinaryOp::Addition,
                Rule::subtract => BinaryOp::Substract,
                Rule::multiply => BinaryOp::Multiply,
                Rule::divide => BinaryOp::Divide,

                Rule::equal => BinaryOp::Equal,
                Rule::not_equal => BinaryOp::NotEqual,
                Rule::less_than => BinaryOp::LessThan,
                Rule::less_equal => BinaryOp::LessEqual,
                Rule::greater_than => BinaryOp::GreaterThan,
                Rule::greater_equal => BinaryOp::GreaterEqual,

                rule => unreachable!("Expr::parse expected infix operation, found {:?}", rule),
            };
            JsExpr::Binary(op, Box::new(lhs), Box::new(rhs))
        })
        .parse(tokens)
}
