use amvm::tokens::{
    AmvmType, BinaryKind, Command, CommandExpression, Program, Value, VariableKind,
};
use amvm::Compilable;
use parser::{parse_file, BinaryOp, JsExpr, JsLiteral, Rule};
use pest::iterators::Pair;

use crate::parser::{generate_tree, JsStatement};

#[macro_use(Parser)]
extern crate pest_derive;

pub mod parser;

fn main() {
    let mut args = std::env::args().skip(1);
    let source_file = args.next().expect("Provide JS source file");
    let source = std::fs::read_to_string(&source_file).expect("Cannot read source file");

    let parsed = parse_file(&source)
        .map_err(|err| panic!("Cannot parse file: {err}"))
        .unwrap();

    // println!("{parsed:#?}");
    //
    // let mut buffer = String::new();
    // for token in parsed.clone() {
    //     buffer = print_rule(buffer, token);
    // }
    // println!("{buffer}");

    let ast = generate_tree(parsed);

    let mut buffer = vec![Command::Function {
        name: Box::from("log"),
        args: vec![(Box::from("obj"), VariableKind::Const, AmvmType::Anonymous)],
        ret: AmvmType::Named(Box::from("null")),
        body: vec![
            Command::Puts {
                value: CommandExpression::Var(String::from("obj")),
            },
            Command::Puts {
                value: CommandExpression::Value(Value::Char('\n')),
            },
        ],
    }];
    for node in ast {
        buffer = process_statement(buffer, node);
    }

    // println!("{buffer:#?}");

    // for node in &buffer {
    //     println!("{node}");
    // }

    let header = amvm::tokens::AmvmHeader {
        sum_kind: amvm::tokens::AmvmTypeCasting::TypeCastingStrictlessString,
    };
    let program = Program::new(header, buffer);
    let compiled = program.compile_bytecode(String::new()).unwrap();

    std::fs::write(format!("{source_file}.amb"), compiled).unwrap();

    // let mut runtime = amvm::runtime::Runtime::new(Box::from(source_file), header, buffer);
    // _ = runtime.run();

    // println!("{ast:#?}");
}

fn process_statement(mut buffer: Vec<Command>, node: JsStatement) -> Vec<Command> {
    let ret = match node {
        JsStatement::Assignment(name, value) => {
            buffer = process_expr(buffer, value);
            Command::AssignVariable {
                name,
                value: CommandExpression::Prev,
            }
        }
        JsStatement::Break => Command::Break,
        JsStatement::Declaration(kind, name, value) => {
            buffer = process_expr(buffer, value);

            Command::DeclareVariable {
                name,
                kind: match kind {
                    parser::DeclarationKind::Let => amvm::tokens::VariableKind::Var,
                    parser::DeclarationKind::Const => amvm::tokens::VariableKind::Mut,
                },
                value: CommandExpression::Prev,
            }
        }
        JsStatement::Expr(node) => return process_expr(buffer, node),
        JsStatement::If(cond, body, otherwise) => {
            buffer = process_expr(buffer, cond);

            let body = process_statements(vec![], body);
            let otherwise = otherwise.map(|x| process_statements(vec![], x));

            Command::Conditional {
                condition: CommandExpression::Prev,
                body,
                otherwise,
            }
        }
        JsStatement::While(cond, body) => {
            let mut body_buff = vec![];
            body_buff = process_expr(body_buff, cond);
            body_buff.push(Command::Conditional {
                condition: CommandExpression::Binary(
                    BinaryKind::Equal,
                    CommandExpression::Prev.into(),
                    CommandExpression::Value(Value::Bool(false)).into(),
                ),
                body: vec![Command::Break],
                otherwise: None,
            });

            let body = process_statements(body_buff, body);

            Command::Loop { body }
        }
    };
    buffer.push(ret);

    buffer
}

fn process_statements(mut buffer: Vec<Command>, node: Vec<JsStatement>) -> Vec<Command> {
    for node in node {
        buffer = process_statement(buffer, node);
    }
    buffer
}

fn process_expr(mut buffer: Vec<Command>, node: JsExpr) -> Vec<Command> {
    let value = match node {
        JsExpr::Call(caller, args) => {
            let caller_args = args.iter().map(|_| CommandExpression::Prev).collect();
            for arg in args.into_iter().rev() {
                buffer = process_expr(buffer, arg);
            }
            buffer = process_expr(buffer, *caller);
            buffer.push(Command::Call {
                name: CommandExpression::Prev,
                args: caller_args,
            });
            return buffer;
        }
        JsExpr::Variable(var) => CommandExpression::Var(var.into_string()),
        JsExpr::Literal(JsLiteral::Boolean(v)) => CommandExpression::Value(Value::Bool(v)),
        JsExpr::Literal(JsLiteral::Number(v)) => CommandExpression::Value(Value::F32(v as f32)),
        JsExpr::Literal(JsLiteral::String(v)) => {
            CommandExpression::Value(Value::String(v.into_string()))
        }
        JsExpr::Binary(kind, a, b) => {
            buffer = process_expr(buffer, *b);
            buffer = process_expr(buffer, *a);
            CommandExpression::Binary(
                match kind {
                    BinaryOp::Addition => BinaryKind::Add,
                    BinaryOp::Substract => BinaryKind::Sub,
                    BinaryOp::Multiply => BinaryKind::Mult,
                    BinaryOp::Divide => todo!("Divide operator"),
                    BinaryOp::Equal => BinaryKind::Equal,
                    BinaryOp::NotEqual => BinaryKind::NotEqual,
                    BinaryOp::GreaterThan => BinaryKind::GreaterThan,
                    BinaryOp::GreaterEqual => BinaryKind::GreaterThanEqual,
                    BinaryOp::LessThan => BinaryKind::LessThan,
                    BinaryOp::LessEqual => BinaryKind::LessThanEqual,
                },
                CommandExpression::Prev.into(),
                CommandExpression::Prev.into(),
            )
        }
        JsExpr::Access(from, to) => {
            buffer = process_expr(buffer, *to);
            buffer = process_expr(buffer, *from);
            CommandExpression::Property(
                CommandExpression::Prev.into(),
                CommandExpression::Prev.into(),
            )
        }
    };

    buffer.push(Command::Push { value });

    buffer
}

#[allow(unused)]
fn print_rule(mut buffer: String, token: Pair<'_, Rule>) -> String {
    use std::fmt::Write;

    let rule = token.as_rule();
    match rule {
        Rule::program | Rule::statement => {
            for token in token.into_inner() {
                buffer = print_rule(buffer, token);
                _ = buffer.write_str(";\n");
            }
        }
        Rule::break_ => {
            _ = write!(buffer, "break");
        }
        Rule::block => {
            _ = write!(buffer, "{{\n");
            for token in token.into_inner() {
                buffer = print_rule(buffer, token);
            }
            _ = write!(buffer, "}}");
        }

        Rule::assignment => {
            let mut inner = token.into_inner();
            let ident = inner.next().expect("Variable");
            let expr = inner.next().expect("Expression");

            buffer = print_rule(buffer, ident);
            _ = write!(buffer, " = ");
            buffer = print_rule(buffer, expr);
        }

        Rule::if_stmt => {
            let mut inner = token.into_inner();
            let cond = inner.next().expect("Condition");
            let block = inner.next().expect("Block");

            _ = write!(buffer, "if ");
            buffer = print_rule(buffer, cond);
            _ = write!(buffer, " ");
            buffer = print_rule(buffer, block);
        }

        Rule::while_loop => {
            let mut inner = token.into_inner();
            let cond = inner.next().expect("Condition");
            let block = inner.next().expect("Block");

            _ = write!(buffer, "while ");
            buffer = print_rule(buffer, cond);
            _ = write!(buffer, " ");
            buffer = print_rule(buffer, block);
        }

        Rule::let_declaration => {
            let mut inner = token.into_inner();
            let ident = inner.next().expect("Variable");
            let exprs = inner.next().expect("Expression");

            _ = write!(buffer, "let ");
            buffer = print_rule(buffer, ident);
            _ = write!(buffer, " = ");
            buffer = print_rule(buffer, exprs);
        }
        Rule::const_declaration => {
            let mut inner = token.into_inner();
            let ident = inner.next().expect("Variable");
            let exprs = inner.next().expect("Expression");

            _ = write!(buffer, "const ");
            buffer = print_rule(buffer, ident);
            _ = write!(buffer, " = ");
            buffer = print_rule(buffer, exprs);
        }

        Rule::ident => {
            let inner = token.as_span().as_str();
            _ = buffer.write_str(inner);
        }

        Rule::expr => {
            let inner = token.as_span().as_str();
            _ = buffer.write_str(inner);
        }

        Rule::EOI => {}

        _ => todo!("{rule:?}"),
    }

    buffer
}
