use crate::types::{AST, JSValue, Number};
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ident(String),
    Number(f64),
    String(String),
    LParen,
    RParen,
    LBracket,
    RBracket,
    Semicolon,
    Comma,
    Colon,
}

impl Token {
    fn to_operator(&self, binary: bool) -> Option<Operator> {
	if let Token::Ident(s) = self {
	    Operator::from_str(s, binary).ok()
	} else {
	    None
	}
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Operator {
    Period,
    Equal,
    DoubleEqual,
    TripleEqual,
    Inequal,
    DoubleInequal,
    LT,
    LE,
    GT,
    GE,
    Exponentiation,
    Multiplication,
    Division,
    Remainder,
    Addition,
    Subtraction,
    LeftShift,
    RightShift,
    UnsignedRightShift,
    In,
    InstanceOf,
    BitwiseAND,
    BitwiseOR,
    BitwiseXOR,
    LogicalAND,
    LogicalOR,
    Nullish,
    AdditionAssignment,
    SubtractionAssignment,
    ExponentiationAssignment,
    MultiplicationAssignment,
    DivisionAssignment,
    RemainderAssignment,
    LeftShiftAssignment,
    RightShiftAssignment,
    UnsignedRightShiftAssignment,
    BitwiseANDAssignment,
    BitwiseORAssignment,
    BitwiseXORAssignment,
    LogicalANDAssignment,
    LogicalORAssignment,
    NullishAssignment,
    Arrow,
    // prefix unary operators
    New,
    PrefixIncrement,
    PrefixDecrement,
    LogicalNot,
    BitwiseNot,
    UnaryPlus,
    UnaryMinus,
    TypeOf,
    Void,
    Delete,
    Await,
    Yield,
    YieldStar,
    Spread
}

impl Operator {
    fn precedence(&self) -> u8 {
	match self {
	    Operator::Period => 17,
	    Operator::Equal => 2,
	    Operator::DoubleEqual => 8,
	    Operator::TripleEqual => 8,
	    Operator::Inequal => 8,
	    Operator::DoubleInequal => 8,
	    Operator::LT => 9,
	    Operator::LE => 9,
	    Operator::GT => 9,
	    Operator::GE => 9,
	    Operator::Exponentiation => 13,
	    Operator::Multiplication => 12,
	    Operator::Division => 12,
	    Operator::Remainder => 12,
	    Operator::Addition => 11,
	    Operator::Subtraction => 11,
	    Operator::LeftShift => 10,
	    Operator::RightShift => 10,
	    Operator::UnsignedRightShift => 10,
	    Operator::In => 9,
	    Operator::InstanceOf => 9,
	    Operator::BitwiseAND => 7,
	    Operator::BitwiseOR => 6,
	    Operator::BitwiseXOR => 5,
	    Operator::LogicalAND => 4,
	    Operator::LogicalOR => 3,
	    Operator::Nullish => 3,
	    Operator::AdditionAssignment => 2,
	    Operator::SubtractionAssignment => 2,
	    Operator::ExponentiationAssignment => 2,
	    Operator::MultiplicationAssignment => 2,
	    Operator::DivisionAssignment => 2,
	    Operator::RemainderAssignment => 2,
	    Operator::LeftShiftAssignment => 2,
	    Operator::RightShiftAssignment => 2,
	    Operator::UnsignedRightShiftAssignment => 2,
	    Operator::BitwiseANDAssignment => 2,
	    Operator::BitwiseORAssignment => 2,
	    Operator::BitwiseXORAssignment => 2,
	    Operator::LogicalANDAssignment => 2,
	    Operator::LogicalORAssignment => 2,
	    Operator::NullishAssignment => 2,
	    Operator::Arrow => 2,
	    Operator::New => 17,
	    Operator::PrefixIncrement => 14,
	    Operator::PrefixDecrement => 14,
	    Operator::LogicalNot => 14,
	    Operator::BitwiseNot => 14,
	    Operator::UnaryPlus => 14,
	    Operator::UnaryMinus => 14,
	    Operator::TypeOf => 14,
	    Operator::Void => 14,
	    Operator::Delete => 14,
	    Operator::Await => 14,
	    Operator::Yield => 2,
	    Operator::YieldStar => 2,
	    Operator::Spread => 2
	}
    }
    fn left_associative(&self) -> bool {
	match self {
	    Operator::Period => true,
	    Operator::Equal => false,
	    Operator::DoubleEqual => true,
	    Operator::TripleEqual => true,
	    Operator::Inequal => true,
	    Operator::DoubleInequal => true,
	    Operator::LT => true,
	    Operator::LE => true,
	    Operator::GT => true,
	    Operator::GE => true,
	    Operator::Exponentiation => false,
	    Operator::Multiplication => true,
	    Operator::Division => true,
	    Operator::Remainder => true,
	    Operator::Addition => true,
	    Operator::Subtraction => true,
	    Operator::LeftShift => true,
	    Operator::RightShift => true,
	    Operator::UnsignedRightShift => true,
	    Operator::In => true,
	    Operator::InstanceOf => true,
	    Operator::BitwiseAND => true,
	    Operator::BitwiseOR => true,
	    Operator::BitwiseXOR => true,
	    Operator::LogicalAND => true,
	    Operator::LogicalOR => true,
	    Operator::Nullish => true,
	    Operator::AdditionAssignment => false,
	    Operator::SubtractionAssignment => false,
	    Operator::ExponentiationAssignment => false,
	    Operator::MultiplicationAssignment => false,
	    Operator::DivisionAssignment => false,
	    Operator::RemainderAssignment => false,
	    Operator::LeftShiftAssignment => false,
	    Operator::RightShiftAssignment => false,
	    Operator::UnsignedRightShiftAssignment => false,
	    Operator::BitwiseANDAssignment => false,
	    Operator::BitwiseORAssignment => false,
	    Operator::BitwiseXORAssignment => false,
	    Operator::LogicalANDAssignment => false,
	    Operator::LogicalORAssignment => false,
	    Operator::NullishAssignment => false,
	    Operator::Arrow => false,
	    Operator::New => false,
	    Operator::PrefixIncrement => false,
	    Operator::PrefixDecrement => false,
	    Operator::LogicalNot => false,
	    Operator::BitwiseNot => false,
	    Operator::UnaryPlus => false,
	    Operator::UnaryMinus => false,
	    Operator::TypeOf => false,
	    Operator::Void => false,
	    Operator::Delete => false,
	    Operator::Await => false,
	    Operator::Yield => false,
	    Operator::YieldStar => false,
	    Operator::Spread => false
	}
    }

    fn from_str(s: &str, binary: bool) -> Result<Operator, String> {
	if binary {
	    match s {
		"." => Ok(Operator::Period),
		"=" => Ok(Operator::Equal),
		"==" => Ok(Operator::DoubleEqual),
		"===" => Ok(Operator::TripleEqual),
		"!=" => Ok(Operator::Inequal),
		"!==" => Ok(Operator::DoubleInequal),
		"<" => Ok(Operator::LT),
		"<=" => Ok(Operator::LE),
		">" => Ok(Operator::GT),
		">=" => Ok(Operator::GE),
		"**" => Ok(Operator::Exponentiation),
		"*" => Ok(Operator::Multiplication),
		"/" => Ok(Operator::Division),
		"%" => Ok(Operator::Remainder),
		"+" => Ok(Operator::Addition),
		"-" => Ok(Operator::Subtraction),
		"<<" => Ok(Operator::LeftShift),
		">>" => Ok(Operator::RightShift),
		">>>" => Ok(Operator::UnsignedRightShift),
		"in" => Ok(Operator::In),
		"instanceof" => Ok(Operator::InstanceOf),
		"&" => Ok(Operator::BitwiseAND),
		"|" => Ok(Operator::BitwiseOR),
		"^" => Ok(Operator::BitwiseXOR),
		"&&" => Ok(Operator::LogicalAND),
		"||" => Ok(Operator::LogicalOR),
		"??" => Ok(Operator::Nullish),
		"+=" => Ok(Operator::AdditionAssignment),
		"-=" => Ok(Operator::SubtractionAssignment),
		"**=" => Ok(Operator::ExponentiationAssignment),
		"*=" => Ok(Operator::MultiplicationAssignment),
		"/=" => Ok(Operator::DivisionAssignment),
		"%=" => Ok(Operator::RemainderAssignment),
		"<<=" => Ok(Operator::LeftShiftAssignment),
		">>=" => Ok(Operator::RightShiftAssignment),
		">>>=" => Ok(Operator::UnsignedRightShiftAssignment),
		"&=" => Ok(Operator::BitwiseANDAssignment),
		"|=" => Ok(Operator::BitwiseORAssignment),
		"^=" => Ok(Operator::BitwiseXORAssignment),
		"&&=" => Ok(Operator::LogicalANDAssignment),
		"||=" => Ok(Operator::LogicalORAssignment),
		"??=" => Ok(Operator::NullishAssignment),
		"=>" => Ok(Operator::Arrow),
		_ => Err(format!("{} is not an operator", s))
	    }
	} else {
	    match s {
		"new" => Ok(Operator::New),
		"++" => Ok(Operator::PrefixIncrement),
		"--" => Ok(Operator::PrefixDecrement),
		"!" => Ok(Operator::LogicalNot),
		"~" => Ok(Operator::BitwiseNot),
		"+" => Ok(Operator::UnaryPlus),
		"-" => Ok(Operator::UnaryMinus),
		"typeof" => Ok(Operator::TypeOf),
		"void" => Ok(Operator::Void),
		"delete" => Ok(Operator::Delete),
		"await" => Ok(Operator::Await),
		"yield" => Ok(Operator::Yield),
		"yield*" => Ok(Operator::YieldStar),
		"..." => Ok(Operator::Spread),
		_ => Err(format!("{} is not an operator", s))
	    }
	}
    }
}

impl ToString for Operator {
    fn to_string(&self) -> String {
	match self {
	    Operator::Period => String::from("."),
	    Operator::Equal => String::from("="),
	    Operator::DoubleEqual => String::from("=="),
	    Operator::TripleEqual => String::from("==="),
	    Operator::Inequal => String::from("!="),
	    Operator::DoubleInequal => String::from("!=="),
	    Operator::LT => String::from("<"),
	    Operator::LE => String::from("<="),
	    Operator::GT => String::from(">"),
	    Operator::GE => String::from(">="),
	    Operator::Exponentiation => String::from("**"),
	    Operator::Multiplication => String::from("*"),
	    Operator::Division => String::from("/"),
	    Operator::Remainder => String::from("%"),
	    Operator::Addition => String::from("+"),
	    Operator::Subtraction => String::from("-"),
	    Operator::LeftShift => String::from("<<"),
	    Operator::RightShift => String::from(">>"),
	    Operator::UnsignedRightShift => String::from(">>>"),
	    Operator::In => String::from("in"),
	    Operator::InstanceOf => String::from("instanceof"),
	    Operator::BitwiseAND => String::from("&"),
	    Operator::BitwiseOR => String::from("|"),
	    Operator::BitwiseXOR => String::from("^"),
	    Operator::LogicalAND => String::from("&&"),
	    Operator::LogicalOR => String::from("||"),
	    Operator::Nullish => String::from("??"),
	    Operator::AdditionAssignment => String::from("+="),
	    Operator::SubtractionAssignment => String::from("-="),
	    Operator::ExponentiationAssignment => String::from("**="),
	    Operator::MultiplicationAssignment => String::from("*="),
	    Operator::DivisionAssignment => String::from("/="),
	    Operator::RemainderAssignment => String::from("%="),
	    Operator::LeftShiftAssignment => String::from("<<="),
	    Operator::RightShiftAssignment => String::from(">>="),
	    Operator::UnsignedRightShiftAssignment => String::from(">>>="),
	    Operator::BitwiseANDAssignment => String::from("&="),
	    Operator::BitwiseORAssignment => String::from("|="),
	    Operator::BitwiseXORAssignment => String::from("^="),
	    Operator::LogicalANDAssignment => String::from("&&="),
	    Operator::LogicalORAssignment => String::from("||="),
	    Operator::NullishAssignment => String::from("??="),
	    Operator::Arrow => String::from("=>"),
	    Operator::New => String::from("new"),
	    Operator::PrefixIncrement => String::from("pre++"),
	    Operator::PrefixDecrement => String::from("pre--"),
	    Operator::LogicalNot => String::from("!"),
	    Operator::BitwiseNot => String::from("~"),
	    Operator::UnaryPlus => String::from("pre+"),
	    Operator::UnaryMinus => String::from("pre-"),
	    Operator::TypeOf => String::from("typeof"),
	    Operator::Void => String::from("void"),
	    Operator::Delete => String::from("delete"),
	    Operator::Await => String::from("await"),
	    Operator::Yield => String::from("yield"),
	    Operator::YieldStar => String::from("yield*"),
	    Operator::Spread => String::from("..."),
	}
    }
}

pub fn lex(st: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    _lex(st, 0, &mut tokens);
    return tokens;
}

fn _lex(st: &str, pos: usize, tokens: &mut Vec<Token>) {
    if pos >= st.len() {
	return;
    }
    let mut pos = pos;
    if &st[pos..pos+1] == "\"" {
	let start = pos+1;
	let mut escaped = false;
	loop {
	    pos += 1;
	    if st.get(pos..pos+1) == Some("\"") && !escaped {
		tokens.push(Token::String(String::from(&st[start..pos])));
		pos += 1;
		break;
	    } else if st.get(pos..pos+1) == Some("\\") {
		escaped = !escaped;
	    } else if escaped {
		escaped = false;
	    }
	}
    } else if st.get(pos..pos+1) == Some("(") {
	tokens.push(Token::LParen);
	pos += 1;
    } else if st.get(pos..pos+1) == Some(")") {
	tokens.push(Token::RParen);
	pos += 1;
    } else if st.get(pos..pos+1) == Some("{") {
	tokens.push(Token::LBracket);
	pos += 1;
    } else if st.get(pos..pos+1) == Some("}") {
	tokens.push(Token::RBracket);
	pos += 1;
    } else if st.get(pos..pos+1) == Some(";") {
	tokens.push(Token::Semicolon);
	pos += 1;
    } else if st.get(pos..pos+1) == Some(",") {
	tokens.push(Token::Comma);
	pos += 1;
    } else if st.get(pos..pos+1) == Some(":") {
	tokens.push(Token::Colon);
	pos += 1;
    } else if st[pos..].chars().next().unwrap().is_whitespace() {
	pos += 1;
    } else if st[pos..].chars().next().unwrap().is_numeric() {
	let start = pos;
	loop {
	    pos += 1;
	    if !(st[pos..].chars().next().unwrap().is_numeric() || &st[pos..pos+1] == ".") {
		tokens.push(Token::Number(f64::from_str(&st[start..pos]).unwrap()));
		break;
	    }
	}
    } else if st[pos..].chars().next().unwrap().is_alphanumeric() || &st[pos..pos+1] == "$" {
	let start = pos;
	loop {
	    pos += 1;
	    if !(st[pos..].chars().next().unwrap().is_alphanumeric() || &st[pos..pos+1] == "$") {
		tokens.push(Token::Ident(String::from(&st[start..pos])));
		break;
	    }
	}
    } else {
	let start = pos;
	loop {
	    pos += 1;
	    if (|c: char| c.is_alphanumeric() || c.is_whitespace())(st[pos..].chars().next().unwrap()) || &st[pos..pos+1] == "$" || &st[pos..pos+1] == "\"" {
		tokens.push(Token::Ident(String::from(&st[start..pos])));
		break;
	    }
	}
    }
    _lex(st, pos, tokens);
}

pub fn parse(st: &str) -> Vec<AST> {
    let tokens = lex(st);
    let mut tokens = tokens.iter().peekable();
    let mut lines = Vec::new();
    while tokens.peek().is_some() {
	lines.push(operator_parse(&mut tokens));
	if tokens.peek() == Some(&&Token::Semicolon) {
	    let _ = tokens.next();
	}
    }
    return lines;
}

fn operator_parse(tokens: &mut std::iter::Peekable<std::slice::Iter<'_,Token>>) -> AST {
    let mut output = Vec::new();
    let mut operators: Vec<(Operator, bool)> = Vec::new();
    let mut last_was_ident = false;
    let mut last_was_operator = false;
    loop {
	let next = tokens.peek();
	if next == None || next == Some(&&Token::LParen) || next == Some(&&Token::RBracket) || next == Some(&&Token::Comma) || next == Some(&&Token::Semicolon) || (last_was_ident && next.unwrap().to_operator(!last_was_operator) == None) {
	    break;
	}
	let next = next.unwrap();

	if let Some(o1) = next.to_operator(!last_was_operator) {
	    let _ = tokens.next().unwrap();
	    let p1 = o1.precedence();
	    operators.retain(|(o2, binary)| {
		let p2 = o2.precedence();
		if p2 > p1 || (p2 == p1 && o1.left_associative()) {
		    let l = output.len();
		    if *binary {
			let lhs = Box::new(output.remove(l-2));
			let rhs = Box::new(output.remove(l-2));
			output.push(AST::FunctionCall(Box::new(AST::Symbol(o2.to_string())), vec![lhs, rhs]));
		    } else {
			let el = Box::new(output.remove(l-1));
			output.push(AST::FunctionCall(Box::new(AST::Symbol(o2.to_string())), vec![el]));
		    }
		    
		    false
		} else {
		    true
		}
	    });
	    operators.insert(0, (o1.clone(), !last_was_operator));
	    last_was_ident = false;
	    last_was_operator = true;
	} else if next == &&Token::LParen {
	    output.push(operator_parse(tokens));
	    assert_eq!(Some(&Token::RParen), tokens.next());
	    last_was_ident = true;
	    last_was_operator = false;
	} else {
	    let val = _parse(tokens);
	    if tokens.peek() == Some(&&Token::LParen) {
		let _ = tokens.next();
		let mut args = Vec::new();
		while tokens.peek() != Some(&&Token::RParen) {
		    args.push(Box::new(operator_parse(tokens)));
		}
		assert_eq!(Some(&Token::RParen), tokens.next());
		output.push(AST::FunctionCall(Box::new(val), args));
	    } else {
		output.push(val);
	    }
	    last_was_ident = true;
	    last_was_operator = false;
	}
    }
    for (op, binary) in operators {
	if binary {
	    let l = output.len();
	    let lhs = Box::new(output.remove(l-2));
	    let rhs = Box::new(output.remove(l-2));
	    output.push(AST::FunctionCall(Box::new(AST::Symbol(op.to_string())), vec![lhs, rhs]));
	} else {
	    let l = output.len();
	    let el = Box::new(output.remove(l-1));
	    output.push(AST::FunctionCall(Box::new(AST::Symbol(op.to_string())), vec![el]));
	}
    }
    assert_eq!(1, output.len());
    return output[0].clone();
}

fn _parse(tokens: &mut std::iter::Peekable<std::slice::Iter<'_,Token>>) -> AST {
    match tokens.next() {
	Some(Token::Ident(name)) => {
	    if name == &String::from("if") {
		assert_eq!(Some(&Token::LParen), tokens.next());
		let cond = Box::new(operator_parse(tokens));
		assert_eq!(Some(&Token::RParen), tokens.next());
		
		let block = Box::new(operator_parse(tokens));
		
		let else_part;
		if tokens.peek() == Some(&&Token::Ident("else".to_string())) {
		    let _ = tokens.next();
		    else_part = Some(Box::new(operator_parse(tokens)));
		} else {
		    else_part = None;
		}
		
		AST::IfStatement(cond, block, else_part)
	    } else if name == &String::from("var") {
		let body = operator_parse(tokens);
		if let AST::Symbol(ref name) = body {
		    AST::VariableDefinition(name.to_string(), None)
		} else if let AST::FunctionCall(fun_name, args) = body {
		    assert_eq!(AST::Symbol("=".to_string()), *fun_name);
		    assert_eq!(2, args.len());
		    let name = args[0].clone();
		    if let AST::Symbol(var_name) = *name {
			let value = args[1].clone();
			AST::VariableDefinition(var_name.to_string(), Some(value))
		    } else {
			panic!("malformed variable definition");
		    }
		} else {
		    panic!("malformed variable definition");
		}
	    } else if name == &String::from("function") {
		let name;
		let next = tokens.next();
		if let Some(Token::Ident(n)) = next {
		    name = Some(n.to_string());
		    assert_eq!(Some(&Token::LParen), tokens.next());
		} else {
		    name = None;
		    assert_eq!(Some(&Token::LParen), next);
		}
		let mut args = Vec::new();
		while tokens.peek() != Some(&&Token::RParen) {
		    if let Some(Token::Ident(name)) = tokens.next().cloned() {
			args.push(name);
			if tokens.peek() == Some(&&Token::Comma) {
			    let _ = tokens.next();
			}
		    } else {
			panic!("Invalid function definition");
		    }
		}
		let _ = tokens.next();
		let body = Box::new(operator_parse(tokens));
		AST::FunctionDefinition(name, args, body)
	    } else if name == &String::from("class") {
		let name;
		if let Some(&&Token::Ident(ref n)) = tokens.peek() {
		    let _ = tokens.next();
		    name = Some(n.to_string());
		} else {
		    name = None;
		}

		assert_eq!(Some(&Token::LBracket), tokens.next());
		let mut items = Vec::new();
		while tokens.peek() != Some(&&Token::RBracket) {
		    if let Some(Token::Ident(n)) = tokens.next() {
			let mut args = Vec::new();
			assert_eq!(Some(&Token::LParen), tokens.next());
			while tokens.peek() != Some(&&Token::RParen) {
			    if let Some(Token::Ident(name)) = tokens.next().cloned() {
				args.push(name);
				if tokens.peek() == Some(&&Token::Comma) {
				    let _ = tokens.next();
				}
			    } else {
				panic!("Invalid function definition");
			    }
			}
			let _ = tokens.next();
			let body = Box::new(operator_parse(tokens));
			items.push(Box::new(AST::FunctionDefinition(Some(n.to_string()), args, body)));
		    } else {
			panic!("illegal class definition");
		    }
		}
		let _ = tokens.next();
		AST::ClassDefinition(name, items)
	    } else if name == &String::from("return") {
		if tokens.peek() == Some(&&Token::Semicolon) {
		    let _ = tokens.next();
		    AST::ReturnStatement(None)
		} else {
		    let val = operator_parse(tokens);
		    AST::ReturnStatement(Some(Box::new(val)))
		}
	    } else if name == &String::from("throw") {
		let val = operator_parse(tokens);
		AST::ThrowStatement(Box::new(val))
	    } else if name == &String::from("break") {
		AST::BreakStatement
	    } else if name == &String::from("continue") {
		AST::ContinueStatement
	    } else if name == &String::from("try") {
		let try_body = Box::new(operator_parse(tokens));
		assert_eq!(Some(&Token::Ident(String::from("catch"))), tokens.next());
		let exception_name;
		if Some(&&Token::LParen) == tokens.peek() {
		    let _ = tokens.next();
		    if let Some(Token::Ident(en)) = tokens.next() {
			exception_name = Some(en.to_string());
			assert_eq!(Some(&Token::RParen), tokens.next());
		    } else {
			panic!("invalid exceptionVar name");
		    }
		} else {
		    exception_name = None;
		}
		let catch_body = Box::new(operator_parse(tokens));
		let finally_body;
		if Some(&&Token::Ident("finally".to_string())) == tokens.peek() {
		    let _ = tokens.next();
		    finally_body = Some(Box::new(operator_parse(tokens)));
		} else {
		    finally_body = None;
		}
		AST::TryStatement(try_body, exception_name, catch_body, finally_body)
	    } else if name == &String::from("while") {
		assert_eq!(Some(&Token::LParen), tokens.next());
		let cond = Box::new(operator_parse(tokens));
		assert_eq!(Some(&Token::RParen), tokens.next());
		let body = Box::new(operator_parse(tokens));
		AST::WhileStatement(cond, body)
	    } else if name == &String::from("for") {
		assert_eq!(Some(&Token::LParen), tokens.next());
		let init = Box::new(operator_parse(tokens));
		assert_eq!(Some(&Token::Semicolon), tokens.next());
		let cond = Box::new(operator_parse(tokens));
		assert_eq!(Some(&Token::Semicolon), tokens.next());
		let incr = Box::new(operator_parse(tokens));
		assert_eq!(Some(&Token::RParen), tokens.next());
		let body = Box::new(operator_parse(tokens));
		AST::ForStatement(init, cond, incr, body)
	    } else {
		AST::Symbol(name.to_string())
	    }
	},
	Some(Token::Number(n)) => {
	    AST::Value(JSValue::Number(Number::Num(*n)))
	},
	Some(Token::String(s)) => {
	    AST::Value(JSValue::String(s.to_string()))
	},
	Some(Token::LBracket) => {
	    // check if this is block or object
	    // if tokens.peek() != Some(&&Token::RBracket) {
		// let first = operator_parse(tokens);
		// if let AST::Value(first_key) = first {
		//     let first_key = if let JSValue::String(s) = first_key { s } else { panic!("malformed object") };
		//     assert_eq!(Some(&Token::Colon), tokens.next());
		//     let first_value = operator_parse(tokens);
		//     if tokens.peek() == Some(&&Token::Comma) {
		// 	let _ = tokens.next();
		//     }

		//     let obj = Object::new();
		//     obj.insert(first_key, first_value);

		//     while tokens.peek() != Some(&&Token::RBracket) {
		// 	if let AST::Value(key) = operator_parse(tokens) {
		// 	    let key = if let JSValue::String(s) = key { s } else { panic!("malformed object") };
		// 	    assert_eq!(Some(&Token::Colon), tokens.next());
		// 	    let value = operator_parse(tokens);
		// 	    obj.insert(key, value);
		// 	    if tokens.peek() == Some(&&Token::Comma) {
		// 		let _ = tokens.next();
		// 	    }
		// 	} else {
		// 	    panic!("Incorrectly formatted object");
		// 	}
		//     }
		//     AST::Value(JSValue::Object(obj))
		// } else {
		    // if tokens.peek() == Some(&&Token::Semicolon) {
			// let _ = tokens.next();
		    // }
	    // let mut lines = vec![Box::new(first)];
	    let mut lines = Vec::new();
	    while tokens.peek() != Some(&&Token::RBracket) {
		lines.push(Box::new(operator_parse(tokens)));
		if tokens.peek() == Some(&&Token::Semicolon) {
		    let _ = tokens.next();
		}
	    }
	    let _ = tokens.next();
	    AST::Block(lines)
	    // }
	    // } else {
		// AST::Block(Vec::new())
	    // }
	}
	t => panic!("Unexpected token {:?}", t)
    }
}
