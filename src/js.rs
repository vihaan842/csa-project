use std::collections::{HashMap, VecDeque};
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug, Clone, PartialEq)]
pub enum AST {
    Value(JSValue),
    Symbol(String),
    Block(Vec<Box<AST>>),
    FunctionCall(Box<AST>, Vec<Box<AST>>),
    VariableDefinition(String, Option<Box<AST>>),
    FunctionDefinition(Option<String>, Vec<String>, Box<AST>),
    ReturnStatement(Option<Box<AST>>),
    ThrowStatement(Box<AST>),
    BreakStatement,
    ContinueStatement,
    IfStatement(Box<AST>, Box<AST>, Option<Box<AST>>),
    TryStatement(Box<AST>, Option<String>, Box<AST>, Option<Box<AST>>),
    WhileStatement(Box<AST>, Box<AST>),
    ForStatement(Box<AST>, Box<AST>, Box<AST>, Box<AST>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum JSValue {
    Undefined,
    Null,
    Boolean(bool),
    Number(Number),
    String(String),
    Object(Rc<Object>),
    Function(JSFunction),
    CompletionRecord(String, Box<JSValue>, String)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    NaN,
    PInf,
    NInf,
    Num(f64)
}

#[derive(Clone)]
pub enum JSFunction {
    BuiltinFunction(Rc<dyn Fn(&mut VecDeque<HashMap<String, JSValue>>, Vec<JSValue>) -> JSValue>),
    BuiltinMacro(Rc<dyn Fn(&mut VecDeque<HashMap<String, JSValue>>, Vec<Box<AST>>) -> JSValue>),
    UserDefined(Vec<String>, Box<AST>)
}

impl std::cmp::PartialEq for JSFunction {
    fn eq(&self, other: &Self) -> bool {
	match self {
            JSFunction::BuiltinFunction(f1) => {
		if let JSFunction::BuiltinFunction(f2) = other {
		    Rc::ptr_eq(f1, f2)
		} else {
		    false
		}
	    },

	    JSFunction::BuiltinMacro(f1) => {
		if let JSFunction::BuiltinMacro(f2) = other {
		    Rc::ptr_eq(f1, f2)
		} else {
		    false
		}
	    },

	    JSFunction::UserDefined(a1, b1) => {
		if let JSFunction::UserDefined(a2, b2) = other {
		    a1 == a2 && b1 == b2
		} else {
		    false
		}
	    },
	}
    }
}

impl std::fmt::Debug for JSFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	match self {
	    JSFunction::BuiltinFunction(_) => {
		write!(f, "BuiltinFunction")
	    },
	    JSFunction::BuiltinMacro(_) => {
		write!(f, "BuiltinMacro")
	    },
	    JSFunction::UserDefined(a1, b1) => {
		write!(f, "UserDefined({:?}, {:?})", a1, b1)
	    }
	}
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct Object {
    properties: Rc<RefCell<HashMap<String, JSValue>>>
}

impl Object {
    pub fn new() -> Object {
	Object { properties: Rc::new(RefCell::new(HashMap::new())) }
    }

    pub fn get(&self, name: String) -> JSValue {
	if let Some(value) = self.properties.borrow().get(&name) {
	    return value.clone();
	} else {
	    return JSValue::Undefined;
	}
    }
    
    pub fn insert(&self, name: String, value: JSValue) {
	let _ = self.properties.borrow_mut().insert(name, value);
    }
}

pub fn interpret(code: Vec<AST>) {
    let mut global_scope = HashMap::new();

    global_scope.insert(".".to_string(), JSValue::Function(JSFunction::BuiltinMacro(Rc::new(
	|vars, args| {
	    assert_eq!(2, args.len());
	    let obj = _interpret(args[0].clone(), vars);
	    if let JSValue::Object(obj) = obj {
		let target = *args[1].clone();
		if let AST::Symbol(s) = target {
		    obj.get(s)
		} else if let AST::FunctionCall(func, args) = target {
		    if let AST::Symbol(name) = *func.clone() {
			if let JSValue::Function(f) = obj.get(name.clone()) {
			    let mut inner_scope = HashMap::new();
			    // todo: may cause big errors! mutability is not reflected
			    inner_scope.insert("this".to_string(), JSValue::Object(obj.clone()));			
			    let ret;
			    match f {
				JSFunction::BuiltinFunction(f) => {
				    let mut values = Vec::new();
				    for arg in args {
					values.push(_interpret(arg, vars));
				    }
				    vars.push_front(inner_scope);
				    ret = f(vars, values);
				    let _ = vars.pop_front();
				},
				JSFunction::BuiltinMacro(f) => {
				    vars.push_front(inner_scope);
				    ret = f(vars, args);
				    let _ = vars.pop_front();
				},
				JSFunction::UserDefined(names, body) => {
				    for (name, arg) in names.iter().zip(args) {
					let value = _interpret(arg, vars);
					inner_scope.insert(name.to_string(), value);
				    }
				    vars.push_front(inner_scope);
				    ret = _interpret(body, vars);
				    let _ = vars.pop_front();
				}
			    }
			    return ret;
			} else {
			    panic!("invalid method {}", name);
			}
		    } else {
			panic!("Invalid operand {:?} for .", func);
		    }
		} else {
		    panic!("Invalid operand for .");
		}
	    } else {
		panic!("Called method on primitive type {:?}", obj);
	    }
		
	}
    ))));

    global_scope.insert("=".to_string(), JSValue::Function(JSFunction::BuiltinMacro(Rc::new(
	|vars, args| {
	    assert_eq!(2, args.len());
	    let value = _interpret(args[1].clone(), vars);
	    if let AST::Symbol(name) = *args[0].clone() {
		for scope in &mut *vars {
		    if scope.contains_key(&name) {
			scope.insert(name, value);
			return JSValue::Null;
		    }
		}
		panic!("Assignment to undeclared variable");
	    } else if let AST::FunctionCall(func, args) = *args[0].clone() {
		if AST::Symbol(".".to_string()) == *func {
		    let obj = _interpret(args[0].clone(), vars);
		    if let JSValue::Object(obj) = obj {
			if let AST::Symbol(name) = *args[1].clone() {
			    obj.insert(name, value);
			    return JSValue::Null;
			} else {
			    panic!("Assignment to unassignable object");
			}
		    } else {
			panic!("Assignment to unassignable object");
		    }
		} else {
		    panic!("Assignment to unassignable object");
		}
	    } else {
		panic!("Assignment to unassignable object");
	    }
	}
    ))));

    global_scope.insert("==".to_string(), JSValue::Function(JSFunction::BuiltinFunction(Rc::new(
	|_vars, args| {
	    assert_eq!(2, args.len());
	    JSValue::Boolean(args[0].clone() == args[1].clone())
	}
    ))));

    global_scope.insert("!=".to_string(), JSValue::Function(JSFunction::BuiltinFunction(Rc::new(
	|_vars, args| {
	    assert_eq!(2, args.len());
	    JSValue::Boolean(args[0].clone() != args[1].clone())
	}
    ))));

    // global_scope.insert(">".to_string(), JSValue::Function(JSFunction::BuiltinFunction(Rc::new(
    // 	|_vars, args| {
    // 	    assert_eq!(2, args.len());
    // 	    JSValue::Boolean(args[0].clone() > args[1].clone())
    // 	}
    // ))));

    // global_scope.insert(">=".to_string(), JSValue::Function(JSFunction::BuiltinFunction(Rc::new(
    // 	|_vars, args| {
    // 	    assert_eq!(2, args.len());
    // 	    JSValue::Boolean(args[0].clone() >= args[1].clone())
    // 	}
    // ))));

    // global_scope.insert("<".to_string(), JSValue::Function(JSFunction::BuiltinFunction(Rc::new(
    // 	|_vars, args| {
    // 	    assert_eq!(2, args.len());
    // 	    JSValue::Boolean(args[0].clone() < args[1].clone())
    // 	}
    // ))));

    // global_scope.insert("<".to_string(), JSValue::Function(JSFunction::BuiltinFunction(Rc::new(
    // 	|_vars, args| {
    // 	    assert_eq!(2, args.len());
    // 	    JSValue::Boolean(args[0].clone() <= args[1].clone())
    // 	}
    // ))));

    global_scope.insert("+".to_string(), JSValue::Function(JSFunction::BuiltinFunction(Rc::new(
	|_vars, args| {
	    assert_eq!(2, args.len());
	    let lhs = args[0].clone();
	    let rhs = args[1].clone();

	    match lhs {
		JSValue::Number(Number::Num(n1)) => {
		    match rhs {
			JSValue::Number(Number::Num(n2)) => {
			    JSValue::Number(Number::Num(n1+n2))
			},
			_ => todo!("Other additions")
		    }
		},
		_ => todo!("Other additions")
	    }
	}
    ))));

    global_scope.insert("-".to_string(), JSValue::Function(JSFunction::BuiltinFunction(Rc::new(
	|_vars, args| {
	    assert_eq!(2, args.len());
	    let lhs = args[0].clone();
	    let rhs = args[1].clone();

	    match lhs {
		JSValue::Number(Number::Num(n1)) => {
		    match rhs {
			JSValue::Number(Number::Num(n2)) => {
			    JSValue::Number(Number::Num(n1-n2))
			},
			_ => todo!("Other subtractions")
		    }
		},
		_ => todo!("Other subtractions")
	    }
	}
    ))));

    global_scope.insert("*".to_string(), JSValue::Function(JSFunction::BuiltinFunction(Rc::new(
	|_vars, args| {
	    assert_eq!(2, args.len());
	    let lhs = args[0].clone();
	    let rhs = args[1].clone();

	    match lhs {
		JSValue::Number(Number::Num(n1)) => {
		    match rhs {
			JSValue::Number(Number::Num(n2)) => {
			    JSValue::Number(Number::Num(n1*n2))
			},
			_ => todo!("Other multiplications")
		    }
		},
		_ => todo!("Other multiplications")
	    }
	}
    ))));

    global_scope.insert("/".to_string(), JSValue::Function(JSFunction::BuiltinFunction(Rc::new(
	|_vars, args| {
	    assert_eq!(2, args.len());
	    let lhs = args[0].clone();
	    let rhs = args[1].clone();

	    match lhs {
		JSValue::Number(Number::Num(n1)) => {
		    match rhs {
			JSValue::Number(Number::Num(n2)) => {
			    JSValue::Number(Number::Num(n1/n2))
			},
			_ => todo!("Other divisions")
		    }
		},
		_ => todo!("Other divisions")
	    }
	}
    ))));

    let console = Object::new();
    console.insert("log".to_string(), JSValue::Function(JSFunction::BuiltinFunction(Rc::new(
	|_vars, args| {
	    assert_eq!(1, args.len());
	    println!("{:?}", args[0].clone());
	    JSValue::Null
	}
    ))));

    global_scope.insert("console".to_string(), JSValue::Object(Rc::new(console)));
    
    let mut vars = VecDeque::new();
    vars.push_front(global_scope);
    for part in code {
	let _ = _interpret(Box::new(part), &mut vars);
    }
}

fn _interpret(code: Box<AST>, vars: &mut VecDeque<HashMap<String, JSValue>>) -> JSValue {
    match &*code {
	AST::Value(v) => v.clone(),
	AST::Symbol(s) => {
	    for h in vars {
		if let Some(v) = h.get(s) {
		    return v.clone();
		}
	    }
	    JSValue::Null
	},
	AST::Block(asts) => {
	    vars.push_front(HashMap::new());
	    for ast in asts {
		if let JSValue::CompletionRecord(name, value, target) = _interpret(ast.clone(), vars) {
		    let _ = vars.pop_front();
		    return JSValue::CompletionRecord(name, value, target);
		}
	    }
	    let _ = vars.pop_front();
	    JSValue::Null
	},
	AST::FunctionCall(func, args)=>  {
	    let func_val = _interpret(func.clone(), vars);
	    if let JSValue::Function(f) = func_val {
		let mut inner_scope = HashMap::new();
		let ret;
		match f {
		    JSFunction::BuiltinFunction(f) => {
			let mut values = Vec::new();
			for arg in args {
			    values.push(_interpret(arg.clone(), vars));
			}
			vars.push_front(inner_scope);
			ret = f(vars, values);
			let _ = vars.pop_front();
		    },
		    JSFunction::BuiltinMacro(f) => {
			vars.push_front(inner_scope);
			ret = f(vars, args.clone());
			let _ = vars.pop_front();
		    },
		    JSFunction::UserDefined(names, body) => {
			for (name, arg) in names.iter().zip(args) {
			    let value = _interpret(arg.clone(), vars);
			    inner_scope.insert(name.to_string(), value);
			}
			vars.push_front(inner_scope);
			let ret1 = _interpret(body, vars);
			if let JSValue::CompletionRecord(name, value, _) = ret1 {
			    if name == "return".to_string() {
				ret = *value.clone();
			    } else {
				panic!("illegal return value");
			    }
			} else {
			    ret = ret1;
			}
			let _ = vars.pop_front();
		    }
		}
		return ret;
	    } else {
		panic!("Invalid function {:?}", func_val);
	    }
	},
	AST::VariableDefinition(name, value) => {
	    if let Some(value) = value {
		let evaled = _interpret(value.clone(), vars);
		vars[0].insert(name.to_string(), evaled);
	    } else {
		vars[0].insert(name.to_string(), JSValue::Undefined);
	    }
	    JSValue::Null
	},
	AST::FunctionDefinition(name, args, body) => {
	    let val = JSValue::Function(JSFunction::UserDefined(args.clone(), body.clone()));
	    let end = vars.len() - 1;
	    if let Some(name) = name {
		vars[end].insert(name.clone(), val.clone());
	    }
	    val
	},
	AST::ReturnStatement(value) => {
	    if let Some(value) = value {
		JSValue::CompletionRecord("return".to_string(), Box::new(_interpret(value.clone(), vars)), "".to_string())
	    } else {
		JSValue::CompletionRecord("return".to_string(), Box::new(JSValue::Null), "".to_string())
	    }
	},
	AST::ThrowStatement(value) => {
	    JSValue::CompletionRecord("throw".to_string(), Box::new(_interpret(value.clone(), vars)), "".to_string())
	},
	AST::BreakStatement => {
	    JSValue::CompletionRecord("break".to_string(), Box::new(JSValue::Null), "".to_string())
	},
	AST::ContinueStatement => {
	    JSValue::CompletionRecord("continue".to_string(), Box::new(JSValue::Null), "".to_string())
	},
	AST::IfStatement(cond, body, else_statement) => {
	    if _interpret(cond.clone(), vars) == JSValue::Boolean(true) {
		_interpret(body.clone(), vars)
	    } else if let Some(second) = else_statement {
		_interpret(second.clone(), vars)
	    } else {
		JSValue::Null
	    }
	},
	AST::TryStatement(try_body, exception_var, catch_body, finally_body) => {
	    let res = _interpret(try_body.clone(), vars);
	    if let JSValue::CompletionRecord(typ, val, targ) = res {
		if typ == "throw" {
		    let catch_res;
		    if let Some(ev) = exception_var {
			vars.push_front(HashMap::new());
			vars[0].insert(ev.clone(), *val.clone());
			catch_res = _interpret(catch_body.clone(), vars);
			let _ = vars.pop_front();
		    } else {
			catch_res = _interpret(catch_body.clone(), vars);
		    }
		    if let JSValue::CompletionRecord(_, _, _) = catch_res {
			return catch_res
		    }
		} else {
		    return JSValue::CompletionRecord(typ, val, targ);
		}
	    }
	    if let Some(finally_body) = finally_body {
		return _interpret(finally_body.clone(), vars);
	    } else {
		return JSValue::Null;
	    }
	},
	AST::WhileStatement(cond, body) => {
	    while _interpret(cond.clone(), vars) == JSValue::Boolean(true) {
		let value = _interpret(body.clone(), vars);
		if let JSValue::CompletionRecord(typ, val, targ) = value {
		    if typ == "break" {
			return JSValue::Null;
		    } else if typ == "return" {
			return JSValue::CompletionRecord(typ, val, targ);
		    }
		}
	    }
	    JSValue::Null
	},
	AST::ForStatement(init, cond, incr, body) => {
	    vars.push_front(HashMap::new());
	    let _ = _interpret(init.clone(), vars);
	    while _interpret(cond.clone(), vars) == JSValue::Boolean(true) {
		let value = _interpret(body.clone(), vars);
		if let JSValue::CompletionRecord(typ, val, targ) = value {
		    if typ == "break" {
			return JSValue::Null;
		    } else if typ == "return" {
			return JSValue::CompletionRecord(typ, val, targ);
		    }
		}
		let _ = _interpret(incr.clone(), vars);
	    }
	    let _ = vars.pop_front();
	    JSValue::Null
	},
    }
}
