use crate::rules;

use std::collections::{HashMap, VecDeque};
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::{Display, Formatter, Error};

#[derive(Debug, Clone, PartialEq)]
pub enum AST {
    Value(JSValue),
    Symbol(String),
    Block(Vec<Box<AST>>),
    FunctionCall(Box<AST>, Vec<Box<AST>>),
    VariableDefinition(String, Option<Box<AST>>),
    FunctionDefinition(Option<String>, Vec<String>, Box<AST>),
    ClassDefinition(Option<String>, Vec<Box<AST>>),
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
    Object(Object),
    // note: this should be an object, but I am lazy
    Array(Vec<Box<JSValue>>),
    Function(JSFunction),
    CompletionRecord(String, Box<JSValue>, String)
}

impl Display for JSValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
	match &self {
	    JSValue::Undefined => write!(f, "undefined"),
	    JSValue::Null => write!(f, "null"),
	    JSValue::Boolean(b) => write!(f, "{}",  b),
	    JSValue::Number(n) => write!(f, "{}",  n),
	    JSValue::String(s) => write!(f, "{}",  s),
	    _ => write!(f, "{:?}", self)
	}
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    NaN,
    PInf,
    NInf,
    Num(f64)
}

impl Display for Number {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
	match &self {
	    Number::NaN => write!(f, "NaN"),
	    Number::PInf => write!(f, "Infinity"),
	    Number::NInf => write!(f, "-Infinity"),
	    Number::Num(n) => write!(f, "{}", n),
	}
    }
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
pub enum Object {
    JS(Rc<JSObject>),
    HTML(Rc<Node>),
    CSS(Rc<RefCell<HashMap<String, String>>>)
}

impl Object {
    pub fn get(&self, name: String) -> JSValue {
	match self {
	    Object::JS(js) => js.get(name),
	    Object::HTML(html) => html.get(name),
	    Object::CSS(css) => css.borrow().get(&name).map_or(JSValue::Undefined, |v| JSValue::String(v.to_string()))
	}
    }

    pub fn insert(&self, name: String, value: JSValue) {
	match self {
	    Object::JS(js) => js.insert(name, value),
	    Object::HTML(html) => html.insert(name, value),
	    Object::CSS(css) => {
		let _ = css.borrow_mut().insert(name, format!("{}", value));
	    }
	}
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct JSObject {
    properties: Rc<RefCell<HashMap<String, JSValue>>>
}

impl JSObject {
    pub fn new() -> JSObject {
	JSObject { properties: Rc::new(RefCell::new(HashMap::new())) }
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

#[derive(Debug, Clone, PartialEq)]
pub enum NodeType {
    // string inside of text
    Text(String),
    // container type, children, and params
    Container(String, RefCell<Vec<Rc<Node>>>, HashMap<String, String>),
    // document
    Document(RefCell<Vec<Rc<Node>>>)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    // what type of node this is
    pub node_type: NodeType,
    // parent node
    pub parent: RefCell<Option<Rc<Node>>>,
    // css properties
    pub css: Rc<RefCell<HashMap<String, String>>>,
    // other properties
    properties: Rc<RefCell<HashMap<String, JSValue>>>
}

impl Node {
    // empty document
    pub fn get_document() -> Node {
	return Node{node_type: NodeType::Document(RefCell::new(Vec::new())), parent: RefCell::new(None), css: Rc::new(RefCell::new(HashMap::new())), properties: Rc::new(RefCell::new(HashMap::new()))}
    }
    // get new container node from tag
    pub fn from_tag(tag_content: String) -> Node {
	let mut parts = Vec::new();
	let mut word = String::from("");
	let mut in_str = false;
	let mut str_char = '\"';
	let mut str_ignore_next = false;
	for c in tag_content.chars() {
	    if in_str {
		if c == str_char && !str_ignore_next {
		    in_str = true;
		} else if c == '\\' && !str_ignore_next {
		    str_ignore_next = true;
		} else {
		    word.push(c);
		    str_ignore_next = false;
		}
	    } else {
		if c == '\'' || c == '\"' {
		    in_str = true;
		    str_char = c.clone();
		} else if c == ' ' {
		    parts.push(word.clone());
		    word = "".to_string();
		} else {
		    word.push(c);
		}
	    }
	}
	parts.push(word.clone());
	let tag_name = parts.remove(0);
	let mut params = HashMap::new();
	for param in parts {
	    let param_parts = param.splitn(2, "=").collect::<Vec<&str>>();
	    // make sure that this is actually a parameter
	    if param_parts.len() == 2 {
		params.insert(param_parts[0].to_string(), param_parts[1].to_string());
	    }
	}
	Node{node_type: NodeType::Container(tag_name.to_string(), RefCell::new(Vec::new()), params), parent: RefCell::new(None), css: Rc::new(RefCell::new(HashMap::new())), properties: Rc::new(RefCell::new(HashMap::new()))}
    }
    // gets children, if there are any
    pub fn children(&self) -> &RefCell<Vec<Rc<Node>>> {
	match &self.node_type {
	    NodeType::Document(children) => children,
	    NodeType::Container(_, children, _) => children,
	    NodeType::Text(_) => panic!("Node has no children!"),
	}
    }
    // get new text node from text
    pub fn from_text(text: String) -> Node {
	return Node{node_type: NodeType::Text(text), parent: RefCell::new(None), css: Rc::new(RefCell::new(HashMap::new())), properties: Rc::new(RefCell::new(HashMap::new()))}
    }
    // checks if container node has end tag
    pub fn is_empty_element(&self) -> bool {
	match &self.node_type {
	    NodeType::Container(tag_name, _, _) => rules::EMPTY_ELEMENTS.iter().any(|e| e==&tag_name),
	    _ => false
	}
    }
    // gets css from <style> tags
    pub fn find_css(&self) -> String {
	let mut css = String::from("");
	match &self.node_type {
	    NodeType::Document(children) => {
		for child in children.borrow().iter() {
		    css += &child.find_css();
		}
	    },
	    NodeType::Container(tag_name, children, _) => {
		if tag_name == &String::from("style") {
		    for child in children.borrow().iter() {
			match &child.node_type {
			    NodeType::Text(text) => css += text,
			    _ => {}
			}
		    }
		} else {
		    for child in children.borrow().iter() {
			css += &child.find_css();
		    }
		}
	    }
	    NodeType::Text(_) => {},
	}
	return css;
    }
    // gets js from <script> tags
    pub fn find_js(&self) -> String {
	let mut js = String::from("");
	match &self.node_type {
	    NodeType::Document(children) => {
		for child in children.borrow().iter() {
		    js += &child.find_js();
		}
	    },
	    NodeType::Container(tag_name, children, _) => {
		if tag_name == &String::from("script") {
		    for child in children.borrow().iter() {
			match &child.node_type {
			    NodeType::Text(text) => js += text,
			    _ => {}
			}
		    }
		} else {
		    for child in children.borrow().iter() {
			js += &child.find_js();
		    }
		}
	    }
	    NodeType::Text(_) => {},
	}
	return js;
    }
    // figures out if a basic selector (tag name, class name, id, etc.) applies
    pub fn basic_selector_applies(&self, selector: String) -> bool {
	match &self.node_type {
	    NodeType::Container(tag_name, _, params) => {
		if selector == "*" {
		    return true;
		} if selector.starts_with(".") {
		    let class_selector = selector.split_at(1).1.to_string();
		    return params.get("class") == Some(&class_selector);
		} else if selector.starts_with("#") {
		    let id_selector = selector.split_at(1).1.to_string();
		    return params.get("id") == Some(&id_selector);
		} else {
		    return &selector == tag_name;
		}
	    },
	    _ => false
	}
    }

    pub fn propogate_css(&self) {
	self._propogate_css(Vec::new());
    }
    
    fn _propogate_css(&self, pairs: Vec<(String, String)>) {
	for (key, value) in pairs {
	    if !self.css.borrow().contains_key(&key) {
		self.css.borrow_mut().insert(key, value);
	    }
	}
	let mut inherited = Vec::new();
	for (key, value) in &*self.css.borrow() {
	    if crate::rules::INHERITED_PROPERTIES.iter().any(|e| e==&key) {
		inherited.push((key.to_string(), value.to_string()));
	    }
	}
	match &self.node_type {
	    NodeType::Container(_, children, _) | NodeType::Document(children) => {
		for child in &*children.borrow() {
		    child._propogate_css(inherited.clone());
		}
	    },
	    _ => {}
	}
    }
    
    pub fn get(&self, name: String) -> JSValue {
	if name == "nodeType" {
	    match self.node_type {
		NodeType::Text(_) => JSValue::Number(Number::Num(3.0)),
		NodeType::Container(_, _, _) => JSValue::Number(Number::Num(1.0)),
		NodeType::Document(_) => JSValue::Number(Number::Num(9.0)),
	    }
	} else if name == "childNodes" {
	    match &self.node_type {
		NodeType::Text(_) => JSValue::Array(Vec::new()),
		NodeType::Container(_, children, _) => JSValue::Array(children.borrow().clone().iter().map(|n| Box::new(JSValue::Object(Object::HTML(Rc::clone(&n))))).collect::<Vec<_>>()),
		NodeType::Document(children) => JSValue::Array(children.borrow().clone().iter().map(|n| Box::new(JSValue::Object(Object::HTML(Rc::clone(&n))))).collect::<Vec<_>>()),
	    }
	} else if name == "nodeValue" {
	    match &self.node_type {
		NodeType::Text(t) => JSValue::String(t.to_string()),
		NodeType::Container(_, _, _) => JSValue::Null,
		NodeType::Document(_) => JSValue::Null,
	    }
	} else if name == "style" {
	    JSValue::Object(Object::CSS(Rc::clone(&self.css)))
	} else {
	    if let Some(value) = self.properties.borrow().get(&name) {
		value.clone()
	    } else {
		JSValue::Undefined
	    }
	}
    }
    
    pub fn insert(&self, name: String, value: JSValue) {
	if name == "innerText" {
	    // this is only a method on html elements
	    match &self.node_type {
		NodeType::Text(_) => {},
		NodeType::Container(_, children, _) | NodeType::Document(children) => {
		    *children.borrow_mut() = vec![Rc::new(Node::from_text(format!("{}", value)))];
		}
	    }
	} else {
	    let _ = self.properties.borrow_mut().insert(name, value);
	}
    }
}

pub fn get_element_by_id(node: Rc<Node>, id: JSValue) -> JSValue {
    match &node.node_type {
	NodeType::Text(_) => JSValue::Null,
	NodeType::Container(_, children, attrs) => {
	    if attrs.get("id").map(|s| JSValue::String(s.clone())) == Some(id.clone()) {
		return JSValue::Object(Object::HTML(Rc::clone(&node)));
	    } else {
		for child in &*children.borrow() {
		    if let JSValue::Object(o) = get_element_by_id(Rc::clone(child), id.clone()) {
			return JSValue::Object(o);
		    }
		}
		JSValue::Null
	    }
	},
	NodeType::Document(children) => {
	    for child in &*children.borrow() {
		if let JSValue::Object(o) = get_element_by_id(Rc::clone(child), id.clone()) {
		    return JSValue::Object(o);
		}
	    }
	    JSValue::Null
	}
    }
}


// print tree for debugging
impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.node_type {
	    NodeType::Text(s) => write!(f, "{}\n", s),
	    NodeType::Container(tag_name, children, params) => {
		let mut printed = format!("{}", tag_name);
		if params.len() > 0 {
		    printed += "(";
		    for (param, value) in params {
			printed += &format!("{}=\"{}\",", param, value);
		    }
		    printed.pop();
		    printed += ")";
		}
		if self.css.borrow().len() > 0 {
		    printed += "{";
		    for (key, value) in &*self.css.borrow() {
			printed += &format!("{}:{},", key, value);
		    }
		    printed.pop();
		    printed += "}";
		}
		if !rules::EMPTY_ELEMENTS.iter().any(|e| e==&tag_name) {
		    printed += ":";
		}
		printed += "\n";
		for child in children.borrow().iter() {
		    // make sure that everything is indented
		    for line in format!("{}", child).split("\n") {
			// ignore whitespace
			if line.chars().filter(|c| !c.is_whitespace()).collect::<String>() != "" {
			    printed += &format!("  {}\n", line);
			}
		    }
		}
		write!(f, "{}", printed)
	    },
	    NodeType::Document(children) => {
		let mut printed = String::from("");
		for child in children.borrow().iter() {
		    printed += &format!("{}", child);
		}
		write!(f, "{}", printed)
	    }
	}
    }
}
