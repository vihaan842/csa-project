use std::rc::Rc;
use std::collections::HashMap;
use crate::types::{Node, NodeType};

// this code is from my previous web browser, mehweb
// I am too lazy to redo parser development

pub fn parse(html: String) -> Rc<Node> {
    // document to return
    let document = Rc::new(Node::get_document());
    // vec containing containers parser is currently in
    let mut current_containers = Vec::new();
    // whether parser is currently inside of tag
    let mut in_tag = false;
    // whether this tag is an open tag
    let mut tag_open = true;
    // text content inside of tag
    let mut tag_content = String::from("");
    // text content inside of containers
    let mut text_content = String::from("");
    // whether parser is in comment
    let mut in_comment = false;
    // stuff inside of comment (to detect end)
    let mut comment_content = String::from("");
    // whether parser is in doctype declaration
    let mut in_doctype = false;
    // special string stuff
    let mut in_str = false;
    let mut str_char = '\"';
    let mut str_ignore_next = false;
    // goes through all characters
    for c in html.chars() {
	if in_tag {
	    if tag_open {
		if in_str {
		    if c == str_char && !str_ignore_next {
			in_str = false;
		    }
		    str_ignore_next = c == '\\' && !str_ignore_next;
		} else {
		    if c == '\'' || c == '\"' {
			in_str = true;
			str_char = c.clone();
		    }
		}
		if c == '/' && tag_content == "".to_string() {
		    tag_open = false;
		} else if tag_content == "!--" && !in_str {
		    in_tag = false;
		    in_comment = true;
		    tag_content = "".to_string();
		} else if tag_content.to_uppercase() == "!DOCTYPE" && !in_str {
		    in_doctype = true;
		    tag_content = "".to_string();
		} else if c == '>' && !in_str {
		    in_tag = false;
		    if in_doctype {
			in_doctype = false;
		    } else {
			// adds new container node
			let node = Rc::new(Node::from_tag(tag_content.clone()));
			if !node.is_empty_element() {
			    current_containers.push(node);
			} else {
			    if current_containers.len() > 0 {
				let index = current_containers.len()-1;
				current_containers[index].children().borrow_mut().push(Rc::clone(&node));
				*node.parent.borrow_mut() = Some(Rc::clone(&current_containers[index])).to_owned();
			    } else {
				document.children().borrow_mut().push(Rc::clone(&node));
			    }
			}
		    }
		    tag_content = "".to_string();
		} else if !(in_str && c == str_char && !str_ignore_next) && !(!in_str && (c == '\'' || c == '\"')){
		    tag_content.push(c);
		}
	    } else {
		if c == '>' {
		    in_tag = false;
		    // checks if there are multiple containers
		    if current_containers.len() > 1 {
			// removes node but adds as child to parent node
			let index = current_containers.len()-2;
			let node = current_containers.remove(current_containers.len()-1);
			current_containers[index].children().borrow_mut().push(Rc::clone(&node));
			*node.parent.borrow_mut() = Some(Rc::clone(&current_containers[index])).to_owned();
		    } else {
			document.children().borrow_mut().push(Rc::clone(&current_containers.remove(0)));
		    }
		    tag_content = "".to_string();
		} else {
		    tag_content.push(c);
		}
	    }
	} else {
	    if in_comment {
		comment_content.push(c);
		if comment_content.ends_with("-->") {
		    in_comment = false;
		    comment_content = "".to_string();
		}
	    } else {
		if c == '<' {
		    in_tag = true;
		    tag_open = true;
		    if text_content.trim() != "".to_string() {
			// adds new text node
			let node = Rc::new(Node::from_text(text_content));
			let index = current_containers.len()-1;
			current_containers[index].children().borrow_mut().push(Rc::clone(&node));
			*node.parent.borrow_mut() = Some(Rc::clone(&current_containers[index])).to_owned();

		    }
		    text_content = "".to_string();
		} else {
		    text_content.push(c);
		}
	    }
	}
    }
    return document;
}

// check wheter css selector applies
fn selector_applies(node: Rc<Node>, selector: String) -> bool {
    let mut descendants = selector.split(" ").collect::<Vec<&str>>();
    let mut applies = true;
    if node.basic_selector_applies(descendants[descendants.len()-1].to_string()) {
	descendants.pop();
	descendants.reverse();
    } else {
	return false;
    }
    let mut current_node = Rc::clone(&node);
    for basic_selector in descendants {
	let mut basic_applies = false;
	loop {
	    if current_node.basic_selector_applies(basic_selector.to_string()) {
		basic_applies = true;
		break;
	    }
	    match &*Rc::clone(&current_node).parent.borrow() {
		None => break,
		Some(n) => current_node = Rc::clone(&n),
	    }
	}
	if !basic_applies {
	    applies = false;
	    break;
	}
    }
    return applies;
}


// apply css to all nodes
pub fn apply_css(css_rules: Vec<(String, HashMap<String, String>)>, node: Rc<Node>) {
    match &node.node_type {
	NodeType::Document(children) => {
	    // get to all the other nodes in tree
	    for child in children.borrow().iter() {
		apply_css(css_rules.clone(), Rc::clone(child));
	    }
	},
	NodeType::Container(tag_name, children, params) => {
	    // applies default rules
	    let default = crate::rules::DEFAULT_CSS.iter().find(|t| t.0 == tag_name);
	    match default {
		Some((_, default)) => {
		    let rules = default.split(";");
		    for rule in rules {
			let parts = rule.split(":").collect::<Vec<&str>>();
			if parts.len() == 2 {
			    let key = parts[0].trim().to_string();
			    let value = parts[1].trim().to_string();
			    apply_css_rule(Rc::clone(&node), key, value);
			}
		    }
		},
		None => {},
	    }
	    // applies rules
	    for (selector, rules) in css_rules.clone() {
		if selector_applies(Rc::clone(&node), selector) {
		    for (key, value) in rules {
			apply_css_rule(Rc::clone(&node), key, value);
		    }
		}
	    }
	    // applies inline rules
	    match params.get("style") {
		None => {},
		Some(s) => {
		    let rules = s.split(";");
		    for rule in rules {
			let parts = rule.split(":").collect::<Vec<&str>>();
			if parts.len() == 2 {
			    let key = parts[0].trim().to_string();
			    let value = parts[1].trim().to_string();
			    apply_css_rule(Rc::clone(&node), key, value);
			}
		    }
		}
	    }
	    // get to all the other nodes in tree
	    for child in children.borrow().iter() {
		apply_css(css_rules.clone(), Rc::clone(child));
	    }
	},
	_ => {}
    }
}

fn apply_css_rule(node: Rc<Node>, key: String, value: String) {
    match &node.node_type {
	NodeType::Container(_, children, _) => {
	    if crate::rules::INHERITED_PROPERTIES.iter().any(|e| e==&key) {
		for child in children.borrow_mut().iter() {
		    apply_css_rule(Rc::clone(child), key.clone(), value.clone());
		}
	    }
	},
	_ => {},
    }
    node.css.borrow_mut().insert(key, value);
}

