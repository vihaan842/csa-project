use std::collections::HashMap;

// css parser
pub fn parse(css: String) -> Vec<(String, HashMap<String, String>)> {
    // all the rules
    let mut rules = Vec::new();
    // params for each selector
    let mut params = HashMap::new();
    // whether parser is in a selector
    let mut in_selector = true;
    // content of selector
    let mut selector_content = String::from("");
    // whether parser is in a key
    let mut in_key = true;
    // whether parser is in a comment
    let mut in_comment = false;
    // content of comment
    let mut comment_content = String::from("");
    // value of the key
    let mut key = String::from("");
    // value of the value
    let mut value = String::from("");
    // go through every character
    for c in css.trim().chars() {
	if in_comment {
	    comment_content.push(c);
	    // ends comment
	    if comment_content.ends_with("*/") {
		in_comment = false;
		comment_content = "".to_string();
	    }
	} else if in_selector {
	    // starts comment
	    if selector_content.ends_with("/*") {
		in_comment = true;
		selector_content.pop();
		selector_content.pop();
	    }
	    // starts params
	    else if c == '{' {
		in_selector = false;
		in_key = true;
	    }
	    else {
		selector_content.push(c);
	    }
	} else if in_key {
	    // starts comment
	    if key.ends_with("/*") {
		in_comment = true;
		key.pop();
		key.pop();
	    }
	    // goes to value
	    else if c == ':' {
		in_key = false;
	    }
	    // ends params
	    else if c == '}'{
		rules.push((selector_content.trim().to_string(), params.clone()));
		params = HashMap::new();
		selector_content = "".to_string();
		in_selector = true;
	    }
	    else {
		key.push(c);
	    }
	} else {
	    // starts comment
	    if value.ends_with("/*") {
		in_comment = true;
		value.pop();
		value.pop();
	    }
	    // goes to next key
	    else if c == ';' {
		in_key = true;
		params.insert(key.trim().to_string(), value.trim().to_string());
		key = "".to_string();
		value = "".to_string();
	    }
	    else {
		value.push(c);
	    }
	}
    }
    return rules;
}
