use std::rc::Rc;

use cairo::{FontWeight, FontSlant};
use crate::{mtk::{distance::Distance, widgets::{Block, Text, EmptyContent, Content}}, types::{Node, NodeType}};

// gets color in rgba
pub fn get_color(color: String) -> [f64;4] {
    if color.starts_with("rgb") {
	let args = color.trim_start_matches("rgb(").trim_end_matches(")").split(",").collect::<Vec<&str>>();
	[args[0].trim().parse::<f64>().unwrap()/255.,
	 args[1].trim().parse::<f64>().unwrap()/255.,
	 args[2].trim().parse::<f64>().unwrap()/255.,
	 1.0]
    } else if color.starts_with("#") {
	let hex_color = color.trim_start_matches("#");
	let r = u32::from_str_radix(&hex_color[0..2], 16).unwrap() as f64;
        let g = u32::from_str_radix(&hex_color[2..4], 16).unwrap() as f64;
        let b = u32::from_str_radix(&hex_color[4..6], 16).unwrap() as f64;
	[r/255.,
	 g/255.,
	 b/255.,
	 1.0]
    } else {
	for (key, value) in crate::rules::DEFAULT_COLORS {
	    if key.to_string() == color {
		return get_color(value.to_string());
	    }
	}
	[0.0, 0.0, 0.0, 1.0]
    }
}

// render nodes into mtk widgets
pub fn render_node(node: Rc<Node>) -> Box<dyn Content> {
    match &node.node_type {
	// document
	NodeType::Document(children) => {
	    let mut content = Block::new(Rc::clone(&node));
	    // get to children
	    for child in children.borrow().iter() {
		content.add_child(render_node(Rc::clone(&child)));
	    }
	    Box::new(content)
	},
	// containers
	NodeType::Container(tag_name, children, _) => {
	    if node.css.borrow().get("display") == Some(&"none".to_string()) {
		return Box::new(EmptyContent);
	    }
	    // find margins and padding
	    let mut margin_left = Distance::ZERO;
	    let mut margin_right = Distance::ZERO;
	    let mut margin_top = Distance::ZERO;
	    let mut margin_bottom = Distance::ZERO;
	    match node.css.borrow().get("margin") {
		Some(m) => {
		    let parts: Vec<&str> = m.split(" ").collect();
		    if parts.len() == 1 {
			margin_left = Distance::from(parts[0]);
			margin_right = Distance::from(parts[0]);
			margin_top = Distance::from(parts[0]);
			margin_bottom = Distance::from(parts[0]);
		    } else if parts.len() == 2 {
			margin_left = Distance::from(parts[1]);
			margin_right = Distance::from(parts[1]);
			margin_top = Distance::from(parts[0]);
			margin_bottom = Distance::from(parts[0]);
		    } else if parts.len() == 3 {
			margin_left = Distance::from(parts[1]);
			margin_right = Distance::from(parts[1]);
			margin_top = Distance::from(parts[0]);
			margin_bottom = Distance::from(parts[2]);
		    } else {
			margin_left = Distance::from(parts[3]);
			margin_right = Distance::from(parts[1]);
			margin_top = Distance::from(parts[0]);
			margin_bottom = Distance::from(parts[2]);
		    }
		},
		None => {},
	    };
	    match node.css.borrow().get("margin-left") {
		Some(m) => margin_left = Distance::from(m.to_string()),
		None => {}
	    };
	    match node.css.borrow().get("margin-right") {
		Some(m) => margin_right = Distance::from(m.to_string()),
		None => {}
	    };
	    match node.css.borrow().get("margin-top") {
		Some(m) => margin_top = Distance::from(m.to_string()),
		None => {}
	    };
	    match node.css.borrow().get("margin-bottom") {
		Some(m) => margin_bottom = Distance::from(m.to_string()),
		None => {}
	    };
	    let mut padding_left = Distance::ZERO;
	    let mut padding_right = Distance::ZERO;
	    let mut padding_top = Distance::ZERO;
	    let mut padding_bottom = Distance::ZERO;
	    match node.css.borrow().get("padding") {
		Some(m) => {
		    let parts: Vec<&str> = m.split(" ").collect();
		    if parts.len() == 1 {
			padding_left = Distance::from(parts[0]);
			padding_right = Distance::from(parts[0]);
			padding_top = Distance::from(parts[0]);
			padding_bottom = Distance::from(parts[0]);
		    } else if parts.len() == 2 {
			padding_left = Distance::from(parts[1]);
			padding_right = Distance::from(parts[1]);
			padding_top = Distance::from(parts[0]);
			padding_bottom = Distance::from(parts[0]);
		    } else if parts.len() == 3 {
			padding_left = Distance::from(parts[1]);
			padding_right = Distance::from(parts[1]);
			padding_top = Distance::from(parts[0]);
			padding_bottom = Distance::from(parts[2]);
		    } else {
			padding_left = Distance::from(parts[3]);
			padding_right = Distance::from(parts[1]);
			padding_top = Distance::from(parts[0]);
			padding_bottom = Distance::from(parts[2]);
		    }
		},
		None => {},
	    };
	    match node.css.borrow().get("padding-left") {
		Some(m) => padding_left = Distance::from(m.to_string()),
		None => {}
	    };
	    match node.css.borrow().get("padding-right") {
		Some(m) => padding_right = Distance::from(m.to_string()),
		None => {}
	    };
	    match node.css.borrow().get("padding-top") {
		Some(m) => padding_top = Distance::from(m.to_string()),
		None => {}
	    };
	    match node.css.borrow().get("padding-bottom") {
		Some(m) => padding_bottom = Distance::from(m.to_string()),
		None => {}
	    };
	    // get width if specified
	    let width = match node.css.borrow().get("width") {
		Some(w) => Distance::from(w.to_string()),
		None => Distance::Auto,
	    };
	    // get height if specified
	    let height = match node.css.borrow().get("height") {
		Some(w) => Distance::from(w.to_string()),
		None => Distance::Auto,
	    };
	    // get color if specified or transparent
	    let color = match node.css.borrow().get("background-color") {
		Some(c) => get_color(c.to_string()),
		None => [1.0, 1.0, 1.0, 0.0]
	    };
	    // set everything
	    let mut content = Box::new(Block::new(Rc::clone(&node)));
	    // weird condition where body's margin is actually padding
	    if tag_name == "body" {
		content.set_margins([margin_top, margin_right, margin_bottom, margin_left]);
	    }
	    content.set_paddings([padding_top, padding_right, padding_bottom, padding_left]);
	    content.set_color(color);
	    content.set_width(width);
	    content.set_height(height);
	    // get to children
	    for child in children.borrow().iter() {
		content.add_child(render_node(Rc::clone(&child)));
	    }
	    content
	},
	NodeType::Text(t) => {
	    // get font/text properties
	    let color = match node.css.borrow().get("color") {
		Some(c) => get_color(c.to_string()),
		None => [0.0, 0.0, 0.0, 1.0]
	    };
	    let font_size = match node.css.borrow().get("font-size") {
		Some(s) => Distance::from(s.to_string()),
		None => Distance::Absolute(crate::rules::DEFAULT_FONT_SIZE as f64)
	    };
	    let weight = match node.css.borrow().get("font-weight") {
		Some(s) => match s.as_str() {
		    "bold" => FontWeight::Bold,
		    _ => FontWeight::Normal,
		},
		None => FontWeight::Normal,
	    };
	    let slant = match node.css.borrow().get("font-style") {
		Some(s) => match s.as_str() {
		    "italic" => FontSlant::Italic,
		    "oblique" => FontSlant::Oblique,
		    _ => FontSlant::Normal,
		},
		None => FontSlant::Normal,
	    };
	    
	    // return the text
	    Box::new(Text::new("DejaVu Sans".to_string(), font_size, t.to_string(), color, slant, weight, Rc::clone(&node)))
	},
    }
}
