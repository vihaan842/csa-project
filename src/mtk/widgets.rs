use super::distance::{Distance, AutoType};
use super::stream::Stream;
use crate::types::{Node, JSValue, AST};

use std::rc::Rc;

use cairo::{Context, Operator, FontFace, FontSlant, FontWeight};

pub trait Content: std::fmt::Debug {
    fn draw(&mut self, context: &Context, stream: &mut Stream, screen: (f64, f64));
    fn draw_root(&mut self, context: &Context, width: i32, height: i32) {
	self.draw(context, &mut Stream::new(0., 0., width as f64, height as f64), (width as f64, height as f64));
    }
    fn propogate_click(&self, x: f64, y: f64, screen: (f64, f64), interpret: Rc<dyn Fn(Box<AST>)>);
}

#[derive(Debug)]
pub struct Block {
    margins: [Distance;4],
    paddings: [Distance;4],
    //borders: [Distance;4],
    width: Distance,
    height: Distance,
    cur_pos: (f64, f64),
    cur_extents: (f64, f64),
    color: [f64;4],
    children: Vec<Box<dyn Content>>,
    associated: Rc<Node>,
}

impl Block {
    pub fn new(associated: Rc<Node>) -> Block {
	Block{
	    margins:[Distance::ZERO, Distance::ZERO, Distance::ZERO, Distance::ZERO],
	    paddings:[Distance::ZERO, Distance::ZERO, Distance::ZERO, Distance::ZERO],
	    //borders:[Distance::ZERO, Distance::ZERO, Distance::ZERO, Distance::ZERO],
	    width: Distance::Auto,
	    height: Distance::Auto,
	    cur_pos: (0., 0.),
	    cur_extents: (0., 0.),
	    color: [0., 0., 0., 0.],
	    children: Vec::new(),
	    associated
	}
    }

    // setters
    pub fn set_margins(&mut self, margins: [Distance;4]) {
	self.margins = margins;
    }
    
    pub fn set_paddings(&mut self, paddings: [Distance;4]) {
	self.paddings = paddings;
    }
    
    // pub fn set_borders(&mut self, borders: [Distance;4]) {
    // 	self.borders = borders;
    // }

    pub fn set_width(&mut self, width: Distance) {
	self.width = width;
    }

    pub fn set_height(&mut self, height: Distance) {
	self.height = height;
    }

    pub fn set_color(&mut self, color: [f64;4]) {
	self.color = color;
    }

    // add a child
    pub fn add_child(&mut self, child: Box<dyn Content>) {
	self.children.push(child);
    }
}

impl Content for Block {
    fn draw(&mut self, context: &Context, parent_stream: &mut Stream, screen: (f64, f64)) {
	let disth = |d: Distance| -> f64 {
	    d.to_absolute(screen.0, AutoType::None)
	};
	let distv = |d: Distance| -> f64 {
	    d.to_absolute(screen.1, AutoType::None)
	};
	
	let mut child_stream = Stream::new(parent_stream.get_x()+disth(self.margins[3])+disth(self.paddings[3]),
					   parent_stream.get_y()+distv(self.margins[0])+distv(self.paddings[0]),
					   parent_stream.get_width()-disth(self.margins[3])-disth(self.margins[1])-disth(self.paddings[1])-disth(self.paddings[3]),
					   parent_stream.get_height());

	for child in &mut self.children {
	    child.draw(context, &mut child_stream, screen);
	}

	let child_width = parent_stream.get_width();
	let disth = |d: Distance| -> f64 {
	    d.to_absolute(screen.0, AutoType::Horizontal(child_width))
	};
	let child_height = child_stream.get_y() - parent_stream.get_y();
	let distv = |d: Distance| -> f64 {
	    d.to_absolute(screen.1, AutoType::Vertical(child_height))
	};

	context.rectangle(
	    parent_stream.get_x()+disth(self.margins[3]),
	    parent_stream.get_y()+distv(self.margins[0]),
	    disth(self.width)-disth(self.margins[1])-disth(self.margins[3]),
	    distv(self.height)-distv(self.margins[2])-distv(self.margins[0])
	);
	context.set_source_rgba(self.color[0], self.color[1], self.color[2], self.color[3]);
	// momentarily draw under
	context.set_operator(Operator::DestOver);
	context.fill().expect("Couldn't fill cairo context");
	// go back
	context.set_operator(Operator::Over);
	
	self.cur_pos = (parent_stream.get_x() + disth(self.margins[3]), parent_stream.get_y() + distv(self.margins[0]));
	
	parent_stream.increase_y((distv(self.height)+distv(self.margins[2])+distv(self.margins[0])).max(child_stream.get_y() - parent_stream.get_y()));

	self.cur_extents = (disth(self.height), parent_stream.get_y() - self.cur_pos.1);
    }

    fn propogate_click(&self, x: f64, y: f64, screen: (f64, f64), interpret: Rc<dyn Fn(Box<AST>)>) {
	if x > self.cur_pos.0 &&
	    y > self.cur_pos.1 &&
	    x < self.cur_pos.0 + self.cur_extents.0 &&
	    y < self.cur_pos.1 + self.cur_extents.1 {
		if self.associated.get("onclick".to_string()) != JSValue::Undefined {
		    interpret(Box::new(AST::FunctionCall(Box::new(AST::Value(self.associated.get("onclick".to_string()))), Vec::new())));
		}
		for child in &self.children {
		    child.propogate_click(x, y, screen, Rc::clone(&interpret));
		}
	    }
    }
}

#[derive(Debug)]
pub struct Text {
    font: String,
    size: Distance,
    text: String,
    color: [f64;4],
    slant: FontSlant,
    weight: FontWeight,
    associated: Rc<Node>,
}

impl Text {
    pub fn new(font: String, size: Distance, text: String, color: [f64;4], slant: FontSlant, weight: FontWeight, associated: Rc<Node>) -> Text {
	Text{font, size, text, color, slant, weight, associated}
    }
}

impl Content for Text {
    fn draw(&mut self, context: &Context, stream: &mut Stream, screen: (f64, f64)) {
	context.set_source_rgba(self.color[0], self.color[1], self.color[2], self.color[3]);
	// create the scaled font
	context.set_font_face(&FontFace::toy_create(&self.font, self.slant, self.weight).unwrap());
	context.set_font_size(self.size.to_absolute(screen.0, AutoType::None));
	let font = context.scaled_font();
	let font_extents = font.extents();
	// draw
	for word in self.text.split(" ") {
	    let (glyphs, _) = font.text_to_glyphs(stream.get_x(), stream.get_y()+font_extents.ascent(), &word).unwrap();
	    let glyph_extents = font.glyph_extents(&glyphs);
	    // wrap
	    if !stream.increase_x_fits(glyph_extents.width()) {
		stream.increase_y(font_extents.height());
		stream.reset_x();
	    }
	    // calculate the glyph positions again
	    let (glyphs, _) = font.text_to_glyphs(stream.get_x(), stream.get_y()+font_extents.ascent(), &word).unwrap();
	    context.glyph_path(&glyphs);
	    stream.increase_x(glyph_extents.x_advance());
	    // handle spaces, which shouldn't be included on newlines
	    {
		let (glyphs, _) = font.text_to_glyphs(stream.get_x(), stream.get_y()+font_extents.ascent(), " ").unwrap();
		let glyph_extents = font.glyph_extents(&glyphs);
		if !stream.increase_x_fits(glyph_extents.width()) {
		    stream.increase_y(font_extents.height());
		    stream.reset_x();
		} else {
		    context.glyph_path(&glyphs);
		    stream.increase_x(glyph_extents.x_advance());
		}
	    }
	}
	stream.increase_y(font_extents.height());
	context.fill().unwrap();
    }

    fn propogate_click(&self, _: f64, _: f64, _: (f64, f64), _: Rc<dyn Fn(Box<AST>)>) { }
}

#[derive(Debug)]
pub struct EmptyContent;

impl Content for EmptyContent {
    fn draw(&mut self, _: &Context, _: &mut Stream, _: (f64, f64)) { }

    fn propogate_click(&self, _: f64, _: f64, _: (f64, f64), _: Rc<dyn Fn(Box<AST>)>) { }
}
