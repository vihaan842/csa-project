// use cairo::{FontSlant, FontWeight};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Distance {
    // pixels
    Absolute(f64),
    // percent
    Relative(f64),
    // a combination of the two
    // Combo(f64, f64),
    Auto,
}

impl Distance {
    pub const ZERO: Distance = Distance::Absolute(0.0);
    pub fn to_absolute(&self, side: f64) -> f64 {
	match self {
	    Distance::Absolute(d) => *d,
	    Distance::Relative(p) => side * p,
	    Distance::Auto => side,
	}
    }
}

// get distance from string
impl From<String> for Distance {
    fn from(s: String) -> Distance {
	if s.ends_with("em") {
	    // TODO: account for dpi
	    Distance::Absolute(s.trim_end_matches("em").trim().parse::<f64>().unwrap()*crate::rules::DEFAULT_FONT_SIZE as f64)
	} else if s.ends_with("%") {
	    Distance::Relative(s.trim_end_matches('%').trim().parse::<f64>().unwrap()/100.)
	} else if s.ends_with("px") {
	    Distance::Absolute(s.trim_end_matches("px").trim().parse::<f64>().unwrap())
	} else if s == "auto" {
	    Distance::Auto
	} else if s == "0" {
	    Distance::Absolute(0.)
	} else {
	    panic!("This unit is not implemented yet!: {}", s);
	}
    }
}

impl From<&str> for Distance {
    fn from(s: &str) -> Distance {
	Distance::from(s.to_string())
    }
}

// add distances
// impl std::ops::Add for Distance {
//     type Output = Self;

//     fn add(self, other: Self) -> Self {
// 	match self {
// 	    Distance::Absolute(pixels) => {
// 		match other {
// 		    Distance::Absolute(other_pixels) => Distance::Absolute(pixels+other_pixels),
// 		    Distance::Relative(other_percent) => Distance::Combo(pixels, other_percent),
// 		    Distance::Combo(other_pixels, other_percent) => Distance::Combo(pixels+other_pixels, other_percent),
// 		}
// 	    },
// 	    Distance::Relative(percent) => {
// 		match other {
// 		    Distance::Absolute(other_pixels) => Distance::Combo(other_pixels, percent),
// 		    Distance::Relative(other_percent) => Distance::Relative(percent+other_percent),
// 		    Distance::Combo(other_pixels, other_percent) => Distance::Combo(other_pixels, percent+other_percent),
// 		}
// 	    },
// 	    Distance::Combo(pixels, percent) => {
// 		match other {
// 		    Distance::Absolute(other_pixels) => Distance::Combo(pixels+other_pixels, percent),
// 		    Distance::Relative(other_percent) => Distance::Combo(pixels, percent+other_percent),
// 		    Distance::Combo(other_pixels, other_percent) => Distance::Combo(pixels+other_pixels, percent+other_percent),
// 		}
// 	    },
// 	}
//     }
// }

// // subtract distances
// impl std::ops::Sub for Distance {
//     type Output = Self;

//     fn sub(self, other: Self) -> Self {
// 	match self {
// 	    Distance::Absolute(pixels) => {
// 		match other {
// 		    Distance::Absolute(other_pixels) => Distance::Absolute(pixels-other_pixels),
// 		    Distance::Relative(other_percent) => Distance::Combo(pixels, -other_percent),
// 		    Distance::Combo(other_pixels, other_percent) => Distance::Combo(pixels-other_pixels, -other_percent),
// 		}
// 	    },
// 	    Distance::Relative(percent) => {
// 		match other {
// 		    Distance::Absolute(other_pixels) => Distance::Combo(-other_pixels, percent),
// 		    Distance::Relative(other_percent) => Distance::Relative(percent-other_percent),
// 		    Distance::Combo(other_pixels, other_percent) => Distance::Combo(-other_pixels, percent-other_percent),
// 		}
// 	    },
// 	    Distance::Combo(pixels, percent) => {
// 		match other {
// 		    Distance::Absolute(other_pixels) => Distance::Combo(pixels-other_pixels, percent),
// 		    Distance::Relative(other_percent) => Distance::Combo(pixels, percent-other_percent),
// 		    Distance::Combo(other_pixels, other_percent) => Distance::Combo(pixels-other_pixels, percent-other_percent),
// 		}
// 	    },
// 	}
//     }
// }

// // add one distance to another
// impl std::ops::AddAssign for Distance {
//     fn add_assign(&mut self, other: Self) {
// 	*self = *self+other;
//     }
// }

// // subtract one distance from another
// impl std::ops::SubAssign for Distance {
//     fn sub_assign(&mut self, other: Self) {
// 	*self = *self-other;
//     }
// }

// // get distance from string
// impl From<String> for Distance {
//     fn from(s: String) -> Distance {
// 	if s.ends_with("em") {
// 	    // TODO: account for dpi
// 	    Distance::Absolute(s.trim_end_matches("em").trim().parse::<f64>().unwrap()*crate::rules::DEFAULT_FONT_SIZE as f64)
// 	} else if s.ends_with("%") {
// 	    Distance::Relative(s.trim_end_matches('%').trim().parse::<f64>().unwrap()/100.)
// 	} else if s.ends_with("px") {
// 	    Distance::Absolute(s.trim_end_matches("px").trim().parse::<f64>().unwrap())
// 	} else if s == "auto" {
// 	    // placeholder
// 	    Distance::Relative(0.333)
// 	} else if s == "0" {
// 	    Distance::Absolute(0.)
// 	} else {
// 	    panic!("This unit is not implemented yet!: {}", s);
// 	}
//     }
// }

// impl From<&str> for Distance {
//     fn from(s: &str) -> Distance {
// 	Distance::from(s.to_string())
//     }
// }

// // box with size data
// #[derive(Debug, Clone, PartialEq)]
// pub struct LayoutBox {
//     pub height: Option<Distance>,
//     pub width: Option<Distance>,
//     pub visual_width: Distance,
//     pub visual_height: Option<Distance>,
//     pub content: Content,
// }

// impl LayoutBox {
//     pub fn empty() -> LayoutBox {
// 	LayoutBox{width: None,
// 		  height: None,
// 		  visual_width: Distance::Absolute(0.),
// 		  visual_height: None,
// 		  content: Content::Solid(Block::new())}
//     }

//     pub fn contains_point(&self, x: f64, y: f64, width: i32, height: i32, left: f64, top: f64) -> bool {
// 	if let Some(self_width) = self.width {
// 	    if let Some(self_height) = self.height {
// 		x > left && y > top && x < self_width.get_absolute_pos(width) && y < self_height.get_absolute_pos(height)
// 	    } else {
// 		false
// 	    }
// 	} else {
// 	    false
// 	}
//     }
// }

// // content of box
// #[derive(Debug, Clone, PartialEq)]
// pub enum Content {
//     Solid(Block),
//     Text(Label)
// }

// // a block element
// #[derive(Debug, Clone, PartialEq)]
// pub struct Block {
//     pub margin_left: Distance,
//     pub margin_top: Distance,
//     pub margin_right: Distance,
//     pub margin_bottom: Distance,
//     pub padding_left: Distance,
//     pub padding_top: Distance,
//     pub padding_right: Distance,
//     pub padding_bottom: Distance,
//     pub color: [f64;4]
// }

// impl Block {
//     pub fn new() -> Block {
// 	Block{margin_left: Distance::Absolute(0.),
// 	      margin_right: Distance::Absolute(0.),
// 	      margin_top: Distance::Absolute(0.),
// 	      margin_bottom: Distance::Absolute(0.),
// 	      padding_left: Distance::Absolute(0.),
// 	      padding_right: Distance::Absolute(0.),
// 	      padding_top: Distance::Absolute(0.),
// 	      padding_bottom: Distance::Absolute(0.), color:[1.0, 1.0, 1.0, 0.0]}
//     }
// }

// // text
// #[derive(Debug, Clone, PartialEq)]
// pub struct Label {
//     pub text: String,
//     pub font_size: Distance,
//     pub font_color: [f64;4],
//     pub weight: FontWeight,
//     pub slant: FontSlant,
// }
