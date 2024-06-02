pub struct Stream {
    start_x: f64,
    start_y: f64,
    x: f64,
    y: f64,
    width: f64,
    height: f64,
}

impl Stream {
    pub fn new(x: f64, y: f64, width: f64, height: f64) -> Stream {
	Stream{start_x: x, start_y: y, x, y, width, height}
    }
    
    pub fn get_x(&self) -> f64 {
	return self.x;
    }

    pub fn get_y(&self) -> f64 {
	return self.y;
    }
    
    pub fn increase_x(&mut self, o: f64) {
	self.x += o;
    }

    pub fn increase_x_fits(&mut self, o: f64) -> bool{
	return self.x + o <= self.start_x + self.width;
    }
    
    pub fn increase_y(&mut self, o: f64) {
	self.y += o;
    }

    pub fn reset_x(&mut self) {
	self.x = self.start_x;
    }

    pub fn reset_y(&mut self) {
	self.y = self.start_y;
    }

    pub fn get_width(&self) -> f64 {
	return self.width;
    }

    pub fn get_height(&self) -> f64 {
	return self.height;
    }
}
