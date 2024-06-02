mod js;
mod js_parser;
mod types;
mod rules;
mod html;
mod css;
mod render;
mod mtk;

use crate::types::Object;
use crate::mtk::widgets::Content;

use std::io::Read;
use std::rc::Rc;
use std::cell::RefCell;

// use glib::clone;
// use gtk::glib;
use gtk::prelude::*;
use gtk::{Application, ApplicationWindow, DrawingArea, GestureClick};

fn main() {
    let app = Application::builder()
        .application_id("org.example.mehweb2")
        .build();

    let document: Rc<RefCell<Option<Box<dyn Content>>>> = Rc::new(RefCell::new(None));
    let document_setter = Rc::clone(&document);

    let mut file = std::fs::File::open("src/test.html").unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();
    let parsed_html = crate::html::parse(contents);
    let css = parsed_html.find_css();
    let parsed_css = css::parse(css);
    html::apply_css(parsed_css.clone(), Rc::clone(&parsed_html));
    let js = parsed_html.find_js();
    let parsed_js = js_parser::parse(&js);
    let vars = Rc::new(RefCell::new(js::interpret(parsed_js, Object::HTML(Rc::clone(&parsed_html)))));
    *document_setter.borrow_mut() = Some(crate::render::render_node(parsed_html));

    let interpret: Rc<dyn Fn(Box<types::AST>)> = Rc::new(move |code| {
	let vars = Rc::clone(&vars);
	let _ = js::_interpret(code, &mut vars.borrow_mut());
    });
    
    app.connect_activate(move |app| {
	let document = Rc::clone(&document);
	let document_click = Rc::clone(&document);
        let win = ApplicationWindow::builder()
            .application(app)
            .default_width(500)
            .default_height(500)
            .title("MehWeb 2")
            .build();

	let drawing_area = DrawingArea::new();
	drawing_area.set_draw_func(move |_, cr, width, height| {
	    let document = Rc::clone(&document);
	    match &*document.borrow() {
		Some(document) => {
		    document.draw_root(cr, width, height);
		},
		None => {}
	    };
	});
	drawing_area.set_size_request(500, 500);

	let interpret = Rc::clone(&interpret);
	let key_press = GestureClick::new();
	key_press.connect_pressed(move |gesture, _n, x, y| {
	    let document = Rc::clone(&document_click);
	    let widget = gesture.widget();
	    
	    document.borrow().as_ref().expect("big oops").propogate_click(x, y, (widget.width().into(), widget.height().into()), Rc::clone(&interpret));
	});

	drawing_area.add_controller(key_press);
	
	win.set_child(Some(&drawing_area));

        win.present();
    });

    // let mut file = std::fs::File::open("src/test.js").unwrap();
    // let mut contents = String::new();
    // file.read_to_string(&mut contents).unwrap();

    // let parsed = crate::js_parser::parse(&contents);
    
    // println!("{:?}", parsed);

    // crate::js::interpret(parsed);

    app.run();
}
