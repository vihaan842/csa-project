mod js;
mod js_parser;

use std::io::Read;

// use glib::clone;
// use gtk::glib;
// use gtk::prelude::*;

// fn on_activate(application: &gtk::Application) {
//     let window = gtk::ApplicationWindow::new(application);
//     let button = gtk::Button::with_label("Hello World!");
//     button.connect_clicked(clone!(@weak window => move |_| window.close()));
//     window.set_child(Some(&button));
//     window.present();
// }

fn main() {
    // let app = gtk::Application::builder()
        // .application_id("com.csa-project.test")
        // .build();
    // app.connect_activate(on_activate);
    // // Run the application
    // app.run();

    let mut file = std::fs::File::open("src/test.js").unwrap();
    let mut contents = String::new();
    file.read_to_string(&mut contents).unwrap();

    let parsed = crate::js_parser::parse(&contents);
    
    println!("{:?}", parsed);

    crate::js::interpret(parsed);
}
