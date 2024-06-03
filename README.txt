This is a basic web browser with support for some amount of JavaScript (i.e. dynamic content). Since the main point of this project was to add JavaScript support, I reused some of my previous code that only worked on static content. I will not describe that part of the project as the JavaScript was really the main focus.

The three main files dealing with this are src/js.rs, src/js_parser.rs, and src/types.rs (types.rs also contains some elements relating to the static content browser; I had to do this to avoid circular dependencies.

js_parser.rs implements a simple JavaScript parser (i.e. it turns the JavaScript source file into a format useful for the program). It first "lexes" the file to a list of Tokens (i.e. simple elements) using the `lex` function. Then, it parses the tokens into an Abstract Syntax Tree using the parse function which calls the operator_parse function (to deal with operators), which in turn calls the _parse function, which may call the operator_parse function as well. The end result of this is a format useful for the interpreter.

js.rs contains two functions, interpret and _interpret. _interpret recursively steps through the Abstract Syntax Tree and performs actions to record the state of the interpreter, e.g. defining functions, assigning variables, etc. However, it is not very useful on its own because there are e.g. no functions actually defined to begin with. The interpret function does this necessary setup so that _interpret actually works.

types.rs contains the definitions of the Abstract Syntax Tree, as well as representations of values used in the JavaScript interpreter.

The dependencies required for this are gtk4, cairo, and freetype. These are just used to render the content onto the screen and this is about the most minimal way I found to do this (I could probably replace gtk4, but I am too lazy to do so). Their websites are https://www.gtk.org/, https://www.cairographics.org/, and https://freetype.org/ respectively.

You will also need the Rust programming language and its package manager cargo to compile this program. The website for that is https://www.rust-lang.org/. After installation, you should be able to run:
cargo run
from the directory containing this file and it will run the browser.

This should run the demo file (which has instructions). I could probably implement it so that you can actually view webpages from the internet (I have done this in the past) but I am too lazy to do this and probably the vast majority of websites won't work because of one missing feature of another.

However, the demo by itself I think is pretty impressive and accomplishes my goal to create a basic web browser with simple JavaScript support. You can change the demo as you like, although you should note that I have not implemented many functions that are normally built into JavaScript, and also some features such as bracket access (i.e. a[b]), inline objects (e.g. {property1:value1,property2:value2}), and postfix operators (e.g. ++) do not work, nor does < due to a quirk in the HTML parser. Basic things such as if statements, for loops, while loops, functions, classes, methods, etc. should work as expected.
