class Date {
    constructor(millis) {
	this.time = millis;
    }

    getTime() {
	return this.time;
    }
}

var a = 0;
var b;
b = -6;
for (var i = 0; i != b; i = i - 1) {
    console.log(i)
}
console.test = "hehehe"
console.log(console.test)

var d = new Date(10000);
console.log(d.getTime())


