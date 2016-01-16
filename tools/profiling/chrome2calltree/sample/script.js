var times = 100000000;

var foo = function() {
    var sum = 0;
    for (var i = 0; i < times; i++) {
        sum += i;
    }
    return sum;
};

var bar = function() {
    var sum = 0;
    for (var i = 0; i < times; i++) {
        sum += i;
    }
    sum += foo();
    sum += foo();
    return sum;
};

var main = function() {
    var sum = 0;
    for (var i = 0; i < times; i++) {
        sum += i;
    }
    sum += foo();
    sum += bar();
    return sum;
};

console.profile("main");
console.log(main());
console.profileEnd();
