exports.clog = function(x) {
    return function() {
        console.log(x);
    };
};
