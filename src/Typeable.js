exports.typerepImpl1 = function(t1) {
  return t1;
};

exports.typerepImpl2 = function(t1) {
  return function(t2) {
    return [t1,t2];
  };
};

exports.typerepImpl3 = function(t1) {
    return function(t2) {
        return function(t3) {
            return [t1,t2,t3];
        };
    };
};

exports.eqTypeRep = function(t1) {
    return function(t2) {
        return nestedArrayEq(t1, t2);
    };
};

exports.clog = function(a) {
    return function() {
        console.log("CLOG:", a);
    };
};

function nestedArrayEq(t1, t2) {
  if(Array.isArray(t1)) {
      if(!(Array.isArray(t2) && (t1.length == t2.length)))
          return false;
      for (var i=0; i<t1.length;i++) {
          if(!nestedArrayEq(t1[i], t2[i]))
              return false;
      }
      return true;
  }
  return t1 === t2;
}
