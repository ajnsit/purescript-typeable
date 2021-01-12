// var _tag_id_counter = 1;

exports.typeRepDefault0 = function(t) {
    // if(t.__uniqueId == undefined) t.__uniqueId = _tag_id_counter++;
    return t;
};

exports.typeRepFromTag1 = pack('tag1');

exports.showTypeRep = function(t) {
    return "" + t;
};

exports.proxy0 = tag;
exports.proxy1 = tag;
exports.proxy2 = tag;
exports.proxy3 = tag;
exports.proxy4 = tag;
exports.proxy5 = tag;
exports.proxy6 = tag;
exports.proxy7 = tag;
exports.proxy8 = tag;
exports.proxy9 = tag;
exports.proxy10 = tag;
exports.proxy11 = tag;

// Just a JS class, instances of which can be tested for equality
function tag() {}

// exports.proxy0FromTag1 = pack('tag1');
exports.proxy1FromTag2 = pack('tag2');
exports.proxy2FromTag3 = pack('tag3');
exports.proxy3FromTag4 = pack('tag4');
exports.proxy4FromTag5 = pack('tag5');
exports.proxy5FromTag6 = pack('tag6');
exports.proxy6FromTag7 = pack('tag7');
exports.proxy7FromTag8 = pack('tag8');
exports.proxy8FromTag9 = pack('tag9');
exports.proxy9FromTag10 = pack('tag10');
exports.proxy10FromTag11 = pack('tag11');

function pack(tagName) {
    return function(origTag) {
        let unwrappedTag = origTag[tagName];
        return function(dict) {
            if(unwrappedTag.tag) return {tag:unwrappedTag.tag, args:unwrappedTag.args.concat(dict)};
            return {tag: origTag, args: [dict]};
        };
    };
};

exports.eqTypeRep = function(t1) {
    return function(t2) {
        return eqTypeRepHelper(t1,t2);
    };
};

// foreign import typeRowToTypeRep :: forall r rl. RL.RowToList r rl => TypeRow rl -> TypeRep (Record r)
exports.typeRowToTypeRep = function() {
    return function(trow) {
        return trow;
    };
};

// foreign import typeRowCons :: forall s t rs. SProxy s -> String -> TypeRep t -> TypeRow rs -> TypeRow (RL.Cons s t rs)
exports.typeRowCons = function(_) {
    return function(s) {
        return function(t) {
            return function(r) {
                return {record: r.record.concat({field:s,typ:t})};
            };
        };
    };
};

// foreign import typeRowNil :: TypeRow RL.Nil
exports.typeRowNil = {record: []};

function eqTypeRepHelper(t1,t2) {
    if(t1.tag0) return t1 === t2;
    if(t1.tag !== t2.tag) return false;
    if(t1.record) {
        if(!t2.record) return false;
        if(t1.record.length !== t2.record.length) return false;
        for(var i=0; i<t1.record.length;i++) {
            if( (t1.record[i].field !== t2.record[i].field)
                || !(eqTypeRepHelper(t1.record[i].typ, t2.record[i].typ))) return false;
        }
        return true;
    }
    if(!t1.args) return false;
    if(t1.args.length !== t2.args.length) return false;
    for (var i=0; i<t1.args.length;i++) {
        if(!eqTypeRepHelper(t1.args[i].typeRep, t2.args[i].typeRep)) return false;
    }
    return true;
}
