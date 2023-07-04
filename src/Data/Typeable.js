// var _tag_id_counter = 1;

export function typeRepDefault0(t) {
  // if(t.__uniqueId == undefined) t.__uniqueId = _tag_id_counter++;
  return t;
}

export const typeRepFromTag1 = pack('tagT');

export function showTypeRep(t) {
  return "" + t;
}

export const proxyT = tag;

// Just a JS class, instances of which can be tested for equality
function tag() { }

export const proxyTFromTagT = pack('tagT');

function pack(tagName) {
  return function(origTag) {
    let unwrappedTag = origTag[tagName];
    return function(dict) {
      if (unwrappedTag.tag) return { tag: unwrappedTag.tag, args: unwrappedTag.args.concat(dict) };
      return { tag: origTag, args: [dict] };
    };
  };
};

export function eqTypeRep(t1) {
  return function(t2) {
    return eqTypeRepHelper(t1, t2);
  };
}

// foreign import typeRowToTypeRep :: forall r rl. RL.RowToList r rl => TypeRow rl -> TypeRep (Record r)
export function typeRowToTypeRep() {
  return function(trow) {
    return trow;
  };
}

// foreign import typeRowCons :: forall s t rs. SProxy s -> String -> TypeRep t -> TypeRow rs -> TypeRow (RL.Cons s t rs)
export function typeRowCons(_) {
  return function(s) {
    return function(t) {
      return function(r) {
        return { record: r.record.concat({ field: s, typ: t }) };
      };
    };
  };
}

export const typeRowNil = { record: [] };

function eqTypeRepHelper(t1, t2) {
  if (t1.tagT) return t1 === t2;
  if (t1.tag !== t2.tag) return false;
  if (t1.record) {
    if (!t2.record) return false;
    if (t1.record.length !== t2.record.length) return false;
    for (var i = 0; i < t1.record.length; i++) {
      if ((t1.record[i].field !== t2.record[i].field)
        || !(eqTypeRepHelper(t1.record[i].typ, t2.record[i].typ))) return false;
    }
    return true;
  }
  if (!t1.args) return false;
  if (t1.args.length !== t2.args.length) return false;
  for (var i = 0; i < t1.args.length; i++) {
    if (!eqTypeRepHelper(t1.args[i].typeRep, t2.args[i].typeRep)) return false;
  }
  return true;
}
