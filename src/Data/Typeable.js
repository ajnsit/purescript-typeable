
export function showTypeRep(t) {
  return "" + t;
}

// Just a JS class, instances of which can be tested for equality
function Tag() { }

export function makeTag(_) {
  return new Tag();
}

export function tag1Impl(origTag) {
  return function(typeRep) {
    if (origTag instanceof Tag) return { tag: origTag, args: [typeRep] };
    else return { tag: origTag.tag, args: origTag.args.concat(typeRep) };
  };
};

export function eqTypeRep(t1) {
  return function(t2) {
    return eqTypeRepHelper(t1, t2);
  };
}

// foreign import typeRowToTypeRepImpl :: forall r rl. RL.RowToList r rl => TypeRow rl -> TypeRep (Record r)
export function typeRowToTypeRepImpl(trow) {
  return trow;
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
  if (t1.record) {
    if (!t2.record) return false;
    if (t1.record.length !== t2.record.length) return false;
    for (var i = 0; i < t1.record.length; i++) {
      if ((t1.record[i].field !== t2.record[i].field)
        || !(eqTypeRepHelper(t1.record[i].typ, t2.record[i].typ))) return false;
    }
    return true;
  }
  if (t1 instanceof Tag) return t1 === t2;
  if (t1.tag !== t2.tag || t1.args.length !== t2.args.length) return false;
  for (var i = 0; i < t1.args.length; i++) {
    if (!eqTypeRepHelper(t1.args[i], t2.args[i])) return false;
  }
  return true;
}
