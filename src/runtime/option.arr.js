// provide *
// provide-types *
//
// import global as g
//
// data Option<a>:
//   | some(elt :: a)
//   | none
// end
var _nothing = undefined;
var $some9 = {"names":["elt"]};
var $none10 = {"names":false};
var Option1 = {"some":function some(elt11) {
  return {"$brand":$some9,
          "$tag":0,
          "elt":elt11};
},
               "none":{"$brand":$none10,
                       "$tag":1},
               "is-some":function some(val) {
                 return val.$brand === $some9;
               },
               "is-none":function none(val) {
                 return val.$brand === $none10;
               }};
var is$Option6 = Option1["Option"];
var is$some5 = Option1["is-some"];
var some4 = Option1["some"];
var is$none3 = Option1["is-none"];
var none2 = Option1["none"];
return module["exports"] = {"is-none":is$none3,
                            "none":none2,
                            "is-Option":is$Option6,
                            "is-some":is$some5,
                            "some":some4,
                            "$answer":_nothing,
                            "$checks":undefined};
