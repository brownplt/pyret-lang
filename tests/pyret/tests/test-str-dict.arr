import str-dict as SD
import string-dict as SDOLD
import valueskeleton as VS

sd1 = SD.make-mutable-string-dict()
sd2 = [SD.mutable-string-dict: {"a"; 15}, {"b"; 10}]

sd3 = [SD.mutable-string-dict: {"a"; 15}, {"b"; 10}]
sd4 = [SD.mutable-string-dict: {"a"; 15}, {"b"; 20}]
sd5 = [SD.mutable-string-dict: {"a"; 15}, {"b"; 10}, {"c"; 15}]

isd2 = [SD.string-dict:{"a"; 15}, {"b"; 10}]

isd3 = [SD.string-dict: {"a"; 15}, {"b"; 10}]
isd4 = [SD.string-dict: {"a"; 15}, {"b"; 20}]
isd5 = [SD.string-dict: {"a"; 15}, {"b"; 10}, {"c"; 15}]

check "basics":

  sd1.set-now("a", 5) is nothing
  sd1.get-value-now("a") is 5
  sd1.get-now("a").or-else(1) is 5
  sd1.set-now("a", 10) is nothing
  sd1.get-value-now("a") is 10
  sd1.get-now("a").or-else(1) is 10

  sd1.get-now("c").or-else(42) is 42
  sd1.get-value-now("c") raises "Key c not found"

  sd2.get-now("a").or-else(42) is 15
  sd2.get-value-now("b") is 10

  fun check-sdstr(s):
    (s == "[mutable-string-dict: { \"a\"; 15 }, { \"b\"; 10 }]") or
      (s == "[mutable-string-dict: { \"b\"; 10 }, { \"a\"; 15 }]")
  end
  torepr(sd2) satisfies check-sdstr

  var long-torepr = nothing
  long-torepr := {
    method _output(self) block:
      var str = ""
      for each(i from range(0, 10000)):
        str := tostring(i)
      end
      VS.vs-value(str)
    end
  }

  torepr([SD.mutable-string-dict: {"a"; long-torepr}]) is "[mutable-string-dict: { \"a\"; \"9999\" }]"

  sd2.keys-now() is [tree-set: "a", "b"]
  sd2.keys-now() is [tree-set: "b", "a"]

  sd2.has-key-now("a") is true
  sd2.has-key-now("z") is false

  sd2 is sd3
  sd2 is-not sd4
  sd2 is-not sd5
  sd2 is-not 2

  [SD.mutable-string-dict: {"a"; 5}] is-not [SD.mutable-string-dict: {"b"; 5}]
  [SD.mutable-string-dict: {"a"; 5}, {"b"; 5}]
    is-not [SD.mutable-string-dict: {"b"; 5}, {"c"; 5}]
  sd-many-as = [SD.mutable-string-dict:]
  sd-almost-many-as = [SD.mutable-string-dict: {"a"; 10}]
  for each(i from range(0, 100)) block:
    sd-many-as.set-now("a" + tostring(i), i)
    when not(i == 54):
      sd-almost-many-as.set-now("a" + tostring(i), i)
    end
  end
  sd-many-as is-not sd-almost-many-as
  sd-almost-many-as is-not sd-many-as

  isd2.keys() is [tree-set: "a", "b"]

  isd2.has-key("a") is true
  isd2.has-key("z") is false
end

check "Immutable string dicts":

  isd2 is-not sd2
  isd3 is-not sd3
  isd4 is-not sd4
  isd5 is-not sd5

  isd2 is isd3
  isd2 is-not isd4
  isd2 is-not isd5
  isd2 is-not 2

  [SD.string-dict: {"a"; 5}] is-not [SD.string-dict: {"b"; 5}]
  [SD.string-dict: {"a"; 5}, {"b"; 5}]
    is-not [SD.string-dict: {"b"; 5}, {"c"; 6}]

  sd-many-as = for fold(ad from [SD.string-dict:], i from range(0, 100)):
    ad.set("a" + tostring(i), i)
  end
  sd-almost-many-as = for fold(ad from [SD.string-dict: {"a"; 5}], i from range(0, 100)):
    if i == 54:
      ad
    else:
      ad.set("a" + tostring(i), i)
    end
  end
  sd-many-as is-not sd-almost-many-as
  sd-almost-many-as is-not sd-many-as

  isd6 = isd5.set("a", 7)

  isd5 is-not isd6
  isd5.get-value("a") is 15
  isd6.get-value("a") is 7
  isd6.get("b").or-else(42) is 10

  sd5.remove-now("a")
  sd5.get-value-now("a") raises "Key a not found"

  isd7 = isd6.remove("a")
  isd7.get-value("a") raises "Key a not found"
  isd6.get-value("a") is 7

  sd7 = [SD.mutable-string-dict: {"a"; false}]
  sd7.has-key-now("a") is true
  sd7.has-key-now("b") is false

  sd5.count-now() is 2
  isd5.count() is 3

  isd8 = sd5.freeze()
  isd8.get-value("b") is 10
  sd5.set-now("b", 5) is nothing
  sd5.get-value-now("b") is 5
  isd8.get-value("b") is 10

  sd8 = isd5.unfreeze()
  sd8.get-value-now("a") is 15
  sd8.set-now("a", 23) is nothing
  sd8.get-value-now("a") is 23
  isd5.get-value("a") is 15

  sd9 = sd3.seal()
  sd9.get-value-now("a") is 15
  sd9.set-now("b", 20) raises "Cannot modify sealed string dict"
  sd3.set-now("b", 20) is nothing
  sd9.get-value-now("b") is 20
end

check "remove and unfreeze":
  d0 = [SD.string-dict: {"A"; 1}, {"B"; 2}, {"C"; 3}]
  d1 = d0.remove("A")
  d2 = d1.unfreeze()
  first-key = d2.keys-list-now().first
  first-val = d2.get-now(first-key)
  first-val is-not none
end

check "cyclic":
  s1 = [SD.mutable-string-dict: {"a"; nothing}]
  s1.set-now("a", s1)
  torepr(s1) is "[mutable-string-dict: { \"a\"; <cyclic-object-1> }]"
end

fun one-of(ans, elts):
  is-some(for find(elt from elts):
    ans == elt
  end)
end 

check "merge":
  s1 = [SD.string-dict: {"a"; 5}, {"c"; 4}]
  s2 = [SD.string-dict: {"a"; 10}, {"b"; 6}]
  s3 = s1.merge(s2)
  s3 is [SD.string-dict: {"a"; 10}, {"b"; 6}, {"c"; 4}]
  s2.merge(s1) is [SD.string-dict: {"a"; 5}, {"b"; 6}, {"c"; 4}]
  s2.merge(s1).merge(s1) is s2.merge(s1)

  s1.keys-list() is%(one-of) [list: [list: "a", "c"], [list: "c", "a"]]
  s2.keys-list() is%(one-of) [list: [list: "a", "b"], [list: "c", "a"]]

  s4 = [SD.string-dict: {"a"; 5}]
  s5 = [SD.string-dict:]
  s4.merge(s5) is s4
  s5.merge(s4) is s4
end 

check "duplicate":
  [SD.string-dict: {"x"; 5}, {"x"; 10}] raises "duplicate key x"
  [SD.string-dict: {"x"; 6}, {"y"; 10}, {"x"; 22}] raises "duplicate key"
  [SD.string-dict: {"x"; 6}, {"y"; 10}, {"z"; 22}] does-not-raise
end 

check "sdo":
  SD.string-dict-of([list: "x", "y", "z"], 5) is [SD.string-dict: {"x"; 5}, {"y"; 5}, {"z"; 5}]
end 

check "predicates":
  SD.is-mutable-string-dict(sd1) is true
  SD.is-string-dict(sd1) is false
  SD.is-mutable-string-dict(isd2) is false
  SD.is-string-dict(isd2) is true
  SD.is-mutable-string-dict(1) is false
  SD.is-string-dict(1) is false
end 

check "merge-now":
  s1 = [SD.mutable-string-dict: {"a"; 5}, {"c"; 4}]
  s2 = [SD.mutable-string-dict: {"a"; 10}, {"b"; 6}]

  isd9 = s1.freeze()
  isd10 = s2.freeze()

  s1.merge-now(s2) is nothing
  s1 is=~ [SD.mutable-string-dict: {"a"; 10}, {"b"; 6}, {"c"; 4}]

  orig-s1 = isd9.unfreeze()
  s2.merge-now(orig-s1) is nothing
  s2 is=~ [SD.mutable-string-dict: {"a"; 5}, {"b"; 6}, {"c"; 4}]

  isd9.keys-list()  is%(one-of) [list: [list: "a", "c"], [list: "c", "a"]]
  isd10.keys-list() is%(one-of) [list: [list: "a", "b"], [list: "b", "a"]]

  orig-s2 = isd10.unfreeze()
  new-s1 = orig-s1
  new-s1.merge-now(orig-s2) is nothing
  new-s1 is=~ [SD.mutable-string-dict: {"a"; 10}, {"b"; 6}, {"c"; 4}]

  s4 = [SD.mutable-string-dict: {"a"; 5}]
  s5 = [SD.mutable-string-dict:]
  s4.merge-now(s5) is nothing
  s4 is=~ [SD.mutable-string-dict: {"a"; 5}]
  s5.merge-now(s4) is nothing
  s5 is=~ [SD.mutable-string-dict: {"a"; 5}]
end

check "longer dict":
  s1 = [SD.string-dict: {"a"; 15}, {"b"; 10}, {"c"; 15}, {"d"; 41}, {"e"; 32}, {"f"; 42}, {"g"; 1}]
  s2 = [SD.string-dict: {"h"; 10}, {"i"; 641}]
  s3 = s1.merge(s2)
  s3 is [SD.string-dict: {"a"; 15}, {"b"; 10}, {"c"; 15}, {"d"; 41}, {"e"; 32}, {"f"; 42}, {"g"; 1}, {"h"; 10}, {"i"; 641}]
  SD.is-string-dict(s1) is true
  s1.get-value("a") is 15
  s1.get-value("b") is 10
  s1.get-value("c") is 15
  s1.get-value("d") is 41
  s1.get-value("e") is 32
  s1.get-value("f") is 42
  s1.get-value("g") is 1
end

check "items":
 s1 = [SD.mutable-string-dict: {"a"; 2}, {"g"; 4}]
 s1items = s1.items()
 s1items.get(0) is {"a"; 2}
 s1items.get(1) is {"g"; 4}
 s2 = [SD.string-dict: {"a"; 15}, {"b"; 10}, {"c"; 15}, {"d"; 41}, {"e"; 32}, {"f"; 42}, {"g"; 1}]
 s2items = s2.items()
 s2items.get(0) is {"a"; 15}
 s2items.get(4) is {"e"; 32}
 s2items.get(6) is {"g"; 1}
 sum = for fold(sum from 0, tup from s2.items()):
   sum + tup.{1}
 end
 sum is 156

 var lst1 = [list-set: ]
 fun f(x):
    lst1 := lst1.add(x)
 end

 var lst2 = [list-set: ]

 var lst = [list-set: ]

 fun g(x):
    lst2 := lst2.add(x)
 end

 s1.each(f) is nothing
 lst1 is [list-set:  {"a"; 2}, {"g"; 4}]

 s2.each(g) is nothing
 lst2 is [list-set: {"a"; 15}, {"b"; 10}, {"c"; 15}, {"d"; 41}, {"e"; 32}, {"f"; 42}, {"g"; 1} ]

  for SD.dict-each(tup from s2):
    {k;v} = tup
    lst := lst.add({k;v})
  end

  lst is lst2
end

check "all forms of fors":
  
  fun createDict(length):
    for fold(dict from [SD.string-dict: ], i from range(0, length)):
       dict.set(tostring(random(length * 10)), random(length * 10))
    end
  end

  fun createOldDict(dict):
    key_lst = dict.keys-list()
    for fold(old_dict from [SDOLD.string-dict: ], i from range(0, dict.count())):
      old_dict.set(key_lst.get(i), dict.get(key_lst.get(i)))
     end
   end
  
  fun createMutableDict(dict) block:
    key_lst = dict.keys-list()
    mutDict = [SD.mutable-string-dict:]
    for each(key from key_lst):
      num = cases(Option) dict.get(key):
       | none => 0
       | some(v) => v
      end
      mutDict.set-now(key, num)
     end
    mutDict
   end 
   
 dict = createDict(3000)
 oldDict = createOldDict(dict)
 mutDict = createMutableDict(dict)

 
 fun sum_old_each_old_dict(shadow dict) block:
  var sum = 0
  for each(name from dict.keys-list()):
    values = dict.get-value(name)
    num = cases(Option) values:
    | none => 0
    | some(v) => v
    end
    sum := sum + num
    end
  sum
 end

 fun sum_old_each(shadow dict) block:
   var sum = 0
   for each(name from dict.keys-list()):
     sum := sum + dict.get-value(name)
   end
   sum
 end

  fun sum_old_each_mut(shadow dict) block:
   var sum = 0
   for each(name from dict.keys-list-now()):
     sum := sum + dict.get-value-now(name)
   end
   sum
 end

 fun sum_new_dict_new_each(shadow dict) block:
   var sum = 0
   for each(tup from dict.items()):
     sum := sum + tup.{1}
   end
   sum
 end

 fun sum_new_dict_each(shadow dict) block:
   var sum = 0
   for SD.dict-each(tup from dict):
     sum := sum + tup.{1}
   end
   sum
 end

 fun sum_new_dict_loop(shadow dict) block:
  var sum = 0
  for SD.dict-each-loop(tup from dict):
    sum := sum + tup.{1}
  end 
  sum
 end

 sum_from_old = sum_old_each_old_dict(oldDict)
 sum_from_old is sum_old_each(dict)
 sum_from_old is sum_old_each_mut(mutDict)
 sum_from_old is sum_new_dict_new_each(dict)
 sum_from_old is sum_new_dict_new_each(mutDict)
 sum_from_old is sum_new_dict_each(dict)
 sum_from_old is sum_new_dict_each(mutDict)
 sum_from_old is sum_new_dict_loop(dict)
 sum_from_old is sum_new_dict_loop(mutDict)

 fun dostuff(x):
  range(0, x)
  end 

 fun sum_old_each_old_dict_b(shadow dict) block:
  var sum = 0
  for each(name from dict.keys-list()) block:
    values = dict.get-value(name)
    num = cases(Option) values:
    | none => 0
    | some(v) => v
    end
    sum := sum + num
    when num-modulo(num, 337) == 0:
  	dostuff(1000)
    end
    end
  sum
 end

 fun sum_old_each_b(shadow dict) block:
   var sum = 0
   for each(name from dict.keys-list()) block:
     sum := sum + dict.get-value(name)
     when num-modulo(dict.get-value(name), 337) == 0:
  	dostuff(1000)
      end
   end
   sum
 end

  fun sum_old_each_mut_b(shadow dict) block:
   var sum = 0
   for each(name from dict.keys-list-now()) block:
     sum := sum + dict.get-value-now(name)
      when num-modulo(dict.get-value-now(name), 337) == 0:
  	dostuff(1000)
      end
   end
   sum
 end

 fun sum_new_dict_new_each_b(shadow dict) block:
   var sum = 0
   for each(tup from dict.items()) block:
     sum := sum + tup.{1}
     when num-modulo(tup.{1}, 337) == 0:
  	dostuff(1000)
      end
   end
   sum
 end

 fun sum_new_dict_each_b(shadow dict) block:
   var sum = 0
   for SD.dict-each(tup from dict) block:
     sum := sum + tup.{1}
     when num-modulo(tup.{1}, 337) == 0:
  	dostuff(1000)
     end
   end
   sum
 end

 fun sum_new_dict_loop_b(shadow dict) block:
  var sum = 0
  for SD.dict-each-loop(tup from dict) block:
    sum := sum + tup.{1}
    when num-modulo(tup.{1}, 337) == 0:
  	dostuff(1000)
    end
  end 
  sum
 end
 
 sum_from_old_b = sum_old_each_old_dict_b(oldDict)
 sum_from_old_b is sum_old_each_b(dict)
 sum_from_old_b is sum_old_each_mut_b(mutDict)
 sum_from_old_b is sum_new_dict_new_each_b(dict)
 sum_from_old_b is sum_new_dict_new_each_b(mutDict)
 sum_from_old_b is sum_new_dict_each_b(dict)
 sum_from_old_b is sum_new_dict_each_b(mutDict)
 sum_from_old_b is sum_new_dict_loop_b(dict)
 sum_from_old_b is sum_new_dict_loop_b(mutDict)

  

 fun set_old_each_old_dict(shadow dict) block:
    var val-set = [list-set: ]
    for each(name from dict.keys-list()) block:
      values = dict.get-value(name)
      num = cases(Option) values:
        | none => 0
        | some(v) => v
      end
      val-set := val-set.add(num)
    end
    val-set
  end

  fun set_old_each(shadow dict) block:
    var val-set = [list-set: ]
    for each(name from dict.keys-list()) block:
      val-set := val-set.add(dict.get-value(name))
    end
    val-set
  end

  fun set_old_each_mut(shadow dict) block:
    var val-set = [list-set: ]
    for each(name from dict.keys-list-now()) block:
      val-set := val-set.add(dict.get-value-now(name))
    end
    val-set
  end

  fun set_new_dict_new_each(shadow dict) block:
    var val-set = [list-set: ]
    for each(tup from dict.items()) block:
      val-set := val-set.add(tup.{1})
    end
    val-set
  end

  fun set_new_dict_each(shadow dict) block:
    var val-set = [list-set: ]
    for SD.dict-each(tup from dict) block:
      val-set := val-set.add(tup.{1})
    end
    val-set
  end

  fun set_new_dict_loop(shadow dict) block:
    var val-set = [list-set: ]
    for SD.dict-each-loop(tup from dict) block:
      val-set := val-set.add(tup.{1})
    end 
    val-set
  end

  
  smalldict = createDict(500)
  smalloldDict = createOldDict(smalldict)
  smallmutDict = createMutableDict(smalldict)

  set_from_old = set_old_each_old_dict(smalloldDict)
  set_from_old is set_old_each(smalldict)
  set_from_old is set_old_each_mut(smallmutDict)
  set_from_old is set_new_dict_new_each(smalldict)
  set_from_old is set_new_dict_new_each(smallmutDict)
  set_from_old is set_new_dict_each(smalldict)
  set_from_old is set_new_dict_each(smallmutDict)
  set_from_old is set_new_dict_loop(smalldict)
  set_from_old is set_new_dict_loop(smallmutDict)
 


 fun set_old_each_old_dict_b(shadow dict) block:
     var val-set = [list-set: ]
     for each(name from dict.keys-list()) block:
       values = dict.get-value(name)
       num = cases(Option) values:
       | none => 0
       | some(v) => v
       end
      val-set := val-set.add(num)
      when num-modulo(num, 339) == 0:
  	dostuff(1000)
     end
      end
    val-set
  end

 fun set_old_each_b(shadow dict) block:
   var val-set = [list-set: ]
   for each(name from dict.keys-list()) block:
     val-set := val-set.add(dict.get-value(name))
     when num-modulo(dict.get-value(name), 339) == 0:
  	dostuff(1000)
     end
   end
   val-set
 end

  fun set_old_each_mut_b(shadow dict) block:
   var val-set = [list-set: ]
   for each(name from dict.keys-list-now()) block:
     val-set := val-set.add(dict.get-value-now(name))
     when num-modulo(dict.get-value-now(name), 339) == 0:
  	dostuff(1000)
     end
   end
   val-set
 end

 fun set_new_dict_new_each_b(shadow dict) block:
   var val-set = [list-set: ]
   for each(tup from dict.items()) block:
     val-set := val-set.add(tup.{1})
     when num-modulo(tup.{1}, 339) == 0:
  	dostuff(1000)
    end
   end
   val-set
 end

 fun set_new_dict_each_b(shadow dict) block:
   var val-set = [list-set: ]
   for SD.dict-each(tup from dict) block:
     val-set := val-set.add(tup.{1})
     when num-modulo(tup.{1}, 339) == 0:
  	dostuff(1000)
     end
   end
   val-set
 end

 fun set_new_dict_loop_b(shadow dict) block:
  var val-set = [list-set: ]
  for SD.dict-each-loop(tup from dict) block:
    val-set := val-set.add(tup.{1})
    when num-modulo(tup.{1}, 339) == 0:
  	dostuff(1000)
    end
  end 
  val-set
end
 

 set_from_old_b = set_old_each_old_dict_b(smalloldDict)
 set_from_old_b is set_old_each_b(smalldict)
 set_from_old_b is set_old_each_mut_b(smallmutDict)
 set_from_old_b is set_new_dict_new_each_b(smalldict)
 set_from_old_b is set_new_dict_new_each_b(smallmutDict)
 set_from_old_b is set_new_dict_each_b(smalldict)
 set_from_old_b is set_new_dict_each_b(smallmutDict)
 set_from_old_b is set_new_dict_loop_b(smalldict)
 set_from_old_b is set_new_dict_loop_b(smallmutDict)

 fun mut_sub_from_all(mutdict, toSub) block:
    key_lst = mutdict.keys-list-now()
    for each(key from key_lst) block:
      num = cases(Option) mutdict.get-now(key):
       | none => 0
       | some(v) => v
      end
      mutDict.set-now(key, num - toSub)
     end
     nothing
   end 

 mut_sub_from_all(mutDict, 3)
 sum_with_sub = sum_from_old - (3 * dict.count())
 sum_with_sub is sum_old_each_mut(mutDict)
 sum_with_sub is sum_new_dict_new_each(mutDict)
 sum_with_sub is sum_new_dict_each(mutDict)
 sum_with_sub is sum_new_dict_loop(mutDict)

  [SD.string-dict: {"a"; 4}, {"a"; 3}] raises "duplicate key a"
  [SD.string-dict: {"goo"; 4}, {"goo"; 3}, {"qwf"; 51}] raises "duplicate key goo"
  [SD.string-dict: {"hello"; 41}, {"hi";53}, {"hi"; 51}] raises "duplicate key hi"
  [SD.string-dict: {"hello"; 41}, {"hi";53}, {"hello"; 51}] raises "duplicate key hello"
  [SD.string-dict: {"hello"; 141}, {"hello"; 41}, {"hi";53}, {"fqf"; 51}] raises "duplicate key hello"
  [SD.string-dict: {"hi"; 141}, {"hello"; 41}, {"hello";53}, {"fqf"; 51}] raises "duplicate key hello" 
  [SD.string-dict: {"hello"; 141}, {"hi"; 41}, {"Sarah";53}, {"Sarah"; 51}] raises "duplicate key Sarah" 
  [SD.string-dict: {"Sarah"; 141}, {"hello"; 41}, {"Sarah";53}, {"Sarah2"; 51}] raises "duplicate key Sarah"
  [SD.string-dict: {"hello"; 141}, {"hi"; 41}, {"Sarah";53}, {"hello"; 51}] raises "duplicate key hello"
  [SD.string-dict: {"hello"; 141}, {"hi"; 41}, {"Sarah";53}, {"hi"; 51}] raises "duplicate key hi"
  [SD.string-dict: {"hello"; 141}, {"hello"; 41}, {"hi";53}, {"fqf"; 51}, {"qdq"; 5}] raises "duplicate key hello" 
  [SD.string-dict: {"hey"; 141}, {"hello"; 41}, {"hello";53}, {"fqf"; 51}, {"qdq"; 5}] raises "duplicate key hello" 
  [SD.string-dict: {"hello"; 141}, {"hellooo"; 41}, {"hi";53}, {"hi"; 51}, {"qdq"; 5}] raises "duplicate key hi"
  [SD.string-dict: {"hello"; 141}, {"helloooo"; 41}, {"hi";53}, {"fqf"; 51}, {"fqf"; 5}] raises "duplicate key fqf"
  [SD.string-dict: {"hello"; 141}, {"helloooo"; 41}, {"hello";53}, {"fqfqeg"; 51}, {"fqf"; 5}] raises "duplicate key hello" 
  [SD.string-dict: {"hello"; 141}, {"helloooo"; 41}, {"higweg";53}, {"hello"; 51}, {"fqf"; 5}] raises "duplicate key hello" 
  [SD.string-dict: {"hello"; 141}, {"helloooo"; 41}, {"hi";53}, {"fqf"; 51}, {"hello"; 5}] raises "duplicate key hello"
  [SD.string-dict: {"hello"; 141}, {"hi there"; 41}, {"hi";53}, {"hi there"; 51}, {"fqf"; 5}] raises "duplicate key hi there" 
  [SD.string-dict: {"hello"; 141}, {"hi there"; 41}, {"hi";53}, {"fqf"; 51}, {"hi there"; 5}] raises "duplicate key hi there" 
  [SD.string-dict: {"hello"; 141}, {"helloooo"; 41}, {"hi";53}, {"hi"; 51}, {"fqf"; 5}] raises "duplicate key hi" 
  [SD.string-dict: {"hello"; 141}, {"helloooo"; 41}, {"hi";53}, {"fqf"; 51}, {"hi"; 5}] raises "duplicate key hi" 
  [SD.string-dict: {"hello"; 141}, {"hrwfq"; 41}, {"hi";53}, {"fqf"; 51}, {"qdq"; 5}, {"hello"; 23}] raises "duplicate key hello"

  
  [SD.mutable-string-dict: {"a"; 4}, {"a"; 3}] raises "duplicate key a"
  [SD.mutable-string-dict: {"goo"; 4}, {"goo"; 3}, {"qwf"; 51}] raises "duplicate key goo"
  [SD.mutable-string-dict: {"hello"; 41}, {"hi";53}, {"hi"; 51}] raises "duplicate key hi"
  [SD.mutable-string-dict: {"hello"; 41}, {"hi";53}, {"hello"; 51}] raises "duplicate key hello"
  [SD.mutable-string-dict: {"hello"; 141}, {"hello"; 41}, {"hi";53}, {"fqf"; 51}] raises "duplicate key hello"
  [SD.mutable-string-dict: {"hi"; 141}, {"hello"; 41}, {"hello";53}, {"fqf"; 51}] raises "duplicate key hello" 
  [SD.mutable-string-dict: {"hello"; 141}, {"hi"; 41}, {"Sarah";53}, {"Sarah"; 51}] raises "duplicate key Sarah" 
  [SD.mutable-string-dict: {"Sarah"; 141}, {"hello"; 41}, {"Sarah";53}, {"Sarah2"; 51}] raises "duplicate key Sarah"
  [SD.mutable-string-dict: {"hello"; 141}, {"hi"; 41}, {"Sarah";53}, {"hello"; 51}] raises "duplicate key hello"
  [SD.mutable-string-dict: {"hello"; 141}, {"hi"; 41}, {"Sarah";53}, {"hi"; 51}] raises "duplicate key hi"
  [SD.mutable-string-dict: {"hello"; 141}, {"hello"; 41}, {"hi";53}, {"fqf"; 51}, {"qdq"; 5}] raises "duplicate key hello" 
  [SD.mutable-string-dict: {"hey"; 141}, {"hello"; 41}, {"hello";53}, {"fqf"; 51}, {"qdq"; 5}] raises "duplicate key hello" 
  [SD.mutable-string-dict: {"hello"; 141}, {"hellooo"; 41}, {"hi";53}, {"hi"; 51}, {"qdq"; 5}] raises "duplicate key hi"
  [SD.mutable-string-dict: {"hello"; 141}, {"helloooo"; 41}, {"hi";53}, {"fqf"; 51}, {"fqf"; 5}] raises "duplicate key fqf"
  [SD.mutable-string-dict: {"hello"; 141}, {"helloooo"; 41}, {"hello";53}, {"fqfqeg"; 51}, {"fqf"; 5}] raises "duplicate key hello" 
  [SD.mutable-string-dict: {"hello"; 141}, {"helloooo"; 41}, {"higweg";53}, {"hello"; 51}, {"fqf"; 5}] raises "duplicate key hello" 
  [SD.mutable-string-dict: {"hello"; 141}, {"helloooo"; 41}, {"hi";53}, {"fqf"; 51}, {"hello"; 5}] raises "duplicate key hello"
  [SD.mutable-string-dict: {"hello"; 141}, {"hi there"; 41}, {"hi";53}, {"hi there"; 51}, {"fqf"; 5}] raises "duplicate key hi there" 
  [SD.mutable-string-dict: {"hello"; 141}, {"hi there"; 41}, {"hi";53}, {"fqf"; 51}, {"hi there"; 5}] raises "duplicate key hi there" 
  [SD.mutable-string-dict: {"hello"; 141}, {"helloooo"; 41}, {"hi";53}, {"hi"; 51}, {"fqf"; 5}] raises "duplicate key hi" 
  [SD.mutable-string-dict: {"hello"; 141}, {"helloooo"; 41}, {"hi";53}, {"fqf"; 51}, {"hi"; 5}] raises "duplicate key hi" 
  [SD.mutable-string-dict: {"hello"; 141}, {"hrwfq"; 41}, {"hi";53}, {"fqf"; 51}, {"qdq"; 5}, {"hello"; 23}] raises "duplicate key hello"
end
