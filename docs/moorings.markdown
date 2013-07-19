    provide {list: list,
    builtins: builtins,
    error: error,
    checkers: checkers,
    option: option}
    
    
    fun mklist(obj):
      'creates a List from something with `first` and `rest` fields, recursively'
    end
    
    
    fun keys(obj):
      'returns a List of the keys of an object, as strings'
    end
    
    
    fun has-field(obj, name):
      'returns true if the object has a field with the name specified'
    end
    
    
    fun num-keys(obj):
      'returns the Number of fields in an object'
    end
    
    
    fun equiv(obj1, obj2):
      'Check if two objects are equal via an _equals method, or
            have all the same keys with equiv fields'
    end
    
    
    builtins = {keys: keys,
    has-field: has-field,
    mklist: mklist,
    equiv: equiv}
    data List:
      empty:
        length(self): '' end
        each(self, f): '' end
        map(self, f): '' end
        filter(self, f): '' end
        foldr(self, f, base): '' end
        foldl(self, f, base): '' end
        member(self, elt): '' end
        append(self, other): '' end
        last(self): '' end
        take(self, n): '' end
        drop(self, n): '' end
        reverse(self): '' end
        get(self, n): '' end
        set(self, n, e): '' end
        _equals(self, other): '' end
        tostring(self): '' end
        sort-by(self, cmp, eq): '' end
        sort(self): '' end
        join-str(self, str): '' end
      link(first, rest :: List):
        length(self): '' end
        each(self, f): '' end
        map(self, f): '' end
        filter(self, f): '' end
        member(self, elt): '' end
        foldr(self, f, base): '' end
        foldl(self, f, base): '' end
        append(self, other): '' end
        last(self): '' end
        reverse(self): '' end
        take(self, n): '' end
        drop(self, n): '' end
        get(self, n): '' end
        set(self, n, e): '' end
        _equals(self, other): '' end
        tostring(self): '' end
        sort-by(self, cmp, eq): '' end
        sort(self): '' end
        join-str(self, str): '' end
    end
    
    
    fun range(start, stop):
      'creates a list of numbers, starting with start, ending with stop-1'
    end
    
    
    fun repeat(n :: Number, e :: Any):
      'creates a list with n copies of e'
    end
    
    
    fun filter(f, lst :: List):
      'returns the subset of lst for which f(elem) is true'
    end
    
    
    fun map(f, lst :: List):
      'returns a list made up of f(elem) for each elem in lst'
    end
    
    
    fun map2(f, l1 :: List, l2 :: List):
      'returns a list made up of f(elem1, elem2) for each elem1 in l1, elem2 in l2'
    end
    
    
    fun map3(f, l1 :: List, l2 :: List, l3 :: List):
      'returns a list made up of f(e1, e2, e3) for each e1 in l1, e2 in l2, e3 in l3'
    end
    
    
    fun map4(f, l1 :: List, l2 :: List, l3 :: List, l4 :: List):
      'returns a list made up of f(e1, e2, e3, e4) for each e1 in l1, e2 in l2, e3 in l3, e4 in l4'
    end
    
    
    fun map_n(f, n :: Number, lst :: List):
      'returns a list made up of f(n, e1), f(n+1, e2) .. for e1, e2 ... in lst'
    end
    
    
    fun map2_n(f, n :: Number, l1 :: List, l2 :: List):
      ''
    end
    
    
    fun map3_n(f, n :: Number, l1 :: List, l2 :: List, l3 :: List):
      ''
    end
    
    
    fun map4_n(f, n :: Number, l1 :: List, l2 :: List, l3 :: List, l4 :: List):
      ''
    end
    
    
    fun each(f, lst :: List):
      'Calls f for each elem in lst, and returns nothing'
    end
    
    
    fun each2(f, l1 :: List, l2 :: List):
      'Calls f on each pair of corresponding elements in l1 and l2, and returns nothing.  Stops after the shortest list'
    end
    
    
    fun each3(f, l1 :: List, l2 :: List, l3 :: List):
      'Calls f on each triple of corresponding elements in l1, l2 and l3, and returns nothing.  Stops after the shortest list'
    end
    
    
    fun each4(f, l1 :: List, l2 :: List, l3 :: List, l4 :: List):
      'Calls f on each tuple of corresponding elements in l1, l2, l3 and l4, and returns nothing.  Stops after the shortest list'
    end
    
    
    fun each_n(f, n :: Number, lst :: List):
      ''
    end
    
    
    fun each2_n(f, n :: Number, l1 :: List, l2 :: List):
      ''
    end
    
    
    fun each3_n(f, n :: Number, l1 :: List, l2 :: List, l3 :: List):
      ''
    end
    
    
    fun each4_n(f, n :: Number, l1 :: List, l2 :: List, l3 :: List, l4 :: List):
      ''
    end
    
    
    fun fold(f, base, lst :: List):
      ''
    end
    
    
    fun fold2(f, base, l1 :: List, l2 :: List):
      ''
    end
    
    
    fun fold3(f, base, l1 :: List, l2 :: List, l3 :: List):
      ''
    end
    
    
    fun fold4(f, base, l1 :: List, l2 :: List, l3 :: List, l4 :: List):
      ''
    end
    
    
    list = {List: List,
    is-empty: is-empty,
    is-link: is-link,
    empty: empty,
    link: link,
    range: range,
    repeat: repeat,
    filter: filter,
    map: map,
    map2: map2,
    map3: map3,
    map4: map4,
    map_n: map_n,
    map2_n: map2_n,
    map3_n: map3_n,
    map4_n: map4_n,
    each: each,
    each2: each2,
    each3: each3,
    each4: each4,
    each_n: each_n,
    each2_n: each2_n,
    each3_n: each3_n,
    each4_n: each4_n,
    fold: fold,
    fold2: fold2,
    fold3: fold3,
    fold4: fold4}
    data Location:
      location(file :: String, line, column):
        _equals(self, other): '' end
        format(self): '' end
    end
    
    
    data Error:
      opaque-error(message :: String, location :: Location):
        name(self): '' end
      field-not-found(message :: String, location :: Location):
        name(self): '' end
      field-non-string(message :: String, location :: Location):
        name(self): '' end
      cases-miss(message :: String, location :: Location):
        name(self): '' end
      invalid-case(message :: String, location :: Location):
        name(self): '' end
      lazy-error(message :: String, location :: Location):
        name(self): '' end
    end
    
    
    fun make-error(obj):
      ''
    end
    
    
    error = {opaque-error: opaque-error,
    is-opaque-error: is-opaque-error,
    field-not-found: field-not-found,
    is-field-not-found: is-field-not-found,
    cases-miss: cases-miss,
    is-cases-miss: is-cases-miss,
    invalid-case: invalid-case,
    is-invalid-case: is-invalid-case,
    make-error: make-error,
    Error: Error,
    Location: Location,
    location: location,
    is-location: is-location}
    data Option:
      none:
        orelse(self, v): '' end
        andthen(self, f): '' end
      some(value):
        orelse(self, v): '' end
        andthen(self, f): '' end
    end
    
    
    option = {Option: Option,
    is-none: is-none,
    is-some: is-some,
    none: none,
    some: some}
    data Result:
      success(name :: String)
      failure(name :: String, reason :: String)
      err(name :: String, exception :: Any)
    end
    
    
    fun check-true(name, val):
      ''
    end
    
    
    fun check-false(name, val):
      ''
    end
    
    
    fun check-equals(name, val1, val2):
      ''
    end
    
    
    fun check-pred(name, val1, pred):
      ''
    end
    
    
    data CheckResult:
      normal-result(name :: String, location :: Location, results :: List)
      error-result(name :: String, location :: Location, results :: List, err :: Any)
    end
    
    
    fun run-checks(checks):
      ''
    end
    
    
    fun clear-results():
      ''
    end
    
    
    fun get-results():
      ''
    end
    
    
    fun format-check-results(results):
      ''
    end
    
    
    checkers = {check-true: check-true,
    check-false: check-false,
    check-equals: check-equals,
    check-pred: check-pred,
    run-checks: run-checks,
    format-check-results: format-check-results,
    clear-results: clear-results,
    get-results: get-results}
