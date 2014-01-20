define(["./js-numbers/src/js-numbers"], function (jsnums) {

    function getBaseBooleanDict(rt) {
        //Grab the stuff we need from the runtime
        var makeMethod = rt['makeMethod'];
        var makeBoolean = rt['makeBoolean'];
        var makeString = rt['makeString'];

        var checkIf = rt['checkIf'];
        var isBoolean = rt['isBoolean'];
        var isFunction = rt['isFunction'];
        var makeMessageException = rt['makeMessageException'];

        //Make the dictionary
        return {
            '_and' : method(me, other) {
                checkIf(me, isBoolean);
                checkIf(other, isFunction);
                checkIf(other, function(f){return f.arity === 0;});

                if(!me.b) {
                    //Short circuit
                    return makeBoolean(false);
                }
                
                
                //Evaluate the thunk to get the result
                var result = other.app();
                if(isBoolean(result)) {
                    return makeBoolean(result.b);
                }
                else {
                    throw makeMessageException("RHS of 'and' was not a boolean");
                }
            },

            '_or' : method(me, other) {
                checkIf(me, isBoolean);
                checkIf(other, isFunction);
                checkIf(other, function(f){return f.arity === 0;});

                if(me.b) {
                    //Short circuit
                    return makeBoolean(true);
                }
                
                //Evaluate the thunk to get the result
                var result = other.app();
                if(isBoolean(result)) {
                    return makeBoolean(result.b);
                }
                else {
                    throw makeMessageException("RHS of 'and' was not a boolean");
                }
            },

            '_not' : method(me) {
                checkIf(me, isBoolean);
                return makeBoolean(!me.b);
            },

            'tostring' : method(me){
                checkIf(me, isBoolean);
                return makeString(String(me.b));
            }
        }
    }
    
    return { getBaseBooleanDict: getBaseBooleanDict};

});
