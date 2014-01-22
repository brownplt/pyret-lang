/************ Begin Macro Definitions ************************/
//type annotations for functions

//  let function = macro {
//      case {_ (){ $body ...}} => {

//         return #{
//             /**

//             */
//             function(){
//                  $body ...
//              }
//          }
//      }

//      case {_ ($params (,) ...){ $body ...}} => {
//          var start = [makeValue(42, #{$body...}[0])];
//          var stop = [makeValue('*/', #{$body...}[0])];

//          start[0].token.leadingcomments = {'type': "Line", value : "HELLO"}

//          letstx $start = start;
//          letstx $stop = stop;


//  //        console.log(start);
//         return #{ 
//             /**$start

//             $stop
//             */
//             function($params (,) ...) {
//                  //Herp
//                 $start
//                 $body ...
//              }
//          }
//      }

//      case {_ $name($params (,) ...){ $body ...}} => {
//         return #{ 
//             function $name($params (,) ...) { //                  //Derp //                  $body ...
//              }
//          }

//      }

//  }

//  export function;


//Methods
macro method{
    //Only takes in self
    case {$m_name ($me) { $body ...}  } => {

        var mkMeth = makeIdent('makeMethod', #{$m_name}); //get the identifier makeMethod in same scope as method
        letstx $mkMeth = [mkMeth];

        
        var newBody = #{ 
            {
                $body ...
            } 
        };

        letstx $new_body = newBody;

        return #{
          $mkMeth(
          (function ($me) { 
            return (function() 
                $new_body
            )}),
          (function ($me) 
                $new_body
          ))
        }
      }

    //Takes in addtl params
    case {$m_name ($me, $param) { $body ...}  } => {

        var mkMeth = makeIdent('makeMethod', #{$m_name}); //get the identifier makeMethod in same scope as method
        letstx $mkMeth = [mkMeth];

        
        var newParam = makeIdent(unwrapSyntax(#{$param}), #{$body...}[0]);
        var newMe = makeIdent(unwrapSyntax(#{$me}), #{$body...}[0]);

        letstx $new_param = [newParam];
        letstx $new_me = [newMe];
        
        var newBody = #{ 
            {
                $body ...
            } 
        };

        letstx $new_body = newBody;

        return #{
          $mkMeth(
          (function ($me) {
            return (function($new_param) 
                $new_body
            )
          }),
          (function ($me, $new_param) 
                $new_body
          ))
        }
      }

    case {$m_name ($me , $params (,) ...) { $body ...} } => {

        var mkMeth = makeIdent('makeMethod', #{$m_name}); //get the identifier makeMethod in same scope as method
        letstx $mkMeth = [mkMeth];

        var newBody = #{ 
            {
                $body ...
            } 
        };
        letstx $new_body = newBody;
        
        

        return #{
          $mkMeth(
          (function ($me, $params (,) ...) 
            $new_body 
          ),
          (function ($me) {
            return function($params (,) ...) {
            $new_body 
             }
          }))
        }
      }

    } 
export method;

/************* End Macro Definitions *************************/
