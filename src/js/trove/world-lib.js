
define(["js/runtime-util"], function(util) {

  return util.defineJSModule("world-lib",
    [],
    [],
    {},
    function(runtime, namespace) {
      var rawJsworld = {};

      // Stuff here is copy-and-pasted from Chris King's JSWorld.
      //
      // dyoo: as I remember, most of this code had been revised from
      // Chris's original code by Ethan Cechetti, who rewrote it to
      // continuation passing style during summer 2010.

      'use strict';

      /* Type signature notation
       * CPS(a b ... -> c) is used to denote
       *    a b ... (c -> void) -> void
       */

      var Jsworld = rawJsworld;

      var currentFocusedNode = false;

      var doNothing = function() {};
      // Just in case external users need this and doNothing might change.
      Jsworld.doNothing = doNothing;

      // forEachK: CPS( array CPS(array -> void) (error -> void) -> void )
      // Iterates through an array and applies f to each element using CPS
      // If an error is thrown, it catches the error and calls f_error on it
      var forEachK = function(a, f, f_error, k) {
          var forEachHelp = function(i) {
              if( i >= a.length ) {
                  if (k) {
                      return k();
                  } else {
                      return;
                  }
              }
              try {
                  return f(a[i], function() { return forEachHelp(i+1); });
              } catch (e) {
                  return Jsworld.shutdown({errorShutdown: e});
              }
          };
          return forEachHelp(0);
      };







      //
      // WORLD STUFFS
      //

      function InitialWorld() {}

      var world = new InitialWorld();
      var worldListeners = [];
      var eventDetachers = [];
      var runningBigBangs = [];

      var changingWorld = false;



      function clear_running_state() {
          var i;
          world = new InitialWorld();
          worldListeners = [];

          for (i = 0; i < eventDetachers.length; i++) {
                  eventDetachers[i]();
          }
          eventDetachers = [];
          changingWorld = false;
      }



      // Close all world computations.
      Jsworld.shutdown = function(options) {
          while(runningBigBangs.length > 0) {
              var currentRecord = runningBigBangs.pop();
              if (currentRecord) { currentRecord.pause(); }
              if (options.cleanShutdown) {
                  currentRecord.success(world);
              }
              if (options.errorShutdown) {
                  currentRecord.fail(options.errorShutdown);
              }
          }
          clear_running_state();
      };



      function add_world_listener(listener) {
          worldListeners.push(listener);
      }


      function remove_world_listener(listener) {
          var i, index = -1;
          for (i = 0; i < worldListeners.length; i++) {
              if (worldListeners[i] === listener) {
                  index = i;
                  break;
              }
          }
          if (index !== -1) {
              worldListeners.splice(index, 1);
          }
      }


      // If we're in the middle of a change_world, delay.
      var DELAY_BEFORE_RETRY = 10;


      // change_world: CPS( CPS(world -> world) -> void )
      // Adjust the world, and notify all listeners.
      var change_world = function(updater, k) {

          // Check to see if we're in the middle of changing
          // the world already.  If so, put on the queue
          // and exit quickly.
          if (changingWorld) {
              setTimeout(
                  function() {
                      change_world(updater, k);
                  },
                  DELAY_BEFORE_RETRY);
              return;
          }


          changingWorld = true;
          var originalWorld = world;

          var changeWorldHelp;
          changeWorldHelp = function() {
              forEachK(worldListeners,
                       function(listener, k2) {
                           listener(world, originalWorld, k2);
                       },
                       function(e) {
                           changingWorld = false;
                           world = originalWorld;
                           throw e; 
                       },
                       function() {
                           changingWorld = false;
                           k();
                       });
          };

          try {
              updater(world, function(newWorld) {
                  world = newWorld;
                  changeWorldHelp();
              });
          } catch(e) {
              changingWorld = false;
              world = originalWorld;
              return Jsworld.shutdown({errorShutdown: e});
          }
      };
      Jsworld.change_world = change_world;



      var map = function(a1, f) {
          var b = new Array(a1.length), i;
          for (i = 0; i < a1.length; i++) {
                  b[i] = f(a1[i]);
          }
          return b;
      };

      var concat_map = function(a, f) {
          var b = [], i;
          for (i = 0; i < a.length; i++) {
                  b = b.concat(f(a[i]));
          }
          return b;
      };


      function member(a, x) {
          var i;
          for (i = 0; i < a.length; i++) {
              if (a[i] === x) {
                  return true;
              }
          }
          return false;
      }


      //
      // DOM UPDATING STUFFS
      //

      // tree(N): { node: N, children: [tree(N)] }
      // relation(N): { relation: 'parent', parent: N, child: N } | { relation: 'neighbor', left: N, right: N }
      // relations(N): [relation(N)]
      // nodes(N): [N]
      // css(N): [css_node(N)]
      // css_node(N): { node: N, attribs: attribs } | { className: string, attribs: attribs }
      // attrib: { attrib: string, values: [string] }
      // attribs: [attrib]

      // treeable(nodes(N), relations(N)) = bool
      /*function treeable(nodes, relations) {
      // for all neighbor relations between x and y
      for (var i = 0; i < relations.length; i++)
      if (relations[i].relation == 'neighbor') {
      var x = relations[i].left, y = relations[i].right;

      // there does not exist a neighbor relation between x and z!=y or z!=x and y
      for (var j = 0; j < relations.length; j++)
      if (relations[j].relation === 'neighbor')
      if (relations[j].left === x && relations[j].right !== y ||
      relations[j].left !== x && relations[j].right === y)
      return false;
      }

      // for all parent relations between x and y
      for (var i = 0; i < relations.length; i++)
      if (relations[i].relation == 'parent') {
      var x = relations[i].parent, y = relations[i].child;

      // there does not exist a parent relation between z!=x and y
      for (var j = 0; j < relations.length; j++)
      if (relations[j].relation == 'parent')
      if (relations[j].parent !== x && relations[j].child === y)
      return false;
      }

      // for all neighbor relations between x and y
      for (var i = 0; i < relations.length; i++)
      if (relations[i].relation == 'neighbor') {
      var x = relations[i].left, y = relations[i].right;

      // all parent relations between z and x or y share the same z
      for (var j = 0; j < relations.length; j++)
      if (relations[j].relation == 'parent')
      for (var k = 0; k < relations.length; k++)
      if (relations[k].relation == 'parent')
      if (relations[j].child === x && relations[k].child === y &&
      relations[j].parent !== relations[k].parent)
      return false;
      }

      return true;
      }*/


      // node_to_tree: dom -> dom-tree
      // Given a native dom node, produces the appropriate tree.
      function node_to_tree(domNode) {
          var result = [domNode], c = domNode.firstChild;
          if (c === undefined) { return result; }
          else {
            for (c = domNode.firstChild; c !== null; c = c.nextSibling) {
                result.push(node_to_tree(c));
            }
            return result;
          }
      }
      Jsworld.node_to_tree = node_to_tree;



      // nodes(tree(N)) = nodes(N)
      function nodes(tree) {
          var ret, i;
          if (tree.node.jsworldOpaque === true) {
              return [tree.node];
          }

          ret = [tree.node];
          for (i = 0; i < tree.children.length; i++) {
              ret = ret.concat(nodes(tree.children[i]));
          }
          return ret;
      }


      // relations(tree(N)) = relations(N)
      function relations(tree) {
          var ret = [];
          var i;
          for (i = 0; i < tree.children.length; i++) {
              ret.push({ relation: 'parent',
                         parent: tree.node,
                         child: tree.children[i].node });
          }

          for (i = 0; i < tree.children.length - 1; i++) {
              ret.push({ relation: 'neighbor',
                         left: tree.children[i].node,
                         right: tree.children[i + 1].node });
          }

          if (! tree.node.jsworldOpaque) {
              for (i = 0; i < tree.children.length; i++) {
                  ret = ret.concat(relations(tree.children[i]));
              }
          }

          return ret;
      }





      // Preorder traversal.
      var preorder = function(node, f) {
          f(node, function() {
              var child = node.firstChild;
              var nextSibling;
              while (child) {
                  nextSibling = child.nextSibling;
                  preorder(child, f);
                  child = nextSibling;
              }
          });
      };


      // nodeEq: node node -> boolean
      // Returns true if the two nodes should be the same.
      var nodeEq = function(node1, node2) {
          return (node1 && node2 && node1 === node2);
      };


      // isMemq: X (arrayof X) -> boolean
      // Produces true if any of the elements of L are nodeEq to x.
      var isMemq = function(x, L) {
          var i;
          for (i = 0 ; i < L.length; i++) {
              if (nodeEq(x, L[i])) {
                  return true;
              }
          }
          return false;
      };



      // If any node cares about the world, send it in.
      function refresh_node_values(nodes) {
          var i;
          for (i = 0; i < nodes.length; i++) {
              if (nodes[i].onWorldChange) {
                  nodes[i].onWorldChange(world);
              }
          }
      }



      // update_dom(nodes(Node), relations(Node)) = void
      function update_dom(toplevelNode, nodes, relations) {
          var i, parent, child;
          // TODO: rewrite this to move stuff all in one go... possible? necessary?

          // move all children to their proper parents
          for (i = 0; i < relations.length; i++) {
              if (relations[i].relation === 'parent') {
                  parent = relations[i].parent;
                  child = relations[i].child;
                  if (child.parentNode !== parent) {
                      parent.appendChild(child);
                  }
              }
          }

          // arrange siblings in proper order
          // truly terrible... BUBBLE SORT
          var unsorted = true;
          while (unsorted) {
              unsorted = false;
              for (i = 0; i < relations.length; i++) {
                  if (relations[i].relation === 'neighbor') {
                      var left = relations[i].left, right = relations[i].right;

                      if (! nodeEq(left.nextSibling, right)) {
                          left.parentNode.insertBefore(left, right);
                          unsorted = true;
                      }
                  }
              }
          }

          // Finally, remove nodes that shouldn't be attached anymore.
          var nodesPlus = nodes.concat([toplevelNode]);
          preorder(toplevelNode, function(aNode, continueTraversalDown) {
              if (aNode.jsworldOpaque) {
                  if (! isMemq(aNode, nodesPlus)) {
                      aNode.parentNode.removeChild(aNode);
                  }
              } else {
                  if (! isMemq(aNode, nodesPlus)) {
                      aNode.parentNode.removeChild(aNode);
                  } else {
                      continueTraversalDown();
                  }
              }
          });

          refresh_node_values(nodes);
      }



      // camelCase: string -> string
      function camelCase(name) {
          return name.replace(/\-(.)/g, function(m, l){return l.toUpperCase();});
      }


      function set_css_attribs(node, attribs) {
          var j;
          for (j = 0; j < attribs.length; j++){
              node.style[camelCase(attribs[j].attrib)] = attribs[j].values.join(" ");
          }
      }


      // isMatchingCssSelector: node css -> boolean
      // Returns true if the CSS selector matches.
      function isMatchingCssSelector(node, css) {
          if (css.id.match(/^\./)) {
              // Check to see if we match the class
              return (node.className && member(node.className.split(/\s+/),
                                               css.id.substring(1)));
          } else {
              return (node.id && node.id === css.id);
          }
      }


      var clearCss = function(node) {
          // FIXME: we should not be clearing the css
    //      if ('style' in node)
    //          node.style.cssText = "";
      };



      function update_css(nodes, css) {
          // clear CSS
          var i, j;
          for (i = 0; i < nodes.length; i++) {
              if ( !nodes[i].jsworldOpaque ) {
                      clearCss(nodes[i]);
              }
          }

          // set CSS
          for (i = 0; i < css.length; i++) {
              if (css[i].id) {
                  for (j = 0; j < nodes.length; j++) {
                      if (isMatchingCssSelector(nodes[j], css[i])) {
                          set_css_attribs(nodes[j], css[i].attribs);
                      }
                  }
              } else {
                  set_css_attribs(css[i].node, css[i].attribs);
              }
          }
      }



      var sexp2tree;
      var sexp2css;
      var maintainingSelection;



      function do_redraw(world, oldWorld, toplevelNode, redraw_func, redraw_css_func, k) {
          if (oldWorld instanceof InitialWorld) {
              // Simple path
              redraw_func(world,
                  function(drawn) {
                          var t = sexp2tree(drawn);
                          var ns = nodes(t);
                          // HACK: css before dom, due to excanvas hack.
                          redraw_css_func(world,
                                  function(css) {
                                          update_css(ns, sexp2css(css));
                                          update_dom(toplevelNode, ns, relations(t));
                                          k();
                                  });
                  });
          } else {
              maintainingSelection(
                  function(k2) {
                      redraw_func(
                          world,
                          function(newRedraw) {

                              redraw_css_func(
                                  world,
                                  function(newRedrawCss) {
                                      var t = sexp2tree(newRedraw);
                                      var ns = nodes(t);
                                      // Try to save the current selection and preserve it across
                                      // dom updates.

                                      // Kludge: update the CSS styles first.
                                      // This is a workaround an issue with excanvas: any style change
                                      // clears the content of the canvas, so we do this first before
                                      // attaching the dom element.
                                      update_css(ns, sexp2css(newRedrawCss));
                                      update_dom(toplevelNode, ns, relations(t));

                                      k2();
                                  });
                          });
                  }, k);
          }
      }



      var FocusedSelection;

      function hasCurrentFocusedSelection() {
          return currentFocusedNode !== undefined;
      }

      function getCurrentFocusedSelection() {
          return new FocusedSelection();
      }


      // maintainingSelection: (-> void) -> void
      // Calls the thunk f while trying to maintain the current focused selection.
      maintainingSelection = function(f, k) {
          var currentFocusedSelection;
          if (hasCurrentFocusedSelection()) {
              currentFocusedSelection = getCurrentFocusedSelection();
              f(function() {
                  currentFocusedSelection.restore();
                  k();
              });
          } else {
              f(function() { k(); });
          }
      };



      FocusedSelection = function() {
          this.focused = currentFocusedNode;
          this.selectionStart = currentFocusedNode.selectionStart;
          this.selectionEnd = currentFocusedNode.selectionEnd;
      };

      // Try to restore the focus.
      FocusedSelection.prototype.restore = function() {
          // FIXME: if we're scrolling through, what's visible
          // isn't restored yet.
          if (this.focused.parentNode) {
              this.focused.selectionStart = this.selectionStart;
              this.focused.selectionEnd = this.selectionEnd;
              this.focused.focus();
          } else if (this.focused.id) {
              var matching = document.getElementById(this.focused.id);
              if (matching) {
                  matching.selectionStart = this.selectionStart;
                  matching.selectionEnd = this.selectionEnd;
                  matching.focus();
              }
          }
      };





      //////////////////////////////////////////////////////////////////////

      var bigBang, StopWhenHandler;

      function BigBangRecord(top, world, handlerCreators, handlers, attribs,
                             success, fail) {
          this.top = top;
          this.world = world;
          this.handlers = handlers;
          this.handlerCreators = handlerCreators;
          this.attribs = attribs;
          this.success = success;
          this.fail = fail;
      }

      BigBangRecord.prototype.restart = function() {
          bigBang(this.top, this.world, this.handlerCreators, this.attribs);
      };

      BigBangRecord.prototype.pause = function() {
          var i;
          for(i = 0 ; i < this.handlers.length; i++) {
              if (! (this.handlers[i] instanceof StopWhenHandler)) {
                  this.handlers[i].onUnregister(this.top);
              }
          }
      };
      //////////////////////////////////////////////////////////////////////


      var copy_attribs;


      // Notes: bigBang maintains a stack of activation records; it should be possible
      // to call bigBang re-entrantly.
      // top: dom
      // init_world: any
      // handlerCreators: (Arrayof (-> handler))
      // k: any -> void
      bigBang = function(top, init_world, handlerCreators, attribs, succ, fail) {
          var i;
          // clear_running_state();

          // Construct a fresh set of the handlers.
          var handlers = map(handlerCreators, function(x) { return x();} );
          if (runningBigBangs.length > 0) {
              runningBigBangs[runningBigBangs.length - 1].pause();
          }

          // Create an activation record for this big-bang.
          var activationRecord =
              new BigBangRecord(top, init_world, handlerCreators, handlers, attribs, 
                                succ, fail);
          runningBigBangs.push(activationRecord);
          function keepRecordUpToDate(w, oldW, k2) {
              activationRecord.world = w;
              k2();
          }
          add_world_listener(keepRecordUpToDate);



          // Monitor for termination and register the other handlers.
          var stopWhen = new StopWhenHandler(function(w, k2) { k2(false); },
                                             function(w, k2) { k2(w); });
          for(i = 0 ; i < handlers.length; i++) {
              if (handlers[i] instanceof StopWhenHandler) {
                  stopWhen = handlers[i];
              } else {
                  handlers[i].onRegister(top);
              }
          }
          var watchForTermination = function(w, oldW, k2) {
              stopWhen.test(w,
                            function(stop) {
                                if (stop) {
                                    Jsworld.shutdown({cleanShutdown: true});
                                }
                                else { k2(); }
                            });
          };
          add_world_listener(watchForTermination);


          // Finally, begin the big-bang.
          copy_attribs(top, attribs);
          change_world(function(w, k2) { k2(init_world); }, doNothing);
      };
      Jsworld.bigBang = bigBang;





      // on_tick: number CPS(world -> world) -> handler
      var on_tick = function(delay, tick) {
          return function() {
              var scheduleTick, ticker;
              scheduleTick = function(t) {
                  ticker.watchId = setTimeout(
                      function() {
                          ticker.watchId = undefined;
                          var startTime = (new Date()).valueOf();
                          change_world(tick,
                                       function() {
                                           var endTime = (new Date()).valueOf();
                                           scheduleTick(Math.max(delay - (endTime - startTime),
                                                                 0));
                                       });
                      },
                      t);
              };

              ticker = {
                  watchId: -1,
                  onRegister: function (top) {
                      scheduleTick(delay);
                  },

                  onUnregister: function (top) {
                      if (ticker.watchId) {
                          clearTimeout(ticker.watchId);
                      }
                  }
              };
              return ticker;
          };
      };
      Jsworld.on_tick = on_tick;

      var preventDefault, stopPropagation;
      var attachEvent, detachEvent;


      function on_key(press) {
          return function() {
              var wrappedPress = function(e) {
                  if(e.keyCode === 27) { return; } // Escape events are not for world; the environment handles them
                  stopPropagation(e);
                  preventDefault(e);
                  change_world(function(w, k) { press(w, e, k); }, doNothing);
              };
              return {
                  onRegister: function(top) {
                      //http://www.w3.org/TR/html5/editing.html#sequential-focus-navigation-and-the-tabindex-attribue
                      jQuery(top).attr('tabindex', 1);
                      jQuery(top).focus();
                      attachEvent(top, 'keydown', wrappedPress);
                  },
                  onUnregister: function(top) {
                      detachEvent(top, 'keydown', wrappedPress);
                  }
              };
          };
      }
      Jsworld.on_key = on_key;




      // http://www.quirksmode.org/js/events_mouse.html
      // http://stackoverflow.com/questions/55677/how-do-i-get-the-coordinates-of-a-mouse-click-on-a-canvas-element
      function on_mouse(mouse) {
          return function() {
              var isButtonDown = false;
              var makeWrapped = function(type) {
                  return function(e) {
                      preventDefault(e);
                      stopPropagation(e);
                      var x = e.pageX, y = e.pageY;
                      var currentElement = e.target;
                      do {
                          x -= currentElement.offsetLeft;
                          y -= currentElement.offsetTop;
                          currentElement = currentElement.offsetParent;
                      } while(currentElement);

                      if (type === 'button-down') {
                          isButtonDown = true;
                      } else if (type === 'button-up') {
                          isButtonDown = false;
                      }
                      if (type === 'move' && isButtonDown) {
                          change_world(function(w, k) {
                              mouse(w, x, y, 'drag', k);
                          }, doNothing);
                      } else {
                          change_world(function(w, k) {
                              mouse(w, x, y, type, k);
                          }, doNothing);
                      }
                  };
              };
              var wrappedDown = makeWrapped('button-down');
              var wrappedUp = makeWrapped('button-up');
              // How do we do drag?
              var wrappedMove = makeWrapped('move');
              var wrappedEnter = makeWrapped('enter');
              var wrappedLeave = makeWrapped('leave');
              return {
                  onRegister: function(top) {
                      attachEvent(top, 'mousedown', wrappedDown);
                      attachEvent(top, 'mouseup', wrappedUp);
                      attachEvent(top, 'mousemove', wrappedMove);
                      attachEvent(top, 'mouseenter', wrappedEnter);
                      attachEvent(top, 'mouseleave', wrappedLeave);
                  },
                  onUnregister: function(top) {
                      detachEvent(top, 'mousedown', wrappedDown);
                      detachEvent(top, 'mouseup', wrappedUp);
                      detachEvent(top, 'mousemove', wrappedMove);
                      detachEvent(top, 'mouseenter', wrappedEnter);
                      detachEvent(top, 'mouseleave', wrappedLeave);
                  }
              };
          };
      }
      Jsworld.on_mouse = on_mouse;




      var checkDomSexp;


      //  on_draw: CPS(world -> (sexpof node)) CPS(world -> (sexpof css-style)) -> handler
      function on_draw(redraw, redraw_css) {
          var wrappedRedraw = function(w, k) {
              redraw(w, function(newDomTree) {
                  checkDomSexp(newDomTree, newDomTree);
                  k(newDomTree);
              });
          };

          return function() {
              var drawer = {
                  _top: null,
                  _listener: function(w, oldW, k2) {
                      do_redraw(w, oldW, drawer._top, wrappedRedraw, redraw_css, k2);
                  },
                  onRegister: function (top) {
                      drawer._top = top;
                      add_world_listener(drawer._listener);
                  },

                  onUnregister: function (top) {
                      remove_world_listener(drawer._listener);
                  }
              };
              return drawer;
          };
      }
      Jsworld.on_draw = on_draw;



      StopWhenHandler = function(test, receiver) {
          this.test = test;
          this.receiver = receiver;
      };
      // stop_when: CPS(world -> boolean) CPS(world -> boolean) -> handler
      function stop_when(test, receiver) {
          return function() {
              if (receiver === undefined) {
                  receiver = function(w, k) { k(w); };
              }
              return new StopWhenHandler(test, receiver);
          };
      }
      Jsworld.stop_when = stop_when;



      function on_world_change(f) {
          var listener = function(world, oldW, k) { f(world, k); };
          return function() {
              return {
                  onRegister: function (top) {
                      add_world_listener(listener); },
                  onUnregister: function (top) {
                      remove_world_listener(listener); }
              };
          };
      }
      Jsworld.on_world_change = on_world_change;





      // Compatibility for attaching events to nodes.
      attachEvent = function(node, eventName, fn) {
          if (node.addEventListener) {
              // Mozilla
              node.addEventListener(eventName, fn, false);
          } else {
              // IE
              node.attachEvent('on' + eventName, fn, false);
          }
      };

      detachEvent = function(node, eventName, fn) {
          if (node.addEventListener) {
              // Mozilla
              node.removeEventListener(eventName, fn, false);
          } else {
              // IE
              node.detachEvent('on' + eventName, fn, false);
          }
      };

      //
      // DOM CREATION STUFFS
      //

      // add_ev: node string CPS(world event -> world) -> void
      // Attaches a world-updating handler when the world is changed.
      function add_ev(node, event, f) {
          var eventHandler = function(e) { change_world(function(w, k) { f(w, e, k); },
                                                         doNothing); };
          attachEvent(node, event, eventHandler);
          eventDetachers.push(function() { detachEvent(node, event, eventHandler); });
      }

      // add_ev_after: node string CPS(world event -> world) -> void
      // Attaches a world-updating handler when the world is changed, but only
      // after the fired event has finished.
      function add_ev_after(node, event, f) {
          var eventHandler = function(e) {
                  setTimeout(function() { change_world(function(w, k) { f(w, e, k); },
                                                       doNothing); },
                             0);
          };

          attachEvent(node, event, eventHandler);
          eventDetachers.push(function() { detachEvent(node, event, eventHandler); });
      }


      function addFocusTracking(node) {
          attachEvent(node, "focus", function(e) {
              currentFocusedNode = node; });
          attachEvent(node, "blur", function(e) {
              currentFocusedNode = undefined;
          });
          return node;
      }





      //
      // WORLD STUFFS
      //


      sexp2tree = function(sexp) {
          if(sexp.length === undefined) { return { node: sexp, children: [] }; }
          else { return { node: sexp[0], children: map(sexp.slice(1), sexp2tree) }; }
      };

      function sexp2attrib(sexp) {
          return { attrib: sexp[0], values: sexp.slice(1) };
      }

      function sexp2css_node(sexp) {
          var attribs = map(sexp.slice(1), sexp2attrib);
          if (typeof sexp[0] === 'string'){
              return [{ id: sexp[0], attribs: attribs }];
          } else if (sexp[0].length !== undefined){
              return map(sexp[0], function (id) { return { id: id, attribs: attribs }; });
          } else {
              return [{ node: sexp[0], attribs: attribs }];
          }
      }

      sexp2css = function(sexp) {
          return concat_map(sexp, sexp2css_node);
      };



      function isTextNode(n) {
          return (n.nodeType === 3);
      }


      function isElementNode(n) {
          return (n.nodeType === 1);
      }

      var JsworldDomError;

      var throwDomError = function(thing, topThing) {
          throw new JsworldDomError(
              "Expected a non-empty array, received " +  thing + " within " + topThing,
              thing);
      };

      // checkDomSexp: X X -> boolean
      // Checks to see if thing is a DOM-sexp.  If not,
      // throws an object that explains why not.
      checkDomSexp = function(thing, topThing) {
          var i;
          if (! thing instanceof Array) {
              throwDomError(thing, topThing);
          }
          if (thing.length === 0) {
              throwDomError(thing, topThing);
          }


          // Check that the first element is a Text or an element.
          if (isTextNode(thing[0])) {
              if (thing.length > 1) {
                  throw new JsworldDomError("Text node " + thing + " can not have children",
                                            thing);
              }
          } else if (isElementNode(thing[0])) {
              for (i = 1; i < thing.length; i++) {
                  checkDomSexp(thing[i], thing);
              }
          } else {
              if (window.console && window.console.log) { window.console.log(thing[0]); }

              throw new JsworldDomError(
                  "expected a Text or an Element, received " + thing + " within " + topThing,
                  thing[0]);
          }
      };

      JsworldDomError = function(msg, elt) {
          this.msg = msg;
          this.elt = elt;
      };
      JsworldDomError.prototype.toString = function() {
          return "JsworldDomError: " + this.msg;
      };





      //
      // DOM CREATION STUFFS
      //


      copy_attribs = function(node, attribs) {
          var a;
          if (attribs) {
              for (a in attribs) {
                  if (hasOwnProperty.call(attribs, a)) {
                      if (typeof attribs[a] === 'function') {
                          add_ev(node, a, attribs[a]);
                      } else {
                          node[a] = attribs[a];
                      }
                  }
              }
          }
          return node;
      };


      //
      // NODE TYPES
      //

      function p(attribs) {
          return addFocusTracking(copy_attribs(document.createElement('p'), attribs));
      }
      Jsworld.p = p;

      function div(attribs) {
          return addFocusTracking(copy_attribs(document.createElement('div'), attribs));
      }
      Jsworld.div = div;

      // Used To Be: (world event -> world) (hashof X Y) -> domElement
      // Now: CPS(world event -> world) (hashof X Y) -> domElement
      function button(f, attribs) {
          var n = document.createElement('button');
          n.onclick = function(e) {return false;};
          add_ev(n, 'click', f);
          return addFocusTracking(copy_attribs(n, attribs));
      }
      Jsworld.button = button;




      preventDefault = function(event) {
          if (event.preventDefault) {
              event.preventDefault();
          } else {
              event.returnValue = false;
          }
      };

      stopPropagation = function(event) {
          if (event.stopPropagation) {
              event.stopPropagation();
          } else {
              event.cancelBubble = true;
          }
      };


      var stopClickPropagation = function(node) {
          attachEvent(node, "click",
                      function(e) {
                          stopPropagation(e);
                      });
          return node;
      };


      var text_input, checkbox_input;

      // input: string CPS(world -> world) 
      function input(aType, updateF, attribs) {
          aType = aType.toLowerCase();
          var dispatchTable = { text : text_input,
                                password: text_input,
                                checkbox: checkbox_input
                                //button: button_input,
                                //radio: radio_input 
          };

          if (dispatchTable[aType]) {
              return (dispatchTable[aType])(aType, updateF, attribs);
          }
          else {
              throw new Error("js-input: does not currently support type " + aType);
          }
      }
      Jsworld.input = input;




      text_input = function(type, updateF, attribs) {
          var n = document.createElement('input');
          n.type = type;

          var lastVal = n.value;
          var onEvent = function() {
              if (! n.parentNode) { return; }
              setTimeout(
                  function() {
                      if (lastVal !== n.value) {
                          lastVal = n.value;
                          change_world(function (w, k) {
                              updateF(w, n.value, k);
                          }, doNothing);
                      }
                  },
                  0);
          };

          attachEvent(n, "keydown", onEvent);
          eventDetachers.push(function() {
              detachEvent(n, "keydown", onEvent); });

          attachEvent(n, "change", onEvent);
          eventDetachers.push(function() {
              detachEvent(n, "change", onEvent); });

          return stopClickPropagation(
              addFocusTracking(copy_attribs(n, attribs)));
      };


      checkbox_input = function(type, updateF, attribs) {
          var n = document.createElement('input');
          n.type = type;
          var onCheck = function(w, e, k) {
              updateF(w, n.checked, k);
          };
          // This established the widget->world direction
          add_ev_after(n, 'change', onCheck);

          attachEvent(n, 'click', function(e) {
              stopPropagation(e);
          });

          return copy_attribs(n, attribs);
      };


      // var button_input = function(type, updateF, attribs) {
      //     var n = document.createElement('button');
      //     add_ev(n, 'click', function(w, e, k) { updateF(w, n.value, k); });
      //     return addFocusTracking(copy_attribs(n, attribs));
      // };




      function text(s, attribs) {
          var result = document.createElement("div");
          result.appendChild(document.createTextNode(String(s)));
          result.jsworldOpaque = true;
          return result;
      }
      Jsworld.text = text;

      var option;

      function select(attribs, opts, f){
          var n = document.createElement('select'), i;
          for(i = 0; i < opts.length; i++) {
              n.add(option({value: opts[i]}), null);
          }
          n.jsworldOpaque = true;
          add_ev(n, 'change', f);
          var result = addFocusTracking(copy_attribs(n, attribs));
          return result;
      }
      Jsworld.select = select;

      option = function(attribs){
          var node = document.createElement("option");
          node.text = attribs.value;
          node.value = attribs.value;
          return node;
      };



      function textarea(attribs){
          return addFocusTracking(copy_attribs(document.createElement('textarea'), attribs));
      }
      Jsworld.textarea = textarea;

      function h1(attribs){
          return addFocusTracking(copy_attribs(document.createElement('h1'), attribs));
      }
      Jsworld.h1 = h1;

      function canvas(attribs){
          return addFocusTracking(copy_attribs(document.createElement('canvas'), attribs));
      }
      Jsworld.canvas = canvas;


      function img(src, attribs) {
          var n = document.createElement('img');
          n.src = src;
          return addFocusTracking(copy_attribs(n, attribs));
      }
      Jsworld.img = img;



      function raw_node(node, attribs) {
          return addFocusTracking(copy_attribs(node, attribs));
      }
      Jsworld.raw_node = raw_node;

      return Jsworld;
  });
});
