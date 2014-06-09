#lang scribble/base

@(define (version . t)
   (apply section "Version: " t))

@require{lib.rkt}

@title{Tutorial: A Flight Lander Game}

@(table-of-contents)

@section{Introduction}

In this tutorial we're going to write a little interactive game. The
game won't be sophisticated, but it'll have all the elements you need
to build much richer games of your own.

Imagine we have an airplane coming in to land. It's unfortunately
trying to do so amidst a hot-air balloon festival, so it naturally
wants to avoid colliding with any (moving) balloons. In addition,
there is both land and water, and the airplane needs to alight on
land. We might also equip it with limited amounts of fuel to complete
its task. Here are some animations of the game:

@itemlist[

@item{@show-url{http://world.cs.brown.edu/1/projects/flight-lander/v9-success.swf}

The airplane comes in to land succcessfully.}

@item{@show-url{http://world.cs.brown.edu/1/projects/flight-lander/v9-collide.swf}

Uh oh---the airplane collides with a balloon!}

@item{@show-url{http://world.cs.brown.edu/1/projects/flight-lander/v9-sink.swf}

Uh oh---the airplane lands in the water!}

]

By the end, you will have written all the relevant portions of this
program. Your program will: animate the airplane to move autonomously;
detect keystrokes and adjust the airplane accordingly; have multiple
moving balloons; detect collisions between the airplane and balloons;
check for landing on water and land; and account for the use of
fuel. Phew: that's a lot going on! Therefore, we won't write it all at
once; instead, we'll build it up bit-by-bit. But we'll get there by
the end.

@section{About Reactive Animations}

We are writing a program with two important interactive elements: it
is an @kwd{animation}, meaning it gives the impression of motion, and
it is @kwd{reactive}, meaning it responds to user input. Both of these
can be challenging to program, but Pyret provides a simple mechanism
that accommodates both and integrates well with other programming
principles such as testing. We will learn about this as we go along.

The key to creating an animation is the @kwd{Movie Principle}. Even in
the most sophisticated movie you can watch, there is no @emph{motion}
(indeed, the very term ``movie''---short for ``moving picture''---is a
clever bit of false advertising). Rather, there is just a sequence of
still images shown in rapid succession, relying on the human brain to
create the @emph{impression} of motion. We are going to exploit the
same idea: our animations will consist of a sequence of individual
images, and we will ask Pyret to show these in rapid succession. We
will then see how reactivity folds into the same process.

@section{Preliminaries}

To begin with, we should inform Pyret that we plan to make use of both
images and animations. We load the libraries as follows:
@pydisp{
import image as I
import world as W
}
This tells Pyret to load to these two libraries and bind the results
to the corresponding names, @pyin{I} and @pyin{W}. Thus, all image
operations are obtained from @pyin{I} and animation operations from
@pyin{W}.

@version{Airplane Moving Across the Screen}

We will start with the simplest version: one in which the airplane
moves horizontally across the screen. Watch this video:

@show-url{http://world.cs.brown.edu/1/projects/flight-lander/v1.swf}

First, here's an image of an airplane:@margin-note*{Have fun finding
your preferred airplane image! But don't spend too long on it,
because we've still got a lot of work to do.}

@show-url{http://world.cs.brown.edu/1/clipart/airplane-small.png}

We can tell Pyret to load this image and give it a name as follows:
@pydisp{
AIRPLANE-URL = 
  "http://world.cs.brown.edu/1/clipart/airplane-small.png"
AIRPLANE = I.image-url(AIRPLANE-URL)
}
Henceforth, when we refer to @pyin{AIRPLANE}, it will always refer to
this image. (Try it out in the interactions area!)

Now look at the video again. Watch what happens at different points in
time. What stays the same, and what changes? What's common is the
water and land, which stay the same. What changes is the (horizontal)
position of the airplane.

@callout{
  The @kwd{World State} consists of everything that changes. Things
  that stay the same do not need to get recorded in the World
  State.}

We can now define our first World State:

@world-def{
  The World State is a number, representing the @math-lite{x}-position
  of the airplane.
}

Observe something important above.

@callout{
  When we record a World State, we don't capture only the type of the
  values, but also their intended meaning.
}

Now we have a representation of the core data, but to generate the
above animation, we still have to do several things:
@enumerate[

  @item{Ask to be notified of the passage of time.}

  @item{As time passes, correspondingly update the World State.}

  @item{Given an updated World State, produce the corresponding visual
  display.}

]
This sounds like a lot! Fortunately, Pyret makes this much easier than
it sounds. We'll do these in a slightly different order than listed
above.

@subsection{Updating the World State}

As we've noted, the airplane doesn't actually ``move''. Rather, we can
ask Pyret to notify us every time a clock ticks ([REF]). If on each
tick we place the airplane in an appropriately different position, and
the ticks happen often enough, we will get the impression of motion.

Because the World State consists of just the airplane's
@math-lite{x}-position, to move it to the right, we simply increment
its value. Let's first give this constant distance a name:
@pydisp{
AIRPLANE-X-MOVE = 10
}
We will need to write a function that reflects this movement. Let's
first write some test cases:
@pydisp{
check:
  move-airplane-x-on-tick(50) is 50 + AIRPLANE-X-MOVE
  move-airplane-x-on-tick(0) is 0 + AIRPLANE-X-MOVE
  move-airplane-x-on-tick(100) is 100 + AIRPLANE-X-MOVE
end
}
The function's definition is now clear:
@pydisp{
fun move-airplane-x-on-tick(w):
  w + AIRPLANE-X-MOVE
end
}
And sure enough, Pyret will confirm that this function passes all of
its tests.

@callout{
  If you have prior experience programming animations and reactive
  programs, you will immediately notice an important difference: it's
  easy to test parts of your program in Pyret!
}

@subsection{Displaying the World State}

Now we're ready to draw the game's visual output. We produce an image
that consists of all the necessary components. It first helps to
define some constants representing the visual output:
@pydisp{
WIDTH = 800
HEIGHT = 500

BASE-HEIGHT = 50
WATER-WIDTH = 500
}
Using these, we can create a blank canvas, and overlay rectangles
representing water and land:
@pydisp{
BLANK-SCENE = I.empty-scene(WIDTH, HEIGHT)

WATER = I.rectangle(WATER-WIDTH, BASE-HEIGHT, "solid", "blue")
LAND = I.rectangle(WIDTH - WATER-WIDTH, BASE-HEIGHT, "solid", "brown")

BASE = I.beside(WATER, LAND)

BACKGROUND =
  I.place-image(BASE,
    WIDTH / 2, HEIGHT - (BASE-HEIGHT / 2),
    BLANK-SCENE)
}
Examine the value of @pyin{BACKGROUND} in the interactions area
to confirm that it looks right.

@incercise{
  The reason we divide by two when placing @pyin{BASE} is because
  Pyret puts the @emph{middle} of the image at the given
  location. Remove the division and see what happens to the resulting
  image.
}

Now that we know how to get our background, we're ready to place the
airplane on it. The expression to do so looks roughly like this:
@pydisp{
I.place-image(AIRPLANE,
  # some x position,
  50,
  BACKGROUND)
}
but what @math-lite{x} position do we use? Actually, that's just what
the World State represents! So we create a function out of this
expression:
@pydisp{
fun place-airplane-x(w):
  I.place-image(AIRPLANE,
    w,
    50,
    BACKGROUND)
end
}

@subsection{Observing Time (and Combining the Pieces)}

Finally, we're ready to put these pieces together. We invoke a
function called @pyin{big-bang}, which creates
animations. @pyin{big-bang} needs to be given an initial World State
as well as @kwd{handlers} that tell it how to react.  Specifying
@pyin{on-tick} tells Pyret to run a clock and, every time the clock
ticks (roughly thirty times a second), invoke the associated
handler. The @pyin{to-draw} handler is used by Pyret to refresh the
visual display. Thus:
@pydisp{
W.big-bang(0, [list:
    W.on-tick(move-airplane-x-on-tick),
    W.to-draw(place-airplane-x)])
}
creates a running program where the airplane flies across the background!

That's it! We've created our first animation. Now that we've gotten
all the preliminaries out of the way, we can go about enhancing it.

@exercise{
If you want the airplane to appear to move faster, what can you change?
}

@version{Wrapping Around}

When you run the preceding program, you'll notice that after a while,
the airplane just disappears. This is because it has gone past the right
edge of the screen; it is still being ``drawn'', but in a location
that you cannot see. That's not very useful!@margin-note*{Also, after
a long while you might get an error because the computer is being
asked to draw the airplane at a location beyond what the graphics
system can manage.} Instead, when the airplane is about to go past the
right edge of the screen, we'd like it to reappear on the left by a
corresponding amount: ``wrapping around'', as it were.

Here's the video for this version:

@show-url{http://world.cs.brown.edu/1/projects/flight-lander/v2.swf}

Let’s think about what we need to change. Clearly, we need to modify
the function that updates the airplane’s location, since this must now
reﬂect our decision to wrap around. But the task of @emph{how} to draw
the airplane doesn't need to change at all! Similarly, the definition of
the World State does not need to change, either.

Therefore, we only need to modify @pyin{move-airplane-x-on-tick}. The
function @pyin{num-modulo} does exactly what we need. That is, we want
the @math-lite{x}-location to always be modulo the width of the scene:
@pydisp{
fun move-airplane-wrapping-x-on-tick(x):
  num-modulo(x + AIRPLANE-X-MOVE, WIDTH)
end
}
Notice that, instead of copying the content of the previous definition
we can simply reuse it:
@pydisp{
fun move-airplane-wrapping-x-on-tick(x):
  num-modulo(move-airplane-x-on-tick(x), WIDTH)
end
}
which makes our intent clearer: compute whatever position we would
have had before, but adapt the coordinate to remain within the scene's
width.

Well, that's a @emph{proposed} re-definition. Be sure to test this
function thoroughly: it's tricker than you might think! Have you
thought about all the cases? For instance, what happens if the airplane
is half-way off the right edge of the screen?

@callout{
It @emph{is} possible to leave @pyin{move-airplane-x-on-tick} unchanged
and perform the modular arithmetic in @pyin{place-airplane-x}
instead. We choose not to do that for the following reason. In this
version, we really do think of the airplane as circling around and
starting again from the left edge (imagine the world is a
cylinder...). Thus, the airplane's @math-lite{x}-position really does
keep going back down. If instead we allowed the World State to
increase monotonically, then it would really be representing the total
distance traveled, contradicting our definition of the World State.
}

@version{Descending}

Of course, we need our airplane to move in more than just one dimension:
to get to the ﬁnal game, it must both ascend and descend as well. For
now, we’ll focus on the simplest version of this, which is a airplane
that continuously descends. Here’s a video:

@show-url{http://world.cs.brown.edu/1/projects/flight-lander/v3.swf}

Let's again consider individual frames of this video. What's staying
the same? Once again, the water and the land. What's changing? The
position of the airplane. But, whereas before the airplane moved only
in the @math-lite{x}-dimension, now it moves in both @math-lite{x} and
@math-lite{y}. That immediately tells us that our definition of the
World State is inadequate, and must be modified.

We therefore define a new structure to hold this pair of data:
@pydisp{
data Posn:
  | posn(x, y)
end
}
Given this, we can revise our definition:

@world-def{
The World State is a @pyin{posn}, representing the
@math-lite{x}-position and @math-lite{y}-position of the airplane on
the screen.
}

@subsection{Moving the Airplane}

First, let’s consider
@pyin{move-airplane-wrapping-x-on-tick}. Previously our airplane moved
only in the @math-lite{x}-direction; now we want it to descend as
well, which means we must add something to the current @math-lite{y}
value:
@pydisp{
AIRPLANE-Y-MOVE = 3
}
Let’s write some test cases for the new function. Here’s one:
@pydisp{
check:
  move-airplane-xy-on-tick(posn(10, 10)) is posn(20, 13)
end
}
Another way to write the test would be:
@pydisp{
check:
  p = posn(10, 10)
  move-airplane-xy-on-tick(p) is
    posn(move-airplane-wrapping-x-on-tick(p.x),
      move-airplane-y-on-tick(p.y))
end
}

@callout{
Which method of writing tests is better? @emph{Both!} They each offer
different advantages:
@itemlist[

@item{The former method has the benefit of being very concrete:
there's no question what you expect, and it demonstrates that you
really can compute the desired answer from first principles.}

@item{The latter method has the advantage that, if you change the
constants in your program (such as the rate of descent), seemingly
correct tests do not suddenly fail. That is, this form of testing is
more about the @emph{relationships} between things rather than their
precise @emph{values}.}

]
There is one more choice available, which often combines the best of
both worlds: write the answer as concretely as possible (the former
style), but using constants to compute the answer (the advantage
of the latter style). For instance:
@pydisp{
check:
  p = posn(10, 10)
  move-airplane-xy-on-tick(p) is
   posn(num-modulo(p.x + AIRPLANE-X-MOVE, WIDTH),
    p.y + AIRPLANE-Y-MOVE)
end
}
}

@exercise{
Before you proceed, have you written enough test cases? Are you sure?
Have you, for instance, tested what should happen when the airplane is
near the edge of the screen in either or both dimensions? We thought
not---go back and write more tests before you proceed!
}

Using the design recipe, now define @pyin{move-airplane-xy-on-tick}. You
should end up with something like this:
@pydisp{
fun move-airplane-xy-on-tick(w):
  posn(move-airplane-wrapping-x-on-tick(w.x),
    move-airplane-y-on-tick(w.y))
end
}
Note that we have reused the existing function for the
@math-lite{x}-dimension and, correspondingly, created a helper for the
@math-lite{y} dimension:
@pydisp{
fun move-airplane-y-on-tick(y):
  y + AIRPLANE-Y-MOVE
end
}
This may be slight overkill for now, but it does lead to a cleaner
@kwd{separation of concerns}, and makes it possible for the complexity
of movement in each dimension to evolve independently while keeping
the code relatively readable.

@subsection{Drawing the Scene}

We have to also examine and update @pyin{place-airplane-x}. Our
earlier definition placed the airplane at an arbitrary
@math-lite{y}-coordinate; now we have to take the
@math-lite{y}-coordinate from the World State: 
@pyin{
fun place-airplane-xy(w):
  I.place-image(AIRPLANE,
    w.x,
    w.y,
    BACKGROUND)
end
}
Notice that we can’t really reuse the previous deﬁnition because it hard-coded
the @math-lite{y}-position, which we must now make a parameter.

@subsection{Finishing Touches}

Are we done? It would seem so: we’ve examined all the procedures that
consume and produce World State and updated them
appropriately. Actually, we’re forgetting one small thing: the initial
World State given to @pyin{big-bang}! If we've changed the definition
of World State, then we need to reconsider this parameter, too. (We
also need to pass the new handlers rather than the old ones.)
@pydisp{
INIT-POS = posn(0, 0)

W.big-bang(INIT-POS, [list:
    W.on-tick(move-airplane-xy-on-tick),
    W.to-draw(place-airplane-xy)])
}

@exercise{
It’s a little unsatisfactory to have the airplane truncated by the
screen. You can use @pyin{I.image-width} and @pyin{I.image-height} to
obtain the dimensions of an image, such as the airplane. Use these to
ensure the airplane ﬁts entirely within the screen for the initial scene,
and similarly in @pyin{move-airplane-xy-on-tick}.
}

@version{Responding to Keystrokes}

Now that we have the airplane descending, there’s no reason it can't
ascend as well. Here’s a video:

@show-url{http://world.cs.brown.edu/1/projects/flight-lander/v4.swf}

We’ll use the keyboard to control its motion: speciﬁcally, the up-key
will make it move up, while the down-key will make it descend even
faster. This is easy to support using what we already know: we just
need to provide one more handler using @pyin{W.on-key}. This handler
takes @emph{two} arguments: the first is the current value of the
world, while the second is a representation of which key was
pressed. For the purposes of this program, the only key values we care
about are @pyin{"up"} and @pyin{"down"}.

Let's define a constant representing how much distance a key
represents:
@pydisp{
KEY-DISTANCE = 10
}
Now we can define a function that alter's the airplane's position by that
distance depending on which key is pressed:
@pydisp{
fun alter-airplane-y-on-key(w, key):
  ask:
    | key == "up"   then: posn(w.x, w.y - KEY-DISTANCE)
    | key == "down" then: posn(w.x, w.y + KEY-DISTANCE)
    | otherwise: w
  end
end
}

@incercise{
Why does this function definition contain
@pydisp{
    | otherwise: w
}
as its last condition?
}

Notice that if we receive any key other than the two we expect, we
leave the World State as it was; from the user’s perspective, this has
the effect of just ignoring the keystroke. Remove this last clause,
press some other key, and watch what happens!

No matter what you choose, be sure to test this! Can the airplane drift
off the top of the screen? How about off the screen at the bottom? Can
it overlap with the land or water?

Once we’ve written and thoroughly tested this function, we simply need
to ask Pyret to use it to handle keystrokes:
@pydisp{
W.big-bang(INIT-POS, [list:
    W.on-tick(move-airplane-xy-on-tick),
    W.on-key(alter-airplane-y-on-key),
    W.to-draw(place-airplane-xy)])
}
Now your airplane moves not only with the passage of time but also in
response to your keystrokes. You can keep it up in the air forever!

@version{Landing}

Remember that the objective of our game is to land the airplane, not to
keep it airborne indeﬁnitely. That means we need to detect when the
airplane reaches the land or water level and, when it does, terminate the
animation:

@show-url{http://world.cs.brown.edu/1/projects/flight-lander/v5.swf}

First, let’s try to characterize when the animation should halt. This
means writing a function that consumes the current World State and
produces a boolean value: @pyin{true} if the animation should halt,
@pyin{false} otherwise. This requires a little arithmetic based on the
airplane’s size:
@pydisp{
fun is-on-land-or-water(w):
  w.y >= (HEIGHT - BASE-HEIGHT)
end
}
We can also inform Pyret to use this predicate to automatically halt
the animation:
@pydisp{
W.big-bang(INIT-POS, [list:
    W.on-tick(move-airplane-xy-on-tick),
    W.on-key(alter-airplane-y-on-key),
    W.to-draw(place-airplane-xy),
    W.stop-when(is-on-land-or-water)])
}

@exercise{
When you test this, you'll see it isn't quite right because it doesn't
take account of the size of the airplane's image. As a result, the
airplane only halts when it's half-way into the land or water, not when
it first touches down. Adjust the formula so that it halts upon first
contact.
}

@exercise{
Extend this so that the airplane rolls for a while upon touching land,
decelerating according to the laws of physics.
}

@exercise{
Suppose the airplane is actually landing at a secret subterranean
airbase. The actual landing strip is actually below ground level, and
opens up only when the airplane comes in to land. That means, after
landing, only the parts of the airplane that stick above ground level
would be visible. Implement this. As a hint, consider modifying
@pyin{place-airplane-xy}.
}

@version{A Fixed Balloon}

Now let’s add a balloon to the scene. Here’s a video of the action:

@show-url{http://world.cs.brown.edu/1/projects/flight-lander/v6.swf}

Notice that while the airplane moves, everything else---including the
balloon---stays immobile. Therefore, we do not need to alter the World
State to record the balloon’s position. All we need to do is alter the
conditions under which the program halts: effectively, there is one
more situation under which it terminates, and that is a collision with
the balloon.

When does the game halt? There are now two circumstances: one is
contact with land or water, and the other is contact with the
balloon. The former remains unchanged from what it was before, so we can
focus on the latter.

Where is the balloon, and how do we represent where it is? The latter
is easy to answer: that’s what @pyin{posn}s are good for. As for the
former, we can decide where it is:
@pydisp{
BALLOON-LOC = posn(600, 300)
}
or we can let Pyret pick a random position:
@pydisp{
BALLOON-LOC = posn(random(WIDTH), random(HEIGHT))
}

@exercise{
Improve the random placement of the balloon so that it is in credible
spaces (e.g., not submerged).
}

Given a position for the balloon, we just need to detect
collision. One simple way is as follows: determine whether the
distance between the airplane and the balloon is within some threshold:
@pydisp{
fun are-overlapping(airplane-posn, balloon-posn):
  distance(airplane-posn, balloon-posn) 
    < COLLISION-THRESHOLD
end
}
where @pyin{COLLISION-THRESHOLD} is some suitable constant computed
based on the sizes of the airplane and balloon images. (For these
particular images, @pyin{75} works pretty well.)

What is @pyin{distance}? It consumes two @pyin{posn}s and determines
the Euclidean distance between them:
@pydisp{
fun distance(p1, p2):
  fun square(n): n * n end
  num-sqrt(square(p1.x - p2.x) + square(p1.y - p2.y))
end
}

Finally, we have to weave together the two termination conditions:
@pydisp{
fun game-ends(w):
  ask:
    | is-on-land-or-water(w)      then: true
    | are-overlapping(w, BALLOON) then: true
    | otherwise: false
  end
end
}
and use it instead:
@pydisp{
W.big-bang(INIT-POS, [list:
    W.on-tick(move-airplane-xy-on-tick),
    W.on-key(alter-airplane-y-on-key),
    W.to-draw(place-airplane-xy),
    W.stop-when(game-ends)])
}

@incercise{
Do you see how to write @pyin{game-ends} more concisely?
}

Here's another version:
@pydisp{
fun game-ends(w):
  is-on-land-or-water(w) or are-overlapping(w, BALLOON-LOC)
end
}

@version{Keep Your Eye on the Tank}

Now we'll introduce the idea of fuel. In our simplified world, fuel
isn't necessary to descend---gravity does that automatically---but it
is needed to climb. We'll assume that fuel is counted in whole number
units, and every ascension consumes one unit of fuel. When you run out
of fuel, the program no longer response to the up-arrow, so you can no
longer avoid either the balloon or water.

In the past, we’ve looked at still images of the game video to
determine what is changing and what isn’t.  For this version, we could
easily place a little gauge on the screen to show the quantity of fuel
left. However, we don’t on purpose, to illustrate a principle.

@callout{
You can’t always determine what is ﬁxed and what is changing just by
looking at the image.  You have to also read the problem statement
carefully, and think about it in depth.
}

It’s clear from our description that there are two things changing:
the position of the airplane and the quantity of fuel left. Therefore,
the World State must capture the current values of both of these. The
fuel is best represented as a single number. However, we do need to
create a new structure to represent the combination of these two.

@world-def{
The World State is a structure representing the airplane’s current
position and the quantity of fuel left.
}

Concretely, we will use this structure:
@pydisp{
data World:
  | world(p, f)
end
}

@exercise{
We could have also deﬁned the World to be a structure consisting of
three components: the airplane’s @math-lite{x}-position, the
airplane’s @math-lite{y}-position, and the quantity of fuel. Why do we
choose to use the representation above?
}

We can again look at each of the parts of the program to determine
what can stay the same and what changes. Concretely, we must focus on
the functions that consume and produce @pyin{World}s.

On each tick, we consume a world and compute one. The passage of time
does not consume any fuel, so this code can remain unchanged, other
than having to create a structure containing the current amount of
fuel. Concretely:
@pydisp{
fun move-airplane-xy-on-tick(w :: World):
  world(
    posn(
      move-airplane-wrapping-x-on-tick(w.p.x),
      move-airplane-y-on-tick(w.p.y)),
    w.f)
end
}
Similarly, the function that responds to keystrokes clearly needs to
take into account how much fuel is left:
@pydisp{
fun alter-airplane-y-on-key(w, key):
  ask:
    | key == "up"   then: 
      if w.f > 0:
        world(posn(w.p.x, w.p.y - KEY-DISTANCE), w.f - 1)
      else:
        w # there's no fuel, so ignore the keystroke
      end
    | key == "down" then: 
      world(posn(w.p.x, w.p.y + KEY-DISTANCE), w.f)
    | otherwise: w
  end
end
}

@exercise{
Updating the function that renders a scene. Recall that the world has
two ﬁelds; one of them corresponds to what we used to draw before, and
the other isn’t being drawn in the output.
}

@exercise{
Extend your program to draw a fuel gauge.
}

@version{The Balloon Moves, Too}

Until now we’ve left our balloon immobile. Let's now make the game
more interesting by letting the balloon move, as this video shows:

@show-url{http://world.cs.brown.edu/1/projects/flight-lander/v8.swf}

Obviously, the balloon’s location needs to also become part of the
World State.

@world-def{
The World State is a structure representing the plane’s current
position, the balloon’s current position, and the quantity of fuel
left.
}

Here is a representation of the world state:
@pydisp{
data World:
  | world(p :: Posn, b :: Posn, f :: Number)
end
}
With this deﬁnition, we obviously need to re-write all our previous
deﬁnitions. Most of this is quite routine relative to what we’ve seen
before. The only detail we haven’t really speciﬁed is how the balloon
is supposed to move: in what direction, at what speed, and what to do
at the edges. We’ll let you use your imagination for this one!
(Remember that the closer the balloon is to land, the harder it is to
safely land the plane.)

We thus have to modify:
@itemlist[

@item{The background image (to remove the static balloon).}

@item{The drawing handler (to draw the balloon at its position).}

@item{The timer handler (to move the balloon as well as the
airplane).}

@item{The key handler (to construct world data that leaves the balloon
unchanged).}

@item{The termination condition (to account for the balloon's dynamic
location).}

]

@exercise{
Modify each of the above functions, along with their test cases.
}

@version{One, Two, ..., Ninety-Nine Luftballons!}

Finally, there’s no need to limit ourselves to only one balloon. How
many is right? Two? Three? Ten? ... Why ﬁx any one number? It could be
a balloon festival!@margin-note*{
  @(image #:scale 1/10 "Pyret-Tutorial/balloon-fiesta.png")
  @(linebreak)
  @(emph "Albuquerque Balloon Fiesta")}

Similarly, many games have levels that become progressively harder; we
could do the same, letting the number of balloons be part of what
changes across levels. However, there is conceptually no big
difference between having two balloons and ﬁve; the code to control
each balloon is essentially the same.

We need to represent a collection of balloons. We can use a list to
represent them. Thus:

@world-def{
The World State is a structure representing the plane’s current
position, a list of balloon positions, and the quantity of fuel
left.
}

You should now use the design recipe for lists of structures to
rewrite the functions. Notice that you’ve already written the function
to move one balloon. What’s left?
@enumerate[

@item{Apply the same function to each balloon in the list.}

@item{Determine what to do if two balloons collide.}

]
For now, you can avoid the latter problem by placing each balloon
sufficiently spread apart along the @math-lite{x}-dimension and
letting them move only up and down.

@exercise{
Introduce a concept of @emph{wind}, which affects balloons but not the
airplane. Afer random periods of time, the wind blows with random
speed and direction, causing the ballooons to move laterally.
}
