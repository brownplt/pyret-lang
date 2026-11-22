include image
include image-structs
import world as W

# The world is the x and y position of the dog, the x and y position of the
# coin, the x and y position of the cat, and the score
data World:
  | world(dogX :: Number, dogY :: Number, coinX :: Number, coinY :: Number,
catX :: Number, catY :: Number, score :: Number)
end

START = world(0, 400, 600, 300, 320, 240, 0)
NEXT = world(10, 400, 595, 300, 320, 240, 0)

BACKGROUND = image-url(
  "http://www.bootstrapworld.org/clipart/Curriculum/bg.jpg")
BACKGROUND2 =
image-url("http://www.bootstrapworld.org/clipart/Curriculum/night-landscape.jpg")
DANGER = flip-horizontal(image-url(
    "http://www.bootstrapworld.org/clipart/Curriculum/dog.png"))
TARGET = image-url(
  "http://www.bootstrapworld.org/clipart/Curriculum/coin.png")
PLAYER = image-url(
  "http://www.bootstrapworld.org/clipart/Curriculum/ninja.png")
CLOUD = image-url(
  "http://www.bootstrapworld.org/clipart/Curriculum/clouds.png")

####################
# GRAPHICS FUNCTIONS
####################

# draw-world : World -> Image:
# Place DANGER, TARGET, CLOUD, and PLAYER onto BACKGROUND at the right
# coordinates
fun draw-world(w):
  ask:
    | w.score > 500 then:
      put-image(text(num-to-string(w.score), 30, purple), 320, 450,
    put-image(PLAYER, w.catX, w.catY,
      put-image(TARGET, w.coinX, w.coinY,
        put-image(CLOUD, 500, 400,
              put-image(DANGER, w.dogX, w.dogY, BACKGROUND2)))))
    | otherwise: 
      put-image(text(num-to-string(w.score), 30, purple), 320, 450,
        put-image(PLAYER, w.catX, w.catY,
          put-image(TARGET, w.coinX, w.coinY,
            put-image(CLOUD, 500, 400,
              put-image(DANGER, w.dogX, w.dogY, BACKGROUND)))))
  end
end

####################
# UPDATING FUNCTIONS
####################

# update-world : World  -> World
# Increase dogX by ten, decrease coinX by five
examples: 
  update-world(START) is NEXT
end

fun update-world(w):
  ask:
    | is-collision(w.catX, w.catY, w.dogX, w.dogY) then:
      world(-50, num-random(480), w.coinX, w.coinY, w.catX, w.catY, w.score -
20)
    | is-collision(w.catX, w.catY, w.coinX, w.coinY) then:
      world(w.dogX, w.dogY, 650, num-random(480), w.catX, w.catY, w.score +
30)
    | is-off-left(w.coinX) then: world(w.dogX, w.dogY, 650, num-random(480),
w.catX, w.catY, w.score)
    | is-off-right(w.dogX) then: world(-50, num-random(480), w.coinX, w.coinY,
w.catX, w.catY, w.score)
    | otherwise: world(w.dogX + 10, w.dogY, w.coinX - 5, w.coinY,  w.catX,
w.catY, w.score)
  end
end

#############
# KEY EVENTS:
#############

# keypress : World, String -> World
# Make cat respond to key events
examples:
  keypress(START, "up") is
  world(START.dogX, START.dogY, START.coinX, START.coinY, START.catX,
START.catY + 10, START.score)
  keypress(NEXT, "up") is
  world(NEXT.dogX, NEXT.dogY, NEXT.coinX, NEXT.coinY, NEXT.catX, NEXT.catY +
10, NEXT.score)
  keypress(START, "down") is
  world(START.dogX, START.dogY, START.coinX, START.coinY, START.catX,
START.catY - 10, START.score)
  keypress(NEXT, "left") is
  world(NEXT.dogX, NEXT.dogY, NEXT.coinX, NEXT.coinY, NEXT.catX - 10,
NEXT.catY, NEXT.score)
  keypress(NEXT, "right") is
  world(NEXT.dogX, NEXT.dogY, NEXT.coinX, NEXT.coinY, NEXT.catX + 10,
NEXT.catY, NEXT.score)
end

fun keypress(w, key):
  ask:
    | string-equal(key, "up") then: world(w.dogX, w.dogY, w.coinX, w.coinY,
w.catX, w.catY + 10, w.score)
    | string-equal(key, "down") then: world(w.dogX, w.dogY, w.coinX, w.coinY,
w.catX, w.catY - 10, w.score)
    | string-equal(key, "left") then: world(w.dogX, w.dogY, w.coinX, w.coinY,
w.catX - 10, w.catY, w.score)
    | string-equal(key, "right") then: world(w.dogX, w.dogY, w.coinX, w.coinY,
w.catX + 10, w.catY, w.score)
    | otherwise: world(w.dogX, w.dogY, w.coinX, w.coinY, w.catX, w.catY,
w.score)
  end
end
  
#################
# TESTS FOR COND:
#################

# is-off-left: Number -> Boolean
# Checks whether an object has gone off the left side of the screen
fun is-off-left(x):
  x < -50
end

# is-off-right: Number -> Boolean
# Checks whether an object has gone off the right side of the screen
fun is-off-right(x):
  x > 690
end

# line-length: Number Number -> Number
# Finds the distance between two points in one demension
fun line-length(x1, x2):
  ask:
    | x1 > x2 then: x1 - x2
    |otherwise: x2 - x1
  end
end

# distance: Number Number Number Number -> Number
# Finds the distance between (x1, y1) and (x2, y2)
fun distance(x1, y1, x2, y2):
  num-sqrt(num-sqr(line-length(x1, x2)) + num-sqr(line-length(y1, y2)))   
end

# is-collision: Number Number Number Number -> Boolean
# Determines whether two objects are within 50 pixels of each other
fun is-collision(x1, y1, x2, y2):
  distance(x1, y1, x2, y2) < 50 
end
###################################
# big-bang using the START world
# on a tick-event, use update-world
# on a draw-event, use draw-world 
# on a key-event, use keypress
###################################

W.big-bang(START, [list:
  W.on-tick(update-world),
  W.to-draw(draw-world),
    W.on-key(keypress)])
