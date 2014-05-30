#lang pyret/whalesong

import image as I
import world as W

place-image = I.place-image
circle = I.circle
rectangle = I.rectangle

sqr = num-expt(_, 2)

_dimension = 500
_width = _dimension
_height = _dimension

_centrex = _width / 2
_centrey = _height / 2

_numberofbirds = 3

_positionspread = 10 * _dimension
_speedspread = 5
_maxspeed = 10.0
_border = _dimension / 50
_leaderborder = _border
_borderspeedchange = 0.2

_mindist = 10
_matchspeedwindow = 40

_leaderbirdrandomspeedchange = 1
_leadermaxspeed = 5

_barriers = [[100,100], [200, 200]]
_barrierradius = 30

fun uniform(start, span):
  random((span * 2) + 1) + (start - span)
end

fun birdinit(_):
  x = uniform(_centrex, _positionspread)
  y = uniform(_centrey, _positionspread)
  vx = uniform(0, _speedspread)
  vy = uniform(0, _speedspread)
  [x, y, vx, vy]
end

birdlist = list.range(0, _numberofbirds).map(birdinit)

world0 = {
  birdlist: birdlist,
  size: {width: _width, height: _height},
  leaderbirdx: _dimension / 3,
  leaderbirdy: _dimension / 3,
  leaderbirdvx: 0,
  leaderbirdvy: 0
}

fun ticker(w):
  var leaderbirdx = w.leaderbirdx
  var leaderbirdy = w.leaderbirdy
  var leaderbirdvx = w.leaderbirdvx
  var leaderbirdvy = w.leaderbirdvy
  
  when leaderbirdx < _leaderborder:
    leaderbirdvx := leaderbirdvx + _borderspeedchange
  end
  
  when leaderbirdy < _leaderborder:
    leaderbirdvy := leaderbirdvy + _borderspeedchange
  end
  
  when leaderbirdx > (_width - _leaderborder):
    leaderbirdvx := leaderbirdvx - _borderspeedchange
  end
  
  when leaderbirdy > (_height - _leaderborder):
    leaderbirdvy := leaderbirdvy - _borderspeedchange
  end
  
  leaderbirdvx := leaderbirdvx + uniform(0, _leaderbirdrandomspeedchange)
  leaderbirdvy := leaderbirdvy + uniform(0, _leaderbirdrandomspeedchange)

  speed = (leaderbirdvx^num-expt(2) + leaderbirdvy^num-expt(2))^num-sqrt()
  
  when speed > (_leadermaxspeed):
    leaderbirdvx := leaderbirdvx * (_leadermaxspeed / speed)
    leaderbirdvy := leaderbirdvy * (_leadermaxspeed / speed)
  end
  
  leaderbirdx := (leaderbirdx + leaderbirdvx)
  leaderbirdy := (leaderbirdy + leaderbirdvy)
  
  var i = 0
  fun update(b):
    var x = b.first
    var y = b.rest.first
    var vx = b.rest.rest.first
    var vy = b.rest.rest.rest.first
    
    when x < _border:
      vx := vx + _borderspeedchange
    end
    
    when y < _border:
      vy := vy + _borderspeedchange
    end
    
    when x > (_width - _border):
      vx := vx - _borderspeedchange
    end
    
    when y > (_height - _border):
      vy := vy - _borderspeedchange
    end
    
    leaderdiffx = (leaderbirdx - x)
    leaderdiffy = (leaderbirdy - y)
    vx := vx + (0.007 * leaderdiffx)
    vy := vy + (0.007 * leaderdiffy)
    
    var j = 0
    var avxtotal = 0
    var avytotal = 0
    var avcount = 0
    fun do_avg(b2): 
      when j <> i:
        dx = b2.first - x
        dy = b2.rest.first - y
        dist = (dx^sqr() + dy^sqr())^num-sqrt()
        when dist < _mindist:
          vx := vx - (dx * 0.2)
          vy := vy - (dy * 0.2)
        end
        when dist < _matchspeedwindow:
          avxtotal := avxtotal + b2.rest.rest.first
          avytotal := avytotal + b2.rest.rest.rest.first
          avcount := avcount + 1
        end
      end
      j := (j + 1)
    end
    
    birdlist.map(do_avg)
    
    
    when 0 <> avcount:
      avx = avxtotal / avcount
      avy = avytotal / avcount
      vx := (0.9 * vx) + (0.1 * avx)
      vy := (0.9 * vy) + (0.1 * avy)
    end
    
    fun bounce(barrier):
      dx = barrier.first - x
      dy = barrier.rest.first - y
      dist = (dx^sqr() + dy^sqr())^num-sqrt()
      when dist < (_barrierradius + 15):
        vx := vx - (dx * 0.1)
        vx := vy * 0.6
        vy := vy - (dy * 0.1)
        vy := vy * 0.6
      end
    end
    _barriers.map(bounce)
    
    
    new-speed = (vx^sqr() + vy^sqr())^num-sqrt()
    when new-speed > _maxspeed:
      vx := vx * (_maxspeed / new-speed)
      vy := vy * (_maxspeed / new-speed)
    end
    
    
    i := i + 1
    [
      b.first + vx,
      b.rest.first + vy,
      vx,
      vy
    ]
  end
  
  {
    birdlist: w.birdlist.map(update),
    size: w.size,
    leaderbirdx: leaderbirdx,
    leaderbirdy: leaderbirdy,
    leaderbirdvx: leaderbirdvx,
    leaderbirdvy: leaderbirdvy
  }
end

background = rectangle(_width, _height, "solid", "black")
  
with-barriers =
  for list.fold(img from background, barrier from _barriers):
    place-image(
      circle(_barrierradius, "solid", "blue"),
      barrier.first,
      barrier.rest.first,
      img)
  end

fun drawer(w):
  with-boids =
    for list.fold(img from with-barriers, boid from w.birdlist):
      place-image(
        circle(5, "solid", "yellow"),
        boid.first,
        boid.rest.first,
        img)
    end

  place-image(
    circle(5, "solid", "white"),
    w.leaderbirdx,
    w.leaderbirdy,
    with-boids)
end

W.big-bang(world0, [
    W.on-tick(ticker),
    W.to-draw(drawer)
  ])

