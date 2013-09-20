#lang pyret

import "image2.arr" as image
import big-bang as bb
import math as Math
big-bang = bb.big-bang

_dimension = 1000
_width = _dimension
_height = _dimension

_centrex = _width / 2
_centrey = _height / 2

_numberofbirds = 25

_positionspread = 10 * _dimension
_speedspread = 5
_maxspeed = 10
_border = _dimension / 50
_leaderborder = _border
_borderspeedchange = 0.2

_mindist = 10
_matchspeedwindow = 40

_leaderbirdrandomspeedchange = 1
_leadermaxspeed = 5

_barriers = [[450,500],[475,500],[500,500],[525,500],[625,500],[650,500],[675,500],[700,500],[400,500],[425,500],[400,525],[400,550],[400,650],[400,675],[400,700],[375,700],[350,700],[325,700],[300,700],[275,700],[250,700],[800,200],[250,700],[100,100]]
_barrierradius = 30

fun uniform(start, range):
  Math.sample(Math.uniform-dist(start - range, start + range))
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

  speed = (leaderbirdvx.sqr() + leaderbirdvy.sqr()).sqrt()
  
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
      vx := vx + _borderspeedchange
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
        dist = (dx.sqr() + dy.sqr()).sqrt()
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
      dist = (dx.sqr() + dy.sqr()).sqrt()
      when dist < (_barrierradius + 15):
        vx := vx - (dx * 0.1)
        vx := vy * 0.6
        vy := vy - (dy * 0.1)
        vy := vy * 0.6
      end
    end
    _barriers.map(bounce)
    
    new-speed = (vx.sqr() + vy.sqr()).sqrt()
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

background = image.rectangle(_width, _height, "solid", "black")
  
with-barriers =
    _barriers.foldr(fun(barrier, img):
                            
      img.place-image(barrier.first, barrier.rest.first, image.circle(_barrierradius, "solid", "blue")) end,
      background)

fun drawer(w):
  
  with-boids =
    w.birdlist.foldr(fun(boid, img):
                          
      img.place-image(boid.first, boid.rest.first, image.circle(5, "solid", "yellow")) end,
      with-barriers)

  with-boids.place-image(w.leaderbirdx, w.leaderbirdy, image.circle(5, "solid", "white")).to-image()
end

big-bang(world0, {
  on-tick: ticker,
  to-draw: drawer
})




