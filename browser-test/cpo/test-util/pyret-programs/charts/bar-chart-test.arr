import color as C
include chart
include image

################################################################################
# CONSTANTS
################################################################################

#####################
# SINGLE BAR CHARTS 
#####################

single-bars = from-list.bar-chart(
  [list: "Pyret", "OCaml", "C", "C++", "Python", "Racket", "Smalltalk"],
  [list: 10,       6,       1,   3,     5,       8,        9])

single-bars-neg = from-list.bar-chart(
  [list: "Pyret", "OCaml", "C", "C++", "Python", "Racket", "Smalltalk"],
  [list: -10,       -6,       1,   3,     -5,       -8,        -9])

single-bars-rep = from-list.bar-chart(
  [list: "Pyret", "OCaml", "C", "C++", "Python", "Racket", "Pyret"],
  [list: 10,       6,       1,   3,     5.5,       8,        9])

single-bars-roughall = from-list.bar-chart(
  [list: "Pyret", "OCaml", "C", "C++", "Python", "Racket", "Smalltalk"],
  [list: ~10.1141,   ~6,   ~20,   ~-3.23,     ~0,       ~8,        ~92.2])

single-bars-roughsome = from-list.bar-chart(
  [list: "Pyret", "OCaml", "C", "C++", "Python", "Racket", "Smalltalk"],
  [list: 10.1141,   ~6,   20,   -3.23,     ~0,       ~8,        ~92.2])

######################
# GROUPED BAR CHARTS 
######################

grouped-bars = from-list.grouped-bar-chart(
  [list: 'CA', 'TX', 'NY', 'FL', 'IL', 'PA'],
  [list:
    [list: 2704659,4499890,2159981,3853788,10604510,8819342,4114496],
    [list: 2027307,3277946,1420518,2454721,7017731,5656528,2472223],
    [list: 1208495,2141490,1058031,1999120,5355235,5120254,2607672],
    [list: 1140516,1938695,925060,1607297,4782119,4746856,3187797],
    [list: 894368,1558919,725973,1311479,3596343,3239173,1575308],
    [list: 737462,1345341,679201,1203944,3157759,3414001,1910571]],
  [list:
    'Under 5 Years',
    '5 to 13 Years',
    '14 to 17 Years',
    '18 to 24 Years',
    '25 to 44 Years',
    '45 to 64 Years',
    '65 Years and Over'])

grouped-bars-neg = from-list.grouped-bar-chart(
    [list: '2010', '2011', '2012', '2013', '2014', 
           '2015', '2016', '2017', '2018', '2019'],
  [list: 
    [list: -3.78, -1.14, -1.06, -3.54, -1.74, -1.23, 0.42],
    [list: -3.81, -1.18, -1.05, -3.56, -1.77, -1.25, 0.35],
    [list: -3.83, -1.21, -1.04, -3.58, -1.81, -1.27, 0.27],
    [list: -3.84, -1.24, -1.05, -3.60, -1.86, -1.29, 0.18],
    [list: -3.86, -1.27, -1.06, -3.61, -1.91, -1.31, 0.09],
    [list: -3.87, -1.29, -1.06, -3.63, -1.97, -1.33, 0],
    [list: -3.88, -1.31, -1.07, -3.64, -2.03, -1.35, -0.08],
    [list: -3.89, -1.32, -1.07, -3.66, -2.09, -1.36, -0.17],
    [list: -3.90, -1.33, -1.07, -3.67, -2.15, -1.38, -0.25],
    [list: -3.91, -1.34, -1.07, -3.68, -2.21, -1.39, -0.33]],
  [list: 'Asia', 'North America', 'Europe', 
     'South America', 'Africa', 'Oceania', 'Angola'])

grouped-bars-rep = from-list.grouped-bar-chart(
    [list: 'Liam', 'Elijah', 'Ava', 'Sophia', 'Ava', 'Emma'],
    [list: 
      [list: 10, 8, 10, 4, 8, 9, 7],
      [list: 9, 7, 8, 9, 5, 5, 8], 
      [list: 8, 10, 9, 4, 5, 10, 7],
      [list: 10, 6, 7, 4, 10, 6, 5], 
      [list: 8, 8, 9, 8, 4, 4, 5], 
      [list: 6, 9, 8, 6, 9, 10, 7]], 
    [list: 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 
   'Friday', 'Saturday', 'Sunday'])

grouped-bars-repgroups = from-list.grouped-bar-chart(
    [list: 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 
           'Friday', 'Saturday', 'Sunday'], 
    [list: 
      [list: 10, 9, 8, 10, 8, 6], 
      [list: 8, 7, 10, 6, 8, 9],
      [list: 10, 9, 9, 7, 9, 8], 
      [list: 4, 9, 4, 4, 8, 6], 
      [list: 8, 5, 5, 10, 4, 9], 
      [list: 9, 5, 10, 6, 4, 10], 
      [list: 7, 8, 7, 5, 5, 7]],
    [list: 'Liam', 'Elijah', 'Ava', 'Sophia', 'Ava', 'Emma'])

grouped-bars-roughall = from-list.grouped-bar-chart(
    [list: 'Sticky Sam', 'Sticky Sam: Even Stickier', 
           'Adhesive Adrian DLC', 'Sticky Sam on Mobile', 
           'Sticky Sam 3: The Return'], 
    [list: 
      [list: ~5, ~8, ~5, ~8, ~1, ~2], 
      [list: ~8, ~3, ~4, ~10, ~0, ~3],
      [list: ~8, ~1, ~10, ~3, ~1, ~8], 
      [list: ~7, ~2, ~5, ~7, ~3, ~4], 
      [list: ~4, ~8, ~5, ~3, ~1, ~7]],
    [list: 
      'People actively involved in the Community', 
      'People who want specific new features', 
      'People who are making mods', 
      'People who have played the game over 100 hours',
      'People who have returned the game', 
      'People who preordered the game']) 

grouped-bars-roughsome = from-list.grouped-bar-chart(
    [list: 'Sticky Sam', 'Sticky Sam: Even Stickier', 
           'Adhesive Adrian DLC', 'Sticky Sam on Mobile', 
           'Sticky Sam 3: The Return'], 
    [list: 
      [list: 5, ~8, 5, 8, ~1, 2], 
      [list: ~8, ~3, 4, ~10, ~0, ~3],
      [list: 8, ~1, ~10, ~3, 1, 8], 
      [list: ~7, ~2, ~5, ~7, ~3, ~4], 
      [list: ~4, ~8, 5, ~3, ~1, ~7]],
    [list: 
      'People actively involved in the Community', 
      'People who want specific new features', 
      'People who are making mods', 
      'People who have played the game over 100 hours',
      'People who have returned the game', 
      'People who preordered the game']) 

######################
# STACKED BAR CHARTS 
######################

stacked-bars = from-list.stacked-bar-chart(
  [list: 'CA', 'TX', 'NY', 'FL', 'IL', 'PA'],
  [list:
    [list: 2704659,4499890,2159981,3853788,10604510,8819342,4114496],
    [list: 2027307,3277946,1420518,2454721,7017731,5656528,2472223],
    [list: 1208495,2141490,1058031,1999120,5355235,5120254,2607672],
    [list: 1140516,1938695,925060,1607297,4782119,4746856,3187797],
    [list: 894368,1558919,725973,1311479,3596343,3239173,1575308],
    [list: 737462,1345341,679201,1203944,3157759,3414001,1910571]],
  [list:
    'Under 5 Years',
    '5 to 13 Years',
    '14 to 17 Years',
    '18 to 24 Years',
    '25 to 44 Years',
    '45 to 64 Years',
    '65 Years and Over'])

stacked-bars-neg = from-list.stacked-bar-chart(
    [list: '2010', '2011', '2012', '2013', '2014', 
           '2015', '2016', '2017', '2018', '2019'],
    [list: 
      [list: -3.78, -1.14, -1.06, -3.54, -1.74, -1.23, 0.42],
      [list: -3.81, -1.18, -1.05, -3.56, -1.77, -1.25, 0.35],
      [list: -3.83, -1.21, -1.04, -3.58, -1.81, -1.27, 0.27],
      [list: -3.84, -1.24, -1.05, -3.60, -1.86, -1.29, 0.18],
      [list: -3.86, -1.27, -1.06, -3.61, -1.91, -1.31, 0.09],
      [list: -3.87, -1.29, -1.06, -3.63, -1.97, -1.33, 0],
      [list: -3.88, -1.31, -1.07, -3.64, -2.03, -1.35, -0.08],
      [list: -3.89, -1.32, -1.07, -3.66, -2.09, -1.36, -0.17],
      [list: -3.90, -1.33, -1.07, -3.67, -2.15, -1.38, -0.25],
      [list: -3.91, -1.34, -1.07, -3.68, -2.21, -1.39, -0.33]],
    [list: 'Asia', 'North America', 'Europe', 
           'South America', 'Africa', 'Oceania', 'Angola'])

stacked-bars-rep = from-list.stacked-bar-chart(
    [list: 'Liam', 'Elijah', 'Ava', 'Sophia', 'Ava', 'Emma'],
    [list: 
      [list: 10, 8, 10, 4, 8, 9, 7],
      [list: 9, 7, 8, 9, 5, 5, 8], 
      [list: 8, 10, 9, 4, 5, 10, 7],
      [list: 10, 6, 7, 4, 10, 6, 5], 
      [list: 8, 8, 9, 8, 4, 4, 5], 
      [list: 6, 9, 8, 6, 9, 10, 7]], 
    [list: 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 
           'Friday', 'Saturday', 'Sunday'])

stacked-bars-repstacks = from-list.stacked-bar-chart(
    [list: 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 
           'Friday', 'Saturday', 'Sunday'], 
    [list: 
      [list: 10, 9, 8, 10, 8, 6], 
      [list: 8, 7, 10, 6, 8, 9],
      [list: 10, 9, 9, 7, 9, 8], 
      [list: 4, 9, 4, 4, 8, 6], 
      [list: 8, 5, 5, 10, 4, 9], 
      [list: 9, 5, 10, 6, 4, 10], 
      [list: 7, 8, 7, 5, 5, 7]],
    [list: 'Liam', 'Elijah', 'Ava', 'Sophia', 'Ava', 'Emma'])

stacked-bars-roughall = from-list.stacked-bar-chart(
    [list: 'Sticky Sam', 'Sticky Sam: Even Stickier', 
           'Adhesive Adrian DLC', 'Sticky Sam on Mobile', 
           'Sticky Sam 3: The Return'], 
    [list: 
      [list: ~5, ~8, ~5, ~8, ~1, ~2], 
      [list: ~8, ~3, ~4, ~10, ~0, ~3],
      [list: ~8, ~1, ~10, ~3, ~1, ~8], 
      [list: ~7, ~2, ~5, ~7, ~3, ~4], 
      [list: ~4, ~8, ~5, ~3, ~1, ~7]],
    [list: 
      'People actively involved in the Community', 
      'People who want specific new features', 
      'People who are making mods', 
      'People who have played the game over 100 hours',
      'People who have returned the game', 
      'People who preordered the game']) 

stacked-bars-roughsome = from-list.stacked-bar-chart(
    [list: 'Sticky Sam', 'Sticky Sam: Even Stickier', 
           'Adhesive Adrian DLC', 'Sticky Sam on Mobile', 
           'Sticky Sam 3: The Return'], 
    [list: 
      [list: 5, ~8, 5, 8, ~1, 2], 
      [list: ~8, ~3, 4, ~10, ~0, ~3],
      [list: 8, ~1, ~10, ~3, 1, 8], 
      [list: ~7, ~2, ~5, ~7, ~3, ~4], 
      [list: ~4, ~8, 5, ~3, ~1, ~7]],
    [list: 
      'People actively involved in the Community', 
      'People who want specific new features', 
      'People who are making mods', 
      'People who have played the game over 100 hours',
      'People who have returned the game', 
      'People who preordered the game'])

################################################################################
# Helper Functions -- Testing 
################################################################################

fun render-image(series):
  render-chart(series).get-image()
end

fun count-vowels(s): 
  fold(
    {(acc, elm): 
      if string-explode('aeiouAEIOU').member(elm): acc + 1
      else: acc
      end
    }, 0, string-explode(s))
end

################################################################################
# Actual Testing -- Checks
################################################################################

#####################
# RENDERING TESTS 
#####################

check "Rendering: Single Bars":
  render-image(single-bars) satisfies is-image
  render-image(single-bars-neg) satisfies is-image
  render-image(single-bars-rep) satisfies is-image
  render-image(single-bars-roughall) satisfies is-image
  render-image(single-bars-roughsome) satisfies is-image

  from-list.bar-chart(empty, empty)
  raises "can't have empty data"

  from-list.bar-chart([list: "label1", "label2"], [list: 1]) 
  raises "labels and values should have the same length"

  from-list.bar-chart([list: "label"], [list: 1, 2]) 
  raises "labels and values should have the same length"
end

check "Rendering: Grouped Bars": 
  render-image(grouped-bars) satisfies is-image
  render-image(grouped-bars-neg) satisfies is-image
  render-image(grouped-bars-rep) satisfies is-image
  render-image(grouped-bars-repgroups) satisfies is-image
  render-image(grouped-bars-roughall) satisfies is-image
  render-image(grouped-bars-roughsome) satisfies is-image

  from-list.grouped-bar-chart(empty, empty, empty)
  raises "can't have empty data"

  from-list.grouped-bar-chart(
        [list: 'There', 'are', 'no', 'stacks'],
        [list: empty, empty, empty, empty], 
        empty) 
  raises "can't have empty legends"

  from-list.grouped-bar-chart(
        [list: 'Bar 1', 'Bar 2', 'Bar 3', 'Bar 4'],
        [list: [list: 1, 3], [list: 2, 2], [list: 0, -3]], 
        [list: 'Component 1', 'Component 2'])
  raises "labels and values should have the same length"
  
  from-list.grouped-bar-chart(
        [list: 'Bar 1', 'Bar 2'],
        [list: [list: 1, 3], [list: 2, 2], [list: 0, -3]], 
        [list: 'Component 1', 'Component 2']) 
  raises "labels and values should have the same length"
  
  from-list.grouped-bar-chart(
        [list: 'Bar 1', 'Bar 2', 'Bar 3', 'Bar 4'],
        [list: [list: 1, 3], [list: 2, 2], [list: 0, -3], [list: 1, 2]], 
        [list: 'Component 1', 'Component 2', 'Component 3'])
  raises "labels and legends should have the same length"

  from-list.grouped-bar-chart(
        [list: 'Bar 1', 'Bar 2', 'Bar 3', 'Bar 4'],
        [list: [list: 1, 3], [list: 2, 2], [list: 0, -3], [list: 1]], 
        [list: 'Component 1', 'Component 2'])
  raises "labels and legends should have the same length"
end

check "Rendering: Stacked Bars": 
  render-image(stacked-bars) satisfies is-image
  render-image(stacked-bars-neg) satisfies is-image
  render-image(stacked-bars-rep) satisfies is-image
  render-image(stacked-bars-repstacks) satisfies is-image
  render-image(stacked-bars-roughall) satisfies is-image
  render-image(stacked-bars-roughsome) satisfies is-image

  from-list.stacked-bar-chart(empty, empty, empty)
  raises "can't have empty data"

  from-list.stacked-bar-chart(
        [list: 'There', 'are', 'no', 'stacks'],
        [list: empty, empty, empty, empty], 
        empty) 
  raises "can't have empty legends"

  from-list.stacked-bar-chart(
        [list: 'Bar 1', 'Bar 2', 'Bar 3', 'Bar 4'],
        [list: [list: 1, 3], [list: 2, 2], [list: 0, -3]], 
        [list: 'Component 1', 'Component 2'])
  raises "labels and values should have the same length"
  
  from-list.stacked-bar-chart(
        [list: 'Bar 1', 'Bar 2'],
        [list: [list: 1, 3], [list: 2, 2], [list: 0, -3]], 
        [list: 'Component 1', 'Component 2']) 
  raises "labels and values should have the same length"
  
  from-list.stacked-bar-chart(
        [list: 'Bar 1', 'Bar 2', 'Bar 3', 'Bar 4'],
        [list: [list: 1, 3], [list: 2, 2], [list: 0, -3], [list: 1, 2]], 
        [list: 'Component 1', 'Component 2', 'Component 3'])
  raises "labels and legends should have the same length"

  from-list.stacked-bar-chart(
        [list: 'Bar 1', 'Bar 2', 'Bar 3', 'Bar 4'],
        [list: [list: 1, 3], [list: 2, 2], [list: 0, -3], [list: 1]], 
        [list: 'Component 1', 'Component 2'])
  raises "labels and legends should have the same length"
end

######################
# COLOR METHOD TESTS 
######################

single-color = [list: C.red]
rainbow-colors = [list: C.red, C.orange, C.yellow, C.green, C.blue, C.indigo, C.violet]
manual-colors = 
  [list: 
    C.color(51, 72, 252, 0.57), C.color(195, 180, 104, 0.87),
    C.color(115, 23, 159, 0.24), C.color(144, 12, 138, 0.13),
    C.color(31, 132, 224, 0.83), C.color(166, 16, 72, 0.59),
    C.color(58, 193, 241, 0.98)]
less-colors = [list: C.red, C.green, C.blue, C.orange, C.purple]
more-colors = [list: C.red, C.green, C.blue, C.orange, C.purple, C.yellow, C.indigo, C.violet]

check "Color Methods: Single Bars":
  render-image(single-bars.color(C.red)) satisfies is-image
  render-image(single-bars-neg.color(C.green)) satisfies is-image
  render-image(single-bars-rep.color(C.violet)) satisfies is-image
  render-image(single-bars-roughall.color(C.orange)) satisfies is-image
  render-image(single-bars-roughsome.color(C.cyan)) satisfies is-image

  render-image(single-bars.colors(empty)) satisfies is-image
  render-image(single-bars.colors(single-color)) satisfies is-image
  render-image(single-bars.colors(rainbow-colors)) satisfies is-image
  render-image(single-bars.colors(manual-colors)) satisfies is-image
  render-image(single-bars.colors(less-colors)) satisfies is-image
  render-image(single-bars.colors(more-colors)) satisfies is-image
  render-image(single-bars-neg.colors(rainbow-colors)) satisfies is-image
  render-image(single-bars-rep.colors(more-colors)) satisfies is-image
  render-image(single-bars-roughall.colors(manual-colors)) satisfies is-image
  render-image(single-bars-roughsome.colors(less-colors)) satisfies is-image
end

check "Color Methods: Grouped Bars":
  render-image(grouped-bars.colors(empty)) satisfies is-image
  render-image(grouped-bars.colors(single-color)) satisfies is-image
  render-image(grouped-bars.colors(rainbow-colors)) satisfies is-image
  render-image(grouped-bars.colors(manual-colors)) satisfies is-image
  render-image(grouped-bars.colors(less-colors)) satisfies is-image
  render-image(grouped-bars.colors(more-colors)) satisfies is-image
  render-image(grouped-bars-neg.colors(single-color)) satisfies is-image
  render-image(grouped-bars-rep.colors(less-colors)) satisfies is-image
  render-image(grouped-bars-repgroups.colors(more-colors)) satisfies is-image
  render-image(grouped-bars-roughall.colors(manual-colors)) satisfies is-image
  render-image(grouped-bars-roughsome.colors(rainbow-colors)) satisfies is-image
end

check "Color Methods: Stacked Bars":
  render-image(stacked-bars.colors(empty)) satisfies is-image
  render-image(stacked-bars.colors(single-color)) satisfies is-image
  render-image(stacked-bars.colors(rainbow-colors)) satisfies is-image
  render-image(stacked-bars.colors(manual-colors)) satisfies is-image
  render-image(stacked-bars.colors(less-colors)) satisfies is-image
  render-image(stacked-bars.colors(more-colors)) satisfies is-image
  render-image(stacked-bars-neg.colors(manual-colors)) satisfies is-image
  render-image(stacked-bars-rep.colors(single-color)) satisfies is-image
  render-image(stacked-bars-repstacks.colors(rainbow-colors)) satisfies is-image
  render-image(stacked-bars-roughall.colors(more-colors)) satisfies is-image
  render-image(stacked-bars-roughsome.colors(less-colors)) satisfies is-image
end

########################
# SORTING METHOD TESTS 
########################

#### CMP Functions ####
ascending-cmp = {(a, b): a < b}
descending-cmp = {(a, b): a > b}
descending-str-len = {(a, b): string-length(a) > string-length(b)}
ascending-even-priority = 
  {(a, b):
    ask: 
      | num-modulo(a, 2) == num-modulo(b, 2) then: a < b
      | num-modulo(a, 2) == 0 then: true 
      | num-modulo(b, 2) == 0 then: false
    end}
ascending-vowels = {(a, b): count-vowels(a) < count-vowels(b)}

#### EQ Functions ####
vanilla-eq = {(a, b): a == b}
str-len-eq = {(a, b): string-length(a) == string-length(b)}
vowel-eq = {(a, b): count-vowels(a) == count-vowels(b)}

#### Scoring Functions ####
sum = {(l): fold({(acc, elm): acc + elm}, 0, l)}
get-first = {(l): l.get(0)}
pos-only-sum = {(l): sum(filter({(elm): elm > 0}, l))}
priority-scoring = 
  {(coeff, l): fold2({(acc, e1, e2): acc + (e1 * e2)}, 0, coeff, l)}
weekend-weekday-scoring = 
  {(l): priority-scoring([list: 1, 1, 1, 1, 1, -1, -1], l)}

check "Sorting Methods: Single Bars":
  render-image(single-bars.sort-by(ascending-cmp, vanilla-eq)) satisfies is-image
  render-image(single-bars.sort-by(descending-cmp, vanilla-eq)) satisfies is-image
  render-image(single-bars.sort-by(ascending-even-priority, vanilla-eq)) satisfies is-image
  render-image(single-bars-neg.sort-by(descending-cmp, vanilla-eq)) satisfies is-image
  render-image(single-bars-rep.sort-by(ascending-cmp, vanilla-eq)) satisfies is-image
  render-image(single-bars-roughall.sort-by(ascending-cmp, vanilla-eq)) satisfies is-image
  render-image(single-bars-roughsome.sort-by(ascending-cmp, vanilla-eq)) 
    satisfies is-image

  render-image(single-bars.sort-by-label(ascending-cmp, vanilla-eq)) satisfies is-image
  render-image(single-bars.sort-by-label(descending-cmp, vanilla-eq)) satisfies is-image
  render-image(single-bars.sort-by-label(descending-str-len, str-len-eq)) satisfies is-image
  render-image(single-bars.sort-by-label(ascending-vowels, vowel-eq)) satisfies is-image
  render-image(single-bars-neg.sort-by-label(descending-str-len, str-len-eq)) satisfies is-image
  render-image(single-bars-rep.sort-by-label(ascending-vowels, vowel-eq)) satisfies is-image
  render-image(single-bars-roughall.sort-by-label(descending-cmp, vanilla-eq)) 
    satisfies is-image
  render-image(single-bars-roughsome.sort-by-label(ascending-vowels, vowel-eq)) 
    satisfies is-image 
end

check "Sorting Methods: Grouped Bars":
  render-image(grouped-bars.sort-by(ascending-cmp, vanilla-eq)) satisfies is-image
  render-image(grouped-bars.sort-by(descending-cmp, vanilla-eq)) satisfies is-image
  render-image(grouped-bars.sort-by(ascending-even-priority, vanilla-eq)) satisfies is-image
  render-image(grouped-bars-neg.sort-by(ascending-cmp, vanilla-eq)) satisfies is-image
  render-image(grouped-bars-rep.sort-by(ascending-even-priority, vanilla-eq)) satisfies is-image
  render-image(grouped-bars-repgroups.sort-by(descending-cmp, vanilla-eq)) satisfies is-image
  render-image(grouped-bars-roughall.sort-by(ascending-cmp, vanilla-eq)) satisfies is-image
  render-image(grouped-bars-roughsome.sort-by(ascending-even-priority, vanilla-eq)) 
    satisfies is-image

  render-image(grouped-bars.sort-by-label(ascending-cmp, vanilla-eq)) satisfies is-image
  render-image(grouped-bars.sort-by-label(descending-cmp, vanilla-eq)) satisfies is-image
  render-image(grouped-bars.sort-by-label(ascending-vowels, vowel-eq)) satisfies is-image
  render-image(grouped-bars-neg.sort-by-label(ascending-vowels, vowel-eq)) satisfies is-image
  render-image(grouped-bars-rep.sort-by-label(descending-cmp, vanilla-eq)) satisfies is-image
  render-image(grouped-bars-repgroups.sort-by-label(ascending-cmp, vanilla-eq)) 
    satisfies is-image
  render-image(grouped-bars-roughall.sort-by-label(descending-cmp, vanilla-eq)) 
    satisfies is-image
  render-image(grouped-bars-roughsome.sort-by-label(ascending-vowels, vowel-eq)) 
    satisfies is-image

  render-image(grouped-bars.sort-by-data(sum, ascending-cmp, vanilla-eq)) satisfies is-image
  render-image(grouped-bars.sort-by-data(sum, descending-cmp, vanilla-eq)) 
    satisfies is-image
  render-image(grouped-bars.sort-by-data(sum, ascending-even-priority, vanilla-eq)) 
    satisfies is-image
  render-image(grouped-bars.sort-by-data(get-first, descending-cmp, vanilla-eq)) 
    satisfies is-image
  render-image(grouped-bars-neg.sort-by-data(pos-only-sum, ascending-cmp, vanilla-eq)) 
    satisfies is-image
  render-image(grouped-bars-repgroups.sort-by-data(sum, ascending-even-priority, vanilla-eq)) 
    satisfies is-image
  render-image(
    grouped-bars-rep.sort-by-data(weekend-weekday-scoring, ascending-cmp, vanilla-eq)) 
    satisfies is-image
  render-image(grouped-bars-roughall.sort-by-data(get-first, descending-cmp, vanilla-eq)) 
    satisfies is-image
  render-image(grouped-bars-roughsome.sort-by-data(get-first, ascending-cmp, vanilla-eq)) 
    satisfies is-image
end

check "Sorting Methods: Stacked Bars":
  render-image(stacked-bars.sort-by(ascending-cmp, vanilla-eq)) satisfies is-image
  render-image(stacked-bars.sort-by(descending-cmp, vanilla-eq)) satisfies is-image
  render-image(stacked-bars.sort-by(ascending-even-priority, vanilla-eq)) satisfies is-image
  render-image(stacked-bars-neg.sort-by(ascending-cmp, vanilla-eq)) satisfies is-image
  render-image(stacked-bars-rep.sort-by(ascending-even-priority, vanilla-eq)) satisfies is-image
  render-image(stacked-bars-repstacks.sort-by(descending-cmp, vanilla-eq)) satisfies is-image
  render-image(stacked-bars-roughall.sort-by(ascending-cmp, vanilla-eq)) satisfies is-image
  render-image(stacked-bars-roughsome.sort-by(ascending-even-priority, vanilla-eq)) 
    satisfies is-image

  render-image(stacked-bars.sort-by-label(ascending-cmp, vanilla-eq)) satisfies is-image
  render-image(stacked-bars.sort-by-label(descending-cmp, vanilla-eq)) satisfies is-image
  render-image(stacked-bars.sort-by-label(ascending-vowels, vowel-eq)) satisfies is-image
  render-image(stacked-bars-neg.sort-by-label(ascending-vowels, vowel-eq)) satisfies is-image
  render-image(stacked-bars-rep.sort-by-label(descending-cmp, vanilla-eq)) satisfies is-image
  render-image(stacked-bars-repstacks.sort-by-label(ascending-cmp, vanilla-eq)) 
    satisfies is-image
  render-image(stacked-bars-roughall.sort-by-label(descending-cmp, vanilla-eq)) 
    satisfies is-image
  render-image(stacked-bars-roughsome.sort-by-label(ascending-vowels, vowel-eq)) 
    satisfies is-image

  render-image(stacked-bars.sort-by-data(sum, ascending-cmp, vanilla-eq)) satisfies is-image
  render-image(stacked-bars.sort-by-data(sum, descending-cmp, vanilla-eq)) satisfies is-image
  render-image(stacked-bars.sort-by-data(sum, ascending-even-priority, vanilla-eq)) 
    satisfies is-image
  render-image(stacked-bars.sort-by-data(get-first, descending-cmp, vanilla-eq)) 
    satisfies is-image
  render-image(stacked-bars-neg.sort-by-data(pos-only-sum, ascending-cmp, vanilla-eq)) 
    satisfies is-image
  render-image(stacked-bars-repstacks.sort-by-data(sum, ascending-even-priority, vanilla-eq)) 
    satisfies is-image
  render-image(
    stacked-bars-rep.sort-by-data(weekend-weekday-scoring, ascending-cmp, vanilla-eq)) 
    satisfies is-image
  render-image(stacked-bars-roughall.sort-by-data(get-first, descending-cmp, vanilla-eq)) 
    satisfies is-image
  render-image(stacked-bars-roughsome.sort-by-data(get-first, ascending-cmp, vanilla-eq)) 
    satisfies is-image
end

#########################
# POINTER METHODS TESTS 
#########################

check "Pointer Method: Single Bars": 
  render-image(single-bars.add-pointers(empty, empty)) satisfies is-image
  render-image(single-bars.add-pointers([list: 6, 7], [list: "median", "mean + 1"])) 
    satisfies is-image
  render-image(single-bars.add-pointers(
    [list: -10000000, 900000000], 
    [list: "to-far-below", "to-far-above"])) # Wont show up
    satisfies is-image
  render-image(single-bars.add-pointers([list: 0, 4, 10], [list: "zero", "four", "max"]))
    satisfies is-image
  render-image(single-bars.add-pointers([list: 4], [list: "thislabelnameiswaytoolong"]))
    satisfies is-image
  render-image(single-bars-neg.add-pointers([list: 3, 1, -2, -5], [list: "a", "b", "c", "d"]))
    satisfies is-image
  render-image(single-bars-neg.add-pointers([list: 1.546, -2.213], [list: 'decimal', 'negdec']))
    satisfies is-image
  render-image(single-bars-rep.add-pointers([list: 3, 5], [list: 'tres', 'cinco']))
    satisfies is-image
  render-image(single-bars-roughall.add-pointers([list: 11, 1, -5], [list: "a", "c", "d"]))
    satisfies is-image
  render-image(single-bars-roughsome.add-pointers([list: 40, 5], [list: 'cuarenta', 'cinco']))
    satisfies is-image

  render-image(
    single-bars.add-pointers([list: 6, 7], [list: "median", "mean + 1"])
               .pointer-color(C.red))
    satisfies is-image
  render-image(
    single-bars-neg.add-pointers([list: 3, 1, -2, -5], [list: "a", "b", "c", "d"])
                   .pointer-color(C.green))
    satisfies is-image
  render-image(
    single-bars-rep.add-pointers([list: 3, 5], [list: 'tres', 'cinco'])
                   .pointer-color(C.cyan))
    satisfies is-image
  render-image(
    single-bars-roughall.add-pointers([list: 11, 1, -5], [list: "a", "c", "d"])
                        .pointer-color(C.magenta))
    satisfies is-image
  render-image(
    single-bars-roughsome.add-pointers([list: 40, 5], [list: 'cuarenta', 'cinco'])
                         .pointer-color(C.orange))
    satisfies is-image

  render-image(single-bars.add-pointers(empty, [list: "base"]))
    raises "pointers values and names should have the same length"
  render-image(single-bars.add-pointers([list: 0], empty))
    raises "pointers values and names should have the same length"
  render-image(single-bars.add-pointers([list: 0], [list: "base", "target"]))
    raises "pointers values and names should have the same length"
  render-image(single-bars.add-pointers([list: 0, 1], [list: "base"]))
    raises "pointers values and names should have the same length"
  render-image(single-bars-neg.add-pointers([list: 0], [list: "base", "target"]))
    raises "pointers values and names should have the same length"
  render-image(single-bars-rep.add-pointers([list: 0, 1], [list: "base"]))
    raises "pointers values and names should have the same length"
  render-image(single-bars-roughall.add-pointers(empty, [list: "base"]))
    raises "pointers values and names should have the same length"
  render-image(single-bars-roughsome.add-pointers([list: 0], empty))
    raises "pointers values and names should have the same length"

  render-image(single-bars.add-pointers([list: 0, 0], [list: "dup", "duplicate"]))
    raises "pointers cannot overlap"
  render-image(single-bars-neg.add-pointers([list: 0, 0], [list: "dup", "duplicate"]))
    raises "pointers cannot overlap"
  render-image(single-bars-rep.add-pointers([list: 0, 0], [list: "dup", "duplicate"]))
    raises "pointers cannot overlap"
  render-image(single-bars-roughall.add-pointers([list: 0, 0], [list: "dup", "duplicate"]))
    raises "pointers cannot overlap"
  render-image(single-bars-roughsome.add-pointers([list: 0, 0], [list: "dup", "duplicate"]))
    raises "pointers cannot overlap"
end

check "Pointer Methods: Grouped Bars": 
  render-image(grouped-bars.add-pointers(empty, empty)) satisfies is-image
  render-image(grouped-bars.add-pointers(
    [list: 1874094, 41417373 / 14], 
    [list: "median (All Bars)", "mean (All Bars)"])) 
    satisfies is-image
  render-image(grouped-bars.add-pointers(
    [list: -10000000, 900000000], 
    [list: "to-far-below", "to-far-above"])) # Wont show up
    satisfies is-image
  render-image(grouped-bars.add-pointers(
    [list: 0, 6000000, 12000000], 
    [list: "zero", "middle", "max"]))
    satisfies is-image
  render-image(grouped-bars.add-pointers(
    [list: 6000000], 
    [list: "thislabelnameiswaytoolong"]))
    satisfies is-image
  render-image(grouped-bars-neg.add-pointers(
    [list: 0.3, 0, -1.5, -3], 
    [list: "a", "b", "c", "d"]))
    satisfies is-image
  render-image(grouped-bars-rep.add-pointers(
    [list: 3.5, 9], 
    [list: "Decimal", "Almost Max"]))
    satisfies is-image
  render-image(grouped-bars-repgroups.add-pointers(
    [list: 6, 9], 
    [list: "Almost Middle", "Almost Max"]))
    satisfies is-image
  render-image(grouped-bars-roughall.add-pointers([list: 8, 1, 4], [list: "a", "c", "d"]))
    satisfies is-image
  render-image(grouped-bars-roughsome.add-pointers([list: 3, 5], [list: 'tres', 'cinco']))
    satisfies is-image

  render-image(
    grouped-bars.add-pointers([list: 1874094, 41417373 / 14], 
                              [list: "median (All Bars)", "mean (All Bars)"])
               .pointer-color(C.red))
    satisfies is-image
  render-image(
    grouped-bars-neg.add-pointers([list: 0.3, 0, -1.5, -3], 
                                  [list: "a", "b", "c", "d"])
                   .pointer-color(C.magenta))
    satisfies is-image
  render-image(
    grouped-bars-rep.add-pointers([list: 3.5, 9], [list: "Decimal", "Almost Max"])
                   .pointer-color(C.yellow))
    satisfies is-image
  render-image(
    grouped-bars-repgroups.add-pointers([list: 6, 9], [list: "~Mid", "~Max"])
                          .pointer-color(C.orange))
    satisfies is-image
  render-image(
    grouped-bars-roughall.add-pointers([list: 11, 1, -5], [list: "a", "c", "d"])
                        .pointer-color(C.red))
    satisfies is-image
  render-image(
    grouped-bars-roughsome.add-pointers([list: 3, 5], [list: 'tres', 'cinco'])
                         .pointer-color(C.cyan))
    satisfies is-image

  render-image(grouped-bars.add-pointers(empty, [list: "base"]))
    raises "pointers values and names should have the same length"
  render-image(grouped-bars.add-pointers([list: 0], empty))
    raises "pointers values and names should have the same length"
  render-image(grouped-bars.add-pointers([list: 0], [list: "base", "target"]))
    raises "pointers values and names should have the same length"
  render-image(grouped-bars.add-pointers([list: 0, 1], [list: "base"]))
    raises "pointers values and names should have the same length"
  render-image(grouped-bars-neg.add-pointers([list: 0], [list: "base", "target"]))
    raises "pointers values and names should have the same length"
  render-image(grouped-bars-rep.add-pointers([list: 0, 1], [list: "base"]))
    raises "pointers values and names should have the same length"
  render-image(grouped-bars-repgroups.add-pointers(empty, [list: "base"]))
    raises "pointers values and names should have the same length"
  render-image(grouped-bars-roughall.add-pointers(empty, [list: "base"]))
    raises "pointers values and names should have the same length"
  render-image(grouped-bars-roughsome.add-pointers([list: 0], empty))
    raises "pointers values and names should have the same length"

  render-image(grouped-bars.add-pointers([list: 0, 0], [list: "dup", "duplicate"]))
    raises "pointers cannot overlap"
  render-image(grouped-bars-neg.add-pointers([list: 0, 0], [list: "dup", "duplicate"]))
    raises "pointers cannot overlap"
  render-image(grouped-bars-rep.add-pointers([list: 0, 0], [list: "dup", "duplicate"]))
    raises "pointers cannot overlap"
  render-image(grouped-bars-repgroups.add-pointers([list: 0, 0], [list: "dup", "duplicate"]))
    raises "pointers cannot overlap"
  render-image(grouped-bars-roughall.add-pointers([list: 0, 0], [list: "dup", "duplicate"]))
    raises "pointers cannot overlap"
  render-image(grouped-bars-roughsome.add-pointers([list: 0, 0], [list: "dup", "duplicate"]))
    raises "pointers cannot overlap"
end

check "Pointers Methods: Stacked Bars": 
  render-image(stacked-bars.add-pointers(empty, empty)) satisfies is-image
  render-image(stacked-bars.add-pointers(
    [list: 18409317.5, 20708686.5],
    [list: "median", "mean"])) 
    satisfies is-image
  render-image(stacked-bars.add-pointers(
    [list: -10000000, 900000000], 
    [list: "to-far-below", "to-far-above"])) # Wont show up
    satisfies is-image
  render-image(stacked-bars.add-pointers(
    [list: 0, 20000000, 40000000], 
    [list: "zero", "middle", "max"]))
    satisfies is-image
  render-image(stacked-bars.add-pointers(
    [list: 20000000], 
    [list: "thislabelnameiswaytoolong"]))
    satisfies is-image
  render-image(stacked-bars-neg.add-pointers(
    [list: 1.3, 0, -1.5, -3], 
    [list: "a", "b", "c", "d"]))
    satisfies is-image
  render-image(stacked-bars-rep.add-pointers(
    [list: 3.5, 59], 
    [list: "Decimal", "Almost Max"]))
    satisfies is-image
  render-image(stacked-bars-repstacks.add-pointers(
    [list: 31, 59], 
    [list: "Almost Middle", "Almost Max"]))
    satisfies is-image
  render-image(stacked-bars-roughall.add-pointers([list: 8, 1, 4], [list: "a", "c", "d"]))
    satisfies is-image
  render-image(stacked-bars-roughsome.add-pointers([list: 3, 5], [list: 'tres', 'cinco']))
    satisfies is-image

  render-image(
    stacked-bars.add-pointers([list: 18409317.5, 20708686.5], [list: "median", "mean"])
                .pointer-color(C.red))
    satisfies is-image
  render-image(
    stacked-bars-neg.add-pointers([list: 1.3, 0, -1.5, -3], [list: "a", "b", "c", "d"])
                    .pointer-color(C.magenta))
    satisfies is-image
  render-image(
    stacked-bars-rep.add-pointers([list: 3.5, 59], [list: "Decimal", "Almost Max"])
                    .pointer-color(C.yellow))
    satisfies is-image
  render-image(
    stacked-bars-repstacks.add-pointers([list: 31, 59], [list: "Almost Middle", "Almost Max"])
                          .pointer-color(C.orange))
    satisfies is-image
  render-image(
    stacked-bars-roughall.add-pointers([list: 11, 1, -5], [list: "a", "c", "d"])
                        .pointer-color(C.red))
    satisfies is-image
  render-image(
    stacked-bars-roughsome.add-pointers([list: 3, 5], [list: 'tres', 'cinco'])
                         .pointer-color(C.cyan))
    satisfies is-image

  render-image(stacked-bars.add-pointers(empty, [list: "base"]))
    raises "pointers values and names should have the same length"
  render-image(stacked-bars.add-pointers([list: 0], empty))
    raises "pointers values and names should have the same length"
  render-image(stacked-bars.add-pointers([list: 0], [list: "base", "target"]))
    raises "pointers values and names should have the same length"
  render-image(stacked-bars.add-pointers([list: 0, 1], [list: "base"]))
    raises "pointers values and names should have the same length"
  render-image(stacked-bars-neg.add-pointers([list: 0], [list: "base", "target"]))
    raises "pointers values and names should have the same length"
  render-image(stacked-bars-rep.add-pointers([list: 0, 1], [list: "base"]))
    raises "pointers values and names should have the same length"
  render-image(stacked-bars-repstacks.add-pointers(empty, [list: "base"]))
    raises "pointers values and names should have the same length"
  render-image(stacked-bars-roughall.add-pointers(empty, [list: "base"]))
    raises "pointers values and names should have the same length"
  render-image(stacked-bars-roughsome.add-pointers([list: 0], empty))
    raises "pointers values and names should have the same length"

  render-image(stacked-bars.add-pointers([list: 0, 0], [list: "dup", "duplicate"]))
    raises "pointers cannot overlap"
  render-image(stacked-bars-neg.add-pointers([list: 0, 0], [list: "dup", "duplicate"]))
    raises "pointers cannot overlap"
  render-image(stacked-bars-rep.add-pointers([list: 0, 0], [list: "dup", "duplicate"]))
    raises "pointers cannot overlap"
  render-image(stacked-bars-repstacks.add-pointers([list: 0, 0], [list: "dup", "duplicate"]))
    raises "pointers cannot overlap"
  render-image(stacked-bars-roughall.add-pointers([list: 0, 0], [list: "dup", "duplicate"]))
    raises "pointers cannot overlap"
  render-image(stacked-bars-roughsome.add-pointers([list: 0, 0], [list: "dup", "duplicate"]))
    raises "pointers cannot overlap"
end

################################
# AXIS FORMATTING METHOD TESTS 
################################

check "Axis Formatting Methods: Single Bars":
  render-image(single-bars.format-axis({(n): num-to-string(n)})) satisfies is-image
  render-image(single-bars.format-axis({(n): num-to-string(n) + " votes"})) satisfies is-image
  render-image(single-bars.format-axis({(n): "Counted " + num-to-string(n) + " votes"}))
    satisfies is-image
  render-image(single-bars.format-axis({(n): num-to-string(n / 10) + " * 10"})) 
    satisfies is-image
  render-image(single-bars.format-axis({(_): "?"})) 
    satisfies is-image
  render-image(single-bars-neg.format-axis({(n): num-to-string(n / 10) + " * 10"})) 
    satisfies is-image
  render-image(single-bars-rep.format-axis({(n): num-to-string(n) + " votes"})) 
    satisfies is-image
  render-image(single-bars-roughall.format-axis({(n): num-to-string(n / 10) + " * 10"})) 
    satisfies is-image
  render-image(single-bars-roughsome.format-axis({(n): num-to-string(n / 10) + " * 10"})) 
    satisfies is-image
end

check "Axis Formatting Methods: Grouped Bars":
  render-image(grouped-bars.format-axis({(n): num-to-string(n)})) satisfies is-image
  render-image(grouped-bars.format-axis({(n): num-to-string(n) + " people"})) satisfies is-image
  render-image(grouped-bars.format-axis({(n): "For " + num-to-string(n) + " people"}))
    satisfies is-image
  render-image(grouped-bars.format-axis({(n): num-to-string(n / 10) + " * 10"})) 
    satisfies is-image
  render-image(grouped-bars.format-axis({(_): "?"})) 
    satisfies is-image
  render-image(grouped-bars-neg.format-axis({(n): num-to-string-digits(n, 2) + " Δ Fertility Rate"})) 
    satisfies is-image
  render-image(grouped-bars-rep.format-axis({(n): num-to-string(n) + " hours"})) 
    satisfies is-image
  render-image(grouped-bars-repgroups.format-axis({(n):"For " + num-to-string(n) + " hours"})) 
    satisfies is-image
  render-image(grouped-bars-roughall.format-axis({(n): num-to-string(n / 2) + " * 2"})) 
    satisfies is-image
  render-image(grouped-bars-roughsome.format-axis({(n): num-to-string(n / 2) + " * 2"})) 
    satisfies is-image
end

check "Axis Formatting Methods: Stacked Bars":
  render-image(stacked-bars.format-axis({(n): num-to-string(n)})) satisfies is-image
  render-image(stacked-bars.format-axis({(n): num-to-string(n) + " people"})) satisfies is-image
  render-image(stacked-bars.format-axis({(n): "For " + num-to-string(n) + " people"}))
    satisfies is-image
  render-image(stacked-bars.format-axis({(n): num-to-string(n / 10) + " * 10"})) 
    satisfies is-image
  render-image(stacked-bars.format-axis({(_): "?"})) 
    satisfies is-image
  render-image(stacked-bars-neg.format-axis({(n): num-to-string-digits(n, 2) + " Δ Fertility Rate"})) 
    satisfies is-image
  render-image(stacked-bars-rep.format-axis({(n): num-to-string(n) + " hours"})) 
    satisfies is-image
  render-image(stacked-bars-repstacks.format-axis({(n):"For " + num-to-string(n) + " hours"})) 
    satisfies is-image
  render-image(stacked-bars-roughall.format-axis({(n): num-to-string(n / 5) + " * 5"})) 
    satisfies is-image
  render-image(stacked-bars-roughsome.format-axis({(n): num-to-string(n / 5) + " * 5"})) 
    satisfies is-image
end

######################
# SCALE METHOD TESTS 
######################

check "Scale Methods: Single Bars":
  render-image(single-bars.scale({(n): n})) satisfies is-image
  render-image(single-bars.scale({(n): n + 5})) satisfies is-image
  render-image(single-bars.scale({(n): 2 * n})) satisfies is-image
  render-image(single-bars.scale({(n): n / 100})) satisfies is-image
  render-image(single-bars.scale(num-log)) satisfies is-image
  render-image(single-bars.scale({(n): num-expt(n, n)}).scale(num-log)) satisfies is-image
  render-image(single-bars-neg.scale({(n): (2 * n) + 231})) satisfies is-image
  render-image(single-bars-rep.scale({(n): n * n})) satisfies is-image
  render-image(single-bars-roughall.scale({(n): n * n})) satisfies is-image
  render-image(single-bars-roughsome.scale({(n): 2 * n})) satisfies is-image
end

check "Scale Methods: Grouped Bars":
  render-image(grouped-bars.scale({(n): n})) satisfies is-image
  render-image(grouped-bars.scale({(n): n + 5})) satisfies is-image
  render-image(grouped-bars.scale({(n): 2 * n})) satisfies is-image
  render-image(grouped-bars.scale({(n): n / 100})) satisfies is-image
  render-image(grouped-bars.scale(num-log)) satisfies is-image
  render-image(grouped-bars.scale({(n): num-expt(n, 1.5)}).scale(num-log)) satisfies is-image
  render-image(grouped-bars-neg.scale({(n): (2 * n) + 231})) satisfies is-image
  render-image(grouped-bars-rep.scale({(n): n * 1.5})) satisfies is-image
  render-image(grouped-bars-repgroups.scale({(n): num-expt(n, 1.5)})) satisfies is-image
  render-image(grouped-bars-roughall.scale({(n): n * n})) satisfies is-image
  render-image(grouped-bars-roughsome.scale({(n): 2 * n})) satisfies is-image
end

check "Scale Methods: Stacked Bars":
  render-image(stacked-bars.scale({(n): n})) satisfies is-image
  render-image(stacked-bars.scale({(n): n + 5})) satisfies is-image
  render-image(stacked-bars.scale({(n): 2 * n})) satisfies is-image
  render-image(stacked-bars.scale({(n): n / 100})) satisfies is-image
  render-image(stacked-bars.scale(num-log)) satisfies is-image
  render-image(stacked-bars.scale({(n): (2 * n) + 231}).scale({(n): n / 2})) satisfies is-image
  render-image(stacked-bars-neg.scale({(n): (2 * n) + 231})) satisfies is-image
  render-image(stacked-bars-rep.scale({(n): n * n})) satisfies is-image
  render-image(stacked-bars-repstacks.scale({(n): n / 100})) satisfies is-image
  render-image(stacked-bars-roughall.scale({(n): n * n})) satisfies is-image
  render-image(stacked-bars-roughsome.scale({(n): 2 * n})) satisfies is-image
end

##############################
# STACKING TYPE METHOD TESTS 
##############################

check "Stacking type: Grouped Bars":
  # Keep as grouped bars 
  render-image(grouped-bars.stacking-type(grouped)) satisfies is-image
  render-image(grouped-bars-neg.stacking-type(grouped)) satisfies is-image
  render-image(grouped-bars-rep.stacking-type(grouped)) satisfies is-image
  render-image(grouped-bars-repgroups.stacking-type(grouped)) satisfies is-image
  render-image(grouped-bars-roughall.stacking-type(grouped)) satisfies is-image
  render-image(grouped-bars-roughsome.stacking-type(grouped)) satisfies is-image

  # Switch to stacked bars 
  render-image(grouped-bars.stacking-type(absolute)) satisfies is-image
  render-image(grouped-bars.stacking-type(relative)) satisfies is-image
  render-image(grouped-bars.stacking-type(percent)) satisfies is-image
  render-image(grouped-bars-neg.stacking-type(relative)) satisfies is-image
  render-image(grouped-bars-rep.stacking-type(percent)) satisfies is-image
  render-image(grouped-bars-repgroups.stacking-type(relative)) satisfies is-image
  render-image(grouped-bars-roughall.stacking-type(absolute)) satisfies is-image
  render-image(grouped-bars-roughsome.stacking-type(percent)) satisfies is-image
end

check "Stacking type: stacked Bars":
  # Switch to grouped bars 
  render-image(stacked-bars.stacking-type(grouped)) satisfies is-image
  render-image(stacked-bars-neg.stacking-type(grouped)) satisfies is-image
  render-image(stacked-bars-rep.stacking-type(grouped)) satisfies is-image
  render-image(stacked-bars-repstacks.stacking-type(grouped)) satisfies is-image
  render-image(stacked-bars-roughall.stacking-type(grouped)) satisfies is-image
  render-image(stacked-bars-roughsome.stacking-type(grouped)) satisfies is-image

  # Switch between stacked bar types 
  render-image(stacked-bars.stacking-type(absolute)) satisfies is-image
  render-image(stacked-bars.stacking-type(relative)) satisfies is-image
  render-image(stacked-bars.stacking-type(percent)) satisfies is-image
  render-image(stacked-bars-neg.stacking-type(relative)) satisfies is-image
  render-image(stacked-bars-rep.stacking-type(percent)) satisfies is-image
  render-image(stacked-bars-repstacks.stacking-type(relative)) satisfies is-image
  render-image(stacked-bars-roughall.stacking-type(absolute)) satisfies is-image
  render-image(stacked-bars-roughsome.stacking-type(percent)) satisfies is-image
end

###########################
# HORIZONTAL METHOD TESTS 
###########################

check "Horizontal Method: Single Bars":
  render-image(single-bars.horizontal(false)) satisfies is-image
  render-image(single-bars-neg.horizontal(false)) satisfies is-image
  render-image(single-bars-rep.horizontal(false)) satisfies is-image
  render-image(single-bars-roughall.horizontal(false)) satisfies is-image
  render-image(single-bars-roughsome.horizontal(false)) satisfies is-image
  render-image(single-bars.horizontal(true)) satisfies is-image
  render-image(single-bars-neg.horizontal(true)) satisfies is-image
  render-image(single-bars-rep.horizontal(true)) satisfies is-image
  render-image(single-bars-roughall.horizontal(true)) satisfies is-image
  render-image(single-bars-roughsome.horizontal(true)) satisfies is-image
end

check "Rendering: Grouped Bars": 
  render-image(grouped-bars.horizontal(false)) satisfies is-image
  render-image(grouped-bars-neg.horizontal(false)) satisfies is-image
  render-image(grouped-bars-rep.horizontal(false)) satisfies is-image
  render-image(grouped-bars-repgroups.horizontal(false)) satisfies is-image
  render-image(grouped-bars-roughall.horizontal(false)) satisfies is-image
  render-image(grouped-bars-roughsome.horizontal(false)) satisfies is-image
  render-image(grouped-bars.horizontal(true)) satisfies is-image
  render-image(grouped-bars-neg.horizontal(true)) satisfies is-image
  render-image(grouped-bars-rep.horizontal(true)) satisfies is-image
  render-image(grouped-bars-repgroups.horizontal(true)) satisfies is-image
  render-image(grouped-bars-roughall.horizontal(true)) satisfies is-image
  render-image(grouped-bars-roughsome.horizontal(true)) satisfies is-image
end

check "Rendering: Stacked Bars": 
  render-image(stacked-bars.horizontal(false)) satisfies is-image
  render-image(stacked-bars-neg.horizontal(false)) satisfies is-image
  render-image(stacked-bars-rep.horizontal(false)) satisfies is-image
  render-image(stacked-bars-repstacks.horizontal(false)) satisfies is-image
  render-image(stacked-bars-roughall.horizontal(false)) satisfies is-image
  render-image(stacked-bars-roughsome.horizontal(false)) satisfies is-image
  render-image(stacked-bars.horizontal(true)) satisfies is-image
  render-image(stacked-bars-neg.horizontal(true)) satisfies is-image
  render-image(stacked-bars-rep.horizontal(true)) satisfies is-image
  render-image(stacked-bars-repstacks.horizontal(true)) satisfies is-image
  render-image(stacked-bars-roughall.horizontal(true)) satisfies is-image
  render-image(stacked-bars-roughsome.horizontal(true)) satisfies is-image
end

#####################
# ANNOTATIONS TESTS
#####################

check "Annotations: Single Bars":
  render-image(single-bars.annotations([list: some("P"), some("O"), some("C"),
      some("CPP"), some("P"), some("R"), some("SM")]))
    satisfies is-image
end

check "Annotations: Multiple Bars":
  multi-annotations = [list: 
        [list: some('0.82%'), some('1.37%'), some('0.66%'), some('1.17%'),
          some('3.23%'), some('2.69%'), some('1.25%')], 
        [list: some('0.62%'), some('1.00%'), some('0.43%'), some('0.75%'), 
          some('2.14%'), some('1.72%'), some('0.75%')],
        [list: some('0.37%'), some('0.65%'), some('0.32%'), some('0.61%'), 
          some('1.63%'), some('1.56%'), some('0.79%')],
        [list: some('0.35%'), some('0.59%'), some('0.28%'), some('0.49%'), 
          some('1.46%'), some('1.45%'), some('0.97%')],
        [list: some('0.27%'), some('0.47%'), some('0.22%'), some('0.40%'), 
          some('1.10%'), some('0.99%'), some('0.48%')],
        [list: some('0.22%'), some('0.41%'), some('0.21%'), some('0.37%'), 
          some('0.96%'), some('1.04%'), some('0.58%')]]
  render-image(grouped-bars.annotations(multi-annotations))
    satisfies is-image
  render-image(stacked-bars.annotations(multi-annotations))
    satisfies is-image
end

###################
# INTERVALS TESTS
###################

check "Intervals: Single Bars":
  render-image(single-bars.intervals([list: [list: 9, 11],
      [list: 1, 2, 3, 4, 5], [list: -1, -2], empty, empty, empty, empty]))
    satisfies is-image
  render-image(single-bars.intervals([list: [list: 9, 11],
      [list: 1, 2, 3, 4, 5], [list: -1, -2], empty, empty, empty, empty])
      .interval-color(C.orange))
    satisfies is-image
end

check "Intervals: Multiple Bars":
  grouped-small-data = from-list.grouped-bar-chart(
      [list: "Year 1", "Year 2"],
      [list:
        [list: 50, 20, 10],
        [list: 20, 40, 10]],
      [list: "Mail", "Phone", "Fax"])
  stacked-small-data = from-list.stacked-bar-chart(
      [list: "Year 1", "Year 2"],
      [list:
        [list: 50, 20, 10],
        [list: 20, 40, 10]],
      [list: "Mail", "Phone", "Fax"])
  intervals = [list:
    [list: [list: 45, 55], [list: 15, 17, 23, 24], [list: ]],
    [list: [list: 25],     [list: ],               [list: ]]]
  
  render-image(stacked-small-data.intervals(intervals)) satisfies is-image
  render-image(grouped-small-data.intervals(intervals)) satisfies is-image

  render-image(stacked-small-data.intervals(intervals).interval-color(C.red))
    satisfies is-image
  render-image(grouped-small-data.intervals(intervals).interval-color(C.green))
    satisfies is-image
end

###################
# ERROR BAR TESTS
###################

check "Error bars: Single Bars":
  render-image(single-bars.error-bars([list: [list: -1, 1], [list: -1, 1],
      [list: -1, 2], [list: -1, 1], [list: -1, 1], [list: -1, 1],
      [list: -1, 1]]))
    satisfies is-image
  render-image(single-bars.error-bars([list: [list: -1, 1], [list: -1, 1],
      [list: -1, 2], [list: -1, 1], [list: -1, 1], [list: -1, 1],
      [list: -1, 1]]).interval-color(C.purple))
    satisfies is-image
end

check "Error bars: Multiple Bars":
  grouped-small-data = from-list.grouped-bar-chart(
      [list: "Year 1", "Year 2"],
      [list:
        [list: 50, 20, 10],
        [list: 20, 40, 10]],
      [list: "Mail", "Phone", "Fax"])
  stacked-small-data = from-list.stacked-bar-chart(
      [list: "Year 1", "Year 2"],
      [list:
        [list: 50, 20, 10],
        [list: 20, 40, 10]],
      [list: "Mail", "Phone", "Fax"])
  error-amounts = [list:
    [list: [list: -5, 2], [list: -4, 2], [list: -3, 2]],
    [list: [list: -3, 6], [list: -1, 4], [list: -5, 5]]]
  
  render-image(stacked-small-data.error-bars(error-amounts)) satisfies is-image
  render-image(grouped-small-data.error-bars(error-amounts)) satisfies is-image

  render-image(stacked-small-data.error-bars(error-amounts).interval-color(C.red))
    satisfies is-image
  render-image(grouped-small-data.error-bars(error-amounts).interval-color(C.green))
    satisfies is-image
end
