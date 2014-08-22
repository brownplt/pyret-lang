# Add Strings in both synthesis and checking
william :: String  = "will" + "i" + "am"
william2 = "will" + "i" + "am"
check-william2 :: String = william2

# Add Numbers in both synthesis and checking
add-num :: Number = 40 + 3
add-num-2 = 40 + 3
check-add-num :: Number = add-num-2

# Subtract Numbers in both synthesis and checking
sub-num :: Number = 40 - 3
sub-num-2 = 40 - 3
check-sub-num :: Number = sub-num-2

# Multiply Numbers in both synthesis and checking
mult-num :: Number = 40 * 3
mult-num-2 = 40 * 3
check-mult-num :: Number = mult-num-2

# Divide Numbers in both synthesis and checking
div-num :: Number = 40 / 3
div-num-2 = 40 / 3
check-div-num :: Number = div-num-2

# Compare Strings in both synthesis and checking
comp-string-lt :: Boolean = "a" < "b"
comp-string-lt-2 = "a" < "b"
check-lt-str :: Boolean = comp-string-lt-2

comp-string-gt :: Boolean = "a" > "b"
comp-string-gt-2 = "a" > "b"
check-gt-str :: Boolean = comp-string-gt-2

comp-string-lte :: Boolean = "a" <= "b"
comp-string-lte-2 = "a" <= "b"
check-lte-str :: Boolean = comp-string-lte-2

comp-string-gte :: Boolean = "a" >= "b"
comp-string-gte-2 = "a" >= "b"
check-gte-str :: Boolean = comp-string-gte-2

# Compare Numbers in both synthesis and checking
comp-num-lt :: Boolean = 1 < 2
comp-num-lt-2 = 1 < 2
check-lt-num :: Boolean = comp-num-lt-2

comp-num-gt :: Boolean = 1 > 2
comp-num-gt-2 = 1 > 2
check-gt-num :: Boolean = comp-num-gt-2

comp-num-lte :: Boolean = 1 <= 2
comp-num-lte-2 = 1 <= 2
check-lte-num :: Boolean = comp-num-lte-2

comp-num-gte :: Boolean = 1 >= 2
comp-num-gte-2 = 1 >= 2
check-gte-num :: Boolean = comp-num-gte-2
