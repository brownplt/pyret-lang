a = if false:
    0
else if true:
    1
else:
    0
end

#Make sure our order of if branches correct
b = if true:
    0
else if true:
    2
else:
    0
end

c = if false:
    0
else if false:
    1
else:
    100
end


d = if false:
    0
else if false:
    1
else if true:
    13
else:
    100
end

{a: a, b : b, c : c, d : d}
