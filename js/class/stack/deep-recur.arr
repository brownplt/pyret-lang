#recurs n times
fun recur-n(n):
    if n == 0:
        true
    else:
        recur-n(n - 1)
    end
end

recur-n(1000)
