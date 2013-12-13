#Calculated fields
fun f(x): 
    x + 1
end

{a : 1}.{b : f(3), c : f(f(5))}
