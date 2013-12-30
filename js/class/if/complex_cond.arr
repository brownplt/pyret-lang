a = 
if (fun (x): true;)(32):
    42
else:
    end

#State
var myVar = 10
b = if (fun():
        myVar := 32
        false
    end)():

    0
else:
    myVar
end
    
{a : a, b : b}
