import iolib as IO

check "accepts user input from stdin":
    var user_input = IO.input("Arggghhh, what's your name matey? ")
    print("Ahoy " + user_input + "!\n")

    user_input is "hello"
end
