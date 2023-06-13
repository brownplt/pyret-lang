import iolib as IO

check "happy path":
    IO.wrap-input-test("hello") is "hello"
end

check "executes multiple times, clearing input and closing readline":
    IO.wrap-input-test("hello1") is "hello1"
    IO.wrap-input-test("hello2") is "hello2"
    IO.wrap-input-test("hello3") is "hello3"
    IO.wrap-input-test("hello4") is "hello4"
end

