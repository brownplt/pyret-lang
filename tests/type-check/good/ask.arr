
fun is-two(n :: Number):
  n == 2
end

fun is-three(n :: Number):
  n == 3
end

n = 5

a = ask:
      | is-two(n) then: "hello"
      | is-three(n) then: 5
      | otherwise: true
    end
