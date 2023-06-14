#lang pyret

import iolib as IO

fun get-user-input():
  string-to-number(
    IO.prompt("Guess a number between 1 and 10000: ")
  ).or-else(-1)
end

fun is-guess-correct(secret-num, guess-num):
 block:
    if secret-num == guess-num:
      false
    else:
      block:
        if secret-num < guess-num:
          print(tostring(guess-num) + " is too high!\n")
        else:
          print(tostring(guess-num) + " is too low!\n")
        end

        true
      end
    end
  end
end

fun evaluate-a-turn(secret-num):
  block:
    guess-num = get-user-input()
    if (guess-num < 1) or (guess-num > 100000):
      block:
        print("That's not a number or not within the expected range.\n")
        true
      end
    else:
      is-guess-correct(secret-num, guess-num)
    end
  end
end

fun play-game(secret-num, turns):
  block:
    game-is-still-on = evaluate-a-turn(secret-num)

    if game-is-still-on:
      play-game(secret-num, turns + 1)
    else:
      print("You win! It took you " + tostring(turns) + " turn(s).\n\n")
    end
  end
end

# same number every time
play-game(num-random(100000), 1)
