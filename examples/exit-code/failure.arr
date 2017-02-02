import error as ERR

# By convention, an exit code not equal to 0 indicates that
# the program failed to run successfully.
exit-code = 1

# check blocks are processed after the program finishes running
# because we will raise an `exit`, no checks will be run
check "this block gets skipped":
  "x" is "y"
end

# This will print "Exited with code 1" and terminate execution
# To terminate execution without such a message, use the `exit-quiet`
# variant instead
raise(ERR.exit(exit-code))
# raise(ERR.exit-quiet(exit-code))

print("This is after the exit, and hence will not print\n")
