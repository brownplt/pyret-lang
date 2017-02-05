import cmdline as C
import file as F
import string-dict as D
import file("lambda-parse.arr") as LP

fun process-file(path) block:
  when not(F.file-exists(path)):
    raise("File does not exist: " + path)
  end
  contents = F.file-to-string(path)
  LP.parse(LP.tokenize(path, contents))
end

fun main(args):
  options = [D.string-dict:]
  params-parsed = C.parse-args(options, args)
  cases(C.ParsedArguments) params-parsed block:
    | success(r, rest) =>
      cases(List) rest block:
        | empty =>
          print("Missing file\n")
          print(C.usage-info(options).join-str("\n"))
          print("\n")
        | link(file, _) =>
          print(process-file(file).tosource().pretty(80).join-str("\n"))
      end
    | arg-error(message, partial) =>
      print(message + "\n")
      print(C.usage-info(options).join-str("\n"))
      print("\n")
  end
end

_ = main(C.args)

print("\n")
#print(LP.tokenize("test-file", "(lambda (x) y)").map(_.format()))