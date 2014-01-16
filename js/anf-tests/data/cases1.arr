data MyList:
  | mt
  | cons(f, r)
end

cases(MyList) mt:
  | mt => test-print("mt")
  | else => test-print("fail")
end

cases(MyList) cons(1, 2):
  | mt => test-print("fail")
  | else => test-print("cons")
end

cases(MyList) cons(1, 2):
  | cons(f, r) => test-print(f + r)
  | else => test-print("fail")
end

cases(MyList) cons(1, cons(4, mt)):
  | cons(f, r) =>
    cases(MyList) r:
      | cons(f1, r1) =>
        test-print(is-mt(r1))
        test-print(f)
        test-print(f1)
      | mt => test-print("fail")
    end
  | mt => test-print("fail")
end

