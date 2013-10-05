data Foo:
  | test with:
    bar(self):
      try:
        {}.x
      except(e):
        5
      end
    end
end
test.bar()