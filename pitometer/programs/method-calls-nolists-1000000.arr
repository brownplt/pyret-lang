lst = {
  method sum(self, n):
    if n <= 0: 0
    else:
      n + self.sum(n - 1)
    end
  end
}

lst.sum(1000000)
