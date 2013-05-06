#lang pyret/whalesong

try:
  raise("just an exceptional value")
except(e):
  print("Should be 'just an exceptional value':")
  print(e)
end

