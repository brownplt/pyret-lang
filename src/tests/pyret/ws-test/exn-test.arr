#lang pyret/whalesong

try:
  raise("just an exceptional value")
except(e):
  print("Should be 'just an exceptional value':")
  print(e)
end

try:
  {}.x
except(e):
  print("Should be 'Field not found':")
  print(e.message)
end
