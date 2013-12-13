try:
  try:
    10
    raise("Help")
    20 # control never reaches here
  except(e):
    e # is "Help"
  end
except(e):
  # control never reaches here
end

#From pyret documentation
