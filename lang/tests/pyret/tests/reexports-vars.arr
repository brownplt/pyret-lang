import file("defines-vars.arr") as D

# Re-exports defines-vars.arr's vars, so importers see them as aliases
provide from D: *, y as y2 end
