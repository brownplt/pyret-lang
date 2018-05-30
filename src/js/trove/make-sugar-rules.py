# could maybe just triple backtick instead
with open("../../arr/trove/pyret.sugar") as sugars:
    sugars_contents = ["\"" + line.replace("\"", "\\\"").rstrip() for line in sugars]
    sugars_str = ("\\n\" + \n").join(sugars_contents) + "\""

with open("./pyret-sugar-rules.js.template") as mod:
    mod_str = mod.read()

new_file = mod_str.replace('$PYRET_RULES', sugars_str)
print(new_file)
