
# Themes

The files in this directory define the various themes for the editor, which affect mostly colors. Where styles are theme-dependent, `editor.css` uses CSS variables which are defined by each theme. 

## Non-color font styling.

For Pyret code styles, themes can control not just color, but also other font styling.

The font styling of the following tokens can be controlled:
- `keyword`
- `comments`
- `booleans`
- `built-ins`
- `function-names`
- `types`
- `variables`
- `numbers`
- `rough-nums`
- `rationals`
- `bad-numbers`
- `unterm-strings`
- `strings`

These styles are controlled by setting variables that look like their corresponding CSS properties but prefixed with the token name:
```
--TOKEN-font-size
--TOKEN-font-weight
--TOKEN-font-style
--TOKEN-text-decoration
```
For instance, to make function names italicized, you would set `--function-names-font-style: italic;` within your theme.

Note: All of these variables are optional and will have no effect if not defined.
