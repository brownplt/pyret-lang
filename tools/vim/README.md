# (G)Vim indent/syntax files setting up

```mkdir -p ~/.vim/{indent,syntax}```

`cp -v pyret-lang/tools/vim/indent/pyret.vim ~/.vim/indent/`

`cp -v pyret-lang/tools/vim/syntax/pyret.vim ~/.vim/syntax/`

Add these lines to the `.vimrc` or `.gvimrc` file in your home directory:

```
au BufRead,BufNewFile *.arr set filetype=pyret
filetype indent on
set smartindent
```
