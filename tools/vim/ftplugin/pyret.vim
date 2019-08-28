if exists('b:did_ftplugin')
  finish
endif

let b:did_ftplugin = 1

" Set comment string
setlocal commentstring=#\ %s
