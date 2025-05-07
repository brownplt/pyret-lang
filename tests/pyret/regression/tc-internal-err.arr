# https://github.com/brownplt/pyret-lang/issues/1619
use context base

provide: o end

o = { m: {(): raise("something") } }


