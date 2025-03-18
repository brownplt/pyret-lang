provide: * end

import either as E
import fetch as F
import parse-pyret as PP
import file("../compile-lib.arr") as CL
import file("../compile-structs.arr") as CS
import file("../js-of-pyret.arr") as JSP
import pathlib as P

fun mockable-url-locator(fetch):
    lam(url, globals):
        var ast = nothing
        {
            url: url,
            globals: globals,
            method get-uncached(self): none end,
            method get-modified-time(self): time-now() end,
            method get-options(self, options): options end,
            method get-module(self) block:
                when ast == nothing block:
                    result = fetch.fetch(self.url)
                    cases(E.Either) result:
                        | left(str) =>
                          ast := CL.pyret-ast(PP.surface-parse(str, self.url))
                        | right(err) =>
                          raise("Error fetching " + self.url + ": " + err)
                    end
                end
                ast
            end,
            method get-dependencies(self):
                CL.get-standard-dependencies(self.get-module(), self.uri())
            end,
            method get-native-modules(self): [list:] end,
            method get-extra-imports(self): CS.standard-imports end,
            method get-globals(self): self.globals end,
            method set-compiled(self, cr, deps) block:
                ast := nothing
                nothing
            end,
            method needs-compile(self, provides): true end,
            method get-compiled(self): none end,
            method uri(self): self.url end,
            method name(self): P.basename(self.url, "") end,
            method _equals(self, other, eq): eq(self.uri(), other.uri()) end
        }
    end
end

url-locator = mockable-url-locator({ fetch: F.fetch })