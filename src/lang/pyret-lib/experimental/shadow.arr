#lang pyret

import "mixin-java.arr" as Java
import "check.arr" as Check

C1 = Java.class({
                 fields: {x : "Number"},
                 methods: {
                   add1(self, _): self.set("x", self.get("x").add(2)) end,
                   add3(self, _): self.set("x", self.get("x").add(3)) end,
                   proxy_add1(self, _):
                     self.invoke("add1", _)
                   end
                 },
                 constructor(self, args):
                   self.super(args)
                   self.set("x", args._x.add(4))
                   self
                 end
                })

C2 = C1.ext({
                fields: {x : "Number"},
                methods: {
                   add1(self, _): self.set("x", self.get("x").add(1)) end
                },
                constructor(self, args):
                  self.super(args)
                  self.set("x", args._x)
                  self
                end
               })

c2 = C2.new({_x: 42})
Check.equal(c2.get("x"), 42, "constructor for C2 works")
c1 = c2.view-as(C1)
Check.equal(c1.get("x"), 46, "constructor for C2 works when viewed as C1")

c2.invoke("add1", nothing)
Check.equal(c2.get("x"), 43, "overridden method uses child class state")
Check.equal(c1.get("x"), 46, "overridden method uses ONLY child class state")

c1.invoke("add1", nothing)
Check.equal(c2.get("x"), 44, "upcast view uses child class method (and state)")
Check.equal(c1.get("x"), 46, "upcast view uses child class method (and ONLY state)")

c1.invoke("add3", nothing)
Check.equal(c2.get("x"), 44, "inherited method uses ONLY parent class state")
Check.equal(c1.get("x"), 49, "inherited method uses parent class state")

c1.invoke("proxy_add1", nothing)
Check.equal(c2.get("x"), 45, "calling overridden method from parent method uses child method and state")
Check.equal(c1.get("x"), 49, "calling overridden method from parent method uses ONLY child method and state")

Check.tru(c1.instance-of(C2), "instance-of works after upcast")
Check.tru(c2.instance-of(C1), "instance-of works after upcast")
Check.tru(c1.instance-of(C1), "instance-of works after upcast")
Check.tru(c2.instance-of(C2), "instance-of works after upcast")
