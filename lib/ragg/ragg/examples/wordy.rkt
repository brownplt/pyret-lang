#lang ragg
;; A parser for a silly language
sentence: verb optional-adjective object
verb: greeting
optional-adjective: ["happy" | "frumpy"]
greeting: "hello" | "hola" | "aloha"
object: "world" | WORLD
