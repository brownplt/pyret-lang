#lang scribble/base
@(require "../../scribble-api.rkt" scribble/core)

@(append-gen-docs
'(module
  "image-structs"
  (path "src/arr/trove/image-structs.arr")
  (data-spec (name "Color") (type-vars ()) (variants ("color")) (shared ()))
  
  (constr-spec
    (name "color")
    (members
      (("red"
        (type normal)
        (contract (a-id "Number" (xref "<global>" "Number"))))
      ("green"
        (type normal)
        (contract (a-id "Number" (xref "<global>" "Number"))))
      ("blue"
        (type normal)
        (contract (a-id "Number" (xref "<global>" "Number"))))
      ("alpha"
        (type normal)
        (contract (a-id "Number" (xref "<global>" "Number"))))))
    (with-members ()))
  (fun-spec
    (name "is-color")
    (arity 1)
    (params [list: ])
    (args ("val"))
    (return (a-id "Boolean" (xref "<global>" "Boolean")))
    (contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean"))))
    (doc "Checks whether the provided argument is in fact a color"))
  (unknown-item
    (name "orange")
    ;; ~color13(255, 165, 0, 255)
    )
  (unknown-item
    (name "red")
    ;; ~color13(255, 0, 0, 255)
    )
  (unknown-item
    (name "orange-red")
    ;; ~color13(255, 69, 0, 255)
    )
  (unknown-item
    (name "tomato")
    ;; ~color13(255, 99, 71, 255)
    )
  (unknown-item
    (name "dark-red")
    ;; ~color13(139, 0, 0, 255)
    )
  (unknown-item
    (name "fire-brick")
    ;; ~color13(178, 34, 34, 255)
    )
  (unknown-item
    (name "crimson")
    ;; ~color13(220, 20, 60, 255)
    )
  (unknown-item
    (name "deep-pink")
    ;; ~color13(255, 20, 147, 255)
    )
  (unknown-item
    (name "maroon")
    ;; ~color13(176, 48, 96, 255)
    )
  (unknown-item
    (name "indian-red")
    ;; ~color13(205, 92, 92, 255)
    )
  (unknown-item
    (name "medium-violet-red")
    ;; ~color13(199, 21, 133, 255)
    )
  (unknown-item
    (name "violet-red")
    ;; ~color13(208, 32, 144, 255)
    )
  (unknown-item
    (name "light-coral")
    ;; ~color13(240, 128, 128, 255)
    )
  (unknown-item
    (name "hot-pink")
    ;; ~color13(255, 105, 180, 255)
    )
  (unknown-item
    (name "pale-violet-red")
    ;; ~color13(219, 112, 147, 255)
    )
  (unknown-item
    (name "light-pink")
    ;; ~color13(255, 182, 193, 255)
    )
  (unknown-item
    (name "rosy-brown")
    ;; ~color13(188, 143, 143, 255)
    )
  (unknown-item
    (name "pink")
    ;; ~color13(255, 192, 203, 255)
    )
  (unknown-item
    (name "orchid")
    ;; ~color13(218, 112, 214, 255)
    )
  (unknown-item
    (name "lavender-blush")
    ;; ~color13(255, 240, 245, 255)
    )
  (unknown-item
    (name "snow")
    ;; ~color13(255, 250, 250, 255)
    )
  (unknown-item
    (name "chocolate")
    ;; ~color13(210, 105, 30, 255)
    )
  (unknown-item
    (name "saddle-brown")
    ;; ~color13(139, 69, 19, 255)
    )
  (unknown-item
    (name "brown")
    ;; ~color13(132, 60, 36, 255)
    )
  (unknown-item
    (name "dark-orange")
    ;; ~color13(255, 140, 0, 255)
    )
  (unknown-item
    (name "coral")
    ;; ~color13(255, 127, 80, 255)
    )
  (unknown-item
    (name "sienna")
    ;; ~color13(160, 82, 45, 255)
    )
  (unknown-item
    (name "salmon")
    ;; ~color13(250, 128, 114, 255)
    )
  (unknown-item
    (name "peru")
    ;; ~color13(205, 133, 63, 255)
    )
  (unknown-item
    (name "dark-goldenrod")
    ;; ~color13(184, 134, 11, 255)
    )
  (unknown-item
    (name "goldenrod")
    ;; ~color13(218, 165, 32, 255)
    )
  (unknown-item
    (name "sandy-brown")
    ;; ~color13(244, 164, 96, 255)
    )
  (unknown-item
    (name "light-salmon")
    ;; ~color13(255, 160, 122, 255)
    )
  (unknown-item
    (name "dark-salmon")
    ;; ~color13(233, 150, 122, 255)
    )
  (unknown-item
    (name "gold")
    ;; ~color13(255, 215, 0, 255)
    )
  (unknown-item
    (name "yellow")
    ;; ~color13(255, 255, 0, 255)
    )
  (unknown-item
    (name "olive")
    ;; ~color13(128, 128, 0, 255)
    )
  (unknown-item
    (name "burlywood")
    ;; ~color13(222, 184, 135, 255)
    )
  (unknown-item
    (name "tan")
    ;; ~color13(210, 180, 140, 255)
    )
  (unknown-item
    (name "navajo-white")
    ;; ~color13(255, 222, 173, 255)
    )
  (unknown-item
    (name "peach-puff")
    ;; ~color13(255, 218, 185, 255)
    )
  (unknown-item
    (name "khaki")
    ;; ~color13(240, 230, 140, 255)
    )
  (unknown-item
    (name "dark-khaki")
    ;; ~color13(189, 183, 107, 255)
    )
  (unknown-item
    (name "moccasin")
    ;; ~color13(255, 228, 181, 255)
    )
  (unknown-item
    (name "wheat")
    ;; ~color13(245, 222, 179, 255)
    )
  (unknown-item
    (name "bisque")
    ;; ~color13(255, 228, 196, 255)
    )
  (unknown-item
    (name "pale-goldenrod")
    ;; ~color13(238, 232, 170, 255)
    )
  (unknown-item
    (name "blanched-almond")
    ;; ~color13(255, 235, 205, 255)
    )
  (unknown-item
    (name "medium-goldenrod")
    ;; ~color13(234, 234, 173, 255)
    )
  (unknown-item
    (name "papaya-whip")
    ;; ~color13(255, 239, 213, 255)
    )
  (unknown-item
    (name "misty-rose")
    ;; ~color13(255, 228, 225, 255)
    )
  (unknown-item
    (name "lemon-chiffon")
    ;; ~color13(255, 250, 205, 255)
    )
  (unknown-item
    (name "antique-white")
    ;; ~color13(250, 235, 215, 255)
    )
  (unknown-item
    (name "cornsilk")
    ;; ~color13(255, 248, 220, 255)
    )
  (unknown-item
    (name "light-goldenrody-ellow")
    ;; ~color13(250, 250, 210, 255)
    )
  (unknown-item
    (name "old-lace")
    ;; ~color13(253, 245, 230, 255)
    )
  (unknown-item
    (name "linen")
    ;; ~color13(250, 240, 230, 255)
    )
  (unknown-item
    (name "light-yellow")
    ;; ~color13(255, 255, 224, 255)
    )
  (unknown-item
    (name "seashell")
    ;; ~color13(255, 245, 238, 255)
    )
  (unknown-item
    (name "beige")
    ;; ~color13(245, 245, 220, 255)
    )
  (unknown-item
    (name "floral-white")
    ;; ~color13(255, 250, 240, 255)
    )
  (unknown-item
    (name "ivory")
    ;; ~color13(255, 255, 240, 255)
    )
  (unknown-item
    (name "green")
    ;; ~color13(0, 255, 0, 255)
    )
  (unknown-item
    (name "lawn-green")
    ;; ~color13(124, 252, 0, 255)
    )
  (unknown-item
    (name "chartreuse")
    ;; ~color13(127, 255, 0, 255)
    )
  (unknown-item
    (name "green-yellow")
    ;; ~color13(173, 255, 47, 255)
    )
  (unknown-item
    (name "yellow-green")
    ;; ~color13(154, 205, 50, 255)
    )
  (unknown-item
    (name "medium-forest-green")
    ;; ~color13(107, 142, 35, 255)
    )
  (unknown-item
    (name "olive-drab")
    ;; ~color13(107, 142, 35, 255)
    )
  (unknown-item
    (name "dark-olive-green")
    ;; ~color13(85, 107, 47, 255)
    )
  (unknown-item
    (name "dark-sea-green")
    ;; ~color13(143, 188, 139, 255)
    )
  (unknown-item
    (name "lime")
    ;; ~color13(0, 255, 0, 255)
    )
  (unknown-item
    (name "dark-green")
    ;; ~color13(0, 100, 0, 255)
    )
  (unknown-item
    (name "lime-green")
    ;; ~color13(50, 205, 50, 255)
    )
  (unknown-item
    (name "forest-green")
    ;; ~color13(34, 139, 34, 255)
    )
  (unknown-item
    (name "spring-green")
    ;; ~color13(0, 255, 127, 255)
    )
  (unknown-item
    (name "medium-spring-green")
    ;; ~color13(0, 250, 154, 255)
    )
  (unknown-item
    (name "sea-green")
    ;; ~color13(46, 139, 87, 255)
    )
  (unknown-item
    (name "medium-sea-green")
    ;; ~color13(60, 179, 113, 255)
    )
  (unknown-item
    (name "aquamarine")
    ;; ~color13(112, 216, 144, 255)
    )
  (unknown-item
    (name "light-green")
    ;; ~color13(144, 238, 144, 255)
    )
  (unknown-item
    (name "pale-green")
    ;; ~color13(152, 251, 152, 255)
    )
  (unknown-item
    (name "medium-aquamarine")
    ;; ~color13(102, 205, 170, 255)
    )
  (unknown-item
    (name "turquoise")
    ;; ~color13(64, 224, 208, 255)
    )
  (unknown-item
    (name "light-sea-green")
    ;; ~color13(32, 178, 170, 255)
    )
  (unknown-item
    (name "medium-turquoise")
    ;; ~color13(72, 209, 204, 255)
    )
  (unknown-item
    (name "honeydew")
    ;; ~color13(240, 255, 240, 255)
    )
  (unknown-item
    (name "mint-cream")
    ;; ~color13(245, 255, 250, 255)
    )
  (unknown-item
    (name "royal-blue")
    ;; ~color13(65, 105, 225, 255)
    )
  (unknown-item
    (name "dodger-blue")
    ;; ~color13(30, 144, 255, 255)
    )
  (unknown-item
    (name "deep-sky-blue")
    ;; ~color13(0, 191, 255, 255)
    )
  (unknown-item
    (name "cornflower-blue")
    ;; ~color13(100, 149, 237, 255)
    )
  (unknown-item
    (name "steel-blue")
    ;; ~color13(70, 130, 180, 255)
    )
  (unknown-item
    (name "light-sky-blue")
    ;; ~color13(135, 206, 250, 255)
    )
  (unknown-item
    (name "dark-turquoise")
    ;; ~color13(0, 206, 209, 255)
    )
  (unknown-item
    (name "cyan")
    ;; ~color13(0, 255, 255, 255)
    )
  (unknown-item
    (name "aqua")
    ;; ~color13(0, 255, 255, 255)
    )
  (unknown-item
    (name "dark-cyan")
    ;; ~color13(0, 139, 139, 255)
    )
  (unknown-item
    (name "teal")
    ;; ~color13(0, 128, 128, 255)
    )
  (unknown-item
    (name "sky-blue")
    ;; ~color13(135, 206, 235, 255)
    )
  (unknown-item
    (name "cadet-blue")
    ;; ~color13(95, 158, 160, 255)
    )
  (unknown-item
    (name "dark-slate-gray")
    ;; ~color13(47, 79, 79, 255)
    )
  (unknown-item
    (name "light-slate-gray")
    ;; ~color13(119, 136, 153, 255)
    )
  (unknown-item
    (name "slate-gray")
    ;; ~color13(112, 128, 144, 255)
    )
  (unknown-item
    (name "light-steel-blue")
    ;; ~color13(176, 196, 222, 255)
    )
  (unknown-item
    (name "light-blue")
    ;; ~color13(173, 216, 230, 255)
    )
  (unknown-item
    (name "powder-blue")
    ;; ~color13(176, 224, 230, 255)
    )
  (unknown-item
    (name "pale-turquoise")
    ;; ~color13(175, 238, 238, 255)
    )
  (unknown-item
    (name "light-cyan")
    ;; ~color13(224, 255, 255, 255)
    )
  (unknown-item
    (name "alice-blue")
    ;; ~color13(240, 248, 255, 255)
    )
  (unknown-item
    (name "azure")
    ;; ~color13(240, 255, 255, 255)
    )
  (unknown-item
    (name "medium-blue")
    ;; ~color13(0, 0, 205, 255)
    )
  (unknown-item
    (name "dark-blue")
    ;; ~color13(0, 0, 139, 255)
    )
  (unknown-item
    (name "midnight-blue")
    ;; ~color13(25, 25, 112, 255)
    )
  (unknown-item
    (name "navy")
    ;; ~color13(36, 36, 140, 255)
    )
  (unknown-item
    (name "blue")
    ;; ~color13(0, 0, 255, 255)
    )
  (unknown-item
    (name "indigo")
    ;; ~color13(75, 0, 130, 255)
    )
  (unknown-item
    (name "blue-violet")
    ;; ~color13(138, 43, 226, 255)
    )
  (unknown-item
    (name "medium-slate-blue")
    ;; ~color13(123, 104, 238, 255)
    )
  (unknown-item
    (name "slate-blue")
    ;; ~color13(106, 90, 205, 255)
    )
  (unknown-item
    (name "purple")
    ;; ~color13(160, 32, 240, 255)
    )
  (unknown-item
    (name "dark-slate-blue")
    ;; ~color13(72, 61, 139, 255)
    )
  (unknown-item
    (name "dark-violet")
    ;; ~color13(148, 0, 211, 255)
    )
  (unknown-item
    (name "dark-orchid")
    ;; ~color13(153, 50, 204, 255)
    )
  (unknown-item
    (name "medium-purple")
    ;; ~color13(147, 112, 219, 255)
    )
  (unknown-item
    (name "medium-orchid")
    ;; ~color13(186, 85, 211, 255)
    )
  (unknown-item
    (name "magenta")
    ;; ~color13(255, 0, 255, 255)
    )
  (unknown-item
    (name "fuchsia")
    ;; ~color13(255, 0, 255, 255)
    )
  (unknown-item
    (name "dark-magenta")
    ;; ~color13(139, 0, 139, 255)
    )
  (unknown-item
    (name "violet")
    ;; ~color13(238, 130, 238, 255)
    )
  (unknown-item
    (name "plum")
    ;; ~color13(221, 160, 221, 255)
    )
  (unknown-item
    (name "lavender")
    ;; ~color13(230, 230, 250, 255)
    )
  (unknown-item
    (name "thistle")
    ;; ~color13(216, 191, 216, 255)
    )
  (unknown-item
    (name "ghost-white")
    ;; ~color13(248, 248, 255, 255)
    )
  (unknown-item
    (name "white")
    ;; ~color13(255, 255, 255, 255)
    )
  (unknown-item
    (name "white-smoke")
    ;; ~color13(245, 245, 245, 255)
    )
  (unknown-item
    (name "gainsboro")
    ;; ~color13(220, 220, 220, 255)
    )
  (unknown-item
    (name "light-gray")
    ;; ~color13(211, 211, 211, 255)
    )
  (unknown-item
    (name "silver")
    ;; ~color13(192, 192, 192, 255)
    )
  (unknown-item
    (name "gray")
    ;; ~color13(190, 190, 190, 255)
    )
  (unknown-item
    (name "dark-gray")
    ;; ~color13(169, 169, 169, 255)
    )
  (unknown-item
    (name "dim-gray")
    ;; ~color13(105, 105, 105, 255)
    )
  (unknown-item
    (name "black")
    ;; ~color13(0, 0, 0, 255)
    )))

@(define (render-color name r g b a)
   (ignore (list name))
   (item name ": "
         (make-element (make-style "relax" (list (make-color-property (list r g b)))) "abc123") " "
         (make-element (make-style "relax" (list (make-background-color-property (list r g b)))) "abc123")))
@(define number (a-id "Number" (xref "<global>" "Number")))
@(define color-args (list
      `("red"   ("type" "normal") ("contract" ,number))
      `("green" ("type" "normal") ("contract" ,number))
      `("blue"  ("type" "normal") ("contract" ,number))
      `("alpha" ("type" "normal") ("contract" ,number))))

@docmodule["image-structs"]{
  @; Ignored type testers
  @ignore[(list "is-color")]
  @section[#:tag "image-structs_DataTypes"]{Data types}
  @data-spec2["Color" (list) (list
    @constructor-spec["Color" "color" color-args]
  )]
  @nested[#:style 'inset]{
    @constructor-doc["Color" "color" color-args (a-id "Color" (xref "image-structs" "Color"))]{
      Valid arguments are in the range 0--255, inclusive.
    }
  }

  @section{Predefined colors}
  The following colors are predefined constants:
  @itemlist[
    @render-color["orange" 255 165 0 255]
    @render-color["red" 255 0 0 255]
    @render-color["orange-red" 255 69 0 255]
    @render-color["tomato" 255 99 71 255]
    @render-color["dark-red" 139 0 0 255]
    @render-color["fire-brick" 178 34 34 255]
    @render-color["crimson" 220 20 60 255]
    @render-color["deep-pink" 255 20 147 255]
    @render-color["maroon" 176 48 96 255]
    @render-color["indian-red" 205 92 92 255]
    @render-color["medium-violet-red" 199 21 133 255]
    @render-color["violet-red" 208 32 144 255]
    @render-color["light-coral" 240 128 128 255]
    @render-color["hot-pink" 255 105 180 255]
    @render-color["pale-violet-red" 219 112 147 255]
    @render-color["light-pink" 255 182 193 255]
    @render-color["rosy-brown" 188 143 143 255]
    @render-color["pink" 255 192 203 255]
    @render-color["orchid" 218 112 214 255]
    @render-color["lavender-blush" 255 240 245 255]
    @render-color["snow" 255 250 250 255]
    @render-color["chocolate" 210 105 30 255]
    @render-color["saddle-brown" 139 69 19 255]
    @render-color["brown" 132 60 36 255]
    @render-color["dark-orange" 255 140 0 255]
    @render-color["coral" 255 127 80 255]
    @render-color["sienna" 160 82 45 255]
    @render-color["salmon" 250 128 114 255]
    @render-color["peru" 205 133 63 255]
    @render-color["dark-goldenrod" 184 134 11 255]
    @render-color["goldenrod" 218 165 32 255]
    @render-color["sandy-brown" 244 164 96 255]
    @render-color["light-salmon" 255 160 122 255]
    @render-color["dark-salmon" 233 150 122 255]
    @render-color["gold" 255 215 0 255]
    @render-color["yellow" 255 255 0 255]
    @render-color["olive" 128 128 0 255]
    @render-color["burlywood" 222 184 135 255]
    @render-color["tan" 210 180 140 255]
    @render-color["navajo-white" 255 222 173 255]
    @render-color["peach-puff" 255 218 185 255]
    @render-color["khaki" 240 230 140 255]
    @render-color["dark-khaki" 189 183 107 255]
    @render-color["moccasin" 255 228 181 255]
    @render-color["wheat" 245 222 179 255]
    @render-color["bisque" 255 228 196 255]
    @render-color["pale-goldenrod" 238 232 170 255]
    @render-color["blanched-almond" 255 235 205 255]
    @render-color["medium-goldenrod" 234 234 173 255]
    @render-color["papaya-whip" 255 239 213 255]
    @render-color["misty-rose" 255 228 225 255]
    @render-color["lemon-chiffon" 255 250 205 255]
    @render-color["antique-white" 250 235 215 255]
    @render-color["cornsilk" 255 248 220 255]
    @render-color["light-goldenrody-ellow" 250 250 210 255]
    @render-color["old-lace" 253 245 230 255]
    @render-color["linen" 250 240 230 255]
    @render-color["light-yellow" 255 255 224 255]
    @render-color["seashell" 255 245 238 255]
    @render-color["beige" 245 245 220 255]
    @render-color["floral-white" 255 250 240 255]
    @render-color["ivory" 255 255 240 255]
    @render-color["green" 0 255 0 255]
    @render-color["lawn-green" 124 252 0 255]
    @render-color["chartreuse" 127 255 0 255]
    @render-color["green-yellow" 173 255 47 255]
    @render-color["yellow-green" 154 205 50 255]
    @render-color["medium-forest-green" 107 142 35 255]
    @render-color["olive-drab" 107 142 35 255]
    @render-color["dark-olive-green" 85 107 47 255]
    @render-color["dark-sea-green" 143 188 139 255]
    @render-color["lime" 0 255 0 255]
    @render-color["dark-green" 0 100 0 255]
    @render-color["lime-green" 50 205 50 255]
    @render-color["forest-green" 34 139 34 255]
    @render-color["spring-green" 0 255 127 255]
    @render-color["medium-spring-green" 0 250 154 255]
    @render-color["sea-green" 46 139 87 255]
    @render-color["medium-sea-green" 60 179 113 255]
    @render-color["aquamarine" 112 216 144 255]
    @render-color["light-green" 144 238 144 255]
    @render-color["pale-green" 152 251 152 255]
    @render-color["medium-aquamarine" 102 205 170 255]
    @render-color["turquoise" 64 224 208 255]
    @render-color["light-sea-green" 32 178 170 255]
    @render-color["medium-turquoise" 72 209 204 255]
    @render-color["honeydew" 240 255 240 255]
    @render-color["mint-cream" 245 255 250 255]
    @render-color["royal-blue" 65 105 225 255]
    @render-color["dodger-blue" 30 144 255 255]
    @render-color["deep-sky-blue" 0 191 255 255]
    @render-color["cornflower-blue" 100 149 237 255]
    @render-color["steel-blue" 70 130 180 255]
    @render-color["light-sky-blue" 135 206 250 255]
    @render-color["dark-turquoise" 0 206 209 255]
    @render-color["cyan" 0 255 255 255]
    @render-color["aqua" 0 255 255 255]
    @render-color["dark-cyan" 0 139 139 255]
    @render-color["teal" 0 128 128 255]
    @render-color["sky-blue" 135 206 235 255]
    @render-color["cadet-blue" 95 158 160 255]
    @render-color["dark-slate-gray" 47 79 79 255]
    @render-color["light-slate-gray" 119 136 153 255]
    @render-color["slate-gray" 112 128 144 255]
    @render-color["light-steel-blue" 176 196 222 255]
    @render-color["light-blue" 173 216 230 255]
    @render-color["powder-blue" 176 224 230 255]
    @render-color["pale-turquoise" 175 238 238 255]
    @render-color["light-cyan" 224 255 255 255]
    @render-color["alice-blue" 240 248 255 255]
    @render-color["azure" 240 255 255 255]
    @render-color["medium-blue" 0 0 205 255]
    @render-color["dark-blue" 0 0 139 255]
    @render-color["midnight-blue" 25 25 112 255]
    @render-color["navy" 36 36 140 255]
    @render-color["blue" 0 0 255 255]
    @render-color["indigo" 75 0 130 255]
    @render-color["blue-violet" 138 43 226 255]
    @render-color["medium-slate-blue" 123 104 238 255]
    @render-color["slate-blue" 106 90 205 255]
    @render-color["purple" 160 32 240 255]
    @render-color["dark-slate-blue" 72 61 139 255]
    @render-color["dark-violet" 148 0 211 255]
    @render-color["dark-orchid" 153 50 204 255]
    @render-color["medium-purple" 147 112 219 255]
    @render-color["medium-orchid" 186 85 211 255]
    @render-color["magenta" 255 0 255 255]
    @render-color["fuchsia" 255 0 255 255]
    @render-color["dark-magenta" 139 0 139 255]
    @render-color["violet" 238 130 238 255]
    @render-color["plum" 221 160 221 255]
    @render-color["lavender" 230 230 250 255]
    @render-color["thistle" 216 191 216 255]
    @render-color["ghost-white" 248 248 255 255]
    @render-color["white" 255 255 255 255]
    @render-color["white-smoke" 245 245 245 255]
    @render-color["gainsboro" 220 220 220 255]
    @render-color["light-gray" 211 211 211 255]
    @render-color["silver" 192 192 192 255]
    @render-color["gray" 190 190 190 255]
    @render-color["dark-gray" 169 169 169 255]
    @render-color["dim-gray" 105 105 105 255]
    @render-color["black" 0 0 0 255]
  ]
}
