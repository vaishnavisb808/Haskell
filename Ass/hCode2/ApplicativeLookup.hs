import Control.Applicative

f x = lookup x [(3,"hello"),
               (4,"hi"),
               (5,"vaish")]

g y = lookup y [(7, "Sup?"  ),
                (8, "Charis"),
                (9, "aloha" )]

h z = lookup z [(2,3),(5,6),(7,8)]
m x = lookup x [(4,10),(8,13),(1,9001)]                   
               