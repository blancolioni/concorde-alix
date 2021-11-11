module Concorde where

import qualified Nazar

ui = Nazar.application box1

box1 = Nazar.verticalBox [ toolbar, box2, statusBar ]

toolbar = Nazar.toolbar [ Nazar.brand 
                        , 


   