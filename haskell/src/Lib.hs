module Lib
    ( someFunc
    ) where

import Text.Printf


data Product =
  Product String Int Float

data Calculator =
  Calculator Product Float

item :: Product
item = (Product "The Little Prince" 12345 20.25)

calculateTax::Product -> Float -> Float
calculateTax (Product _ _ p) t  = p + (p * t)

price :: Product -> Float
price (Product _ _ p) = p

reportPrice :: Calculator ->  IO()
reportPrice (Calculator prod tax) =
  printf "Product price price reported as $%.2f before tax and and $%.2f after %.0f%% tax.\n"
  (price prod) (calculateTax prod tax) (tax * 100)



someFunc :: IO ()
someFunc = do reportPrice (Calculator item 0.2)
              reportPrice (Calculator item 0.21)
