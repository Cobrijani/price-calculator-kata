module Lib
    ( someFunc
    ) where

import Text.Printf


data Product =
  Product String Int Float


data Discount =
  Discount Float
data Tax =
  Tax Float

-- Product, tax, discounts
data Calculator =
  Calculator Product Tax Discount

getTaxPercentage :: Tax -> Float
getTaxPercentage (Tax p) = p
getDiscountPercentage :: Discount -> Float
getDiscountPercentage (Discount p) = p


item :: Product
item = (Product "The Little Prince" 12345 20.25)

initialPrice:: Calculator -> Float
initialPrice (Calculator (Product _ _ p) _ _) = p

tax::Calculator -> Float
tax (Calculator (Product _ _ p) (Tax t) _ ) = p * t

discount::Calculator -> Float
discount (Calculator (Product _ _ p) _ (Discount d)) = p * d

taxPercentage::Calculator -> Float
taxPercentage (Calculator _ (Tax p) _ ) = p

discountPercentage::Calculator -> Float
discountPercentage (Calculator _ _ (Discount p)) = p

grossPrice::Calculator -> Float
grossPrice calc = (initialPrice calc) + (tax calc)

discountPrice::Calculator -> Float
discountPrice x = (initialPrice x) - (discount x)

totalPrice::Calculator -> Float
totalPrice calc = (initialPrice calc) + (tax calc) - (discount calc)

reportPrice :: Calculator ->  IO()
reportPrice calc = do
  printf "Tax=%.0f%%, discount=%.0f%%\n" ((taxPercentage calc) * 100) ((discountPercentage calc)* 100)
  printf "Tax amount = $%.2f; Discount amount = $%.2f\n"(tax calc) (discount calc)
  printf "Price before = $%.2f, price after = $%.2f\n" (initialPrice calc) (totalPrice calc)



someFunc :: IO ()
someFunc = do reportPrice (Calculator item (Tax 0.2) (Discount 0.15))
