module Color where
-- Need to revisit this code, because the operations need to probably
-- be correctly defined.

type Red = Int
type Blue = Int
type Green = Int
type Cyan = Int
type Magenta = Int
type Yellow = Int
type Key = Int -- Key and black are synonyms
type Black = Int

data Color = RGB (Red, Blue, Green)  | CMYK (Cyan, Magenta, Yellow, Key)
mod256 x = x `mod` 256
instance Num Color where
    c1 + c2 = case (c1, c2) of
                (RGB(a, b, c), RGB(x, y, z)) -> RGB ( mod256 $ a + x, mod256 $ b + y, mod256 $ c + z)
                (CMYK(a , b, c, d), CMYK(w, x, y, z)) -> CMYK(mod256 $ a + w,
                                                    mod256 $ b + x,
                                                    mod256 $ c + y,
                                                    mod256 $ d + z)
                (_, _) -> error "Invalid type"
    c1 * c2 = case (c1, c2) of
                (RGB(a, b, c), RGB(x, y, z)) -> RGB (mod256 $ a * x, mod256 $ b * y, mod256 $ c * z)
                (CMYK(a , b, c, d), CMYK(w, x, y, z)) -> CMYK(mod256 $ a * w,
                                                         mod256 $ b * x,
                                                         mod256 $ c * y,
                                                         mod256 $ d* z)
                (_, _) -> error "Invalid type"


