-- 
-- Semantics of shape language
--
module Shape where


-- (1) Syntax of shapes
--
data Shape = X 
           | TD Shape Shape
           | LR Shape Shape
           deriving Show


-- (2) Semantic domain: an image is a set of pixels
--
type Pixel = (Int,Int)
type Image = [Pixel]


-- (3) Semantic function
--
sem :: Shape -> Image 
sem X           = [(1,1)]
sem (LR s1 s2) = d1 ++ [(x+maxx d1,y) | (x,y) <- sem s2] 
                 where d1 = sem s1
sem (TD s1 s2) = d2 ++ [(x,y+maxy d2) | (x,y) <- sem s1] 
                 where d2 = sem s2

maxx :: [Pixel] -> Int
maxx = maximum . map fst

maxy :: [Pixel] -> Int
maxy = maximum . map snd

type BBox = (Int, Int)

bbox :: Shape -> BBox
bbox shape = (maxx img, maxy img)
           where img = sem shape

rect :: Shape -> Maybe BBox
rect X                        = Just (bbox X)
rect s@(TD s0 s1) | w0 == w1  = Just (bbox s)
                  | otherwise = Nothing
                  where w0 = case rect s0 of
                               Just bb -> fst bb
                        w1 = case rect s1 of
                               Just bb -> fst bb
rect s@(LR s0 s1) | w0 == w1  = Just (bbox s)
                  | otherwise = Nothing
                  where w0 = case rect s0 of
                               Just bb -> snd bb
                        w1 = case rect s1 of
                               Just bb -> snd bb
rect _                     = Nothing
