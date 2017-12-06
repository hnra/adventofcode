findSide :: Int -> Int -> Int -> Int
findSide max' width num =
  f max' width num 0
  where
    f m w n c
      | m - c * (w - 1) < n = c
      | otherwise = f m w n (c + 1)

gridWidthContaining :: Int -> (Int, Int)
gridWidthContaining x =
  let ret = f x 1 0 in
  (widthFromCnt $ fst ret, snd ret)
  where
    f num curr cnt
      | curr + 2 * (widthFromCnt cnt) - 2 >= num = (cnt, (curr + 2 * (widthFromCnt cnt) - 2))
      | otherwise = f num (curr + cnt * 8 + 4) (cnt + 1)
    widthFromCnt cnt = cnt + 1 + cnt

getCoord :: Int -> Int -> Int -> Int -> (Int, Int)
getCoord side width max' num
  | side == 1 = (width - (max' - num), width)
  | side == 2 = (1, (width - ((max' - width + 1) - num)))
  | side == 3 = ((max' - 2 * (width - 1)) - num + 1, 1)
  | side == 4 = (width, (max' - 3 * (width - 1)) - num + 1)
  | otherwise = (0,0)

getXpos :: Int -> (Int, Int)
getXpos w = (a,a)
  where a = ceiling $ (fromIntegral w) / 2

getSteps :: (Int, Int) -> (Int, Int) -> Int
getSteps xpos coord = x + y
  where
    x = abs $ fst coord - fst xpos
    y = abs $ snd coord - snd xpos

main :: IO ()
main = do
  line <- getLine
  let num = read $ line
  if num > 1 then do
    let mw = gridWidthContaining num
    let width = fst mw
    let max' = snd mw
    let side = findSide max' width num
    let coords = getCoord side width max' num
    let xpos = getXpos width
    let steps = getSteps xpos coords
    putStrLn $ "Steps: " ++ (show steps)
  else
    putStrLn $ "Steps: 0"
