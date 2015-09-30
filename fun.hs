module Main where

myGetLine :: IO [Char]
myGetLine = do
  c <- getChar
  if c == '\n' then
    return []
  else
    do cs <- myGetLine
       return (c:cs)

main = do
  myGetLine
