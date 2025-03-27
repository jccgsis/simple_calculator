import Data.Char(isDigit, isSpace)

-- Main function to evaluate the expression
calculate :: String -> Int
calculate s = fst (evalExpr (filter (not . isSpace) s))

-- Evaluate expression, return result and remaining string
evalExpr :: String -> (Int, String)
evalExpr s = eval s 0 1
  where
    eval [] acc sign = (acc, [])
    eval (c:cs) acc sign
      | isDigit c =
          let (num, rest) = readNumber (c:cs)
          in eval rest (acc + sign * num) sign
      | c == '+'  = eval cs acc 1
      | c == '-'  = eval cs acc (-1)
      | c == '('  =
          let (val, rest) = evalExpr cs
          in eval rest (acc + sign * val) 1
      | c == ')'  = (acc, cs)
      | otherwise = eval cs acc sign

-- Parse a full number from the string
readNumber :: String -> (Int, String)
readNumber s =
  let (digits, rest) = span isDigit s
  in (read digits, rest)

main :: IO ()
main = do
  print $ calculate "1 + 1"               -- Output: 2
  print $ calculate " 2-1 + 2 "           -- Output: 3
  print $ calculate "(1+(4+5+2)-3)+(6+8)" -- Output: 23