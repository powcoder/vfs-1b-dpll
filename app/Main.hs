https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
import Parser
import Tseitin
import DPLL

main :: IO ()
main = do
  raw <- getLine
  let p = parse raw
  putStrLn $ if satisfiable $ equisat p
    then "SAT"
    else "UNSAT"
