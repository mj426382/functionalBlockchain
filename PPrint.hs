-- mj426382
module PPrint where

writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (k,v) = showString k . showString ": " . shows v

pprH, pprV :: [ShowS] -> ShowS
pprV = intercalateS (showChar '\n')
pprH = intercalateS (showChar ' ')

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS sep [] = showString ""
intercalateS sep [s] = s
intercalateS sep (x : z) = x . sep . intercalateS sep z

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith fun [] = showString ""
pprListWith fun [x] = fun x
pprListWith fun (x : z) = fun x . pprListWith fun z

runShows :: ShowS -> IO ()
runShows = putStrLn . ($"")
