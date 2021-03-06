--Reyes Granados Naomi Itzel
import System.Random

type Only a = [a]
type Set a = Only a

prefixes :: [a]	-> (Set [a])
prefixes [] = []
prefixes (x:xs) = [x]: (map (x:) (prefixes xs))

sufixes :: [a] -> (Set [a])
sufixes [] = []
sufixes (x:xs) = (x:xs): (sufixes xs)

segments :: [a] -> (Set [a])
segments  = concat . (map sufixes) . prefixes

choose :: [a] -> IO a
choose x = do i <- randomRIO (0, (length x)-1)
       	      return (x!!i)

segment :: [a] -> IO [a]
segment = choose.segments