--Reyes Granados Naomi Itzel
import System.Random
import System.IO.Unsafe

type Only a = [a]
type Set a = Only a

prefixes :: [a]	-> (Set [a])
prefixes [] = []
prefixes (x:xs) = if r then [[x]] else (map (x:) (prefixes xs))
	 	  where r = unsafePerformIO(randomIO :: IO Bool)

sufixes :: [a] -> (Set [a])
sufixes [] = []
sufixes (x:xs) =  if r then [(x:xs)] else (sufixes xs)
	 	  where r = unsafePerformIO(randomIO :: IO Bool)

segments :: [a] -> (Set [a])
segments  = concat . (map sufixes) . prefixes

choose :: Set [a] -> Set [a]
choose x = x

segment :: [a] -> Set [a]
segment = choose.segments