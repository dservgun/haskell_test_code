module State where
import System.Random
import Control.Monad.State

type RandomState a = State StdGen a
    
getRandom1 :: Random a => RandomState a
getRandom1 = 
    get >>= \gen -> 
            let (val, gen1) = random gen  in
                put gen1 >>
                return val
                

                
getRandom a = randomRIO (0, maxBound a)

rand :: IO Int
rand = getStdRandom (randomR (0, maxBound))

twoBadRandoms :: RandomGen g => g -> (Int, Int)
twoBadRandoms gen = (fst $ random gen, fst $ random gen)


twoGoodRandoms gen = let (a, gen1) = random gen
                         (b, gen2) = random gen1
                         in (a,b)

getTwoRandoms = liftM2 (,) rand rand




        