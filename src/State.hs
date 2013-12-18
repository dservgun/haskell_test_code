module State where
import System.Random
newtype State s a = State {
    runState :: s -> (a, s)
}

rand :: IO Int
rand = getStdRandom (randomR (0, maxBound))

