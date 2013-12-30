module GeneralizedLifting where
import Control.Monad

data MovieReview = MovieReview {
    revTitle :: String
    , revUser :: String
    , revReview :: String
}
    
lookup1 key alist = case lookup key alist of
    Just (Just s@(_:_)) -> Just s
    _ -> Nothing

    
review:: [(String, Maybe String)] -> Maybe String
review aList = Nothing

testLookup key aList = lookup1 key aList

apReview alist =
    MovieReview `liftM` lookup1 "title" alist
                   `ap` lookup1 "user" alist
                   `ap` lookup1 "review" alist