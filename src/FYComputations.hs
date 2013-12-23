module FYComputations where

import Data.Time(UTCTime)
import Data.Time.Calendar

import Data.Time.Clock
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.Framework
import Control.Monad
data FYState = Open | Closed 
    deriving(Show, Eq, Ord)
instance Arbitrary FYState where 
    arbitrary = elements [Open, Closed]
data FYPeriodType = Standard | Adjustment
    deriving (Show, Eq, Ord)
instance Arbitrary FYPeriodType where
    arbitrary = elements[Standard, Adjustment]
type FYDate = Int    
data Year = FiscalYear{
    startDate :: FYDate,
    endDate :: FYDate,
    state :: FYState
} deriving(Show, Eq, Ord)
days = 365 * 24 *3600

getUTCTime :: Int -> FYDate
getUTCTime anInt = anInt

instance Arbitrary Year where
    arbitrary = do
        utcTime <- choose (0, 2000) :: Gen Int
        return FiscalYear{startDate = getUTCTime utcTime, 
        endDate = getUTCTime utcTime, state = Closed}        
    
data FYPeriod = FiscalYearPeriod {
    period ::Year,
    periodStartDate:: FYDate,
    periodEndDate  :: FYDate,
    periodState :: FYState,
    periodType :: FYPeriodType
} deriving(Show, Eq, Ord)
instance Arbitrary FYPeriod where
    arbitrary = do
        utcTime <- choose (0, 2000) :: Gen Int
        let {computedDate = getUTCTime utcTime}
        return FiscalYearPeriod{
            period = FiscalYear{startDate = computedDate, endDate = computedDate, state = Closed}, 
            periodStartDate = computedDate,
            periodEndDate = computedDate,
            periodState = Closed,
            periodType = Standard}
    
prop_valid_annual_period :: Year -> Bool 
prop_valid_annual_period aYear = startDate aYear < endDate aYear

prop_valid_period :: FYPeriod -> Bool 
prop_valid_period aFYPeriod = periodStartDate aFYPeriod < periodEndDate aFYPeriod

prop_fyperiod :: Year -> FYPeriod -> Bool
prop_fyperiod aYear aPeriod = 
    case periodType aPeriod of
      Standard -> (startDate aYear >= periodStartDate aPeriod) && (endDate aYear <= periodEndDate aPeriod)
      Adjustment -> True

prop_fyperiods :: Year -> [FYPeriod] -> Bool
prop_fyperiods aYear []  = True


{-foldr (&&) False (map (validFiscalPeriod aFY) [aPeriod]) -}
--prop_fyperiods aYear fyPeriods = foldr (&&) False (map (prop_fyperiod aYear) fyPeriods)
prop_fyperiods aYear fyPeriods = foldr ((&&) . prop_fyperiod aYear) False fyPeriods

