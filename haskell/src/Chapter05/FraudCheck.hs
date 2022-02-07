{-# LANGUAGE ScopedTypeVariables #-}
module Chapter05.FraudCheck where


data FraudStatus = Fraud | NoFraud

-- Simple Fraud Analysis

fraudOrNo :: FraudStatus -> Float
fraudOrNo Fraud   = 1
fraudOrNo NoFraud = 0

-- Given list of neighbor node fraud statuses, determine if new node is fraud or not
simpleFraudCheck :: [FraudStatus] -> Bool
simpleFraudCheck neighbors = sumFraud / fromIntegral (length neighbors) > 0.3
                            where sumFraud = sum $ map fraudOrNo neighbors

-- Watchtower fraud analytics

newtype WatchTowerFraudNode = WatchTowerFraudNode { normalizedDOS :: Float  }
data RiskLevel =  NoRisk | LowRisk | MediumRisk | HighRisk deriving (Show)


riskLevel :: Float -> RiskLevel
riskLevel x | x == 0    = NoRisk
            | x <= 0.10 = LowRisk
            | x <= 0.30 = MediumRisk
            | otherwise = HighRisk

-- Given list of neighbor nodes containing normalized DOS (calculating those is separate, fairly simple though)
-- determine risk level of potential new node
watchTowerFraudCheck :: [WatchTowerFraudNode] -> RiskLevel
watchTowerFraudCheck neighbors = riskLevel $ sumFraud / fromIntegral (length neighbors)
                                where sumFraud = foldr (\n x -> x + normalizedDOS n) 0 neighbors