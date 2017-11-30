-- Fill in the DistanceConversions module first, and import it here
-- create a higher-order function for converting an area between two dimensions
-- this will take the function for converting a distance, and an area to convert
-- using the functions defined in the DistanceConversions module
-- Example areaConv inchesToCentimetres 9 = 58.0644

module DistanceConversions
( yardsToFeet
, feetToInches
, inchesToCentimetres
) where

-- Define yards to feet
yardsToFeet ::  Float -> Float
yardsToFeet y = 3*y

-- Define feet to inches
feetToInches :: Float -> Float
feetToInches f = 12*f

-- Define inches to centimetres
inchesToCentimetres :: Float -> Float
inchesToCentimetres i = 2.54*i

areaConv :: (Float -> Float) -> Float -> Float
areaConv linearConversion area = linearConversion (area*area)

-- define a function for converting square inches into square centimetres
sqInToSqCm :: Float -> Float
sqInToSqCm = undefined

-- define a function for converting square chains (22 yards) to square metres
sqChainsToSqM :: Float -> Float
sqChainsToSqM = undefined
