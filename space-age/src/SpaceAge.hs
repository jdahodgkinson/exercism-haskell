module SpaceAge (Planet (..), ageOn) where

data Planet
  = Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = seconds / 31557600 / inEarthYears planet
  where
    inEarthYears :: Planet -> Float
    inEarthYears Earth = 1
    inEarthYears Mercury = 0.2408467
    inEarthYears Venus = 0.61519726
    inEarthYears Mars = 1.8808158
    inEarthYears Jupiter = 11.862615
    inEarthYears Saturn = 29.447498
    inEarthYears Uranus = 84.016846
    inEarthYears Neptune = 164.79132
