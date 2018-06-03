import Data.Vect

--1,2
data PowerSource = Petrol | Pedal | Elec
data Vehicle : PowerSource -> Type where
  Bicycle : Vehicle Pedal
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  Unicycle : Vehicle Pedal
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  Tram : (battery : Nat) -> Vehicle Elec
  ElecCar : (battery : Nat) -> Vehicle Elec

wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels Unicycle = 1
wheels (Motorcycle fuel) = 2
wheels (Tram battery) = 0
wheels (ElecCar battery) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel (Motorcycle fuel) = Motorcycle 50

recharge : Vehicle Elec -> Vehicle Elec
recharge (Tram battery) = Tram 100
recharge (ElecCar battery) = ElecCar 100

--3, 4
vectTake : (n : Nat) -> Vect (n + m) a -> Vect n a
vectTake Z y = []
vectTake (S k) (x :: xs) = x :: vectTake k xs

--5
sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys =
  case integerToFin pos n of
    Just p => Just (Data.Vect.index p xs + Data.Vect.index p ys)
    Nothing => Nothing
