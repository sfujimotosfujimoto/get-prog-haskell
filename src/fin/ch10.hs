module Chapter10 where

ageAndHeight :: (Int, Int)
ageAndHeight = (34, 74)

half :: Int -> Double
half n = fromIntegral n/2

halve :: Int -> Int
halve n = div n 2

printDouble :: Int -> String
printDouble n = show (n * 2)

makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

ifEven :: (Int -> Int) -> Int -> Int
ifEven f n = if even n
                then f n
                else n

makeTriple :: a -> b -> c -> (a,b,c)
makeTriple x y z = (x,y,z)


type FirstName = String
type LastName = String
type Age = Int
type Height = Int
type PatientName = (String,String)

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient

testPatient = ("Tom", "Growde") :: PatientName

patientInfo :: PatientName -> Age -> Height -> String

patientInfo name age height = name2 ++ " " ++ ageHeight
  where name2 = firstName name ++ ", " ++ lastName name
        ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

data RhType = Pos | Neg

data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A  = "A"
showABO B  = "B"
showABO AB = "AB"
showABO O  = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _                = True
canDonateTo _ (BloodType AB _)               = True
canDonateTo (BloodType A _) (BloodType A _ ) = True
canDonateTo (BloodType B _) (BloodType B _)  = True
canDonateTo _ _                              = False --otherwise

type MiddleName = String
data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l)             = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l
data Sex = Male | Female

-- data Patient = Patient Name Sex Int Int Int BloodType

data Patient = Patient { name      :: Name
                       , sex       :: Sex
                       , age       :: Int
                       , height    :: Int
                       , weight    :: Int
                       , bloodType :: BloodType }

jackieSmith :: Patient
jackieSmith = Patient { name = Name "Jackie" "Smith"
                      , age = 43
                      , sex = Female
                      , height = 62
                      , weight = 115
                      , bloodType = BloodType O Neg }

getPatientBT :: Patient -> BloodType
getPatientBT p = bloodType p

canDonateTo' :: Patient -> Patient -> Bool
canDonateTo' = undefined


-- Ch12

class Describable a where
  describe :: a -> String

data SixSidedDie = S1
                 | S2
                 | S3
                 | S4
                 | S5
                 | S6 deriving (Show, Enum)

{-
instance Enum SixSidedDie where
  toEnum 0 = S1
  toEnum 1 = S2
  toEnum 2 = S3
  toEnum 3 = S4
  toEnum 4 = S5
  toEnum 5 = S6
  toEnum _ = error "No Such Value"
  fromEnum S1 = 0
  fromEnum S2 = 1
  fromEnum S3 = 2
  fromEnum S4 = 3
  fromEnum S5 = 4
  fromEnum S6 = 5
-}

{-
instance Show SixSidedDie where
  show S1 = "1"
  show S2 = "2"
  show S3 = "3"
  show S4 = "4"
  show S5 = "5"
  show S6 = "6"


instance Eq SixSidedDie where
  (==) S6 S6 = True
  (==) S5 S5 = True
  (==) S4 S4 = True
  (==) S3 S3 = True
  (==) S2 S2 = True
  (==) S1 S1 = True
  (==) _ _ = False

-}


-- type Name' = (String,String)

data Name' = Name' (String,String) deriving (Show, Eq)


names :: [Name']
names = [ Name' ("Emil", "Cioran")
        , Name' ("Eugene","Thacker")
        , Name' ("Friedrich", "Nietzsche")]




instance Ord Name' where
  compare (Name' (f1, l1)) (Name' (f2,l2)) = compare (l1,f1)(l2,f2)

