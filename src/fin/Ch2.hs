module Ch2 where

sumSquareOrSquareSum x y = let sumSquare = (x^2 + y^2)
                               squareSum = (x + y)^2
                            in
                             if sumSquare > squareSum
                                then sumSquare
                                else squareSum

compareLastNames name1 name2 = if lastName1 > lastName2
                               then GT
                               else if lastName1 < lastName2
                                 then LT
                                 else EQ
                                   where lastName1 = snd name1
                                         lastName2 = snd name2

sfOffice name = if lastName < "L"
                   then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
                   else nameText
                     ++ " - PO Box 1010 - San Francisco, CA, 94109"
                       where lastName = (snd name)
                             nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
  where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
  where nameText = snd name

getLocationFunction location = case location of
                                 "ny" -> nyOffice
                                 "sf" -> sfOffice
                                 "reno" -> renoOffice
                                 _ -> (\name -> (fst name) ++ " " ++ (snd name))


addressLetter name location = locationFunction name
  where locationFunction = getLocationFunction location

compareLastNames' name1 name2 = compare c1 c2
  where c1 = head (snd name1)
        c2 = head (snd name2)


