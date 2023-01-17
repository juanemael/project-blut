import System.Random 
import Text.Read (readMaybe)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))

main :: IO ()
main = mainMenu

data Admin = Admin {email :: String , username :: String , password :: String} 
type ListAdmin = [Admin]

-- menuPilihan :: ListMurid -> IO()
-- menuPilihan murid = do 
--     putStrLn "Masukkan nama murid: "
--     namaMurid <- getLine
--     print namaMurid 
--     putStrLn "Masukkan kelas: "
--     kelas <- getLine
--     print kelas
--     putStrLn "Masukkan nilai: "
--     nilai <- getLine
--     print nilai
--     let newValue = murid ++ [(Murid {nama = namaMurid, kelas = kelas, nilai = read nilai})]
--     print murid
--     main7 newValue

mainMenu = do
    putStrLn  "(Main Menu) Pilih menu:"
    menuOption <- getLine
    case menuOption of 
        "1" -> do
            registerMenu
        "2" -> do
            loginMenu

userMenu = do
    putStrLn  "(Donor Menu) Pilih menu:"
    putStrLn "1. Manage Donor Information "
    putStrLn "2. Show all of blood donor information"
    putStrLn "3. Logout"
    putStrLn "---------------------------------------------"
    putStr "Insert your option (1,2,3): "
    menuOption <- getLine
    case menuOption of
        "1" -> do
            donorMenu
        "2" -> do
            ioDonorToListDonor 
        "3" -> do
            loginMenu
insertDonor = do 
    putStrLn "Insert donor number ID (Number): "
    donorID <- getLine
    putStrLn  "Insert donor name (Text):"
    donorName <- getLine
    putStrLn  "Insert donor age (Number):"
    donorAge <- getLine
    putStrLn  "Insert donor weight (Number):"
    donorWeight <- getLine
    putStrLn  "Insert donor hemoglobin level (Number):"
    donorHemoglogbin <- getLine
    putStrLn  "Insert donor surgery status (True/False):"
    donorSurgery <- getLine
    putStrLn  "Insert donor tattoo (True/False):"
    donorTattoo <- getLine
    putStrLn  "Insert donor alcohol status (True/False):"
    donorAlcohol <- getLine
    putStrLn  "Insert donor caffeine status (True/False):"
    donorCaffeine <- getLine
    putStrLn  "Insert donor hiv status (True/False):"
    donorHiv <- getLine
    putStrLn  "Insert donor hepatitis status (True/False):"
    donorHepatitis <- getLine
    putStrLn  "Insert donor syphilis status (True/False):"
    donorSyphilis <- getLine
    putStrLn  "Insert donor malaria status (True/False):"
    donorMalaria <- getLine

    -- if(read donorHemoglogbin < 14 || read donorHemoglogbin > 18 || read donorBloodPressure < 120 || read donorWeight < 45 || read donorAge < 17 || read donorAge > 65 || donorSurgery == "False" || donorTattoo == "False" || donorAlcohol == "False" || donorCaffeine == "False" || donorHiv == "False" || donorHepatitis == "False" || donorSyphilis == "False" || donorMalaria == "False" ) 
    if(read donorHemoglogbin < 14 || read donorHemoglogbin > 18 || read donorWeight < 45 || read donorAge < 17 || read donorAge > 65 || donorSurgery == "False" || donorTattoo == "False" || donorAlcohol == "False" || donorCaffeine == "False" || donorHiv == "False" || donorHepatitis == "False" || donorSyphilis == "False" || donorMalaria == "False" ) 
        then 
            appendFile "./database_donor.txt" (donorID ++ " " ++ donorName ++ " " ++ donorAge ++ " " ++ donorWeight ++ " " ++ donorHemoglogbin ++ " " ++  donorTattoo ++ " " ++ donorSurgery ++ " " ++
                donorAlcohol ++ " " ++ donorCaffeine ++ " "  ++ donorHiv ++ " " ++ donorHepatitis ++ " " 
                ++ donorSyphilis  ++ " " ++ donorMalaria ++ " " ++ "False" ++ "\n")
    else 
         appendFile "./database_donor.txt" (donorID ++ " " ++ donorName ++ " " ++ donorAge ++ " " ++ donorWeight ++ " " ++ donorHemoglogbin ++ " " ++ donorTattoo ++ " " ++ donorSurgery ++ " " ++
                donorAlcohol ++ " " ++ donorCaffeine ++ " "  ++ donorHiv ++ " " ++ donorHepatitis ++ " " 
                ++ donorSyphilis  ++ " " ++ donorMalaria ++ " " ++ "True" ++ "\n")

-- if(donorMalaria == True) then     appendFile "./app/database_donor.txt" ( donorID ++ " " ++ donorName ++ " " ++ donorAge ++ " " ++ donorWeight ++ " " ++ donorTattoo ++ " " ++ donorSurgery ++ " " ++
--  donorAlcohol ++ " " ++ donorCaffeine ++ " " ++ donorBloodPressure ++ " " ++ donorHemoglogbin  ++ " " ++ donorHiv ++ " " ++ donorHepatitis ++ " " 
--  ++ donorSyphilis  ++ " " ++ donorMalaria ++ " " ++ verified ++ "\n") else 
    -- appendFile "./database_donor.txt" (donorID ++ " " ++ donorName ++ " " ++ donorAge ++ " " ++ donorWeight ++ " " ++ donorTattoo ++ " " ++ donorSurgery ++ " " ++
    --     donorAlcohol ++ " " ++ donorCaffeine ++ " " ++ donorBloodPressure ++ " " ++ donorHemoglogbin  ++ " " ++ donorHiv ++ " " ++ donorHepatitis ++ " " 
    --     ++ donorSyphilis  ++ " " ++ donorMalaria ++ " " ++ "True" ++ "\n")
    userMenu
  
-- randomizeID = x <- randomRIO (0,10)
--     print x 

test a = do 
    print a

-- readVerified :: MaybeT IO Bool 
readTattoo tattooStatus  = do
    if(tattooStatus == "False") 
        then return $ Just "True"
        else return Just "False"

readSurgery surgeryStatus  = do
    if(surgeryStatus == "False") 
        then return $ Just "True"
        else return Just "False"

readVerified :: [Char] -> [Char] -> String -> String -> String-> String-> String-> String-> String ->  String
readVerified surgeryStatus tattooStatus alcoholStatus caffeineStatus hivStatus hepatitisStatus syphilisStatus malariaStatus = do
    if(surgeryStatus == "False" || tattooStatus == "False" || alcoholStatus == "False" || caffeineStatus == "False" || hivStatus == "False" || hepatitisStatus == "False" || syphilisStatus == "False" || malariaStatus == "False" ) 
        then return "True"
        else return "False"
-- readVerified surgeryStatus  = do
--     if(surgeryStatus == "False" ) 
--         then return $ Just "True"
--         else return Nothing


donorMenu = do
    putStrLn  "(Donor Menu) Pilih menu:"
    putStrLn  "1. Insert Donor Information:"
    putStrLn  "2. Update Donor Information:"
    putStrLn  "3. Delete Donor Information:"
    putStrLn "---------------------------------------------"
    putStr "Insert your option (1,2,3): "
    menuOption <- getLine
    case menuOption of
        "1" -> do
            insertDonor
        -- "2" -> do
            -- updateDonor
        -- "3" -> do
            -- deleteDonor
        "4" -> do
            loginMenu
        -- "2. Validate your patient information " -> do
        --     registerPatient

loginMenu = do 
    putStrLn "Please insert your username admin: "
    usernameAdmin <- getLine
    putStrLn "Please insert your password: "
    passwordAdmin <- getLine
    -- readFile "database_admin.txt"
    -- admin <- readFile "./app/database_admin.txt" 
    -- putStrLn $ readFile "database_admin.txt"
    -- print $ words admin
    -- let x = words admin
    userMenu

registerMenu :: IO ()
registerMenu = do
    putStrLn "Please create your email admin: " 
    emailAdmin <- getLine
    putStrLn "Please create your username admin: "
    usernameAdmin <- getLine
    putStrLn "Please create your password: "
    passwordAdmin <- getLine
    -- let newValue = (Admin {email = emailAdmin, username = usernameAdmin, password = passwordAdmin})
    writeFile "./app/database_admin.txt" (emailAdmin ++ " " ++ usernameAdmin ++ " " ++ passwordAdmin)
    -- return email
    print "You have successfully registered!"

prompt :: String -> IO (IO ())
prompt x = do 
    putStr "What is your " >> putStrLn (x ++ " ?")
    >> getLine 
    >>= \a -> return (putStrLn ("Your " ++ x ++ " is: " ++ a))

-- validateDonor = do
--     a <- getDonorInfo
--     -- print $ words a
--     let b  = splitAt 1 a
--     -- print $ map b
--     print b
-- name :: String, 
-- age :: Int , weight :: Int, bloodPressure :: Int, hemoglobinLevel :: Int, 
-- tattoo :: Bool, surgery :: Bool, alcohol ::  Bool, caffeine :: Bool,  hiv :: Bool,
-- hepatitis :: Bool, syphilis :: Bool, malaria :: Bool, verified :: Bool

-- validateDonor Donor {name = nameStatus, age = ageStatus , weight = weightStatus, bloodPressure = bloodPressureStatus, hemoglobinLevel = hemoglobinLevelStatus , 
-- tattoo = tattooStatus, surgery = surgeryStatus, alcohol = alcoholStatus, caffeine = caffeineStatus, hiv = hivStatus,
-- hepatitis = hepatitisStatus, syphilis = syphilisStatus, malaria = malariaStatus, verified = False} = do
--     if (validateAge ageStatus == False || validateWeight weightStatus == False || validateBloodPressure bloodPressureStatus == False || 
--         validateHemoglobinLevel hemoglobinLevelStatus == False || validateTattoo tattooStatus == False ||
--         validateSurgery surgeryStatus == False || validateAlcohol alcoholStatus == False || validateCaffeine caffeineStatus == False || validateHIV hivStatus  == False||
--         validateHepatitis hepatitisStatus == False || validateSyphilis syphilisStatus == False || validateMalaria malariaStatus == False )
--         then return False
--             -- let x = Murid{verified = False}
--             --     return x
--     else
--         return True
        -- let x = Murid{verified = False}
        --     return x

-- validateName (Donor {name = name, verified = verified}) = do
--     if 

validateAge age = 
    if (age < 17 || age > 65) then False else True

validateWeight weight =
    if (weight < 45) then False else True

validateHemoglobinLevel hemoglobinLevel = do
    if (hemoglobinLevel < 14  || hemoglobinLevel > 18) then
        False
    else True
validateTattoo tattoo = do
    if (tattoo == False) then
        False
    else True
validateSurgery surgery = do
    if (surgery == False) then 
        False
    else True

validateAlcohol alcohol = do
    if (alcohol == False) then 
        False
    else True

validateCaffeine caffeine = do
    if (caffeine == False) then 
        False
    else True

validateHIV hiv = do
    if (hiv == False) then 
        False
    else True

validateHepatitis hepatitis = do
    if (hepatitis == False) then 
        False
    else True

validateSyphilis hepatitis = do
    if (hepatitis == False) then 
        False
    else True

validateMalaria malaria = do
    if (malaria == False) then 
        False
    else True
    

-- getDonorInfoNameText =  do
--     a <- getDonorInfo
--     let b  = words a
--     return $ head b

-- getDonorInfoIntBool =  do
--     a <- getDonorInfo
--     let b  = words a
--     return $ tail b
-- getDonorInfoIntBool =  do
--     a <- getDonorInfo
--     let b  = words a
--     return $ tail b

-- getDonorPattern [] = []
-- getDonorPattern (x:xs) 
--     | x > 0 = x : xs 
--     | (x == True || x == False) = getDonorPattern [] 
--     | otherwise = getDonorPattern xs
-- getDonorPattern _ = if(x >0) then x : xs else getDonorPattern xs
-- getDonorInfoIO :: IO [Donor]
-- getDonorInfoIO =  do
--     donor <- readFile "database_donor.txt"
--     let line_donor = lines donor
--     return $ head line_donor

-- arrayToObject :: [a] -> a
arrayToObject (x:xs) = x

-- ioToObject = do 
--     donorObject <- validateDonor2 
--     let object = validateDonor2a donorObject
--     return object


-- validateDonor2 = do 
--     stringDonor <- getDonorInfo 
--     let a = parseDonor stringDonor
    -- print a

-- validateDonor2 = do 
--     stringDonor <- getDonorInfo 
--     -- return $ parseDonor stringDonor
--     let a = parseDonor stringDonor
--     -- print a
--     return $ parseDonor stringDonor


validateDonor2a (x:xs) = x

validateDonor3 a = arrayToObject a >>= \b -> b

parseDonor :: String -> [Donor]
parseDonor content = do
    [donor_id', name', age', weight', hemoglobinLevel', tattoo', surgery', alcohol', caffeine', hiv', hepatitis', syphilis', malaria', verified' ] <- words <$> lines content
    Just age'' <- return $ readMaybe age'
    Just donor_id'' <- return $ readMaybe donor_id'
    Just weight'' <- return $ readMaybe weight'
    Just hemoglobinLevel'' <- return $ readMaybe hemoglobinLevel'
    tattoo'' <- if tattoo' == "True" || tattoo' == "true" || tattoo' == "TRUE" then return True else return False
    surgery'' <- if surgery' == "True" || surgery' == "true" || surgery' == "TRUE" then return True else return False
    alcohol'' <- if alcohol' == "True" || alcohol' == "true" || alcohol' == "TRUE" then return True else return False
    caffeine'' <- if caffeine' == "True" || caffeine' == "true" || caffeine' == "TRUE" then return True else return False
    hiv'' <- if hiv' == "True" || hiv' == "true" || hiv' == "TRUE" then return True else return False
    hepatitis'' <- if hepatitis' == "True" || hepatitis' == "true" || hepatitis' == "TRUE" then return True else return False
    syphilis'' <- if syphilis' == "True" || syphilis' == "true" || syphilis' == "TRUE" then return True else return False
    malaria'' <- if malaria' == "True" || malaria' == "true" || malaria' == "TRUE" then return True else return False
    verified'' <- if verified' == "True" || verified' == "true" || verified' == "TRUE" then return True else return False
    return (Donor {donor_id = donor_id'', name =name', age = age'' , weight = weight'', hemoglobinLevel = hemoglobinLevel'', 
                   tattoo = tattoo', surgery = surgery', alcohol = alcohol', caffeine = caffeine',  hiv = hiv',
                   hepatitis = hepatitis', syphilis = syphilis', malaria = malaria', verified = verified' })
    
    -- return (Donor {donor_id = donor_id'', name =name', age = age'' , weight = weight'', hemoglobinLevel = hemoglobinLevel'', 
    --                tattoo = tattoo'', surgery = surgery'', alcohol = alcohol'', caffeine = caffeine'',  hiv = hiv'',
    --                hepatitis = hepatitis'', syphilis = syphilis'', malaria = malaria'', verified = verified'' })
    

data Donor = Donor {donor_id :: Int, name :: String, 
age :: Int , weight :: Int, hemoglobinLevel :: Int, 
tattoo :: String, surgery :: String, alcohol ::  String, caffeine :: String,  hiv :: String,
hepatitis :: String, syphilis :: String, malaria :: String, verified :: String } deriving Show
type ListDonor = [Donor]
getDonorInfo :: IO [String]
getDonorInfo =  do
    donor <- readFile "database_donor.txt"
    let line_donor = lines donor
    -- return $ head line_donor
    return line_donor

-- getDonorInfoArr :: [String] -> IO ()
getDonorInfoArr [] = []
getDonorInfoArr (x:xs) = do 
    parseDonor x ++ getDonorInfoArr xs
    -- return a
    -- : getDonorInfoArr xs

stringToIODonor = do
    stringVar <- getDonorInfo3
    let ioDonor = getDonorInfoArr stringVar
    return ioDonor

ioDonorToListDonor = do 
    donor <- stringToIODonor
    printAllDonor donor

printAllDonor :: ListDonor -> IO ()
printAllDonor list = do 
    putStrLn ""
    putStrLn "---------------------------------------------------------------- --------------------- Report Data for All Donor --------------------- ----------------------------------------------------------------\n" >>
       putStrLn "\n| ID.\t| Name\t| Age\t| Weight\t| Hemoglobin\t| Tattooo\t| Surgery\t| Alcohol\t| Caffeine\t| Hiv\t| Hepatitis\t| Syphilis\t| Malaria\t| Verified\t|" >>

        recDonorData list


divideDonorData (Donor {donor_id = donor_id, name = name, age = age, weight = weight, hemoglobinLevel = hemoglobinLevel, tattoo = tattoo, surgery = surgery,
    alcohol = alcohol, caffeine = caffeine, hiv = hiv, hepatitis = hepatitis, syphilis = syphilis, malaria = malaria, verified = verified}) = do
         -- putStrLn "\n|" ++ donor_id ++ "\t|" ++ name ++ "\t|" ++ age ++ "\t|" ++ weight ++ "\t|" ++ hemoglobinLevel ++ "\t|" ++ tattoo ++ "\t|" ++ surgery ++ "\t|" ++ alcohol ++ "\t|" ++ caffeine ++ "\t|" ++ hiv "\t|" ++ hepatitis ++ "\t|" ++ syphilis ++ "\t|" ++ malaria ++ "\t|" ++ verified ++ "\t|"
        putStrLn ("\n| " ++ show donor_id ++ "\t| " ++ name ++ "\t| " ++ show age ++ "\t| " ++ show weight ++ "\t\t| " ++ show hemoglobinLevel ++ " \t\t| " ++ tattoo ++ "\t\t| " ++ surgery ++ "\t\t| " ++ alcohol ++ "\t\t| " ++ caffeine ++ "\t\t| " ++ hiv ++ "\t| " ++ hepatitis ++ "\t\t| " ++ syphilis ++ "\t\t| " ++ malaria ++ "\t\t| " ++ verified ++ "\t\t| ")


recDonorData :: [Donor] -> IO ()
recDonorData [] = print ""
recDonorData (x:xs) = do
    divideDonorData x
    recDonorData xs

-- getDonorInfoArr2 :: ListDonor -> IO ()
getDonorInfoArr2 donor = do
    ioToDonorArr <- stringToIODonor
    let a = donor ++ ioToDonorArr
    return a

getDonorInfo4 = do
    a <- getDonorInfo2
    -- let b = parseDonor a
    return $ a

getDonorInfo2 = do
    text <- readFile "database_donor.txt"
    let a = words text
    putStrLn("get string with words: ")
    return a

getDonorInfo3 = do
    text <- readFile "database_donor.txt"
    let b = lines text
    putStrLn("get string with lines: ")
    -- putStrLn(show b)
    return b

-- append :: [Int] -> [Int]
-- append = concatMap digs

-- digs :: Integral x => x -> [x]
-- digs 0 = []
-- digs x = digs (x `div` 10) ++ [x `mod` 10]
