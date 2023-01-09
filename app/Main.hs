import System.Random 
import Text.Read (readMaybe)

main :: IO ()
main = mainMenu

-- login :: IO ()
-- login = 

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
    menuOption <- getLine
    putStrLn "1. Manage Donor Information "
    putStrLn "2. Validate Donor "
    putStrLn "3. Show all of blood donor information"
    case menuOption of
        "1" -> do
            insertDonor
        "2" -> do
            validateDonor
        "3" -> do
            loginMenu

-- data Donor = Donor {name :: String, age :: Int , weight :: Int, tattoo :: Bool, 
-- surgery :: Bool, alcohol ::  Bool, 
-- caffeine :: Bool, bloodPressure :: Int, hemoglobinLevel :: Int, hiv :: Bool,
-- hepatitis :: Bool, syphilis :: Bool, malaria :: Bool } deriving Show

insertDonor = do
    putStrLn  "Insert donor name (Text):"
    donorName <- getLine
    putStrLn  "Insert donor age (Number):"
    donorAge <- getLine
    putStrLn  "Insert donor weight (Number):"
    donorWeight <- getLine
    putStrLn  "Insert donor blood pressure (Number):"
    donorBloodPressure <- getLine
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

    appendFile "./app/database_donor.txt" ( donorName ++ " " ++ donorAge ++ " " ++ donorWeight ++ " " ++ donorTattoo ++ " " ++ donorSurgery ++ " " ++
     donorAlcohol ++ " " ++ donorCaffeine ++ " " ++ donorBloodPressure ++ " " ++ donorHemoglogbin  ++ " " ++ donorHiv ++ " " ++ donorHepatitis ++ " " 
     ++ donorSyphilis  ++ " " ++ donorMalaria ++ "\n")
    userMenu
  
-- randomizeID = x <- randomRIO (0,10)
--     print x 

donorMenu = do
    putStrLn  "(Donor Menu) Pilih menu:"
    menuOption <- getLine
    putStrLn  "1. Insert Donor Information:"
    putStrLn  "2. Update Donor Information:"
    putStrLn  "3. Delete Donor Information:"
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
    print "asdasd"

prompt :: String -> IO (IO ())
prompt x = do 
    putStr "What is your " >> putStrLn (x ++ " ?")
    >> getLine 
    >>= \a -> return (putStrLn ("Your " ++ x ++ " is: " ++ a))

validateDonor = do
    a <- getDonorInfo
    -- print $ words a
    let b  = splitAt 1 a
    -- print $ map b
    print b

getDonorInfoNameText =  do
    a <- getDonorInfo
    let b  = words a
    return $ head b

getDonorInfoIntBool =  do
    a <- getDonorInfo
    let b  = words a
    return $ tail b
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

validateDonor2 = do 
    stringDonor <- getDonorInfo 
    let a = parseDonor stringDonor
    return a 

parseDonor :: String -> [Donor]
parseDonor content = do
    [name', age', weight', bloodPressure', hemoglobinLevel', tattoo', surgery', alcohol', caffeine', hiv', hepatitis', syphilis', malaria' ] <- words <$> lines content
    Just age'' <- return $ readMaybe age'
    Just weight'' <- return $ readMaybe weight'
    Just bloodPressure'' <- return $ readMaybe bloodPressure'
    Just hemoglobinLevel'' <- return $ readMaybe hemoglobinLevel'
    tattoo'' <- if tattoo' == "True" || tattoo' == "true" || tattoo' == "TRUE" then return True else return False
    surgery'' <- if surgery' == "True" || surgery' == "true" || surgery' == "TRUE" then return True else return False
    alcohol'' <- if alcohol' == "True" || alcohol' == "true" || alcohol' == "TRUE" then return True else return False
    caffeine'' <- if caffeine' == "True" || caffeine' == "true" || caffeine' == "TRUE" then return True else return False
    hiv'' <- if hiv' == "True" || hiv' == "true" || hiv' == "TRUE" then return True else return False
    hepatitis'' <- if hepatitis' == "True" || hepatitis' == "true" || hepatitis' == "TRUE" then return True else return False
    syphilis'' <- if syphilis' == "True" || syphilis' == "true" || syphilis' == "TRUE" then return True else return False
    malaria'' <- if malaria' == "True" || malaria' == "true" || malaria' == "TRUE" then return True else return False
    return (Donor {name =name', age = age'' , weight = weight'', bloodPressure = bloodPressure'', hemoglobinLevel = hemoglobinLevel'', 
                   tattoo = tattoo'', surgery = surgery'', alcohol = alcohol'', caffeine = caffeine'',  hiv = hiv'',
                   hepatitis = hepatitis'', syphilis = syphilis'', malaria = malaria'' })


data Donor = Donor {name :: String, 
age :: Int , weight :: Int, bloodPressure :: Int, hemoglobinLevel :: Int, 
tattoo :: Bool, surgery :: Bool, alcohol ::  Bool, caffeine :: Bool,  hiv :: Bool,
hepatitis :: Bool, syphilis :: Bool, malaria :: Bool } deriving Show
getDonorInfo =  do
    donor <- readFile "database_donor.txt"
    let line_donor = lines donor
    return $ head line_donor

append :: [Int] -> [Int]
append = concatMap digs

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]
