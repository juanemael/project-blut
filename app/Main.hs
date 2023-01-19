import System.Random 
import System.Directory
import Text.Read (readMaybe)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import GHC.IO.Handle (hClose,hGetContents)
import Control.Monad (when)
import System.IO.Unsafe 
import GHC.IO.IOMode (IOMode(ReadMode))
import System.IO  

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
    putStrLn "1. Register Account"
    putStrLn "2. Login Account"
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
    putStrLn "Insert donor ID number (NIK) (Number): "
    nik <- getLine
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
            appendFile "./database_donor.txt" (nik ++ " " ++ donorName ++ " " ++ donorAge ++ " " ++ donorWeight ++ " " ++ donorHemoglogbin ++ " " ++  donorTattoo ++ " " ++ donorSurgery ++ " " ++
                donorAlcohol ++ " " ++ donorCaffeine ++ " "  ++ donorHiv ++ " " ++ donorHepatitis ++ " " 
                ++ donorSyphilis  ++ " " ++ donorMalaria ++ " " ++ "False" ++ "\n")
    else 
         appendFile "./database_donor.txt" (nik ++ " " ++ donorName ++ " " ++ donorAge ++ " " ++ donorWeight ++ " " ++ donorHemoglogbin ++ " " ++ donorTattoo ++ " " ++ donorSurgery ++ " " ++
                donorAlcohol ++ " " ++ donorCaffeine ++ " "  ++ donorHiv ++ " " ++ donorHepatitis ++ " " 
                ++ donorSyphilis  ++ " " ++ donorMalaria ++ " " ++ "True" ++ "\n")
    userMenu
  
donorMenu = do
    putStrLn  "(Donor Menu) Pilih menu:"
    putStrLn  "1. Insert Donor Information"
    putStrLn  "2. Show Selected Donor Information"
    putStrLn  "3. Delete Donor Information"
    putStrLn  "4. Logout"
    putStrLn "---------------------------------------------"
    putStr "Insert your option (1,2,3): "
    menuOption <- getLine
    case menuOption of
        "1" -> do
            insertDonor
        "2" -> do
            putStr "Insert Donor ID Number (NIK): " 
            nik <- getLine
            printSelectedDonor $ read nik
        "3" -> do
            ioDonorToListDonor
            putStr "Select Donor ID Number to be Deleted (NIK): " 
            nik <- getLine
            combineDeleteToWrite $ read nik
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
    -- donor <- readFile "database_donor.txt"
    -- let line_donor = lines donor
    -- return line_donor
    -- admin <- readFile "database_admin.txt" 
    -- putStrLn $ readFile "database_admin.txt"
    -- print $ words admin
    -- let x = words admin
    userMenu

parseAdmin :: String -> [Admin]
parseAdmin content = do
    [ email', username', password' ] <- words <$> lines content
    -- Just age'' <- return $ readMaybe age'
    -- Just nik'' <- return $ readMaybe nik'
    -- Just weight'' <- return $ readMaybe weight'
    -- Just hemoglobinLevel'' <- return $ readMaybe hemoglobinLevel'
    -- verified'' <- if verified' == "True" || verified' == "true" || verified' == "TRUE" then return True else return False
    return (Admin { email = email', username = username', password = password' })

getAdminInfoArr [] = []
getAdminInfoArr (x:xs) = do 
    parseAdmin x ++ getAdminInfoArr xs

stringToIOAdmin = do
    stringVar <- getAdminInfo
    let ioAdmin = getAdminInfoArr stringVar
    return ioAdmin

getAdminInfo = do
    text <- readFile "database_admin.txt"
    let res = lines text
    return res

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

parseDonor :: String -> [Donor]
parseDonor content = do
    [nik', name', age', weight', hemoglobinLevel', tattoo', surgery', alcohol', caffeine', hiv', hepatitis', syphilis', malaria', verified' ] <- words <$> lines content
    Just age'' <- return $ readMaybe age'
    Just nik'' <- return $ readMaybe nik'
    Just weight'' <- return $ readMaybe weight'
    Just hemoglobinLevel'' <- return $ readMaybe hemoglobinLevel'
    -- verified'' <- if verified' == "True" || verified' == "true" || verified' == "TRUE" then return True else return False
    return (Donor {nik = nik'', name =name', age = age'' , weight = weight'', hemoglobinLevel = hemoglobinLevel'', 
                   tattoo = tattoo', surgery = surgery', alcohol = alcohol', caffeine = caffeine',  hiv = hiv',
                   hepatitis = hepatitis', syphilis = syphilis', malaria = malaria', verified = verified' })
    
data Donor = Donor {nik :: Int, name :: String, 
age :: Int , weight :: Int, hemoglobinLevel :: Int, 
tattoo :: String, surgery :: String, alcohol ::  String, caffeine :: String,  hiv :: String,
hepatitis :: String, syphilis :: String, malaria :: String, verified :: String } deriving (Show,Eq)
type ListDonor = [Donor]

getDonorInfo :: IO [String]
getDonorInfo =  do
    donor <- readFile "database_donor.txt"
    let line_donor = lines donor
    return line_donor

getDonorInfoArr [] = []
getDonorInfoArr (x:xs) = do 
    parseDonor x ++ getDonorInfoArr xs

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
       putStrLn "\n| ID.\t| Name                      \t\t\t| Age\t| Weight\t| Hemoglobin\t| Tattooo\t| Surgery\t| Alcohol\t| Caffeine\t| Hiv\t| Hepatitis\t| Syphilis\t| Malaria\t| Verified\t|" >>

        recDonorData list

lookupParameter (Donor {nik = nik, name = name, age = age, weight = weight, hemoglobinLevel = hemoglobinLevel, tattoo = tattoo, surgery = surgery,
    alcohol = alcohol, caffeine = caffeine, hiv = hiv, hepatitis = hepatitis, syphilis = syphilis, malaria = malaria, verified = verified}) = do
    return (nik,(Donor {nik = nik, name = name, age = age, weight = weight, hemoglobinLevel = hemoglobinLevel, tattoo = tattoo, surgery = surgery,
    alcohol = alcohol, caffeine = caffeine, hiv = hiv, hepatitis = hepatitis, syphilis = syphilis, malaria = malaria, verified = verified}))

printSelectedDonor nik = do 
    let res = deleteDonor nik 
    paramDonor <- res
    printSelectedDonor2 paramDonor

printSelectedDonor2 (Donor {nik = nik, name = name, age = age, weight = weight, hemoglobinLevel = hemoglobinLevel, tattoo = tattoo, surgery = surgery,
    alcohol = alcohol, caffeine = caffeine, hiv = hiv, hepatitis = hepatitis, syphilis = syphilis, malaria = malaria, verified = verified}) = 
        do
            putStrLn "--------------------- Report Data for Selected Donor ---------------------\n" 
            putStr"Donor Blood Status: " 
            if verified == "True" then print "Verified" else print "Not Verified"
            putStr"Donor ID Number (NIK): " 
            print nik
            putStr "Donor Name: " 
            print name 
            putStr "Donor Age: " 
            print age
            putStr "Donor Weight: " 
            print weight
            putStr "Donor Hemoglobin Level: " 
            print hemoglobinLevel
            putStr "Donor Tattoo Status: " 
            if tattoo == "True" then print "Has Tattoo" else print "No Tattoo"
            putStr "Donor Surgery Status: " 
            if surgery == "True" then print "Had Surgery in 12 Months" else print "Had no Surgery in 12 Months"
            putStr "Donor Alcohol Status: " 
            if alcohol == "True" then print "Had Alcohol for the last 24 hours" else print "No Alcohol for the last 24 hours"
            putStr "Donor Caffeine Status: "
            if caffeine == "True" then print "Had Caffeine for the last 24 hours" else print "No Caffeine for the last 24 hours" 
            putStr "Donor HIV Status: " 
            if hiv == "True" then print "Positive HIV" else print "Negative HIV" 
            putStr "Donor Hepatitis Status: " 
            if hepatitis == "True" then print "Positive Hepatitis" else print "Negative Hepatitis" 
            putStr "Donor Syphilis Status: " 
            if syphilis == "True" then print "Positive Syphilis" else print "Negative Syphilis" 
            putStr "Donor Malaria Status: " 
            if malaria == "True" then print "Positive Malaria" else print "Negative Malaria" 

chooseDonor nik = do
    arr <- stringToIODonor
    let arr2 = lookupParameterArray arr
    return $ lookup nik arr2

deleteDonor a = do
    result <- chooseDonor a
    case result of 
        Just a -> return a
        Nothing -> return $ Donor {nik = 0, name = "asd", age = 0, weight = 0, hemoglobinLevel = 0, tattoo = "True", surgery = "True", alcohol = "True", caffeine = "True", hiv = "True", hepatitis = "True", syphilis = "True", malaria = "True", verified = "True"}

lookupParameterArray [] = []
lookupParameterArray (x:xs) = do
    lookupParameter x ++ lookupParameterArray xs

deleteFromDonorDataArray _ [] = []
deleteFromDonorDataArray x (y:ys) | x == y = deleteFromDonorDataArray x ys
                                  | otherwise = y : deleteFromDonorDataArray x ys

ioDonorToDeleteDonor var = do
    io <- deleteDonor var
    list <- stringToIODonor
    let res = deleteFromDonorDataArray io list
    return res

writeDeletedData [] = [] 
writeDeletedData (x:xs) = x  ++ writeDeletedData xs

combineDelete var = do
    arr <- ioDonorToDeleteDonor var
    let res = dataToString2 arr
    return res

combineDeleteToWrite var = do
    arrVar <- combineDelete var
    -- let concatStr = writeDeletedData arrVar
    let concatStr = writeDeletedData arrVar
    print concatStr
    writeFile "database_donor.txt" concatStr
    putStrLn "Data has been successfully deleted and saved! "
    -- removeFile "database_donor.txt"
    -- renameFile "database_donor2.txt" "database_donor3.txt"
    -- putStrLn test

dataToString Donor {nik = nik, name = name, age = age, weight = weight, hemoglobinLevel = hemoglobinLevel, tattoo = tattoo, surgery = surgery,
    alcohol = alcohol, caffeine = caffeine, hiv = hiv, hepatitis = hepatitis, syphilis = syphilis, malaria = malaria, verified = verified} = 
        return (show nik ++ " " ++ name ++ " " ++ show age ++ " " ++ show weight ++ " " ++ show hemoglobinLevel ++ " " ++  tattoo ++ " " ++ surgery ++ " " ++
                alcohol ++ " " ++ caffeine ++ " "  ++ hiv ++ " " ++ hepatitis ++ " " 
                ++ syphilis  ++ " " ++ malaria ++ " " ++ verified ++ "\n")
dataToString2 [] = [] 
dataToString2 (x:xs) = 
        dataToString x ++ dataToString2 xs

divideDonorData (Donor {nik = nik, name = name, age = age, weight = weight, hemoglobinLevel = hemoglobinLevel, tattoo = tattoo, surgery = surgery,
    alcohol = alcohol, caffeine = caffeine, hiv = hiv, hepatitis = hepatitis, syphilis = syphilis, malaria = malaria, verified = verified}) = do
        putStrLn ("\n| " ++ show nik ++ "\t| " ++ name ++ "                      \t\t\t| " ++ show age ++ "\t| " ++ show weight ++ "\t\t| " ++ show hemoglobinLevel ++ " \t\t| " ++ tattoo ++ "\t\t| " ++ surgery ++ "\t\t| " ++ alcohol ++ "\t\t| " ++ caffeine ++ "\t\t| " ++ hiv ++ "\t| " ++ hepatitis ++ "\t\t| " ++ syphilis ++ "\t\t| " ++ malaria ++ "\t\t| " ++ verified ++ "\t\t| ")

recDonorData :: [Donor] -> IO ()
recDonorData [] = print ""
recDonorData (x:xs) = do
    divideDonorData x
    recDonorData xs

getDonorInfoArr2 donor = do
    ioToDonorArr <- stringToIODonor
    let res = donor ++ ioToDonorArr
    return res

getDonorInfo2 = do
    text <- readFile "database_donor.txt"
    let res = words text
    putStrLn("get string with words: ")
    return res

getDonorInfo3 = do
    renameFile "database_donor.txt" "database_donor2.txt"
    text <- readFile "database_donor2.txt"
    let res = lines text
    let ioDonor = getDonorInfoArr res
    let arrVar = dataToString2 ioDonor
    let concatStr = writeDeletedData arrVar
    writeFile "database_donor.txt" concatStr
    putStrLn("get string with lines: ")
    print res
    return res

stringToIODonorDatabase2 var = do
    stringVar <- getDonorInfo3
    let ioDonor = getDonorInfoArr stringVar
    let arrVar = dataToString2 ioDonor
    -- let concatStr = writeDeletedData arrVar
    let concatStr = writeDeletedData arrVar
    writeFile "database_donor.txt" concatStr
    combineDeleteToWrite var
