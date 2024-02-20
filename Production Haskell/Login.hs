data Information = Information 
    {
        username::String,
        password::String
    } deriving (Show)

class Login info where
    connection:: info -> IO ()

instance Login Information where
    connection (
        Information
        username
        password ) =
        let name = username
            pass = password
        in
            if name == "Min Min" &&
               pass == "123456"
            then putStrLn $ "Welcome "++username++"!"
            else putStrLn $ "You don't have permission to access this file."
    
main :: IO ()
main = do
    putStrLn $ "Hello World!"
    connection (Information "Min Min" "123456")