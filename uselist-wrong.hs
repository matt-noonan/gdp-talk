main :: IO ()
main = do
  args <- getArgs
  name args $ \args -> case args of
    Nil  -> putStrLn "No arguments!"
    Cons -> do
      print (head args)
      print (head (reverse args))
