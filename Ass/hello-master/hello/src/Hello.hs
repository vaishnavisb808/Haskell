module Hello(sayHello) where
sayHello ::String -> IO()
sayHello name =do
  putStrLn ("Hello "++ name)
--sayHi :: IO()
--sayHi = do  
  --putStrLn "hi"