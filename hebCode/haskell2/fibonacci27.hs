fib_mem :: Int -> Integer
fib_mem = (map fib [0..] !!)
   where fib 0 = 1
         fib 1 = 1
         fib n = fib_mem (n-2) + fib_mem (n-1)

fib_mem_arg :: Int -> Integer
fib_mem_arg x = (map fib [0..] !!) x
   where fib 0 = 1
         fib 1 = 1
         fib n = fib_mem_arg (n-2) + fib_mem_arg (n-1)