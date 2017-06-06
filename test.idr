func1 : Int -> Int
func1 x = 2 * x

func2 : Int -> Int -> IO ()
func2 x y = do
  let z = func1 y
  print (z + x)

main: IO ()
main = do
  func2 3 2 
