top =
  let len xs = case xs of 
                 [] -> 0 ;
                 (y:ys) -> 1 + len ys
  in 
  let l = [1, 2, 3, 4] in
  len l 
