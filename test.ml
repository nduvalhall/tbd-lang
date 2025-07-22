let pos = (12, 24, 45)
let () = match pos with x, y, z -> failwith ""
let f pos = match pos with x, y, z -> failwith ""
let f (x, y, z) = failwith ""
let () = f (1, 2, 3)
