

let rec loopa lx ly = 
    if (ly < 10) then
        loopa (lx + 1) (ly + 1)
    else lx

let rec loopb bx by =
    if (bx > 0) then 
        loopb (bx - 1) (bx - 1)
    else by > (-1)

let main (mm:unit(*-:{v:Unit | unit}*)) =
  let x = 0 in
  let y = 0 in

  let ansa = loopa x y in
  assert (loopb ansa ansa = true)

let _ = main()