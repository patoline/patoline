let readInt f n0=
  let rec readInt_ n x=
    if n=n0 then x else
      readInt_ (n+1) ((x lsl 8) + (int_of_char (input_char f)))
  in
    readInt_ 0 0
      
let rec pow n=
  if n<=0 then 1 else
    n*(pow n-1)
