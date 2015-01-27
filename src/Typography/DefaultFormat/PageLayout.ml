open Typography

type pageMaster =
  {
    paperWidth: float;
    paperHeight: float;
    marginTop: float;
    marginBottom: float;
    marginLeft: float;
    marginRight: float;
  }

type master = pageMaster array

let default =
  let w = fst Util.a4
  and h = snd Util.a4 in
  {
    paperWidth = w;
    paperHeight = h;
    marginTop = h /. 6.;
    marginBottom = h /. 6.;
    marginLeft = w /. 6.;
    marginRight = w /. 6.;
  }

let inner_frame master ((page, _) as zip) =
  Box.(frame
    (page.frame_x0 +. master.marginLeft)
    (page.frame_y0 +. master.marginTop)
    (page.frame_x1 -. master.marginRight)
    (page.frame_y1 -. master.marginBottom)
    zip)

let new_page master zip =
  let ((page, _) as zip) = Box.(make_page (master.paperWidth, master.paperHeight) (frame_top zip)) in
  inner_frame master zip

let lr_new_page master zip =
  let (page, zip') as zip = Box.(make_page (master.paperWidth, master.paperHeight) (frame_top zip)) in
  let pagenum = match zip' with
  | (pagenum, _) :: _ -> pagenum
  | _ -> assert false (* After Box.make_page, zip contains at least the new frame *)
  in
  if pagenum mod 2 = 0
  then
    (* We are on a right page *)
    inner_frame master zip
  else
    (* We are on a left page *)
    inner_frame { master with
        marginRight = master.marginLeft;
        marginLeft = master.marginRight;
      }
      zip

let default_new_page = new_page default
