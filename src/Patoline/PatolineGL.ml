open Arg
open GL

let spec = [
  "--rgb", Unit (fun () -> prefs := { !prefs with subpixel_anti_aliasing = RGB_SAA })
         , "Set subpixel anti aliasing for RGB lcd screens (default)";
  "--bgr", Unit (fun () -> prefs := { !prefs with subpixel_anti_aliasing = BGR_SAA })
         , "Set subpixel anti aliasing for BRG lcd screens";
  "--vrgb", Unit (fun () -> prefs := { !prefs with subpixel_anti_aliasing = VRGB_SAA })
         , "Set subpixel anti aliasing for VRGB lcd screens";
  "--vbgr", Unit (fun () -> prefs := { !prefs with subpixel_anti_aliasing = VBGR_SAA })
         , "Set subpixel anti aliasing for VBRG lcd screens";
  "--no-saa", Unit (fun () -> prefs := { !prefs with subpixel_anti_aliasing = No_SAA })
         , "Do not use subpixel anti aliasing";
  "--graisse", Float (fun x -> prefs := { !prefs with graisse = x })
         , (Printf.sprintf "Fixes the thickness of rendering (default: %.2f pixel), warning : -1 and 1 are big" !prefs.graisse);
  "--tesselation-factor", Float (fun x -> prefs := { !prefs with tesselation_factor = x })
         , (Printf.sprintf "Fixes the tessalation precision in pixels (default: %.2f)" !prefs.graisse);
  "--fit-width", Unit (fun () -> prefs := { !prefs with init_zoom = FitWidth })
         , "Start with fitting the page width in the window";
  "--fit-height", Unit (fun () -> prefs := { !prefs with init_zoom = FitHeight })
         , "Start with fitting the page height in the window";
  "--fit-page", Unit (fun () -> prefs := { !prefs with init_zoom = FitHeight })
         , "Start with fitting the page in the window";

]

let files = ref []

let _ = 
  Arg.parse spec (fun x->files := x::(!files)) (Printf.sprintf "Usage : %s [options] file.bin" Sys.argv.(0));
  match !files with
    [f] ->
      let ch = open_in f in
      let pages = input_value ch in
      close_in ch;
      output pages f
  | _ ->
    Printf.fprintf stderr "%s: more than one file given!" Sys.argv.(0)



