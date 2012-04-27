open Typography
open Typography.Util
open Typography.Syntax
open Typography.Document

let math arg = [B (fun env0 -> List.map (fun b -> Boxes.resize env0.size b) (* math <$lala$> *)
  (let style = Maths.Text and _env = (Maths.env_style Maths.default Maths.Text) in 
   Maths.draw_maths Maths.default style ((arg ))))]

let _=macros:=StrMap.add "diagram" (fun x-> begin
  " (let module MaFigure (Arg : sig val env : user environment end) = struct \n" ^
    "module Lib = Env_Diagram (struct let arg1 = \"\" end) (Arg) \n open Lib \n" ^
    x ^
    "end in \n" ^
    "[B (fun env -> \n" ^
    "let module Res = MaFigure (struct let env = env end) in \n" ^
    "[ Drawing (Res.Lib.make ()) ])]) " end) !macros

