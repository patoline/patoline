open Typography
open CamomileLibrary
open Fonts.FTypes
open OutputCommon
open OutputPaper
open Util

let filename file=try (Filename.chop_extension file)^".bin" with _->file^".bin"

let output ?(structure:structure={name="";displayname=[];metadata=[];tags=[];
				  page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    pages fileName=

  let fileName = filename fileName in
  let ch = open_out_bin fileName in
  output_value ch pages;
  output_value ch structure;
  close_out ch;
  Printf.fprintf stderr "File %s written.\n" fileName;
  flush stderr
