open CamomileLibrary
open Printf
open Fonts.FTypes
open OutputCommon
open Box
open Line
open Document
open Util
module Buf=UTF8.Buf


type page = { mutable pageFormat:float*float; mutable pageContents:contents list }



module type Driver=sig
  val filename:string->string
  val output: ?structure:structure -> page array -> string -> unit
end
