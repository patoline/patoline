(*
  Copyright Tom Hirschowitz, Florian Hatat, Pierre-Etienne Meunier,
  Christophe Raffalli and others, 2012.

  This file is part of Patoline.

  Patoline is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Patoline is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Patoline.  If not, see <http://www.gnu.org/licenses/>.
*)
open CamomileLibrary
open Printf
open Fonts.FTypes
open OutputCommon
open Box
open Document
open Util
module Buf=UTF8.Buf


type page = { mutable pageFormat:float*float; mutable pageContents:raw list }

let defaultPage={pageFormat=(0.,0.);pageContents=[]}

module type Driver=sig
  val output: ?structure:structure -> page array -> string -> unit
end
