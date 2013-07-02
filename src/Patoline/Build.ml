(*
  Copyright Florian Hatat, Tom Hirschowitz, Pierre Hyvernat,
  Pierre-Etienne Meunier, Christophe Raffalli, Guillaume Theyssier 2012.

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
open Util



let mpids=Mutex.create ()
let pids=ref IntSet.empty
let stop_all ()=
  Mutex.lock mpids;
#ifndef __WINDOWS__
  IntSet.iter (fun x->
    (* Printf.fprintf stderr "killing child %d\n" x;flush stderr; *)
    Unix.kill x Sys.sigkill;
  ) !pids;
#endif
  Mutex.unlock mpids

exception No_rule of string
exception Circular_build of (string*(string list))
let known_targets=ref StrMap.empty
let mut_targets=Mutex.create()
let j=ref 1



type known=Known of Mutex.t | Not_known of Mutex.t

let known x=
  Mutex.lock mut_targets;
  let resp=
    try
      let mut=StrMap.find x !known_targets in
      known_targets:=StrMap.add x mut !known_targets;
      Known mut
    with
        Not_found->(
          let m=Mutex.create () in
          known_targets:=StrMap.add x m !known_targets;
          Mutex.lock m;
          Not_known m
        )
  in
  Mutex.unlock mut_targets;
  resp

type sem= { mut_value:Mutex.t;
            mutable value:int;
            mut_signal:Condition.t }

let sem_up mut=
  Mutex.lock mut.mut_value;
  mut.value<-mut.value+1;
  (if mut.value>0 then Condition.signal mut.mut_signal);
  Mutex.unlock mut.mut_value

let sem_down mut=
  Mutex.lock mut.mut_value;
  while mut.value<=0 do
    Condition.wait mut.mut_signal mut.mut_value
  done;
  mut.value<-mut.value-1;
  (if mut.value>0 then Condition.signal mut.mut_signal);
  Mutex.unlock mut.mut_value

let sem_create x=
  {mut_value=Mutex.create ();
   mut_signal=Condition.create ();
   value=max 0 x }

let sem_set mut y=
  Mutex.lock mut.mut_value;
  while mut.value<=0 do
    Condition.wait mut.mut_signal mut.mut_value
  done;
  mut.value<-y;
  (if mut.value>0 then Condition.signal mut.mut_signal);
  Mutex.unlock mut.mut_value

let sem=sem_create 1


let command ?builddir:(builddir=".") cmd args=
  sem_down sem;
  Mutex.lock mpids;
  let a_in,a_out=Unix.pipe () in
  let b_in,b_out=Unix.pipe () in
  let c_in,c_out=Unix.pipe () in
  let pid=Unix.fork () in
  if pid=0 then (
      Unix.chdir builddir;
      Unix.dup2 Unix.stdin a_in;
      Unix.dup2 Unix.stdout b_out;
      Unix.dup2 Unix.stderr c_out;
      Unix.execvp cmd args;
  ) else (

    Unix.close a_in;
    Unix.close a_out;
    Unix.close b_out;
    Unix.close c_out;
    pids:=IntSet.add pid !pids;
    Mutex.unlock mpids;

    let buf_out=Rbuffer.create 1000 in
    let buf_err=Rbuffer.create 1000 in

    let str=String.create 1000 in
    let rec read_all chans=
      if chans<>[] then (
        let a,b,c=Unix.select chans [] chans (-.1.) in
        match a,c with
            ha::_ , _->(
              let x=Unix.read ha str 0 (String.length str) in
              (if ha==b_in then (
              (* output stdout str 0 x;flush stdout; *)
                Rbuffer.add_substring buf_out str 0 x
               ) else
                  Rbuffer.add_substring buf_err str 0 x);
              if x>0 then
                read_all chans
              else (
                Unix.close ha;
                read_all (List.filter (fun ch->not (ch==ha)) chans)
              )
            )
          | _, hc::_->(
            read_all (List.filter (fun ch->not (ch==hc)) chans)
          )
          | [],[]->(
            read_all chans
          )
      )
    in
    read_all [b_in;c_in];

    let _,stat=Thread.wait_pid pid in

    Rbuffer.output_buffer stdout buf_out;
    Rbuffer.output_buffer stderr buf_err;
    Mutex.lock mpids;
    pids:=IntSet.remove pid !pids;
    Mutex.unlock mpids;

    sem_up sem;
    match stat with
        Unix.WEXITED err->
          if err=0 then 0 else (
            stop_all ();
            1
          )
      | Unix.WSIGNALED _
      | Unix.WSTOPPED _->(stop_all ();1)
  )

type rule_t=Node of rule_t*rule_t | Leaf of (string->string->bool)

let rules=ref (Leaf (fun _ _->false))
let append_rule r=rules:=Node(!rules,Leaf r)
let macros:(string->string) StrMap.t ref=ref StrMap.empty

let rec build_with_rule ?builddir:(builddir=".") r hs=
  match hs with
      []->()
    | h::s->
      match known h with
          Known m->(Mutex.lock m;Mutex.unlock m)
        | Not_known m->(
          (try
             if not (r builddir hs) then
               raise (No_rule h);
             Mutex.unlock m;
           with
               e->(Mutex.unlock m;raise e));
        )
