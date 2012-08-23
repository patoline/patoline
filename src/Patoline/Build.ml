open Util
exception No_rule of string

let known_targets=ref StrMap.empty
let mut_targets=Mutex.create()
let j=ref 1
type known=Known of Mutex.t | Not_known of Mutex.t

let known x=
  Mutex.lock mut_targets;
  let resp=
    try
      let mut=StrMap.find x !known_targets in
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
            mut_signal:Mutex.t }

let sem_up mut=
  Mutex.lock mut.mut_value;
  mut.value<-mut.value+1;
  (if mut.value>0 then Mutex.unlock mut.mut_signal);
  Mutex.unlock mut.mut_value

let sem_down mut=
  Mutex.lock mut.mut_signal;
  Mutex.lock mut.mut_value;
  mut.value<-mut.value-1;
  (if mut.value>0 then Mutex.unlock mut.mut_signal);
  Mutex.unlock mut.mut_value

let sem_create x=
  let mut=Mutex.create () in
  (if x=0 then Mutex.lock mut);
  {mut_value=Mutex.create ();
   mut_signal=mut;
   value=max 0 x }

let sem_set mut y=
  Mutex.lock mut.mut_signal;
  Mutex.lock mut.mut_value;
  mut.value<-y;
  (if mut.value>0 then Mutex.unlock mut.mut_signal);
  Mutex.unlock mut.mut_value

let sem=sem_create 1
let command cmd=
  sem_down sem;
  let err=Sys.command cmd in
  sem_up sem;
  err

type rule_t=Node of rule_t*rule_t | Leaf of (string->bool)

let rules=ref (Leaf (fun _->false))
let append_rule r=rules:=Node(!rules,Leaf r)
let macros:(string->string) StrMap.t ref=ref StrMap.empty
let rec build_with_rule r h=
  match known h with
      Known m->(Mutex.lock m;Mutex.unlock m)
    | Not_known m->(
      (try
         if not (r h) then
           raise (No_rule h);
         Mutex.unlock m;
       with
           e->(Mutex.unlock m;raise e));
    )

let rec build h=
  match known h with
      Known m->(Mutex.lock m;Mutex.unlock m)
    | Not_known m->(
      (try
         let rec apply_rules t=match t with
             Leaf l->l h
           | Node (a,b)->
             (let aa=apply_rules a in
              if aa then true else
                let bb=apply_rules b in
                bb)
         in
         (if not (apply_rules !rules) then raise (No_rule h));
         Mutex.unlock m;
       with
           e->(Mutex.unlock m;raise e));
    )
