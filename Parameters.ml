open Util



let normal measure paragraphs line allow_impossible=
  let rec break_next j sum_min sum_nom sum_max result=
    if j>=Array.length paragraphs.(line.paragraph) then (
      if sum_min<=measure || allow_impossible then (
        { line with lineEnd=j; min_width=sum_min; nom_width=sum_nom; max_width=sum_max }::result
      ) else result
    ) else (
      let a,b,c=box_interval paragraphs.(line.paragraph).(j) in
      let result0=
        match paragraphs.(line.paragraph).(j) with
            Hyphen x ->(
              let rec hyphenation k res=
                if k>=Array.length x.hyphenated then res else (
                  let (a',b',c')=boxes_interval (fst x.hyphenated.(k)) in
                    if sum_min+.a' <= measure && sum_max+.c' >= measure then
                      hyphenation (k+1)
                        ({ line with lineEnd=j; hyphenEnd=k; min_width=sum_min+.a';
                             nom_width=sum_nom+.b'; max_width=sum_max+.c';
                         }::res)
                    else hyphenation (k+1) res)
              in
                hyphenation 0 result)
          | _ -> result
      in
        if sum_max >= measure || allow_impossible then
          match paragraphs.(line.paragraph).(j) with

              Glue _ when (sum_min <= measure || allow_impossible) && j>line.lineStart ->
                break_next (j+1) (sum_min+.a) (sum_nom+.b) (sum_max+.c)
                  ({ line with lineEnd=j; hyphenEnd=(-1); min_width=sum_min;
                       nom_width=sum_nom; max_width=sum_max
                   }::(if allow_impossible then [] else result0))

            | Glue _ when allow_impossible && result0=[] && j>line.lineStart ->
                [{ line with lineEnd=j; hyphenEnd=(-1); min_width=sum_min;
                     nom_width=sum_nom; max_width=sum_max
                 }]

            | _ when sum_min<=measure || (allow_impossible && result0=[]) ->
                break_next (j+1) (sum_min+. a) (sum_nom+.b) (sum_max+. c) result0
            | _ -> result0
        else
          break_next (j+1) (sum_min+. a) (sum_nom+.b) (sum_max+. c) result0
    )
  in
    if line.hyphenStart>=0 then (
      match paragraphs.(line.paragraph).(line.lineStart) with
          Hyphen x->let a,b,c=boxes_interval (snd x.hyphenated.(line.hyphenStart)) in
            break_next (line.lineStart+1) a b c []
        | _->break_next line.lineStart 0. 0. 0. []
    ) else break_next line.lineStart 0. 0. 0. []

let greedy measure paragraphs line allow_impossible=
  let rec break_next j sum_min sum_nom sum_max result=
    if j>=Array.length paragraphs.(line.paragraph) then (
      if sum_min<=measure || allow_impossible then (
        [{ line with lineEnd=j; min_width=sum_min; nom_width=sum_nom; max_width=sum_max }]
      ) else (
        result
      )

    ) else (
      let a,b,c=box_interval paragraphs.(line.paragraph).(j) in
        if sum_max >= measure then
          match paragraphs.(line.paragraph).(j) with

              Glue _ when sum_nom <= measure->
                break_next (j+1) (sum_min+.a) (sum_nom+.b) (sum_max+.c)
                  [{ line with lineEnd=j; hyphenEnd=(-1); min_width=sum_min;
                       nom_width=sum_nom; max_width=sum_max
                   }]

            | Glue _ when allow_impossible ->
                [{ line with lineEnd=j; hyphenEnd=(-1); min_width=sum_min;
                     nom_width=sum_nom; max_width=sum_max
                 }]

            | _ when sum_nom<=measure || allow_impossible ->
                break_next (j+1) (sum_min+. a) (sum_nom+.b) (sum_max+. c) result
            | _ -> result
        else
          break_next (j+1) (sum_min+. a) (sum_nom+.b) (sum_max+. c) result
    )
  in
    if line.hyphenStart>=0 then (
      match paragraphs.(line.paragraph).(line.lineStart) with
          Hyphen x->let a,b,c=boxes_interval (snd x.hyphenated.(line.hyphenStart)) in
            break_next (line.lineStart+1) a b c []
        | _->break_next line.lineStart 0. 0. 0. []
    ) else break_next line.lineStart 0. 0. 0. []
