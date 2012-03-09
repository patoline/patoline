open Typography
open Parameters
open Fonts.FTypes
open Util
open Fonts
open Drivers
open Diag
let _=Random.self_init ()

let fig env=
  let (a,b) as u=([|0.; 10.; 20.;20.|], [|0.; 0.;10.;20.|]) in
  let (c,d) as v=([|0.; 0.; 10.;20.|], [|20.; 10.;0.;0.|]) in

  let cont=Glyph { glyph_x=10.;glyph_y=1.; glyph_size=5.; glyph_color=black;
                   glyph=(Fonts.loadGlyph env.font
                            { FTypes.empty_glyph with
                                FTypes.glyph_index=34}) }
  in
    drawing ~offset:(-10.) [
      Path ({ default with strokingColor=Some (RGB { red=0.;green=1.;blue=0. }); lineWidth=0.1 }, [| a,b |]);
      Path ({ default with lineWidth=0.1 }, [| c,d |]);
      cont
    ]

let fig2 env=
  let contents_a=[Glyph { glyph_x=10.;glyph_y=1.; glyph_size=5.; glyph_color=black;
                   glyph=(Fonts.loadGlyph env.font
                            { FTypes.empty_glyph with
                                FTypes.glyph_index=35}) }]
  in
  let contents_b=[Glyph { glyph_x=20.;glyph_y=30.; glyph_size=5.; glyph_color=black;
                   glyph=(Fonts.loadGlyph env.font
                            { FTypes.empty_glyph with
                                FTypes.glyph_index=43}) }]
  in
  let contents_c=[Glyph { glyph_x=50.;glyph_y=15.; glyph_size=3.; glyph_color=black;
                   glyph=(Fonts.loadGlyph env.font
                            { FTypes.empty_glyph with
                                FTypes.glyph_index=38}) }]
  in
  let state = [] in
  let params = { default with strokingColor=Some (RGB { red=0.;green=1.;blue=0. }); lineWidth=0.1 } in  
  let a, state = Node.make_draw state 
    ~contents:contents_a 
    ~parameters:params
    ~shape:(NodeShape.circle ~inner_sep:5.)
    () in
  let b, state = Node.make_draw state ~contents:contents_b ~parameters:params () in
  let c, state = Node.make_draw state 
    ~contents:contents_c 
    ~parameters:params
    ~shape:(NodeShape.flower ~amplitude:3.)
    () in
  let ab = Node.edge
    ~parameters:{ default with strokingColor=Some (RGB { red=0.;green=1.;blue=0. }); lineWidth=0.1 }
    ~controls:(Node.bend_left a b)
    a b
  in
  let ab' = Node.edge
    ~parameters:{ default with strokingColor=Some (RGB { red=0.7;green=0.7;blue=0.7 }); lineWidth=0.3 }
    ~controls:(Node.bend_right ~angle:60.0 a b)
    a b
  in
  let ac = Node.edge
    ~parameters:{ default with strokingColor=Some (RGB { red=0.7;green=0.7;blue=0.7 }); lineWidth=0.1 }
    ~controls:(Node.bend_right ~angle:10.0 a c)
    a c
  in
  let cb = Node.edge
    ~parameters:{ default with strokingColor=Some (RGB { red=0.;green=0.7;blue=0.7 }); lineWidth=0.2 }
    ~controls:(Node.bend_left ~angle:20.0 c b)
    c b
  in
    drawing ~offset:(-10.) (ab'::ab::ac::cb::state)

let fig3 env=
  let params = { default with strokingColor=Some (RGB { red=0.;green=1.;blue=0. }); lineWidth=0.1 } in  
  let bezier1 = ([| -1.0;30.0 |],[|-1.0;0.0|]) in
  let bezier2 = ([| 10.0;20.0 |],[|-10.0;20.0|]) in
  let p1 = Path (params, [| bezier1 |]) in
  let p2 = Path (params, [| bezier2 |]) in
  let l = 
    match Bezier.intersect bezier1 bezier2 with
      | [] -> []
      | (t,t') :: _ -> 
	let x = Bezier.eval (fst bezier1) t in
	let y = Bezier.eval (snd bezier1) t in
	[Glyph { glyph_x=x;glyph_y=y; glyph_size=3.; glyph_color=black;
                   glyph=(Fonts.loadGlyph env.font
                            { FTypes.empty_glyph with
                                FTypes.glyph_index=34}) }]
  in
    drawing ~offset:(-10.) (p1 :: p2 ::l)

let _=
  title "Bacon Ipsum";
  let params a b c line=
    let par=parameters a b c line in
      { par with
          measure=120.;
          left_margin=par.left_margin +. (par.measure-.120.)/.2. }
  in
  newPar (normal 120.) params ([font "AGaramondPro-Italic.otf" [T "Résumé."]; B (fun env->env.stdGlue); T "Bacon ipsum dolor sit amet ut bacon deserunt, eu pancetta aliqua ham hock sed pig pastrami elit et. Ribeye qui cillum sirloin, reprehenderit pork chop aliqua."; B (fun env->env.stdGlue); B (fun env->Drawing (fig2 env)); B (fun env->env.stdGlue);T "In pariatur laborum est chuck in, et commodo culpa excepteur tri-tip tenderloin. Occaecat meatball proident, labore ground round salami in sed beef ribs officia. Spare ribs qui sausage, beef et beef ribs strip steak leberkase."] @ (
                                 let rec f i=if i=0 then [] else (
                                   (B (fun env->env.stdGlue))::
                                     (FileRef ("tests/document.ml", 0x4f6, 20))::
                                     (f (i-1))
                                 ) in
                                   f 20
                               ));
  let ragged_left a b c line=
    let par=parameters a b c line in
      { par with measure=line.nom_width }
  in
  let ragged_right a b c line=
    let par=parameters a b c line in
    let m=line.nom_width in
      { par with
          measure=m;
          left_margin=par.left_margin+.par.measure-.m }
  in
  newStruct "About meatloaf";
  newPar (nominal 150.) ragged_right [glues [T "Meatloaf pork anim, ad pancetta dolore pastrami ribeye elit laborum. Cillum sint officia, id ham hock ad non cow. Pork belly pork chop swine bresaola velit drumstick, turducken ut beef ribs reprehenderit tongue enim meatloaf. Ea fatback esse flank fugiat shank, officia anim short loin swine bacon sunt. Occaecat mollit eiusmod prosciutto exercitation. Pig occaecat do, ut labore beef ribs ball tip prosciutto deserunt id in."]];
  newStruct "Pour aller danser le jerky...";
  newPar (nominal 150.) ragged_left [glues [T "Jerky quis excepteur, sunt aute reprehenderit commodo kielbasa turducken in et strip steak eu. Nostrud laborum veniam cillum, eu et tempor ball tip pork beef ribs ad dolore capicola sunt. Qui andouille sunt flank strip steak pastrami. Aute quis est, pork nostrud in et consectetur cillum brisket ribeye occaecat. Drumstick t-bone chuck, frankfurter velit mollit voluptate flank dolore andouille. Flank sirloin eu incididunt exercitation capicola."]];

  up();
  newStruct "Du hamburger";
  newPar (normal 150.) parameters [T "Hamburger ex spare ribs salami bresaola eu et, fatback id nostrud jowl turkey ut. Dolore kielbasa ham beef pastrami. Frankfurter pancetta magna, do meatloaf turducken jowl salami. Ad ribeye rump biltong swine, ut in pig drumstick flank strip steak cow. Aute eu boudin aliqua qui sed. Tail tongue duis, do irure culpa in bacon sint short loin nisi commodo fatback cillum nulla. Ex minim spare ribs do chuck." ];
  newPar (normal 150.) parameters [T "Venison tail consequat short loin beef ribs in, tenderloin reprehenderit officia. Pariatur pork belly swine strip steak kielbasa sausage. Minim aliqua non dolore, pariatur sunt qui chuck pastrami laborum tri-tip drumstick. Tail bresaola ut turducken beef ribs ut. Consequat rump pig strip steak, tri-tip swine aliqua ad ea short loin shoulder nulla ham turducken. Occaecat chicken ribeye pig, sirloin chuck ball tip laborum pork loin hamburger magna dolore."];

  up();
  up();
  newStruct "¡Viva el jamon!";
  newPar (normal 150.) parameters [
    size 2. [T "Nostrud bresaola fugiat magna consequat, do incididunt strip steak kielbasa turkey pancetta. Eu do sirloin, beef venison veniam chicken in officia t-bone pork belly. T-bone nulla aliquip pancetta bacon non. Reprehenderit in corned beef ham hock officia venison. Sunt ex non voluptate, chicken deserunt brisket. Duis ullamco pariatur meatloaf exercitation andouille in. Filet mignon velit sed shankle kielbasa."] ;

    size 10. [T "Flank kielbasa duis, hamburger exercitation venison eu drumstick. Dolor swine fatback biltong, ullamco boudin cillum shoulder jerky pariatur jowl qui ea short ribs. Velit ea pork qui pork loin. Biltong prosciutto in, sint in frankfurter elit duis bresaola andouille officia pork loin laboris commodo consequat. In tail pork loin officia."]];

  let gr=open_out "doc_graph" in
    doc_graph gr !str;
    close_out gr;

  let params,compl,pars=flatten defaultEnv !str in
  let (_,pages)=Typeset.typeset
    ~completeLine:compl
    ~parameters:params
    ~badness:(Badness.badness pars)
    pars
  in
  let u,v=Output.routine pars [||] defaultEnv pages in
    Drivers.Pdf.output ~structure:(make_struct v !str) u "test.pdf"
