open Typography
open Fonts.FTypes

let _=
  title "Bacon Ipsum";
  newPar [font "AGaramondPro-Italic.otf" [T "Résumé."]; B defaultEnv.stdGlue; T "Bacon ipsum dolor sit amet ut bacon deserunt, eu pancetta aliqua ham hock sed pig pastrami elit et. Ribeye qui cillum sirloin, reprehenderit pork chop aliqua. In pariatur laborum est chuck in, et commodo culpa excepteur tri-tip tenderloin. Occaecat meatball proident, labore ground round salami in sed beef ribs officia. Spare ribs qui sausage, beef et beef ribs strip steak leberkase."];

  newStruct "About meatloaf";
  newPar [T "Meatloaf pork anim, ad pancetta dolore pastrami ribeye elit laborum. Cillum sint officia, id ham hock ad non cow. Pork belly pork chop swine bresaola velit drumstick, turducken ut beef ribs reprehenderit tongue enim meatloaf. Ea fatback esse flank fugiat shank, officia anim short loin swine bacon sunt. Occaecat mollit eiusmod prosciutto exercitation. Pig occaecat do, ut labore beef ribs ball tip prosciutto deserunt id in."];

  newStruct "Pour aller danser le jerky...";
  newPar [add_features [OldStyleFigures] [T "Jerky quis excepteur, sunt aute reprehenderit commodo kielbasa turducken in et strip steak eu. Nostrud laborum veniam cillum, eu et tempor ball tip pork beef ribs ad dolore capicola sunt. Qui andouille sunt flank strip steak pastrami. Aute quis est, pork nostrud in et consectetur cillum brisket ribeye occaecat. Drumstick t-bone chuck, frankfurter velit mollit voluptate flank dolore andouille. Flank sirloin eu incididunt exercitation capicola."]];

  up();
  newStruct "Du hamburger";
  newPar [T "Hamburger ex spare ribs salami bresaola eu et, fatback id nostrud jowl turkey ut. Dolore kielbasa ham beef pastrami. Frankfurter pancetta magna, do meatloaf turducken jowl salami. Ad ribeye rump biltong swine, ut in pig drumstick flank strip steak cow. Aute eu boudin aliqua qui sed. Tail tongue duis, do irure culpa in bacon sint short loin nisi commodo fatback cillum nulla. Ex minim spare ribs do chuck." ];
  newPar [T "Venison tail consequat short loin beef ribs in, tenderloin reprehenderit officia. Pariatur pork belly swine strip steak kielbasa sausage. Minim aliqua non dolore, pariatur sunt qui chuck pastrami laborum tri-tip drumstick. Tail bresaola ut turducken beef ribs ut. Consequat rump pig strip steak, tri-tip swine aliqua ad ea short loin shoulder nulla ham turducken. Occaecat chicken ribeye pig, sirloin chuck ball tip laborum pork loin hamburger magna dolore."];

  up();
  up();
  defaultEnv.hyphenate <- (fun _ -> [||]) ;
  newStruct "¡Viva el jamon!";
  newPar [
    size 2. [T "Nostrud bresaola fugiat magna consequat, do incididunt strip steak kielbasa turkey pancetta. Eu do sirloin, beef venison veniam chicken in officia t-bone pork belly. T-bone nulla aliquip pancetta bacon non. Reprehenderit in corned beef ham hock officia venison. Sunt ex non voluptate, chicken deserunt brisket. Duis ullamco pariatur meatloaf exercitation andouille in. Filet mignon velit sed shankle kielbasa."] ;

    size 10. [T "Flank kielbasa duis, hamburger exercitation venison eu drumstick. Dolor swine fatback biltong, ullamco boudin cillum shoulder jerky pariatur jowl qui ea short ribs. Velit ea pork qui pork loin. Biltong prosciutto in, sint in frankfurter elit duis bresaola andouille officia pork loin laboris commodo consequat. In tail pork loin officia."]];






  (* doc_graph stdout !str; *)

  let pars=flatten defaultEnv !str in
  let (_,pages)=Typeset.typeset
    ~measure:measure
    ~parameters:(parameters pars [||])
    ~badness:(Badness.badness pars)
    pars
  in
  let u,v=Output.routine pars [||] defaultEnv pages in
    Drivers.Pdf.output ~structure:(make_struct v !str) u "test.pdf"
