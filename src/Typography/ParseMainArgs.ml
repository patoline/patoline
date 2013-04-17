open Typography
open Typography.Util
open Typography.Config

let _args=ref []
let _driver=ref "Pdf"
let _atmost=ref 3
let _spec = [("--extra-fonts-dir",Arg.String (fun x->Config.fontspath:=x::(!Config.fontspath)),"Adds directories to the font search path");
("--extra-hyph-dir",Arg.String (fun x->Config.hyphenpath:=x::(!Config.hyphenpath)), "Adds directories to the search path for hyphenation dictionaries");
("--extra-plugins-dir",Arg.String (fun x->Config.pluginspath:=x::(!Config.pluginspath)), "Adds directories to the plugins search path");
("-I",Arg.String (fun x->Config.local_path:=x::(!Config.local_path)), "Adds directories to the font search path");
("--at-most",Arg.Int (fun x->_atmost:=x),"Compile at most n times");
("--",Arg.Rest (fun x->_args:=x:: !_args),"Other arguments")
]

let _=Arg.parse _spec ignore "Usage :";;
