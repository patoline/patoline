open Typography
open Typography.Config


let spec = [
("--extra-fonts-dir",Arg.String (fun x->Config.fontspath:=x::(!Config.fontspath)),
 "Adds directories to the font search path");
("--extra-driver-dir",Arg.String (fun x->Config.driverdir:=x::(!Config.driverdir)),
 "Adds directories to the driver search path");
("--extra-hyph-dir",Arg.String (fun x->Config.hyphenpath:=x::(!Config.hyphenpath)),
 "Adds directories to the search path for hyphenation dictionaries");
("--extra-plugins-dir",Arg.String (fun x->Config.pluginspath:=x::(!Config.pluginspath)), 
 "Adds directories to the plugins search path");
("--unicode-data",Arg.String (fun x->UnicodeLibConfig.datafile:=x), 
 "Specify the file where to find unicode data");
("-I",Arg.String (fun x->Config.local_path:=x::(!Config.local_path)),
 "Adds directories to the font search path");
("--at-most",Arg.Int (fun x->Config.atmost:=x),
 "Compile at most n times");
("--in",Arg.String (fun x->Config.input_bin := Some x),
 "input a .bin file instead of generating pages");
("--driver",Arg.String (fun x->Config.driver := Some x),
 "specify a driver to dynlink");
("--",Arg.Unit (fun () -> raise Exit),
 "Driver specific arguments")
]

let parse () = try Arg.parse spec ignore "Usage :" with Exit -> ();;
