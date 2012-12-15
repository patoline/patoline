open Typography.Document
open Typography.OutputCommon
let output ?(structure:structure={name="";displayname=[];metadata=[];tags=[];
				  page= -1;struct_x=0.;struct_y=0.;substructures=[||]})
    (pages:Typography.OutputPaper.page array) (fileName:string)=
  ()
let output' = output_to_prime output
