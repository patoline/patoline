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
open Typography
open Fonts
open FTypes
open Typography.Document
open Util
open UsualMake
open Typography.ConfigUtil
open Typography.Box
open Printf

	  
module Format=functor (D:DocumentStructure)->struct

include FormatThese.Format(D)
let defaultEnv = { defaultEnv with hyphenate=DefaultFormat.hyphenate_dict "hyph-fr.hdict"}
			  
  module Env_defi=Default.Make_theorem
			  (struct
			    let refType="definition"
			    let counter="definition"
			    let counterLevel=0
			    let display num=alternative Bold [tT ("Définition "^num^"."); (tT " ")]
			  end)
  module Env_theoreme=Default.Make_theorem
			(struct
			  let refType="theoreme"
			  let counter="theoreme"
			  let counterLevel=0
			  let display num=alternative Bold [tT ("Théorème "^num^"."); (tT " ")]
			end)
  module Env_lemme=Default.Make_theorem
		     (struct
		       let refType="lemme"
		       let counter="lemme"
		       let counterLevel=0
		       let display num=alternative Bold [tT ("Lemme "^num^"."); (tT " ")]
		     end)
  module Env_prop=Default.Make_theorem
			   (struct
			     let refType="proposition"
			     let counter="proposition"
			     let counterLevel=0
			     let display num=alternative Bold [tT ("Proposition "^num^"."); (tT " ")]
			   end)
  module Env_corollaire=Default.Make_theorem
			  (struct
			    let refType="corollaire"
			    let counter="corollaire"
			    let counterLevel=0
			    let display num=alternative Bold [tT ("Corollaire "^num^"."); (tT " ")]
			  end)
  module Env_exemple=Default.Make_theorem
		       (struct
			 let refType="exemple"
			 let counter="exemple"
			 let counterLevel=0
			 let display num=alternative Bold [tT ("Exemple "^num^"."); (tT " ")]
		       end)
  module Env_hypothese=Default.Make_theorem
			 (struct
			   let refType="hypothese"
			   let counter="hypothese"
			   let counterLevel=0
			   let display num=alternative Bold [tT ("Hypothèse "^num^"."); (tT " ")]
			 end)
  module Env_remarque=Default.Make_theorem
			(struct
			  let refType="remarque"
			  let counter="remarque"
			  let counterLevel=0
			  let display num=alternative Bold [tT ("Remarque "^num^"."); (tT " ")]
			end)
  module Env_condition=Default.Make_theorem
			(struct
			  let refType="condition"
			  let counter="condition"
			  let counterLevel=0
			  let display num=alternative Bold [tT ("Condition "^num^"."); (tT " ")]
			end)
  module Env_notation=Default.Make_theorem
			(struct
			  let refType="notation"
			  let counter="notation"
			  let counterLevel=0
			  let display num=alternative Bold [tT ("Notation "^num^"."); (tT " ")]
			end)
  module Env_exercice=Default.Make_theorem
			(struct
			  let refType="exercice"
			  let counter="exercice"
			  let counterLevel=0
			  let display num=alternative Bold [tT ("Exercice "^num^"."); (tT " ")]
			end)
  module Env_preuve = Env_gproof (struct 
				   let arg1 = italic [tT "Preuve.";bB (fun env->let w=env.size in [glue w w w])]
				 end)

end
