module Config=Config
module IntMap=Binary.IntMap
module StrMap=Binary.StrMap
module Fonts=Fonts
module Util=Util
module Break=Break
module Document=Document
module DefaultFormat=functor (D:Document.DocumentStructure)->DefaultFormat.DefaultFormat(D)
module Hyphenate=Hyphenate
module Maths=Maths
module Badness=Badness
module Output=struct
  module Paper=struct
    include OutputPaper
    module Pdf=OutputPaper.Output(Pdf)
  end
  module Common=OutputCommon
end
