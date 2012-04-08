module Config=Config
module Fonts=Fonts
module Util=Util
module Break=Break
module Document=Document
module DefaultFormat=DefaultFormat
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
