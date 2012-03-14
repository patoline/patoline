val v_badness :
  'a Util.box array array ->
  float ->
  Util.line -> float -> float -> Util.line -> float -> float -> float
val h_badness : 'a Util.box array array -> Util.line -> float -> float
val badness :
  'a Util.box array array ->
  ?figures:Util.drawingBox array ->
  ?citations:(int * int) array ->
  Util.line -> Util.parameters -> Util.line -> Util.parameters -> float
