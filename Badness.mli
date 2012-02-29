val v_badness :
  Util.box array array ->
  float ->
  Util.line -> float -> float -> Util.line -> float -> float -> float
val h_badness : Util.box array array -> Util.line -> float -> float
val badness :
  Util.box array array ->
  ?figures:'a array ->
  ?citations:(int * int) array ->
  Util.line -> Util.parameters -> Util.line -> Util.parameters -> float
