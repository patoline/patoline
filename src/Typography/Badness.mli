val v_badness :
  float ->
  'a Box.box array ->
  int ->
  Line.parameters ->
  float -> 'b Box.box array -> int -> Line.parameters -> float -> float
val h_badness : 'a Box.box array array -> Line.line -> float -> float
