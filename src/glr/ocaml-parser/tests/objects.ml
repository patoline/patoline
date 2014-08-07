class counter =
object
  val mutable n = 0
  method incr = n <- n+1
  method get = n
end
