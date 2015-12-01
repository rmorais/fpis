val p = Process.liftOne((x: Int) => x * 4)
val xs = p(Stream(1,2,3)).toList

val r = Process.lift((x: Int) => x * 4)
val xss = r(Stream(1,2,3)).toList

val units = Stream.continually(())
val ones = Process.lift((_:Unit) => 1)(units)

val even = Process.filter((x: Int) => x % 2 == 0)
val evens = even(Stream(1,2,3,4)).toList

val s = Process.sum(Stream(1.0, 2.0, 3.0, 4.0)).toList


