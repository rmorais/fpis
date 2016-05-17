exercises.fib(0) == 0
exercises.fib(1) == 1
exercises.fib(2) == 1
exercises.fib(3) == 2
exercises.fib(4) == 3
exercises.fib(5) == 5
exercises.fib(6) == 8

val ascOrder: (Int, Int) => Boolean = _ <= _
val descOrder: (Int, Int) => Boolean = _ >= _
val sortedArray = Array(1,2,3,4,5)
val descSortedArray = Array(5,4,3,2,1)
val notSortedArray = Array(3,1,2,4,5)

println(s"isSorted(sortedArray, ascOrder): ${exercises.isSorted(sortedArray, ascOrder)}")
println(s"isSorted(notSortedArray, ascOrder): ${exercises.isSorted(notSortedArray, ascOrder)}")
println(s"isSorted(descSortedArray, descOrder): ${exercises.isSorted(descSortedArray, descOrder)}")
println(s"isSorted(notSortedArray, descOrder): ${exercises.isSorted(notSortedArray, descOrder)}")