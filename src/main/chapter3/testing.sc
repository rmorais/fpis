val l = List(1,2,3,4,5)
List.drop(l, 1)
List.drop(l, 4)
List.drop(Nil, 1)
List.drop(l, 0)

List.dropWhile(l, (x: Int) => x < 0)
List.dropWhile(l, (x: Int) => x < 3)
List.dropWhile(Nil, (x: Int) => x < 3)

List.dropWhile2(l, (x: Int) => x < 0)
List.dropWhile2(l, (x: Int) => x < 3)
List.dropWhile2(Nil, (x: Int) => x < 3)

List.init(l)
List.init(Nil)