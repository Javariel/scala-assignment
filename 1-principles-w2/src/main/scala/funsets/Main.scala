package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  // Union
  println(contains(union(x => x < 3, y=> y > 10), 1))
  println(contains(union(x => x < 3, y=> y > 10), 5))
  // Intersect
  println(contains(intersect(x => x > 3, y=> y < 10), 5))
  println(contains(intersect(x => x > 3, y=> y < 10), 5))
  // diff
  println(contains(diff(x => x > 3, y=> y < 10), 15))
  println(contains(diff(x => x > 3, y=> y < 10), 7))

  // filter
  println(contains(filter(x => x > 3, y=> y < 10), 15))
  println(contains(filter(x => x > 3, y=> y < 10), 7))

  // forall
//  printSet(x => x > 3)
  println("Forall:" + forall(x => x > 10, y => y > 5))
  println("Forall:" + forall(x => x > 2, y => y > 5))
  println("Forall:" + forall(x => x < 2, y => y < 5))
  println("Forall:" + forall(x => x < 10, y => y < 5))
  // exists
  println(exists(x => x < 10, y => y < 5))
  println(exists(x => x < 5, y => y < 10))

  // map
  println(contains(map(x => x < 5, y => y + 2), 0))
  println(contains(map(x => x < 5, y => y + 2), 7))
  println(contains(map(x => x < 5, y => y + 2), 10))


}
