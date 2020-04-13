object CachedValueTest2 {

  def main(args: Array[String]) = {
    val expr1 = Add(Add(Constant(1), Add(Variable(1), Constant(1))), Add(Constant(2), Constant(2)))
    println(extend(expr1))

    val expr2 = Add(Add(Constant(1), Add(Add(Variable(2), Constant(1)), Constant(1))), Add(Constant(2), Add(Variable(1), Constant(2))))
    println(extend(expr2))
  }

  def extend(value: Value): UsingCache = {
    def replace(input: Value, stack: List[(Add, Option[UsingCache], Option[UsingCache])], map: Map[Value, UsingCache]): UsingCache = {
      input match {
        case in @ Constant(d) =>
          val (v, newMap) = map.get(in) match {
            case Some(entry) => (entry, map)
            case None =>
              val entry = new Constant(d) with UsingCache
              (entry, map + (in -> entry))
          }
          popStack(v, stack, newMap)
        case in @ Variable(i) =>
          val (v, newMap) = map.get(in) match {
            case Some(entry) => (entry, map)
            case None =>
              val entry = new Variable(i) with UsingCache
              (entry, map + (in -> entry))
          }
          popStack(v, stack, newMap)
        case in @ Add(v1, v2) =>
          map.get(in) match {
            case Some(entry) => entry
            case None => replace(v1, (in, None, None) :: stack, map)
          }

      }
    }

    def popStack(input: UsingCache, stack: List[(Add, Option[UsingCache], Option[UsingCache])], map: Map[Value, UsingCache]): UsingCache = {
      stack match {
        case head :: tail =>
          head match {
            case (add, None, None) =>
              replace(add.v2, (add, Some(input), None) :: tail, map)
            case (add, Some(v1), None) =>
              val v = new Add(v1, input) with UsingCache
              val newMap = map + (add -> v)
              popStack(v, tail, newMap)
          }
        case Nil => input
      }
    }

    replace(value, List(), Map())
  }

  abstract class Value {
    def basicEvaluate(varArray: Array[Double]): Double

    def evaluate(varArray: Array[Double]) = basicEvaluate(varArray)
  }

  case class Constant(d: Double) extends Value {
    override def basicEvaluate(varArray: Array[Double]) = d
  }

  case class Variable(i: Int) extends Value {
    override def basicEvaluate(varArray: Array[Double]) = varArray(i)
  }

  case class Add(v1: Value, v2: Value) extends Value {
    override def basicEvaluate(varArray: Array[Double]) = v1.evaluate(varArray) + v2.evaluate(varArray)
  }

  trait UsingCache extends Value {
    var caches : Map[Array[Double], Double] = Map()

    override def evaluate(varArray: Array[Double]) = {
      caches.get(varArray) match {
        case Some(result) =>
          result
        case None =>
          val result = basicEvaluate(varArray)
          caches = caches + (varArray -> result)
          result
      }

    }
  }

}