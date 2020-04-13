
object CachedValueTest1 {

  def main(args: Array[String]) = {
    val expr = Add(Add(Constant(1), Add(Variable(1), Constant(1))), Add(Constant(2), Constant(2)))
    println(extend(expr))
  }

  def extend(value: Value): UsingCache = {
    def addToStack(input: Value, stack: List[Value with UsingCache]): List[Value with UsingCache] = {
      input match {
        case Constant(d) =>
          val head = new CachedConstant(d)
          head :: stack
        case Variable(i) =>
          val head = new CachedVariable(i)
          head :: stack
        case Add(v1, v2) =>
          val pushStack2 = addToStack(v2, stack)
          val pushStack1 = addToStack(v1, pushStack2)
          val head1 = pushStack1.head
          val head2 = pushStack1.tail.head
          val head = new CachedAdd(head1, head2)
          head :: stack
      }
    }

    addToStack(value, List()).head
  }


  abstract class Value {
    def basicEvaluate(varArray: Array[Double]): Double

    def evaluate(varArray: Array[Double]) = basicEvaluate(varArray)
  }

  case class Constant(d: Double) extends Value {
    override def basicEvaluate(varArray: Array[Double]) = d
  }

  case class CachedConstant(d: Double) extends Value with UsingCache {
    override def basicEvaluate(varArray: Array[Double]) = d
  }

  case class Variable(i: Int) extends Value {
    override def basicEvaluate(varArray: Array[Double]) = varArray(i)
  }

  case class CachedVariable(i: Int) extends Value with UsingCache {
    override def basicEvaluate(varArray: Array[Double]) = varArray(i)
  }

  case class Add(v1: Value, v2: Value) extends Value {
    override def basicEvaluate(varArray: Array[Double]) = v1.evaluate(varArray) + v2.evaluate(varArray)
  }

  case class CachedAdd(v1: Value, v2: Value) extends Value with UsingCache {
    override def basicEvaluate(varArray: Array[Double]) = v1.evaluate(varArray) + v2.evaluate(varArray)
  }

  trait UsingCache extends Value {
    var cached: Option[Double] = None

    override def evaluate(varArray: Array[Double]) = {
      if (cached == None) {
        cached = Some(basicEvaluate(varArray))
      }
      cached.get
    }
  }

}