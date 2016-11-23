package CodeEval.Easy

/**
  * Time:           755 ms
  * Memory:         9957376 bytes
  * Unique:         Yes
  * Ranking Points: 25.370
  */
class FibonacciSeriesV1(args : Array[String]) {
  val source = scala.io.Source.fromFile(args(0))
  for (l <- source.getLines.filter(_.length > 0)) {
    println(fib(l.toInt))
  }

  def fib(n : Int) : Int = {
    def fibImpl(n : Int, f : Int, s : Int): Int = n match {
      case 0 => f
      case _ => fibImpl(n - 1, s, f + s)
    }
    fibImpl(n, 0, 1)
  }
}
