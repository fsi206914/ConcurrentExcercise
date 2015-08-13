

object Excerses201 extends App{

  def parallel[A, B](a: =>A, b: =>B)(implicit eva: Null <:< A, evb: Null <:< B): (A, B) ={

    var resA: A = null
    var resB: B = null

    val tA = Thread.thread {
      resA = a
    }
    val tB = Thread.thread {
      resB = b
    }
    tA.join(); tB.join()
    (resA, resB)

  }
}

object Thread {
  def thread(body: => Unit):java.lang.Thread = {
    val t = new Thread {
      override def run() = body
    }
    t.start()
    t
  }
}