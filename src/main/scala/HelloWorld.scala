

object Excerses201 extends App{

  def parallel[A, B](a: =>A, b: =>B)(implicit eva: Null <:< A, evb: Null <:< B): (A, B) ={
    var resA: A = null
    var resB: B = null

    val tA = MyThread.thread {
      resA = a
    }
    val tB = MyThread.thread {
      resB = b
    }
    tA.join(); tB.join()
    (resA, resB)
  }
}

object Excerses202 extends App{

  def periodically(duration: Long)(b: =>Unit): Unit ={
    while(true){
      Thread.sleep(duration);
      MyThread.thread(b);
    }
  }
}

object Excerses203 extends App{

  class SyncVar[T] {
    var elem: T = null.asInstanceOf[T]

    def get(): T = this.synchronized{
      if(elem == null) throw new Exception("some Exception");
      val ret = elem
      elem = null.asInstanceOf[T]
      ret
    }

    def put(x: T): Unit = this.synchronized{
      if(elem != null) throw new Exception("some Exception");
      else elem = x
    }

    def isEmpty: Boolean = this.synchronized{
      if(elem == null) return true;
      else return false;
    }

  }

  val elem = new SyncVar[Int]
  MyThread.thread{
    for(i <- 0 until 15){
      elem.synchronized{
        println("before producer")
        elem.wait()
        elem.put(i)
        println("during producer")
        elem.notify()
        println("after producer")
      }
    }
  }
  MyThread.thread{
    while(true){
      elem.synchronized{
        println("before wait")
        elem.wait()
        println("after wait")
        if(elem.isEmpty == false)
          println("elem=" + elem.get())
        elem.notify()
      }
    }
  }
}

object MyThread {
  def thread(body: => Unit):java.lang.Thread = {
    val t = new Thread {
      override def run() = body
    }
    t.start()
    t
  }
}