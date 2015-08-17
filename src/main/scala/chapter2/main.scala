package chapter2;

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
        while(elem.isEmpty == false)elem.wait()
        elem.put(i)
        elem.notify()
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

object Excerses204Answer extends App{

  // 4
  class SyncVar2[T] {
    private var state: Option[T] = None
    def get(): T = state.synchronized {
      state match {
        case None => throw new IllegalArgumentException()
        case Some(x) =>
          val tmp: T = x
          state = None // if state.notify() was called afterwards, an IllegalMonitorStateExc would be thrown as the current thread does not own the object monitor anymore; it was lost on assigning None to state
          tmp
      }
    }
    def put(el: T): Unit = state.synchronized {
      state match {
        case None => state = Some(el)
        case Some(x) => throw new IllegalArgumentException()
      }
    }
    def isEmpty: Boolean = state.synchronized {
      state match {
        case None => true
        case _ => false
      }
    }
    def nonEmpty: Boolean = !isEmpty
  }

   val sv2 = new SyncVar2[Int]
   val tprod = MyThread.thread {
      sv2.synchronized{
        for(i <- 0 until 15){
        println("before producer")
        while(sv2.nonEmpty)sv2.wait()
        sv2.put(i)
        println("during producer")
        sv2.notify()
        println("after producer")
      }
    }
   }
   val tcons = MyThread.thread {

     def go(): Unit = {
       var get: Int = 0
       sv2.synchronized {
         while (sv2.isEmpty) sv2.wait()
         get = sv2.get()
         sv2.notify()
       }
       println(get)
       if (get < 14) go()
     }
     go()
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