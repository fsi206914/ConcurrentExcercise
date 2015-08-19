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


object Excerses205 extends App{

  class SyncVar[T] {
    var elem: T = null.asInstanceOf[T]

    def getWait(): T = this.synchronized{
      while(isEmpty == true) this.wait();
      val ret = elem
      elem = null.asInstanceOf[T]
      this.notify()
      ret
    }

    def putWait(x: T): Unit = this.synchronized{
      while(isEmpty == false) this.wait();
      elem = x
      this.notify()
    }

    def isEmpty: Boolean = this.synchronized{
      if(elem == null) return true;
      else return false;
    }
  }

  val elem = new SyncVar[Int]
  MyThread.thread{
    for(i <- 0 until 15){
      elem.putWait(i)
    }
  }
  MyThread.thread{
    while(true){
      println("elem=" + elem.getWait())
    }
  }
}

object Excerses206 extends App{

  class SyncQueue[T](num: Int) {
    val queue: scala.collection.mutable.Queue[T] = new scala.collection.mutable.Queue()

    def getWait(): T = this.synchronized{
      while(isEmpty == true) this.wait();
      this.notify()
      queue.dequeue()
    }

    def putWait(x: T): Unit = this.synchronized{
      while(isFull == true) this.wait();
      queue += x;
      this.notify()
    }

    def isEmpty: Boolean = this.synchronized{
      if(queue.size == 0) return true;
      else return false;
    }
    def isFull: Boolean = this.synchronized{
      if(queue.size == num) return true;
      else return false;
    }
  }

  val elem = new SyncQueue[Int](5)
  MyThread.thread{
    for(i <- 0 until 15){
      elem.putWait(i)
    }
  }
  MyThread.thread{
    while(true){
      println("elem=" + elem.getWait())
    }
  }
}

object Excerses207 extends App {
  class Account(val name: String, var money: Int)

  def send(a: Account, b: Account, n: Int) = a.synchronized {
    b.synchronized {
      a.money -= n
      b.money += n
    }
  }

  def sendAll(accounts: Set[Account], target: Account): Unit = {
    for(acc<-accounts){
      acc.synchronized{
        target.synchronized{
          target.money += acc.money;
          acc.money = 0;
        }
      }
    }
  }
}

object Excerses208 extends App {

  import scala.math.Ordering
  import collection.mutable.PriorityQueue;

  class PriorityTaskPool(p:Int, important:Int) {
    val pq: PriorityQueue[(Int, () => Unit)] = new PriorityQueue()(Ordering.by(t2 => -t2._1))
    @volatile var terminated = false
    val workers = for(_ <- 0 until p) yield new java.lang.Thread {
      def poll() = pq.synchronized {
        while(pq.isEmpty && !terminated) pq.wait()
        println(this.getName)
        pq.headOption match {
          case Some(task) if !terminated || task._1 < important => Some(pq.dequeue())
          case _ => None
        }
      }

      override def run() = poll() match {
        case Some(task) => task._2(); run()
        case None => println("exit")
      }
    }
    def go(): Unit = workers.foreach(_.start())
    def asynchronous(priority:Int)(task: => Unit): Unit = pq.synchronized {
      pq.enqueue(priority -> (() => task))
      pq.notify()
    }
    def shutdown(): Unit = {
      terminated = true
      pq.synchronized { pq.notify() }
    }
  }

  val ptp = new PriorityTaskPool(2,2)
  ptp.asynchronous(3)(println(3))
  ptp.asynchronous(2)(println(2))
  ptp.asynchronous(1)(println(1))
  ptp.asynchronous(1)(println(1))
  ptp.asynchronous(2)(println(2))
  ptp.asynchronous(3)(println(3))
  ptp.asynchronous(-1)(println(-1))
  ptp.go()
  ptp.shutdown()

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