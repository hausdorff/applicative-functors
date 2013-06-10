object mapfunc {
  println("Let's do wacky things with lists")     //> Let's do wacky things with lists
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  
  
  // Hold a list of functions of no arguments producing A
  class SeqF[A] (l : Seq[()=>A]) {
    def map[B](f : A =>B) = new SeqF(l.map( x => () => f(x())))
    def scanLeft[B](b0 : B)(f : (B,A)=>B) : SeqF[B] = {
      val l2 = l.scanLeft( ()=>b0 )( (b,a) => () => f(b(),a()) )
      new SeqF[B](l2)
    }
    def values() = l.map(_())
    def futs() : Seq[Future[A]] = l.tail.scanLeft( future{l.head.apply()} )  ( (f:Future[A],x:()=>A) =>  f.map( _ => x() ))
  }

  class SeqFMaker[A](l : Seq[A]) {
    def wrapf() = new SeqF[A](l.map( x => () => x) )
  }

  implicit def SeqToSeqFMaker[A](l : Seq[A]) = new SeqFMaker[A](l)
  //> SeqToSeqFMaker: [A](l: Seq[A])mapfunc.SeqFMaker[A]

  def add1(i:Int) = {println(s"adding 1 to $i"); 1 +i}
  //> add1: (i: Int)Int
  def mult2(i:Int) = {println(s"multiplying $i by 2");2*i}
  //> mult2: (i: Int)Int

  // Plain old ints. Note that operations are staged.
  (1 to 5).map(add1).map(mult2).scanLeft(0)(_+_)
                                                  //> adding 1 to 1
                                                  //| adding 1 to 2
                                                  //| adding 1 to 3
                                                  //| adding 1 to 4
                                                  //| adding 1 to 5
                                                  //| multiplying 2 by 2
                                                  //| multiplying 3 by 2
                                                  //| multiplying 4 by 2
                                                  //| multiplying 5 by 2
                                                  //| multiplying 6 by 2
                                                  //| res0: scala.collection.immutable.IndexedSeq[Int] = Vector(0, 4, 10, 18, 28,
                                                  //|  40)


  // Now a view.  Operations are composed (interleaved)
  val vv = (1 to 5).view.map(add1).map(mult2).scanLeft(0)(_+_)
                                                  //> adding 1 to 1
                                                  //| multiplying 2 by 2
                                                  //| adding 1 to 2
                                                  //| multiplying 3 by 2
                                                  //| adding 1 to 3
                                                  //| multiplying 4 by 2
                                                  //| adding 1 to 4
                                                  //| multiplying 5 by 2
                                                  //| adding 1 to 5
                                                  //| multiplying 6 by 2
                                                  //| vv  : scala.collection.SeqView[Int,Seq[_]] = SeqViewMMC(...)

  // Is there a way to get the
  vv(0)                                     //> res1: Int = 0
  val vvf = vv.map(x => future {x+0})       //> vvf  : scala.collection.SeqView[scala.concurrent.Future[Int],Seq[_]] = SeqV
  //| iewMMCM(...)
  vvf.map(_.value)                          //> res2: scala.collection.SeqView[Option[scala.util.Try[Int]],Seq[_]] = SeqVie
                                            //| wMMCMM(...)

  vv.force                                  //> res3: Seq[Int] = Vector(0, 4, 10, 18, 28, 40)

  // Now use wrapf so that each operation is wrapped in ()=>op.  Operations will now be both interleaved and deferred
  val v1 =   (1 to 5).wrapf.map(add1).map(mult2).scanLeft(0)(_+_)
                                                  //> v1  : mapfunc.SeqF[Int] = mapfunc$$anonfun$main$1$SeqF$1@6b9547
                                                  
  // Nothing yet, as expected.  Trigger the calc.
  v1.values()                             //> adding 1 to 1
                                                  //| multiplying 2 by 2
                                                  //| adding 1 to 1
                                                  //| multiplying 2 by 2
                                                  //| adding 1 to 2
                                                  //| multiplying 3 by 2
                                                  //| adding 1 to 1
                                                  //| multiplying 2 by 2
                                                  //| adding 1 to 2
                                                  //| multiplying 3 by 2
                                                  //| adding 1 to 3
                                                  //| multiplying 4 by 2
                                                  //| adding 1 to 1
                                                  //| multiplying 2 by 2
                                                  //| adding 1 to 2
                                                  //| multiplying 3 by 2
                                                  //| adding 1 to 3
                                                  //| multiplying 4 by 2
                                                  //| adding 1 to 4
                                                  //| multiplying 5 by 2
                                                  //| adding 1 to 1
                                                  //| multiplying 2 by 2
                                                  //| adding 1 to 2
                                                  //| multiplying 3 by 2
                                                  //| adding 1 to 3
                                                  //| multiplying 4 by 2
                                                  //| adding 1 to 4
                                                  //| multiplying 5 by 2
                                                  //| adding 1 to 5
                                                  //| multiplying 6 by 2
                                                  //| res4: Seq[Int] = Vector(0, 4, 10, 18, 28, 40)

  // Now add a delay
  val v1d = v1.map(x => {Thread.sleep(5); x.toDouble})
                                                  //> v1d  : mapfunc.SeqF[Double] = mapfunc$$anonfun$main$1$SeqF$1@e914c8

  // And further wrap the outermost composed functions in chained futures that get busy.
  val v2 = v1d.futs()                             //> v2  : Seq[scala.concurrent.Future[Double]] = Vector(scala.concurrent.impl.P
                                                  //| romise$DefaultPromise@970e7c, scala.concurrent.impl.Promise$DefaultPromise@
                                                  //| 137fc87, scala.concurrent.impl.Promise$DefaultPromise@a41d56, scala.concurr
                                                  //| ent.impl.Promise$DefaultPromise@8209d7, scala.concurrent.impl.Promise$Defau
                                                  //| ltPromise@1543f09, scala.concurrent.impl.Promise$DefaultPromise@1f6929c)

  // If we print out the resolutions of the futures immediately, we see that nothing's been done yet
  v2.map(_.value)                           //> res5: Seq[Option[scala.util.Try[Double]]] = Vector(None, None, None, None, 
                                                  //| None, None)

  Thread.sleep(10)                          //> adding 1 to 1
                                                  //| multiplying 2 by 2
  v2.map(_.value)                           //> res6: Seq[Option[scala.util.Try[Double]]] = Vector(Some(Success(0.0)), None
                                                  //| , None, None, None, None)
  Thread.sleep(50)                          //> adding 1 to 1
                                                  //| multiplying 2 by 2
                                                  //| adding 1 to 2
                                                  //| multiplying 3 by 2
                                                  //| adding 1 to 1
                                                  //| multiplying 2 by 2
                                                  //| adding 1 to 2
                                                  //| multiplying 3 by 2
                                                  //| adding 1 to 3
                                                  //| multiplying 4 by 2
                                                  //| adding 1 to 1
                                                  //| multiplying 2 by 2
                                                  //| adding 1 to 2
                                                  //| multiplying 3 by 2
                                                  //| adding 1 to 3
                                                  //| multiplying 4 by 2
                                                  //| adding 1 to 4
                                                  //| multiplying 5 by 2
                                                  //| adding 1 to 1
                                                  //| multiplying 2 by 2
                                                  //| adding 1 to 2
                                                  //| multiplying 3 by 2
                                                  //| adding 1 to 3
                                                  //| multiplying 4 by 2
                                                  //| adding 1 to 4
                                                  //| multiplying 5 by 2
                                                  //| adding 1 to 5
                                                  //| multiplying 6 by 2

  v2.map(_.value)                           //> res7: Seq[Option[scala.util.Try[Double]]] = Vector(Some(Success(0.0)), Some
                                                  //| (Success(4.0)), Some(Success(10.0)), Some(Success(18.0)), Some(Success(28.0
                                                  //| )), Some(Success(40.0)))

  // Make sure we're really done
  import scala.concurrent.duration._
  Await.result(v2.last, 0 nanos)            //> res8: Double = 40.0

  v2.map(_.value)                           //> res9: Seq[Option[scala.util.Try[Double]]] = Vector(Some(Success(0.0)), Some
                                                  //| (Success(4.0)), Some(Success(10.0)), Some(Success(18.0)), Some(Success(28.0
                                                  //| )), Some(Success(40.0)))

  import scala.util.Success
  v2.map(_.value match { case Some(Success(x)) => x})
                                                  //> res10: Seq[Double] = Vector(0.0, 4.0, 10.0, 18.0, 28.0, 40.0)

  // Could we think of this as a monad, where the value is stored in a closure?
  case class ClosureMonad[+A](fc : () => A) {
    def this(a : A) = this( ()=>{a} )
    // bind should return a function of no arguments that evaluates item and evaluates f on the result
    // def bind[B](f : A => ClosureMonad[B]) : ClosureMonad[B] = f(item())  // forces item to be run immediately
    // The question hinges on whether this is kosher, as it unwraps the result of f

  def bind[B](f : A => ClosureMonad[B]) : ClosureMonad[B] = new ClosureMonad( () =>  f(fc()).fc() )
    // This actually seems more useful
    def bindy[B](g : A =>B ) : ClosureMonad[B] = new ClosureMonad( () =>  g(fc()) )
  }
  class SeqFM[A](l : Seq[ClosureMonad[A]] ) {
    def map[B] (f : A => B) = new SeqFM[B] (l.map(_.bind(x => new ClosureMonad[B]( ()=>f(x) ))))
    def values() = {l}
  }

  class SeqFMMaker[A](l : Seq[A]) {
    def wrapm() = new SeqFM[A](l.map( x => new ClosureMonad[A](x) ) )
  }

  implicit def SeqToSeqFMMaker[A](l : Seq[A]) = new SeqFMMaker[A](l)
                                                  //> SeqToSeqFMMaker: [A](l: Seq[A])mapfunc.SeqFMMaker[A]
  val v3 = (1 to 5).wrapm.map(add1).map(mult2)
                                                  //> v3  : mapfunc.SeqFM[Int] = mapfunc$$anonfun$main$1$SeqFM$1@156d401
  v3.values().map(i => i match {case ClosureMonad(f) => f()} )
                                                  //> adding 1 to 1
                                                  //| multiplying 2 by 2
                                                  //| adding 1 to 2
                                                  //| multiplying 3 by 2
                                                  //| adding 1 to 3
                                                  //| multiplying 4 by 2
                                                  //| adding 1 to 4
                                                  //| multiplying 5 by 2
                                                  //| adding 1 to 5
                                                  //| multiplying 6 by 2
                                                  //| res11: Seq[Int] = Vector(4, 6, 8, 10, 12)
}
