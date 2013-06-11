/** Most of this was written by Peter Fraenkel. Mainly the hard parts.
 Code was reviewed and supplemented with help of Zach Allaun. */

package applicative

import scala.concurrent._
import ExecutionContext.Implicits.global


/** Implements functor such that repeated calls to map result in
 function composition on each of the components of the functor.
 So, when `map` is called, rather than repeatedly mapping over the
 functor, each element is computed individually as a series of
 function compositions. */
class SeqF[A] (l: Seq[()=>A]) {
  def map[B](f: A =>B) =
    new SeqF(l.map( x => () => f(x())))
  def scanLeft[B](b0: B)(f: (B,A)=>B): SeqF[B] = {
    val l2 = l.scanLeft( ()=>b0 )( (b,a) => () => f(b(),a()) )
    new SeqF[B](l2)
  }
  def values() = l.map(_())
  def inSeries(): Seq[Future[A]] =
    l.tail.scanLeft( future{l.head.apply()} ) ( (f: Future[A],x: ()=>A) =>  f.map( _ => x() ))
}
object SeqF { 
  class SeqFMaker[A](l: Seq[A]) {
    def wrapf() = new SeqF[A](l.map( x => () => x) )
  }
  

  implicit def SeqToSeqFMaker[A](l: Seq[A]) = new SeqFMaker[A](l)
}
