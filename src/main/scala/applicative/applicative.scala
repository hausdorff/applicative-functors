/** Most of this was written by Peter Fraenkel. Mainly the hard parts.
 Code was reviewed and supplemented with help of Zach Allaun. */

package applicative

import scala.concurrent._
import ExecutionContext.Implicits.global


/** Ziplists are a type of applicative functor that is allowed to contain
 either data or functions, and which defines laws for composing functions
 and applying them to data. So for example, in Haskell notation:

 [(+3),(*2)] <*> [1,2] = [4,4]
*/
class ZipSeq[A] (l: Seq[()=>A]) {
  def map[B](f: A =>B) =
    new ZipSeq(l.map( x => () => f(x())))
  def scanLeft[B](b0: B)(f: (B,A)=>B): ZipSeq[B] = {
    val l2 = l.scanLeft( ()=>b0 )( (b,a) => () => f(b(),a()) )
    new ZipSeq[B](l2)
  }
  def values() = l.map(_())
  def inSeries(): Seq[Future[A]] =
    l.tail.scanLeft(future{l.head.apply()})(
      (f: Future[A],x: ()=>A) =>  f.map( _ => x() ))
}
object ZipSeq {
  class ZipSeqMaker[A](l: Seq[A]) {
    def wrapf() = new ZipSeq[A](l.map( x => () => x) )
  }

  implicit def SeqToZipSeqMaker[A](l: Seq[A]) =
    new ZipSeqMaker[A](l)
}
