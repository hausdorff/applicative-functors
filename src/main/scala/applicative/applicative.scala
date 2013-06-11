/** Most of this was written by Peter Fraenkel. Mainly the hard parts.
 Code was reviewed and supplemented with help of Zach Allaun. */

package applicative

import scala.concurrent._
import ExecutionContext.Implicits.global


/** Implements the a pointwise applicative functor. That is, when one
 composes a series of `map` calls, normally one has to wait for each
 successive map call to complete over the complete functor before the next
 map is called. In this implementation, each component is seen as a
 completely independent set of function compositions, allowing us to pick any
 particular element, of the functor, and find the result, independent of
 whether the other elements have fully evaluated.
 
 Much like the Haskell implementation. A normal functor is simply
 something on which `fmap` is defined. For example, `map` is defined for
 lists, making them a functor. More formally a functor fnctr is defined
 for some fmap:

 fmap :: (a -> b) -> fnctr a -> fnctr b

 An *applicative* functor is just a functor that allows us to apply
 a functor full of functions to a functor full of data. More formally:

 appmap :: fnctr (a -> b) -> fnctr a -> fnctr b

 This class defines the applicative functor in this way. The crucial
 difference (as noted above) is that when we chain together calls of `map`,
 normally we have to wait for one complete `map` to complete before we can
 start the next `map`, where in this implemenation, each element is
 implemented as an independent composition of functions, meaning that each
 can be observed independent of whether others have completed computation.
*/
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
