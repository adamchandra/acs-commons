package code
package model

import org.specs.{Sugar, Specification, ScalaCheck}

import net.liftweb.util.BindHelpers._
import TreeVis._
import scalaz._

object TreeVisSpec extends Specification with Sugar with ScalaCheck { 
  // import XmlGen.Implicits._
  // import XmlGen._
  import Scalaz._

  "tree vis" should {
    "move tree" in {
      val tree = (0.0, unitExtent, "a").node(
        (0.0, unitExtent, "b").leaf
      )
      val expect = (1.0, unitExtent, "a").node(
        (1.0, unitExtent, "b").leaf
      )
      val actual = movetree(tree, 1.0)
      actual ≟ expect must_== true
    }

    "test moveextent" in {
      val expect = Seq((1.0, 1.0), (1.0, 1.0))
      moveextent(Seq((0.0, 0.0), (0.0, 0.0)), 1.0) ≟ expect must_== true
    }


    "test merge" in {
      val expect = Seq((1.0, 2.0), (1.0, 2.0))
      val actual = merge(
        Seq((1.0, 1.0), (1.0, 1.0)),
        Seq((0.0, 2.0), (0.0, 2.0)))
      actual ≟ expect must_== true
    }

    "test mergelist" in {  
      val expect = Seq((1.0, 2.0), (1.0, 2.0))
      val actual = mergelist(Seq(
        Seq((1.0, 1.0), (1.0, 1.0)),
        Seq((0.3, 0.3), (0.3, 0.3)),
        Seq((0.3, 0.3), (0.3, 0.3)),
        Seq((0.0, 2.0), (0.0, 2.0))))
      actual ≟ expect must_== true
    }


    "test fit" in {
      val actual = fit(
        Seq((1.0, 2.0), (1.0, 3.0)),
        Seq((1.0, 3.0), (1.0, 4.0)))
      val expect = 3
      actual ≟ expect must_== true
    }

    "test fitlistl" in {
      val es = Seq(
        Seq((1.0, 2.0), (1.0, 3.0)),
        Seq((1.0, 3.0), (1.0, 4.0))
      )

      val actual = fitlistl(es)
      val expect = Seq(0.0, 3.0)
      actual ≟ expect must_== true
    }


  // 
  // "test movetree" in {
  //   tree = [["a", 0], ["b", 0]]
  //   test.deepEqual(tf.movetree(tree, 2), [["a", 2], ["b", 0]])
  // }
  // 
  // 

  "test design" in {
 
    // val dbl[A]: Lens[(A, String, Double), Dbl] = Lens(_, (t3, i) => (t3._1, t3._2, i))
    
    case class Layout[A](a: A, offset: Double, extent: Seq[Span])
    
    // Layout.extent.set(v, ext)


    val tree = 
      1.0.node(
        2.0.leaf, 2.0.leaf, 2.0.leaf)


    //val expect = (0.0, Seq[Span]((0.0, 0.0), (0.0, 2.0)), 1.0).node(
    //    (0.0, Seq[Span]((0.0, 0.0)), 2.0).leaf
    //    (1.0, Seq[Span]((0.0, 0.0)), 2.0).leaf
    //    (2.0, Seq[Span]((0.0, 0.0)), 2.0).leaf
    //)

    val actual = design(tree)
    println("" + actual.drawTree)
    //actual ≟ expect must_== true
  }
  
  // 
  // "test design2" in {
  //   tree = ["a", ["b"], ["c"]]
  //   tfmt = tf.design(tree)
  //   test.deepEqual(tfmt, [['a',0],[['b',0]],[['c',1]]])
  // }
  // 
  }
}


