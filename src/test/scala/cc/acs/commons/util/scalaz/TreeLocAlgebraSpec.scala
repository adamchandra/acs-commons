package cc.acs.commons.util.scalaz

import org.specs2.mutable
import org.specs2.mutable._

import scalaz._
import scalaz.{Scalaz => Z}
import Z.{node => _, _}

import cc.acs.commons.util.StringOps._
import cc.acs.commons.util.FileOps._

import acs.boxes.Boxes._


object TreeLocAlgebraSpec extends mutable.Specification {
  import TreeLocDrawing._  
  import TreeLocUtils._

  "combining two treelocs" should {
    "sum the parent axes" in {

      val tree = 1.node(8.node(64.leaf))

      val allLocs = tree.loc.cojoin.toTree.flatten

      for {
        l1 <- allLocs
        l2 <- allLocs
      } {
        val l12 = combineTreeLocs(l1, l2)
        val b1 = drawTreeLoc(l1) ∘ (text(_))
        val b2 = drawTreeLoc(l2) ∘ (text(_))
        val b12 = drawTreeLoc(l12) ∘ (text(_))

        val tb1 = vcat(AlignFirst)(b1.toList)
        val tb2 = vcat(AlignFirst)(b2.toList)
        val tb12 = vcat(AlignFirst)(b12.toList)

        val all = tb1 +| tb2 +| tb12
        val result = (renderBox(all)).mkString("\n")
        println("result: \n" + result)
      }

      todo
    }
  }
}
