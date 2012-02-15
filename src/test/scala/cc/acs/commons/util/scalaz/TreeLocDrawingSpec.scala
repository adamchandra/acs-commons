package cc.acs.commons.util.scalaz

import org.specs2.mutable
import org.specs2.mutable._

import scalaz._
import scalaz.{Scalaz => Z}
import Z.{node => _, _}

import cc.acs.commons.util.StringOps._
import cc.acs.commons.util.FileOps._

import acs.boxes.Boxes._


object TreeLocDrawingSpec extends mutable.Specification {
  import TreeLocDrawing._

  "annotation" should {
    "render all treelocs" in {
      val renderedTreeLocs = """
      |0           0           0           0           0           0           0           0           0           0
      |├ 1         ┡━1         ┡━1         ┡━1         ┡━1         ┡━1         ┡━1         ┡━1         ┡━1         ┠─1
      |│ ├ 2       │ ├ 2       │ ┡━2       │ ┡━2       │ ┡━2       │ ┠─2       │ ┠─2       │ ┠─2       │ ┠─2       ┃ ├ 2
      |│ │ ├ 3     │ │ ├ 3     │ │ ├ 3     │ │ ┡━3     │ │ ┠─3     │ ┃ ├ 3     │ ┃ ├ 3     │ ┃ ├ 3     │ ┃ ├ 3     ┃ │ ├ 3
      |│ │ └ 6     │ │ └ 6     │ │ └ 6     │ │ └─6     │ │ ┗━6     │ ┃ └ 6     │ ┃ └ 6     │ ┃ └ 6     │ ┃ └ 6     ┃ │ └ 6
      |│ ├ 4       │ ├ 4       │ ├─4       │ ├─4       │ ├─4       │ ┡━4       │ ┠─4       │ ┠─4       │ ┠─4       ┃ ├ 4
      |│ └ 8       │ └ 8       │ └─8       │ └─8       │ └─8       │ └─8       │ ┗━8       │ ┗━8       │ ┗━8       ┃ └ 8
      |│   ├ 3     │   ├ 3     │   ├ 3     │   ├ 3     │   ├ 3     │   ├ 3     │   ├ 3     │   ┡━3     │   ┠─3     ┃   ├ 3
      |│   └ 4     │   └ 4     │   └ 4     │   └ 4     │   └ 4     │   └ 4     │   └ 4     │   └─4     │   ┗━4     ┃   └ 4
      |└ 5         └─5         └─5         └─5         └─5         └─5         └─5         └─5         └─5         ┗━5
      """.trim.stripMargin
      
      val tree = 0.node(1.node(2.node(3.leaf, 6.leaf), 4.leaf, 8.node(3.leaf, 4.leaf)), 5.leaf)
      val row = tree.loc.cojoin.toTree.flatten.foldl(text("")) { case (acc, tl) => {
        val tboxes = drawTreeLoc(tl) ∘ (text(_))
        val tbox = vcat(AlignFirst)(tboxes.toList)
        acc +| "   " +| tbox
      }}

      val actual:List[String] = renderBox(row)
      val trimmed = (actual ∘ (_.trim)).mkString("\n")

      trimmed must_== renderedTreeLocs
    }
  }
}
