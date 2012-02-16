package cc.acs.commons.util.scalaz

import org.specs2.mutable
import org.specs2.mutable._

import scalaz._
import scalaz.{Scalaz => Z}
import Z.{node => _, _}

import cc.acs.commons.util.StringOps._
import cc.acs.commons.util.FileOps._

import acs.boxes.Boxes._


object StringUtilsSpec extends mutable.Specification {

  "border stripping" should {
    "correctly remove these borders" in {
      val strs = """
      | xx |
      """
      val borderless = stripBorder(strs)
      borderless must_== " xx "
    }

    "bork on these borders" in {
      todo
    }
  }

}
