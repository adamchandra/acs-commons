package cc.acs.commons.util.scalaz

import scalaz._
import scalaz.{Scalaz => Z}
import Z.{node => _, _}

import cc.acs.commons.util.StringOps._
import cc.acs.commons.util.FileOps._


object TreeUtils {
  def pathToRoot(treeloc: TreeLoc[_]): Stream[Int] = 
    treeloc.lefts.length #:: treeloc.parents.map(_._1.length)
}

object TreeDrawing {
  import TreeUtils._

  def drawTree[A, B >: A](tree: Tree[A])(implicit sh: Show[B]): String = {
    implicit val showa: Show[A] = sh ∙ (a => a)
    drawTree_(tree).foldMap(_ + "\n")
  }

  def drawSubTrees[A : Show](s: Stream[Tree[A]]): Stream[String] = {
    s match {
      case Stream.Empty => Stream.Empty
      case Stream(t) => shift("└ ", "  ", drawTree_(t))
      case t #:: ts =>  shift("├ ", "│ ", drawTree_(t)) append drawSubTrees(ts)
    }}

  def shift(first: String, other: String, s: Stream[String]): Stream[String] =
    s.ʐ <*> ((first #:: other.repeat[Stream]).ʐ ∘ ((_: String) + (_: String)).curried)

  /** A 2D String representation of this Tree, separated into lines. */
  def drawTree_[A, B >: A](tree: Tree[A])(implicit sh: Show[B]): Stream[String] = {
    implicit val showa: Show[A] = sh ∙ (x => x)
    tree.rootLabel.shows #:: drawSubTrees(tree.subForest)
  }
}


object TreeLocDrawing {
  import TreeUtils._
  import TreeDrawing._


  def drawTreeLoc[A, B >: A](treeloc: TreeLoc[A])(implicit sh: Show[B]): Stream[String] = {
    implicit val showa: Show[A] = sh contramap (x => x)
    
    val pathfrom:(Stream[Stream[String]], Stream[String], Stream[Stream[String]]) =
      (treeloc.lefts.reverse ∘ (drawTree_(_)),
       drawTree_(treeloc.tree),
       treeloc.rights ∘ (drawTree_(_)))


    val path:(Stream[Stream[String]], Stream[String], Stream[Stream[String]]) =
      treeloc.parents.reverse.foldr(pathfrom) {
        case ((plefts   : Stream[Tree[A]], 
               ppath    , 
               prights  : Stream[Tree[A]]), 
              (acclefts  : Stream[Stream[_]], 
               accmid    : Stream[_], 
               accrights : Stream[Stream[_]])) => {
                
                // acc is a stream of formatted trees, where each tree is a Stream[String]
                val lefts = acclefts ∘ (ss => shift("┠─", "┃ ", ss))

                val mid = 
                  if   (!accrights.isEmpty) shift("┡━", "│ ", accmid)
                  else                      shift("┗━", "  ", accmid)

                val rights = accrights.splitAt(accrights.length-1) 
                val r1 = rights._1 ∘ (ss => shift("├─", "│ ", ss))
                val r2 = rights._2 ∘ (ss => shift("└─", "  ", ss))

                val middle = Stream(ppath.shows) ++ lefts.join ++ mid ++ (r1 ++ r2).join
                val pathfrom:(Stream[Stream[String]], Stream[String], Stream[Stream[String]]) =
                  (plefts.reverse ∘ (drawTree_(_)), 
                   middle, 
                   prights ∘ (drawTree_(_)))
                 
                pathfrom
              }}

    path match {
      case (l, m, r) => l.join ++ m ++ r.join
    }
  }


  def renderTreeLoc[A, B >: A](treeloc: TreeLoc[A])(implicit sh: Show[B]): String = {
    implicit val showa: Show[A] = sh contramap (x => x)
    val lines = drawTreeLoc(treeloc)
    (lines ∘ (_.mkString("\n"))).mkString("\n")
  }
}

