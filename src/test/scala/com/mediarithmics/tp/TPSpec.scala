package com.mediarithmics.tp

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, BooleanOperators}

object TPSpec extends Properties("TP") {

  property("double") =
    forAll { (x: Int, f: Int => Int) =>
      TP.double(f)(x) == Solutions.double(f)(x)
    }

  property("compose") =
    forAll { (x: Int, f: Int => Int, g: Int => Int) =>
      TP.compose(f, g)(x) == Solutions.compose(f, g)(x)
    }

  property("map") =
    forAll { (f: Int => Int, fa: Option[Int]) =>
      TP.map(f)(fa) == Solutions.map(f)(fa)
    }

  val cmp: (Int, Int) => TP.Order =
    (x: Int, y: Int) =>
      if (x < y) TP.LT else if (x > y) TP.GT else TP.EQ

  sealed trait ThreeWay[A]

  case class L[A](v: A)

  case object C

  case class R[A](v: A)

  /*
  def isBST(tree: TP.BinaryTree[Int]): Boolean = {
    def go(tree: TP.BinaryTree[Int], min: Int, max: Int): Boolean =
      tree match {
        case TP.Branch(l, v, r) =>
          (v >= min) && (v <= max) && go(l, min, v) && go(r, v, max)
        case TP.Leaf =>
          true
      }

    go(tree, Int.MinValue, Int.MaxValue)
  }

  property("insert") =
    forAll { (vs: List[Int]) =>
      val t = vs.foldLeft[TP.BinaryTree[Int]](Leaf)((t, v) => TP.insert(t, v, cmp))
      isBST(t)
    }
    */

  property("fromList implies contains with cmp") =
    forAll { (vs: List[Int]) =>
      val t = TP.fromList(vs, cmp)
      vs.forall(x => TP.contains(t, x, cmp))
    }

  property("not in list implies not in tree with cmp") =
    forAll { (vs: List[Int], v: Int) =>
      val t = TP.fromList(vs, cmp)
      !vs.contains(v) ==> !TP.contains(t, v, cmp)
    }

  property("fromList implies contains without cmp") =
    forAll { (vs: List[Int]) =>
      val t = TP.fromList(vs, cmp)
      vs.forall(x => TP.contains(t, x))
    }

  property("not in list implies not in tree without cmp") =
    forAll { (vs: List[Int], v: Int) =>
      val t = TP.fromList(vs, cmp)
      !vs.contains(v) ==> !TP.contains(t, v)
    }


  def pairCmp(x: (Int, String), y: (Int, String)): TP.Order =
    cmp(x._1, y._1)

  property("get") =
    forAll { (vs: List[(Int, String)]) =>
      val t = TP.fromList[(Int, String)](vs, pairCmp)
      vs.forall(v => TP.get(t, v._1, cmp) == Some(v._2))
    }

  property("values") =
    forAll { (vs: List[(Int, String)]) =>
      val t = TP.fromList[(Int, String)](vs, pairCmp)
      TP.values(t).toSet == vs.map(_._2).toSet
    }
}
