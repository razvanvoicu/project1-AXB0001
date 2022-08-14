import scala.annotation.targetName
import scala.collection.immutable.Queue
import Math.{floor, max, min, log10}

// Need to change height as well to max(__1.height, __2.height, ...)

sealed trait IArr[+T]:
  def height: Int           // distance from current node to any leaf
  def length: Int           // size of the array
  def isEmpty: Boolean
  def apply(i: Int): T      // array dereference, 0-based. a(3) returns element at index 3.
  def update[T1 >: T](i: Int, value: T1): IArr[T1] // return a new array which is different only at index i
  def iterator: Iterator[T] // iterate through all the elements of an array

  def slice(idx1: Int, idx2: Int): IArr[T] = // returns the slice between min(idx1, idx2) and max(idx1, idx2)
    IArr(iterator.slice(idx1, idx2).toSeq : _*)

  // Convenience methods, to be used in the rest of the classes
  def outOfBounds(i: Int): Nothing =
    throw new IndexOutOfBoundsException(s"Attempt to access non-existent index $i in an array of size $length")

  def withinBounds[T1](i: Int)(code: => T1): T1 =
    if i < 0 || i >= length then outOfBounds(i)
    code

  def withinBoundsF[T1](i: Int)(f: Int => T1): T1 = withinBounds(i)(f(i))

  def append[T1 >: T](secArr: IArr[T1]): IArr[T1]

  def log4(x: Double): Int = floor(log10(x) / log10(4.0)).toInt

  def condForAppend: Boolean = 2 * log4(length) < height

object IArr: // Companion object, containing the constructor
  def  apply[T](elems: T*): IArr[T] = // Construct an array from a list of arguments
    val leafs = elems.grouped(4).map {
      case Seq() => IArrLeaf0
      case Seq(a1) => IArrLeaf1(a1)
      case Seq(a1, a2) => IArrLeaf2(a1, a2)
      case Seq(a1, a2, a3) => IArrLeaf3(a1, a2, a3)
      case Seq(a1, a2, a3, a4) => IArrLeaf4(a1, a2, a3, a4)
      case _ => assert(false)
    }.to(List)
    Iterator.iterate((leafs: List[IArr[T]], leafs.length)) {
      case (l, count) =>
        val nextLevel = l.grouped(4).map {
          case Seq(a1) => IArrNode1(a1)
          case Seq(a1, a2) => IArrNode2(a1, a2)
          case Seq(a1, a2, a3) => IArrNode3(a1, a2, a3)
          case Seq(a1, a2, a3, a4) => IArrNode4(a1, a2, a3, a4)
          case _ => assert(false)
        }.to(List)
        (nextLevel, (count+3)/4)
    }.dropWhile(_._2 > 1).next._1.headOption.getOrElse(IArrLeaf0)



sealed trait IArrLeaf[+T] extends IArr[T]: // Array leafs, coming in 5 sizes: empty, with 1 elem, with 2 elems, with 3 elems, and with 4 elems
  override def height = 0

case object IArrLeaf0 extends IArrLeaf[Nothing] { // Leaf of size 0, usable for empty arrays
  override def length: Int = 0
  override def isEmpty: Boolean = true
  override def apply(i: Int): Nothing = outOfBounds(i)
  override def update[T1 >: Nothing](i: Int, value: T1): IArr[T1] = outOfBounds(i)
  override def iterator: Iterator[Nothing] = Iterator.empty
  override def slice(idx1: Int, idx2: Int): IArr[Nothing] = outOfBounds(idx1)
  override def append[T1 >: Nothing](secArr: IArr[T1]): IArr[T1] = secArr
}


sealed trait IArrLeafNonEmpty[+T] extends IArrLeaf[T]: // Base trait for the 4 non-empty leafs
  override def isEmpty: Boolean = false

case class IArrLeaf1[+T](__1: T) extends IArrLeafNonEmpty[T]: // Leaf with 1 elem
  override def apply(i: Int): T = withinBounds(i)(__1)
  override def update[T1 >: T](i: Int, value: T1): IArr[T1] = withinBounds(i)(IArrLeaf1(value))
  override def iterator: Iterator[T] = Iterator(__1)
  override def length: Int = 1
  override def append[T1 >: T](secArr: IArr[T1]): IArr[T1] = {
    if (!secArr.isEmpty) {
      val newNode = IArrNode2(this, secArr)
      if (newNode.condForAppend) {
        IArr(newNode.iterator.toSeq : _*)
      } else {
        newNode
      }
    } else {
      this
    }
    // if (!secArr.isEmpty) IArrNode2(this, secArr) else this
  }


case class IArrLeaf2[+T](__1: T, __2: T) extends IArrLeafNonEmpty[T]: // Leaf with 2 elems
  override def length: Int = 2
  override def apply(i: Int): T = withinBoundsF(i) {
    case _ if i % 2 == 0 => __1
    case _ if i % 2 == 1 => __2
  }

  override def update[T1 >: T](i: Int, value: T1): IArr[T1] =
    withinBoundsF(i) {
    case _ if i % 2 == 0 => IArrLeaf2(value, __2)
    case _ if i % 2 == 1 => IArrLeaf2(__1, value)
  }

  override def iterator: Iterator[T] = Iterator(__1, __2)

  override def append[T1 >: T](secArr: IArr[T1]): IArr[T1] = {
    if (!secArr.isEmpty) {
      val newNode = IArrNode2(this, secArr)
      if (newNode.condForAppend) {
        IArr(newNode.iterator.toSeq: _*)
      } else {
        newNode
      }
    } else {
      this
    }
    // if (!secArr.isEmpty) IArrNode2(this, secArr) else this
  }
    // if (!secArr.isEmpty) IArrNode2(this, secArr) else this

case class IArrLeaf3[+T](__1: T, __2: T, __3: T) extends IArrLeafNonEmpty[T]: // Leaf with 3 elems
  override def length: Int = 3
  override def apply(i: Int): T = withinBoundsF(i) {
    case _ if i % 3 == 0 => __1
    case _ if i % 3 == 1 => __2
    case _ if i % 3 == 2 => __3
  }

  override def update[T1 >: T](i: Int, value: T1): IArr[T1] = withinBoundsF(i) {
    case _ if i % 3 == 0 => IArrLeaf3(value, __2, __3)
    case _ if i % 3 == 1 => IArrLeaf3(__1, value, __3)
    case _ if i % 3 == 2 => IArrLeaf3(__1, __2, value)
  }

  override def iterator: Iterator[T] = Iterator(__1, __2, __3)

  override def append[T1 >: T](secArr: IArr[T1]): IArr[T1] = {
    if (!secArr.isEmpty) {
      val newNode = IArrNode2(this, secArr)
      if (newNode.condForAppend) {
        IArr(newNode.iterator.toSeq: _*)
      } else {
        newNode
      }
    } else {
      this
    }
    // if (!secArr.isEmpty) IArrNode2(this, secArr) else this
  }
    // if (!secArr.isEmpty) IArrNode2(this, secArr) else this

case class IArrLeaf4[+T](__1: T, __2: T, __3: T, __4: T) extends IArrLeafNonEmpty[T]: // Leafs with 4 elems
  override def length: Int = 4
  override def apply(i: Int): T = withinBoundsF(i) {
    case _ if i % 4 == 0 => __1
    case _ if i % 4 == 1 => __2
    case _ if i % 4 == 2 => __3
    case _ if i % 4 == 3 => __4
  }

  override def update[T1 >: T](i: Int, value: T1): IArr[T1] = withinBoundsF(i) {
    case _ if i % 4 == 0 => IArrLeaf4(value, __2, __3, __4)
    case _ if i % 4 == 1 => IArrLeaf4(__1, value, __3, __4)
    case _ if i % 4 == 2 => IArrLeaf4(__1, __2, value, __4)
    case _ if i % 4 == 3 => IArrLeaf4(__1, __2, __3, value)
  }

  override def iterator: Iterator[T] = Iterator(__1, __2, __3, __4)

  override def append[T1 >: T](secArr: IArr[T1]): IArr[T1] = {
    if (!secArr.isEmpty) {
      val newNode = IArrNode2(this, secArr)
      if (newNode.condForAppend) {
        IArr(newNode.iterator.toSeq: _*)
      } else {
        newNode
      }
    } else {
      this
    }
    // if (!secArr.isEmpty) IArrNode2(this, secArr) else this
  }
    // if (!secArr.isEmpty) IArrNode2(this, secArr) else this



sealed trait IArrNode[+T] extends IArr[T]: // Base trait for all inner nodes, coming in 4 sizes
  // def __1: IArr[T]

  // override val height: Int = __1.height + 1

  override def isEmpty: Boolean = false

  // Convenience methods to be used in all the nodes
  def withinBoundsWithIndexSplit[T](i: Int)(f: ((Int, Int)) => T): T =
    withinBounds(i)(f(indexSplit(i)))

  def indexSplit(i: Int): (Int, Int) =
    val cellCapacity = 1 << (2 * height)
    (i / cellCapacity, i % cellCapacity)


case class IArrNode1[+T](__1: IArr[T]) extends IArrNode[T]:
  override val length: Int = __1.length

  override val height: Int = __1.height + 1

  override def apply(i: Int): T = withinBoundsF(i)(_ => __1(i))

  override def update[T1 >: T](i: Int, value: T1): IArr[T1] =
    withinBoundsF(i)(_ => IArrNode1(__1.update(i, value)))

  override def iterator: Iterator[T] = __1.iterator

  override def append[T1 >: T](secArr: IArr[T1]): IArr[T1] = {
    if (!secArr.isEmpty) {
      val newNode = IArrNode2(__1, secArr)
      if (newNode.condForAppend) {
        IArr(newNode.iterator.toSeq: _*)
      } else {
        newNode
      }
    } else {
      this
    }
  }
    // if (!secArr.isEmpty) IArrNode2(__1, secArr) else this


case class IArrNode2[+T](__1: IArr[T], __2: IArr[T]) extends IArrNode[T]:
  override val length: Int = __1.length + __2.length

  override val height: Int = max(__1.height, __2.height) + 1

  override def apply(i: Int): T = withinBoundsF(i) {
    case _ if i < __1.length => __1(i)
    case _ => __2(i - __1.length)
  }

  override def update[T1 >: T](i: Int, value: T1): IArr[T1] = withinBoundsF(i) {
    case _ if i < __1.length => IArrNode2(__1.update(i, value), __2)
    case _ => IArrNode2(__1, __2.update(i - __1.length, value))
  }

  override def iterator: Iterator[T] = __1.iterator ++ __2.iterator

  override def append[T1 >: T](secArr: IArr[T1]): IArr[T1] = {
    if (!secArr.isEmpty) {
      val newNode = IArrNode3(__1, __2, secArr)
      if (newNode.condForAppend) {
        IArr(newNode.iterator.toSeq: _*)
      } else {
        newNode
      }
    } else {
      this
    }
  }
    // if (!secArr.isEmpty) IArrNode3(__1, __2, secArr) else this


case class IArrNode3[+T](__1: IArr[T], __2: IArr[T], __3: IArr[T]) extends IArrNode[T]:
  override val length: Int = __1.length + __2.length + __3.length

  override val height: Int = max(__1.height, max(__2.height, __3.height)) + 1

  override def apply(i: Int): T = withinBoundsF(i) {
    case _ if i < __1.length => __1(i)
    case _ if i < __1.length + __2.length => __2(i - __1.length)
    case _ => __3(i - (__1.length + __2.length))
  }

  override def update[T1 >: T](i: Int, value: T1): IArr[T1] = withinBoundsF(i) {
    case _ if i < __1.length => IArrNode3(__1.update(i, value), __2, __3)
    case _ if i < __1.length + __2.length => IArrNode3(__1, __2.update(i - __1.length, value), __3)
    case _ => IArrNode3(__1, __2, __3.update(i - (__1.length + __2.length), value))
  }

  override def iterator: Iterator[T] = __1.iterator ++ __2.iterator ++ _3.iterator

  override def append[T1 >: T](secArr: IArr[T1]): IArr[T1] = {
    if (!secArr.isEmpty) {
      val newNode = IArrNode4(__1, __2, __3, secArr)
      if (newNode.condForAppend) {
        IArr(newNode.iterator.toSeq: _*)
      } else {
        newNode
      }
    } else {
      this
    }
  }
    // if (!secArr.isEmpty) IArrNode4(__1, __2, __3, secArr) else this


case class IArrNode4[+T](__1: IArr[T], __2: IArr[T], __3: IArr[T], __4: IArr[T]) extends IArrNode[T]:
  override val length: Int = __1.length + __2.length + __3.length + __4.length

  override val height: Int = max(__1.height, max(__2.height, max(__3.height, __4.height))) + 1

  override def apply(i: Int): T = withinBoundsF(i) {
    case _ if i < __1.length => __1(i)
    case _ if i < __1.length + __2.length => __2(i - __1.length)
    case _ if i < __1.length + __2.length + __3.length => __3(i - (__1.length + __2.length))
    case _ => __4(i - (__1.length + __2.length + __3.length))
  }

  override def update[T1 >: T](i: Int, value: T1): IArr[T1] = withinBoundsF(i) {
    case _ if i < __1.length =>
      IArrNode4(__1.update(i, value), __2, __3, __4)
    case _ if i < __1.length + __2.length =>
      IArrNode4(__1, __2.update(i - __1.length, value), __3, __4)
    case _ if i < __1.length + __2.length + __3.length =>
      IArrNode4(__1, __2, __3.update(i - (__1.length + __2.length), value), __4)
    case _ =>
      IArrNode4(__1, __2, __3, __4.update(i - (__1.length + __2.length + __3.length), value))
  }

  override def iterator: Iterator[T] = __1.iterator ++ __2.iterator ++ _3.iterator ++ _4.iterator

  override def append[T1 >: T](secArr: IArr[T1]): IArr[T1] = {
    if (!secArr.isEmpty) {
      val newNode = IArrNode2(this, secArr)
      if (newNode.condForAppend) {
        IArr(newNode.iterator.toSeq: _*)
      } else {
        newNode
      }
    } else {
      this
    }
  }
    // if (!secArr.isEmpty) IArrNode2(this, secArr) else this



object Solution // Keep IntelliJ happy
