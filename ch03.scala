package funcprogscala.ch03

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    // Exercise 3.1
    val x = List(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
    } // == 3

    // Exercise 3.2
    def tail[A](xs: List[A]): List[A] = xs match {
        case Nil => Nil
        case Cons(y, ys) => ys
        }

    // Exercise 3.3
    def setHead[A](as: List[A], a: A): List[A] = as match {
        case Nil => Nil
        case Cons(x, xs) => Cons(a, xs)
    }

    // Exercise 3.4
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
        case (Nil, _) => Nil
        case (xs, 0) => xs
        case (Cons(x, xs), n) => drop(xs, n-1)
    }

    // Exercise 3.5
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
        case Nil => Nil
        case Cons(x, xs) if f(x) => dropWhile(xs, f)
        case _ => l
    }

    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
        case Nil => a2
        case Cons(x, xs) => Cons(x, append(xs, a2))
    }

    // Exercise 3.6
    def init[A](l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(x, Nil) => Nil
        case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A,B](as: List[A], b: B) (f: (A,B) => B): B =
        as match {
            case Nil => b
            case Cons(x, xs) => f(x, foldRight(xs, b)(f))
        }

    def sum2(ints: List[Int]) = foldRight(ints, 0)(_ + _)
    def product2(ds: List[Double]) = foldRight(ds, 1.0) (_ * _)

    // Exercise 3.8
    val y = foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_))
        // == Cons(1, Cons(2, Cons(3, Nil)))

    // Exercise 3.9
    def length[A](as: List[A]): Int =
        foldRight(as, 0)((x,y) => 1 + y)

    // Exercise 3.10
    def foldLeft[A,B](as: List[A], b: B)(f: (B,A) => B): B =
        as match {
            case Nil => b
            case Cons(x, xs) => foldLeft(xs, f(b,x))(f)
        }

    // Exercise 3.11
    def sum3(ints: List[Int]): Int =
        foldLeft(ints, 0)(_+_)
    def product3(ds: List[Double]): Double =
        foldLeft(ds, 1.0)(_*_)
    def length2[A](as: List[A]): Int =
        foldLeft(as, 0)((x,y) => x + 1)

    // Exercise 3.12
    def reverse[A](as: List[A]): List[A] =
        foldLeft(as, Nil: List[A])((xs, x) => Cons(x, xs))

    // Exercise 3.13


    // Exercise 3.14
    def append2[A](as1: List[A], as2: List[A]): List[A] =
        foldRight(as1, as2)((x,y) => Cons(x, y))

    // Exercise 3.15
    def concat[A](ls: List[List[A]]): List[A] =
        foldRight(ls, Nil: List[A])((x,y) => append2(x, y))

    // Exercise 3.16
    def listAddOne(xs: List[Int]): List[Int] =
        xs match {
            case Nil => Nil
            case Cons(n, ns) => Cons(n+1, addOne(ns))
        }

    // Exercise 3.17
    def listToString(ds: List[Double]): List[String] =
        ds match {
            case Nil => Nil
            case Cons(x, xs) => Cons(x.toString, listToString(xs))
        }

    // Exercise 3.18
    def map[A,B](as: List[A])(f: A => B): List[B] =
        foldRight(as, Nil: List[B])((x,y) => Cons(f(x), y))

    // Exercise 3.19
    def filter[A](as: List[A])(f: A => Bool): List[A] =
        foldRight(as, Nil: List[A])((x,y) => if (f(x)) then Cons(x, y) else y)

    // Exercise 3.20
    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
        foldRight(as, Nil: List[A])( (x,y) => append2(f(x), y))

    // Exercise 3.21
    def filterFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
        flatMap(as)( a => if (f(a)) then List(a) else Nil)

    // Exercise 3.22
    def zipSum(xs: List[Int], ys: List[Int]): List[Int] =
        (xs, ys) match {
            case (Nil, _) => Nil
            case (_, Nil) => Nil
            case (Cons(a,as), Cons(b,bs)) => Cons(a+b, zipSum(as,bs))
        }

    // Exercise 3.23
    def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] =
        (as, bs) match {
            case (Nil, _) => Nil
            case (_, Nil) => Nil
            case (Cons(x,xs), Cons(y,ys)) =>
                Cons(f(x,y), zipWith(xs,ys)(f))
        }

    // Exercise 3.24
    def prefix[A](as: List[A], pre: List[A]): Boolean =
        (as, pre) match {
            case (_, Nil) => true
            case (Nil, _) => false
            case (Cons(x,xs), Cons(y,ys)) =>
                if (x == y) prefix(xs, ys)
                else false
        }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
        prefix(sup, sub) || hasSubsequence(tail(sup), sub)
    }

}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

    // Exercise 3.25
    def size[A](tree: Tree[A]): Int = tree match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + size(l) + size(r)
    }

    // Exercise 3.26
    def maximum(tree: Tree[Int]): Int = tree match {
        case Leaf(x) => x
        case Branch(l, r) => maximum(l) max maximum(r)
    }

    // Exercise 3.27
    def depth[A](tree: Tree[A]): Int = tree match {
        case Leaf(x) => 1
        case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

    // Exercise 3.28
    def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
        case Leaf(x) => Leaf(f(x))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    // Exercise 3.29
    def fold[A,B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B = tree match {
        case Leaf(x) => f(x)
        case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
    def size2[A](tree: Tree[A]): Int =
        fold(tree)(x => 1)((x, y) => x + y + 1)
    def maximum2(tree: Tree[Int]): Int =
        fold(tree)(x => x)((x,y) => x max y)
    def depth2[A](tree: Tree[A]): Int =
        fold(tree)(x => 1)((x,y) => 1 + (x max y))
    def map2[A,B](tree: Tree[A])(f: A => B): Tree[B] =
        fold(tree)(x => Leaf(f(x)): Tree[B])(Branch(_,_))


}
