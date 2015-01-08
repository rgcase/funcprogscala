object ch02 {

    // Exercise 2.1
    def fib(n: Int): Int = {
        def fibhelp(n: Int, a: Int, b: Int): Int =
            if (n == 0) a
            else fibhelp(n-1, b, a + b)

            fibhelp(n, 0, 1)
        }

    // Exercise 2.2
    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
        @annotation.tailrec
        def go(as: Array[A], ind: Int, ord: (A,A) => Boolean, acc: Boolean): Boolean =
            if (ind < as.length-1)
            go(as, ind+1, ord, ord(as(ind), as(ind+1)) && acc)
            else acc

            go(as, 0, ordered, true)
        }

    // Exercise 2.3
    def curry[A,B,C](f: (A,B) => C): A => B => C =
        (a:A) => ((b:B) => f(a,b))

    // Exercise 2.4
    def uncurry[A,B,C](f: A => B => C): (A, B) => C =
        (a: A, b: B) => f(a)(b)

    // Exercise 2.5
    def compose[A,B,C](f: B => C, g: A => B): A => C =
        (a: A) => f(g(a))

}
