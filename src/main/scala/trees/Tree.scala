package trees

// Define a recursive Bin Tree structure
case class Tree[+T](value: T, left: Option[Tree[T]], right: Option[Tree[T]])

object TreeObj {

  /**
   * Helper function: For o return f: A => Int if Some or 0 if None
   *   also could try opt map f getOrElse 0
   */
  def zeroIfNone[A](o: Option[A], f: A => Int): Int = o match {
    case None => 0
    case Some(x) => f(x)
  }

  /**
   * Helper function: Return value of function provided if Some, or 0 otherwise
   */
  def zeroIfNoneVal[A](o: Option[A], n: => Int) = zeroIfNone[A](o, _ => n)

  /**
   * Helper function: Default to provided value if option is None
   */
  def defaultIfNone[A](o: Option[A], f: A => Int, d : Int): Int = o match {
    case None => d
    case Some(x) => f(x)
  }

  /**
   * Return the size of the tree
   */
  def size[A](t : Option[Tree[A]]) : Int = t match {

    case None => 0
    case Some(x) =>
      zeroIfNoneVal(x.left, size(x.left)) + zeroIfNoneVal(x.right, size(x.right)) + 1
      //zeroIfNone(x.left, (_:Tree[Any]) => (size(x.left))) +
      //zeroIfNone(x.right, (_:Tree[Any]) => (size(x.right))) + 1
  }

  /**
   * Alternative size using nested function
   */
  def size2[A](t : Option[Tree[A]]) : Int = {

    def sizeB(b : Option[Tree[A]]) : Int = b match {
      case None => 0
      case Some(x) => size(b);
    }

    t match {
      case None => 0
      case Some(x) =>
        sizeB(x.left) + sizeB(x.right) + 1
    }
  }

  /**
   * Return the max depth of the tree
   */
  def depth[A](t : Option[Tree[A]]) : Int = t match {

    case None => 0
    case Some(x) =>
      math.max(zeroIfNoneVal(x.left, depth(x.left)), zeroIfNoneVal(x.right, depth(x.right))) + 1
  }

  /**
   * Alternative deptch, return the max depth of the tree using nested function
   */
  def depth2[A](t : Option[Tree[A]]) : Int = {

    def depthB(b : Option[Tree[A]]) : Int = b match {
      case None => 0
      case Some(x) => depth(b);
    }

    t match {

      case None => 0
      case Some(x) =>
        math.max(depthB(x.left), depthB(x.right)) + 1
    }
  }

  /**
   * Breadth first search on a tree, a list is used as a basic fifo queue to
   *   give the bfs traversal
   */
  def breadthFirstSearch[A](t: Tree[A], f: Tree[A] => Unit): Unit = {
    def bfs(ts: List[Tree[A]]): Unit = ts match {
      case Nil => // ignore
      case _ =>
        val children = for{tree <- ts
          Some(child) <- List(tree.left, tree.right)}
          yield child
      ts map f
      bfs(children)
    }

    bfs(List(t))
  }

  /**
   * Sum up the elements of a numeric tree
   */
  def sumTree[A <: Int](t: Option[Tree[A]]) : Int = t match {

    case None => 0
    case Some(x) =>
      sumTree(x.left) + sumTree(x.right) + x.value
  }

  /**
   * Fold up a tree into a single value, using provided map and join functions
   */
  def treeFold[A, B](t: Option[Tree[A]],
                     f: Option[Tree[A]] => Option[B],
                     j: (Option[B], Option[B]) => Option[B]) : Option[B] = t match {

    case None => None
    case Some(x) => j(j(f(t), treeFold(x.left, f, j)), treeFold(x.right, f, j))
  }

  /**
   * Perform an inorder tree traversal using the provided Tree[A] => Unit functio
   */
  def inOrder[A](t: Option[Tree[A]], f: Tree[A] => Unit) : Unit = t match {

    case None =>
    case Some(x) =>
      if (x.left != None) inOrder(x.left, f)
      f(x)
      if (x.right != None) inOrder(x.right, f)
  }

  /**
   * Perform a preorder tree traversal using the provided Tree[A] => Unit functio
   */
  def preOrder[A](t: Option[Tree[A]], f: Tree[A] => Unit) : Unit = t match {

    case None =>
    case Some(x) =>
      f(x)
      if (x.left != None) inOrder(x.left, f)
      if (x.right != None) inOrder(x.right, f)
  }

  /**
   * Perform a postorder tree traversal using the provided Tree[A] => Unit functio
   */
  def postOrder[A](t: Option[Tree[A]], f: Tree[A] => Unit) : Unit = t match {

    case None =>
    case Some(x) =>
      if (x.left != None) inOrder(x.left, f)
      if (x.right != None) inOrder(x.right, f)
      f(x)
  }

  /**
   * build tree and run tree functions on it
   */
  def main(args : Array[String]) {

    /* Test Tree structure

                1
               / \
              2   3
             / \ / \
            4  x 5  6
           
    */

    // build a tree
    val myTree = Tree(1,
      Some(Tree(2,
        Some(Tree(4, None, None)),
          None
        )
      ),
      Some(Tree(3,
        Some(Tree(5, None, None)),
          Some(Tree(6, None, None))
        )
      )
    )

    printBfsTree(myTree)

    println("Size = " + size(Some(myTree)))

    println("Depth = " + depth(Some(myTree)))

    println("Sum = " + sumTree(Some(myTree)))
    
    var sum = treeFold[Int, Int](
                Some(myTree),
                (t: Option[Tree[Int]]) => t match { case None => Some(0); case Some(x) => Some(x.value) },
                (x: Option[Int], y: Option[Int]) =>
                  Some(zeroIfNone[Int](x, (x:Int) => x) + zeroIfNone[Int](y, y => y))
              )

    var mult = treeFold[Int, Int](
                Some(myTree),
                (t: Option[Tree[Int]]) => t match { case None => Some(0); case Some(x) => Some(x.value) },
                (x: Option[Int], y: Option[Int]) =>
                  Some(defaultIfNone[Int](x, (x:Int) => x, 1) * defaultIfNone[Int](y, y => y, 1))
               )

    println("Sum Result = " + sum)
    println("Mult Result = " + mult)

    println("InOrder = ")
    inOrder(Some(myTree), (t: Tree[Any]) => println(t.value))

    println("PreOrder = ")
    preOrder(Some(myTree), (t: Tree[Any]) => println(t.value))

    println("PostOrder = ")
    postOrder(Some(myTree), (t: Tree[Any]) => println(t.value))
  }

  /**
   * Print a tree using BFS, passing in print unit function
   */
  def printBfsTree[A](tree: Tree[A]) {

    breadthFirstSearch(tree, (t: Tree[A]) => println(t.value))
  }
}
