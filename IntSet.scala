// A class of objects to represent a set

class IntSet{
  // State: S : P(Int) (where "P" represents power set)

  // The following lines just define some aliases, so we can subsequently
  // write "Node" rather than "IntSet.Node".
  private type Node = IntSet.Node
  // Constructor
  private def Node(datum: Int, next: Node) = new IntSet.Node(datum, next)

  // Init: S = {}
  private var theSet : Node = Node(0, null) // or however empty set is represented
  private var _size = 0

  /** Convert the set to a string.
    * (The given implementation is not sufficient.) */
  override def toString : String = theSet.toString

  /** Add element e to the set
    * Post: S = S_0 U {e} 
    * Time complexity: O(n)
    * Space complexity: O(1)
    */
  def add(e: Int) : Unit = {
      if (!contains_unchecked(e)) {
        theSet.next = Node(e, theSet.next)
        _size += 1
      }
  }

  /** Length of the list
    * Post: S = S_0 && returns #S 
    * Time complexity: O(1)
    * Space complexity: O(1)
    */
  def size : Int = {
      _size
  }

  /** Does the set contain e?
    * Post: S = S_0 && returns (e in S)
    * Time complexity: O(n)
    * Space complexity: O(1)
     */
  def contains(e: Int) : Boolean = {
      var node = theSet
      // Invariant I ^= e not in map (\x -> x.datum) L(theSet.next, node.next)
      while (node.next!=null && node.next.datum != e) {
          // I
          node = node.next
          // I
      }
      if (node.next != null) {
        // Bubble element to the top of the list
        var temp = node.next.next
        node.next.next = theSet.next
        theSet.next = node.next
        node.next = temp
        true
      } else false
  }

  private def contains_unchecked(e: Int) : Boolean = {
      var node = theSet
      // Invariant I ^= e not in map (\x -> x.datum) L(theSet.next, node.next)
      while (node.next!=null && node.next.datum != e) {
          // I
          node = node.next
          // I
      }
      node.next != null
  }

  /** Return any member of the set.  (This is comparable to the operation
    * "head" on scala.collection.mutable.Set, but we'll use a name that does
    * not suggest a particular order.)
    * Pre: S != {}
    * Post: S = S_0 && returns e s.t. e in S */
  def any : Int = {
    require(theSet.next != null)
    theSet.next.datum
  }

  /** Does this equal that?
    * Post: S = S_0 && returns that.S = S */
  override def equals(that: Any) : Boolean = that match {
    case s: IntSet => {
        var node = s.theSet
        var counter = 0
        // Invariant I ^= map (\x -> x.datum) L(that, node) subset of
        //                map (\x -> x.datum) L(theSet)
        //             && counter == length L(that, node)
        while (node.next != null && contains_unchecked(node.next.datum)) {
          // OPTIMIZATION: Prevent bubbling as it pushes elements to the top
          // Which would otherwise cause worse performance
          node = node.next
          counter += 1
        }
        counter == _size
    }
    case _ => false
  }

  /** Remove e from the set; result says whether e was in the set initially
    * Post S = S_0 - {e} && returns (e in S_0) */
  def remove(e: Int) : Boolean = {
    var node = theSet
    while (node.next != null && node.next.datum != e) {
      node = node.next
    }
    if (node.next == null) return false
    else {
      node.next = node.next.next
      _size -= 1
      true
    }
  }
    
  /** Test whether this is a subset of that.
    * Post S = S_0 && returns S subset-of that.S */
  def subsetOf(that: IntSet) : Boolean = {
    var node = theSet
    while (node.next != null && that.contains_unchecked(node.next.datum)) {
      node = node.next
    }
    node.next == null
  }

  // ----- optional parts below here -----

  /** return union of this and that.  
    * Post: S = S_0 && returns res s.t. res.S = this U that.S */
  def union(that: IntSet) : IntSet = {
    val newSet = IntSet()
    var node1 = theSet.next
    var node2 = that.theSet.next
    while (node1 != null) {
      newSet.add(node1.datum)
      node1 = node1.next
    }
    while (node2 != null) {
      newSet.add(node2.datum)
      node2 = node2.next
    }
    newSet
  }

  /** return intersection of this and that.  
    * Post: S = S_0 && returns res s.t. res.S = this intersect that.S */
  def intersect(that: IntSet) : IntSet = {
    val newSet = IntSet()
    var node1 = theSet.next
    while (node1 != null && that.contains_unchecked(node1.datum)) {
      newSet.add(node1.datum)
      node1 = node1.next
    }
    newSet
  }

  /** map
    * Post: S = S_0 && returns res s.t. res.S = {f(x) | x <- S} */
  def map(f: Int => Int) : IntSet = {
    val newSet = IntSet()
    var node1 = theSet.next
    while (node1 != null) {
      newSet.add(f(node1.datum))
      node1 = node1.next
    }
    newSet
  }

  /** filter
    * Post: S = S_0 && returns res s.t. res.S = {x | x <- S && p(x)} */
  def filter(p : Int => Boolean) : IntSet = {
    val newSet = IntSet()
    var node1 = theSet.next
    while (node1 != null) {
      if (p(node1.datum)) newSet.add(node1.datum)
      node1 = node1.next
    }
    newSet
  }
}


// The companion object
object IntSet{
  /** The type of nodes defined in the linked list */
  private class Node(var datum: Int, var next: Node) {
    override def toString : String = {
        var output = "{";
        var node = this;
        // Invariant I: node != null && node.datum NOT in output str
        while (node.next != null) {
            // I
            output = output + s"${node.datum},"; // node.datum in output str
            node = node.next; // I
        }
        // I && node.next == null;
        // node is last && node.datum NOT in output str
        // Let's add last node datum to output str
        output = output + s"${node.datum}}";
        output;
    }
  }

  /** Factory method for sets.
    * This will allow us to write, for example, IntSet(3,5,1) to
    * create a new set containing 3, 5, 1 -- once we have defined 
    * the main constructor and the add operation. 
    * post: returns res s.t. res.S = {x1, x2,...,xn}
    *       where xs = [x1, x2,...,xn ] */
  def apply(xs: Int*) : IntSet = {
    val s = new IntSet; for(x <- xs) s.add(x); s
  }
}