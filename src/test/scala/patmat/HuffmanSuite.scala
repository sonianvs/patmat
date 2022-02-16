package patmat

class HuffmanSuite extends munit.FunSuite {
  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val t3 = makeCodeTree(makeCodeTree(Leaf('a',2), Leaf('e',3)), makeCodeTree((makeCodeTree(Leaf('i',1), Leaf('o',2))), Leaf('u', 2)))

  }


  test("weight of a larger tree (10pts)") {
    new TestTrees {
      assertEquals(weight(t2), 9)
    }
  }


  test("chars of a larger tree (10pts)") {
    new TestTrees {
      assertEquals(chars(t2), List('a','b','d'))
    }
  }

  test("string2chars hello world") {
    assertEquals(string2Chars("hello, world"), List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("characters frequency of a string") {
    new TestTrees {
      assertEquals(times(string2Chars("papallona")), List(('p',2),('a',3),('l',2),('o',1),('n',1)))
    }
  }


  test("make ordered leaf list for some frequency table (15pts)") {
    assertEquals(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))), List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("make ordered leaf list for some frequency table (2nd test)") {
    assertEquals(makeOrderedLeafList(List(('p',2),('a',3),('l',2),('o',1),('n',1))), List(Leaf('o',1), Leaf('n',1), Leaf('p',2), Leaf('l',2), Leaf('a',3)))
  }


  test("detect a singleton tree list") {
    new TestTrees {
      val singletonList = List(t2)
      assert(singleton(singletonList))
    }
  }


  test("combine of some leaf list (15pts)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(combine(leaflist), List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("create a single CodeTree from a list of chars") {
    val charslist = string2Chars("papallona")
    assertEquals(createCodeTree(charslist), Fork(Fork(Fork(Leaf('o', 1), Leaf('n', 1), List('o', 'n'), 2), Leaf('p',2), List('o', 'n', 'p'), 4), Fork(Leaf('l',2), Leaf('a', 3), List('l', 'a'), 5), List('o', 'n', 'p', 'l', 'a'),9))
  }

  test("decode a very short text") {
    new TestTrees {
      val t4 = Fork(Fork(Fork(Leaf('o', 1), Leaf('n', 1), List('o', 'n'), 2), Leaf('p',2), List('o', 'n', 'p'), 4), Fork(Leaf('l',2), Leaf('a', 3), List('l', 'a'), 5), List('o', 'n', 'p', 'l', 'a'),9)
      val bitList = List(1,0,1,1)
      assertEquals(decode(t4, bitList), List('l','a'))
    }
  }


  test("decode and encode a very short text should be identity (10pts)") {
    new TestTrees {
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
    }
  }


  import scala.concurrent.duration._
  override val munitTimeout = 10.seconds
}
