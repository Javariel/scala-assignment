package patmat

import org.junit._
import org.junit.Assert.assertEquals

class HuffmanSuite {
  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }


  @Test def `weight of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(5, weight(t1))
    }


  @Test def `chars of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(List('a','b','d'), chars(t2))
    }

  @Test def `timesReduce hello world`: Unit =
    println(
      timesReduce(('r', 1), times(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')))
    )

  @Test def `times hello world`: Unit =
    assertEquals(
      List(('d',3), ('l',3), ('r',1), ('o',2), ('w',1), (' ',1), (',',1), ('e',1), ('h',1)),
      times(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd', 'd', 'd'))
    )

  @Test def `times empty list`: Unit =
    assertEquals(
      List(),
      times(List())
    )

  @Test def `string2chars hello world`: Unit =
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'), string2Chars("hello, world"))


  @Test def `make ordered leaf list for some frequency table (15pts)`: Unit =
    assertEquals(List(Leaf('e',1), Leaf('t',2), Leaf('x',3)), makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))


  @Test def `combine of some leaf list (15pts)`: Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)), combine(leaflist))
  }

  @Test def `until of some leaf list (15pts)`: Unit = {
    val leaflist = List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),Leaf('x',4),List('e', 't', 'x'),7))
    assertEquals(List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)),
      until(singleton, combine)(leaflist))
  }

  @Test def `createCodeTree hello world`: Unit =
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'), createCodeTree(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd')))

  @Test def `createCodeTree example`: Unit =
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'),
      createCodeTree(
        List('A', 'A', 'A', 'A', 'A', 'A', 'A', 'A',
          'B', 'B', 'B',
          'C', 'D', 'E', 'F', 'G', 'H')))


  @Test def `decode example`: Unit =
    assertEquals((),
      decode(
        createCodeTree(
          List('A', 'A', 'A', 'A', 'A', 'A', 'A', 'A',
          'B', 'B', 'B',
          'C', 'D', 'E', 'F', 'G', 'H')),
        List(1,0,0,0,1,0,1,0))
    )

  @Test def `encode example`: Unit =
    assertEquals(List(1, 0, 0, 1),
      encode(
        createCodeTree(
          List('A', 'A', 'A', 'A', 'A', 'A', 'A', 'A',
            'B', 'B', 'B',
            'C', 'D', 'E', 'F', 'G', 'H')))
      (List('C'))
    )


  @Test def `decode and encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("ab".toList, decode(t1, encode(t1)("ab".toList)))
    }

  @Test def `decode and encode a short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("adb".toList, decode(t2, encode(t2)("adb".toList)))
    }

  @Test def `covert code table`: Unit =
    System.out.println(convert(createCodeTree(
      List('A', 'A', 'A', 'A', 'A', 'A', 'A', 'A',
        'B', 'B', 'B',
        'C', 'D', 'E', 'F', 'G', 'H'))))

  @Test def `decode and quick encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("ab".toList, decode(t1, quickEncode(t1)("ab".toList)))
    }

  @Test def `convert t2`: Unit =
    new TestTrees {
      System.out.println(convert(t2))
      System.out.println(t2)
    }

  @Test def `decode and quick encode a short text should be identity (10pts)`: Unit =
    new TestTrees {
      assertEquals("adb".toList, decode(t2, quickEncode(t2)("adb".toList)))
    }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
