package u04lab.code

import Optionals._
import Lists._
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

class PowerIteratorsTest {

  val factory = new PowerIteratorsFactoryImpl()

  @Test
  def testIncremental() {
    val pi = factory.incremental(5,_+2); // pi produce 5,7,9,11,13,...
    assertEquals(Option.of(5), pi.next())
    assertEquals(Option.of(7), pi.next())
    assertEquals(Option.of(9), pi.next())
    assertEquals(Option.of(11), pi.next())

    assertEquals(List.Cons(5, List.Cons(7, List.Cons(9, List.Cons(11,List.Nil())))), pi.allSoFar()) // elementi già prodotti

    for (_ <- 0 until 10) {
      pi.next() // procedo in avanti per un po'..
    }
    assertEquals(Option.of(33), pi.next()) // sono arrivato a 33

    val piRev = pi.reversed()
    assertEquals(Option.of(33), piRev.next())
    assertEquals(Option.of(31), piRev.next())
    assertEquals(Option.of(29), piRev.next())
  }

  @Test
  def testFromList(): Unit ={
    val pi = factory.fromList(List.Cons(27, List.Cons(4, List.Cons(7, List.Cons(12,List.Nil())))))
    assertEquals(Option.of(27), pi.next())
    assertEquals(Option.of(4), pi.next())
    assertEquals(Option.of(7), pi.next())

    assertEquals(List.Cons(27, List.Cons(4, List.Cons(7, List.Nil()))), pi.allSoFar())

    for (_ <- 0 until 2) {
      pi.next()
    }
    assertEquals(Option.empty, pi.next())

    val piRev = pi.reversed()
    assertEquals(Option.of(12), piRev.next())
    assertEquals(Option.of(7), piRev.next())
  }

  @Test
  def testRandomBooleans(): Unit ={
    val pi = factory.randomBooleans(5)
    pi.next()
    pi.next()
    pi.next()
    assertEquals(3, List.length(pi.allSoFar()))

    val piRev = pi.reversed()
    piRev.next()
    assertEquals(1, List.length(piRev.allSoFar()))
  }
}