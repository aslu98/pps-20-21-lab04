package u04lab.code

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class ComplexTest {

  val complexArray = Array(Complex(10,20), Complex(1,1), Complex(7,0))
  val caseComplexArray = Array(CaseComplex(10,20), CaseComplex(1,1), CaseComplex(7,0))

  @Test
  def testSum() {
    val c = complexArray(0) + complexArray(1) + complexArray(2)
    assertEquals(c.re, 18.0)
    assertEquals(c.im, 21.0)

    val c1 = caseComplexArray(0) + caseComplexArray(1) + caseComplexArray(2)
    assertEquals(c1.re, 18.0)
    assertEquals(c1.im, 21.0)
  }

  @Test
  def testMul() {
    val c = complexArray(0) * complexArray(1)
    assertEquals(c.re, -10.0)
    assertEquals(c.im, 30.0)

    val c1 = caseComplexArray(0) * caseComplexArray(1)
    assertEquals(c1.re, -10.0)
    assertEquals(c1.im, 30.0)
  }

  @Test
  def testTostring(): Unit ={
    assertNotEquals("ComplexImpl(10.0,20.0)", complexArray(0).toString)
    assertEquals("CaseComplexImpl(10.0,20.0)", caseComplexArray(0).toString)
  }

  @Test
  def testEquality(): Unit ={
    assertFalse(complexArray(0) == Complex(10,20))
    assertTrue(caseComplexArray(0) == CaseComplex(10,20))
  }

}