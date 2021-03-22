package u04lab.code

import u04lab.code.Complex.ComplexImpl

trait Complex {
  def re: Double
  def im: Double
  def +(c: Complex): Complex // should implement the sum of two complex numbers..
  def *(c: Complex): Complex // should implement the product of two complex numbers
}

object Complex {
  def apply(re:Double, im:Double):Complex = new ComplexImpl(re, im)

  private class ComplexImpl(override val re: Double, override val im: Double) extends Complex {
    assert(re != null && im != null)
    override def +(c: Complex): Complex = Complex(re + c.re, im + c.im) //uso implicitamente Complex.apply, data l'implementazione di apply corrisponde a fare new ComplexImpl()
    override def *(c: Complex): Complex = Complex(re*c.re - im*c.im, re*c.im + im*c.re)
  }
}

object CaseComplex{
  def apply(re:Double, im:Double):Complex = CaseComplexImpl(re, im)

  private case class CaseComplexImpl (re:Double, im:Double) extends Complex{
    assert(re != null && im != null)
    override def +(c: Complex): Complex = CaseComplex(re + c.re, im + c.im)
    override def *(c: Complex): Complex = CaseComplex(re*c.re - im*c.im, re*c.im + im*c.re)
  }
}

/** Hints:
  * - implement Complex with a ComplexImpl class, similar to PersonImpl in slides
  * - check that equality and toString do not work
  * - use a case class ComplexImpl instead, creating objects without the 'new' keyword
  * - check equality and toString now
  */