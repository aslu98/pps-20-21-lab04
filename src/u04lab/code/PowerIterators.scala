package u04lab.code

import Optionals._
import Lists._
import Streams._

import scala.util.Random

trait PowerIterator[A] {
  def next(): Option[A]
  def allSoFar(): List[A]
  def reversed(): PowerIterator[A]
}

trait PowerIteratorsFactory {
  def incremental(start: Int, successive: Int => Int): PowerIterator[Int]
  def fromList[A](list: List[A]): PowerIterator[A]
  def randomBooleans(size: Int): PowerIterator[Boolean]
}

class PowerIteratorsFactoryImpl extends PowerIteratorsFactory {

  private case class fromStream[A](iterator: Stream[A], var count:Int = 0) extends PowerIterator[A]{
    override def next(): Option[A] = {count = count + 1; Stream.get(iterator)(count)}
    override def allSoFar(): List[A] = Stream.toList(Stream.take(iterator)(count))
    override def reversed(): PowerIterator[A] = fromStream(List.toStream(List.reverse(allSoFar())))
  }

  override def incremental(start: Int, successive: Int => Int): PowerIterator[Int] =
    fromStream(Stream.iterate(start)(successive))

  override def fromList[A](list: List[A]): PowerIterator[A] =
    fromStream(List.toStream(list))

  override def randomBooleans(size: Int): PowerIterator[Boolean] =
    fromStream(Stream.take(Stream.generate(Random.nextBoolean))(size))
}
