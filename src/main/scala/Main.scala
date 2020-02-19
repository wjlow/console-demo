import scala.io.StdIn

/**
 * Practice code from http://degoes.net/articles/easy-monads
 */

sealed trait Console[A] {
  def map[B](transformer: A => B): Console[B] = Map(this, transformer)

  def flatMap[B](next: A => Console[B]): Console[B] = Chain(this, next)
}

case class WriteLine(line: String) extends Console[Unit]

case object ReadLine extends Console[String]

case class Map[Z, A](console: Console[Z], transformer: Z => A) extends Console[A]

case class Chain[Z, A](console: Console[Z], next: Z => Console[A]) extends Console[A]

case class Pure[A](value: A) extends Console[A]

object Main {

  def main(args: Array[String]): Unit = {
    val program =
      WriteLine("Type something!").flatMap(_ =>
        ReadLine
          .flatMap(userInput =>
            WriteLine(s"The user typed: $userInput")
              .map(_ => userInput)))

    val lengthProgram: Console[Int] = program.map((str: String) => str.length)

    val result = interpret(lengthProgram)

    println(s"The length of the message is: $result")

    ()
  }

  def interpret[A](console: Console[A]): A =
    console match {
      case WriteLine(line) => println(line)
      case ReadLine => StdIn.readLine()
      case Map(consoleZ, transformer) =>
        val z = interpret(consoleZ)
        val a = transformer(z)
        a
      case Chain(consoleZ, next) =>
        val z = interpret(consoleZ)
        val consoleA = next(z)
        interpret(consoleA)
      case Pure(value) => value
    }

}









