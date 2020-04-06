import scala.io.StdIn

object Main {

  // Console program
  // Writes a string
  // Reads user input

  // Console[A]: when interpreted, it will give you a VALUE of TYPE A
  // interpret(Console[Int]) ----> 1 or 2 or 3 etc.
  sealed trait Console[A] {
    def map[B](f: A => B): Console[B] = Map(this, f)
    def flatMap[B](f: A => Console[B]): Console[B] = FlatMap(this, f)
  }

  case class WriteLine(line: String) extends Console[Unit]

  case object ReadLine extends Console[String]

  case class Map  [Z, A](consoleZ: Console[Z], f: Z => A) extends Console[A]

  case class FlatMap[Z, A](consoleZ: Console[Z], f: Z => Console[A]) extends Console[A]

  case class EndWith[A](result: A) extends Console[A]

  def main(args: Array[String]): Unit = {

    val program = ReadLine.flatMap((name: String) => WriteLine(s"Hello $name").map(_ => name))

    val anotherProgram: Console[Int] = program.map((name: String) => name.length)

    val program3 = anotherProgram.flatMap((num: Int) => WriteLine(s"The length is $num"))

    val result = interpret(program3)

    println("\n***\n")

    println(s"Result is: $result")

    ()

  }

  // Console[A]: when interpreted, it will give you a VALUE of TYPE A
  def interpret[A](console: Console[A]): A =
    console match {
      case WriteLine(line) => println(line)
      case ReadLine => StdIn.readLine()
      case Map(consoleZ, f) =>
        val z = interpret(consoleZ)
        val a = f(z)
        a
      case FlatMap(consoleZ, f) =>
        val z = interpret(consoleZ)
        val consoleA = f(z)
        val a = interpret(consoleA)
        a
      case EndWith(result) => result
    }

}









