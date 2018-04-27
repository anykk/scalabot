object Main {
  private val parser = CommandParser

  def main(args: Array[String]): Unit = {
    /*
    val (s1, a1) = addPoll(Poll("asd", 123, true, true, None, None, Vector.empty))(Polls(Map.empty))
    val (s2, a2) = addPoll(Poll("asd", 123, true, true, None, None, Vector.empty))(s1)
    val (s3, a3) = deletePoll(3)(s2)
    println(s3)
    */
    loop.run
  }

  class IO[A] private (run0: => A) {
    def run = run0

    def flatMap[B](f: A => IO[B]): IO[B] = IO(f(run).run)

    def map[B](f: A => B): IO[B] = flatMap(a => IO(f(a)))
  }

  object IO {
    def apply[A](v: => A): IO[A] = new IO(v)
  }

  def getLine: IO[String] = IO(scala.io.StdIn.readLine("Command: "))

  def putStrLn(v: String): IO[Unit] = IO(println(v))

  def loop: IO[Unit] = for {
    input <- getLine
    _     <- putStrLn(parser.parse(input).toString)
    _     <- if (input == "/bye") IO(Unit) else loop
  } yield Unit
}
