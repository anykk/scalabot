import Models.Polls

object Main {
  private val cp = CommandParser
  private val sm = StateManager

  private var pr = Polls(Map.empty)

  def main(args: Array[String]): Unit = {
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
    _     <- putStrLn(
      { val (s, a) = sm.performAction(cp.parse(input))(pr)
        pr = s
        a.toString
      }
    )
    _     <- if (input == "/bye") IO(Unit) else loop
  } yield Unit
}
