package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

object task_seq_riddle {

  /**
    * Рассмотрим последовательность с числами:
    *
    * 1
    * 1 1
    * 2 1
    * 1 2 1 1
    * 1 1 1 2 2 1
    * 3 1 2 2 1 1
    * ...........
    *
    * 1. Реализуйте функцию генерирующую след последовательность из текущей
    * */
  def nextLine(line: Seq[Int]): List[Int] = {

    def acc(line: Seq[Int], counter: Int, prev: Int): Seq[Int] = {
      val newList = line match {
        case Nil => List(1, prev)
        case head :: Nil => {
          if (prev == head)
            Seq(counter + 1, head)
          else
            Seq(counter, prev, 1, head)
        }
        case el2 :: xs => {
          if (prev == el2) {
            acc(xs.asInstanceOf[Seq[Int]], counter + 1, el2)
          } else {
            Seq(counter, prev.asInstanceOf[Int]) ++ acc(
              xs.asInstanceOf[Seq[Int]],
              1,
              el2
            )
          }
        }

      }

      newList.asInstanceOf[Seq[Int]]
    }

    acc(line.tail, 1, line.head).toList
  }

  case class test(res: List[Int], counter: Int, current: Int, i: Int)
  def nextLine1(line: Seq[Int]): List[Int] = {
    val result = line.foldRight(test(List[Int](), 0, 0, line.size - 1)) {
      (x, z) =>
        if ((z.current == x || z.counter == 0) && z.i > 0) {
          test(z.res, z.counter + 1, x, z.i - 1)
        } else {
          z.i match {
            case 0 if z.current == x =>
              test(List(z.counter + 1, z.current) ::: z.res, 1, x, z.i - 1)
            case 0 if z.current != x =>
              test(List(1, x, z.counter, z.current) ::: z.res, 1, x, z.i - 1)
            case _ => test(List(z.counter, z.current) ::: z.res, 1, x, z.i - 1)
          }
        }
    }

    result.res
  }
  task"Реализуйте функцию генерирующую след последовательность из текущей" ()

  /**
    * 2. Реализуйте ленивый список, который генерирует данную последовательность
    * Hint: см. LazyList.cons
    *
    * lazy val funSeq: LazyList[List[Int]]  ...
    *
    */
  val funSeq: LazyList[List[Int]] = List(1) #:: funSeq.map(
    line => nextLine(line)
  )
  task"Реализуйте ленивый список, который генерирует данную последовательность" ()
}
