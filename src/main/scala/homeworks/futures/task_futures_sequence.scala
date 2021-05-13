package homeworks.futures

import homeworks.HomeworksUtils.TaskSyntax

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object task_futures_sequence {

  /**
    * В данном задании Вам предлагается реализовать функцию fullSequence,
    * похожую на Future.sequence, но в отличии от нее,
    * возвращающую все успешные и не успешные результаты.
    * Возвращаемое тип функции - кортеж из двух списков,
    * в левом хранятся результаты успешных выполнений,
    * в правово результаты неуспешных выполнений.
    * Не допускается использование методов объекта Await и мутабельных переменных var
    */
  /**
    * @param futures список асинхронных задач
    * @return асинхронную задачу с кортежом из двух списков
    */
  def fullSequence1[A](
    futures: List[Future[A]]
  )(implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {

    def acc(accumulator: (List[A], List[Throwable]),
            futures: List[Future[A]]): Future[(List[A], List[Throwable])] = {
      futures match {
        case Nil => Future(accumulator)
        case x :: xs =>
          x.transformWith {
            case Success(a) => acc((accumulator._1 :+ a, accumulator._2), xs)
            case Failure(exception) =>
              acc((accumulator._1, accumulator._2 :+ exception), xs)
          }
      }
    }

    acc((List(), List()), futures)

  }

  def fullSequence[A](
    futures: List[Future[A]]
  )(implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {

    val futuresWithTry = futures.map(
      f => f.map(Success(_)).recover { case t: Throwable => Failure(t) }
    )

    futuresWithTry.foldLeft(
      (Future.successful((List.empty[A], List.empty[Throwable])))
    ) { (acc, future) =>
      for {
        a <- acc
        x <- future

      } yield {

        x match {
          case Success(xx) => (a._1 :+ xx, a._2)
          case Failure(ex) => (a._1, a._2 :+ ex)
        }
      }

    }
  }
  task"Реализуйте метод `fullSequence`" ()
}
