import scala.collection.parallel.ForkJoinTaskSupport

/**
  * Created by ghostbarik on 7/14/2016.
  */
object Config {

  /**
    * execution context for parallel collections
    */
  implicit val executionContext = new ForkJoinTaskSupport(
    new scala.concurrent.forkjoin.ForkJoinPool(8)
  )
}
