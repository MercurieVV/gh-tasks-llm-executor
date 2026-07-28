import higherkindness.droste.{scheme, Algebra}
import higherkindness.droste.data.Mu
import com.github.mercurievv.ghllm.task.TaskF
import cats.Functor

object TaskTree:

  export higherkindness.droste.{ scheme, Algebra }
  export higherkindness.droste.data.Mu
  export com.github.mercurievv.ghllm.task.TaskF

  given Functor[TaskF] with
    def map[A, B](fa: TaskF[A])(f: A => B): TaskF[B] = fa match
      case TaskF.Branch(name, children) => TaskF.Branch(name, children.map(f))
      case TaskF.Leaf(task)             => TaskF.Leaf(task)

  type Tree = Mu[TaskF]

  def branch(name: String, children: List[Tree]): Tree =
    Mu(TaskF.Branch(name, children))

  def leaf(task: Option[Int]): Tree =
    Mu(TaskF.Leaf(task))
