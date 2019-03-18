import java.util.UUID
import cats.implicits._

sealed trait BADTree[+T <: Player] { // it means Binary Attack-Defense Tree, dammit!
  /**
    * All leaves in the tree. Should be implemented as lazy in all abstract cases
    */
  val leaves: (List[Action[T]], List[Action[T#Other]])
  lazy val propActs: List[Action[T]] = leaves._1
  lazy val oppActs: List[Action[T#Other]] = leaves._2
}

final case class Action[+T <: Player](name: String = "") extends BADTree[T] {
  override val leaves: (List[Action[T]], List[Action[T#Other]]) = (List(this), List.empty)

  override def toString: String = name

  private val uuid: UUID = UUID.randomUUID()

  override def equals(obj: Any): Boolean = super.equals(obj) && uuid == obj.asInstanceOf[Action[T]].uuid
}

final case class Conj[T <: Player](left: BADTree[T], right: BADTree[T]) extends BADTree[T] {
  override lazy val leaves: (List[Action[T]], List[Action[T#Other]]) = left.leaves.combine(right.leaves)
}

final case class Disj[T <: Player](left: BADTree[T], right: BADTree[T]) extends BADTree[T] {
  override lazy val leaves: (List[Action[T]], List[Action[T#Other]]) = left.leaves.combine(right.leaves)
}

// TODO: add polarity evaluation function
final case class Neg[T <: Player](neg: BADTree[T]) extends BADTree[T] { // invert goal (eg maximize -> minimize)
  override lazy val leaves: (List[Action[T]], List[Action[T#Other]]) = neg.leaves
}

final case class Complement[T <: Player](blocked: BADTree[T]) extends BADTree[T#Other] {
  // invert goal and switch player
  override lazy val leaves: (List[Action[T#Other]], List[Action[T#Other#Other]]) = blocked.leaves.swap
    .asInstanceOf[(List[Action[T#Other]], List[Action[T#Other#Other]])] // NOTE this is a safe cast, since the T#Other#Other = T
}
