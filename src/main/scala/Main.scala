object Main extends App {
  sealed trait Player {
    type Other
  }
  sealed trait Attacker extends Player {
    override type Other = Defender
  }
  sealed trait Defender extends Player {
    override type Other = Attacker
  }

  sealed trait BADTree[T <: Player]

  final case class Action[T <: Player](/* TODO action details -- cost, probability */) extends BADTree[T]
  final case class Conj[T <: Player](left: BADTree[T], right: BADTree[T]) extends BADTree[T]
  final case class Disj[T <: Player](left: BADTree[T], right: BADTree[T]) extends BADTree[T]
  final case class Counter[T <: Player](blocked: BADTree[T]) extends BADTree[T#Other]
  final case class Complement[T <: Player](blocked: BADTree[T]) extends BADTree[T#Other]
  final case object TRUE extends BADTree[Player] // trivially-successful action [Asl16]
  final case object FALSE extends BADTree[Player] // trivially-failed action [Asl16]

  val is: BADTree[Attacker] = Action() // identify bribable employee
  val bs: BADTree[Attacker] = Action() // bribe employee

  val t1: BADTree[Defender] = Action() // train for security
  val tf: BADTree[Defender] = Action() // threaten to fire if bribed

  val t2: BADTree[Defender] = Action() // train for not being tricked
  val at: BADTree[Defender] = Action() // authenticate tag against forgery

  val im: BADTree[Attacker] = Action() // infiltrate management
  val ot: BADTree[Attacker] = Action() // order tag replacement
  val st: BADTree[Attacker] = Action() // send false tag
  val ba: BADTree[Attacker] = Action() // bypass tag authentication

  val bribe: BADTree[Attacker] = Conj(is, Conj(bs, Complement(Conj(t1, tf))))
  val b: BADTree[Attacker] = Action() // blackmail employee
  val t: BADTree[Attacker] = Action() // trick employee

  val trick = Conj(
    Disj(
      Conj(
        st,
        Complement(
          Conj(
            at,
            Complement(ba)
          )
        )
      ),
      Conj(im, ot)
    ),
    Complement(t2)
  )

  val tree = Disj(bribe, Disj(t, Disj(b, trick)))

}
