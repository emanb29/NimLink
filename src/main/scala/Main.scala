import java.util.UUID

import cats._
import cats.instances._
import cats.implicits._

object Main extends App {

  sealed trait Player {
    type Other <: Player
  }

  sealed trait Attacker extends Player {
    override type Other = Defender
  }

  sealed trait Defender extends Player {
    override type Other = Attacker
  }

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

  val is: Action[Attacker] = Action("is: identify bribable employee")
  val bs: Action[Attacker] = Action("bs: bribe employee")

  val t1: Action[Defender] = Action("t1: train for security")
  val tf: Action[Defender] = Action("tf: threaten to fire if bribed")

  val t2: Action[Defender] = Action("t2: train for not being tricked")
  val at: Action[Defender] = Action("at: authenticate tag against forgery")

  val im: Action[Attacker] = Action("im: infiltrate management")
  val ot: Action[Attacker] = Action("ot: order tag replacement")
  val st: Action[Attacker] = Action("st: send false tag")
  val ba: Action[Attacker] = Action("ba: bypass tag authentication")

  val bribe: BADTree[Attacker] = Conj(is, Conj(bs, Complement(Conj(t1, tf))))
  val b: Action[Attacker] = Action("b: blackmail employee")
  val t: Action[Attacker] = Action("t: threaten employees")

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

  val testtree = Disj(bribe, Disj(t, Disj(b, trick)))
  val probabilities: Map[Action[Player], (Double, Double)] = Map(
    is -> (0.2, 0.8),
    bs -> (0, 0.7),
    t1 -> (0.1, 0.3),
    tf -> (0.1, 0.4),
    t -> (0, 0.7),
    b -> (0, 0.7),
    st -> (0, 0.5),
    at -> (0.1, 0.7),
    ba -> (0.1, 0.6),
    im -> (0, 0.5),
    ot -> (0, 0.6),
    t2 -> (0.1, 0.4)
  )
  val costs: Map[Action[Player], Double] = Map(
    is -> 80,
    bs -> 100,
    t1 -> 0,
    tf -> 0,
    t -> 160,
    b -> 150,
    st -> 50,
    at -> 0,
    ba -> 85,
    im -> 70,
    ot -> 0,
    t2 -> 0
  )

  //  println(testtree)
  //  println(testtree.leaves)
  //  println(algBoolEval(testtree)) // should be false, true
  //  println(algProbEval(testtree, probabilities)) // should be 0, 0.97
  //  println(algBoolEvalWithCost(testtree, costs)) // should be ???


  println(algProbEvalWithCost(testtree, probabilities, costs))

  /** *
    * Evaluate the tree recursively as a boolean expression, given a boolean map m(a). This is "Curvy B" in the literature
    *
    * @param tree
    * @param actionBooleanVals A map of each basic action to its boolean value. This is m(a) (ie the combination of m_o and m_p) in the literature
    * @tparam T the type/proponent of the tree -- by root node
    * @return The boolean analysis/evaluation of the tree
    */
  def boolAnalyze[T <: Player](tree: BADTree[T], actionBooleanVals: Action[Player] => Boolean): Boolean = tree match {
    case a@Action(_) => actionBooleanVals(a) // this is "m" in the literature
    case Conj(left, right) => boolAnalyze(left, actionBooleanVals) && boolAnalyze(right, actionBooleanVals)
    case Disj(left, right) => boolAnalyze(left, actionBooleanVals) || boolAnalyze(right, actionBooleanVals)
    case Neg(neg) => !boolAnalyze(neg, actionBooleanVals)
    case Complement(blocked) => !boolAnalyze(blocked, actionBooleanVals)
  }

  /** *
    * Semantically evaluate a tree -- this is the most basic M(t)
    *
    * @param tree
    * @tparam T the type/proponent of the tree -- by root node
    * @return A pair of the minimum and maximum possible success values of the goal of the proponent (T)
    */
  @deprecated("Use algBoolEval instead -- it has the same result for polarity-consistent trees and runs in O(n) time instead of EXP")
  def semBoolEval[T <: Player](tree: BADTree[T]): (Boolean, Boolean) = {
    val (propActs, oppActs) = tree.leaves
    val allPlayerActions: Seq[Action[Player]] = propActs ++ oppActs
    // "for every boolean assignment"
    // generate every boolean vector with len == # actions (ie all bool numbers up to 2^(# actions) - 1 )+
    // zip each with actions and convert to Map
    // analyze each finding min/max as applicable[?] TODO: read paper again for more details
    ???
  }

  /** *
    * Algorithmically evaluate a tree -- this is the most basic INT(t) in the literature
    *
    * @param tree a polarity-consistent tree
    * @tparam T the type/proponent of the tree -- by root node
    * @return A pair of the minimum and maximum possible success values of the goal of the proponent (T)
    */
  def algBoolEval[T <: Player](tree: BADTree[T]): (Boolean, Boolean) = algProbEval(tree, _ => (0, 1)).bimap(_.round == 1, _.round == 1)

  /** *
    * Algorithmically evaluate a tree probabilistically
    *
    * @param tree          a polarity-consistent tree
    * @param successChance A map of each basic action to its probability values. This is (p_1(a), p_2(a)) in the literature.
    *                      0 corresponds to failure, 1 to success.
    *                      p_1 is the success probability of the action without taking it -- the minimum
    *                      probability of success for the action independent of action
    *                      p_2 is the success probability of the action having taken the action -- the
    *                      maximum probability of success given best-effort
    *
    *                      As an example, for decrypting passwords, p_1 might be .1 if there's a 10% chance the passwords
    *                      aren't encrypted to begin with. p_2 would be perhaps .5 if there's a 50% chance a given password
    *                      can be cracked by the proponent.
    *
    *                      NOTE: For all A, p_2(a) >= p_1(a)
    * @tparam T the type/proponent of the tree -- by root node
    * @return A pair of the minimum and maximum possible success values of the goal of the proponent (T)
    */
  def algProbEval[T <: Player](tree: BADTree[T], successChance: Action[Player] => (Double, Double)): (Double, Double) = {
    val (propActs, oppActs) = tree.leaves // disclaimer -- this can actually make it worst-case (and I think average-case) quadratic

    def recursiveAssist(subtree: BADTree[T]): (Double, Double) = subtree match {
      case a@Action(_) if propActs.contains(a) => successChance(a)
      case a@Action(_) if oppActs.contains(a) => successChance(a).swap
      case a@Action(_) => throw new MatchError("Unexpected match error in algProbEval recursion -- basic Action in neither propActs nor oppActs")
      case Conj(left, right) =>
        val ((min1, max1), (min2, max2)) = (recursiveAssist(left), recursiveAssist(right))
        (min1 * min2, max1 * max2)
      case Disj(left, right) =>
        val ((min1, max1), (min2, max2)) = (recursiveAssist(left), recursiveAssist(right))
        (1 - (1 - min1) * (1 - min2),
          1 - (1 - max1) * (1 - max2))
      case Neg(inner) => recursiveAssist(inner).bimap(1 - _, 1 - _)
      case Complement(inner) => algProbEval(inner, successChance).bimap(1 - _, 1 - _)
    }

    recursiveAssist(tree)
  }

  /**
    * Find dominating strategies by selecting only the elements of the set Z with a high first value or a low second value
    *
    * @param Z the set to assess
    * @param O an ordering of Decider elements -- should be provided by default by scala
    * @tparam Decider the type that will be used to determine if/when an action gets taken. Boolean for the boolean case and Double for the probabilistic case.
    * @return
    */
  def MRMaxMin[Decider](Z: Set[(Decider, Double)])(implicit O: Ordering[Decider]): Set[(Decider, Double)] =
    Z.filter(pair => {
      val (x1, y1) = pair
      (Z - pair).forall { case (x2, y2) => (O.gteq(x1, x2) || y1 < y2) && (O.gt(x1, x2) || y1 <= y2) }
    })

  /**
    * Find dominated strategies by selecting only the elements of the set Z with a low first value or a low second value
    *
    * @param Z the set to assess
    * @param O an ordering of Decider elements -- should be provided by default by scala
    * @tparam Decider
    * @return
    */
  def MRMinMin[Decider](Z: Set[(Decider, Double)])(implicit O: Ordering[Decider]): Set[(Decider, Double)] =
    Z.filter(pair => {
      val (x1, y1) = pair
      (Z - pair).forall { case (x2, y2) => (O.lteq(x1, x2) || y1 < y2) && (O.lt(x1, x2) || y1 <= y2) }
    })


  def algBoolEvalWithCost[T <: Player](tree: BADTree[T], cost: Action[Player] => Double)
  : (Set[(Boolean, Double)], Set[(Boolean, Double)]) = {
    val (propActs, oppActs) = tree.leaves // disclaimer -- this can actually make it worst-case (and I think average-case) quadratic

    def recursiveAssist(subtree: BADTree[Player]): (Set[(Boolean, Double)], Set[(Boolean, Double)]) = subtree match {
      case a@Action(_) if propActs.contains(a) => (
        MRMinMin(Set((false, 0), (true, cost(a)))),
        MRMaxMin(Set((false, 0), (true, cost(a)))),
      )
      case a@Action(_) if oppActs.contains(a) => (
        MRMinMin(Set((true, 0))), // TODO is this as strictly uneccesary as it appears?
        MRMaxMin(Set((false, 0)))
      )
      case a@Action(_) => throw new MatchError("Unexpected match error in algBoolEvalWithCost recursion -- basic Action in neither propActs nor oppActs")
      case Conj(left, right) => {
        val (leftMins, leftMaxes) = recursiveAssist(left) // this is (V_1, W_1) in Aslanyan's paper
        val (rightMins, rightMaxes) = recursiveAssist(right) // this is (V_2, W_2) in Aslanyan's paper
        val minSet: Set[(Boolean, Double)] = MRMinMin(for {
          (leftActTaken, leftCost) <- leftMins
          (rightActTaken, rightCost) <- rightMins
        } yield (leftActTaken && rightActTaken, leftCost + rightCost))

        val maxSet: Set[(Boolean, Double)] = MRMaxMin(for {
          (leftActTaken, leftCost) <- leftMaxes
          (rightActTaken, rightCost) <- rightMaxes
        } yield (leftActTaken && rightActTaken, leftCost + rightCost))
        (minSet, maxSet)
      }
      case Disj(left, right) => {
        val (leftMins, leftMaxes) = recursiveAssist(left) // this is (V_1, W_1) in Aslanyan's paper
        val (rightMins, rightMaxes) = recursiveAssist(right) // this is (V_2, W_2) in Aslanyan's paper
        val minSet: Set[(Boolean, Double)] = MRMinMin(for {
          (leftActTaken, leftCost) <- leftMins
          (rightActTaken, rightCost) <- rightMins
        } yield (leftActTaken || rightActTaken, leftCost + rightCost))

        val maxSet: Set[(Boolean, Double)] = MRMaxMin(for {
          (leftActTaken, leftCost) <- leftMaxes
          (rightActTaken, rightCost) <- rightMaxes
        } yield (leftActTaken || rightActTaken, leftCost + rightCost))
        (minSet, maxSet)
      }
      case Neg(inner) =>
        val (mins, maxes) = recursiveAssist(inner)
        (
          MRMinMin(maxes.map { case (actTaken, innerCost) => (!actTaken, innerCost) }),
          MRMaxMin(mins.map { case (actTaken, innerCost) => (!actTaken, innerCost) }),
        )
      case Complement(inner) =>
        val (mins, maxes) = recursiveAssist(inner) // TODO I don't like this since we lose the info of who is currently active
        (
          MRMinMin(maxes.map { case (actTaken, innerCost) => (!actTaken, innerCost) }),
          MRMaxMin(mins.map { case (actTaken, innerCost) => (!actTaken, innerCost) }),
        )
    }

    recursiveAssist(tree)
  }

  /**
    * Evaluate a tree with min/max success probabilities AND costs.
    * @param tree
    * @param probabilities
    * @param cost
    * @tparam T
    * @return A pair of lists -- the first is the pareto-efficient solutions with the minimum success values for the
    *         proponent, the second is the pareto-efficient solutions with the maximmum success values for the
    *         proponent. The solutions are all in (probability, cost) format.
    */
  def algProbEvalWithCost[T <: Player](tree: BADTree[T], probabilities: Action[Player] => (Double, Double), cost: Action[Player] => Double)
  : (Set[(Double, Double)], Set[(Double, Double)]) = {
    val (propActs, oppActs) = tree.leaves // disclaimer -- this can actually make it worst-case (and I think average-case) quadratic

    def recursiveAssist(subtree: BADTree[Player]): (Set[(Double, Double)], Set[(Double, Double)]) = subtree match {
      case a@Action(_) if propActs.contains(a) => (
        MRMinMin(Set((probabilities(a)._1, 0), (probabilities(a)._2, cost(a)))),
        MRMaxMin(Set((probabilities(a)._1, 0), (probabilities(a)._2, cost(a)))),
      )
      case a@Action(_) if oppActs.contains(a) => (
        MRMinMin(Set((probabilities(a)._2, 0))), // TODO is this as strictly uneccesary as it appears?
        MRMaxMin(Set((probabilities(a)._1, 0)))
      )
      case a@Action(_) => throw new MatchError("Unexpected match error in algProbEvalWithCost recursion -- basic Action in neither propActs nor oppActs")
      case Conj(left, right) => {
        val (leftMins, leftMaxes) = recursiveAssist(left) // this is (V_1, W_1) in Aslanyan's paper
        val (rightMins, rightMaxes) = recursiveAssist(right) // this is (V_2, W_2) in Aslanyan's paper
        val minSet: Set[(Double, Double)] = MRMinMin(for {
          (leftChance, leftCost) <- leftMins
          (rightChance, rightCost) <- rightMins
        } yield (leftChance * rightChance, leftCost + rightCost))

        val maxSet: Set[(Double, Double)] = MRMaxMin(for {
          (leftChance, leftCost) <- leftMaxes
          (rightChance, rightCost) <- rightMaxes
        } yield (leftChance * rightChance, leftCost + rightCost))
        (minSet, maxSet)
      }
      case Disj(left, right) => {
        val (leftMins, leftMaxes) = recursiveAssist(left) // this is (V_1, W_1) in Aslanyan's paper
        val (rightMins, rightMaxes) = recursiveAssist(right) // this is (V_2, W_2) in Aslanyan's paper
        val minSet: Set[(Double, Double)] = MRMinMin(for {
          (leftChance, leftCost) <- leftMins
          (rightChance, rightCost) <- rightMins
        } yield (
          1 - ((1 - leftChance) * (1 - rightChance)),
          leftCost + rightCost
        ))

        val maxSet: Set[(Double, Double)] = MRMaxMin(for {
          (leftChance, leftCost) <- leftMaxes
          (rightChance, rightCost) <- rightMaxes
        } yield (
          1 - ((1 - leftChance) * (1 - rightChance)),
          leftCost + rightCost
        ))
        (minSet, maxSet)
      }
      case Neg(inner) =>
        val (mins, maxes) = recursiveAssist(inner)
        (
          MRMinMin(maxes.map { case (probability, treecost) => (1 - probability, treecost) }),
          MRMaxMin(mins.map { case (probability, treecost) => (1 - probability, treecost) }),
        )
      case Complement(inner) =>
        val (mins, maxes) = recursiveAssist(inner) // TODO I don't like this because we lost the info of who is currently active
        (
          MRMinMin(maxes.map { case (probability, treecost) => (1 - probability, treecost) }),
          MRMaxMin(mins.map { case (probability, treecost) => (1 - probability, treecost) }),
        )
    }

    recursiveAssist(tree)
  }

}
