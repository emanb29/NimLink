import java.util.UUID

import cats._
import cats.instances._
import cats.implicits._
import Util._

object Main extends App {
  // Definition of test case as outlined in Asl16
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
  // End test case (Asl16) definition

  println(algProbEvalWithCost(testtree, probabilities, costs))

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


  def algBoolEvalWithCost[T <: Player](tree: BADTree[T], cost: Action[Player] => Double)
  : (Set[(Boolean, Double)], Set[(Boolean, Double)]) = algProbEvalWithCost(tree, _ => (0, 1), cost)
    .bimap(_.map(x => (x._1.round == 1, x._2)),
      _.map(x => (x._1.round == 1, x._2)))

  /**
    * Evaluate a tree with min/max success probabilities AND costs.
    *
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
