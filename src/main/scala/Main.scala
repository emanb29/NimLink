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

  sealed trait BADTree[+T <: Player] // it means Binary Attack-Defense Tree, dammit!

  final case class Action[+T <: Player](name: String = "") extends BADTree[T] {
    override def toString: String = name

    private val uuid: UUID = UUID.randomUUID()
    override def equals(obj: Any): Boolean = super.equals(obj) && uuid == obj.asInstanceOf[Action[T]].uuid
  }

  // T <: Player so Action[T] should be upcast to Action[Player] safely

  final case class Conj[T <: Player](left: BADTree[T], right: BADTree[T]) extends BADTree[T]

  final case class Disj[T <: Player](left: BADTree[T], right: BADTree[T]) extends BADTree[T]

  // TODO: add polarity evaluation function
  final case class Neg[T <: Player](neg: BADTree[T]) extends BADTree[T] // invert goal (eg maximize -> minimize)
  final case class Complement[T <: Player](blocked: BADTree[T]) extends BADTree[T#Other] // invert goal and switch player
  final case object TRUE extends BADTree[Player] // trivially-successful action [Asl16]
  final case object FALSE extends BADTree[Player] // trivially-failed action [Asl16]

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

  val tree = Disj(bribe, Disj(t, Disj(b, trick)))
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

  println(tree)
  println(allActionsInTree(tree))
  println(algBoolEval(tree)) // should be false, true
  println(algProbEval(tree, probabilities)) // should be 0, 0.97

  /** *
    * Pull out all basic actions from a tree into a pair of lists of actions: those of the proponent, and those of the opponent
    * NOTE: for linear trees, we could just use a pair of sets, since that's precisely the linear property
    *
    * @param tree
    * @tparam T
    * @return
    */
  def allActionsInTree[T <: Player](tree: BADTree[T]): (List[Action[T]], List[Action[T#Other]]) = {
    tree match {
      case a@Action(_) => (List(a), List.empty)
      case Conj(left, right) => allActionsInTree(left).combine(allActionsInTree(right))
      case Disj(left, right) => allActionsInTree(left).combine(allActionsInTree(right))
      case Neg(neg) => allActionsInTree(neg)
      case Complement(blocked) => allActionsInTree(blocked).swap
        .asInstanceOf[(List[Action[T]], List[Action[T#Other]])] // NOTE this is a safe cast, since the T#Other#Other = T
      case TRUE | FALSE => (List.empty, List.empty)
      case _ => throw new MatchError("Unexpected match error in action finder -- probably a failed Complement match")
    }
  }

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
    case TRUE => true
    case FALSE => false
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
    val (propActs, oppActs) = allActionsInTree(tree)
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
  def algBoolEval[T <: Player](tree: BADTree[T]): (Boolean, Boolean) = { // TODO rewrite as a special case of algProbEval
    val (propActs, oppActs) = allActionsInTree(tree) // disclaimer -- this can actually make it worst-case (and I think average-case) quadratic

    def recursiveAssist(subtree: BADTree[T]): (Boolean, Boolean) = subtree match {
      case a@Action(_) if propActs.contains(a) => (false, true)
      case a@Action(_) if oppActs.contains(a) => (true, false)
      case a@Action(_) => throw new MatchError("Unexpected match error in algBoolEval recursion -- basic Action in neither propActs nor oppActs")
      case Conj(left, right) =>
        val ((min1, max1), (min2, max2)) = (recursiveAssist(left), recursiveAssist(right))
        (min1 && min2, max1 && max2)
      case Disj(left, right) =>
        val ((min1, max1), (min2, max2)) = (recursiveAssist(left), recursiveAssist(right))
        (min1 || min2, max1 || max2)
      case Neg(inner) => recursiveAssist(inner).map(b => !b) // TODO verify this applies the map to both elems
      case Complement(inner) => algBoolEval(inner).map(b => !b) // TODO verify this applies the map to both elems
      case TRUE => (true, true)
      case FALSE => (false, false)
    }

    recursiveAssist(tree)
  }

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
    val (propActs, oppActs) = allActionsInTree(tree) // disclaimer -- this can actually make it worst-case (and I think average-case) quadratic

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
      case Neg(inner) => recursiveAssist(inner).map(p => 1 - p) // TODO verify this applies the map to both elems
      case Complement(inner) => algProbEval(inner, successChance).map(p => 1 - p) // TODO do I need to invert all success chances?
      case TRUE => (1, 1)
      case FALSE => (0, 0)
    }

    recursiveAssist(tree)
  }
}
