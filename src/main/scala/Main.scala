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

  final case class Action[+T <: Player](/* TODO action details -- cost, probability */) extends BADTree[T]

  // T <: Player so Action[T] should be upcast to Action[Player] safely

  final case class Conj[T <: Player](left: BADTree[T], right: BADTree[T]) extends BADTree[T]

  final case class Disj[T <: Player](left: BADTree[T], right: BADTree[T]) extends BADTree[T]

  // TODO: add polarity evaluation function
  final case class Neg[T <: Player](neg: BADTree[T]) extends BADTree[T] // invert goal (eg maximize -> minimize)
  final case class Complement[T <: Player](blocked: BADTree[T]) extends BADTree[T#Other] // invert goal and switch player
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

  println(tree)
  println(allActionsInTree(tree))
  println(algBoolEval(tree))

  /** *
    * Pull out all basic actions from a tree into a pair of lists of actions: those of the proponent, and those of the opponent
    *
    * @param tree
    * @tparam T
    * @return
    */
  def allActionsInTree[T <: Player](tree: BADTree[T]): (List[Action[T]], List[Action[T#Other]]) = {
    tree match {
      case a@Action() => (List(a), List.empty)
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
    * @param actionBooleanVals A Map of each basic action to its boolean value. This is m(a) (ie the combination of m_o and m_p) in the literature
    * @tparam T the type/proponent of the tree -- by root node
    * @return The boolean analysis/evaluation of the tree
    */
  def boolAnalyze[T <: Player](tree: BADTree[T], actionBooleanVals: Map[Action[Player], Boolean]): Boolean = tree match {
    case a@Action() => actionBooleanVals(a) // this is "m" in the literature
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
  def algBoolEval[T <: Player](tree: BADTree[T]): (Boolean, Boolean) = {
    val (propActs, oppActs) = allActionsInTree(tree) // disclaimer -- this can actually make it worst-case (and I think average-case) quadratic

    def recursiveAssist(subtree: BADTree[T]): (Boolean, Boolean) = subtree match {
      case a@Action() if propActs.contains(a) => (false, true)
      case a@Action() if oppActs.contains(a) => (true, false)
      case a@Action() => throw new MatchError("Unexpected match error in algBoolEval recursion -- basic Action in neither propActs nor oppActs")
      case Conj(left, right) =>
        val ((min1, max1), (min2, max2)) = (recursiveAssist(left), recursiveAssist(right))
        (min1 && min2, max1 && max2)
      case Disj(left, right) =>
        val ((min1, max1), (min2, max2)) = (recursiveAssist(left), recursiveAssist(right))
        (min1 || min2, max1 || max2)
      case Neg(inner) => recursiveAssist(inner).map(b => !b) // TODO verify this applies to both
      case Complement(inner) => algBoolEval(inner).map(b => !b) // TODO verify this applies to both
      case TRUE => (true, true)
      case FALSE => (false, false)
    }

    recursiveAssist(tree)
  }
}
