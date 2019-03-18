object Util {
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
}
