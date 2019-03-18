sealed trait Player {
  type Other <: Player
}

sealed trait Attacker extends Player {
  override type Other = Defender
}

sealed trait Defender extends Player {
  override type Other = Attacker
}