signature Validation =
sig

  val valid : Proof.proof -> bool

  exception ValidationError of string

end
