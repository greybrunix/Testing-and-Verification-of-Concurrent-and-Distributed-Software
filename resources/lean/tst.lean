theorem and_comm (p q : Prop) : forall p q . p and q -> q and p :=

  fun hpq : p and q =>
  have hp : p := And.left hpq
  have hq : q := And.right hpq
  show q and p from And.intro hq hp
end
