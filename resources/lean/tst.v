
def add : nat -> nat -> nat
| m nat.zero := m
| m (nat.succ n) := nat.succ (add m n)
def even (n : ℕ) : Prop := exists m, n = 2 * m
def mul_add : nat -> nat -> nat
| m n := m * n
#eval 0 * 1
theorem even_add : ∀ m n, even m → even n → even (n + m) :=
begin
  intros m n hm hn,
  cases hm with k hk,
  cases hn with l hl,
  unfold even,
  existsi (k + l),
  simp [hk, hl, *]
end
