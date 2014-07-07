
tru  = lam <A> (t :: A): lam <B> (f :: B): t;;
fals = lam <A> (t :: A): lam <B> (f :: B): f;;
test = lam <A> (cond :: A -> A -> A): lam(consq :: A): lam(altern :: A): cond(consq)(altern);;;

tru(5)
tru<Number>(5)<Number>(6)
fals<Number>(5)<Number>(6)
test(tru)(5)(6)
