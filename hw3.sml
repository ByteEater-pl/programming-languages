datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

fun typecheck_patterns (env, ps) =
    case ps of
        [] => SOME Anything
      | h :: t => let
                val rec unify = fn
                    ((Anything, x) | (x, Anything)) => SOME x
                  | (TupleT l1, TupleT l2) =>
                        SOME (TupleT (map
                            (valOf o unify)
                            (ListPair.zipEq (l1, l2))))
                  | (u, v) => if u = v then SOME u else NONE
                val rec ty = fn
                    Wildcard => Anything
                  | Variable _ => Anything
                  | UnitP => UnitT
                  | ConstP _ => IntT
                  | TupleP l => TupleT (map ty l)
                  | ConstructorP (s, n) =>
                        let val SOME (_, d, a) = List.find (fn (x, _, _) => x = s) env
                        in if isSome (unify (a, ty n)) then Datatype d else raise Bind end
            in unify (ty h, valOf (typecheck_patterns (env, t))) end
            handle _ => NONE
