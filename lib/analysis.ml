(*
What we want is a data dependency graph for actions in a program.

for now we assume
1. actions don't call each other
2. nothing happens in the apply block, it all happens in actions
3. no tables
4. just one control block

control C(H h, Z z, W w) {
   action a() {
     h.f.a = 0;
   }
   action b() {
     h.f.b = 1;
   }
   action b c ...
   apply { a(h); c(z); b(w); }
}

1. calculate for each action:
   a) things read from
   b) things written to
2. calculate program order in apply block

If there's an edge
 a1 --> a2
keep the edge if there's a data dependency, drop it otherwise.
Remaining edges are the dependency graph we want(?)

If a1 has reads R1, writes W1, resp a2 has R2 W2, there is a data
dependency if any of the following are true
1. v in R2 and v in W1
2. v in R1 and v in W2
3. v in W1 and v in W2
*)

open Prog

let stmt_writes (s: Statement.t) : Expression.t list =
  match (snd s).stmt with
  | Assignment {lhs; rhs} ->
     (* x.f[10] = 5; *)
     [lhs]
  | _ -> failwith "unimplemented"

let block_writes (b: Block.t) : Expression.t list =
  List.concat (List.map stmt_writes (snd b).statements)

let expr_reads (e: Expression.t) : Expression.t list =
  match (snd e).expr with
  | Name n -> [e]
  | _ -> failwith "unimplemented"

let stmt_reads (s: Statement.t) : Expression.t list =
  match (snd s).stmt with
  | Assignment {lhs; rhs} ->
     expr_reads rhs
  | _ -> failwith "unimplemented"

let block_reads (b: Block.t) : Expression.t list =
  List.concat (List.map stmt_reads (snd b).statements)
