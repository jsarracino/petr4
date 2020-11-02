Require Import Coq.Strings.String.
Require Import Coq.FSets.FMapList.
Require Import Coq.Structures.OrderedTypeEx.
Require Import Coq.Bool.Bvector.
Require Import Coq.Numbers.BinNums.
Require Import Coq.ZArith.BinIntDef.
Require Coq.Strings.String.

Require Import Monads.Monad.
Require Import Monads.Option.

Require Import Utils.

Module Import MStr := FMapList.Make(String_as_OT).

Open Scope monad.

Inductive lvalue :=
  | LValName (var: string)
  | LValMember (base: lvalue) (member: string)
.

Inductive extern :=
  | Packet (bits: list bool)
.

Inductive value :=
| ValVoid
| ValBool (b: bool)
| ValFixedBit (width: nat) (value: Bvector width)
| ValVarBit (width: nat) (value: Bvector width)
| ValFixedInt (width: nat) (n: Z)
| ValInfInt (n: Z)
| ValString (s: string)
| ValArray (arr: list value)
| ValError (msg: string)
(* I would rather this was MStr.t value but that is not a strictly
positive definition. The difference is that [Raw.t value] is
basically list (string * value) while MStr.t value is a dependent
record { raw: MStr.Raw.t; sorted: Sorted ...} which includes a proof
that the list [raw] is sorted. *)
| ValRecord (fs: MStr.Raw.t value)
| ValBuiltinFunc (name: string) (obj: lvalue)
| ValExternFunc (name: string) (obj: lvalue)
| ValExternObj (ext: extern)
| ValHeader (value: header)
| ValHeaderStack (size: nat) (nextIndex: nat) (elements: list header)

(* unused value types from the OCAML implementation

  | VStruct of
      { fields : (string * value) list; }
  | VUnion of
      { fields : (string * value) list; }
  | VEnumField of
      { typ_name : string;
        enum_name : string; }
  | VSenumField of
      { typ_name : string;
        enum_name : string;
        v : value; }
  | VSenum of (string * value) list *)

with header := MkHeader (valid: bool) (fields: MStr.Raw.t value).
Open Scope nat_scope.
(* TODO: wrap this in a module and call it eqb, and prove that l `eqb` r => l = r *)
Fixpoint eq_value (l: value) (r: value) : bool :=
  match (l, r) with
  | (ValVoid, ValVoid) => true
  | (ValBool b_l, ValBool b_r) => eqb b_l b_r
  | (ValFixedBit w_l v_l, ValFixedBit w_r v_r) => (Nat.eqb w_l w_r) && (BVeq _ _ v_l v_r)
  | (ValVarBit w_l v_l, ValVarBit w_r v_r) => (Nat.eqb w_l w_r) && (BVeq _ _ v_l v_r)
  | (ValFixedInt w_l v_l, ValFixedInt w_r v_r) => (Nat.eqb w_l w_r) && (Z.eqb v_l v_r)
  | (ValInfInt v_l, ValInfInt v_r) => Z.eqb v_l v_r
  | (ValString v_l, ValString v_r) => String.eqb v_l v_r
  | _ => false (* TODO: arrays, errors, records, funcs, headers, headerstacks*)
  end.

Definition update_member (obj: value) (member: string) (val: value) : option value :=
  match obj with
  | ValRecord map =>
    let* map' := assoc_update member val map in
    mret (ValRecord map')
  | _ => None
  end.
