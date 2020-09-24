Require Import String.

Class Monad (M : Type -> Type) : Type :=
  { mret : forall A, A -> M A;
    mbind : forall A B, M A -> (A -> M B) -> M B
  }.

(* Adapted from coq-ext-lib *)
(* https://github.com/coq-community/coq-ext-lib/blob/v8.5/theories/Structures/Monad.v*)
Module MonadNotation.

  Delimit Scope monad_scope with monad.

  Notation "c >>= f" := (@mbind _ _ c f) (at level 50, left associativity) : monad_scope.
  Notation "f =<< c" := (@mbind _ _ c f) (at level 51, right associativity) : monad_scope.

  Notation "x <- c1 ;; c2" := (@mbind _ _ c1 (fun x => c2))
    (at level 100, c1 at next level, right associativity) : monad_scope.

  Notation "e1 ;; e2" := (_ <- e1%monad ;; e2%monad)%monad
    (at level 100, right associativity) : monad_scope.

End MonadNotation.

Inductive direction :=
  | In
  | Out
  | InOut
  | Directionless.

Inductive function_kind := 
  | Parser
  | Control
  | Extern
  | Table
  | Action
  | Function
  | Builtin
.

Inductive name :=
  | BareName (nm: string)
  | QualifiedName (path: list string) (nm: string)
.

Inductive unaryoperator :=
  | Not
  | BitNot
  | BitMinus
.

Inductive binaryoperator :=
  | Plus
  | PlusSat
  | Minus
  | MinusSat
  | Mul
  | Div
  | Mod
  | Shl
  | Shr
  | Le
  | Ge
  | Lt
  | Gt
  | Eq
  | NotEq
  | BitAnd
  | BitXor
  | BitOr
  | PlusPlus
  | And
  | Or
.

Inductive type := 
  | Bool
  | String
  | Integer
  | Int (width: nat)
  | Bit (width: nat)
  | VarBit (width: nat)
  | Array (inner: type) (size: nat)
  | Tuple (types: list type)
  | RecordType (fields: list (string * type))
  | SetType (inner: type)
  | Error
  | MatchKind
  | TypeName (name: string)
  | NewType (inner: type)
  | Void
  | Header (fields: list (string * type))
  | HeaderUnion (fields: list (string * type))
  | Struct (fields: list (string * type))
  | Enum (name: string) (members: list string) (inner: option type)
  | SpecializedType (base: type) (args: list type)
  | ExternType (name: string) (type_params: list string) (methods: list (string * function))
  | FunctionType (inner: function) 
  | ActionType (data_params: list param) (control_params: list param)
  | Constructor (type_params: list string) (params: list param) (return_type: type)

with function := MkFunction 
  (type_params: list string)
  (parameters: list param)
  (kind: function_kind)
  (return_type: type)

with param := MkParam
  (dir: direction)
  (typ: type)
  (variable: string)
  (opt_value: option expression)
  
with keyvalue := MkKeyValue
  (key: string)
  (expr: expression)

with argument :=
  | Expression (value: expression)
  | KeyValue (key: string) (value: expression)
  | Missing

with expression := 
  | True
  | False
  | IntExpression (value: nat)
  | StringExpression (value: string)
  | NameExpression (value: name)
  | ArrayAccess (array: expression) (index: expression)
  | BitStringAccess (array: expression) (hi: expression) (lo: expression)
  | List (values: list expression)
  | Record (entries: list keyvalue)
  | UnaryOp (op: unaryoperator) (arg: expression)
  | BinaryOp (op: binaryoperator) (arg: expression)
  | Cast (type: type) (expr: expression)
  | TypeMember (type: name) (name: string)
  | ErrorMember (error: string)
  | ExpressionMember (expr: expression) (name: string)
  | Ternary (cond: expression) (true: expression) (false: expression)
  | FunctionCall (function: expression) (type_args: list type) (args: list argument)
  | NamelessInstantiation (type: type) (args: list argument)
  | Mask (expr: expression) (mask: expression)
  | Range (lo: expression) (hi: expression)

with value :=
.

Inductive declaration :=
  | DeclarationConstant (type: type) (name: string) (value: expression)
  | DeclarationVariable (type: type) (name: string) (init: option expression)
  | Instantiation (type: type) (args: list expression) (name: string)
.

Inductive statement := 
  | MethodCall (func: expression) (type_args: list type) (args: list (option expression))
  | Assignment (lhs: expression) (rhs: expression)
  | BlockStatement (block: list statement)
  (* same as the corresponding cases of declaration *)
  | StatementConstant (type: type) (name: string) (value: expression)
  | StatementVariable (type: type) (name: string) (init: option expression)
.

Inductive match_expression := 
  | DontCare
  | MatchExpression (expr: expression)
.

Module Case.
Record case := { 
  matches: list match_expression;
  next: string 
}.
End Case.

Module Transition.
Record transition := { 
  exprs: list expression;
  cases: list Case.case 
}.
End Transition.

Module State.
Record state := { 
  name: string;
  statements: list statement;
  transition: Transition.transition
}.
End State.


Module Parser.
Record parser := MkParser { 
  parser_name: string;
  type_params: list string;
  params: list param;
  constructor_params: list param;
  locals: list declaration;
  states: list State.state 
}.
End Parser.

Inductive environment :=.

Definition interp_monad (A: Type) :=
  environment -> option (A * environment).

Definition interp_return (A: Type) (a: A) : interp_monad A :=
  fun env => Some (a, env).

Definition interp_bind (A B: Type) (c: interp_monad A) (f: A -> interp_monad B) : interp_monad B :=
  fun env =>
    match c env with
    | Some (a, env') => f a env'
    | None => None
    end.

Instance interp_monad_inst : Monad interp_monad :=
  { mret := interp_return;
    mbind := interp_bind
  }.

Fixpoint evalStatements (statements: list statement) (env: environment) 
  : option (bool * environment) :=
  
  match statements with
  | nil => (true, env)
  | stmt :: rest => 
    match stmt with
    | MethodCall func type_args args =>
      match func with
      | 
      end
    
    | Assignment lhs rhs =>
      let (lvalue, envl) = evalLValue lhs env
          (rvalue, envr) = evalRvalue rhs envl
      in match updateEnvironment envr lvalue rvalue with
      | None => None
      | Some envfinal => Some (true, envfinal)
      end
          
    | BlockStatement block =>
      match evalStatements block (push env) with
      | None => None
      | Some (reject, envnew) => Some (reject, pop envnew)
      end
      
    | StatementConstant type name value =>
      match insertEnvironment env name value with
      | None => None
      | Some envnew => Some (true, envnew)
      end
      
    | StatementVariable type name init =>
      let opt_val_envnew = match init with
                           | None => Some (defaultValue type, env)
                           | Some expr => evalRvalue expr env
                           end
      in match opt_val_envnew with
         | None => None
         | Some (value, envnew) =>
           match insertEnvironment env name value with
           | None => None
           | Some envfinal => Some (true, envfinal)
           end
         end
    end 
  end
.







