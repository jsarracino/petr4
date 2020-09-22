Require Import String.

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
Inductive name :=
  | BareName (name: string)
  | QualifiedName (path: list string) (name: string)
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