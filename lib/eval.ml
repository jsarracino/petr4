module I = Info
open Core
open Env
open Types
open Value
open P4core
module Info = I (* JNF: ugly hack *)

type signal = packet pre_signal
type vruntime = packet pre_vruntime
type set = packet pre_set

(*----------------------------------------------------------------------------*)
(* Declaration Evaluation *)
(*----------------------------------------------------------------------------*)

let rec eval_decl (env : EvalEnv.t) (d : Declaration.t) : EvalEnv.t =
  match snd d with
  | Constant {
      annotations = _;
      typ = t;
      value = v;
      name = (_,n);
    } -> eval_const_decl env t v n
  | Instantiation {
      annotations = _;
      typ = typ;
      args = args;
      name = (_,n);
    } -> eval_instantiation env typ args n
  | Parser {
      annotations = _;
      name = (_,n);
      type_params = _;
      params = _;
      constructor_params = _;
      locals = _;
      states = _;
    } -> eval_parser_decl env n d
  | Control {
      annotations = _;
      name = (_,n);
      type_params = _;
      params = _;
      constructor_params = _;
      locals = _;
      apply = _;
    } -> eval_control_decl env n d
  | Function {
      return = _;
      name = (_,n);
      type_params = _;
      params = ps;
      body = b;
    } -> eval_fun_decl env n ps b
  | ExternFunction {
      annotations = _;
      return = _;
      name = (_,n);
      type_params = _;
      params = ps;
    } -> eval_extern_fun env n ps
  | Variable {
      annotations = _;
      typ = t;
      name = (_,n);
      init = v;
    } -> eval_var_decl env t n v
  | ValueSet {
      annotations = _;
      typ = _;
      size = _;
      name = _;
    } -> eval_set_decl ()
  | Action {
      annotations = _;
      name = (_,n);
      params = ps;
      body = b;
    } -> eval_action_decl env n ps b
  | Table {
      annotations = _;
      name = _;
      properties = _;
    } -> eval_table_decl ()
  | Header {
      annotations = _;
      name = (_,n);
      fields = _;
    } -> eval_header_decl env n d
  | HeaderUnion {
      annotations = _;
      name = (_,n);
      fields = _;
    } -> eval_union_decl env n d
  | Struct {
      annotations = _;
      name = (_,n);
      fields = _;
    } -> eval_struct_decl env n d
  | Error {
      members = l;
    } -> eval_error_decl env l
  | MatchKind {
      members = l;
    } -> eval_matchkind_decl env l
  | Enum {
      annotations = _;
      name = (_,n);
      members = _;
    } -> eval_enum_decl env n d
  | SerializableEnum {
      annotations = _;
      typ = _;
      name = (_,n);
      members = _;
    } -> eval_senum_decl env n d
  | ExternObject {
      annotations = _;
      name = (_,n);
      type_params = tps;
      methods = ms;
    } -> eval_extern_obj env n ms
  | TypeDef {
      annotations = _;
      name = (_,n);
      typ_or_decl = _;
    } -> eval_type_def env n d
  | NewType {
      annotations = _;
      name = (_,n);
      typ_or_decl = _;
    } -> eval_type_decl env n d
  | ControlType {
      annotations = _;
      name = (_,n);
      type_params = _;
      params = _;
    } -> eval_ctrltyp_decl env n d
  | ParserType {
      annotations = _;
      name = (_,n);
      type_params = _;
      params = _;
    } -> eval_prsrtyp_decl env n d
  | PackageType {
      annotations = _;
      name = (_,n);
      type_params = _;
      params = _;
    } -> eval_pkgtyp_decl env n d

and eval_const_decl (env : EvalEnv.t) (typ : Type.t) (e : Expression.t)
    (name : string) : EvalEnv.t =
  let name_expr = (Info.dummy, Expression.Name(Info.dummy, name)) in
  let env' = EvalEnv.insert_typ name typ env in
  fst (eval_assign env' SContinue name_expr e)

and eval_instantiation (env:EvalEnv.t) (typ : Type.t) (args : Argument.t list)
    (name : string) : EvalEnv.t =
  let (env', obj) = eval_nameless env typ args in
  EvalEnv.insert_val name obj env'

and eval_parser_decl (env : EvalEnv.t) (name : string)
    (decl : Declaration.t) : EvalEnv.t =
  EvalEnv.insert_decl name decl env

and eval_control_decl (env : EvalEnv.t) (name : string)
    (decl : Declaration.t) : EvalEnv.t =
  EvalEnv.insert_decl name decl env

and eval_fun_decl (env : EvalEnv.t) (name : string) (params : Parameter.t list)
    (body : Block.t) : EvalEnv.t =
  EvalEnv.insert_val name (VFun(params,body)) env

and eval_extern_fun (env : EvalEnv.t) (name : string)
    (params : Parameter.t list) : EvalEnv.t =
  EvalEnv.insert_val name (VExternFun params) env

and eval_var_decl (env : EvalEnv.t) (typ : Type.t) (name : string)
    (init : Expression.t option) : EvalEnv.t =
  let env' = EvalEnv.insert_typ name typ env in
  match init with
  | None -> EvalEnv.insert_val name (init_val_of_typ env' name typ) env'
  | Some e ->
    let (env'', v) = eval_expression env' e in
    EvalEnv.insert_val name v env''

and eval_set_decl () = failwith "set decls unimplemented"

and eval_action_decl (env : EvalEnv.t) (name : string) (params : Parameter.t list)
    (body : Block.t) : EvalEnv.t  =
  EvalEnv.insert_val name (VAction(params, body)) env

and eval_table_decl () = failwith "tables unimplemented"

and eval_header_decl (env : EvalEnv.t) (name : string)
    (decl : Declaration.t) : EvalEnv.t =
  EvalEnv.insert_decl name decl env

and eval_union_decl (env : EvalEnv.t) (name : string)
    (decl : Declaration.t) : EvalEnv.t =
  EvalEnv.insert_decl name decl env

and eval_struct_decl (env : EvalEnv.t) (name : string)
    (decl : Declaration.t) : EvalEnv.t =
  EvalEnv.insert_decl name decl env

and eval_error_decl (env : EvalEnv.t) (errs : P4String.t list) : EvalEnv.t =
  let errs' = List.map errs ~f:snd in
  EvalEnv.insert_errs errs' env

and eval_matchkind_decl (env : EvalEnv.t) (mems : P4String.t list) : EvalEnv.t =
  mems
  |> List.map ~f:snd
  |> List.map ~f:(fun a -> (a, VMatchKind))
  |> (fun a -> EvalEnv.insert_vals a env)

and eval_enum_decl (env : EvalEnv.t) (name : string)
    (decl : Declaration.t) : EvalEnv.t =
  EvalEnv.insert_decl name decl env

and eval_senum_decl (env : EvalEnv.t) (name : string)
    (decl : Declaration.t) :EvalEnv.t =
  EvalEnv.insert_decl name decl env

and eval_extern_obj (env : EvalEnv.t) (name : string)
    (methods : MethodPrototype.t list) : EvalEnv.t =
  EvalEnv.insert_val name (VExternObject (name, methods)) env

and eval_type_def (env : EvalEnv.t) (name : string)
    (decl : Declaration.t) : EvalEnv.t =
  EvalEnv.insert_decl name decl env

and eval_type_decl (env : EvalEnv.t) (name : string)
    (decl : Declaration.t) : EvalEnv.t =
  EvalEnv.insert_decl name decl env

and eval_ctrltyp_decl (env : EvalEnv.t) (name : string)
    (decl : Declaration.t) : EvalEnv.t =
  EvalEnv.insert_decl name decl env

and eval_prsrtyp_decl (env : EvalEnv.t) (name : string)
    (decl : Declaration.t) : EvalEnv.t =
  EvalEnv.insert_decl name decl env

and eval_pkgtyp_decl (env : EvalEnv.t) (name : string)
    (decl : Declaration.t) : EvalEnv.t =
  EvalEnv.insert_decl name decl env

(*----------------------------------------------------------------------------*)
(* Functions to Calculate Initialization Values *)
(*----------------------------------------------------------------------------*)

and init_val_of_typ (env : EvalEnv.t) (name : string) (typ : Type.t) : value =
  match snd typ with
  | Bool                      -> VBool false
  | Error                     -> VError "NoError"
  | IntType expr              -> init_val_of_int env expr
  | BitType expr              -> init_val_of_bit env expr
  | VarBit expr               -> failwith "varbit init unimplemented"
  | TopLevelType (_,n)        -> init_val_of_typname env n name true
  | TypeName (_,n)            -> init_val_of_typname env n name false
  | SpecializedType _         -> failwith "specialized init unimplemented"
  | HeaderStack{header; size} -> init_val_of_stack env name header size
  | Tuple _                   -> failwith "tuple init unimplemented"
  | Void                      -> VNull
  | DontCare                  -> failwith "no init value for dont care types"

and init_val_of_int (env : EvalEnv.t) (expr : Expression.t) : value =
  match snd (eval_expression env expr) with
  | VInteger n -> VInt(Bigint.to_int_exn n, Bigint.zero)
  | _ -> failwith "int width is not an int"

and init_val_of_bit (env : EvalEnv.t) (expr : Expression.t) : value =
  match snd (eval_expression env expr) with
  | VInteger n -> VBit(Bigint.to_int_exn n, Bigint.zero)
  | _ -> failwith "bit width is not an int"

and init_val_of_typname (env : EvalEnv.t) (tname : string) (vname : string) (b : bool) : value =
  let f = EvalEnv.(if b then find_decl_toplevel else find_decl) in
  match snd (f tname env) with
  | Struct {fields=fs;_}      -> init_val_of_struct env vname fs
  | Header {fields=fs;_}      -> init_val_of_header env vname fs
  | HeaderUnion {fields=fs;_} -> init_val_of_union env vname fs
  | _ -> failwith "decl init value unimplemented"

and init_val_of_stack (env: EvalEnv.t) (name : string)
    (hdr : Type.t) (size : Expression.t) : value =
  let size' = size |> eval_expression env |> snd |> int_of_val in
  let hdrs = size' |> List.init ~f:string_of_int
             |> List.map ~f:(fun s -> init_val_of_typ env s hdr) in
  VStack(name, hdrs, size', 0)

and init_val_of_struct (env : EvalEnv.t) (name : string)
    (fs : Declaration.field list) : value =
  VStruct (name, List.map fs ~f:(init_binding_of_field env))

and init_val_of_header (env : EvalEnv.t) (name : string)
    (fs : Declaration.field list) : value =
  VHeader (name, List.map fs ~f:(init_binding_of_field env), false)

and init_val_of_union (env : EvalEnv.t) (name : string)
    (fs : Declaration.field list) : value =
  let fs' = List.map fs ~f:(init_binding_of_field env) in
  let bs = List.map fs' ~f:(fun (a,b) -> (a,false)) in
  let v = fs' |> List.hd_exn |> snd in
  VUnion (name, v, bs)

(*----------------------------------------------------------------------------*)
(* Statement Evaluation *)
(*----------------------------------------------------------------------------*)

and eval_statement (env :EvalEnv.t) (sign : signal)
    (stm : Statement.t) : (EvalEnv.t * signal) =
  match snd stm with
  | MethodCall{func;type_args;args} -> eval_method_call env sign func args
  | Assignment{lhs;rhs}             -> eval_assign env sign lhs rhs
  | DirectApplication{typ;args}     -> eval_app ()
  | Conditional{cond;tru;fls}       -> eval_cond env sign cond tru fls
  | BlockStatement{block}           -> eval_block env sign block
  | Exit                            -> eval_exit ()
  | EmptyStatement                  -> (env, sign)
  | Return{expr}                    -> eval_return env sign expr
  | Switch{expr;cases}              -> eval_switch ()
  | DeclarationStatement{decl}      -> eval_decl_stm env sign decl

and eval_method_call (env : EvalEnv.t) (sign : signal) (func : Expression.t)
    (args : Argument.t list) : EvalEnv.t * signal =
  match sign with
  | SContinue -> (fst (eval_funcall env func args), sign)
  | SReject
  | SReturn _ -> (env, sign)
  | SExit     -> failwith "exit unimplemented"

and eval_assign (env : EvalEnv.t) (s : signal) (lhs : Expression.t)
    (rhs : Expression.t) : EvalEnv.t * signal =
  let (env', v) = eval_expression env rhs in
  let lv = lvalue_of_expr lhs in
  match s with
  | SContinue -> eval_assign' env' lv v
  | SReject
  | SReturn _ -> (env, s)
  | SExit     -> failwith "exit unimplemented"

and eval_app () = failwith "direct application unimplemented"

and eval_cond (env : EvalEnv.t) (sign : signal) (cond : Expression.t)
    (tru : Statement.t) (fls : Statement.t option) : EvalEnv.t * signal =
  let eval_cond' env cond tru fls =
    let (env', v) = eval_expression env cond in
    match v with
    | VBool true  -> eval_statement env' SContinue tru
    | VBool false ->
      begin match fls with
        | None -> (env, SContinue)
        | Some fls' -> eval_statement env' SContinue fls'  end
    | _ -> failwith "conditional guard must be a bool" in
  match sign with
  | SContinue -> eval_cond' env cond tru fls
  | SReject
  | SReturn _ -> (env, sign)
  | SExit     -> failwith "exit unimplmented"

and eval_block (env : EvalEnv.t) (sign :signal) (block : Block.t) : (EvalEnv.t * signal) =
  let block = snd block in
  let f (env,sign) stm =
    match sign with
    | SContinue -> eval_statement env sign stm
    | SReject
    | SReturn _ -> (env, sign)
    | SExit     -> failwith "exit unimplemented" in
  List.fold_left block.statements ~init:(env,sign) ~f:f

and eval_exit () = failwith "exit unimplemented"

and eval_return (env : EvalEnv.t) (sign : signal)
    (expr : Expression.t option) : (EvalEnv.t * signal) =
  let (env',v) =
    match expr with
    | None   -> (env, VNull)
    | Some e -> eval_expression env e in
  match sign with
  | SContinue -> (env', SReturn v)
  | SReject
  | SReturn _ -> (env, sign)
  | SExit     -> failwith "exit unimplemented"

and eval_switch () = failwith "switch stm unimplemented"

and eval_decl_stm (env : EvalEnv.t) (sign : signal)
    (decl : Declaration.t) : EvalEnv.t * signal =
  match sign with
  | SContinue -> (eval_decl env decl, SContinue)
  | SReject
  | SReturn _ -> (env, sign)
  | SExit     -> failwith "exit unimplemented"

(*----------------------------------------------------------------------------*)
(* Asssignment Evaluation *)
(*----------------------------------------------------------------------------*)

and eval_assign' (env : EvalEnv.t)(lhs : lvalue)
    (rhs : value) : EvalEnv.t * signal =
  match lhs with
  | LName n            -> (assign_name env n rhs, SContinue)
  | LTopName n         -> (assign_toplevel env n rhs, SContinue)
  | LMember(lv,mname)  -> assign_member env lv mname rhs
  | LBitAccess _       -> (assign_bitaccess (), SContinue)
  | LArrayAccess(lv,e) -> assign_arrayaccess env lv e rhs

and assign_name (env : EvalEnv.t) (name : string) (rhs : value) : EvalEnv.t =
  let t = EvalEnv.find_typ name env in
  match rhs with
    | VTuple l ->
      let f = match snd (decl_of_typ env t) with
        | Declaration.Struct _ -> struct_of_list env name
        | Declaration.Header _ -> header_of_list env name
        | _ -> (fun l -> VTuple l) in
      EvalEnv.insert_val name (f l) env
    | VStruct(n,l)    -> EvalEnv.insert_val name (VStruct(name,l)) env
    | VHeader(n,l,b)  -> EvalEnv.insert_val name (VHeader(name,l,b)) env
    | VUnion(n,v,l)   -> EvalEnv.insert_val name (VUnion(name,v,l)) env
    | VStack(n,v,a,i) -> EvalEnv.insert_val name (VStack(name,v,a,i)) env
    | _ -> EvalEnv.insert_val name rhs env

and assign_toplevel (env : EvalEnv.t) (name : string) (rhs : value) : EvalEnv.t =
  failwith "toplevel assignment unimplemented"

and assign_member (env : EvalEnv.t) (lv : lvalue) (mname : string)
    (rhs : value) : EvalEnv.t * signal =
  let v = value_of_lvalue env lv in
  match v with
  | VStruct(n,l)       -> (assign_struct_mem env lv rhs mname n l, SContinue)
  | VHeader(n,l,b)     -> (assign_header_mem env lv rhs mname n l b, SContinue)
  | VUnion(n,vs,bs)    -> (assign_union_mem env lv rhs mname n bs, SContinue)
  | VStack(n,hdrs,s,i) -> assign_stack_mem env lv rhs n mname hdrs s i
  | _ -> failwith "member assignment unimplemented"

and assign_bitaccess () =
  failwith "bitstring access assignment unimplemented"

and assign_arrayaccess (env : EvalEnv.t) (lv : lvalue) (e : Expression.t)
    (rhs : value) : EvalEnv.t * signal =
  let v = value_of_lvalue env lv in
  let (env', i) = eval_expression env e in
  let i' = int_of_val i in
  let rhs' = match v with
    | VStack(n,hdrs,size,next) ->
      let (hdrs1, hdrs2) = List.split_n hdrs i' in
      let hdrs' = match hdrs2 with
        | _ :: t -> hdrs1 @ (rhs :: t)
        | [] -> failwith "empty header stack" in
      VStack(n,hdrs',size,next)
    | _ -> failwith "array access is not a header stack" in
  eval_assign' env lv rhs'

and assign_struct_mem (env : EvalEnv.t) (lhs : lvalue) (rhs : value)
    (fname : string) (sname : string) (l : (string * value) list) : EvalEnv.t =
  EvalEnv.insert_val sname (VStruct(sname, (fname, rhs) :: l)) env

and assign_header_mem (env : EvalEnv.t) (lhs : lvalue) (rhs : value)
    (fname : string) (hname : string) (l : (string * value) list)
    (b : bool) : EvalEnv.t =
  EvalEnv.insert_val hname (VHeader(hname,(fname,rhs) :: l,b)) env

and assign_union_mem (env : EvalEnv.t) (lhs : lvalue) (rhs : value)
    (fname : string) (uname : string) (vbs : (string * bool) list) : EvalEnv.t =
  let t = typ_of_union_field env uname fname in
  let dummy_env = EvalEnv.insert_typ fname t env in
  let rhs' = match rhs with
    | VTuple l -> header_of_list dummy_env fname l
    | x -> x in (* TODO: this should not scale to nested structs *)
  let vbs' = List.map vbs ~f:(fun (s,_) -> (s, s=fname)) in
  EvalEnv.insert_val uname (VUnion(uname, rhs', vbs')) env

and assign_stack_mem (env : EvalEnv.t) (lhs : lvalue) (rhs : value)
    (sname : string) (mname : string) (hdrs : value list) (size : int)
    (next : int) : EvalEnv.t * signal =
  let () =
    match mname with
    | "next" -> ()
    | _ -> failwith "stack mem not an lvalue" in
  if next >= size
  then (env, SReject)
  else
    let t = typ_of_stack_mem env sname in
    let dummy_env = EvalEnv.insert_typ mname t env in
    let rhs' =
      match rhs with
      | VTuple l -> header_of_list dummy_env mname l
      | x -> x in
    let (hdrs1, hdrs2) = List.split_n hdrs next in
    let hdrs' =
      match hdrs2 with
      | _ :: t -> hdrs1 @ (rhs' :: t)
      | [] -> failwith "header stack is empty" in
    (EvalEnv.insert_val sname (VStack(sname,hdrs',size,next)) env, SContinue)

(*----------------------------------------------------------------------------*)
(* Functions on L-Values*)
(*----------------------------------------------------------------------------*)

and lvalue_of_expr (expr : Expression.t) =
  match snd expr with
  | Name(_,n) -> LName n
  | TopLevel(_,n) -> LTopName n
  | ExpressionMember{expr=e; name=(_,n)} -> LMember(lvalue_of_expr e, n)
  | BitStringAccess{bits;lo;hi} -> LBitAccess(lvalue_of_expr bits, lo, hi)
  | ArrayAccess{array;index} -> LArrayAccess(lvalue_of_expr array, index)
  | _ -> failwith "not an lvalue"

and value_of_lvalue (env : EvalEnv.t) (lv : lvalue) : value =
  match lv with
  | LName n                -> EvalEnv.find_val n env
  | LTopName n             -> EvalEnv.find_val_toplevel n env
  | LMember(lv, n)         -> value_of_lmember env lv n
  | LBitAccess(lv, hi, lo) -> value_of_lbit env lv hi lo
  | LArrayAccess(lv, idx)  -> value_of_larray env lv idx

and value_of_lmember (env : EvalEnv.t) (lv : lvalue) (n : string) : value =
  match value_of_lvalue env lv with
  | VStruct(_,l)
  | VHeader(_,l, _)  -> List.Assoc.find_exn l n ~equal:(=)
  | VUnion(_,v,_)    -> v
  | VStack(_,vs,s,i) -> value_of_stack_mem_lvalue n vs s i
  | _ -> failwith "no lvalue member"

and value_of_lbit (env : EvalEnv.t) (lv : lvalue) (hi : Expression.t)
    (lo : Expression.t) : value =
  failwith "value of bitstring l value unimplemented"

and value_of_larray (env : EvalEnv.t) (lv : lvalue)
    (idx : Expression.t) : value =
  match value_of_lvalue env lv with
  | VStack(n,vs,s,i) -> List.nth_exn vs (i mod s)
  | _ -> failwith "array access is not a header stack "

and value_of_stack_mem_lvalue (name : string) (vs : value list) (size : int)
    (next : int) : value =
  match name with
  | "next" -> List.nth_exn vs (next mod size)
  | _ -> failwith "not an lvalue"

(*----------------------------------------------------------------------------*)
(* Expression Evaluation *)
(*----------------------------------------------------------------------------*)

and eval_expression (env : EvalEnv.t) (exp : Expression.t) : EvalEnv.t * value =
  match snd exp with
  | True                              -> (env, VBool true)
  | False                             -> (env, VBool false)
  | Int(_,n)                          -> (env, eval_p4int n)
  | String (_,value)                  -> (env, VString value)
  | Name (_,name)                     -> (env, EvalEnv.find_val name env)
  | TopLevel (_,name)                 -> (env, EvalEnv.find_val_toplevel name env)
  | ArrayAccess({array=a; index=i})   -> eval_array_access env a i
  | BitStringAccess({bits;lo;hi})     -> eval_bitstring_access env bits lo hi
  | List{values}                      -> eval_list env values
  | UnaryOp{op;arg}                   -> eval_unary env op arg
  | BinaryOp{op; args=(l,r)}          -> eval_binop env op l r
  | Cast{typ;expr}                    -> eval_cast env typ expr
  | TypeMember{typ;name}              -> eval_typ_mem env typ (snd name)
  | ErrorMember t                     -> (env, EvalEnv.find_err (snd t) env)
  | ExpressionMember{expr;name}       -> eval_expr_mem env expr name
  | Ternary{cond;tru;fls}             -> eval_ternary env cond tru fls
  | FunctionCall{func;type_args;args} -> eval_funcall env func args
  | NamelessInstantiation{typ;args}   -> eval_nameless env typ args
  | Mask{expr;mask}                   -> eval_mask env expr mask
  | Range{lo;hi}                      -> eval_range env lo hi

and eval_p4int (n : P4Int.pre_t) : value =
  match n.width_signed with
  | None          -> VInteger n.value
  | Some(w,true)  -> VInt (w, n.value)
  | Some(w,false) -> VBit (w, n.value)

and eval_array_access (env : EvalEnv.t) (a : Expression.t)
    (i : Expression.t) : EvalEnv.t * value =
  let (env', a') = eval_expression env a in
  let (env'', i') = eval_expression env' i in
  let idx = int_of_val i' in
  match a' with
  | VStack(name,hdrs,size,next) ->
    let idx' = idx mod size in
    (env'', List.nth_exn hdrs idx')
  | _ -> failwith "array access must be on header stack"

and eval_bitstring_access (env : EvalEnv.t) (s : Expression.t)
    (m : Expression.t) (l : Expression.t) : EvalEnv.t * value =
  let (env', m) = eval_expression env m in
  let (env'', l) = eval_expression env' l in
  let (env''', s) = eval_expression env'' s in
  let m' = int_of_val m in
  let l' = int_of_val l in
  match m, l, s with
  | VBit(_, vm), VBit(_, vl), VBit(w1, v1) ->
    let v = (Bigint.shift_left v1 (l'+1) |>
             Bigint.shift_right) (w1-m'+l') in
    (env''', VBit(m'-l'+1, v))
  | _ -> failwith "bit string access unimplemented"

and eval_list (env : EvalEnv.t) (values : Expression.t list) : EvalEnv.t * value =
  values
  |> List.fold_map ~f:eval_expression ~init:env
  |> (fun (e,l) -> (e, VTuple l))

and eval_unary (env : EvalEnv.t) (op : Op.uni) (e : Expression.t) : EvalEnv.t * value =
  let (env', e') = eval_expression env e in
  match snd op, e' with
  | UMinus, VBit(w,v) -> Bigint.(env', VBit(w, (power_of_two w) - v))
  | UMinus, VInt(w,v) -> Bigint.(env', VBit(w, -v))
  | BitNot, VBit(w,v) -> Bigint.(env', VBit(w, -v))
  | BitNot, VInt(w,v) -> Bigint.(env', VInt(w, -v))
  | Not, VBool b      -> (env', VBool (not b))
  | _ -> failwith "unary op unimplemented"

and eval_binop (env : EvalEnv.t) (op : Op.bin) (l : Expression.t)
    (r : Expression.t) : EvalEnv.t * value =
  let (env',l) = eval_expression env l in
  let (env'',r) = eval_expression env' r in
  let f = begin match snd op with
    | Plus     -> eval_two Bigint.( + )
    | PlusSat  -> eval_sat Bigint.( + )
    | Minus    -> eval_two Bigint.( - )
    | MinusSat -> eval_sat Bigint.( - )
    | Mul      -> eval_two Bigint.( * )
    | Div      -> eval_two Bigint.( / )
    | Mod      -> eval_two Bigint.rem
    | Shl      -> eval_shift Bigint.shift_left
    | Shr      -> eval_shift Bigint.shift_right
    | Le       -> eval_compare (<=)
    | Ge       -> eval_compare (>=)
    | Lt       -> eval_compare (<)
    | Gt       -> eval_compare (>)
    | Eq       -> eval_eq true
    | NotEq    -> eval_eq false
    | BitAnd   -> eval_two Bigint.bit_and
    | BitXor   -> eval_two Bigint.bit_xor
    | BitOr    -> eval_two Bigint.bit_or
    | PlusPlus -> eval_concat
    | And      -> eval_and_or (&&)
    | Or       -> eval_and_or (||) end in
  (env'', f l r)

and eval_cast (env : EvalEnv.t) (typ : Type.t)
    (expr : Expression.t) : EvalEnv.t * value =
  let build_bit w v =
    VBit (int_of_val w, Bigint.of_int v) in
  let changesize w v l =
    let new_w = l |> int_of_val in
    let value = if new_w >= w then v
      else (Bigint.shift_left v (w - new_w) |>
            Bigint.shift_right) (w - new_w) in
    (new_w, value) in
  let (env', expr') = eval_expression env expr in
  match expr', snd typ with
  | VBit(1, v), Type.Bool ->
    if Bigint.(=) v Bigint.zero
    then (env', VBool(false))
    else if Bigint.(=) v Bigint.one
    then (env', VBool(true))
    else failwith "can't cast this bitstring to bool"
  | VBool(b), Type.BitType(e) ->
    let (env'', e') = eval_expression env' e in
    if b then (env'', build_bit e' 1)
    else (env'', build_bit e' 0)
  | VInt(w, v), Type.BitType(w') ->
    let turn_pos w v =
      if Bigint.(<) v Bigint.zero
      then Bigint.(+) v (power_of_two (w+1))
      else v in
    (env', VBit(w, turn_pos w v))
  | VBit(w, v), Type.IntType(w') ->
    let neg_bit w v =
      if Bigint.(>=) v (power_of_two (w-1))
      then Bigint.(-) v (power_of_two w)
      else v in
    (env', VInt(w, neg_bit w v))
  | VBit(w, v), Type.BitType(l) ->
    let (env'', l') = eval_expression env' l in
    let width, value = changesize w v l' in
    (env'', VBit(width, value))
  (* TODO: validate: Should be shift_right_truncate*)
  | VInt(w, v), Type.IntType(l) ->
    let (env'', l') = eval_expression env l in
    let (width, value) = changesize w v l' in
    (env'', VInt(width, value))
  | _ -> failwith "type cast unimplemented" (* TODO *)

and eval_typ_mem (env : EvalEnv.t) (typ : Type.t)
    (name : string) : EvalEnv.t * value =
  match snd (decl_of_typ env typ) with
  | Declaration.Enum {members=ms;name=(_,n);_} ->
    let mems = List.map ms ~f:snd in
    if List.mem mems name ~equal:(=)
    then (env, VEnumField (n, name))
    else raise (UnboundName name)
  | Declaration.SerializableEnum {members=ms;name=(_,n);_ } ->
    let f = fun e (a,b) ->
      let (e', v) = eval_expression e b in
      (env, (snd a, v)) in
    let (env', vs) = List.fold_map ms ~init:env ~f:f in
    let v = List.Assoc.find_exn vs name ~equal:(=) in
    (env, VSenumField(n,name,v))
  | _ -> failwith "typ mem unimplemented"

and eval_expr_mem (env : EvalEnv.t) (expr : Expression.t)
    (name : P4String.t) : EvalEnv.t * value =
  let (env', v) = eval_expression env expr in
  match v with
  | VNull
  | VBool _
  | VInteger _
  | VBit _
  | VInt _
  | VTuple _
  | VSet _
  | VString _
  | VError _
  | VMatchKind
  | VFun _
  | VBuiltinFun _
  | VAction _           -> failwith "expr member does not exist"
  | VStruct (_,fs)      -> eval_struct_mem env' (snd name) fs
  | VHeader (_,fs,vbit) -> eval_header_mem env' (snd name) expr fs vbit
  | VUnion (_,v,_)      -> (env', v)
  | VStack (_,hdrs,s,n) -> eval_stack_mem env' (snd name) expr hdrs s n
  | VRuntime v          -> eval_runtime_mem env' (snd name) expr v
  | VEnumField _
  | VSenumField _
  | VExternFun _
  | VExternObject _
  | VObjstate _         -> failwith "expr member unimplemented"

and eval_ternary (env : EvalEnv.t) (c : Expression.t) (te : Expression.t)
    (fe : Expression.t) : EvalEnv.t * value =
  let (env', c') = eval_expression env c in
  match c' with
  | VBool(true)  -> (eval_expression env' te)
  | VBool(false) -> (eval_expression env' fe)
  | _ -> failwith "ternary guard must be a bool"

and eval_funcall (env : EvalEnv.t) (func : Expression.t)
    (args : Argument.t list) : EvalEnv.t * value =
  let (env', cl) = eval_expression env func in
  match cl with
  | VAction (params, body)
  | VFun (params, body)    -> eval_funcall' env' params args body
  | VBuiltinFun(n,lv)      -> eval_builtin env n lv args
  | VNull
  | VBool _
  | VInteger _
  | VBit _
  | VInt _
  | VTuple _
  | VSet _
  | VString _
  | VError _
  | VMatchKind
  | VStruct _
  | VHeader _
  | VUnion _
  | VStack _
  | VEnumField _
  | VSenumField _
  | VExternFun _
  | VExternObject _
  | VRuntime _
  | VObjstate _            -> failwith "unreachable"

and eval_nameless (env : EvalEnv.t) (typ : Type.t)
    (args : Argument.t list) : EvalEnv.t * value =
  let (info ,decl) = decl_of_typ env typ in
  match decl with
  | Control typ_decl ->
    let (env',state) = eval_inargs env typ_decl.constructor_params args in
    let state' = state |> EvalEnv.get_val_firstlevel |> List.rev in
    (env', VObjstate((info, decl), state'))
  | Parser typ_decl ->
    let (env',state) = eval_inargs env typ_decl.constructor_params args in
    let state' = state |> EvalEnv.get_val_firstlevel |> List.rev in
    (env', VObjstate((info, decl), state'))
  | PackageType pack_decl ->
    let (env', state) = eval_inargs env pack_decl.params args in
    let state' = state |> EvalEnv.get_val_firstlevel |> List.rev in
    (env', VObjstate((info, decl), state'))
  | _ -> failwith "instantiation unimplemented"

and eval_mask env e m =
  let (env', v1)  = eval_expression env  e in
  let (env'', v2) = eval_expression env' m in
  (env'', VSet(SMask(v1,v2)))

and eval_range env lo hi =
  let (env', v1)  = eval_expression env  lo in
  let (env'', v2) = eval_expression env' hi in
  (env'', VSet(SRange(v1,v2)))

(*----------------------------------------------------------------------------*)
(* Binary Operator Evaluation *)
(*----------------------------------------------------------------------------*)

and eval_two op l r =
  match l,r with
  | VBit(w1, v1), VBit(_, v2) -> VBit(w1, op v1 v2)
  | VInt(w1, v1), VInt(_, v2) -> VInt(w1, op v1 v2)
  | _ -> failwith "binary logical operation only works on bitstrings"

and eval_sat op l r =
  let compute m n w =
    let a = Bigint.abs (op m n) in
    if  Bigint.(<) a m || Bigint.(<) a n then power_of_two w else a in
  match l,r with
  | VBit(w1,v1), VBit(w2, v2) -> VBit(w1, compute v1 v2 w1)
  | VInt(w1,v1), VInt(w2, v2) -> VInt(w1, compute v1 v2 (w1-1))
  | _ -> failwith "binary logical operation only works on bitstrings"

and eval_shift op l r =
  match l,r with
  | VBit(w1, v1), VBit(_, v2)
  | VBit(w1, v1), VInt(_, v2) ->
    if v1 >= Bigint.zero
    then VBit(w1, op v1 (Bigint.to_int_exn v2))
    else failwith "can't shift with a negative amount"
  | _ -> failwith "shift doesn't works on this type"

and eval_compare op l r =
  match l,r with
  | VBit(_, v1), VBit(_, v2)
  | VInt(_, v1), VInt(_, v2) -> VBool(op v1 v2)
  | _ -> failwith " binary comparison only works on fixed length integer"

and eval_eq (op : bool) (l : value) (r : value) : value =
  match l,r with
  | VBit(_, v1), VBit(_, v2)
  | VInt(_, v1), VInt(_, v2)
  | VInteger v1, VInteger v2 -> VBool (if op then v1 = v2 else v1 <> v2)
  | VBool v1, VBool v2 -> VBool (if op then v1 = v2 else v1 <> v2)
  | _ -> failwith "equality for varbit binary comparison only works on bitstrings"

and eval_concat l r =
  let concat m n wn =
    Bigint.( + ) (Bigint.shift_left m wn) n in
  match l,r with
  | VBit(w1, v1), VBit(w2, v2) -> VBit(w1+w2, concat v1 v2 w2)
  | VInt(w1, v1), VInt(w2, v2) -> VInt(w1+w2, concat v1 v2 w2)
  | _ -> failwith " binary concatenation only works on fixed length integer"

and eval_and_or op l r =
  match l,r with
  | VBool(bl), VBool(br) -> VBool(op bl br)
  | _ -> failwith "and / or operation only works on Bools"

(*----------------------------------------------------------------------------*)
(* Membership Evaluation *)
(*----------------------------------------------------------------------------*)

and eval_struct_mem (env : EvalEnv.t) (name : string)
    (fs : (string * value) list) : EvalEnv.t * value =
  (env, List.Assoc.find_exn fs name ~equal:(=))

and eval_header_mem (env : EvalEnv.t) (fname : string) (e : Expression.t)
    (fs : (string * value) list) (valid : bool) : EvalEnv.t * value =
  match fname with
  | "isValid"
  | "setValid"
  | "setInvalid" -> (env, VBuiltinFun(fname, lvalue_of_expr e))
  | _            -> (env, List.Assoc.find_exn fs fname ~equal:(=))

and eval_stack_mem (env : EvalEnv.t) (fname : string) (e : Expression.t)
    (hdrs : value list) (size : int) (next : int) : EvalEnv.t * value =
  match fname with
  | "size"       -> eval_stack_size env size
  | "next"       -> eval_stack_next env hdrs size next
  | "last"       -> eval_stack_last env hdrs size next
  | "lastIndex"  -> eval_stack_lastindex env next
  | "pop_front"
  | "push_front" -> eval_stack_builtin env fname e
  | _ -> failwith "stack member unimplemented"

and eval_runtime_mem (env : EvalEnv.t) (mname : string) (expr : Expression.t)
    (v : vruntime) : EvalEnv.t * value =
  match v with
  | Packet p -> eval_packet_mem env mname expr p

and eval_stack_size (env : EvalEnv.t) (size : int) : EvalEnv.t * value =
  (env, VBit(32, Bigint.of_int size))

and eval_stack_next (env : EvalEnv.t) (hdrs : value list) (size : int)
    (next : int) : EvalEnv.t * value =
  let hdr =
    if next >= size
    then failwith "signal reject unimplemented"
    else List.nth_exn hdrs next in
  (env, hdr)

and eval_stack_last (env : EvalEnv.t) (hdrs : value list) (size : int)
    (next : int) : EvalEnv.t * value =
  let hdr =
    if next < 1 || next > size
    then failwith "signal reject unimplemented"
    else List.nth_exn hdrs next in
  (env, hdr)

and eval_stack_lastindex (env : EvalEnv.t) (next : int) : EvalEnv.t * value =
  (env, VBit(32, Bigint.of_int (next - 1)))

and eval_stack_builtin (env : EvalEnv.t) (fname : string)
    (e : Expression.t) : EvalEnv.t * value =
  (env, VBuiltinFun(fname, lvalue_of_expr e))

and eval_packet_mem (env : EvalEnv.t) (mname : string) (expr : Expression.t)
    (p : packet) : EvalEnv.t * value =
  match mname with
  | "extract" -> (env, VBuiltinFun(mname, lvalue_of_expr expr))
  | "emit" -> (env, VBuiltinFun(mname, lvalue_of_expr expr))
  | _ -> failwith "packet member unimplemented"


(*----------------------------------------------------------------------------*)
(* Function and Method Call Evaluation *)
(*----------------------------------------------------------------------------*)

and eval_funcall' (env : EvalEnv.t) (params : Parameter.t list)
  (args : Argument.t list) (body : Block.t) : EvalEnv.t * value =
  let (env', fenv) = eval_inargs env params args in
  let (fenv', sign) = eval_block fenv SContinue body in
  let final_env = eval_outargs env' fenv' params args in
  match sign with
  | SReject -> (env, VNull)
  | SReturn v -> (final_env, v)
  | SContinue -> (final_env, VNull)
  | SExit -> failwith "function did not return"

and eval_inargs (env : EvalEnv.t) (params : Parameter.t list)
      (args : Argument.t list) : EvalEnv.t * EvalEnv.t =
  let f env e =
    match snd e with
    | Argument.Expression {value=expr} -> eval_expression env expr
    | Argument.KeyValue _
    | Argument.Missing -> failwith "missing args unimplemented" in
  let (env',arg_vals) = List.fold_map args ~f:f ~init:env in
  let fenv = EvalEnv.push_scope (EvalEnv.get_toplevel env') in
  let g e (p : Parameter.t) v =
    let name = snd (snd p).variable in
    let v' = match v with
      | VHeader (n,l,b) -> VHeader (name, l, b)
      | VStruct (n,l) -> VStruct (name,l)
      | _ -> v in
    match (snd p).direction with
    | None -> e
      |> EvalEnv.insert_val (snd (snd p).variable) v'
      |> EvalEnv.insert_typ (snd (snd p).variable) (snd p).typ
    | Some x -> begin match snd x with
      | InOut
      | In ->
        e
        |> EvalEnv.insert_val (snd (snd p).variable) v'
        |> EvalEnv.insert_typ (snd (snd p).variable) (snd p).typ
      | Out -> e end in
  let fenv' = List.fold2_exn params arg_vals ~init:fenv ~f:g in
  (env', fenv')

and eval_outargs (env : EvalEnv.t) (fenv : EvalEnv.t)
    (params : Parameter.t list) (args : Argument.t list) : EvalEnv.t =
  let h e (p:Parameter.t) a =
    match (snd p).direction with
    | None -> e
    | Some x -> begin match snd x with
      | InOut
      | Out ->
        let v = EvalEnv.find_val (snd (snd p).variable) fenv in
        let lhs = begin match snd a with
          | Argument.Expression {value=expr} -> expr
          | Argument.KeyValue _
          | Argument.Missing -> failwith "missing args unimplemented" end in
        fst (eval_assign' e (lvalue_of_expr lhs) v) (* shouldnt have function calls in parsers *)
      | In -> e end in
  List.fold2_exn params args ~init:env ~f:h

(*----------------------------------------------------------------------------*)
(* Built-in Function Evaluation *)
(*----------------------------------------------------------------------------*)

and eval_builtin (env : EvalEnv.t) (name : string) (lv : lvalue)
    (args : Argument.t list) : EvalEnv.t * value =
  match name with
  | "isValid"    -> eval_isvalid env lv
  | "setValid"   -> eval_setvalid env lv
  | "setInvalid" -> eval_setinvalid env lv
  | "pop_front"  -> eval_popfront env lv args
  | "push_front" -> eval_pushfront env lv args
  | "extract"    -> eval_extract env lv args
  | "emit"       -> eval_emit env lv args
  | _ -> failwith "builtin unimplemented"

and eval_isvalid (env : EvalEnv.t) (lv : lvalue) : EvalEnv.t * value =
  let v = value_of_lvalue env lv in
  match v with
  | VHeader(_,_,b) -> (env, VBool b)
  | _ -> failwith "isvalid call is not a header"

and eval_setvalid (env : EvalEnv.t) (lv : lvalue) : EvalEnv.t * value =
  match lv with
  | LName n
  | LTopName n ->
    begin match EvalEnv.find_val n env with
      | VHeader (n',fs,b) -> (fst (eval_assign' env lv (VHeader(n',fs,true))), VNull)
      | _ -> failwith "not a header" end
  | LMember(lv', n2) ->
    begin match value_of_lvalue env lv' with
      | VUnion (n1, fs, vs) ->
        let vs' = List.map vs ~f:(fun (a,_) -> (a,a=n2)) in
        (fst (eval_assign' env lv' (VUnion(n1, fs, vs'))), VNull)
      | _ -> failwith "not a union" end
  | LArrayAccess(lv', e) ->
    begin match value_of_lvalue env lv' with
      | VStack(n1,hdrs,size,next) ->
        let (env', i) = eval_expression env e in
        let i' = int_of_val i in
        let (hdrs1, hdrs2) = List.split_n hdrs i' in
        let hdrs' = match hdrs2 with
          | VHeader(n2,vs,b) :: t -> hdrs1 @ (VHeader(n2,vs,true) :: t)
          | _ -> failwith "not a header" in
        (fst (eval_assign' env' lv' (VStack(n1,hdrs',size,next))), VNull)
      | _ -> failwith "not a stack" end
  | LBitAccess _ -> failwith "not a header"

and eval_setinvalid (env : EvalEnv.t) (lv : lvalue) : EvalEnv.t * value =
  match lv with
  | LName n
  | LTopName n ->
    begin match EvalEnv.find_val n env with
      | VHeader (n',fs,b) -> (fst (eval_assign' env lv (VHeader(n',fs,false))), VNull)
      | _ -> failwith "not a header" end
  | LMember(lv', n2) ->
    begin match value_of_lvalue env lv' with
      | VUnion (n1, fs, vs) ->
        let vs' = List.map vs ~f:(fun (a,_) -> (a,false)) in
        (fst (eval_assign' env lv' (VUnion(n1, fs, vs'))), VNull)
      | _ -> failwith "not a union" end
  | LArrayAccess(lv', e) ->
    begin match value_of_lvalue env lv' with
      | VStack(n1,hdrs,size,next) ->
        let (env', i) = eval_expression env e in
        let i' = int_of_val i in
        let (hdrs1, hdrs2) = List.split_n hdrs i' in
        let hdrs' = match hdrs2 with
          | VHeader(n2,vs,b) :: t -> hdrs1 @ (VHeader(n2,vs,false) :: t)
          | _ -> failwith "not a header" in
        (fst (eval_assign' env' lv' (VStack(n1,hdrs',size,next))), VNull)
      | _ -> failwith "not a stack" end
  | LBitAccess _ -> failwith "not a header"

and eval_pushfront (env : EvalEnv.t) (lv : lvalue)
    (args : Argument.t list) : EvalEnv.t * value =
  let (env', a) = eval_push_pop_args env args in
  let (n, hdrs, size, next) =
    match value_of_lvalue env lv with
    | VStack(n,hdrs,size,next) -> (n,hdrs,size,next)
    | _ -> failwith "push call not a header stack" in
  let (hdrs1, hdrs2) = List.split_n hdrs (size - a) in
  let t = typ_of_stack_mem env n in
  let hdrs0 = List.init a ~f:(fun x -> init_val_of_typ env (string_of_int x) t) in
  let hdrs' = hdrs0 @ hdrs1 in
  let v = VStack(n,hdrs',size,next+a) in
  (fst (eval_assign' env lv v), VNull)

and eval_popfront (env : EvalEnv.t) (lv : lvalue)
    (args : Argument.t list) : EvalEnv.t * value =
  let (env', a) = eval_push_pop_args env args in
  let (n, hdrs, size, next) =
    match value_of_lvalue env lv with
    | VStack(n,hdrs,size,next) -> (n,hdrs,size,next)
    | _ -> failwith "push call not a header stack" in
  let (hdrs1, hdrs2) = List.split_n hdrs a in
  let t = typ_of_stack_mem env n in
  let hdrs0 = List.init a ~f:(fun x -> init_val_of_typ env (string_of_int x) t) in
  let hdrs' = hdrs2 @ hdrs0 in
  let v = VStack(n,hdrs',size,next-a) in
  (fst (eval_assign' env lv v), VNull)

and eval_push_pop_args (env : EvalEnv.t)
    (args : Argument.t list) : EvalEnv.t * int =
  let args' = List.map args ~f:snd in
  match args' with
  | [Argument.Expression{value}]
  | [Argument.KeyValue{value=value;_}] ->
    let (env', v) = eval_expression env value in
    (env', int_of_val v)
  | _ -> failwith "invalid push or pop args"

and eval_extract (env : EvalEnv.t) (lv : lvalue)
    (args : Argument.t list) : EvalEnv.t * value =
  match args with
  | [a]   -> eval_fixed_extract env lv args
  | [a;b] -> eval_var_extract env lv args
  | _ -> failwith "wrong number of args for extract"

and eval_emit (env : EvalEnv.t) (lv : lvalue)
  (args : Argument.t list) : EvalEnv.t * value =
  let args' = match args with
    | [a] -> List.map args ~f:snd
    | _ -> failwith "invalid emission args" in
  let (env', v) = match args' with
    | [Argument.Expression{value}]
    | [Argument.KeyValue{value=value;_}] -> eval_expression env value
    | _ -> failwith "invalid emission args" in
  let p = lv |> value_of_lvalue env |> assert_runtime |> assert_packet in
  let p' = emit p v in
  (fst (eval_assign' env' lv (VRuntime(Packet p'))), VNull)

and eval_fixed_extract (env : EvalEnv.t) (lv : lvalue)
    (args : Argument.t list) : EvalEnv.t * value =
  let args' = List.map args ~f:snd in
  let (env', v, lhdr) = match args' with
    | [Argument.Expression{value}]
    | [Argument.KeyValue{value=value;_}] ->
      let (env', v) = eval_expression env value in
      let lhdr = lvalue_of_expr value in
      (env', v, lhdr)
    | _ -> failwith "invalid extraction args" in
  let p = lv |> value_of_lvalue env' |> assert_runtime |> assert_packet in
  match v with
  | VHeader(n,fs,_) ->
    let ns, vs = List.unzip fs in
    let (p', vs') = List.fold_map vs ~init:p ~f:fixed_width_extract in
    let fs' = List.zip_exn ns vs' in
    let env'' = fst (eval_assign' env' lhdr (VHeader(n,fs',true))) in
    (fst (eval_assign' env'' lv (VRuntime(Packet p'))), VNull)
  | _ -> failwith "not a header"

and eval_var_extract (env : EvalEnv.t) (lv : lvalue)
    (args : Argument.t list) : EvalEnv.t * value =
  failwith "variable width extraction unimplemented"

(*----------------------------------------------------------------------------*)
(* Parser Evaluation *)
(*----------------------------------------------------------------------------*)

and eval_parser (env : EvalEnv.t) (params : Parameter.t list)
    (args : Argument.t list) (vs : (string * value) list)
    (locals : Declaration.t list) (states : Parser.state list) : EvalEnv.t * string =
  let (env', penv) = eval_inargs env params args in
  let f a (x,y) = EvalEnv.insert_val x y a in
  let penv' = List.fold_left vs ~init:penv ~f:f in
  let penv'' = List.fold_left locals ~init:penv' ~f:eval_decl in
  let states' = List.map states ~f:(fun s -> snd (snd s).name, s) in
  let start = List.Assoc.find_exn states' "start" ~equal:(=) in
  let (penv''',final_state) = eval_state_machine penv'' states' start in
  (eval_outargs env' penv''' params args, final_state)

and eval_state_machine (env : EvalEnv.t) (states : (string * Parser.state) list)
    (state : Parser.state) : EvalEnv.t * string =
  let (stms, transition) =
    match snd state with
    | {statements=stms; transition=t;_} -> (stms, t) in
  let stms' = (Info.dummy, Statement.BlockStatement
                 {block = (Info.dummy, {annotations = []; statements = stms})}) in
  let (env', sign) = eval_statement env SContinue stms' in
  match sign with
  | SContinue -> eval_transition env' states transition
  | SReject -> (env', "reject")
  | SReturn _ -> failwith "return statements not permitted in parsers"
  | SExit -> failwith "exit statements not permitted in parsers"

and eval_transition (env : EvalEnv.t) (states : (string * Parser.state) list)
    (transition : Parser.transition) : EvalEnv.t * string =
  match snd transition with
  | Direct{next = (_, next)} -> eval_direct env states next
  | Select{exprs;cases} -> eval_select env states exprs cases

and eval_direct (env : EvalEnv.t) (states : (string * Parser.state) list)
    (next : string) : EvalEnv.t * string =
  if next = "accept" || next = "reject"
  then
    (env, next)
  else
    let state = List.Assoc.find_exn states next ~equal:(=) in
    eval_state_machine env states state

and eval_select (env : EvalEnv.t) (states : (string * Parser.state) list)
    (exprs : Expression.t list) (cases : Parser.case list) : EvalEnv.t * string =
  let (env', vs) = List.fold_map exprs ~init:env ~f:eval_expression in
  let (env'', ss) = List.fold_map cases ~init:env' ~f:set_of_case in
  let (env''', ms) = List.fold_map ss ~init:env'' ~f:(values_match_set vs) in
  let next = List.Assoc.find_exn (List.zip_exn ms cases) true ~equal:(=) in
  let next' = snd (snd next).next in
  eval_direct env''' states next'

and set_of_case (env : EvalEnv.t) (case : Parser.case) : EvalEnv.t * set =
  let matches = (snd case).matches in
  match matches with
  | []  -> failwith "invalid set"
  | [m] -> set_of_match env m
  | l   -> let (env', l') = List.fold_map l ~init:env ~f:set_of_match in
    (env', SProd l')

and set_of_match (env : EvalEnv.t) (m : Match.t) : EvalEnv.t * set =
  match snd m with
  | Default
  | DontCare         -> (env, SUniversal)
  | Expression{expr} -> let (env', v) = eval_expression env expr in
    (env', assert_set v)

and values_match_set (vs : value list) (env : EvalEnv.t)
    (s : set) : EvalEnv.t * bool =
  match s with
  | SSingleton n  -> (env, values_match_singleton vs n)
  | SUniversal    -> (env, true)
  | SMask(v1,v2)  -> (env, values_match_mask vs v1 v2)
  | SRange(v1,v2) -> (env, values_match_range env vs v1 v2)
  | SProd l       -> values_match_prod env vs l

and values_match_singleton (vs :value list) (n : Bigint.t) : bool =
  let v = assert_singleton vs in
  v
  |> int_of_val
  |> Bigint.of_int
  |> (Bigint.(=) n)

and values_match_mask (vs : value list) (v1 : value)
    (v2 : value) : bool =
  let two = Bigint.(one + one) in
  let v = assert_singleton vs in
  let (a,b,c) = assert_bit v, assert_bit v1, assert_bit v2 in
  let rec h (w0,b0) (w1,b1) (w2,b2) =
    if not (w0 = w1 && w1 = w2)
    then false
    else if w0 = 0
    then true
    else if Bigint.(b2%two = zero) || Bigint.(b1%two = b0%two)
    then h (w0-1,Bigint.(b0/two)) (w1-1,Bigint.(b1/two)) (w2-1,Bigint.(b2/two))
    else false in
  h a b c

and values_match_range (env : EvalEnv.t) (vs : value list) (v1 : value)
    (v2 : value) : bool =
  let v = assert_singleton vs in
  match (v, v1, v2) with
  | VBit(w0,b0), VBit(w1,b1), VBit(w2,b2)
  | VInt(w0,b0), VInt(w1,b1), VInt(w2,b2) ->
    w0 = w1 && w1 = w2 && Bigint.(b1 <= b0 && b0 <= b2)
  | _ -> failwith "implicit casts unimplemented"

and values_match_prod (env : EvalEnv.t) (vs : value list)
    (l : set list) : EvalEnv.t * bool =
  let (env', bs) = List.fold_mapi l ~init:env
      ~f:(fun i e x -> values_match_set [List.nth_exn vs i] e x) in
  (env', List.for_all bs ~f:(fun b -> b))

(* -------------------------------------------------------------------------- *)
(* Control Evaluation *)
(* -------------------------------------------------------------------------- *)

and eval_control (env : EvalEnv.t) (params : Parameter.t list)
    (args : Argument.t list) (vs : (string * value) list)
    (locals : Declaration.t list) (apply : Block.t) : EvalEnv.t =
  let (env', cenv) = eval_inargs env params args in
  let f a (x,y) = EvalEnv.insert_val x y a in
  let cenv' = List.fold_left vs ~init:cenv ~f:f in
  let cenv'' = List.fold_left locals ~init:cenv' ~f:eval_decl in
  let block = (Info.dummy, Statement.BlockStatement {block = apply}) in
  let (cenv''', sign) = eval_statement cenv'' SContinue block in
  match sign with
  | SContinue
  | SExit     -> eval_outargs env' cenv''' params args
  | SReject   -> failwith "control should not reject"
  | SReturn _ -> failwith "control should not return"

(*----------------------------------------------------------------------------*)
(* Helper functions *)
(*----------------------------------------------------------------------------*)


and assert_singleton (vs : value list) : value =
  match vs with
  | [v] -> v
  | _ -> failwith "value list has more than one element"

and assert_bit (v : value) : int * Bigint.t =
  match v with
  | VBit(w,n) -> (w,n)
  | _ -> failwith "not a bitstring"

and assert_set (v : value) : set =
  match v with
  | VSet s -> s
  | VInteger i -> SSingleton i
  | _ -> failwith "not a set"

and assert_runtime (v : value) : vruntime =
  match v with
  | VRuntime r -> r
  | _ -> failwith "not a runtime value"

and assert_packet (p : vruntime) : packet =
  match p with Packet x -> x

and decl_of_typ (e : EvalEnv.t) (t : Type.t) : Declaration.t =
  match snd t with
  | TypeName (_,s)                 -> (EvalEnv.find_decl s e)
  | TopLevelType (_,s)             -> (EvalEnv.find_decl_toplevel s e)
  | _ -> (Info.dummy, Error{members = []}) (* TODO: find better solution *)

and init_binding_of_field (env : EvalEnv.t)
    (f : Declaration.field) : string * value =
  let n = snd (snd f).name in
  (n, init_val_of_typ env n (snd f).typ)

and int_of_val (v : value) : int =
  match v with
  | VInteger n
  | VBit (_,n)
  | VInt (_,n) -> Bigint.to_int_exn n
  | _ -> failwith "not an int"

and power_of_two (w : int) : Bigint.t =
  Bigint.shift_left (Bigint.of_int 1) (w-1)

and typ_of_union_field (env : EvalEnv.t) (uname : string)
    (fname : string) : Type.t =
  let t = EvalEnv.find_typ uname env in
  let (_, d) = decl_of_typ env t in
  let fs = match d with
    | HeaderUnion u -> u.fields
    | _ -> failwith "not a union" in
  match List.filter fs ~f:(fun a -> snd (snd a).name = fname) with
  | h :: _ -> (snd h).typ
  | _ -> failwith "field name not found"

and typ_of_stack_mem (env : EvalEnv.t) (name : string) : Type.t =
  let t = EvalEnv.find_typ name env in
  match snd t with
  | HeaderStack{header;_} -> header
  | _ -> failwith "not a header stack"

and struct_of_list (env : EvalEnv.t) (name : string) (l : value list) : value =
  env
  |> EvalEnv.find_typ name
  |> decl_of_typ env
  |> snd
  |> (function Declaration.Struct s -> s.fields | _ -> failwith "not a struct")
  |> List.map ~f:(fun x -> snd (snd x).name)
  |> (fun fs i v -> (List.nth_exn fs i, v))
  |> (fun f -> List.mapi l ~f:f)
  |> (fun l -> VStruct (name, l))

and header_of_list (env : EvalEnv.t) (name : string) (l : value list) : value =
  env
  |> EvalEnv.find_typ name
  |> decl_of_typ env
  |> snd
  |> (function Declaration.Header s -> s.fields | _ -> failwith "not a struct")
  |> List.map ~f:(fun x -> snd (snd x).name)
  |> (fun fs i v -> (List.nth_exn fs i, v))
  |> (fun f -> List.mapi l ~f:f)
  |> (fun l -> VHeader (name, l, true))

(* -------------------------------------------------------------------------- *)
(* Target and Architecture Dependent Evaluation *)
(* -------------------------------------------------------------------------- *)

let rec eval_main (env : EvalEnv.t) (pack : packet) : packet =
  let (_, obj, vs) =
    match EvalEnv.find_val "main" env with
    | VObjstate ((info, obj), vs) -> (info, obj, vs)
    | _ -> failwith "main not a stateful object" in
  let name =
    match obj with
    | Declaration.PackageType {name=(_,n);_} -> n
    | _ -> failwith "main is no a package" in
  match name with
  | "V1Switch" -> eval_v1switch env vs pack
  | "EmptyPackage" -> pack
  | _ -> failwith "architecture not supported"

and eval_v1switch (env : EvalEnv.t) (vs : (string * value) list)
    (pack : packet) : packet =
  let parser =
    List.Assoc.find_exn vs "p"   ~equal:(=) in
  let verify =
    List.Assoc.find_exn vs "vr"  ~equal:(=) in
  let ingress =
    List.Assoc.find_exn vs "ig"  ~equal:(=) in
  let egress =
    List.Assoc.find_exn vs "eg"  ~equal:(=) in
  let compute =
    List.Assoc.find_exn vs "ck"  ~equal:(=) in
  let deparser =
    List.Assoc.find_exn vs "dep" ~equal:(=) in
  let (_, obj, pvs) =
    match parser with
    | VObjstate ((info, obj), pvs) -> (info, obj, pvs)
    | _ -> failwith "parser is not a stateful object" in
  let params =
    match obj with
    | Parser {params=ps;_} -> ps
    | _ -> failwith "parser is not a parser object" in
  let pckt = VRuntime (Packet pack) in
  let hdr =
    init_val_of_typ env "hdr"      (snd (List.nth_exn params 1)).typ in
  let meta =
    init_val_of_typ env "meta"     (snd (List.nth_exn params 2)).typ in
  let std_meta =
    init_val_of_typ env "std_meta" (snd (List.nth_exn params 3)).typ in
  let env =
    EvalEnv.(env
             |> insert_val "packet"   pckt
             |> insert_val "hdr"      hdr
             |> insert_val "meta"     meta
             |> insert_val "std_meta" std_meta
             |> insert_typ "packet"   (snd (List.nth_exn params 0)).typ
             |> insert_typ "hdr"      (snd (List.nth_exn params 1)).typ
             |> insert_typ "meta"     (snd (List.nth_exn params 2)).typ
             |> insert_typ "std_meta" (snd (List.nth_exn params 3)).typ) in
  (* TODO: implement a more responsible way to generate variable names *)
  let pckt_expr =
    (Info.dummy, Argument.Expression {value = (Info.dummy, Name (Info.dummy, "packet"))}) in
  let hdr_expr =
    (Info.dummy, Argument.Expression {value = (Info.dummy, Name (Info.dummy, "hdr"))}) in
  let meta_expr =
    (Info.dummy, Argument.Expression {value = (Info.dummy, Name (Info.dummy, "meta"))}) in
  let std_meta_expr =
    (Info.dummy, Argument.Expression {value = (Info.dummy, Name (Info.dummy, "std_meta"))}) in
  let env = env
            |> eval_v1parser  parser   [pckt_expr; hdr_expr; meta_expr; std_meta_expr]
            |> fst (* TODO: handle errors and parser rejections *)
            |> eval_v1control verify   [hdr_expr; meta_expr]
            |> eval_v1control ingress  [hdr_expr; meta_expr; std_meta_expr]
            |> eval_v1control egress   [hdr_expr; meta_expr; std_meta_expr]
            |> eval_v1control compute  [hdr_expr; meta_expr]
            |> eval_v1control deparser [pckt_expr; hdr_expr] in
  print_endline "After runtime evaluation";
  EvalEnv.print_env env;
  match EvalEnv.find_val "packet" env with
  | VRuntime (Packet p) -> p
  | _ -> failwith "pack not a packet"

and eval_v1parser (parser : value) (args : Argument.t list)
    (env : EvalEnv.t) : EvalEnv.t * string =
  let (_, decl, vs) =
    match parser with
    | VObjstate((info, decl), vs) -> (info, decl, vs)
    | _ -> failwith "v1 parser is not a stateful object" in
  let (params, locals, states) =
    match decl with
    | Parser {params=ps;locals=ls;states=ss;_} -> (ps,ls,ss)
    | _ -> failwith "v1 parser is not a parser" in
  eval_parser env params args vs locals states

and eval_v1control (control : value) (args : Argument.t list)
    (env : EvalEnv.t) : EvalEnv.t =
  let (_, decl, vs) =
    match control with
    | VObjstate((info, decl), vs) -> (info,  decl, vs)
    | _ -> failwith "v1 control is not a stateful object" in
  let (params, locals, apply) =
    match decl with
    | Control{params=ps; locals=ls; apply=b; _} -> (ps, ls, b)
    | _ -> failwith "v1 control is not a control" in
  eval_control env params args vs locals apply

(*----------------------------------------------------------------------------*)
(* Program Evaluation *)
(*----------------------------------------------------------------------------*)

let byte_packet_fortytwo = [false;false;true;false;true;false;true;false]

let eval_program = function Program l ->
  let env = List.fold_left l ~init:EvalEnv.empty_eval_env ~f:eval_decl in
  EvalEnv.print_env env;
  Format.printf "Done\n";
  let packetin = byte_packet_fortytwo @ byte_packet_fortytwo @ [true;false]
                 |> packet_of_list in
  let packout = eval_main env packetin in
  ignore packout
