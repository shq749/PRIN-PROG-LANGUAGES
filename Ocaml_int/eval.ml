open Ast

exception TypeError
exception UndefinedVar
exception DivByZeroError

(* Remove shadowed bindings *)
let prune_env (env : environment) : environment =
  let binds = List.sort_uniq compare (List.map (fun (id, _) -> id) env) in
  List.map (fun e -> (e, List.assoc e env)) binds

(* Env print function to stdout *)
let print_env_std (env : environment): unit =
  List.fold_left (fun _ (var, value) ->
      match value with
        | Int_Val i -> Printf.printf "- %s => %s\n" var (string_of_int i)
        | Bool_Val b -> Printf.printf "- %s => %s\n" var (string_of_bool b)
        | Closure _ -> ()) () (prune_env env)

(* Env print function to string *)
let print_env_str (env : environment): string =
  List.fold_left (fun acc (var, value) ->
      match value with
        | Int_Val i -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_int i))
        | Bool_Val b -> acc ^ (Printf.sprintf "- %s => %s\n" var (string_of_bool b))
        | Closure _ -> acc
      ) "" (prune_env env)


(***********************)
(****** Your Code ******)
(***********************)

(* evaluate an arithmetic expression in an environment *)
let rec eval_expr (e : exp) (env : environment) : value =
    match e with
    | Var x ->
     let rec lookup env x =
    ( match env with
      | []-> raise UndefinedVar
      | (a,b)::env1 ->
         if a=x then b
         else lookup env1 x
    )
      in lookup env x
  
      
    | Number n -> Int_Val n
  
    | Fun (s1,e1) ->
      Closure(env,s1,e1)

    | App(e1,e2) ->
      let x1 = eval_expr e2 env in
      let c1 = eval_expr e1 env in
      (match c1 with
      | Bool_Val _ -> raise TypeError
      | Int_Val _ -> raise TypeError
      | Closure (env1,s2,e3) -> 
        
         let evn = (s2,x1)::env1 in
          eval_expr e3 evn )
      
    | Plus (e1,e2) ->
      let n1 = eval_expr e1 env in
      (match n1 with
      | Bool_Val _ -> raise TypeError
      | Closure (_,_,_) -> raise TypeError
      | Int_Val c1 ->
      let n2 = eval_expr e2 env in
       match n2 with
      | Bool_Val _ -> raise TypeError
      | Closure (_,_,_) -> raise TypeError
      | Int_Val c2 ->
      let n3 = c1+c2 in
       Int_Val n3 )

    | Minus (e1,e2) ->
      let n1 = eval_expr e1 env in
      (match n1 with
      | Bool_Val _ -> raise TypeError
      | Closure (_,_,_) -> raise TypeError
      | Int_Val c1 ->
      let n2 = eval_expr e2 env in
       match n2 with
      | Bool_Val _ -> raise TypeError
      | Closure (_,_,_) -> raise TypeError
      | Int_Val c2 ->
      let n3 = c1-c2 in
       Int_Val n3 )

    | Times (e1,e2) ->
      let n1 = eval_expr e1 env in
      (match n1 with
      | Bool_Val _ -> raise TypeError
      | Closure (_,_,_) -> raise TypeError
      | Int_Val c1 ->
      let n2 = eval_expr e2 env in
       match n2 with
      | Bool_Val _ -> raise TypeError
      | Closure (_,_,_) -> raise TypeError
      | Int_Val c2 ->
      let n3 = c1*c2 in
       Int_Val n3 )
    
    | Div (e1,e2) ->
      let n1 = eval_expr e1 env in
      (match n1 with
      | Bool_Val _ -> raise TypeError
      | Closure (_,_,_) -> raise TypeError
      | Int_Val c1 ->
      let n2 = eval_expr e2 env in
       match n2 with
      | Bool_Val _ -> raise TypeError
      | Closure (_,_,_) -> raise TypeError
      | Int_Val 0 -> raise DivByZeroError
      | Int_Val c2 ->
      let n3 = c1/c2 in
       Int_Val n3 )

    | Mod (e1,e2) ->
      let n1 = eval_expr e1 env in
      (match n1 with
      | Bool_Val _ -> raise TypeError
      | Closure (_,_,_) -> raise TypeError
      | Int_Val c1 ->
      let n2 = eval_expr e2 env in
       match n2 with
      | Bool_Val _ -> raise TypeError
      | Closure (_,_,_) -> raise TypeError
      | Int_Val 0 -> raise DivByZeroError
      | Int_Val c2 ->
      let n3 = c1 mod c2 in
       Int_Val n3 )

    | False -> Bool_Val false

    | True -> Bool_Val true

    | And (e1, e2) ->
      let b1 = eval_expr e1 env in
      (match b1 with
      |Closure (_,_,_) -> raise TypeError
      |Int_Val _ -> raise TypeError
      | Bool_Val be -> 
       let b2 = eval_expr e2 env in
       match b2 with
      |Closure (_,_,_) -> raise TypeError
      |Int_Val _ -> raise TypeError
      | Bool_Val b22 -> 
      if b1=b2 then b1
      else Bool_Val false
      
        )

    | Or (e1,e2) ->
    let b1 = eval_expr e1 env in
      (match b1 with
      |Closure (_,_,_) -> raise TypeError
      |Int_Val _ -> raise TypeError
      | Bool_Val be -> 
       let b2 = eval_expr e2 env in
       match b2 with
      |Closure (_,_,_) -> raise TypeError
      |Int_Val _ -> raise TypeError
      | Bool_Val b22 -> 
      if be=b22 then Bool_Val be
      else Bool_Val true
      )

    | Not e1 ->
      let b1 = eval_expr e1 env in
      (match b1 with
      |Closure (_,_,_) -> raise TypeError
      |Int_Val _ -> raise TypeError
      | Bool_Val be -> 
      if be = true then Bool_Val false
      else Bool_Val true
      )

    | Eq (e1,e2) ->
      let b1 = eval_expr e1 env in
      ( match b1 with
      |Closure (_,_,_) -> raise TypeError
      |Int_Val be -> 
      let b2 = eval_expr e2 env in
        (match b2 with
        |Closure (_,_,_) -> raise TypeError
        |Bool_Val _ ->raise TypeError
        |Int_Val b22 -> 
        if b22=be then Bool_Val true
        else Bool_Val false)
      | Bool_Val b11 -> let b3 = eval_expr e2 env in
        match b3 with
          |Closure (_,_,_) -> raise TypeError
          |Int_Val _ -> raise TypeError
          |Bool_Val b33 ->
          if b33=b11 then Bool_Val true
          else Bool_Val false)

     | Lt(e1,e2) ->
       let n1 = eval_expr e1 env in
      (match n1 with
       |Closure (_,_,_) -> raise TypeError
       |Bool_Val _ -> raise TypeError
       |Int_Val n11 -> 
       let n2 = eval_expr e2 env in
        match n2 with
       |Closure (_,_,_) -> raise TypeError
       |Bool_Val _ -> raise TypeError
       |Int_Val n22 -> 
       if n11<n22 then Bool_Val true
       else Bool_Val false)

      | Leq(e1,e2) ->
        let n1 = eval_expr e1 env in
      (match n1 with
       |Closure (_,_,_) -> raise TypeError
       |Bool_Val _ -> raise TypeError
       |Int_Val n11 -> 
       let n2 = eval_expr e2 env in
        match n2 with
       |Closure (_,_,_) -> raise TypeError
       |Bool_Val _ -> raise TypeError
       |Int_Val n22 -> 
       if n11<n22 then Bool_Val true
       else if n22=n11 then Bool_Val true
       else Bool_Val false)
       

      
      
  

       

      


(* evaluate a command in an environment *)
let rec eval_command (c : com) (env : environment) : environment =
  match c with
| Skip -> env

| Comp (c1,c2) ->
  let env1 = eval_command c1 env in
  eval_command c2 env1 

| Declare (d1,s1) ->
  (match d1 with
  | Int_Type  -> (s1, (Int_Val 0)) ::env
  | Bool_Type  -> (s1,(Bool_Val false)) :: env
  | Lambda_Type  -> (s1,(Closure (env, "x", Var "x"))) :: env
  )

| Assg (s1,e1) ->
let e2 = eval_expr e1 env in
  let rec looup env s1 =
   match env with 
   [] -> raise UndefinedVar
  | (a,b)::env1->
   if a=s1 then
    (match e2,b with
    | Int_Val e2, Bool_Val e3 -> raise TypeError
    | Int_Val e2, Closure (_,_,_) -> raise TypeError
    | Bool_Val e2, Int_Val e3 -> raise TypeError
    | Bool_Val e2, Closure (_,_,_) -> raise TypeError
    | Closure (_,_,_) , Int_Val e3 -> raise TypeError
    | Closure (_,_,_) , Bool_Val e3 -> raise TypeError
    | _, _ -> (s1,e2)::env )
   
   else (a,b)::looup env1 s1
   in looup env s1 

   | Cond (e1, c1,c2) ->
     let e2= eval_expr e1 env in
     (match e2 with
     |Closure (_,_,_) -> raise TypeError
     |Int_Val _ -> raise TypeError
     | Bool_Val e3 -> 
     if e3=true then eval_command c1 env
     else eval_command c2 env
     )

  | While (e1,c1) ->
    let rec helper e1 env=
    let v= eval_expr e1 env in
     (match v with
     |Closure (_,_,_) -> raise TypeError
     |Int_Val _ -> raise TypeError
     | Bool_Val v1 -> 
       if v1=true then 
         let env1 = eval_command c1 env in
         helper e1 env1
       else 
         env
     )
      in helper e1 env

  | For (e1,c1) ->
    let n = eval_expr e1 env in
    (match n with
    |Closure (_,_,_) -> raise TypeError
    | Bool_Val _ -> raise TypeError
    |Int_Val n1 -> 
      let rec helper env n1 = 
      if n1<=0 then env
      else let env1 = eval_command c1 env in
         helper env1 (n1-1)
    
      in helper env n1

    )
      

    
      
  

 
     
    
   
   
  

 
