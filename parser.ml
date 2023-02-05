
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | VAR
    | UMINUS
    | TIMES
    | THIS
    | THEN
    | SUPER
    | STR of (
# 17 "parser.mly"
       (string)
# 17 "parser.ml"
  )
    | SEMICOLON
    | RPAREN
    | RETURN
    | RESULT
    | RELOP of (
# 8 "parser.mly"
       (Ast.opComp)
# 26 "parser.ml"
  )
    | RCBR
    | PLUS
    | OVERRIDE
    | OBJECT
    | NEW
    | MINUS
    | LPAREN
    | LCBR
    | IS
    | IF
    | IDCLASS of (
# 25 "parser.mly"
       (string)
# 41 "parser.ml"
  )
    | ID of (
# 5 "parser.mly"
       (string)
# 46 "parser.ml"
  )
    | EXTENDS
    | EOF
    | ELSE
    | DOT
    | DIV
    | DEF
    | CSTE of (
# 7 "parser.mly"
       (int)
# 57 "parser.ml"
  )
    | CONCATE
    | COLON
    | CLASS
    | AUTO
    | ASSIGN
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState102
  | MenhirState100
  | MenhirState97
  | MenhirState91
  | MenhirState86
  | MenhirState85
  | MenhirState82
  | MenhirState78
  | MenhirState72
  | MenhirState68
  | MenhirState66
  | MenhirState62
  | MenhirState61
  | MenhirState60
  | MenhirState57
  | MenhirState55
  | MenhirState54
  | MenhirState53
  | MenhirState47
  | MenhirState45
  | MenhirState44
  | MenhirState42
  | MenhirState41
  | MenhirState40
  | MenhirState39
  | MenhirState38
  | MenhirState37
  | MenhirState35
  | MenhirState33
  | MenhirState32
  | MenhirState31
  | MenhirState30
  | MenhirState29
  | MenhirState28
  | MenhirState22
  | MenhirState20
  | MenhirState19
  | MenhirState12
  | MenhirState11
  | MenhirState4
  | MenhirState3
  | MenhirState0

# 1 "parser.mly"
  
    open Ast

# 127 "parser.ml"

let rec _menhir_goto_methode : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 43 "parser.mly"
      (Ast.method_def)
# 132 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEF ->
        _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | RCBR ->
        _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102

and _menhir_reduce15 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 44 "parser.mly"
      (Ast.ident)
# 152 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (id : (
# 44 "parser.mly"
      (Ast.ident)
# 158 "parser.ml"
    ))) = _menhir_stack in
    let _v : (
# 37 "parser.mly"
      (Ast.expression)
# 163 "parser.ml"
    ) = 
# 148 "parser.mly"
                 ( Ident(id) )
# 167 "parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (id : (
# 25 "parser.mly"
       (string)
# 187 "parser.ml"
            ))), _, (le : (Ast.expression list))) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _v : (
# 37 "parser.mly"
      (Ast.expression)
# 194 "parser.ml"
            ) = 
# 156 "parser.mly"
                                                         ( NewInstance(id , le) )
# 198 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (
# 37 "parser.mly"
      (Ast.expression)
# 213 "parser.ml"
        ))), _, (xs : (Ast.expression list))) = _menhir_stack in
        let _v : (Ast.expression list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 218 "parser.ml"
         in
        _menhir_goto_list_expression_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), (id : (
# 25 "parser.mly"
       (string)
# 233 "parser.ml"
            ))), _, (le : (Ast.expression list))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (
# 37 "parser.mly"
      (Ast.expression)
# 241 "parser.ml"
            ) = 
# 155 "parser.mly"
                                                             ( NewInstance(id , le) )
# 245 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 37 "parser.mly"
      (Ast.expression)
# 260 "parser.ml"
) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | ID _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | IDCLASS _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | NEW ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | RESULT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | STR _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | SUPER ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | THIS ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | UMINUS ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 37 "parser.mly"
      (Ast.expression)
# 295 "parser.ml"
) -> _menhir_state -> (
# 8 "parser.mly"
       (Ast.opComp)
# 299 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | ID _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | IDCLASS _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NEW ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | RESULT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | STR _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | SUPER ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | THIS ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | UMINUS ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 37 "parser.mly"
      (Ast.expression)
# 334 "parser.ml"
) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | ID _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | IDCLASS _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | NEW ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | RESULT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | STR _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _v
    | SUPER ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | THIS ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | UMINUS ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run41 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 37 "parser.mly"
      (Ast.expression)
# 369 "parser.ml"
) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | ID _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | IDCLASS _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | NEW ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | RESULT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | STR _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
    | SUPER ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | THIS ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | UMINUS ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState41
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 37 "parser.mly"
      (Ast.expression)
# 404 "parser.ml"
) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | RESULT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | SUPER ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | THIS ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run39 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 37 "parser.mly"
      (Ast.expression)
# 427 "parser.ml"
) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | ID _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | IDCLASS _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | NEW ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | RESULT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | STR _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
    | SUPER ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | THIS ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | UMINUS ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState39
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39

and _menhir_run35 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 37 "parser.mly"
      (Ast.expression)
# 462 "parser.ml"
) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | ID _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | IDCLASS _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | NEW ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | RESULT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | STR _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | SUPER ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | THIS ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | UMINUS ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_goto_list_methode_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.method_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (
# 43 "parser.mly"
      (Ast.method_def)
# 504 "parser.ml"
        ))), _, (xs : (Ast.method_def list))) = _menhir_stack in
        let _v : (Ast.method_def list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 509 "parser.ml"
         in
        _menhir_goto_list_methode_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RCBR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _, (ld : (Ast.declaration list))), _, (lm : (Ast.method_def list))) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : (
# 42 "parser.mly"
      (Ast.block_class)
# 527 "parser.ml"
            ) = 
# 105 "parser.mly"
                                                                  (
   {  declarations = ld ;
    methodes = lm ;
    }
)
# 535 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (bb : (
# 42 "parser.mly"
      (Ast.block_class)
# 542 "parser.ml"
            )) = _v in
            let ((((((_menhir_stack, _menhir_s, (c : (bool))), (o : (bool))), (n : (
# 25 "parser.mly"
       (string)
# 547 "parser.ml"
            ))), _, (lp : (Ast.declaration list))), (s : (string option))), _, (b : (Ast.block option))) = _menhir_stack in
            let _8 = () in
            let _4 = () in
            let _v : (
# 38 "parser.mly"
      (Ast.class_def)
# 554 "parser.ml"
            ) = 
# 84 "parser.mly"
    (
        {
            name_class = n;
            is_class = c;
            is_object = o;
            params = lp;
            superclass = s;
            constructor = b;
            content = bb;
        }
    )
# 568 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CLASS ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | LCBR ->
                _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | IDCLASS _ | OBJECT ->
                _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_boption_OVERRIDE_ : _menhir_env -> 'ttv_tail -> (bool) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | VAR ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | RPAREN ->
                _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState91
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_option_block_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.block option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LCBR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | VAR ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | DEF | RCBR ->
                _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_list_instruction_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.instruction list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RCBR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (ld : (Ast.declaration list))), _, (instrs : (Ast.instruction list))) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : (
# 41 "parser.mly"
      (Ast.block)
# 690 "parser.ml"
            ) = 
# 98 "parser.mly"
                                                                        (
    {
        declarations = ld;
        instructions = instrs;
    }
)
# 699 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState3 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EOF ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, (ld : (Ast.class_def list))), _, (bl : (
# 41 "parser.mly"
      (Ast.block)
# 714 "parser.ml"
                    ))) = _menhir_stack in
                    let _3 = () in
                    let _v : (
# 61 "parser.mly"
       (Ast.prog)
# 720 "parser.ml"
                    ) = 
# 66 "parser.mly"
                                     ( 
        {
                classes = ld ;
                block = bl ;
        }                                      
    )
# 729 "parser.ml"
                     in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_1 : (
# 61 "parser.mly"
       (Ast.prog)
# 736 "parser.ml"
                    )) = _v in
                    Obj.magic _1
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState97 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((((_menhir_stack, _menhir_s), (o : (bool))), (n : (
# 5 "parser.mly"
       (string)
# 751 "parser.ml"
                ))), _, (lp : (Ast.declaration list))), (r : (string))), _, (b : (
# 41 "parser.mly"
      (Ast.block)
# 755 "parser.ml"
                ))) = _menhir_stack in
                let _8 = () in
                let _6 = () in
                let _4 = () in
                let _1 = () in
                let _v : (
# 43 "parser.mly"
      (Ast.method_def)
# 764 "parser.ml"
                ) = 
# 124 "parser.mly"
    (
        {
            is_override = o;
            name_meth = n;
            params = lp;
            return_type = Some r;
            content = b;
        }
    )
# 776 "parser.ml"
                 in
                _menhir_goto_methode _menhir_env _menhir_stack _menhir_s _v
            | MenhirState100 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((((_menhir_stack, _menhir_s), (o : (bool))), (n : (
# 5 "parser.mly"
       (string)
# 785 "parser.ml"
                ))), _, (lp : (Ast.declaration list))), (r : (string option))), _, (b : (
# 41 "parser.mly"
      (Ast.block)
# 789 "parser.ml"
                ))) = _menhir_stack in
                let _8 = () in
                let _6 = () in
                let _4 = () in
                let _1 = () in
                let _v : (
# 43 "parser.mly"
      (Ast.method_def)
# 798 "parser.ml"
                ) = 
# 114 "parser.mly"
    (
        {
            is_override = o;
            name_meth = n;
            params = lp;
            return_type = r;
            content = b;
        }
    )
# 810 "parser.ml"
                 in
                _menhir_goto_methode _menhir_env _menhir_stack _menhir_s _v
            | MenhirState82 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (
# 41 "parser.mly"
      (Ast.block)
# 819 "parser.ml"
                ))) = _menhir_stack in
                let _v : (Ast.block option) = 
# 116 "<standard.mly>"
    ( Some x )
# 824 "parser.ml"
                 in
                _menhir_goto_option_block_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (
# 46 "parser.mly"
      (Ast.instruction)
# 841 "parser.ml"
        ))), _, (xs : (Ast.instruction list))) = _menhir_stack in
        let _v : (Ast.instruction list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 846 "parser.ml"
         in
        _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 46 "parser.mly"
      (Ast.instruction)
# 855 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CSTE _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | ID _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | IDCLASS _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | IF ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | NEW ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | RESULT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | RETURN ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | STR _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
            | SUPER ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | THIS ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | UMINUS ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState57
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), _, (si : (
# 37 "parser.mly"
      (Ast.expression)
# 910 "parser.ml"
        ))), _), _, (alors : (
# 46 "parser.mly"
      (Ast.instruction)
# 914 "parser.ml"
        ))), _, (sinon : (
# 46 "parser.mly"
      (Ast.instruction)
# 918 "parser.ml"
        ))) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _v : (
# 46 "parser.mly"
      (Ast.instruction)
# 926 "parser.ml"
        ) = 
# 179 "parser.mly"
                                                                       (Ite(si,alors,sinon))
# 930 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState66 | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CSTE _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | ID _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | IDCLASS _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | IF ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | LPAREN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | NEW ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | RESULT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | RETURN ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | STR _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | SUPER ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | THIS ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | UMINUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | RCBR ->
            _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
    | _ ->
        _menhir_fail ()

and _menhir_goto_ident : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 44 "parser.mly"
      (Ast.ident)
# 974 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState60 | MenhirState53 | MenhirState12 | MenhirState19 | MenhirState20 | MenhirState44 | MenhirState28 | MenhirState41 | MenhirState39 | MenhirState37 | MenhirState35 | MenhirState31 | MenhirState29 | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (a : (
# 37 "parser.mly"
      (Ast.expression)
# 988 "parser.ml"
        ))), _), _, (i : (
# 44 "parser.mly"
      (Ast.ident)
# 992 "parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 37 "parser.mly"
      (Ast.expression)
# 998 "parser.ml"
        ) = 
# 160 "parser.mly"
                                   ( Access(a,i) )
# 1002 "parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState11 | MenhirState66 | MenhirState55 | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ASSIGN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CSTE _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | ID _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | IDCLASS _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | NEW ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | RESULT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | STR _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | SUPER ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | THIS ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | UMINUS ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
        | CONCATE | DIV | DOT | MINUS | PLUS | RELOP _ | SEMICOLON | TIMES ->
            _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce41 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.expression list) = 
# 211 "<standard.mly>"
    ( [] )
# 1055 "parser.ml"
     in
    _menhir_goto_list_expression_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce29 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 5 "parser.mly"
       (string)
# 1062 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (n : (
# 5 "parser.mly"
       (string)
# 1068 "parser.ml"
    ))) = _menhir_stack in
    let _v : (
# 44 "parser.mly"
      (Ast.ident)
# 1073 "parser.ml"
    ) = 
# 138 "parser.mly"
             ( Local n )
# 1077 "parser.ml"
     in
    _menhir_goto_ident _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 37 "parser.mly"
      (Ast.expression)
# 1084 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState19 | MenhirState28 | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCATE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | CSTE _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | ID _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | IDCLASS _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | LPAREN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | MINUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | NEW ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | RELOP _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | RESULT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | STR _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | SUPER ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | THIS ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | UMINUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | RPAREN ->
            _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCATE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | RELOP _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | CSTE _ | DIV | ELSE | ID _ | IDCLASS _ | IF | LPAREN | MINUS | NEW | PLUS | RCBR | RESULT | RETURN | RPAREN | SEMICOLON | STR _ | SUPER | THEN | THIS | TIMES | UMINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (a : (
# 37 "parser.mly"
      (Ast.expression)
# 1150 "parser.ml"
            ))), _), _, (b : (
# 37 "parser.mly"
      (Ast.expression)
# 1154 "parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 37 "parser.mly"
      (Ast.expression)
# 1160 "parser.ml"
            ) = 
# 151 "parser.mly"
                                                      ( Times(a,b) )
# 1164 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30)
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCATE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | CSTE _ | DIV | ELSE | ID _ | IDCLASS _ | IF | LPAREN | MINUS | NEW | PLUS | RCBR | RELOP _ | RESULT | RETURN | RPAREN | SEMICOLON | STR _ | SUPER | THEN | THIS | TIMES | UMINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (a : (
# 37 "parser.mly"
      (Ast.expression)
# 1185 "parser.ml"
            ))), _, (op : (
# 8 "parser.mly"
       (Ast.opComp)
# 1189 "parser.ml"
            ))), _, (b : (
# 37 "parser.mly"
      (Ast.expression)
# 1193 "parser.ml"
            ))) = _menhir_stack in
            let _v : (
# 37 "parser.mly"
      (Ast.expression)
# 1198 "parser.ml"
            ) = 
# 159 "parser.mly"
                                               (Compo(op,a,b))
# 1202 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (a : (
# 37 "parser.mly"
      (Ast.expression)
# 1215 "parser.ml"
        ))), _), _, (b : (
# 37 "parser.mly"
      (Ast.expression)
# 1219 "parser.ml"
        ))) = _menhir_stack in
        let _2 = () in
        let _v : (
# 37 "parser.mly"
      (Ast.expression)
# 1225 "parser.ml"
        ) = 
# 153 "parser.mly"
                                            ( Concate(a,b) )
# 1229 "parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCATE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | RELOP _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | CSTE _ | ELSE | ID _ | IDCLASS _ | IF | LPAREN | MINUS | NEW | PLUS | RCBR | RESULT | RETURN | RPAREN | SEMICOLON | STR _ | SUPER | THEN | THIS | UMINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (a : (
# 37 "parser.mly"
      (Ast.expression)
# 1252 "parser.ml"
            ))), _), _, (b : (
# 37 "parser.mly"
      (Ast.expression)
# 1256 "parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 37 "parser.mly"
      (Ast.expression)
# 1262 "parser.ml"
            ) = 
# 149 "parser.mly"
                                                      ( Plus(a,b) )
# 1266 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCATE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState40
        | RELOP _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
        | CSTE _ | DIV | ELSE | ID _ | IDCLASS _ | IF | LPAREN | MINUS | NEW | PLUS | RCBR | RESULT | RETURN | RPAREN | SEMICOLON | STR _ | SUPER | THEN | THIS | TIMES | UMINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (a : (
# 37 "parser.mly"
      (Ast.expression)
# 1289 "parser.ml"
            ))), _), _, (b : (
# 37 "parser.mly"
      (Ast.expression)
# 1293 "parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 37 "parser.mly"
      (Ast.expression)
# 1299 "parser.ml"
            ) = 
# 152 "parser.mly"
                                                  (Div(a,b))
# 1303 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCATE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | RELOP _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState42
        | CSTE _ | ELSE | ID _ | IDCLASS _ | IF | LPAREN | MINUS | NEW | PLUS | RCBR | RESULT | RETURN | RPAREN | SEMICOLON | STR _ | SUPER | THEN | THIS | UMINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (a : (
# 37 "parser.mly"
      (Ast.expression)
# 1330 "parser.ml"
            ))), _), _, (b : (
# 37 "parser.mly"
      (Ast.expression)
# 1334 "parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 37 "parser.mly"
      (Ast.expression)
# 1340 "parser.ml"
            ) = 
# 150 "parser.mly"
                                                      ( Minus(a,b) )
# 1344 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCATE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | MINUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | RELOP _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState45 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (tipe : (
# 5 "parser.mly"
       (string)
# 1376 "parser.ml"
            ))), _, (e : (
# 37 "parser.mly"
      (Ast.expression)
# 1380 "parser.ml"
            ))) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : (
# 37 "parser.mly"
      (Ast.expression)
# 1387 "parser.ml"
            ) = 
# 157 "parser.mly"
                                             ( Cast(tipe , e) )
# 1391 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCATE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | MINUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | RELOP _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState47 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (
# 37 "parser.mly"
      (Ast.expression)
# 1425 "parser.ml"
            ))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (
# 37 "parser.mly"
      (Ast.expression)
# 1432 "parser.ml"
            ) = 
# 158 "parser.mly"
                                     ( e )
# 1436 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (e : (
# 37 "parser.mly"
      (Ast.expression)
# 1451 "parser.ml"
        ))) = _menhir_stack in
        let _1 = () in
        let _v : (
# 37 "parser.mly"
      (Ast.expression)
# 1457 "parser.ml"
        ) = 
# 154 "parser.mly"
                            ( Unary(e) )
# 1461 "parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCATE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | MINUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | RELOP _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState54 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CSTE _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | ID _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | IDCLASS _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | IF ->
                _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | NEW ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | RESULT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | RETURN ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | STR _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState55 _v
            | SUPER ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | THIS ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | UMINUS ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState55
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState55)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCATE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | MINUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | RELOP _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState61
        | CSTE _ | ELSE | ID _ | IDCLASS _ | IF | LPAREN | NEW | RCBR | RESULT | RETURN | STR _ | SUPER | THIS | UMINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (n : (
# 44 "parser.mly"
      (Ast.ident)
# 1546 "parser.ml"
            ))), _, (r : (
# 37 "parser.mly"
      (Ast.expression)
# 1550 "parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 46 "parser.mly"
      (Ast.instruction)
# 1556 "parser.ml"
            ) = 
# 178 "parser.mly"
                                    (Aff(n,r))
# 1560 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
    | MenhirState11 | MenhirState66 | MenhirState55 | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCATE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | DIV ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | MINUS ->
            _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | PLUS ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | RELOP _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState62 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (n : (
# 37 "parser.mly"
      (Ast.expression)
# 1592 "parser.ml"
            ))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 46 "parser.mly"
      (Ast.instruction)
# 1598 "parser.ml"
            ) = 
# 176 "parser.mly"
                             ( Exp(n) )
# 1602 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState62
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62)
    | _ ->
        _menhir_fail ()

and _menhir_goto_option_returned_type_ : _menhir_env -> 'ttv_tail -> (string option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LCBR ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState100
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.method_def list) = 
# 211 "<standard.mly>"
    ( [] )
# 1644 "parser.ml"
     in
    _menhir_goto_list_methode_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run87 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | OVERRIDE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (bool) = 
# 135 "<standard.mly>"
    ( true )
# 1662 "parser.ml"
         in
        _menhir_goto_boption_OVERRIDE_ _menhir_env _menhir_stack _v
    | ID _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (bool) = 
# 133 "<standard.mly>"
    ( false )
# 1670 "parser.ml"
         in
        _menhir_goto_boption_OVERRIDE_ _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_option_extends_ : _menhir_env -> 'ttv_tail -> (string option) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LCBR ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState82
    | IS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState82 in
        let _v : (Ast.block option) = 
# 114 "<standard.mly>"
    ( None )
# 1695 "parser.ml"
         in
        _menhir_goto_option_block_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState82

and _menhir_reduce43 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.instruction list) = 
# 211 "<standard.mly>"
    ( [] )
# 1708 "parser.ml"
     in
    _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | ID _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | IDCLASS _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | NEW ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | RESULT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | STR _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | SUPER ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | THIS ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | UMINUS ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (
# 44 "parser.mly"
      (Ast.ident)
# 1751 "parser.ml"
    ) = 
# 139 "parser.mly"
           ( This )
# 1755 "parser.ml"
     in
    _menhir_goto_ident _menhir_env _menhir_stack _menhir_s _v

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (
# 44 "parser.mly"
      (Ast.ident)
# 1767 "parser.ml"
    ) = 
# 140 "parser.mly"
            ( Super )
# 1771 "parser.ml"
     in
    _menhir_goto_ident _menhir_env _menhir_stack _menhir_s _v

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 17 "parser.mly"
       (string)
# 1778 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s : (
# 17 "parser.mly"
       (string)
# 1786 "parser.ml"
    )) = _v in
    let _v : (
# 37 "parser.mly"
      (Ast.expression)
# 1791 "parser.ml"
    ) = 
# 147 "parser.mly"
              ( StringCste s )
# 1795 "parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (
# 46 "parser.mly"
      (Ast.instruction)
# 1807 "parser.ml"
    ) = 
# 180 "parser.mly"
           ( Return )
# 1811 "parser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (
# 44 "parser.mly"
      (Ast.ident)
# 1823 "parser.ml"
    ) = 
# 141 "parser.mly"
             ( Result )
# 1827 "parser.ml"
     in
    _menhir_goto_ident _menhir_env _menhir_stack _menhir_s _v

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDCLASS _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CSTE _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | ID _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | IDCLASS _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | NEW ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | RESULT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | STR _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
            | SUPER ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | THIS ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | UMINUS ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | RPAREN ->
                _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState19
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState20 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CSTE _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | ID _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | IDCLASS _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | LPAREN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | NEW ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | RESULT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | STR _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
        | SUPER ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | THIS ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | UMINUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState44
        | CONCATE | DIV | DOT | MINUS | PLUS | RELOP _ | RPAREN | TIMES ->
            _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44)
    | IDCLASS _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | NEW ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | RESULT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | STR _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _v
    | SUPER ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | THIS ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | UMINUS ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run53 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | ID _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | IDCLASS _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | NEW ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | RESULT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | STR _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState53 _v
    | SUPER ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | THIS ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | UMINUS ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState53
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 25 "parser.mly"
       (string)
# 1983 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CSTE _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | ID _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | IDCLASS _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | LPAREN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | NEW ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | RESULT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | STR _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState22 _v
        | SUPER ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | THIS ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | UMINUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | RPAREN ->
            _menhir_reduce41 _menhir_env (Obj.magic _menhir_stack) MenhirState22
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 5 "parser.mly"
       (string)
# 2031 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce29 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (int)
# 2041 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 7 "parser.mly"
       (int)
# 2049 "parser.ml"
    )) = _v in
    let _v : (
# 37 "parser.mly"
      (Ast.expression)
# 2054 "parser.ml"
    ) = 
# 146 "parser.mly"
              ( IntCste n )
# 2058 "parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_declaration_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.declaration list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CSTE _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | ID _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | IDCLASS _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | IF ->
            _menhir_run53 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | LPAREN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | NEW ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | RESULT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | RETURN ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | STR _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | SUPER ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | THIS ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | UMINUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | RCBR ->
            _menhir_reduce43 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (
# 39 "parser.mly"
      (Ast.declaration)
# 2107 "parser.ml"
        ))), _, (xs : (Ast.declaration list))) = _menhir_stack in
        let _v : (Ast.declaration list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 2112 "parser.ml"
         in
        _menhir_goto_list_declaration_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EXTENDS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | IDCLASS _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (c : (
# 25 "parser.mly"
       (string)
# 2132 "parser.ml"
                )) = _v in
                let _1 = () in
                let _v : (string) = 
# 96 "parser.mly"
                              ( c )
# 2138 "parser.ml"
                 in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (x : (string)) = _v in
                let _v : (string option) = 
# 116 "<standard.mly>"
    ( Some x )
# 2146 "parser.ml"
                 in
                _menhir_goto_option_extends_ _menhir_env _menhir_stack _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | IS | LCBR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _v : (string option) = 
# 114 "<standard.mly>"
    ( None )
# 2159 "parser.ml"
             in
            _menhir_goto_option_extends_ _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEF ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | RCBR ->
            _menhir_reduce45 _menhir_env (Obj.magic _menhir_stack) MenhirState86
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState86)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | COLON ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDCLASS _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (c : (
# 25 "parser.mly"
       (string)
# 2203 "parser.ml"
                    )) = _v in
                    let _1 = () in
                    let _v : (string) = 
# 135 "parser.mly"
                      ( c )
# 2209 "parser.ml"
                     in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    assert (not _menhir_env._menhir_error);
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | ASSIGN ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | LCBR ->
                            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
                    | IS ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, (x : (string))) = _menhir_stack in
                        let _v : (string option) = 
# 116 "<standard.mly>"
    ( Some x )
# 2233 "parser.ml"
                         in
                        _menhir_goto_option_returned_type_ _menhir_env _menhir_stack _v
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | IS ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _v : (string option) = 
# 114 "<standard.mly>"
    ( None )
# 2252 "parser.ml"
                 in
                _menhir_goto_option_returned_type_ _menhir_env _menhir_stack _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_boption_AUTO_ : _menhir_env -> 'ttv_tail -> (bool) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (typevar : (
# 5 "parser.mly"
       (string)
# 2295 "parser.ml"
                )) = _v in
                let (((_menhir_stack, _menhir_s), (a : (bool))), (n : (
# 5 "parser.mly"
       (string)
# 2300 "parser.ml"
                ))) = _menhir_stack in
                let _4 = () in
                let _1 = () in
                let _v : (
# 39 "parser.mly"
      (Ast.declaration)
# 2307 "parser.ml"
                ) = 
# 74 "parser.mly"
                                                    (
        {
            name = n;
            class_type = typevar;
            is_auto = a;
        }
    )
# 2317 "parser.ml"
                 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | VAR ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState68
                | CSTE _ | DEF | EXTENDS | ID _ | IDCLASS _ | IF | IS | LCBR | LPAREN | NEW | RCBR | RESULT | RETURN | RPAREN | STR _ | SUPER | THIS | UMINUS ->
                    _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState68
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.declaration list) = 
# 211 "<standard.mly>"
    ( [] )
# 2356 "parser.ml"
     in
    _menhir_goto_list_declaration_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AUTO ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (bool) = 
# 135 "<standard.mly>"
    ( true )
# 2374 "parser.ml"
         in
        _menhir_goto_boption_AUTO_ _menhir_env _menhir_stack _v
    | ID _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (bool) = 
# 133 "<standard.mly>"
    ( false )
# 2382 "parser.ml"
         in
        _menhir_goto_boption_AUTO_ _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | VAR ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | CSTE _ | ID _ | IDCLASS _ | IF | LPAREN | NEW | RCBR | RESULT | RETURN | STR _ | SUPER | THIS | UMINUS ->
        _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_goto_boption_OBJECT_ : _menhir_env -> 'ttv_tail -> (bool) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDCLASS _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | VAR ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | EXTENDS | IS | LCBR ->
                _menhir_reduce39 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_list_classe_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.class_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LCBR ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (
# 38 "parser.mly"
      (Ast.class_def)
# 2472 "parser.ml"
        ))), _, (xs : (Ast.class_def list))) = _menhir_stack in
        let _v : (Ast.class_def list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 2477 "parser.ml"
         in
        _menhir_goto_list_classe_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_boption_CLASS_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (bool) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | OBJECT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _1 = () in
        let _v : (bool) = 
# 135 "<standard.mly>"
    ( true )
# 2498 "parser.ml"
         in
        _menhir_goto_boption_OBJECT_ _menhir_env _menhir_stack _v
    | IDCLASS _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (bool) = 
# 133 "<standard.mly>"
    ( false )
# 2506 "parser.ml"
         in
        _menhir_goto_boption_OBJECT_ _menhir_env _menhir_stack _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState102 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState86 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState82 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState78 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState62 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState55 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState37 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (bool) = 
# 133 "<standard.mly>"
    ( false )
# 2691 "parser.ml"
     in
    _menhir_goto_boption_CLASS_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce37 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.class_def list) = 
# 211 "<standard.mly>"
    ( [] )
# 2700 "parser.ml"
     in
    _menhir_goto_list_classe_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (bool) = 
# 135 "<standard.mly>"
    ( true )
# 2712 "parser.ml"
     in
    _menhir_goto_boption_CLASS_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 61 "parser.mly"
       (Ast.prog)
# 2731 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLASS ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LCBR ->
        _menhir_reduce37 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IDCLASS _ | OBJECT ->
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 2759 "parser.ml"
