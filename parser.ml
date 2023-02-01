
module MenhirBasics = struct
  
  exception Error
  
  let _eRR : exn =
    Error
  
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
# 20 "parser.ml"
  )
    | SEMICOLON
    | RPAREN
    | RETURN
    | RESULT
    | RELOP of (
# 8 "parser.mly"
       (Ast.opComp)
# 29 "parser.ml"
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
# 44 "parser.ml"
  )
    | ID of (
# 5 "parser.mly"
       (string)
# 49 "parser.ml"
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
# 60 "parser.ml"
  )
    | CONCATE
    | COLON
    | CLASS
    | AUTO
    | ASSIGN
  
end

include MenhirBasics

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState105
  | MenhirState103
  | MenhirState100
  | MenhirState94
  | MenhirState89
  | MenhirState88
  | MenhirState85
  | MenhirState81
  | MenhirState75
  | MenhirState71
  | MenhirState69
  | MenhirState65
  | MenhirState64
  | MenhirState63
  | MenhirState60
  | MenhirState58
  | MenhirState57
  | MenhirState56
  | MenhirState50
  | MenhirState48
  | MenhirState47
  | MenhirState45
  | MenhirState44
  | MenhirState43
  | MenhirState42
  | MenhirState41
  | MenhirState40
  | MenhirState38
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

# 128 "parser.ml"

let rec _menhir_goto_methode : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.method_def) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEF ->
        _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | RCBR ->
        _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState105
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState105

and _menhir_reduce15 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ident) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (id : (Ast.ident))) = _menhir_stack in
    let _v : (Ast.expression) = 
# 148 "parser.mly"
                 ( Ident(id) )
# 152 "parser.ml"
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
# 172 "parser.ml"
            ))), _, (le : (Ast.expression list))) = _menhir_stack in
            let _v : (Ast.expression) = 
# 156 "parser.mly"
                                                         ( NewInstance(id , le) )
# 177 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s, (a : (Ast.expression))), _), _, (n : (Ast.ident))), _, (le : (Ast.expression list))) = _menhir_stack in
            let _v : (Ast.expression) = 
# 161 "parser.mly"
                                                                       ( EnvoiMsg(a,n,le) )
# 199 "parser.ml"
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
        let ((_menhir_stack, _menhir_s, (x : (Ast.expression))), _, (xs : (Ast.expression list))) = _menhir_stack in
        let _v : (Ast.expression list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 215 "parser.ml"
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
# 230 "parser.ml"
            ))), _, (le : (Ast.expression list))) = _menhir_stack in
            let _v : (Ast.expression) = 
# 155 "parser.mly"
                                                             ( NewInstance(id , le) )
# 235 "parser.ml"
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

and _menhir_run29 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
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

and _menhir_run31 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> (
# 8 "parser.mly"
       (Ast.opComp)
# 281 "parser.ml"
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

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | ID _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | IDCLASS _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NEW ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | RESULT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | STR _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | SUPER ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | THIS ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | UMINUS ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
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
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run33 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
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

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | ID _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | IDCLASS _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NEW ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | RESULT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | STR _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | SUPER ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | THIS ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | UMINUS ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run38 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | ID _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | IDCLASS _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | NEW ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | RESULT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | STR _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
    | SUPER ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | THIS ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | UMINUS ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState38
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38

and _menhir_goto_list_methode_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.method_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.method_def))), _, (xs : (Ast.method_def list))) = _menhir_stack in
        let _v : (Ast.method_def list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 467 "parser.ml"
         in
        _menhir_goto_list_methode_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RCBR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _, (ld : (Ast.declaration list))), _, (lm : (Ast.method_def list))) = _menhir_stack in
            let _v : (Ast.block_class) = 
# 105 "parser.mly"
                                                                  (
   {  declarations = ld ;
    methodes = lm ;
    }
)
# 487 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (bb : (Ast.block_class)) = _v in
            let ((((((_menhir_stack, _menhir_s, (c : (bool))), (o : (bool))), (n : (
# 25 "parser.mly"
       (string)
# 495 "parser.ml"
            ))), _, (lp : (Ast.declaration list))), (s : (string option))), _, (b : (Ast.block option))) = _menhir_stack in
            let _v : (Ast.class_def) = 
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
# 510 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CLASS ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | LCBR ->
                _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | IDCLASS _ | OBJECT ->
                _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState75
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75)
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
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | RPAREN ->
                _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState94
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState94)
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
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | DEF | RCBR ->
                _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88)
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
            let _v : (Ast.block) = 
# 98 "parser.mly"
                                                                        (
    {
        declarations = ld;
        instructions = instrs;
    }
)
# 635 "parser.ml"
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
                    let ((_menhir_stack, _menhir_s, (ld : (Ast.class_def list))), _, (bl : (Ast.block))) = _menhir_stack in
                    let _v : (Ast.prog) = 
# 66 "parser.mly"
                                     ( 
        {
                classes = ld ;
                block = bl ;
        }                                      
    )
# 656 "parser.ml"
                     in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_1 : (Ast.prog)) = _v in
                    Obj.magic _1
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState100 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((((_menhir_stack, _menhir_s), (o : (bool))), (n : (
# 5 "parser.mly"
       (string)
# 674 "parser.ml"
                ))), _, (lp : (Ast.declaration list))), (r : (string))), _, (b : (Ast.block))) = _menhir_stack in
                let _v : (Ast.method_def) = 
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
# 687 "parser.ml"
                 in
                _menhir_goto_methode _menhir_env _menhir_stack _menhir_s _v
            | MenhirState103 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((((_menhir_stack, _menhir_s), (o : (bool))), (n : (
# 5 "parser.mly"
       (string)
# 696 "parser.ml"
                ))), _, (lp : (Ast.declaration list))), (r : (string option))), _, (b : (Ast.block))) = _menhir_stack in
                let _v : (Ast.method_def) = 
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
# 709 "parser.ml"
                 in
                _menhir_goto_methode _menhir_env _menhir_stack _menhir_s _v
            | MenhirState85 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (Ast.block))) = _menhir_stack in
                let _v : (Ast.block option) = 
# 116 "<standard.mly>"
    ( Some x )
# 719 "parser.ml"
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
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.instruction))), _, (xs : (Ast.instruction list))) = _menhir_stack in
        let _v : (Ast.instruction list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 737 "parser.ml"
         in
        _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.instruction) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState58 ->
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
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | ID _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | IDCLASS _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
            | IF ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | NEW ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | RESULT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState60
            | RETURN ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState60
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
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), _, (si : (Ast.expression))), _), _, (alors : (Ast.instruction))), _, (sinon : (Ast.instruction))) = _menhir_stack in
        let _v : (Ast.instruction) = 
# 179 "parser.mly"
                                                                       (Ite(si,alors,sinon))
# 798 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState69 | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CSTE _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | ID _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | IDCLASS _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | IF ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | LPAREN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | NEW ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | RESULT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | RETURN ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | STR _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
        | SUPER ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | THIS ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | UMINUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | RCBR ->
            _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState69
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69)
    | _ ->
        _menhir_fail ()

and _menhir_goto_ident : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ident) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState63 | MenhirState56 | MenhirState12 | MenhirState19 | MenhirState20 | MenhirState47 | MenhirState28 | MenhirState44 | MenhirState42 | MenhirState40 | MenhirState38 | MenhirState35 | MenhirState31 | MenhirState29 | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        _menhir_reduce15 _menhir_env (Obj.magic _menhir_stack)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
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
            | RPAREN ->
                _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
        | CONCATE | CSTE _ | DIV | DOT | ELSE | ID _ | IDCLASS _ | IF | MINUS | NEW | PLUS | RCBR | RELOP _ | RESULT | RETURN | RPAREN | SEMICOLON | STR _ | SUPER | THEN | THIS | TIMES | UMINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (a : (Ast.expression))), _), _, (i : (Ast.ident))) = _menhir_stack in
            let _v : (Ast.expression) = 
# 160 "parser.mly"
                                   ( Access(a,i) )
# 888 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState11 | MenhirState69 | MenhirState58 | MenhirState60 ->
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
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | ID _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | IDCLASS _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | NEW ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | RESULT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | STR _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | SUPER ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | THIS ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | UMINUS ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
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

and _menhir_reduce42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.expression list) = 
# 211 "<standard.mly>"
    ( [] )
# 947 "parser.ml"
     in
    _menhir_goto_list_expression_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce30 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 5 "parser.mly"
       (string)
# 954 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (n : (
# 5 "parser.mly"
       (string)
# 960 "parser.ml"
    ))) = _menhir_stack in
    let _v : (Ast.ident) = 
# 138 "parser.mly"
             ( Local n )
# 965 "parser.ml"
     in
    _menhir_goto_ident _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState19 | MenhirState28 | MenhirState35 | MenhirState22 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCATE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | CSTE _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | ID _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | IDCLASS _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
        | LPAREN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | NEW ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState28
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState28
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
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState28
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
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState30
        | RELOP _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
        | CSTE _ | DIV | ELSE | ID _ | IDCLASS _ | IF | LPAREN | MINUS | NEW | PLUS | RCBR | RESULT | RETURN | RPAREN | SEMICOLON | STR _ | SUPER | THEN | THIS | TIMES | UMINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (a : (Ast.expression))), _), _, (b : (Ast.expression))) = _menhir_stack in
            let _v : (Ast.expression) = 
# 151 "parser.mly"
                                                      ( Times(a,b) )
# 1035 "parser.ml"
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
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32
        | CSTE _ | DIV | ELSE | ID _ | IDCLASS _ | IF | LPAREN | MINUS | NEW | PLUS | RCBR | RELOP _ | RESULT | RETURN | RPAREN | SEMICOLON | STR _ | SUPER | THEN | THIS | TIMES | UMINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (a : (Ast.expression))), _, (op : (
# 8 "parser.mly"
       (Ast.opComp)
# 1056 "parser.ml"
            ))), _, (b : (Ast.expression))) = _menhir_stack in
            let _v : (Ast.expression) = 
# 159 "parser.mly"
                                               (Compo(op,a,b))
# 1061 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (a : (Ast.expression))), _), _, (b : (Ast.expression))) = _menhir_stack in
        let _v : (Ast.expression) = 
# 153 "parser.mly"
                                            ( Concate(a,b) )
# 1075 "parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCATE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | RELOP _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState41
        | CSTE _ | ELSE | ID _ | IDCLASS _ | IF | LPAREN | MINUS | NEW | PLUS | RCBR | RESULT | RETURN | RPAREN | SEMICOLON | STR _ | SUPER | THEN | THIS | UMINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (a : (Ast.expression))), _), _, (b : (Ast.expression))) = _menhir_stack in
            let _v : (Ast.expression) = 
# 149 "parser.mly"
                                                      ( Plus(a,b) )
# 1099 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCATE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState43
        | RELOP _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
        | CSTE _ | DIV | ELSE | ID _ | IDCLASS _ | IF | LPAREN | MINUS | NEW | PLUS | RCBR | RESULT | RETURN | RPAREN | SEMICOLON | STR _ | SUPER | THEN | THIS | TIMES | UMINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (a : (Ast.expression))), _), _, (b : (Ast.expression))) = _menhir_stack in
            let _v : (Ast.expression) = 
# 152 "parser.mly"
                                                  (Div(a,b))
# 1123 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCATE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | RELOP _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | CSTE _ | ELSE | ID _ | IDCLASS _ | IF | LPAREN | MINUS | NEW | PLUS | RCBR | RESULT | RETURN | RPAREN | SEMICOLON | STR _ | SUPER | THEN | THIS | UMINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (a : (Ast.expression))), _), _, (b : (Ast.expression))) = _menhir_stack in
            let _v : (Ast.expression) = 
# 150 "parser.mly"
                                                      ( Minus(a,b) )
# 1151 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCATE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | RELOP _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState48 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (tipe : (
# 5 "parser.mly"
       (string)
# 1183 "parser.ml"
            ))), _, (e : (Ast.expression))) = _menhir_stack in
            let _v : (Ast.expression) = 
# 157 "parser.mly"
                                             ( Cast(tipe , e) )
# 1188 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState48
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
    | MenhirState20 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCATE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | RELOP _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState50 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expression))) = _menhir_stack in
            let _v : (Ast.expression) = 
# 158 "parser.mly"
                                     ( e )
# 1223 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (e : (Ast.expression))) = _menhir_stack in
        let _v : (Ast.expression) = 
# 154 "parser.mly"
                            ( Unary(e) )
# 1239 "parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCATE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | RELOP _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState57 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CSTE _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | ID _v ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | IDCLASS _v ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | IF ->
                _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | LPAREN ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | NEW ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | RESULT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | RETURN ->
                _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | STR _v ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
            | SUPER ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | THIS ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | UMINUS ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState58
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState57
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCATE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | RELOP _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState64
        | CSTE _ | ELSE | ID _ | IDCLASS _ | IF | LPAREN | NEW | RCBR | RESULT | RETURN | STR _ | SUPER | THIS | UMINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (n : (Ast.ident))), _, (r : (Ast.expression))) = _menhir_stack in
            let _v : (Ast.instruction) = 
# 178 "parser.mly"
                                    (Aff(n,r))
# 1325 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64)
    | MenhirState11 | MenhirState69 | MenhirState58 | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONCATE ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | DIV ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | DOT ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | MINUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | PLUS ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | RELOP _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState65 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (n : (Ast.expression))) = _menhir_stack in
            let _v : (Ast.instruction) = 
# 176 "parser.mly"
                             ( Exp(n) )
# 1358 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState65
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65)
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
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState103
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.method_def list) = 
# 211 "<standard.mly>"
    ( [] )
# 1400 "parser.ml"
     in
    _menhir_goto_list_methode_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run90 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | OVERRIDE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (bool) = 
# 135 "<standard.mly>"
    ( true )
# 1417 "parser.ml"
         in
        _menhir_goto_boption_OVERRIDE_ _menhir_env _menhir_stack _v
    | ID _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (bool) = 
# 133 "<standard.mly>"
    ( false )
# 1425 "parser.ml"
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
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | IS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState85 in
        let _v : (Ast.block option) = 
# 114 "<standard.mly>"
    ( None )
# 1450 "parser.ml"
         in
        _menhir_goto_option_block_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_reduce44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.instruction list) = 
# 211 "<standard.mly>"
    ( [] )
# 1463 "parser.ml"
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
    let _v : (Ast.ident) = 
# 139 "parser.mly"
           ( This )
# 1505 "parser.ml"
     in
    _menhir_goto_ident _menhir_env _menhir_stack _menhir_s _v

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.ident) = 
# 140 "parser.mly"
            ( Super )
# 1516 "parser.ml"
     in
    _menhir_goto_ident _menhir_env _menhir_stack _menhir_s _v

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 17 "parser.mly"
       (string)
# 1523 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s : (
# 17 "parser.mly"
       (string)
# 1531 "parser.ml"
    )) = _v in
    let _v : (Ast.expression) = 
# 147 "parser.mly"
              ( StringCste s )
# 1536 "parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run55 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.instruction) = 
# 180 "parser.mly"
           ( Return )
# 1547 "parser.ml"
     in
    _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.ident) = 
# 141 "parser.mly"
             ( Result )
# 1558 "parser.ml"
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
                _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState19
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
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | ID _v ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | IDCLASS _v ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | LPAREN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | NEW ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | RESULT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | STR _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | SUPER ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | THIS ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | UMINUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState47
        | CONCATE | DIV | DOT | MINUS | PLUS | RELOP _ | RPAREN | TIMES ->
            _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
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

and _menhir_run56 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | ID _v ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | IDCLASS _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | LPAREN ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | NEW ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | RESULT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | STR _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | SUPER ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | THIS ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | UMINUS ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 25 "parser.mly"
       (string)
# 1714 "parser.ml"
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
            _menhir_reduce42 _menhir_env (Obj.magic _menhir_stack) MenhirState22
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
# 1762 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (int)
# 1772 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 7 "parser.mly"
       (int)
# 1780 "parser.ml"
    )) = _v in
    let _v : (Ast.expression) = 
# 146 "parser.mly"
              ( IntCste n )
# 1785 "parser.ml"
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
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | LPAREN ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | NEW ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | RESULT ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | RETURN ->
            _menhir_run55 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | STR _v ->
            _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | SUPER ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | THIS ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | UMINUS ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | RCBR ->
            _menhir_reduce44 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.declaration))), _, (xs : (Ast.declaration list))) = _menhir_stack in
        let _v : (Ast.declaration list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 1835 "parser.ml"
         in
        _menhir_goto_list_declaration_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState81 ->
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
# 1855 "parser.ml"
                )) = _v in
                let _v : (string) = 
# 96 "parser.mly"
                              ( c )
# 1860 "parser.ml"
                 in
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (x : (string)) = _v in
                let _v : (string option) = 
# 116 "<standard.mly>"
    ( Some x )
# 1868 "parser.ml"
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
# 1881 "parser.ml"
             in
            _menhir_goto_option_extends_ _menhir_env _menhir_stack _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEF ->
            _menhir_run90 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | RCBR ->
            _menhir_reduce46 _menhir_env (Obj.magic _menhir_stack) MenhirState89
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
    | MenhirState94 ->
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
# 1925 "parser.ml"
                    )) = _v in
                    let _v : (string) = 
# 135 "parser.mly"
                      ( c )
# 1930 "parser.ml"
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
                            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState100
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
                    | IS ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, (x : (string))) = _menhir_stack in
                        let _v : (string option) = 
# 116 "<standard.mly>"
    ( Some x )
# 1954 "parser.ml"
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
# 1973 "parser.ml"
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
# 2016 "parser.ml"
                )) = _v in
                let (((_menhir_stack, _menhir_s), (a : (bool))), (n : (
# 5 "parser.mly"
       (string)
# 2021 "parser.ml"
                ))) = _menhir_stack in
                let _v : (Ast.declaration) = 
# 74 "parser.mly"
                                                    (
        {
            name = n;
            class_type = typevar;
            is_auto = a;
        }
    )
# 2032 "parser.ml"
                 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | VAR ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | CSTE _ | DEF | EXTENDS | ID _ | IDCLASS _ | IF | IS | LCBR | LPAREN | NEW | RCBR | RESULT | RETURN | RPAREN | STR _ | SUPER | THIS | UMINUS ->
                    _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState71
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71)
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

and _menhir_reduce40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.declaration list) = 
# 211 "<standard.mly>"
    ( [] )
# 2071 "parser.ml"
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
        let _v : (bool) = 
# 135 "<standard.mly>"
    ( true )
# 2088 "parser.ml"
         in
        _menhir_goto_boption_AUTO_ _menhir_env _menhir_stack _v
    | ID _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (bool) = 
# 133 "<standard.mly>"
    ( false )
# 2096 "parser.ml"
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
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
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
        _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState4
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
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | EXTENDS | IS | LCBR ->
                _menhir_reduce40 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
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
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.class_def))), _, (xs : (Ast.class_def list))) = _menhir_stack in
        let _v : (Ast.class_def list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 2187 "parser.ml"
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
        let _v : (bool) = 
# 135 "<standard.mly>"
    ( true )
# 2207 "parser.ml"
         in
        _menhir_goto_boption_OBJECT_ _menhir_env _menhir_stack _v
    | IDCLASS _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _v : (bool) = 
# 133 "<standard.mly>"
    ( false )
# 2215 "parser.ml"
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
    | MenhirState105 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState103 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState94 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState88 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState64 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState56 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
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
# 2404 "parser.ml"
     in
    _menhir_goto_boption_CLASS_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce38 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.class_def list) = 
# 211 "<standard.mly>"
    ( [] )
# 2413 "parser.ml"
     in
    _menhir_goto_list_classe_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (bool) = 
# 135 "<standard.mly>"
    ( true )
# 2424 "parser.ml"
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

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.prog) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLASS ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LCBR ->
        _menhir_reduce38 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IDCLASS _ | OBJECT ->
        _menhir_reduce5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 2466 "parser.ml"
