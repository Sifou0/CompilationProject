
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
    | STR
    | SEMICOLON
    | RPAREN
    | RETURN
    | RESULT
    | RELOP of (
# 8 "parser.mly"
       (Ast.opComp)
# 25 "parser.ml"
  )
    | RCBR
    | RBRACKET
    | PLUS
    | OVERRIDE
    | OBJECT
    | NEW
    | MINUS
    | LPAREN
    | LCBR
    | LBRACKET
    | IS
    | IF
    | IDCLASS of (
# 25 "parser.mly"
       (string)
# 42 "parser.ml"
  )
    | ID of (
# 5 "parser.mly"
       (string)
# 47 "parser.ml"
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
# 58 "parser.ml"
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
  | MenhirState128
  | MenhirState125
  | MenhirState121
  | MenhirState117
  | MenhirState113
  | MenhirState111
  | MenhirState110
  | MenhirState107
  | MenhirState100
  | MenhirState97
  | MenhirState95
  | MenhirState93
  | MenhirState89
  | MenhirState87
  | MenhirState85
  | MenhirState81
  | MenhirState80
  | MenhirState79
  | MenhirState74
  | MenhirState72
  | MenhirState68
  | MenhirState67
  | MenhirState66
  | MenhirState63
  | MenhirState61
  | MenhirState60
  | MenhirState59
  | MenhirState58
  | MenhirState54
  | MenhirState52
  | MenhirState51
  | MenhirState49
  | MenhirState48
  | MenhirState47
  | MenhirState46
  | MenhirState45
  | MenhirState44
  | MenhirState42
  | MenhirState41
  | MenhirState40
  | MenhirState39
  | MenhirState31
  | MenhirState29
  | MenhirState28
  | MenhirState19
  | MenhirState18
  | MenhirState17
  | MenhirState16
  | MenhirState3
  | MenhirState0

# 1 "parser.mly"
  
    open Ast

# 133 "parser.ml"

let rec _menhir_goto_classe : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.class_def) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLASS ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | LCBR ->
        _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState128
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128

and _menhir_goto_methode : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.method_def) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEF ->
        _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | RCBR ->
        _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) MenhirState100
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100

and _menhir_reduce11 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ident) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (id : (Ast.ident))) = _menhir_stack in
    let _v : (Ast.expression) = 
# 191 "parser.mly"
                 ( Ident(id) )
# 173 "parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.ident) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (n : (
# 5 "parser.mly"
       (string)
# 189 "parser.ml"
        )) = _v in
        let (_menhir_stack, _menhir_s, (id : (Ast.ident))) = _menhir_stack in
        let _v : (Ast.expression) = 
# 197 "parser.mly"
                            ( Access(id , n) )
# 195 "parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_list_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expression list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState31 ->
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
# 221 "parser.ml"
            ))), _, (le : (Ast.expression list))) = _menhir_stack in
            let _v : (Ast.expression) = 
# 199 "parser.mly"
                                                         ( NewInstance(id , le) )
# 226 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.expression))), _, (xs : (Ast.expression list))) = _menhir_stack in
        let _v : (Ast.expression list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 242 "parser.ml"
         in
        _menhir_goto_list_expression_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState28 ->
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
# 257 "parser.ml"
            ))), _, (le : (Ast.expression list))) = _menhir_stack in
            let _v : (Ast.expression) = 
# 198 "parser.mly"
                                                             ( NewInstance(id , le) )
# 262 "parser.ml"
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

and _menhir_goto_instruction : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.instruction) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState61 ->
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
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | ID _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | IDCLASS _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | IF ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | NEW ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | SUPER ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | THIS ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | UMINUS ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((((_menhir_stack, _menhir_s), _, (si : (Ast.expression))), _), _, (alors : (Ast.instruction))), _, (sinon : (Ast.instruction))) = _menhir_stack in
        let _v : (Ast.instruction) = 
# 209 "parser.mly"
                                                                       (Ite(si,alors,sinon))
# 323 "parser.ml"
         in
        _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
    | MenhirState72 | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CSTE _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | ID _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | IDCLASS _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
        | IF ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | LPAREN ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | NEW ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | SUPER ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | THIS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | UMINUS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | RCBR ->
            _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState72
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72)
    | _ ->
        _menhir_fail ()

and _menhir_run40 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | ID _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | IDCLASS _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | SUPER ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | THIS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | UMINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState40
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40

and _menhir_run42 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> (
# 8 "parser.mly"
       (Ast.opComp)
# 388 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | ID _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | IDCLASS _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | SUPER ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | THIS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | UMINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | ID _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | IDCLASS _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | SUPER ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | THIS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | UMINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | ID _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | IDCLASS _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | SUPER ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | THIS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | UMINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * (Ast.expression) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | ID _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | IDCLASS _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | SUPER ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | THIS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | UMINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_goto_list_methode_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.method_def list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.method_def))), _, (xs : (Ast.method_def list))) = _menhir_stack in
        let _v : (Ast.method_def list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 508 "parser.ml"
         in
        _menhir_goto_list_methode_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RCBR ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (ld : (Ast.declaration list))), _, (lm : (Ast.method_def list))) = _menhir_stack in
            let _v : (Ast.block_class) = 
# 134 "parser.mly"
                                                                  (
   {  declarations = ld ;
    methodes = lm ;
    }
)
# 528 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState79 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | RBRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((((_menhir_stack, _menhir_s), (n : (
# 25 "parser.mly"
       (string)
# 544 "parser.ml"
                    ))), _, (lp : (Ast.declaration list))), _, (construc : (Ast.block))), _, (bl : (Ast.block_class))) = _menhir_stack in
                    let _v : (Ast.class_def) = 
# 115 "parser.mly"
                                                                                                                                                            ( {
                                                                name = n;
                                                                params = lp;
                                                                constructor = Some(construc);
                                                                content = bl;
                                                                is_class = true;
                                                                superclass = None;
                                                                 
                                                                } 
                                                             )
# 558 "parser.ml"
                     in
                    _menhir_goto_classe _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState107 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | RBRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((((_menhir_stack, _menhir_s), (n : (
# 25 "parser.mly"
       (string)
# 579 "parser.ml"
                    ))), _, (lp : (Ast.declaration list))), _, (bl : (Ast.block_class))) = _menhir_stack in
                    let _v : (Ast.class_def) = 
# 81 "parser.mly"
                                                                                                      ( {
                                                                name = n;
                                                                params = lp;
                                                                superclass = None;
                                                                content = bl;
                                                                constructor = None;
                                                                is_class = true;
                                                           
                                                                            } 
                                                             )
# 593 "parser.ml"
                     in
                    _menhir_goto_classe _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState113 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | RBRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((((((_menhir_stack, _menhir_s), (n : (
# 25 "parser.mly"
       (string)
# 614 "parser.ml"
                    ))), _, (ld : (Ast.declaration list))), _, (spr_class : (Ast.expression))), _), _, (bl : (Ast.block_class))) = _menhir_stack in
                    let _v : (Ast.class_def) = 
# 92 "parser.mly"
                                                                                                                                      ( 
                                                            {
                                                                name = n;
                                                                params = ld;
                                                                superclass = Some(spr_class);
                                                                constructor = None;
                                                                content = bl;
                                                                is_class = true;
                                                             
                                                                            } 
                                                            )
# 629 "parser.ml"
                     in
                    _menhir_goto_classe _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState121 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | RBRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (((((((_menhir_stack, _menhir_s), (n : (
# 25 "parser.mly"
       (string)
# 650 "parser.ml"
                    ))), _, (ld : (Ast.declaration list))), _, (spr_class : (Ast.expression))), _, _), _, (construc : (Ast.block))), _, (bl : (Ast.block_class))) = _menhir_stack in
                    let _v : (Ast.class_def) = 
# 103 "parser.mly"
                                                                                                                                                                             ( 
                                                            {
                                                                name = n;
                                                                params = ld;
                                                                superclass = Some(spr_class);
                                                                constructor = Some(construc);
                                                                content = bl;
                                                                is_class = true;
                                                              
                                                            } 
                                                        )
# 665 "parser.ml"
                     in
                    _menhir_goto_classe _menhir_env _menhir_stack _menhir_s _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_instruction_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.instruction list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState18 ->
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
# 126 "parser.mly"
                                                                        (
    {
        declarations = ld;
        instructions = instrs;

    }
)
# 708 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            (match _menhir_s with
            | MenhirState16 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | RBRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | IS ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | LBRACKET ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | LCBR ->
                                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState79
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79)
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
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState89 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((((_menhir_stack, _menhir_s), (n : (
# 5 "parser.mly"
       (string)
# 762 "parser.ml"
                ))), _, (lp : (Ast.declaration list))), _), (cls : (
# 25 "parser.mly"
       (string)
# 766 "parser.ml"
                ))), _, (bl : (Ast.block))) = _menhir_stack in
                let _v : (Ast.method_def) = 
# 172 "parser.mly"
            (
                {
                    name = n;
                    params = lp;
                    return_type = Some(cls) ;
                    content = bl;
                    is_override = true;
                }
            )
# 779 "parser.ml"
                 in
                _menhir_goto_methode _menhir_env _menhir_stack _menhir_s _v
            | MenhirState87 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), (n : (
# 5 "parser.mly"
       (string)
# 788 "parser.ml"
                ))), _, (lp : (Ast.declaration list))), _, (bl : (Ast.block))) = _menhir_stack in
                let _v : (Ast.method_def) = 
# 162 "parser.mly"
            (
                {
                    name = n;
                    params = lp;
                    return_type = None;
                    content = bl;
                    is_override = true;
                }
            )
# 801 "parser.ml"
                 in
                _menhir_goto_methode _menhir_env _menhir_stack _menhir_s _v
            | MenhirState97 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((((_menhir_stack, _menhir_s), (n : (
# 5 "parser.mly"
       (string)
# 810 "parser.ml"
                ))), _, (lp : (Ast.declaration list))), _), (cls : (
# 25 "parser.mly"
       (string)
# 814 "parser.ml"
                ))), _, (bl : (Ast.block))) = _menhir_stack in
                let _v : (Ast.method_def) = 
# 141 "parser.mly"
            (
                {
                    name = n;
                    params = lp;
                    return_type = Some(cls);
                    content = bl ;
                    is_override = false;
                }

            )
# 828 "parser.ml"
                 in
                _menhir_goto_methode _menhir_env _menhir_stack _menhir_s _v
            | MenhirState95 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), (n : (
# 5 "parser.mly"
       (string)
# 837 "parser.ml"
                ))), _, (lp : (Ast.declaration list))), _, (bl : (Ast.block))) = _menhir_stack in
                let _v : (Ast.method_def) = 
# 152 "parser.mly"
            (
                {
                    name = n;
                    params = lp;
                    return_type = None ;
                    content = bl;
                    is_override = false ;
                }
            )
# 850 "parser.ml"
                 in
                _menhir_goto_methode _menhir_env _menhir_stack _menhir_s _v
            | MenhirState117 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | RBRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | IS ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | LBRACKET ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | LCBR ->
                                _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState121
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
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
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | MenhirState125 ->
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | EOF ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let ((_menhir_stack, _menhir_s, (ld : (Ast.class_def list))), _, (bl : (Ast.block))) = _menhir_stack in
                    let _v : (Ast.prog) = 
# 62 "parser.mly"
                                       ( 
    {
             classes = ld ;
             block = bl ;
    } 
                                         
 )
# 915 "parser.ml"
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
            | _ ->
                _menhir_fail ())
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.instruction))), _, (xs : (Ast.instruction list))) = _menhir_stack in
        let _v : (Ast.instruction list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 942 "parser.ml"
         in
        _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_ident : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.ident) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState110 | MenhirState66 | MenhirState59 | MenhirState19 | MenhirState28 | MenhirState29 | MenhirState51 | MenhirState39 | MenhirState48 | MenhirState46 | MenhirState44 | MenhirState42 | MenhirState40 | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | CSTE _ | DIV | ELSE | ID _ | IDCLASS _ | IF | IS | LPAREN | MINUS | NEW | PLUS | RCBR | RELOP _ | RPAREN | SEMICOLON | SUPER | THEN | THIS | TIMES | UMINUS ->
            _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState18 | MenhirState72 | MenhirState61 | MenhirState63 ->
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
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
            | ID _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
            | IDCLASS _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | NEW ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | SUPER ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | THIS ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | UMINUS ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState66
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66)
        | DOT ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DIV | MINUS | PLUS | RELOP _ | SEMICOLON | TIMES ->
            _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_reduce32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.expression list) = 
# 211 "<standard.mly>"
    ( [] )
# 1015 "parser.ml"
     in
    _menhir_goto_list_expression_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce10 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 5 "parser.mly"
       (string)
# 1022 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (s : (
# 5 "parser.mly"
       (string)
# 1028 "parser.ml"
    ))) = _menhir_stack in
    let _v : (Ast.expression) = 
# 190 "parser.mly"
             ( StringCste s )
# 1033 "parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expression) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState28 | MenhirState39 | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CSTE _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
        | DIV ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | ID _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
        | IDCLASS _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
        | LPAREN ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | NEW ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | RELOP _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState39 _v
        | SUPER ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | THIS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | TIMES ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | UMINUS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | RPAREN ->
            _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack) MenhirState39
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState39)
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RELOP _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
        | CSTE _ | DIV | ELSE | ID _ | IDCLASS _ | IF | IS | LPAREN | MINUS | NEW | PLUS | RCBR | RPAREN | SEMICOLON | SUPER | THEN | THIS | TIMES | UMINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (a : (Ast.expression))), _), _, (b : (Ast.expression))) = _menhir_stack in
            let _v : (Ast.expression) = 
# 194 "parser.mly"
                                          ( Times(a,b) )
# 1091 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, (a : (Ast.expression))), _, (op : (
# 8 "parser.mly"
       (Ast.opComp)
# 1104 "parser.ml"
        ))), _, (b : (Ast.expression))) = _menhir_stack in
        let _v : (Ast.expression) = 
# 202 "parser.mly"
                                               (Compo(op,a,b))
# 1109 "parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | RELOP _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
        | TIMES ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState45
        | CSTE _ | ELSE | ID _ | IDCLASS _ | IF | IS | LPAREN | MINUS | NEW | PLUS | RCBR | RPAREN | SEMICOLON | SUPER | THEN | THIS | UMINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (a : (Ast.expression))), _), _, (b : (Ast.expression))) = _menhir_stack in
            let _v : (Ast.expression) = 
# 192 "parser.mly"
                                           ( Plus(a,b) )
# 1129 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RELOP _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
        | CSTE _ | DIV | ELSE | ID _ | IDCLASS _ | IF | IS | LPAREN | MINUS | NEW | PLUS | RCBR | RPAREN | SEMICOLON | SUPER | THEN | THIS | TIMES | UMINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (a : (Ast.expression))), _), _, (b : (Ast.expression))) = _menhir_stack in
            let _v : (Ast.expression) = 
# 195 "parser.mly"
                                        (Div(a,b))
# 1149 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | RELOP _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
        | TIMES ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | CSTE _ | ELSE | ID _ | IDCLASS _ | IF | IS | LPAREN | MINUS | NEW | PLUS | RCBR | RPAREN | SEMICOLON | SUPER | THEN | THIS | UMINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (a : (Ast.expression))), _), _, (b : (Ast.expression))) = _menhir_stack in
            let _v : (Ast.expression) = 
# 193 "parser.mly"
                                          ( Minus(a,b) )
# 1173 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | RELOP _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState52 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (tipe : (
# 5 "parser.mly"
       (string)
# 1201 "parser.ml"
            ))), _, (e : (Ast.expression))) = _menhir_stack in
            let _v : (Ast.expression) = 
# 200 "parser.mly"
                                             ( Cast(tipe , e) )
# 1206 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState52
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | RELOP _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState54 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expression))) = _menhir_stack in
            let _v : (Ast.expression) = 
# 201 "parser.mly"
                                     ( e )
# 1237 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState54
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RELOP _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
        | CSTE _ | DIV | ELSE | ID _ | IDCLASS _ | IF | IS | LPAREN | MINUS | NEW | PLUS | RCBR | RPAREN | SEMICOLON | SUPER | THEN | THIS | TIMES | UMINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expression))) = _menhir_stack in
            let _v : (Ast.expression) = 
# 196 "parser.mly"
                            ( Unary(e) )
# 1259 "parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | RELOP _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState60 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CSTE _v ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
            | ID _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
            | IDCLASS _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
            | IF ->
                _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | NEW ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | SUPER ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | THIS ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | UMINUS ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState61
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61)
        | TIMES ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState60
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60)
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | RELOP _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
        | TIMES ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState67
        | CSTE _ | ELSE | ID _ | IDCLASS _ | IF | LPAREN | NEW | RCBR | SUPER | THIS | UMINUS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (n : (Ast.ident))), _, (r : (Ast.expression))) = _menhir_stack in
            let _v : (Ast.instruction) = 
# 208 "parser.mly"
                                    (Aff(n,r))
# 1335 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67)
    | MenhirState18 | MenhirState72 | MenhirState61 | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | RELOP _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState68 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (n : (Ast.expression))) = _menhir_stack in
            let _v : (Ast.instruction) = 
# 206 "parser.mly"
                             ( Exp(n) )
# 1364 "parser.ml"
             in
            _menhir_goto_instruction _menhir_env _menhir_stack _menhir_s _v
        | TIMES ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState68
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68)
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DIV ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState111 in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LCBR ->
                    _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState117
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | IS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState111 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | LBRACKET ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LCBR ->
                    _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState113
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MINUS ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | PLUS ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | RELOP _v ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _v
        | TIMES ->
            _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState111
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111)
    | _ ->
        _menhir_fail ()

and _menhir_reduce36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.method_def list) = 
# 211 "<standard.mly>"
    ( [] )
# 1448 "parser.ml"
     in
    _menhir_goto_list_methode_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run82 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
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
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | RPAREN ->
                _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | OVERRIDE ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | RPAREN ->
                    _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState85
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85)
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
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce34 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.instruction list) = 
# 211 "<standard.mly>"
    ( [] )
# 1531 "parser.ml"
     in
    _menhir_goto_list_instruction_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run59 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | ID _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | IDCLASS _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | SUPER ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | THIS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | UMINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run80 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | VAR ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | DEF | RCBR ->
        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState80
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState80

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | ID _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | IDCLASS _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | SUPER ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | THIS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | UMINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (x : (
# 5 "parser.mly"
       (string)
# 1622 "parser.ml"
            )) = _v in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _v : (Ast.ident) = 
# 184 "parser.mly"
                     ( This(x) )
# 1628 "parser.ml"
             in
            _menhir_goto_ident _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ID _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (x : (
# 5 "parser.mly"
       (string)
# 1662 "parser.ml"
            )) = _v in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _v : (Ast.ident) = 
# 185 "parser.mly"
                      ( Super(x) )
# 1668 "parser.ml"
             in
            _menhir_goto_ident _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | ID _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | IDCLASS _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | NEW ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | SUPER ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | THIS ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | UMINUS ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | RPAREN ->
                _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
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

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CSTE _v ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState29 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CSTE _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | ID _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | IDCLASS _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
        | LPAREN ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | NEW ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | SUPER ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | THIS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | UMINUS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState51
        | DIV | MINUS | PLUS | RELOP _ | RPAREN | TIMES ->
            _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51)
    | IDCLASS _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | NEW ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | SUPER ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | THIS ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | UMINUS ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState29
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 25 "parser.mly"
       (string)
# 1793 "parser.ml"
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
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
        | ID _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
        | IDCLASS _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
        | LPAREN ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | NEW ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | SUPER ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | THIS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | UMINUS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | RPAREN ->
            _menhir_reduce32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 5 "parser.mly"
       (string)
# 1837 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (int)
# 1847 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (n : (
# 7 "parser.mly"
       (int)
# 1855 "parser.ml"
    )) = _v in
    let _v : (Ast.expression) = 
# 189 "parser.mly"
              ( IntCste n )
# 1860 "parser.ml"
     in
    _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | VAR ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | CSTE _ | ID _ | IDCLASS _ | IF | LPAREN | NEW | RCBR | SUPER | THIS | UMINUS ->
        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState17
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_goto_list_declaration_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.declaration list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EXTENDS ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | CSTE _v ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
                | ID _v ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
                | IDCLASS _v ->
                    _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState110 _v
                | LPAREN ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | NEW ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | SUPER ->
                    _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | THIS ->
                    _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | UMINUS ->
                    _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState110
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState110)
            | IS ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | LBRACKET ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | LCBR ->
                        _menhir_run80 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | LPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | RPAREN ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | LBRACKET ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | LCBR ->
                            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState16
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
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
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CSTE _v ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | ID _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | IDCLASS _v ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState18 _v
        | IF ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | LPAREN ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | NEW ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | SUPER ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | THIS ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | UMINUS ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | RCBR ->
            _menhir_reduce34 _menhir_env (Obj.magic _menhir_stack) MenhirState18
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18)
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.declaration))), _, (xs : (Ast.declaration list))) = _menhir_stack in
        let _v : (Ast.declaration list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 2026 "parser.ml"
         in
        _menhir_goto_list_declaration_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DEF ->
            _menhir_run82 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | RCBR ->
            _menhir_reduce36 _menhir_env (Obj.magic _menhir_stack) MenhirState81
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81)
    | MenhirState85 ->
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
                let _menhir_s = MenhirState87 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDCLASS _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | LCBR ->
                        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState89
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | LCBR ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState87
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState93 ->
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
                let _menhir_s = MenhirState95 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | IDCLASS _v ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | LCBR ->
                        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | LCBR ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState95
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_declaration : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.declaration) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | VAR ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | CSTE _ | DEF | ID _ | IDCLASS _ | IF | LPAREN | NEW | RCBR | RPAREN | SUPER | THIS | UMINUS ->
        _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState74
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

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
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125)
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Ast.class_def))), _, (xs : (Ast.class_def list))) = _menhir_stack in
        let _v : (Ast.class_def list) = 
# 213 "<standard.mly>"
    ( x :: xs )
# 2177 "parser.ml"
         in
        _menhir_goto_list_classe_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.declaration list) = 
# 211 "<standard.mly>"
    ( [] )
# 2188 "parser.ml"
     in
    _menhir_goto_list_declaration_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AUTO ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
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
# 2221 "parser.ml"
                    )) = _v in
                    let ((_menhir_stack, _menhir_s), (n : (
# 5 "parser.mly"
       (string)
# 2226 "parser.ml"
                    ))) = _menhir_stack in
                    let _v : (Ast.declaration) = 
# 75 "parser.mly"
                                       ( {
        name = n;
        class_type = typevar;
        is_auto = true;
    } )
# 2235 "parser.ml"
                     in
                    _menhir_goto_declaration _menhir_env _menhir_stack _menhir_s _v
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
                let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
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
# 2274 "parser.ml"
                )) = _v in
                let ((_menhir_stack, _menhir_s), (n : (
# 5 "parser.mly"
       (string)
# 2279 "parser.ml"
                ))) = _menhir_stack in
                let _v : (Ast.declaration) = 
# 70 "parser.mly"
                                            ( {
        name = n;
        class_type = typevar;
        is_auto = false;
    } )
# 2288 "parser.ml"
                 in
                _menhir_goto_declaration _menhir_env _menhir_stack _menhir_s _v
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
            let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState117 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState113 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState111 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState110 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState107 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState97 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState95 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState80 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState74 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState72 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState68 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState66 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState60 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState58 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState44 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState40 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState39 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState18 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce28 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.class_def list) = 
# 211 "<standard.mly>"
    ( [] )
# 2518 "parser.ml"
     in
    _menhir_goto_list_classe_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
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
            | VAR ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | RPAREN ->
                _menhir_reduce30 _menhir_env (Obj.magic _menhir_stack) MenhirState3
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
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
        _menhir_reduce28 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "<standard.mly>"
  

# 2596 "parser.ml"
