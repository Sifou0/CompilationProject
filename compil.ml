open Ast

let countEti = ref 0

let makeEti () = 
  let value = ! countEti in 
  let str_val string_of_int value in
  countEti := value + 1 ;


    let compileExpr e chan = 
        match e with
     
            Id x -> 
                output_string chan "PUSHS"; output_string chan (x);
                output_string chan "\n"
            | Cste v ->
                output_string chan "PUSHI "; output_string chan (string_of_int v);
                output_string chan "\n";
            | Plus(g, d) ->
                compileExpr g env; compileExpr d env; output_string chan "ADD\n";
            | Minus (g, d) ->
                compileExpr g env; compileExpr d env; output_string chan "SUB\n";
            | Times (g, d) ->
                compileExpr g env; compileExpr d env; output_string chan "MUL\n";
            | Div (g, d) ->
                compileExpr g env; compileExpr d env; output_string chan "DIV\n";
          
            | Compo(op, g, d) ->
                compileExpr g env; compileExpr d env;
                begin 
                    match op with
                        Eq ->   output_string chan "EQUAL\n"
                        | Neq ->  output_string chan "EQUAL\nNOT\n"
                        | Lt ->   output_string chan "INF\n"
                        | Le ->   output_string chan "INFEQ\n"
                        | Gt ->   output_string chan "SUP\n"
                        | Ge ->   output_string chan "SUPEQ\n"
                end
            
            | NewInstance (x, le) -> 
                compileExpr x;
                let rec f ld = 
                    match le with 
                        | [] -> None 
                        | a::b -> compileExpr a; f b;
            
            | StringCste x -> 
                compileExpr x;
                (*TO FINISH*)
            | Concate (e, e2) -> 
                compilExpr e env; compileExpr e2 env;
                output_string chan "CONCAT\n";
    in
    output_string chan "START\n";
    
    output_string chan "PUSHS \"Resultat: \"\nWRITES\nWRITEI\nPUSHS \"\\n\"\nWRITES\nSTOP\n";
    flush chan;
    close_out chan;;

