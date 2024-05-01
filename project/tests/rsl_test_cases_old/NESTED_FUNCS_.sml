structure RT_Int = RT_Int;

structure RT_x_1 =
    struct
        type t = RT_Int.t * RT_Int.t;
        
        fun equ (x:t, y:t) = RT_Int.equ(#1 x, #1 y) andalso 
                             RT_Int.equ(#2 x, #2 y);
        
        fun toString (x:t) = "(" ^
                             (RT_Int.toString(#1 x )) ^ "," ^
                             (RT_Int.toString(#2 x )) ^
                             ")";
        
        fun toStringSafe x = toString(x())
          handle RSL.RSL_exception s => (RSL.inc_exception_count(); s);
        
        fun typeToString () = "(" ^
                              (RT_Int.typeToString ()) ^ " >< " ^
                              (RT_Int.typeToString ()) ^
                              ")";
    end;
    
structure NESTED_FUNCS =
    struct
        fun calculate'199_ (x'207_, y'20A_) = (R_coverage.cancel(RT_Text.fromLit "../project/tests/rsl_test_cases/NESTED_FUNCS.rsl", (5, 28)); RT_Int.R_add (RT_Int.R_mul (x'207_, y'20A_), RT_Int.fromLit "3"));
        
    end;
    
open NESTED_FUNCS;

RSL.print_load_errs();
RSL.set_time();
R_coverage.init();
(R_coverage.mark(RT_Text.fromLit "../project/tests/rsl_test_cases/NESTED_FUNCS.rsl", (5, 28), (6, 7)));
RSL.print_error_count();
R_coverage.save_marked();
