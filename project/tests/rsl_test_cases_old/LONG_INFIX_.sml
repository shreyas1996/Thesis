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
    
structure LONG_INFIX =
    struct
        fun complexCalc'199_ (x'209_, y'20C_) = (R_coverage.cancel(RT_Text.fromLit "../project/tests/rsl_test_cases/LONG_INFIX.rsl", (5, 30)); RT_Int.R_add ((RT_Int.R_mul ((RT_Int.R_add (x'209_, RT_Int.fromLit "2")), (RT_Int.R_sub (y'20C_, RT_Int.fromLit "3")))), RT_Int.R_div (RT_Int.fromLit "5", (RT_Int.R_add (x'209_, y'20C_)))));
        
    end;
    
open LONG_INFIX;

RSL.print_load_errs();
RSL.set_time();
R_coverage.init();
(R_coverage.mark(RT_Text.fromLit "../project/tests/rsl_test_cases/LONG_INFIX.rsl", (5, 30), (6, 7)));
RSL.print_error_count();
R_coverage.save_marked();
