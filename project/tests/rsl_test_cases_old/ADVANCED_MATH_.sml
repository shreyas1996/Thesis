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
    
structure ADVANCED_MATH =
    struct
        fun compute'199_ (x'205_, y'208_) = (R_coverage.cancel(RT_Text.fromLit "../project/tests/rsl_test_cases/ADVANCED_MATH.rsl", (6, 11)); let
            val temp'2C9_ = ((RT_Int.R_mul (x'205_, y'208_)):RT_Int.t)
        in
            RT_Int.R_div (temp'2C9_, (RT_Int.R_add (x'205_, y'208_)))
        end);
        
    end;
    
open ADVANCED_MATH;

RSL.print_load_errs();
RSL.set_time();
R_coverage.init();
(R_coverage.mark(RT_Text.fromLit "../project/tests/rsl_test_cases/ADVANCED_MATH.rsl", (6, 11), (10, 7)));
RSL.print_error_count();
R_coverage.save_marked();
