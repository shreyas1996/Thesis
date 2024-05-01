structure RT_Int = RT_Int;

structure BRACKET_USAGE =
    struct
        fun simplify'199_ x'206_ = (R_coverage.cancel(RT_Text.fromLit "../project/tests/rsl_test_cases/BRACKET_USAGE.rsl", (5, 24)); (RT_Int.R_add (RT_Int.R_mul ((RT_Int.R_add (x'206_, RT_Int.fromLit "3")), (RT_Int.R_sub (x'206_, RT_Int.fromLit "5"))), RT_Int.fromLit "12")));
        
    end;
    
open BRACKET_USAGE;

RSL.print_load_errs();
RSL.set_time();
R_coverage.init();
(R_coverage.mark(RT_Text.fromLit "../project/tests/rsl_test_cases/BRACKET_USAGE.rsl", (5, 24), (6, 7)));
RSL.print_error_count();
R_coverage.save_marked();
