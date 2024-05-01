structure RT_Int = RT_Int;

structure RT_Bool = RT_Bool;

structure CONDITIONAL_TEST =
    struct
        fun check'199_ x'203_ = (R_coverage.cancel(RT_Text.fromLit "../project/tests/rsl_test_cases/CONDITIONAL_TEST.rsl", (5, 21)); if RT_Int.R_gt (x'203_, RT_Int.fromLit "10") then (R_coverage.cancel(RT_Text.fromLit "../project/tests/rsl_test_cases/CONDITIONAL_TEST.rsl", (5, 36)); true) else (R_coverage.cancel(RT_Text.fromLit "../project/tests/rsl_test_cases/CONDITIONAL_TEST.rsl", (5, 46)); false));
        
    end;
    
open CONDITIONAL_TEST;

RSL.print_load_errs();
RSL.set_time();
R_coverage.init();
(R_coverage.mark(RT_Text.fromLit "../project/tests/rsl_test_cases/CONDITIONAL_TEST.rsl", (5, 46), (5, 52));
R_coverage.mark(RT_Text.fromLit "../project/tests/rsl_test_cases/CONDITIONAL_TEST.rsl", (5, 36), (5, 44));
R_coverage.mark(RT_Text.fromLit "../project/tests/rsl_test_cases/CONDITIONAL_TEST.rsl", (5, 21), (6, 7)));
RSL.print_error_count();
R_coverage.save_marked();
