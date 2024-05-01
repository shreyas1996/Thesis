structure RT_Int = RT_Int;

structure SIMPLE_CALC =
    struct
        fun increment'1FD_ n'26B_ = (R_coverage.cancel(RT_Text.fromLit "../project/tests/rsl_test_cases/SIMPLE_CALC.rsl", (6, 25)); RT_Int.R_add (n'26B_, RT_Int.fromLit "1"));
        
        val number'199_ = RT_Int.fromLit "10";
        
    end;
    
open SIMPLE_CALC;

RSL.print_load_errs();
RSL.set_time();
R_coverage.init();
(R_coverage.mark(RT_Text.fromLit "../project/tests/rsl_test_cases/SIMPLE_CALC.rsl", (6, 25), (7, 7)));
RSL.print_error_count();
R_coverage.save_marked();
