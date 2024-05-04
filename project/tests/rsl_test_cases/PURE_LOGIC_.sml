structure RT_Bool = RT_Bool;

structure RT_x_1 =
    struct
        type t = RT_Bool.t * RT_Bool.t;
        
        fun equ (x:t, y:t) = RT_Bool.equ(#1 x, #1 y) andalso 
                             RT_Bool.equ(#2 x, #2 y);
        
        fun toString (x:t) = "(" ^
                             (RT_Bool.toString(#1 x )) ^ "," ^
                             (RT_Bool.toString(#2 x )) ^
                             ")";
        
        fun toStringSafe x = toString(x())
          handle RSL.RSL_exception s => (RSL.inc_exception_count(); s);
        
        fun typeToString () = "(" ^
                              (RT_Bool.typeToString ()) ^ " >< " ^
                              (RT_Bool.typeToString ()) ^
                              ")";
    end;
    
structure PURE_LOGIC =
    struct
        fun logicGate'199_ (a'207_, b'20A_) = (R_coverage.cancel(RT_Text.fromLit "PURE_LOGIC.rsl", (5, 28)); (not (((a'207_) orelse (R_coverage.cancel(RT_Text.fromLit "PURE_LOGIC.rsl", (5, 35)); b'20A_)))) andalso (R_coverage.cancel(RT_Text.fromLit "PURE_LOGIC.rsl", (5, 41)); ((a'207_) orelse (R_coverage.cancel(RT_Text.fromLit "PURE_LOGIC.rsl", (5, 47)); not (b'20A_)))));
        
    end;
    
open PURE_LOGIC;

RSL.print_load_errs();
RSL.set_time();
R_coverage.init();
(R_coverage.mark(RT_Text.fromLit "PURE_LOGIC.rsl", (5, 47), (5, 49));
R_coverage.mark(RT_Text.fromLit "PURE_LOGIC.rsl", (5, 35), (5, 36));
R_coverage.mark(RT_Text.fromLit "PURE_LOGIC.rsl", (5, 41), (6, 7));
R_coverage.mark(RT_Text.fromLit "PURE_LOGIC.rsl", (5, 28), (6, 7)));
RSL.print_error_count();
R_coverage.save_marked();
