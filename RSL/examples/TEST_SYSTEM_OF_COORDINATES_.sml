structure RT_Real = RT_Real;

structure RT_x_1 =
    struct
        type t = RT_Real.t * RT_Real.t;
        
        fun equ (x:t, y:t) = RT_Real.equ(#1 x, #1 y) andalso 
                             RT_Real.equ(#2 x, #2 y);
        
        fun toString (x:t) = "(" ^
                             (RT_Real.toString(#1 x )) ^ "," ^
                             (RT_Real.toString(#2 x )) ^
                             ")";
        
        fun toStringSafe x = toString(x())
          handle RSL.RSL_exception s => (RSL.inc_exception_count(); s);
        
        fun typeToString () = "(" ^
                              (RT_Real.typeToString ()) ^ " >< " ^
                              (RT_Real.typeToString ()) ^
                              ")";
    end;
    
structure RT_x_2 =
    struct
        type t = RT_x_1.t * RT_x_1.t;
        
        fun equ (x:t, y:t) = RT_x_1.equ(#1 x, #1 y) andalso 
                             RT_x_1.equ(#2 x, #2 y);
        
        fun toString (x:t) = "(" ^
                             (RT_x_1.toString(#1 x )) ^ "," ^
                             (RT_x_1.toString(#2 x )) ^
                             ")";
        
        fun toStringSafe x = toString(x())
          handle RSL.RSL_exception s => (RSL.inc_exception_count(); s);
        
        fun typeToString () = "(" ^
                              (RT_x_1.typeToString ()) ^ " >< " ^
                              (RT_x_1.typeToString ()) ^
                              ")";
    end;
    
structure RT_Bool = RT_Bool;

structure TEST_SYSTEM_OF_COORDINATES =
    struct
        type Position_ = RT_x_1.t;
        
        fun distance'83B_ ((x1'8A9_, y1'8AC_), (x2'8B1_, y2'8B4_)) = (R_coverage.cancel(RT_Text.fromLit "SYSTEM_OF_COORDINATES.rsl", (9, 9)); RT_Real.R_exp ((RT_Real.R_add (RT_Real.R_exp ((RT_Real.R_sub (x2'8B1_, x1'8A9_)), RT_Real.fromLit "2.0"), RT_Real.R_exp ((RT_Real.R_sub (y2'8B4_, y1'8AC_)), RT_Real.fromLit "2.0"))), RT_Real.fromLit "0.5"));
        
        val origin'7D7_ = (RT_Real.fromLit "0.0", RT_Real.fromLit "0.0");
        
        val pos3'32B_ = (RT_Real.fromLit "5.0", RT_Real.fromLit "1.0");
        
        val pos1'263_ = (RT_Real.fromLit "1.0", RT_Real.fromLit "1.0");
        
        val pos2'2C7_ = (RT_Real.fromLit "1.0", RT_Real.fromLit "5.0");
        
    end;
    
open TEST_SYSTEM_OF_COORDINATES;

RSL.print_load_errs();
RSL.set_time();
R_coverage.init();
(R_coverage.mark(RT_Text.fromLit "SYSTEM_OF_COORDINATES.rsl", (9, 9), (10, 5)));
(RSL.C_output "[t1] " RT_Bool.toStringSafe (fn _ => RT_Real.equ (((distance'83B_) (origin'7D7_, origin'7D7_)), RT_Real.fromLit "0.0")));

(RSL.C_output "[t2] " RT_Bool.toStringSafe (fn _ => RT_Real.equ (((distance'83B_) (pos1'263_, pos2'2C7_)), RT_Real.fromLit "4.0")));

(RSL.C_output "[t3] " RT_Bool.toStringSafe (fn _ => RT_Real.equ (((distance'83B_) (pos1'263_, pos3'32B_)), RT_Real.fromLit "4.0")));

(RSL.C_output "[t4] " RT_Real.toStringSafe (fn _ => ((distance'83B_) (pos1'263_, pos2'2C7_))));

RSL.print_error_count();
R_coverage.save_marked();
