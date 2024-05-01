structure RT_u_Color__1 =
    struct
        datatype t = Red_
                   | Yellow_
                   | Blue_;
                   
        
        fun equ (Red_, Red_) = true
          | equ (Yellow_, Yellow_) = true
          | equ (Blue_, Blue_) = true
          | equ (_, _) = false;
        
        fun toString  (Red_) = "Red"
          | toString  (Yellow_) = "Yellow"
          | toString  (Blue_) = "Blue";
        
        fun toStringSafe x = toString(x())
          handle RSL.RSL_exception s => (RSL.inc_exception_count(); s);
        
        fun typeToString () = "Color";
    end;
    
structure COLORFUL =
    struct
        type Color_ = RT_u_Color__1.t;
        
        val favoriteColor'261_ = RT_u_Color__1.Red_;
        
    end;
    
open COLORFUL;

RSL.print_load_errs();
RSL.set_time();
R_coverage.init();
();
RSL.print_error_count();
R_coverage.save_marked();
