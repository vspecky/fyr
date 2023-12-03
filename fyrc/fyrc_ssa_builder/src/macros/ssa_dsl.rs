#[macro_export]
#[doc(hidden)]
macro_rules! __dsl_expr {
    ($b:ident,$v:ident (+ $lhs:tt $rhs:tt)) => {{
        let a = $crate::__dsl_expr!($b,$v $lhs);
        let b = $crate::__dsl_expr!($b,$v $rhs);
        $b.ins().add(a, b).expect("add ins")
    }};

    ($b:ident,$v:ident (- $lhs:tt $rhs:tt)) => {{
        let a = $crate::__dsl_expr!($b,$v $lhs);
        let b = $crate::__dsl_expr!($b,$v $rhs);
        $b.ins().sub(a, b).expect("sub ins")
    }};

    ($b:ident,$v:ident $num:literal) => {
        $b.ins().const32($num).expect("const32 ins")
    };

    ($b:ident,$v:ident $var:ident) => {{
        let var_name = stringify!($var);
        let var = $v.get(&var_name)
            .copied()
            .expect(&format!("variable '{var_name}' not defined"));

        $b.use_variable(var)
            .expect(&format!("couldn't use '{var_name}' variable"))
    }};
}

pub use __dsl_expr;

#[macro_export]
#[doc(hidden)]
macro_rules! __dsl_block {
    ($b:ident,$v:ident $(,$lh:ident,$lo:ident)? ()) => {};

    ($b:ident,$v:ident $(,$lh:ident,$lo:ident)? ($(($($in:tt)+))+)) => {
        $crate::__dsl_block!($b,$v $(,$lh,$lo)? $(($($in)+))+)
    };

    ($b:ident,$v:ident $(,$lh:ident,$lo:ident)? (ret $var:ident) $($rest:tt)*) => {{
        let var_val = $crate::__dsl_expr!($b,$v $var);
        $b.ins().ret(Some(var_val)).expect("ret ins");
    }};

    ($b:ident,$v:ident $(,$lh:ident,$lo:ident)? ($($in:tt)+) $($rest:tt)+) => {
        $crate::__dsl_stmt!($b,$v $(,$lh,$lo)? $($in)+);
        $crate::__dsl_block!($b,$v $(,$lh,$lo)? $($rest)+)
    };

    ($b:ident,$v:ident $(,$lh:ident,$lo:ident)? ($($in:tt)+)) => {
        $crate::__dsl_stmt!($b,$v $(,$lh,$lo)? $($in)+)
    };
}

pub use __dsl_block;

#[doc(hidden)]
macro_rules! __dsl_block2__ {
    ($b:ident,$v:ident $(,$lh:ident,$lo:ident)? ($(($($in:tt)+))+)) => {
        $(__dsl_stmt!($b,$v $(,$lh,$lo)? $($in)+))+
    };

    ($b:ident,$v:ident $(,$lh:ident,$lo:ident)? ($($in:tt)+)) => {
        __dsl_stmt!($b,$v $(,$lh,$lo)? $($in)+)
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! __dsl_stmt {
    ($b:ident,$v:ident $(,$lh:ident,$lo:ident)? let $var:ident $expr:tt) => {{
        let var_name: &'static str = stringify!($var);
        if $v.get(&var_name).is_some() {
            panic!("variable '{var_name}' already defined");
        }

        let value = $crate::__dsl_expr!($b,$v $expr);
        let var = $b.declare_variable(var_name.to_string(), ValueType::Int32)
            .expect(&format!("failed to declare variable '{var_name}'"));

        $b.define_variable(
            var,
            value,
        )
        .expect(&format!("failed variable '{var_name}' definition"));

        $v.insert(var_name, var);
    }};

    ($b:ident,$v:ident $(,$lh:ident,$lo:ident)? mut $var:ident $expr:tt) => {{
        let var_name: &'static str = stringify!($var);
        let var = $v.get(&var_name)
            .copied()
            .expect(&format!("variable '{var_name}' not defined for mutation"));
        let value = $crate::__dsl_expr!($b,$v $expr);

        $b.define_variable(var, value)
            .expect(&format!("failed variable '{var_name}' mutation"));
    }};

    ($b:ident,$v:ident $(,$lh:ident,$lo:ident)? if $lhs:tt $rhs:tt $thenb:tt $elseb:tt) => {{
        let lval = $crate::__dsl_expr!($b,$v $lhs);
        let rval = $crate::__dsl_expr!($b,$v $rhs);
        $b.ins().cmps(lval, rval).expect("cmps ins");
        let thenb = $b.make_block().expect("block make");
        let elseb = $b.make_block().expect("block make");
        let contb = $b.make_block().expect("block make");
        $b.ins().brs(instr::Cond::Equal, thenb, elseb).expect("brs ins");

        $b.seal_block(thenb).expect("block seal");
        $b.seal_block(elseb).expect("block seal");

        {
            $b.switch_to_block(thenb).expect("block switch");

            #[allow(unused)]
            let mut vars = $v.clone();
            $crate::__dsl_block!($b,vars $(,$lh,$lo)? $thenb);

            if !$b.is_current_block_filled().expect("block filled check") {
                $b.ins().jmp(contb).expect("jmp ins");
            }
        }
        {
            $b.switch_to_block(elseb).expect("block switch");

            #[allow(unused)]
            let mut vars = $v.clone();
            $crate::__dsl_block!($b,vars $(,$lh,$lo)? $elseb);

            if !$b.is_current_block_filled().expect("block filled check") {
                $b.ins().jmp(contb).expect("jmp ins");
            }
        }

        $b.switch_to_block(contb).expect("block switch");
        $b.seal_block(contb).expect("block seal");
    }};

    ($b:ident,$v:ident $(,$lh:ident,$lo:ident)? while $lhs:tt $rhs:tt $body:tt) => {{
        let headb = $b.make_block().expect("block make");
        let loopb = $b.make_block().expect("block make");
        let contb = $b.make_block().expect("block make");
        $b.ins().jmp(headb).expect("jmp ins");
        $b.switch_to_block(headb).expect("block switch");

        let lval = $crate::__dsl_expr!($b,$v $lhs);
        let rval = $crate::__dsl_expr!($b,$v $rhs);
        $b.ins().cmps(lval, rval).expect("cmps ins");
        $b.ins().brs(instr::Cond::Equal, loopb, contb).expect("brs ins");
        $b.switch_to_block(loopb).expect("block switch");
        $b.seal_block(loopb).expect("block seal");

        #[allow(unused)]
        let mut vars = $v.clone();

        $crate::__dsl_block!($b,vars,headb,contb $body);

        if !$b.is_current_block_filled().expect("block filled check") {
            $b.ins().jmp(headb).expect("jmp ins");
        }
        $b.seal_block(headb).expect("block seal");
        $b.seal_block(contb).expect("block seal");
        $b.switch_to_block(contb).expect("block switch");
    }};

    ($b:ident,$v:ident $(,$lh:ident,$lo:ident)? break) => {
        $($b.ins().jmp($lo).expect("jmp ins");)?
    };

    ($b:ident,$v:ident $(,$lh:ident,$lo:ident)? continue) => {
        $($b.ins().jmp($lh).expect("jmp ins");)?
    };
}

pub use __dsl_stmt;

#[macro_export]
macro_rules! ssa_dsl {
    (($(($($tok:tt)+))+)) => {{
        use std::collections::HashMap;
        #[allow(unused_imports)]
        use $crate::__private::fyrc_ssa::{
            instr,
            function,
            value::ValueType,
            variable::Variable,
        };

        let isa = function::InstructionSet::Thumb;
        let mut module = $crate::module::SsaModule::construct(isa);
        let mut b = module.build_function(module.get_main_function()).expect("get builder");

        #[allow(unused)]
        let mut vars = HashMap::<&'static str, Variable>::new();

        $crate::__dsl_block!(b,vars ($(($($tok)+))+));

        b.finalize().expect("finalization");
        module.get_main_function_data()
            .expect("main function")
            .clone()
    }};
}
