#[doc(hidden)]
macro_rules! __instr__ {
    ($builder:ident: #vdef $var:ident $val:ident) => {
        $builder.define_variable($var, $val)
            .expect("var def");
    };

    ($builder:ident: #block $name:ident) => {
        let $name = $builder.make_block().expect("make block");
    };

    ($builder:ident: #seal $block:ident) => {
        $builder.seal_block($block).expect("block seal");
    };

    ($builder:ident: #switch $block:ident) => {
        $builder.switch_to_block($block).expect("switch block");
    };

    ($builder:ident: #vdecl $res:ident $name:literal::$var_type:ident) => {
        let $res = $builder.declare_variable(
            $name.to_string(),
            $crate::ssa::ValueType::$var_type
        )
        .expect("var decl");
    };

    ($builder:ident: #vuse $var:ident $($res:ident)?) => {
        $(let $res =)? $builder.use_variable($var).expect("var use");
    };

    ($builder:ident: $res:ident = $ins:ident $($arg:expr),+) => {
        let $res = $builder.ins().$ins($($arg),+).expect("instruction addition");
    };

    ($builder:ident: brs $cond:ident, $then:ident, $else:ident) => {
        $builder
            .ins()
            .brs($crate::ssa::instr::Cond::$cond, $then, $else)
            .expect("brs instr");
    };

    ($builder:ident: $ins:ident $($arg:expr),+) => {
        $builder.ins().$ins($($arg),+).expect("instruction addition");
    };
}

#[allow(unused_imports)]
pub(crate) use __instr__;

#[macro_export]
macro_rules! build_function {
    ($(($($tree:tt)+))+) => {{
        let isa = $crate::ssa::function::InstructionSet::Thumb;
        let mut module = $crate::ssa::module::SsaModule::construct(isa);
        let mut b = module.build_function(module.get_main_function()).expect("get builder");

        $(
            $crate::ssa::macros::__instr__!(b:$($tree)+);
        )+

        b.func_data.clone()
    }};
}

#[doc(hidden)]
macro_rules! __dsl_expr__ {
    ($b:ident,$v:ident (+ $lhs:tt $rhs:tt)) => {{
        let a = __dsl_expr__!($b,$v $lhs);
        let b = __dsl_expr__!($b,$v $rhs);
        $b.ins().add(a, b).expect("add ins")
    }};

    ($b:ident,$v:ident (- $lhs:tt $rhs:tt)) => {{
        let a = __dsl_expr__!($b,$v $lhs);
        let b = __dsl_expr__!($b,$v $rhs);
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

#[doc(hidden)]
macro_rules! __dsl_block__ {
    ($b:ident,$v:ident $(,$lh:ident,$lo:ident)? ($(($($in:tt)+))+)) => {
        __dsl_block__!($b,$v $(,$lh,$lo)? $(($($in)+))+)
    };

    ($b:ident,$v:ident $(,$lh:ident,$lo:ident)? ($($in:tt)+) $($rest:tt)+) => {
        __dsl_stmt__!($b,$v $(,$lh,$lo)? $($in)+);
        __dsl_block__!($b,$v $(,$lh,$lo)? $($rest)+)
    };

    ($b:ident,$v:ident $(,$lh:ident,$lo:ident)? ($($in:tt)+)) => {
        __dsl_stmt__!($b,$v $(,$lh,$lo)? $($in)+)
    };
}

#[doc(hidden)]
macro_rules! __dsl_block2__ {
    ($b:ident,$v:ident $(,$lh:ident,$lo:ident)? ($(($($in:tt)+))+)) => {
        $(__dsl_stmt__!($b,$v $(,$lh,$lo)? $($in)+))+
    };

    ($b:ident,$v:ident $(,$lh:ident,$lo:ident)? ($($in:tt)+)) => {
        __dsl_stmt__!($b,$v $(,$lh,$lo)? $($in)+)
    };
}

#[doc(hidden)]
macro_rules! __dsl_stmt__ {
    ($b:ident,$v:ident $(,$lh:ident,$lo:ident)? let $var:ident $expr:tt) => {{
        let var_name: &'static str = stringify!($var);
        if $v.get(&var_name).is_some() {
            panic!("variable '{var_name}' already defined");
        }

        let value = __dsl_expr__!($b,$v $expr);
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
        let value = __dsl_expr__!($b,$v $expr);

        $b.define_variable(var, value)
            .expect(&format!("failed variable '{var_name}' mutation"));
    }};

    ($b:ident,$v:ident $(,$lh:ident,$lo:ident)? if $lhs:tt $rhs:tt $thenb:tt $elseb:tt) => {{
        let lval = __dsl_expr__!($b,$v $lhs);
        let rval = __dsl_expr__!($b,$v $rhs);
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
            __dsl_block__!($b,vars $(,$lh,$lo)? $thenb);

            if !$b.is_current_block_filled().expect("block filled check") {
                $b.ins().jmp(contb).expect("jmp ins");
            }
        }
        {
            $b.switch_to_block(elseb).expect("block switch");

            #[allow(unused)]
            let mut vars = $v.clone();
            __dsl_block__!($b,vars $(,$lh,$lo)? $elseb);

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

        let lval = __dsl_expr__!($b,$v $lhs);
        let rval = __dsl_expr__!($b,$v $rhs);
        $b.ins().cmps(lval, rval).expect("cmps ins");
        $b.ins().brs(instr::Cond::Equal, loopb, contb).expect("brs ins");
        $b.switch_to_block(loopb).expect("block switch");

        #[allow(unused)]
        let mut vars = $v.clone();

        __dsl_block__!($b,vars,headb,contb $body);

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

macro_rules! ssa_dsl {
    (($(($($tok:tt)+))+)) => {{
        use std::collections::HashMap;
        #[allow(unused_imports)]
        use $crate::ssa::{self, instr, function, macros::*, module, ValueType, Variable};

        let isa = function::InstructionSet::Thumb;
        let mut module = module::SsaModule::construct(isa);
        let mut b = module.build_function(module.get_main_function()).expect("get builder");

        #[allow(unused)]
        let mut vars = HashMap::<&'static str, Variable>::new();

        __dsl_block__!(b,vars ($(($($tok)+))+));

        b.func_data.clone()
    }};
}

fn test() {
    fn test_func() {}

    let _data = ssa_dsl!(
        ((let hello (+ 5 6))
         (let never (+ 5 (- hello 7)))
         (if hello never
           (mut hello 7)
           (let kello 9))
         (while hello never
           (if hello never
             (while hello never
               (let zello 9))
             (while hello never
               (let xello 10)))))
    );
}

