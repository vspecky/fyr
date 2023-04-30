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
