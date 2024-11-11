use fyrc_utils::DenseMap;

use crate::func::{MachFunc, MachFuncData};

pub struct MachModule {
    funcs: DenseMap<MachFunc, MachFuncData>,
    main: MachFunc,
}

impl MachModule {
    pub fn new(main_data: MachFuncData) -> Self {
        let mut funcs = DenseMap::new();
        let main = funcs.insert(main_data);

        Self { funcs, main }
    }

    pub fn get_all_funcs(&self) -> impl Iterator<Item = MachFunc> {
        self.funcs.keys()
    }

    pub fn get_func(&mut self, func: MachFunc) -> Option<&MachFuncData> {
        self.funcs.get(func)
    }

    pub fn get_func_mut(&mut self, func: MachFunc) -> Option<&mut MachFuncData> {
        self.funcs.get_mut(func)
    }

    pub fn iter_funcs(&self) -> impl Iterator<Item = (MachFunc, &MachFuncData)> {
        self.funcs.iter()
    }

    pub fn iter_funcs_mut(&mut self) -> impl Iterator<Item = (MachFunc, &mut MachFuncData)> {
        self.funcs.iter_mut()
    }
}
