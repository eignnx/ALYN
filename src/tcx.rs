use crate::ty::Ty;
use internment::Intern;
use std::collections::HashMap;

pub struct Tcx {
    maps: Vec<HashMap<Intern<String>, SymData>>,
}

impl Tcx {
    pub fn new(current_subr_ret_ty: Ty) -> Self {
        Self {
            maps: vec![HashMap::new()],
        }
    }

    const SUBR_RET_TY: &str = "<subr_ret_ty>";

    pub fn set_subr_ret_ty(&mut self, ty: Ty) {
        self.insert(Intern::from_ref(Self::SUBR_RET_TY), SymData { ty });
    }

    pub fn get_subr_ret_ty(&self) -> Option<&Ty> {
        self.get(&Intern::from_ref(Self::SUBR_RET_TY))
            .map(|data| &data.ty)
    }

    pub fn get(&self, sym: &Intern<String>) -> Option<&SymData> {
        for map in self.maps.iter().rev() {
            if let Some(data) = map.get(sym) {
                return Some(data);
            }
        }
        None
    }

    #[track_caller]
    pub fn insert(&mut self, sym: Intern<String>, data: SymData) -> Option<SymData> {
        let last = self.maps.last_mut().unwrap();
        last.insert(sym, data)
    }

    pub fn enter_scope(&mut self) {
        self.maps.push(HashMap::new());
    }

    #[track_caller]
    pub fn exit_scope(&mut self) -> HashMap<Intern<String>, SymData> {
        self.maps.pop().unwrap()
    }
}

#[derive(Debug)]
pub struct SymData {
    pub ty: Ty,
}
