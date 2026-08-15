use std::collections::HashMap;

use crate::analysis::types::{BuiltIn, KindData};

use super::types::{Type, TypeInfo};

/// Represents analyzed types, and can answer queries about program types.
/// We use this instead of just a `HashMap` because queries can ask about types
/// that have not yet been seen, such as T* (for known T).
pub struct TypeStore {
    types: HashMap<Type, TypeInfo>,
}

impl TypeStore {
    pub fn new() -> TypeStore {
        TypeStore { types: HashMap::new() }
    }

    /// "Basic Info" comprises info about the object itself, not anything it contains or allows access to.
    /// Succeeds even when parts of the type are unknown, like when T* is incomplete.
    pub fn get_basic_info(&self, a_type: &Type) -> Option<&TypeInfo> {
        // We are leaning on constant promotion here to make a &'static from TypeInfo, which relies on TypeInfo
        // being a pretty simple (constant + some other restrictions) type.
        Some(match a_type {
            Type::BuiltIn(BuiltIn::U8 | BuiltIn::I8 | BuiltIn::Boolean) => {
                &TypeInfo { size: 1, alignment: 1, kind: KindData::BuiltIn }
            }
            Type::BuiltIn(BuiltIn::U16 | BuiltIn::I16) => &TypeInfo { size: 2, alignment: 2, kind: KindData::BuiltIn },
            Type::BuiltIn(BuiltIn::U32 | BuiltIn::I32) => &TypeInfo { size: 4, alignment: 4, kind: KindData::BuiltIn },
            Type::BuiltIn(BuiltIn::U64 | BuiltIn::I64) => &TypeInfo { size: 8, alignment: 8, kind: KindData::BuiltIn },
            Type::BuiltIn(BuiltIn::Unit) => {
                // Not sure if this should have an alignment
                &TypeInfo { size: 0, alignment: 1, kind: KindData::BuiltIn }
            }
            Type::BuiltIn(BuiltIn::Bottom) => &TypeInfo { size: 0, alignment: 1, kind: KindData::BuiltIn },
            Type::Pointer(_) => {
                // I guess the Kind is a built in? Maybe we should call the other built in "Primitive".
                &TypeInfo { size: 8, alignment: 8, kind: KindData::BuiltIn }
            }
            _ => return self.types.get(a_type),
        })
    }

    /// Defines a new user type.
    pub fn define(&mut self, name: String, info: TypeInfo) {
        self.types.insert(Type::UserDefined(name), info);
    }
}
