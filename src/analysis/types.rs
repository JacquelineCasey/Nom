use std::collections::HashMap;

use crate::{instructions::IntSize, CompilationEnvironment, CompileError};

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum Type {
    BuiltIn(BuiltIn),
    UserDefined(String),
    PartiallyKnown(PartialType),
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum BuiltIn {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    Unit,
    Boolean,
    Bottom, // The type of return expressions - this type is uninhabitted.
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum PartialType {
    IntLiteral, // The type of int literals. Will decay into i32 if nothing comes along to override it.
}

// Sometimes, with int types in particular, we need to decide what type is the smallest
// upper bound of two types. For instance, if we add an i8 and an i16, then the result
// is an i16.
pub fn upper_bound_type(left: &Type, right: &Type) -> Option<Type> {
    let (Type::BuiltIn(left), Type::BuiltIn(right)) = (left, right) else {
        return None;
    }; // Cannot unify non builtin.

    if type_fits(left, right) {
        Some(Type::BuiltIn(right.clone()))
    } else if type_fits(right, left) {
        Some(Type::BuiltIn(left.clone()))
    } else {
        None // TODO - try more types.
    }
}

fn type_fits(source: &BuiltIn, target: &BuiltIn) -> bool {
    if *source == BuiltIn::Bottom {
        return true;
    }

    match source {
        source if source.is_signed() => {
            target.is_signed()
                && source.get_int_size().expect("known") <= target.get_int_size().expect("known")
        }
        source if source.is_unsigned() => {
            if target.is_unsigned() || target.is_unsigned() {
                source.get_int_size().expect("known") <= target.get_int_size().expect("known")
            } else {
                false
            }
        }
        _ => false,
    }
}

impl BuiltIn {
    pub fn is_signed(&self) -> bool {
        use BuiltIn as B;

        matches!(self, B::I8 | B::I16 | B::I32 | B::I64)
    }

    pub fn is_unsigned(&self) -> bool {
        use BuiltIn as B;

        matches!(self, B::U8 | B::U16 | B::U32 | B::U64)
    }

    pub fn get_int_size(&self) -> Option<IntSize> {
        use BuiltIn as B;
        use IntSize as IS;

        match self {
            B::U8 | B::I8 => Some(IS::OneByte),
            B::U16 | B::I16 => Some(IS::TwoByte),
            B::U32 | B::I32 => Some(IS::FourByte),
            B::U64 | B::I64 => Some(IS::EightByte),
            _ => None,
        }
    }
}

impl From<String> for Type {
    fn from(value: String) -> Self {
        match &value[..] {
            "i8" => Type::BuiltIn(BuiltIn::I8),
            "i16" => Type::BuiltIn(BuiltIn::I16),
            "i32" => Type::BuiltIn(BuiltIn::I32),
            "i64" => Type::BuiltIn(BuiltIn::I64),
            "u8" => Type::BuiltIn(BuiltIn::U8),
            "u16" => Type::BuiltIn(BuiltIn::U16),
            "u32" => Type::BuiltIn(BuiltIn::U32),
            "u64" => Type::BuiltIn(BuiltIn::U64),
            "unit" => Type::BuiltIn(BuiltIn::Unit),
            "bool" => Type::BuiltIn(BuiltIn::Boolean),
            other => Type::UserDefined(other.to_string()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeInfo {
    pub size: usize,      // Number of bytes the types takes on the stack.
    pub alignment: usize, // In bytes
    pub kind: KindData, // I'm gonna call the "type" of a type a "kind". So there's BuiltIn, Struct, etc.
}

#[derive(Debug, Clone)]
pub enum KindData {
    BuiltIn,
    Struct { members: HashMap<String, (Type, usize)> }, // members maps name to (offset, type)
}

pub fn get_default_types() -> HashMap<Type, TypeInfo> {
    let mut map = HashMap::new();

    map.insert(
        Type::BuiltIn(BuiltIn::U8),
        TypeInfo { size: 1, alignment: 1, kind: KindData::BuiltIn },
    );
    map.insert(
        Type::BuiltIn(BuiltIn::U16),
        TypeInfo { size: 2, alignment: 2, kind: KindData::BuiltIn },
    );
    map.insert(
        Type::BuiltIn(BuiltIn::U32),
        TypeInfo { size: 4, alignment: 4, kind: KindData::BuiltIn },
    );
    map.insert(
        Type::BuiltIn(BuiltIn::U64),
        TypeInfo { size: 8, alignment: 8, kind: KindData::BuiltIn },
    );
    map.insert(
        Type::BuiltIn(BuiltIn::I8),
        TypeInfo { size: 1, alignment: 1, kind: KindData::BuiltIn },
    );
    map.insert(
        Type::BuiltIn(BuiltIn::I16),
        TypeInfo { size: 2, alignment: 2, kind: KindData::BuiltIn },
    );
    map.insert(
        Type::BuiltIn(BuiltIn::I32),
        TypeInfo { size: 4, alignment: 4, kind: KindData::BuiltIn },
    );
    map.insert(
        Type::BuiltIn(BuiltIn::I64),
        TypeInfo { size: 8, alignment: 8, kind: KindData::BuiltIn },
    );

    map.insert(
        Type::BuiltIn(BuiltIn::Boolean),
        TypeInfo { size: 1, alignment: 1, kind: KindData::BuiltIn },
    );

    map.insert(
        Type::BuiltIn(BuiltIn::Unit),
        TypeInfo { size: 0, alignment: 1, kind: KindData::BuiltIn },
    ); // Not sure if this should have an alignment

    map.insert(
        Type::BuiltIn(BuiltIn::Bottom),
        TypeInfo { size: 0, alignment: 1, kind: KindData::BuiltIn },
    );

    map
}

pub fn add_struct_type(
    env: &mut CompilationEnvironment,
    name: String,
    members: Vec<(String, String)>,
) -> Result<(), CompileError> {
    if members.is_empty() {
        return Err("Structs with no members are not implemented. Consider `unit` instead.".into());
    }

    let mut alignment: usize = 1;

    /* Check type validity, determine overall struct alignment. */

    for (_, member_type_name) in &members {
        let member_type = member_type_name.clone().into();

        let Some(TypeInfo { alignment: member_alignment, .. }) = env.types.get(&member_type) else {
            return Err("Bad Member Type. (Make sure to declare structs in right order.)".into());
        };

        alignment = std::cmp::max(alignment, *member_alignment);
    }

    /* Compute offsets, ensure fields respect alignment. */
    let mut processed_members: HashMap<String, (Type, usize)> = HashMap::new(); // usize is offset.
    let mut curr_offset = 0;

    for (member_name, member_type_name) in members {
        let member_type = member_type_name.clone().into();
        let TypeInfo { alignment: member_alignment, size: member_size, .. } =
            env.types.get(&member_type).expect("Known to Exist");

        if curr_offset % member_alignment != 0 {
            curr_offset += member_alignment - (curr_offset % member_alignment);
        }

        processed_members.insert(member_name, (member_type, curr_offset));
        curr_offset += member_size;
    }

    env.types.insert(
        Type::UserDefined(name.clone()),
        TypeInfo {
            size: curr_offset,
            alignment,
            kind: KindData::Struct { members: processed_members },
        },
    );

    Ok(())
}
