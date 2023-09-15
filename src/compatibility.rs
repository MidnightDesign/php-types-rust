use crate::r#type::{
    flatten, intersection_to_map, to_iterable, to_list, to_map, Callable, Int, Iterable, List, Map,
    StringFlag, StructMember, Type,
};
use std::collections::HashMap;

pub fn compare_bools(sub_value: &Option<bool>, upper_value: &Option<bool>) -> bool {
    match (sub_value, upper_value) {
        (_, None) => true,
        (None, Some(_)) => false,
        (Some(sub_value), Some(uper_value)) => sub_value == uper_value,
    }
}

pub fn compare_callables(sub: &Callable, sup: &Callable) -> bool {
    if !sub.return_type.is_subtype_of(&sup.return_type) {
        return false;
    }
    for (i, sub_param) in sub.parameters.iter().enumerate() {
        if i >= sup.parameters.len() {
            return false;
        }
        let super_param = &sup.parameters[i];
        if !super_param.type_.is_subtype_of(&sub_param.type_) {
            return false;
        }
    }
    return true;
}

fn compare_ints(sub: &Int, sup: &Int) -> bool {
    let sub = (
        sub.min.map_or(isize::MIN, |v| v),
        sub.max.map_or(isize::MAX, |v| v),
    );
    let sup = (
        sup.min.map_or(isize::MIN, |v| v),
        sup.max.map_or(isize::MAX, |v| v),
    );
    sub.0 >= sup.0 && sub.1 <= sup.1
}

fn compare_struct_map(sub_members: &HashMap<String, StructMember>, sup_value: &Type) -> bool {
    for StructMember {
        type_: sub_type, ..
    } in sub_members.values()
    {
        if sub_type.is_subtype_of(sup_value) {
            continue;
        }
        return false;
    }
    true
}

fn compare_map_map(sub: &Type, sup: &Type) -> bool {
    if let Type::Map(Map {
        key: sub_key,
        value: sub_value,
        non_empty: sub_non_empty,
    }) = sub
    {
        if let Type::Map(Map {
            key: sup_key,
            value: sup_value,
            non_empty: sup_non_empty,
        }) = &sup
        {
            if *sup_non_empty && !*sub_non_empty {
                return false;
            }
            return sub_key.is_subtype_of(sup_key) && sub_value.is_subtype_of(sup_value);
        }
    }
    false
}

fn compare_classlike_classlike(sub: &Type, sup: &Type) -> bool {
    match (sub, sup) {
        (
            Type::ClassLike {
                name: sub_name,
                parameters: sub_parameters,
                ..
            },
            Type::ClassLike {
                name: sup_name,
                parameters: sup_parameters,
                ..
            },
        ) if sub_name == sup_name => {
            for (sub_parameter, sup_parameter) in sub_parameters.iter().zip(sup_parameters) {
                if !sub_parameter.is_subtype_of(sup_parameter) {
                    return false;
                }
            }
            true
        }
        (Type::ClassLike { parents, .. }, _) => {
            parents.iter().any(|parent| parent.is_subtype_of(sup))
        }
        _ => false,
    }
}

fn compare_iterables(sub: &Iterable, sup: &Iterable) -> bool {
    sub.key.is_subtype_of(&sup.key) && sub.value.is_subtype_of(&sup.value)
}

fn compare_maps(sub: &Map, sup: &Map) -> bool {
    if sup.non_empty && !sub.non_empty {
        return false;
    }
    sub.key.is_subtype_of(&sup.key) && sub.value.is_subtype_of(&sup.value)
}

fn compare_lists(sub: &List, sup: &List) -> bool {
    if sup.non_empty && !sub.non_empty {
        return false;
    }
    sub.type_.is_subtype_of(&sup.type_)
}

fn are_all_subtypes_of(subs: Vec<Type>, sup: &Type) -> bool {
    for sub in subs {
        if sub.is_subtype_of(sup) {
            continue;
        }
        return false;
    }
    true
}

fn is_numeric(str: &str) -> bool {
    // TODO Should mimic PHP's is_numeric. This doesn't cover any of the following strings that PHP
    //      considers numeric: `+42`, `42e2`, `42.`, `42.69`, `42e-2`, and many more.
    str.chars().all(|c| c.is_numeric())
}

fn compare_structs(
    sub: &HashMap<String, StructMember>,
    sup: &HashMap<String, StructMember>,
) -> bool {
    for (name, sup) in sup {
        let sub = sub.get(name);
        let ok = match (sub, sup) {
            (None, StructMember { optional, .. }) => *optional,
            (Some(StructMember { type_: sub, .. }), StructMember { type_: sup, .. }) => {
                sub.is_subtype_of(sup)
            }
        };
        if ok {
            continue;
        }
        return false;
    }
    true
}

fn compare_tuples(sub: &Vec<Type>, sup: &Vec<Type>) -> bool {
    if sub.len() < sup.len() {
        return false;
    }
    for (sub, sup) in sub.iter().zip(sup) {
        if sub.is_subtype_of(sup) {
            continue;
        }
        return false;
    }
    true
}

impl Type {
    pub fn is_subtype_of(&self, sup: &Type) -> bool {
        if self == sup {
            return true;
        }
        match (self, sup) {
            (_, Type::Mixed) => true,
            (Type::Bool(sub_value), Type::Bool(upper_value)) => {
                compare_bools(sub_value, upper_value)
            }
            (
                Type::Bool(_)
                | Type::Int { .. }
                | Type::Float
                | Type::String(_)
                | Type::IntLiteral(_)
                | Type::StringLiteral(_)
                | Type::ClassString(_),
                Type::Scalar,
            ) => true,
            (Type::String(_), Type::String(None)) => true,
            (Type::Callable(sub), Type::Callable(sup)) => compare_callables(sub, sup),
            (_, Type::Void) => true,
            (Type::Int(..), Type::Float) => true,
            (Type::Int(sub), Type::Int(sup)) => compare_ints(sub, sup),
            (Type::Int(Int { min, max }), Type::IntLiteral(value)) => {
                min.map_or(false, |min| min == *value) && max.map_or(false, |max| max == *value)
            }
            (Type::IntLiteral(_), Type::Float | Type::Int { .. }) => true,
            (Type::Struct(sub_members), Type::Map(Map { value, .. })) => {
                compare_struct_map(sub_members, value)
            }
            (Type::Intersection(left, right), Type::Map(..)) => {
                compare_map_map(&intersection_to_map(left, right), sup)
            }
            (Type::ClassLike { .. }, Type::ClassLike { .. }) => {
                compare_classlike_classlike(self, sup)
            }
            (Type::Union(..), _) => are_all_subtypes_of(flatten(self.clone()), sup),
            (_, Type::List(sup)) => to_list(self).map_or(false, |sub| compare_lists(&sub, sup)),
            (_, Type::Iterable(sup)) => {
                to_iterable(self).map_or(false, |sub| compare_iterables(&sub, sup))
            }
            (_, Type::Union(left, right)) => self.is_subtype_of(left) || self.is_subtype_of(right),
            (_, Type::Map(sup_map)) => {
                to_map(self).map_or(false, |sub_map| compare_maps(&sub_map, sup_map))
            }
            (Type::ClassString(_), Type::String(_)) => true,
            (Type::StringLiteral(_), Type::String(None)) => true,
            (Type::ClassString(_), Type::ClassString(None)) => true,
            (Type::ClassString(Some(sub)), Type::ClassString(Some(sup))) => sub.is_subtype_of(sup),
            (Type::String(Some(StringFlag::Numeric)), Type::String(Some(StringFlag::NonEmpty))) => {
                true
            }
            (Type::StringLiteral(str), Type::String(Some(StringFlag::NonEmpty))) => !str.is_empty(),
            (Type::StringLiteral(str), Type::String(Some(StringFlag::Numeric))) => is_numeric(str),
            (Type::Struct(sub), Type::Struct(sup)) => compare_structs(sub, sup),
            (
                Type::Map(..) | Type::List(..) | Type::Struct(..) | Type::Tuple(..),
                Type::Tuple(elements),
            ) if elements.is_empty() => true,
            (Type::Tuple(sub), Type::Tuple(sup)) => compare_tuples(sub, sup),
            (Type::Never, _) => true,
            (_, Type::Intersection(left, right)) => {
                self.is_subtype_of(left) && self.is_subtype_of(right)
            }
            (_, _) => false,
        }
    }
}
