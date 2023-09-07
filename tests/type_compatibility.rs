use php_types_rust::parser::parse_type;
use php_types_rust::scope::Scope;
use php_types_rust::r#type::Type;

const SOURCES: &[(&str, &str)] = &[
    ("bool", include_str!("./assertions/bool.md")),
    ("callable", include_str!("./assertions/callable.md")),
    // ("class-like", include_str!("./assertions/class-like.md")),
    ("int", include_str!("./assertions/int.md")),
    ("int-literal", include_str!("./assertions/int-literal.md")),
    ("intersection", include_str!("./assertions/intersection.md")),
    // ("iterable", include_str!("./assertions/iterable.md")),
    ("list", include_str!("./assertions/list.md")),
    ("map", include_str!("./assertions/map.md")),
    ("misc", include_str!("./assertions/misc.md")),
    // ("string", include_str!("./assertions/string.md")),
    ("string-literal", include_str!("./assertions/string-literal.md")),
    // ("struct", include_str!("./assertions/struct.md")),
    ("tuple", include_str!("./assertions/tuple.md")),
    ("union", include_str!("./assertions/union.md")),
];

fn cases_from_src(src: &str, scope: &Scope) -> Vec<(Type, Type)> {
    return src.lines().filter_map(|line| {
        if line.is_empty() {
            return None;
        }
        let parts = line[3..line.len() - 1].split("` is a subtype of `").collect::<Vec<_>>();
        if parts.len() != 2 {
            panic!("invalid line: {}", line);
        }
        let sub = parse_type(parts[0], scope).unwrap();
        let sup = parse_type(parts[1], scope).unwrap();
        Some((sub, sup))
    }).collect::<Vec<_>>();
}

fn collect_cases<'a>(scope: &Scope) -> Vec<(Type, Type, &'a str)> {
    return SOURCES.iter().flat_map(|(file, src)| {
        return cases_from_src(src, scope).iter().map(|(sub, sup)| {
            return (sub.clone(), sup.clone(), file.clone());
        }).collect::<Vec<_>>();
    }).collect::<Vec<_>>();
}

#[test]
fn compatibility() {
    let scope = Scope::global();
    collect_cases(&scope).iter().for_each(|(sub, sup, file)| {
        assert!(sub.is_subtype_of(sup), "{}: {} is a subtype of {}", file, sub, sup);
    });
}
