use php_types_rust::parser::parse_type;
use php_types_rust::r#type::Type;
use php_types_rust::scope::Scope;

const TYPES: &[&str] = &[
    include_str!("./types/bool.txt"),
    include_str!("./types/callable.txt"),
    include_str!("./types/class-like.txt"),
    include_str!("./types/int.txt"),
    include_str!("./types/int-literal.txt"),
    include_str!("./types/intersection.txt"),
    include_str!("./types/iterable.txt"),
    include_str!("./types/list.txt"),
    include_str!("./types/map.txt"),
    // include_str!("./types/misc.txt"),
    // include_str!("./types/string.txt"),
    include_str!("./types/string-literal.txt"),
    // include_str!("./types/struct.txt"),
    // include_str!("./types/tuple.txt"),
    include_str!("./types/union.txt"),
];

const ASSERTIONS: &[(&str, &str)] = &[
    ("bool", include_str!("./assertions/bool.md")),
    ("callable", include_str!("./assertions/callable.md")),
    ("class-like", include_str!("./assertions/class-like.md")),
    ("int", include_str!("./assertions/int.md")),
    ("int-literal", include_str!("./assertions/int-literal.md")),
    ("intersection", include_str!("./assertions/intersection.md")),
    ("iterable", include_str!("./assertions/iterable.md")),
    ("list", include_str!("./assertions/list.md")),
    ("map", include_str!("./assertions/map.md")),
    ("misc", include_str!("./assertions/misc.md")),
    ("string", include_str!("./assertions/string.md")),
    (
        "string-literal",
        include_str!("./assertions/string-literal.md"),
    ),
    ("struct", include_str!("./assertions/struct.md")),
    ("tuple", include_str!("./assertions/tuple.md")),
    ("union", include_str!("./assertions/union.md")),
];

fn collect_combinations() -> Vec<(String, String)> {
    let types: Vec<_> = TYPES
        .iter()
        .flat_map(|t| t.lines())
        .map(|t| t.to_string())
        .collect();
    let mut combinations = vec![];
    for sub in &types {
        for sup in &types {
            combinations.push((sub.clone(), sup.clone()));
        }
    }
    combinations
}

fn parse_assertion(line: &str) -> (String, String) {
    let line = line.strip_prefix("- ").unwrap();
    let mut parts = line.split(" is a subtype of ");
    let sub = parts.next().unwrap().trim_matches('`');
    let sup = parts.next().unwrap().trim_matches('`');
    (sub.to_string(), sup.to_string())
}

fn collect_assertions() -> Vec<(String, String)> {
    ASSERTIONS
        .iter()
        .flat_map(|(_, f)| f.lines())
        .filter(|l| !l.is_empty())
        .map(parse_assertion)
        .collect()
}

#[test]
fn compatibility() {
    let mut scope = Scope::global();
    let foo_interface = Type::ClassLike {
        name: "FooInterface".to_string(),
        parameters: vec![],
        parents: vec![],
    };
    let foo_class = Type::ClassLike {
        name: "Foo".to_string(),
        parameters: vec![],
        parents: vec![foo_interface.clone()],
    };
    let runnable = Type::ClassLike {
        name: "Runnable".to_string(),
        parameters: vec![],
        parents: vec![],
    };
    let loggable = Type::ClassLike {
        name: "Loggable".to_string(),
        parameters: vec![],
        parents: vec![],
    };
    let runnable_and_loggable = Type::ClassLike {
        name: "RunnableAndLoggable".to_string(),
        parameters: vec![],
        parents: vec![runnable.clone(), loggable.clone()],
    };
    scope.register("FooInterface".to_string(), foo_interface);
    scope.register("Foo".to_string(), foo_class);
    scope.register("Runnable".to_string(), runnable);
    scope.register("Loggable".to_string(), loggable);
    scope.register("RunnableAndLoggable".to_string(), runnable_and_loggable);
    let assertions = collect_assertions();
    for combination in collect_combinations() {
        let (sub_str, sup_str) = combination;
        let expected = sub_str == sup_str
            || assertions
                .iter()
                .any(|(sub_, sup_)| sub_ == &sub_str && sup_ == &sup_str);
        let sub = parse_type(&sub_str, &scope).unwrap();
        let sup = parse_type(&sup_str, &scope).unwrap();
        let actual = sub.is_subtype_of(&sup);
        if expected != actual {
            if expected {
                panic!(
                    "Expected \"{}\" to be a subtype of \"{}\", but it is not.",
                    sub_str, sup_str
                );
            } else {
                panic!(
                    "Expected \"{}\" to not be a subtype of \"{}\", but it is.",
                    sub_str, sup_str
                );
            }
        }
    }
}
