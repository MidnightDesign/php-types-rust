use php_types_rust::parser::parse_type;
use php_types_rust::r#type::Type;
use php_types_rust::scope::Scope;

const SOURCES: &[(&str, &str)] = &[
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

fn cases_from_src(src: &str, scope: &Scope) -> Vec<(Type, Type)> {
    return src
        .lines()
        .filter_map(|line| {
            if line.is_empty() {
                return None;
            }
            let parts = line[3..line.len() - 1]
                .split("` is a subtype of `")
                .collect::<Vec<_>>();
            if parts.len() != 2 {
                panic!("invalid line: {}", line);
            }
            let sub = parse_type(parts[0], scope).unwrap();
            let sup = parse_type(parts[1], scope).unwrap();
            Some((sub, sup))
        })
        .collect::<Vec<_>>();
}

fn collect_cases<'a>(scope: &Scope) -> Vec<(Type, Type, &'a str)> {
    return SOURCES
        .iter()
        .flat_map(|(file, src)| {
            return cases_from_src(src, scope)
                .iter()
                .map(|(sub, sup)| (sub.clone(), sup.clone(), file.clone()))
                .collect::<Vec<_>>();
        })
        .collect::<Vec<_>>();
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
    let mut failing_cases = vec![];
    let cases = collect_cases(&scope);
    cases.iter().for_each(|(sub, sup, file)| {
        if sub.is_subtype_of(sup) {
            return;
        }
        failing_cases.push(format!("{}: {} is a subtype of {}", file, sub, sup));
    });
    let n_failing = failing_cases.len();
    let n_total = cases.len();
    assert!(
        failing_cases.is_empty(),
        "{}/{} ({}%) cases failed:\n{}",
        n_failing,
        n_total,
        n_failing / n_total * 100,
        failing_cases.join("\n")
    );
}
