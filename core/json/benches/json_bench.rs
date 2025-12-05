use criterion::{BenchmarkId, Criterion, black_box, criterion_group, criterion_main};
use fiber_json::{JsValue, parse, stringify_into};

struct StringifyCase {
    name: &'static str,
    fiber_value: JsValue,
    serde_value: serde_json::Value,
}

fn numeric_array_payload() -> String {
    let mut s = String::from("[");
    for i in 0..2000 {
        if i > 0 {
            s.push(',');
        }
        s.push_str(&i.to_string());
    }
    s.push(']');
    s
}

fn nested_payload() -> String {
    let mut s = String::from("{\"layers\":[");
    for i in 0..50 {
        if i > 0 {
            s.push(',');
        }
        s.push_str(&format!("{{\"level\":{},\"name\":\"node{}\"}}", i, i));
    }
    s.push_str("],\"meta\":{\"depth\":50,\"ok\":true}}");
    s
}

fn heavy_string_payload() -> String {
    let snippet = "emoji 😀 snowman ☃ rocket 🚀 arrows → ← star ✨";
    format!(
        "{{\"text\":\"{}\",\"padding\":\"{}\"}}",
        snippet.repeat(16),
        "a".repeat(2048)
    )
}

fn escape_payload() -> String {
    r#"{"path":"C:\\temp\\notes.txt","text":"line1\nline2 with \u03C0 and \t tab"}"#.to_string()
}

fn parse_cases() -> Vec<(&'static str, String)> {
    vec![
        ("tiny", r#"{"ok":true,"id":7,"label":"hi"}"#.to_string()),
        ("escapes", escape_payload()),
        ("numeric_array", numeric_array_payload()),
        ("nested", nested_payload()),
        ("large_text", heavy_string_payload()),
    ]
}

fn stringify_cases() -> Vec<StringifyCase> {
    parse_cases()
        .into_iter()
        .map(|(name, json)| StringifyCase {
            name,
            fiber_value: parse(&json).expect("fiber_json parse"),
            serde_value: serde_json::from_str(&json).expect("serde_json parse"),
        })
        .collect()
}

fn bench_parse(c: &mut Criterion) {
    for (name, json) in parse_cases() {
        c.bench_with_input(
            BenchmarkId::new("parse/fiber_json", name),
            json.as_str(),
            |b, input| {
                b.iter(|| {
                    let value = parse(black_box(input)).expect("fiber_json parse");
                    black_box(value);
                });
            },
        );

        c.bench_with_input(
            BenchmarkId::new("parse/serde_json", name),
            json.as_str(),
            |b, input| {
                b.iter(|| {
                    let value: serde_json::Value =
                        serde_json::from_str(black_box(input)).expect("serde_json parse");
                    black_box(value);
                });
            },
        );
    }
}

fn bench_stringify(c: &mut Criterion) {
    for case in stringify_cases() {
        c.bench_function(&format!("stringify/fiber_json/{}", case.name), |b| {
            b.iter(|| {
                let mut out = String::new();
                stringify_into(&mut out, black_box(&case.fiber_value))
                    .expect("fiber_json stringify");
                black_box(out);
            });
        });

        c.bench_function(&format!("stringify/serde_json/{}", case.name), |b| {
            b.iter(|| {
                let out = serde_json::to_string(black_box(&case.serde_value))
                    .expect("serde_json stringify");
                black_box(out);
            });
        });
    }
}

criterion_group!(json_benches, bench_parse, bench_stringify);
criterion_main!(json_benches);
