use super::*;
use fiber_json::JsValue;
use std::pin::Pin;
use std::task::Context;

fn run(src: &str, root: JsValue) -> JsValue {
    Script::compile(src)
        .unwrap()
        .block_exec(root, None)
        .unwrap()
}

#[test]
fn executes_return_expression() {
    let result = run("return 1 + 2 * 3;", JsValue::Null);
    assert_eq!(result, JsValue::Int(7));
}

#[test]
fn supports_let_bindings_and_variables() {
    let result = run("let a = 2; let b = a + 3; return b * 2;", JsValue::Null);
    assert_eq!(result, JsValue::Int(10));
}

#[test]
fn returns_root_value() {
    let root = JsValue::String(fiber_string::JsString::from("payload"));
    let result = run("return $;", root.clone());
    assert_eq!(result, root);
}
#[test]
fn assignment_expression_returns_value() {
    let result = run("let a = 1; return a = 5;", JsValue::Null);
    assert_eq!(result, JsValue::Int(5));
}

#[test]
fn function_receives_attach_context() {
    #[derive(Debug)]
    struct Payload(i64);

    let mut library = Library::empty();
    library.register_function(None, "read_attach", |ctx: VmCallContext<'_>| {
        let payload = unsafe { ctx.attach_as::<Payload>() }
            .map(|p| p.0)
            .unwrap_or(0);
        Ok(JsValue::Int(payload))
    });

    let attach = AttachPtr::from_ref(&Payload(42));
    let script = Script::compile_with("return read_attach();", library).unwrap();
    let result = script.block_exec(JsValue::Null, Some(attach)).unwrap();
    assert_eq!(result, JsValue::Int(42));
}

#[test]
fn async_function_uses_context_args() {
    struct AsyncDouble;

    #[async_trait::async_trait(?Send)]
    impl VmAsyncFunction for AsyncDouble {
        async fn call(self: std::sync::Arc<Self>, ctx: VmCallContext<'_>) -> VmResult {
            let arg = ctx.arg(0).cloned().unwrap_or(JsValue::Undefined);
            match arg {
                JsValue::Int(n) => Ok(JsValue::Int(n * 2)),
                other => Ok(other),
            }
        }
    }

    let mut library = Library::empty();
    library.register_async_function(None, "adouble", AsyncDouble);

    let script = Script::compile_with("return adouble(7);", library).unwrap();
    let mut vm = script.exec(JsValue::Null, None);
    let mut fut = Pin::new(&mut vm).into_future();
    let waker = noop_waker();
    let mut cx = Context::from_waker(&waker);
    let res = Pin::new(&mut fut).poll(&mut cx);
    match res {
        std::task::Poll::Ready(Ok(val)) => assert_eq!(val, JsValue::Int(14)),
        other => panic!("unexpected poll result: {:?}", other),
    }
}

#[test]
fn operators_follow_doc_examples() {
    let result = run(
        "
        let num = 1;
        let txt = \"this is string\";
        let obj = {n:num};
        let arr = [1,2,num];
        let cond = num > 0 ? \"yes\" : \"no\";
        let neg = -num;
        let not = !0;
        let add_txt = num + txt;
        let in_obj = \"n\" in obj;
        let modv = (num + 10) % 3;
        return {cond, neg, not, add_txt, in_obj, modv, arr0: arr[0], objn: obj.n};
        ",
        JsValue::Null,
    );

    let expected = fiber_json::parse(
        r#"{
            "cond":"yes",
            "neg": -1,
            "not": true,
            "add_txt": "1this is string",
            "in_obj": true,
            "modv": 2,
            "arr0": 1,
            "objn": 1
        }"#,
    )
    .unwrap();

    assert_eq!(result, expected);
}

#[test]
fn binary_ops_match_user_doc() {
    let result = run(
        "
        let num = 1;
        let txt = \"this is string\";
        let obj = {n:num};

        return {
            add: num + 3,
            add_txt: num + txt,
            and: num - 1 && \"not return this, return 0\",
            or: num - 1 || \"return this\",
            mod: (num + 10) % 3,
            \"in\": \"n\" in obj
        };
        ",
        JsValue::Null,
    );

    let expected = fiber_json::parse(
        r#"{
            "add":4,
            "add_txt":"1this is string",
            "and":0,
            "or":"return this",
            "mod":2,
            "in":true
        }"#,
    )
    .unwrap();

    assert_eq!(result, expected);
}

#[test]
fn binary_ops_across_types_follow_js_semantics() {
    let result = run(
        "
        let res = {};
        res.int_sum = 1 + 2;
        res.int_float = 1 + 2.5;
        res.str_num = 1 + \"2\";
        res.num_str = 2 + \"3\";
        res.bool_add = true + true;
        res.subtract_string = \"5\" - 2;
        res.multiply_bool = \"6\" * false;
        res.divide = 5 / 2;
        res.mod_float = 5 % 2.5;
        res.and_zero = 0 && 1;
        res.and_truthy = 1 && \"ok\";
        res.or_truthy = 0 || \"fallback\";
        res.or_left = \"left\" || \"right\";
        return res;
        ",
        JsValue::Null,
    );

    let expected = fiber_json::parse(
        r#"{
            "int_sum":3,
            "int_float":3.5,
            "str_num":"12",
            "num_str":"23",
            "bool_add":2.0,
            "subtract_string":3.0,
            "multiply_bool":0.0,
            "divide":2.5,
            "mod_float":0.0,
            "and_zero":0,
            "and_truthy":"ok",
            "or_truthy":"fallback",
            "or_left":"left"
        }"#,
    )
    .unwrap();

    assert_eq!(result, expected);
}

#[test]
fn object_and_array_assignment() {
    let result = run(
        "
        let obj = {a:1};
        obj.b = 2;
        let arr = [0,1,2];
        arr[1] = obj.a + obj.b;
        arr[3] = 9;
        return {a: obj.a, b: obj.b, arr1: arr[1], arr3: arr[3]};
        ",
        JsValue::Null,
    );

    assert_eq!(
        result,
        fiber_json::parse(
            r#"{
                "a":1,
                "b":2,
                "arr1":3,
                "arr3":9
            }"#
        )
        .unwrap()
    );
}

#[test]
fn ternary_expression_selects_branch() {
    // Ensure parser builds a conditional expression.
    let ast = Parser::new(
        "
        let num = 2;
        let val = num > 1 ? \"yes\" : \"no\";
        return val;
        ",
    )
    .unwrap()
    .parse_script()
    .unwrap();
    fn stmt_has_conditional(stmt: &parse::ast::Stmt) -> bool {
        match &stmt.kind {
            parse::ast::StmtKind::Let(bindings) => bindings.iter().any(|b| {
                b.init
                    .as_ref()
                    .map(|init| matches!(init.kind, parse::ast::ExprKind::Conditional { .. }))
                    .unwrap_or(false)
            }),
            parse::ast::StmtKind::Block(block) => block.statements.iter().any(stmt_has_conditional),
            _ => false,
        }
    }
    let has_conditional = ast.iter().any(stmt_has_conditional);
    assert!(has_conditional, "conditional not parsed: {:?}", ast);

    let result = run(
        "
        let num = 2;
        let val = num > 1 ? \"yes\" : \"no\";
        return val;
        ",
        JsValue::Null,
    );
    assert_eq!(result, JsValue::String(fiber_string::JsString::from("yes")));
}

#[test]
fn comparison_gt_produces_bool() {
    let result = run("return 2 > 1;", JsValue::Null);
    assert_eq!(result, JsValue::Bool(true));
}
