use super::*;
use crate::parse::compile::FunctionRef;
use crate::stdlib::{Directive, DirectiveFactory};
use crate::vm::VmError;
use crate::vm::VmOperand;
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
fn unary_operations_follow_js_like_semantics() {
    let result = run(
        "
        let u;
        return [
            +1,
            +\"2\",
            -true,
            -\"3\",
            !0,
            !1,
            typeof 1,
            typeof null,
            typeof u
        ];
        ",
        JsValue::Null,
    );

    let expected = JsValue::array(vec![
        JsValue::Int(1),
        JsValue::Float(2.0),
        JsValue::Int(-1),
        JsValue::Float(-3.0),
        JsValue::Bool(true),
        JsValue::Bool(false),
        JsValue::String("number".into()),
        JsValue::String("object".into()),
        JsValue::String("undefined".into()),
    ]);

    assert_eq!(result, expected);
}

#[test]
fn function_spread_arguments_flatten_iterables() {
    let mut library = Library::empty();
    library.register_function(None, "gather", |ctx: VmCallContext<'_>| {
        let mut items = Vec::new();
        for idx in 0..ctx.arg_count() {
            items.push(ctx.arg(idx).cloned().unwrap_or(JsValue::Undefined));
        }
        Ok(JsValue::array(items))
    });

    let script = Script::compile_with(
        "
        let arr = [1,2,3];
        let obj = {a:4, b:5};
        let v = 3;
        return gather(0, ...arr, ...obj, ...v, 6);
        ",
        library,
    )
    .unwrap();

    let result = script.block_exec(JsValue::Null, None).unwrap();
    assert_eq!(
        result,
        JsValue::array(vec![
            JsValue::Int(0),
            JsValue::Int(1),
            JsValue::Int(2),
            JsValue::Int(3),
            JsValue::Int(4),
            JsValue::Int(5),
            JsValue::Int(6),
        ])
    );
}

#[test]
fn directive_namespace_resolves_functions() {
    #[derive(Debug)]
    struct HttpDirective {
        base: String,
    }
    impl Directive for HttpDirective {
        fn find_function(&self, _namespace: &str, name: &str) -> Option<FunctionRef> {
            if name != "sendRequest" {
                return None;
            }
            let base = self.base.clone();
            let func = move |ctx: VmCallContext<'_>| {
                let arg = match ctx.arg(0) {
                    Some(JsValue::String(s)) => s.to_std_string_lossy(),
                    Some(other) => format!("{other:?}"),
                    None => String::new(),
                };
                Ok(JsValue::String(fiber_string::JsString::from(format!(
                    "{base}{arg}"
                ))))
            };
            Some(FunctionRef {
                operand: VmOperand::Function(std::sync::Arc::new(func)),
                is_async: false,
            })
        }
    }
    #[derive(Debug)]
    struct HttpDirectiveFactory;
    impl DirectiveFactory for HttpDirectiveFactory {
        fn create_directive(
            &self,
            _ty: &str,
            _namespace: &str,
            literal: &str,
        ) -> Option<std::rc::Rc<dyn Directive>> {
            Some(std::rc::Rc::new(HttpDirective {
                base: literal.to_string(),
            }))
        }
    }
    let mut library = Library::empty();
    library.register_directive_factory("http", std::rc::Rc::new(HttpDirectiveFactory));

    let script =
        Script::compile_with(r#"directive google = http "https://google.com"; return google.sendRequest("/search?q=rust");"#, library)
            .unwrap();
    let result = script.block_exec(JsValue::Null, None).unwrap();
    assert_eq!(
        result,
        JsValue::String(fiber_string::JsString::from(
            "https://google.com/search?q=rust"
        ))
    );
}

#[test]
fn directive_allows_async_functions() {
    #[derive(Debug)]
    struct AsyncAdderDirective;
    impl Directive for AsyncAdderDirective {
        fn find_function(&self, _namespace: &str, name: &str) -> Option<FunctionRef> {
            if name != "add" {
                return None;
            }
            struct AsyncAdder;
            #[async_trait::async_trait(?Send)]
            impl VmAsyncFunction for AsyncAdder {
                async fn call(self: std::sync::Arc<Self>, ctx: VmCallContext<'_>) -> VmResult {
                    let val = ctx.arg(0).cloned().unwrap_or(JsValue::Int(0));
                    let JsValue::Int(n) = val else {
                        return Ok(JsValue::Undefined);
                    };
                    Ok(JsValue::Int(n + 1))
                }
            }
            Some(FunctionRef {
                operand: VmOperand::AsyncFunction(std::sync::Arc::new(AsyncAdder)),
                is_async: true,
            })
        }
    }
    #[derive(Debug)]
    struct AsyncAdderFactory;
    impl DirectiveFactory for AsyncAdderFactory {
        fn create_directive(
            &self,
            _ty: &str,
            _namespace: &str,
            _literal: &str,
        ) -> Option<std::rc::Rc<dyn Directive>> {
            Some(std::rc::Rc::new(AsyncAdderDirective))
        }
    }

    let mut library = Library::empty();
    library.register_directive_factory("asyncns", std::rc::Rc::new(AsyncAdderFactory));

    let script = Script::compile_with(
        r#"
        directive svc = asyncns "meta";
        return svc.add(5);
        "#,
        library,
    )
    .unwrap();

    let mut vm = script.exec(JsValue::Null, None);
    let mut fut = Pin::new(&mut vm).into_future();
    let waker = noop_waker();
    let mut cx = Context::from_waker(&waker);
    let res = Pin::new(&mut fut).poll(&mut cx);
    match res {
        std::task::Poll::Ready(Ok(val)) => assert_eq!(val, JsValue::Int(6)),
        other => panic!("unexpected poll result: {:?}", other),
    }
}

#[test]
fn directive_scope_not_hoisted() {
    #[derive(Debug)]
    struct DummyDirectiveFactory;
    impl DirectiveFactory for DummyDirectiveFactory {
        fn create_directive(
            &self,
            _ty: &str,
            _namespace: &str,
            _literal: &str,
        ) -> Option<std::rc::Rc<dyn Directive>> {
            None
        }
    }
    let mut library = Library::empty();
    library.register_directive_factory("http", std::rc::Rc::new(DummyDirectiveFactory));

    let script = Script::compile_with(
        r#"
        return google.sendRequest("/before");
        directive google = http "https://example.com";
        "#,
        library,
    );
    assert!(
        script.is_err(),
        "directive should not be usable before declaration"
    );
}

#[test]
fn directive_available_after_declaration_even_in_block() {
    #[derive(Debug)]
    struct HttpDirective {
        base: String,
    }
    impl Directive for HttpDirective {
        fn find_function(&self, _namespace: &str, name: &str) -> Option<FunctionRef> {
            if name != "sendRequest" {
                return None;
            }
            let base = self.base.clone();
            let func = move |ctx: VmCallContext<'_>| {
                let arg = match ctx.arg(0) {
                    Some(JsValue::String(s)) => s.to_std_string_lossy(),
                    Some(other) => format!("{other:?}"),
                    None => String::new(),
                };
                Ok(JsValue::String(fiber_string::JsString::from(format!(
                    "{base}{arg}"
                ))))
            };
            Some(FunctionRef {
                operand: VmOperand::Function(std::sync::Arc::new(func)),
                is_async: false,
            })
        }
    }
    #[derive(Debug)]
    struct HttpDirectiveFactory;
    impl DirectiveFactory for HttpDirectiveFactory {
        fn create_directive(
            &self,
            _ty: &str,
            _namespace: &str,
            literal: &str,
        ) -> Option<std::rc::Rc<dyn Directive>> {
            Some(std::rc::Rc::new(HttpDirective {
                base: literal.to_string(),
            }))
        }
    }

    let mut library = Library::empty();
    library.register_directive_factory("http", std::rc::Rc::new(HttpDirectiveFactory));

    let script = Script::compile_with(
        r#"
        if (1) {
            directive svc = http "https://scoped.com";
        }
        return svc.sendRequest("/path");
        "#,
        library,
    )
    .unwrap();

    let result = script.block_exec(JsValue::Null, None).unwrap();
    assert_eq!(
        result,
        JsValue::String(fiber_string::JsString::from("https://scoped.com/path"))
    );
}

#[test]
fn variable_must_be_declared_before_use() {
    let script = Script::compile("return not_defined;");
    assert!(script.is_err(), "using undeclared variable should error");
}

#[test]
fn inner_scope_can_shadow_outer() {
    let result = run(
        "
        let a = 1;
        if (true) {
            let a = 2;
            let b = a + 1;
            // returning here would end script; keep value to outer
            b;
        }
        return a;
        ",
        JsValue::Null,
    );
    assert_eq!(result, JsValue::Int(1));
}

#[test]
fn inner_scope_variable_not_visible_outside() {
    let script = Script::compile(
        "
        if (true) {
            let x = 10;
        }
        return x;
        ",
    );
    assert!(script.is_err(), "inner scope variable should not escape");
}

#[test]
fn if_else_executes_correct_branch() {
    let result = run(
        "
        let flag = false;
        let val;
        if (flag) {
            val = 1;
        } else {
            val = 2;
        }
        return val;
        ",
        JsValue::Null,
    );
    assert_eq!(result, JsValue::Int(2));
}

#[test]
fn for_of_iterates_array_and_breaks() {
    let result = run(
        "
        let arr = [1,2,3,4];
        let sum = 0;
        for (let idx, item of arr) {
            if (item > 3) {
                break;
            }
            sum = sum + item;
        }
        return sum;
        ",
        JsValue::Null,
    );
    assert_eq!(result, JsValue::Int(6));
}

#[test]
fn for_of_respects_continue() {
    let result = run(
        "
        let arr = [1,2,3,4,5];
        let sum = 0;
        for (let _, item of arr) {
            if (item % 2 == 0) {
                continue;
            }
            sum = sum + item;
        }
        return sum;
        ",
        JsValue::Null,
    );
    assert_eq!(result, JsValue::Int(9));
}

#[test]
fn for_of_with_async_function_calls() {
    #[derive(Debug)]
    struct AsyncDouble;
    #[async_trait::async_trait(?Send)]
    impl VmAsyncFunction for AsyncDouble {
        async fn call(self: std::sync::Arc<Self>, ctx: VmCallContext<'_>) -> VmResult {
            let val = ctx.arg(0).cloned().unwrap_or(JsValue::Int(0));
            if let JsValue::Int(n) = val {
                Ok(JsValue::Int(n * 2))
            } else {
                Ok(JsValue::Undefined)
            }
        }
    }

    let mut library = Library::empty();
    library.register_async_function(None, "adouble", AsyncDouble);

    let script = Script::compile_with(
        "
        let arr = [1,2,3];
        let sum = 0;
        for (let _, n of arr) {
            sum = sum + adouble(n);
        }
        return sum;
        ",
        library,
    )
    .unwrap();

    let result = run_async_script(script);
    assert_eq!(result, JsValue::Int(12));
}

#[test]
fn for_of_with_async_constant() {
    let mut library = Library::empty();
    library.register_async_const(
        Some("env"),
        "val",
        |_ctx: VmCallContext<'_>| async { Ok(JsValue::Int(5)) },
    );

    let script = Script::compile_with(
        "
        let sum = 0;
        for (let _, n of [1,2,3]) {
            sum = sum + $env.val;
        }
        return sum;
        ",
        library,
    )
    .unwrap();

    let result = run_async_script(script);
    // $env.val resolved each iteration => 3 * 5 = 15
    assert_eq!(result, JsValue::Int(15));
}

#[test]
fn try_catch_captures_thrown_value() {
    let result = run(
        "
        let value = 0;
        try {
            value = 1;
            throw 99;
        } catch (err) {
            return err;
        }
        return value;
        ",
        JsValue::Null,
    );
    assert_eq!(result, JsValue::Int(99));
}

#[test]
fn try_catch_skips_when_no_error() {
    let result = run(
        "
        let value = 1;
        try {
            value = value + 1;
        } catch (err) {
            value = value + 100;
        }
        return value;
        ",
        JsValue::Null,
    );
    assert_eq!(result, JsValue::Int(2));
}

#[test]
fn try_catch_handles_function_error() {
    let mut library = Library::empty();
    library.register_function(None, "fail", |_ctx: VmCallContext<'_>| {
        Err(VmError::new("boom", None))
    });

    let script = Script::compile_with(
        "
        try {
            fail();
        } catch (err) {
            return err;
        }
        return 0;
        ",
        library,
    )
    .unwrap();

    let result = script.block_exec(JsValue::Null, None).unwrap();
    match result {
        JsValue::Exception(exc) => assert_eq!(exc.message, "boom"),
        other => panic!("expected exception, got {:?}", other),
    }
}

fn run_async_script(script: Script) -> JsValue {
    let mut vm = script.exec(JsValue::Null, None);
    let waker = noop_waker();
    let mut cx = Context::from_waker(&waker);
    let mut fut = Pin::new(&mut vm).into_future();
    loop {
        match Pin::new(&mut fut).poll(&mut cx) {
            std::task::Poll::Ready(res) => break res.unwrap(),
            std::task::Poll::Pending => continue,
        }
    }
}

#[test]
fn try_catch_handles_async_function_error() {
    #[derive(Debug)]
    struct FailingAsyncFunc;
    #[async_trait::async_trait(?Send)]
    impl VmAsyncFunction for FailingAsyncFunc {
        async fn call(self: std::sync::Arc<Self>, _ctx: VmCallContext<'_>) -> VmResult {
            Err(VmError::new("async_fail", None))
        }
    }

    let mut library = Library::empty();
    library.register_async_function(None, "afail", FailingAsyncFunc);

    let script = Script::compile_with(
        "
        try {
            afail();
        } catch (err) {
            return err;
        }
        return 0;
        ",
        library,
    )
    .unwrap();

    let result = run_async_script(script);
    match result {
        JsValue::Exception(exc) => assert_eq!(exc.message, "async_fail"),
        other => panic!("expected exception, got {:?}", other),
    }
}

#[test]
fn try_catch_handles_async_constant_error() {
    let mut library = Library::empty();
    library.register_async_const(
        Some("env"),
        "fail",
        |_ctx: VmCallContext<'_>| async { Err(VmError::new("aconst_fail", None)) },
    );

    let script = Script::compile_with(
        "
        try {
            return $env.fail;
        } catch (err) {
            return err;
        }
        return 0;
        ",
        library,
    )
    .unwrap();

    let result = run_async_script(script);
    match result {
        JsValue::Exception(exc) => assert_eq!(exc.message, "aconst_fail"),
        other => panic!("expected exception, got {:?}", other),
    }
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
fn object_creation_with_computed_and_spread() {
    let result = run(
        r#"
        let a = 1;
        let d = "d";
        let p = {e: 5};
        let obj = {a, "b": 2, c: 3, [d]: 4, ...p};
        return obj;
        "#,
        JsValue::Null,
    );

    assert_eq!(
        result,
        fiber_json::parse(
            r#"{
                "a":1,
                "b":2,
                "c":3,
                "d":4,
                "e":5
            }"#
        )
        .unwrap()
    );
}

#[test]
fn nested_try_catch_with_for_of() {
    let result = run(
        "
        let nums = [1,2,3];
        let sum = 0;
        try {
            for (let _, n of nums) {
                try {
                    if (n == 2) {
                        throw 2;
                    }
                    if (n == 3) {
                        throw 3;
                    }
                    sum = sum + n;
                } catch (err) {
                    sum = sum + err;
                    if (err == 3) {
                        throw err;
                    }
                }
            }
        } catch (outer) {
            sum = sum - outer;
        }
        return sum;
        ",
        JsValue::Null,
    );

    // Flow:
    // n=1 => sum=1
    // n=2 => inner catch adds 2 => sum=3
    // n=3 => inner catch adds 3 => sum=6, rethrows 3, outer catch subtracts 3 => 3
    assert_eq!(result, JsValue::Int(3));
}

#[test]
fn for_of_over_object_iterates_keys_and_values() {
    let result = run(
        "
        let obj = {a:1, b:2, c:3};
        let res = {};
        for (let k, v of obj) {
            res[k] = v * 2;
        }
        return res;
        ",
        JsValue::Null,
    );

    assert_eq!(
        result,
        fiber_json::parse(
            r#"{
                "a":2,
                "b":4,
                "c":6
            }"#
        )
        .unwrap()
    );
}

#[test]
fn for_of_non_container_skips_iteration() {
    let result = run(
        "
        let sum = 0;
        for (let v of 123) {
            sum = sum + v;
        }
        for (let v of true) {
            sum = sum + 1;
        }
        return sum;
        ",
        JsValue::Null,
    );

    assert_eq!(result, JsValue::Int(0));
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
