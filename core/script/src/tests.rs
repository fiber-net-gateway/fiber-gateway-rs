use super::*;
use std::pin::Pin;
use std::task::Context;
use fiber_json::JsValue;

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
        let payload = unsafe { ctx.attach_as::<Payload>() }.map(|p| p.0).unwrap_or(0);
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
