use {curios_compiler::run_text, curios_runtime::OsHost, std::time::Duration};

// A network-gated manual check of the cleartext HTTP client path: a real
// `http://` GET against a public host through the real `OsHost`. It drives a
// live connect / GET / response-parse round trip through `/std/Http` +
// `/std/Tcp` + `/std/Parse`, end to end through codegen. The TLS sibling is
// `crs_https`.
//
// Deliberately not a `#[test]`: it depends on outbound network and a live
// endpoint, so it has no place in the hermetic suite (the mock host covers the
// wiring; `http_perform_parses_a_scripted_response` pins the exact parse). Run
// it by hand to confirm a real connection works:
//
//     cargo run --example crs_http --features cli
fn main() {
    let source = r#"
        use /std/{Http, Io, Str, Nat, Task};

        match Task/block_on(Http/perform(Http/get("example.com", 80, "/"))) : {}
        | success(response) =>
            Io/print(Str/flatten([
                "status: ",
                Nat/to_str(response.status.code),
                " ",
                response.status.reason
            ]))
        | failure(_) => Io/print("request failed")
        end
        "#;

    run_text(Duration::from_secs(30), source, OsHost::new()).expect("expected result");
}
