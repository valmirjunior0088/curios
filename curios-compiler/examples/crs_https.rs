use {curios_compiler::run_text, curios_rt::OsHost, std::time::Duration};

// A network-gated manual check of the real TLS *client* path: a genuine
// `https://` GET against a public host. It exercises the whole stack end to end
// — `/sys/Io/start_tls` (SNI derived from the hostname, certificate
// verification against the bundled `webpki-roots`), the `ClientTls` conduit's
// encrypted `read`/`write`, and the `/std/Http` + `/std/Parse` response parser.
//
// This is deliberately NOT a `#[test]`: it depends on outbound network and a
// live endpoint, so it has no place in the hermetic suite (the mock host, which
// runs cleartext, covers the wiring; this covers the real handshake). Run it by
// hand to confirm a real connection works:
//
//     cargo run --example crs_https --features cli
fn main() {
    let source = r#"
        use /std/{Http, Io, Str, Nat, Task};

        match Task/block_on(Http/perform(Http/get_tls("example.com", 443, "/"))) : {}
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
