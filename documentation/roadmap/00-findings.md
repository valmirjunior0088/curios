# Findings

- **`Proxy::from_archivable` is fallible and never fails** (`curios-archive/src/proxy.rs`): its four implementors — interned symbols and qualifiers, `curios-num`'s integers, `curios-text`'s ordered map — each return `Ok`, and `Via`'s deserialization wraps an error path nothing reaches. Fix: make it infallible, or keep it with the first fallible proxy's reason. Check: the archive round-trip tests. Size: quick.
