---
paths:
  - "curios-abi/**"
  - "curios-runtime/src/host.rs"
  - "curios-runtime/src/os_host*"
  - "curios-runtime/src/os_host/**"
  - "curios-runtime/src/mock_host*"
  - "curios-runtime/src/mock_host/**"
  - "curios-js/src/**"
---

# Host operations

- `curios-abi` is the source of truth for the host/guest wire. A host operation is complete only when its ABI row, the compiler's use, the native implementation and the JavaScript implementation agree.
