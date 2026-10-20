---
paths:
  - "curios-abi/**"
  - "curios-native/src/host.rs"
  - "curios-native/src/os_host*"
  - "curios-native/src/os_host/**"
  - "curios-native/src/mock_host*"
  - "curios-native/src/mock_host/**"
  - "curios-browser/src/**"
---

# Host operations

- `curios-abi` is the source of truth for the host/guest wire. A host operation is complete only when its ABI row, the compiler's use, the native implementation and the JavaScript implementation agree.
