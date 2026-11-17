//! A reading over a certifier for the suites that drive a walk behind its doors directly.

use super::*;

/// Run `read` in a reading of `certifier`, its terms handed as the suite wrote them.
pub fn reading<T>(certifier: &mut Certifier, read: impl FnOnce(&mut ByValue) -> T) -> T {
    read(&mut ByValue {
        certifier,
        plain: false,
    })
}
