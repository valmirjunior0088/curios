use super::{Flt, Int, Nat, Term};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Prim {
    BlnType,
    Bln(bool),
    BlnAnd(Term, Term),
    BlnOr(Term, Term),
    BlnXor(Term, Term),
    NatType,
    Nat(Nat),
    NatEql(Term, Term),
    NatNeq(Term, Term),
    NatAdd(Term, Term),
    NatSub(Term, Term),
    NatMul(Term, Term),
    NatLt(Term, Term),
    NatDiv(Term, Term),
    NatRem(Term, Term),
    NatGt(Term, Term),
    NatLte(Term, Term),
    NatGte(Term, Term),
    NatAnd(Term, Term),
    NatOr(Term, Term),
    NatXor(Term, Term),
    NatShl(Term, Term),
    NatShr(Term, Term),
    NatToStr(Term),
    IntType,
    Int(Int),
    IntEql(Term, Term),
    IntNeq(Term, Term),
    IntAdd(Term, Term),
    IntSub(Term, Term),
    IntMul(Term, Term),
    IntDiv(Term, Term),
    IntRem(Term, Term),
    IntLt(Term, Term),
    IntGt(Term, Term),
    IntLte(Term, Term),
    IntGte(Term, Term),
    IntAnd(Term, Term),
    IntOr(Term, Term),
    IntXor(Term, Term),
    IntShl(Term, Term),
    IntShr(Term, Term),
    IntToStr(Term),
    FltType,
    Flt(Flt),
    FltAdd(Term, Term),
    FltSub(Term, Term),
    FltMul(Term, Term),
    FltDiv(Term, Term),
    FltEql(Term, Term),
    FltNeq(Term, Term),
    FltLt(Term, Term),
    FltGt(Term, Term),
    FltLte(Term, Term),
    FltGte(Term, Term),
    FltMin(Term, Term),
    FltMax(Term, Term),
    FltNeg(Term),
    FltAbs(Term),
    FltSqrt(Term),
    FltFloor(Term),
    FltCeil(Term),
    FltTrunc(Term),
    FltNearest(Term),
    FltToStr(Term),
    NatToInt(Term),
    NatToFlt(Term),
    IntToNat(Term),
    IntToFlt(Term),
    FltToNat(Term),
    FltToLeBin(Term),
    FltToInt(Term),
    BinType,
    Bin(Vec<u8>),
    BinLen(Term),
    BinEql(Term, Term),
    BinGet(Term, Term),
    BinSlice(Term, Term, Term),
    BinAppend(Term, Term),
    BinConcat(Vec<Term>),
    BinFlatten(Term),
    StrType,
    Str(Vec<u8>),
    StrToBin(Term),
    StrOfBin(Term),
    ArrType(Term),
    Arr(Vec<Term>),
    ArrLen(Term, Term),
    ArrGet(Term, Term, Term),
    ArrSlice(Term, Term, Term, Term),
    ArrAppend(Term, Term, Term),
    ArrConcat(Term, Vec<Term>),
    ArrFlatten(Term, Term),
    // (@A, @B, f : (A) -> B, arr : Arr(A)) -> Arr(B): a structural map. Opaque
    // under reduction (a stuck chunk like `ArrFlatten`), so it never unfolds a
    // literal spine during type-checking. Erases to a single O(n) fill loop.
    ArrMap(Term, Term, Term, Term),
    IoType,
    Io(u32),
    // (a, b) -> Bln: identity of two handles. The one pure operation on `Io` --
    // handles are opaque i31 tokens, so this erases to the `Nat` equality op.
    IoEql(Term, Term),
    IoRead(Term, Term),
    IoWrite(Term, Term),
    IoOpen(Term, Term),
    // (host, port) -> { status, addresses }: resolve a host:port to a list of
    // opaque address blobs the socket lifecycle consumes. The blobs are the
    // host's private encoding (it derives the address family from them); the
    // guest only shuttles them back into `socket`/`bind`/`connect`.
    IoResolve(Term, Term),
    // (addr) -> { status, handle }: create an unconnected socket for the address
    // family encoded in `addr`. The handle is a bare `Io`, configured via the
    // setters before `bind`/`connect`/`listen` transition it.
    IoSocket(Term),
    // (handle, addr) -> Nat status: bind a socket to a local address.
    IoBind(Term, Term),
    // (handle, addr) -> Nat status: connect a socket to a resolved address. The
    // handle then serves the read/write/close plumbing like any byte stream.
    IoConnect(Term, Term),
    // (handle, backlog) -> Nat status: mark a bound socket as listening with the
    // given accept-queue depth (OS-clamped to somaxconn). `accept` pulls
    // connections from it and `close` releases it.
    IoListen(Term, Term),
    // (handle) -> { status, handle }: pull the next connection from a listener.
    // The returned handle is an ordinary `Io` the read/write/close plumbing
    // serves, exactly like a `connect`ed socket.
    IoAccept(Term),
    // (handle, sni) -> Nat status: upgrade a connected socket to a TLS client
    // stream in place, running the handshake inline. The same handle then
    // serves the read/write/close plumbing as an encrypted byte stream.
    IoStartTls(Term, Term),
    // (cert, key) -> { status, handle }: build an opaque server TLS config from
    // a PEM certificate chain and key. The handle is a host-owned config token
    // (no socket), consumed by `start_tls_server` and released by `close`.
    IoTlsServerConfig(Term, Term),
    // (handle, cfg) -> Nat status: upgrade an accepted socket to a TLS server
    // stream in place using a config token, running the handshake inline.
    IoStartTlsServer(Term, Term),
    // (handle, on) -> Nat status: set the handle's non-blocking flag (`on` is a
    // `Bln` riding the i31 carrier). fcntl O_NONBLOCK.
    IoSetNonblocking(Term, Term),
    // (handle, ms) -> Nat status: SO_RCVTIMEO; `0` ms clears the timeout.
    IoSetRecvTimeout(Term, Term),
    // (handle, ms) -> Nat status: SO_SNDTIMEO; `0` ms clears the timeout.
    IoSetSendTimeout(Term, Term),
    // (handle, on) -> Nat status: SO_REUSEADDR (`on` a `Bln`); set before bind.
    IoSetReuseaddr(Term, Term),
    // (handles, events, timeout) -> Arr(Nat) revents: the readiness oracle. The
    // `handles : Arr(Io)` and `events : Arr(Nat)` are parallel — `events[i]` is
    // the interest bitmask for `handles[i]` — and the result is parallel too.
    // `timeout : Int` mirrors `poll(2)`: `< 0` waits forever, `0` returns
    // immediately, `> 0` waits that many milliseconds.
    IoPoll(Term, Term, Term),
    IoClose(Term),
    IoClockWall,
    IoClockMono,
    IoRandom(Term),
    // argv as an immutable snapshot: inert at the type level (reduce-to-self,
    // like `Io(token)`), a host call only at erasure.
    IoArgs,
    IoEnv(Term),
    // `(@A : Type) -> Nat -> A`: polymorphic bottom. The type argument keeps the
    // kernel from naming `/std/Void`; it is dropped at erasure.
    IoExit(Term, Term),
    CellType(Term),
    Cell(Term, Term),          // type, init
    CellSet(Term, Term, Term), // type, cell, value
    CellGet(Term, Term),       // type, cell
}

impl Prim {
    pub fn nat_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatEql(left.into(), right.into())
    }

    pub fn io_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IoEql(left.into(), right.into())
    }

    pub fn nat_neq<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatNeq(left.into(), right.into())
    }

    pub fn nat_add<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatAdd(left.into(), right.into())
    }

    pub fn nat_sub<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatSub(left.into(), right.into())
    }

    pub fn nat_mul<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatMul(left.into(), right.into())
    }

    pub fn nat_div<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatDiv(left.into(), right.into())
    }

    pub fn nat_rem<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatRem(left.into(), right.into())
    }

    pub fn nat_lt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatLt(left.into(), right.into())
    }

    pub fn nat_gt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatGt(left.into(), right.into())
    }

    pub fn nat_lte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatLte(left.into(), right.into())
    }

    pub fn nat_gte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::NatGte(left.into(), right.into())
    }

    pub fn int_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntEql(left.into(), right.into())
    }

    pub fn int_add<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntAdd(left.into(), right.into())
    }

    pub fn int_sub<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntSub(left.into(), right.into())
    }

    pub fn int_mul<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntMul(left.into(), right.into())
    }

    pub fn int_neq<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntNeq(left.into(), right.into())
    }

    pub fn int_div<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntDiv(left.into(), right.into())
    }

    pub fn int_rem<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntRem(left.into(), right.into())
    }

    pub fn int_lt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntLt(left.into(), right.into())
    }

    pub fn int_gt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntGt(left.into(), right.into())
    }

    pub fn int_lte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntLte(left.into(), right.into())
    }

    pub fn int_gte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::IntGte(left.into(), right.into())
    }

    pub fn flt_add<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltAdd(left.into(), right.into())
    }

    pub fn flt_sub<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltSub(left.into(), right.into())
    }

    pub fn flt_mul<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltMul(left.into(), right.into())
    }

    pub fn flt_neg<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltNeg(inner.into())
    }

    pub fn flt_abs<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltAbs(inner.into())
    }

    pub fn flt_sqrt<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltSqrt(inner.into())
    }

    pub fn flt_floor<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltFloor(inner.into())
    }

    pub fn flt_ceil<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltCeil(inner.into())
    }

    pub fn flt_trunc<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltTrunc(inner.into())
    }

    pub fn flt_nearest<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltNearest(inner.into())
    }

    pub fn flt_div<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltDiv(left.into(), right.into())
    }

    pub fn flt_min<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltMin(left.into(), right.into())
    }

    pub fn flt_max<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltMax(left.into(), right.into())
    }

    pub fn flt_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltEql(left.into(), right.into())
    }

    pub fn flt_neq<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltNeq(left.into(), right.into())
    }

    pub fn flt_lt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltLt(left.into(), right.into())
    }

    pub fn flt_gt<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltGt(left.into(), right.into())
    }

    pub fn flt_lte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltLte(left.into(), right.into())
    }

    pub fn flt_gte<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::FltGte(left.into(), right.into())
    }

    pub fn nat_to_int<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::NatToInt(inner.into())
    }

    pub fn nat_to_str<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::NatToStr(inner.into())
    }

    pub fn int_to_nat<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::IntToNat(inner.into())
    }

    pub fn int_to_flt<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::IntToFlt(inner.into())
    }

    pub fn int_to_str<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::IntToStr(inner.into())
    }

    pub fn nat_to_flt<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::NatToFlt(inner.into())
    }

    pub fn flt_to_int<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltToInt(inner.into())
    }

    pub fn flt_to_nat<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltToNat(inner.into())
    }

    pub fn flt_to_str<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltToStr(inner.into())
    }

    pub fn flt_to_le_bin<T>(inner: T) -> Self
    where
        T: Into<Term>,
    {
        Self::FltToLeBin(inner.into())
    }

    pub fn bin_len<B>(bin: B) -> Self
    where
        B: Into<Term>,
    {
        Self::BinLen(bin.into())
    }

    pub fn bin_eql<F, S>(left: F, right: S) -> Self
    where
        F: Into<Term>,
        S: Into<Term>,
    {
        Self::BinEql(left.into(), right.into())
    }

    pub fn bin_get<B, I>(bin: B, index: I) -> Self
    where
        B: Into<Term>,
        I: Into<Term>,
    {
        Self::BinGet(bin.into(), index.into())
    }

    pub fn bin_slice<B, S, E>(bin: B, start: S, end: E) -> Self
    where
        B: Into<Term>,
        S: Into<Term>,
        E: Into<Term>,
    {
        Self::BinSlice(bin.into(), start.into(), end.into())
    }

    pub fn bin_append<B, E>(bin: B, byte: E) -> Self
    where
        B: Into<Term>,
        E: Into<Term>,
    {
        Self::BinAppend(bin.into(), byte.into())
    }

    pub fn bin_concat<I>(operands: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<Term>,
    {
        Self::BinConcat(operands.into_iter().map(|e| e.into()).collect())
    }

    pub fn bin_flatten<A>(array: A) -> Self
    where
        A: Into<Term>,
    {
        Self::BinFlatten(array.into())
    }

    pub fn str_to_bin<S>(str: S) -> Self
    where
        S: Into<Term>,
    {
        Self::StrToBin(str.into())
    }

    pub fn str_of_bin<B>(bin: B) -> Self
    where
        B: Into<Term>,
    {
        Self::StrOfBin(bin.into())
    }

    pub fn arr<I, A>(items: I) -> Self
    where
        I: IntoIterator<Item = A>,
        A: Into<Term>,
    {
        Self::Arr(items.into_iter().map(Into::into).collect())
    }

    pub fn arr_type<T>(elem: T) -> Self
    where
        T: Into<Term>,
    {
        Self::ArrType(elem.into())
    }

    pub fn arr_len<T, L>(type_: T, list: L) -> Self
    where
        T: Into<Term>,
        L: Into<Term>,
    {
        Self::ArrLen(type_.into(), list.into())
    }

    pub fn arr_get<T, L, I>(type_: T, list: L, index: I) -> Self
    where
        T: Into<Term>,
        L: Into<Term>,
        I: Into<Term>,
    {
        Self::ArrGet(type_.into(), list.into(), index.into())
    }

    pub fn arr_slice<T, L, S, E>(type_: T, list: L, start: S, end: E) -> Self
    where
        T: Into<Term>,
        L: Into<Term>,
        S: Into<Term>,
        E: Into<Term>,
    {
        Self::ArrSlice(type_.into(), list.into(), start.into(), end.into())
    }

    pub fn arr_append<T, L, E>(type_: T, list: L, elem: E) -> Self
    where
        T: Into<Term>,
        L: Into<Term>,
        E: Into<Term>,
    {
        Self::ArrAppend(type_.into(), list.into(), elem.into())
    }

    pub fn arr_concat<T, O>(type_: T, operands: O) -> Self
    where
        T: Into<Term>,
        O: IntoIterator,
        O::Item: Into<Term>,
    {
        Self::ArrConcat(
            type_.into(),
            operands.into_iter().map(|e| e.into()).collect(),
        )
    }

    pub fn arr_flatten<T, A>(type_: T, array: A) -> Self
    where
        T: Into<Term>,
        A: Into<Term>,
    {
        Self::ArrFlatten(type_.into(), array.into())
    }

    pub fn arr_map<A, B, F, R>(a: A, b: B, f: F, arr: R) -> Self
    where
        A: Into<Term>,
        B: Into<Term>,
        F: Into<Term>,
        R: Into<Term>,
    {
        Self::ArrMap(a.into(), b.into(), f.into(), arr.into())
    }

    pub fn io_read<H, N>(handle: H, count: N) -> Self
    where
        H: Into<Term>,
        N: Into<Term>,
    {
        Self::IoRead(handle.into(), count.into())
    }

    pub fn io_write<H, B>(handle: H, bytes: B) -> Self
    where
        H: Into<Term>,
        B: Into<Term>,
    {
        Self::IoWrite(handle.into(), bytes.into())
    }

    pub fn cell_type<T>(elem: T) -> Self
    where
        T: Into<Term>,
    {
        Self::CellType(elem.into())
    }

    pub fn cell_new<T, I>(type_: T, init: I) -> Self
    where
        T: Into<Term>,
        I: Into<Term>,
    {
        Self::Cell(type_.into(), init.into())
    }

    pub fn cell_set<T, C, V>(type_: T, cell: C, value: V) -> Self
    where
        T: Into<Term>,
        C: Into<Term>,
        V: Into<Term>,
    {
        Self::CellSet(type_.into(), cell.into(), value.into())
    }

    pub fn cell_get<T, C>(type_: T, cell: C) -> Self
    where
        T: Into<Term>,
        C: Into<Term>,
    {
        Self::CellGet(type_.into(), cell.into())
    }
}
