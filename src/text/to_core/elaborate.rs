use {
    super::{DefStack, ModuleInfo},
    crate::{
        core,
        text::{Bin, Match, Name, Nat, Prim, Term},
    },
    std::collections::HashMap,
};

pub struct Elaborate<'a> {
    qualifiers: &'a HashMap<String, Name>,
    bindings: &'a HashMap<String, Name>,
    table: &'a HashMap<Name, ModuleInfo>,
    aliases: &'a HashMap<Name, Name>,
    def_stack: &'a DefStack,
}

impl<'a> Elaborate<'a> {
    pub fn new(
        qualifiers: &'a HashMap<String, Name>,
        bindings: &'a HashMap<String, Name>,
        table: &'a HashMap<Name, ModuleInfo>,
        aliases: &'a HashMap<Name, Name>,
        def_stack: &'a DefStack,
    ) -> Self {
        Self {
            qualifiers,
            bindings,
            table,
            aliases,
            def_stack,
        }
    }

    pub fn term(&self, term: &Term) -> core::Term {
        match term {
            Term::Type => core::Term::Type,
            Term::Prim(prim) => core::Term::Prim(self.prim(prim)),
            Term::Name(name) => core::Var::free(match name.is_single() {
                true => {
                    let label = name.head();

                    if let Some(full) = self.bindings.get(label) {
                        full.join()
                    } else if let Some(full) = self.def_stack.get(label) {
                        full.join()
                    } else {
                        label.to_string()
                    }
                }
                false => self.resolve_name(name).join(),
            })
            .into(),
            Term::Atom(atom) => core::Term::Atom(core::Atom::from(atom.as_str())),
            Term::AtomType(at) => {
                core::AtomType::new(at.atoms.iter().map(|atom| core::Atom::from(atom.as_str())))
                    .into()
            }
            Term::FuncType(ft) => core::FuncType::new(
                ft.label.clone().unwrap_or_default(),
                self.term(&ft.input),
                self.term(&ft.output),
            )
            .into(),
            Term::Func(func) => core::Func::new(func.label.clone(), self.term(&func.body)).into(),
            Term::Apply(ap) => core::Apply::new(self.term(&ap.head), self.term(&ap.param)).into(),
            Term::TupleType(tt) => core::TupleType::new(
                tt.fields
                    .iter()
                    .map(|(label, type_)| (label.clone().unwrap_or_default(), self.term(type_))),
            )
            .into(),
            Term::Tuple(tuple) => {
                core::Tuple::new(tuple.fields.iter().map(|field| self.term(field))).into()
            }
            Term::Proj(proj) => core::Proj::new(self.term(&proj.head), proj.index).into(),
            Term::Match(match_) => match match_ {
                Match::Bln(bm) => core::BlnMatch::new(
                    self.term(&bm.head),
                    bm.motive.label.as_deref(),
                    self.term(&bm.motive.body),
                    self.term(&bm.false_case),
                    self.term(&bm.true_case),
                )
                .into(),
                Match::NatFold(nf) => core::NatFold::new(
                    self.term(&nf.head),
                    nf.motive.label.as_deref(),
                    self.term(&nf.motive.body),
                    self.term(&nf.zero_case),
                    nf.pred_label.clone(),
                    nf.ih_label.clone(),
                    self.term(&nf.succ_case),
                )
                .into(),
                Match::Nat(nm) => core::NatMatch::new(
                    self.term(&nm.head),
                    nm.motive.label.as_deref(),
                    self.term(&nm.motive.body),
                    nm.cases.iter().map(|(&nat, body)| (nat, self.term(body))),
                    self.term(&nm.default),
                )
                .into(),
                Match::Atom(am) => core::Match::new(
                    self.term(&am.head),
                    am.motive.label.as_deref(),
                    self.term(&am.motive.body),
                    am.cases
                        .iter()
                        .map(|(atom, body)| (core::Atom::from(atom.as_str()), self.term(body))),
                )
                .into(),
            },
            Term::DefFrom(from) => core::Seal::new(
                core::Var::free(
                    self.def_stack
                        .get(&from.label)
                        .unwrap_or_else(|| panic!("coercion outside def block: {}", from.label))
                        .join(),
                ),
                self.term(&from.body),
            )
            .into(),
            Term::DefInto(into) => core::Unseal::new(
                core::Var::free(
                    self.def_stack
                        .get(&into.label)
                        .unwrap_or_else(|| panic!("coercion outside def block: {}", into.label))
                        .join(),
                ),
                self.term(&into.body),
            )
            .into(),
            Term::Let(let_) => core::Let::new(
                let_.label.clone(),
                self.term(&let_.type_),
                self.term(&let_.body),
                self.term(&let_.tail),
            )
            .into(),
            Term::Rec(rec) => core::Rec::new(
                rec.items
                    .iter()
                    .map(|it| (it.label.clone(), self.term(&it.type_), self.term(&it.value))),
                self.term(&rec.tail),
            )
            .into(),
        }
    }

    pub fn prim(&self, prim: &Prim) -> core::Prim {
        match prim {
            Prim::BlnType => core::Prim::BlnType,
            Prim::Bln(b) => core::Prim::Bln(*b),
            Prim::NatType => core::Prim::NatType,
            Prim::Nat(Nat::Number(number)) => core::Prim::Nat(*number),
            Prim::Nat(Nat::Char(character)) => core::Prim::Nat(*character as u32),
            Prim::NatEql(left, right) => core::Prim::nat_eql(self.term(left), self.term(right)),
            Prim::NatNeq(left, right) => core::Prim::nat_neq(self.term(left), self.term(right)),
            Prim::NatAdd(left, right) => core::Prim::nat_add(self.term(left), self.term(right)),
            Prim::NatSub(left, right) => core::Prim::nat_sub(self.term(left), self.term(right)),
            Prim::NatMul(left, right) => core::Prim::nat_mul(self.term(left), self.term(right)),
            Prim::NatLt(left, right) => core::Prim::nat_lt(self.term(left), self.term(right)),
            Prim::NatDiv(left, right) => core::Prim::nat_div(self.term(left), self.term(right)),
            Prim::NatRem(left, right) => core::Prim::nat_rem(self.term(left), self.term(right)),
            Prim::NatGt(left, right) => core::Prim::nat_gt(self.term(left), self.term(right)),
            Prim::NatLte(left, right) => core::Prim::nat_lte(self.term(left), self.term(right)),
            Prim::NatGte(left, right) => core::Prim::nat_gte(self.term(left), self.term(right)),
            Prim::IntType => core::Prim::IntType,
            Prim::Int(value) => core::Prim::Int(*value),
            Prim::IntEql(left, right) => core::Prim::int_eql(self.term(left), self.term(right)),
            Prim::IntNeq(left, right) => core::Prim::int_neq(self.term(left), self.term(right)),
            Prim::IntAdd(left, right) => core::Prim::int_add(self.term(left), self.term(right)),
            Prim::IntSub(left, right) => core::Prim::int_sub(self.term(left), self.term(right)),
            Prim::IntMul(left, right) => core::Prim::int_mul(self.term(left), self.term(right)),
            Prim::IntDiv(left, right) => core::Prim::int_div(self.term(left), self.term(right)),
            Prim::IntRem(left, right) => core::Prim::int_rem(self.term(left), self.term(right)),
            Prim::IntLt(left, right) => core::Prim::int_lt(self.term(left), self.term(right)),
            Prim::IntGt(left, right) => core::Prim::int_gt(self.term(left), self.term(right)),
            Prim::IntLte(left, right) => core::Prim::int_lte(self.term(left), self.term(right)),
            Prim::IntGte(left, right) => core::Prim::int_gte(self.term(left), self.term(right)),
            Prim::FltType => core::Prim::FltType,
            Prim::Flt(flt) => core::Prim::Flt(core::Flt::from_f32(*flt)),
            Prim::FltAdd(left, right) => core::Prim::flt_add(self.term(left), self.term(right)),
            Prim::FltSub(left, right) => core::Prim::flt_sub(self.term(left), self.term(right)),
            Prim::FltMul(left, right) => core::Prim::flt_mul(self.term(left), self.term(right)),
            Prim::FltDiv(left, right) => core::Prim::flt_div(self.term(left), self.term(right)),
            Prim::FltEql(left, right) => core::Prim::flt_eql(self.term(left), self.term(right)),
            Prim::FltNeq(left, right) => core::Prim::flt_neq(self.term(left), self.term(right)),
            Prim::FltLt(left, right) => core::Prim::flt_lt(self.term(left), self.term(right)),
            Prim::FltGt(left, right) => core::Prim::flt_gt(self.term(left), self.term(right)),
            Prim::FltLte(left, right) => core::Prim::flt_lte(self.term(left), self.term(right)),
            Prim::FltGte(left, right) => core::Prim::flt_gte(self.term(left), self.term(right)),
            Prim::FltMin(left, right) => core::Prim::flt_min(self.term(left), self.term(right)),
            Prim::FltMax(left, right) => core::Prim::flt_max(self.term(left), self.term(right)),
            Prim::FltNeg(inner) => core::Prim::flt_neg(self.term(inner)),
            Prim::FltAbs(inner) => core::Prim::flt_abs(self.term(inner)),
            Prim::FltSqrt(inner) => core::Prim::flt_sqrt(self.term(inner)),
            Prim::FltFloor(inner) => core::Prim::flt_floor(self.term(inner)),
            Prim::FltCeil(inner) => core::Prim::flt_ceil(self.term(inner)),
            Prim::FltTrunc(inner) => core::Prim::flt_trunc(self.term(inner)),
            Prim::FltNearest(inner) => core::Prim::flt_nearest(self.term(inner)),
            Prim::NatToStr(inner) => core::Prim::nat_to_str(self.term(inner)),
            Prim::SysPrint(inner) => core::Prim::sys_print(self.term(inner)),
            Prim::SysRead => core::Prim::SysRead,
            Prim::IntToStr(inner) => core::Prim::int_to_str(self.term(inner)),
            Prim::FltToStr(inner) => core::Prim::flt_to_str(self.term(inner)),
            Prim::NatToInt(inner) => core::Prim::nat_to_int(self.term(inner)),
            Prim::NatToFlt(inner) => core::Prim::nat_to_flt(self.term(inner)),
            Prim::IntToNat(inner) => core::Prim::int_to_nat(self.term(inner)),
            Prim::IntToFlt(inner) => core::Prim::int_to_flt(self.term(inner)),
            Prim::FltToNat(inner) => core::Prim::flt_to_nat(self.term(inner)),
            Prim::FltToInt(inner) => core::Prim::flt_to_int(self.term(inner)),
            Prim::BinType => core::Prim::BinType,
            Prim::Bin(Bin::Bytes(bytes)) => core::Prim::Bin(bytes.clone()),
            Prim::Bin(Bin::String(string)) => core::Prim::Bin(string.as_bytes().to_vec()),
            Prim::BinLen(inner) => core::Prim::bin_len(self.term(inner)),
            Prim::BinEql(left, right) => core::Prim::bin_eql(self.term(left), self.term(right)),
            Prim::BinGet(bin, index) => core::Prim::bin_get(self.term(bin), self.term(index)),
            Prim::BinSlice(bin, start, end) => {
                core::Prim::bin_slice(self.term(bin), self.term(start), self.term(end))
            }
            Prim::BinAppend(bin, byte) => core::Prim::bin_append(self.term(bin), self.term(byte)),
            Prim::BinConcat(operands) => {
                core::Prim::bin_concat(operands.iter().map(|operand| self.term(operand)))
            }
            Prim::ArrType(inner) => core::Prim::arr_type(self.term(inner)),
            Prim::Arr(elems) => {
                core::Prim::Arr(elems.iter().map(|elem| self.term(elem).into()).collect())
            }
            Prim::ArrLen(inner) => core::Prim::arr_len(self.term(inner)),
            Prim::ArrGet(list, index) => core::Prim::arr_get(self.term(list), self.term(index)),
            Prim::ArrSlice(list, start, end) => {
                core::Prim::arr_slice(self.term(list), self.term(start), self.term(end))
            }
            Prim::ArrAppend(list, elem) => core::Prim::arr_append(self.term(list), self.term(elem)),
            Prim::ArrConcat(operands) => {
                core::Prim::arr_concat(operands.iter().map(|operand| self.term(operand)))
            }
        }
    }

    fn resolve_name(&self, name: &Name) -> Name {
        let qualifier = name.head();

        let mut current = self
            .qualifiers
            .get(qualifier)
            .unwrap_or_else(|| panic!("unresolved qualifier: {qualifier}"))
            .clone();

        for segment in name.interior() {
            let info = self
                .table
                .get(&current)
                .unwrap_or_else(|| panic!("module not found: {}", current.join()));

            let is_pub = info
                .get_child(segment)
                .unwrap_or_else(|| panic!("child module not found: {segment}"));

            if !is_pub {
                panic!("private child module: {segment}");
            }

            current = current.with(segment);

            if let Some(canonical) = self.aliases.get(&current) {
                current = canonical.clone();
            }
        }

        let last = name.last();

        let info = self
            .table
            .get(&current)
            .unwrap_or_else(|| panic!("module not found: {}", current.join()));

        let is_pub = info
            .get_binding(last)
            .unwrap_or_else(|| panic!("binding not found: {last}"));

        if !is_pub {
            panic!("private binding: {last}");
        }

        current.with(last)
    }
}
