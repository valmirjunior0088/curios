//! A manifest's parsed text read into the shape a manifest is declared by, refused as every other refusal of a manifest is: what serde's derived readers find wrong kept as data by its kind, worded here, and placed by the keys that lead to it.
//!
//! **This reader rather than toml's.** toml's reader words each refusal for a reader of its own, `unknown field `nmae`, expected one of …`, and keeps only that text, so a manifest of the wrong shape was refused as a `syntax error` in toml's words and Rust's names for the types. Reading the table toml parsed, each refusal keeps what went wrong and the keys it stands under, and what a key takes is said by the method serde calls for it rather than by serde's text.

#[cfg(test)]
mod tests;

use {
    crate::{Refused, Step},
    curios_common::{Code, nearest},
    serde::de::{
        self, DeserializeOwned, DeserializeSeed, Deserializer, IntoDeserializer, MapAccess,
        SeqAccess, Visitor,
    },
    std::fmt,
    toml::{Table, Value},
};

/// `table`, a manifest's parsed text, read as a `T`, or refused where it is not one.
pub fn decode<T: DeserializeOwned>(table: Table) -> Result<T, Refused> {
    T::deserialize(Node {
        value: Value::Table(table),
        at: Vec::new(),
    })
    .map_err(Unread::refused)
}

/// Why a manifest is not the shape it is read into, as serde's readers ask of an error, kept by its kind, and the keys that lead to where it stands, once the innermost reader that knows them has set them.
#[derive(Debug)]
pub struct Unread {
    why: Why,
    at: Option<Vec<Step>>,
}

#[derive(Debug)]
enum Why {
    UnknownField {
        field: String,
        expected: &'static [&'static str],
    },
    MissingField(&'static str),
    /// A value of another kind than its key takes: what was written, and what the key takes, each in TOML's words.
    Kind {
        written: String,
        takes: &'static str,
    },
    /// A string none of the ones its key takes.
    UnknownValue {
        written: String,
        expected: &'static [&'static str],
    },
    /// What a key's own reader says of its value, which it read.
    Said(String),
}

impl Unread {
    fn of(why: Why) -> Self {
        Self { why, at: None }
    }

    /// This error, standing at `at` unless an inner reader already said where.
    fn at(mut self, at: &[Step]) -> Self {
        self.at.get_or_insert_with(|| at.to_vec());
        self
    }

    /// This error as the manifest's refusal: a field at its key, a value at itself, a missing one at the table that misses it.
    fn refused(self) -> Refused {
        let at = self.at.unwrap_or_default();
        match self.why {
            Why::UnknownField { field, expected } => {
                let refused = Refused::of("unknown field").stating(format_args!(
                    "`{field}` is no field of {}",
                    table(&at[..at.len().saturating_sub(1)])
                ));
                let refused = match expected {
                    [] => refused.stating("it takes no field"),
                    expected => refused.stating(format_args!("it takes: {}", listed(expected))),
                };
                let refused = match nearest(&field, expected.iter().copied()) {
                    Some(near) => refused.stating(format_args!("the nearest: {}", Code(near))),
                    None => refused,
                };

                refused.entry(at)
            }
            Why::MissingField(field) => {
                let refused = Refused::of("missing field")
                    .stating(format_args!("{} requires `{field}`", table(&at)));

                match at.is_empty() {
                    true => refused,
                    false => refused.entry(at),
                }
            }
            Why::Kind { written, takes } => misshapen(&written, takes, at),
            Why::UnknownValue { written, expected } => Refused::of("invalid value")
                .stating(format_args!("written: {}", Code(written)))
                .stating(format_args!(
                    "{} one of: {}",
                    taker(&at),
                    expected
                        .iter()
                        .map(|value| Code(format!("{value:?}")).to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                ))
                .value(at),
            Why::Said(said) => Refused::of("invalid value").stating(said).value(at),
        }
    }
}

impl fmt::Display for Unread {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{:?}", self.why)
    }
}

impl std::error::Error for Unread {}

impl de::Error for Unread {
    fn custom<T: fmt::Display>(said: T) -> Self {
        Self::of(Why::Said(said.to_string()))
    }

    fn unknown_field(field: &str, expected: &'static [&'static str]) -> Self {
        Self::of(Why::UnknownField {
            field: field.to_string(),
            expected,
        })
    }

    fn missing_field(field: &'static str) -> Self {
        Self::of(Why::MissingField(field))
    }

    fn unknown_variant(variant: &str, expected: &'static [&'static str]) -> Self {
        Self::of(Why::UnknownValue {
            written: format!("{variant:?}"),
            expected,
        })
    }
}

/// The refusal of a value of another kind than the key `at` leads to takes: what was `written`, in TOML's words — a table or an array by its kind, a scalar as it reads — and what the key `takes`, standing at the value.
pub fn misshapen(written: &str, takes: &str, at: Vec<Step>) -> Refused {
    Refused::of("invalid value")
        .stating(format_args!("written: {written}"))
        .stating(format_args!("{} {takes}", taker(&at)))
        .value(at)
}

/// The table `at` leads to as TOML heads it: the manifest itself, a table by its header, a row of an array of tables as one of its rows.
fn table(at: &[Step]) -> String {
    let header = at
        .iter()
        .filter_map(|step| match step {
            Step::Key(key) => Some(key.as_str()),
            Step::Row(_) | Step::Named(_) => None,
        })
        .collect::<Vec<_>>()
        .join(".");

    match at.last() {
        None => "the manifest".to_string(),
        Some(Step::Key(_)) => format!("`[{header}]`"),
        Some(Step::Row(_) | Step::Named(_)) => format!("an `[[{header}]]` row"),
    }
}

/// What takes the value `at` leads to, as a fact's subject: a field by its name, an entry of an array as one.
fn taker(at: &[Step]) -> String {
    let key = at.iter().rev().find_map(|step| match step {
        Step::Key(key) => Some(key.as_str()),
        Step::Row(_) | Step::Named(_) => None,
    });

    match (at.last(), key) {
        (Some(Step::Key(_)), Some(key)) => format!("`{key}` is"),
        (_, Some(key)) => format!("each entry of `{key}` is"),
        (_, None) => "the manifest is".to_string(),
    }
}

/// `names`, each quoted, in the order given.
fn listed(names: &[&str]) -> String {
    names
        .iter()
        .map(|name| Code(name).to_string())
        .collect::<Vec<_>>()
        .join(", ")
}

/// A value of the parsed manifest and the keys that lead to it.
struct Node {
    value: Value,
    at: Vec<Step>,
}

impl Node {
    /// The refusal of this value where a key takes `takes`: a scalar as written, a table or an array by its kind.
    fn kind(self, takes: &'static str) -> Unread {
        let written = match &self.value {
            Value::Table(_) => "a table".to_string(),
            Value::Array(_) => "an array".to_string(),
            Value::String(string) => Code(format!("{string:?}")).to_string(),
            Value::Integer(integer) => Code(integer).to_string(),
            Value::Float(float) => Code(float).to_string(),
            Value::Boolean(boolean) => Code(boolean).to_string(),
            Value::Datetime(datetime) => Code(datetime).to_string(),
        };

        Unread::of(Why::Kind { written, takes }).at(&self.at)
    }

    /// This value read by `visitor` where it takes a whole number.
    fn whole<'de, V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Unread> {
        match self.value {
            Value::Integer(integer) => visitor
                .visit_i64::<Unread>(integer)
                .map_err(|error| error.at(&self.at)),
            _ => Err(self.kind("a whole number")),
        }
    }
}

impl<'de> Deserializer<'de> for Node {
    type Error = Unread;

    fn deserialize_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Unread> {
        let at = self.at.clone();
        match self.value {
            Value::String(string) => visitor.visit_string::<Unread>(string),
            Value::Integer(integer) => visitor.visit_i64::<Unread>(integer),
            Value::Float(float) => visitor.visit_f64(float),
            Value::Boolean(boolean) => visitor.visit_bool::<Unread>(boolean),
            Value::Datetime(datetime) => visitor.visit_string(datetime.to_string()),
            Value::Array(rows) => visitor.visit_seq(Rows {
                rows: rows.into_iter().enumerate(),
                at: at.clone(),
            }),
            Value::Table(fields) => visitor.visit_map(Fields {
                fields: fields.into_iter(),
                value: None,
                at: at.clone(),
            }),
        }
        .map_err(|error| error.at(&at))
    }

    fn deserialize_bool<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Unread> {
        match self.value {
            Value::Boolean(boolean) => visitor
                .visit_bool::<Unread>(boolean)
                .map_err(|error| error.at(&self.at)),
            _ => Err(self.kind("a boolean")),
        }
    }

    fn deserialize_i8<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Unread> {
        self.whole(visitor)
    }

    fn deserialize_i16<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Unread> {
        self.whole(visitor)
    }

    fn deserialize_i32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Unread> {
        self.whole(visitor)
    }

    fn deserialize_i64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Unread> {
        self.whole(visitor)
    }

    fn deserialize_u8<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Unread> {
        self.whole(visitor)
    }

    fn deserialize_u16<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Unread> {
        self.whole(visitor)
    }

    fn deserialize_u32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Unread> {
        self.whole(visitor)
    }

    // A count is never below zero, so a negative one is a value of another kind than the key takes, said as the others are rather than by serde's `u64`.
    fn deserialize_u64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Unread> {
        match self.value {
            Value::Integer(integer) if integer >= 0 => self.whole(visitor),
            _ => Err(self.kind("a whole number")),
        }
    }

    fn deserialize_str<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Unread> {
        match self.value {
            Value::String(string) => visitor
                .visit_string::<Unread>(string)
                .map_err(|error| error.at(&self.at)),
            _ => Err(self.kind("a string")),
        }
    }

    fn deserialize_string<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Unread> {
        self.deserialize_str(visitor)
    }

    // A key present is a value present: a field the manifest leaves out never reaches a reader, serde's derive answering it `None`.
    fn deserialize_option<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Unread> {
        visitor.visit_some(self)
    }

    fn deserialize_newtype_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Unread> {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Unread> {
        match self.value {
            Value::Array(rows) => visitor
                .visit_seq(Rows {
                    rows: rows.into_iter().enumerate(),
                    at: self.at.clone(),
                })
                .map_err(|error| error.at(&self.at)),
            _ => Err(self.kind("an array")),
        }
    }

    fn deserialize_map<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Unread> {
        match self.value {
            Value::Table(fields) => visitor
                .visit_map(Fields {
                    fields: fields.into_iter(),
                    value: None,
                    at: self.at.clone(),
                })
                .map_err(|error| error.at(&self.at)),
            _ => Err(self.kind("a table")),
        }
    }

    fn deserialize_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Unread> {
        self.deserialize_map(visitor)
    }

    // An enum of the manifest is one of the strings it names, each a unit variant.
    fn deserialize_enum<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Unread> {
        match self.value {
            Value::String(string) => visitor
                .visit_enum(IntoDeserializer::<Unread>::into_deserializer(string))
                .map_err(|error| error.at(&self.at)),
            _ => Err(self.kind("a string")),
        }
    }

    fn deserialize_ignored_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Unread> {
        visitor.visit_unit()
    }

    serde::forward_to_deserialize_any! {
        i128 u128 f32 f64 char bytes byte_buf unit unit_struct tuple tuple_struct identifier
    }
}

/// The rows of an array, each at its position.
struct Rows {
    rows: std::iter::Enumerate<std::vec::IntoIter<Value>>,
    at: Vec<Step>,
}

impl<'de> SeqAccess<'de> for Rows {
    type Error = Unread;

    fn next_element_seed<T: DeserializeSeed<'de>>(
        &mut self,
        seed: T,
    ) -> Result<Option<T::Value>, Unread> {
        let Some((row, value)) = self.rows.next() else {
            return Ok(None);
        };
        let mut at = self.at.clone();
        at.push(Step::Row(row));

        seed.deserialize(Node {
            value,
            at: at.clone(),
        })
        .map(Some)
        .map_err(|error| error.at(&at))
    }
}

/// The fields of a table, each key read before its value, and the value of the key last read.
struct Fields {
    fields: <Table as IntoIterator>::IntoIter,
    value: Option<(Value, Vec<Step>)>,
    at: Vec<Step>,
}

impl<'de> MapAccess<'de> for Fields {
    type Error = Unread;

    fn next_key_seed<K: DeserializeSeed<'de>>(
        &mut self,
        seed: K,
    ) -> Result<Option<K::Value>, Unread> {
        let Some((key, value)) = self.fields.next() else {
            return Ok(None);
        };
        let mut at = self.at.clone();
        at.push(Step::key(key.as_str()));
        self.value = Some((value, at.clone()));

        seed.deserialize(IntoDeserializer::<Unread>::into_deserializer(key))
            .map(Some)
            .map_err(|error| error.at(&at))
    }

    fn next_value_seed<V: DeserializeSeed<'de>>(&mut self, seed: V) -> Result<V::Value, Unread> {
        let (value, at) = self
            .value
            .take()
            .expect("a field's value is read after its key");

        seed.deserialize(Node {
            value,
            at: at.clone(),
        })
        .map_err(|error| error.at(&at))
    }
}
