#![allow(dead_code)]
#![allow(unused_macros)]
use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    rc::Rc,
};

use bincode::{Encode, enc::Encoder, error::EncodeError};
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::memory::GcRef;

#[derive(Debug, Eq, PartialEq)]
pub enum ObjectKind {
    Int,
    Float,
    String,
    Boolean,
    Obj,
    Void,
}

#[derive(Clone)]
pub enum Value {
    Int(i32),
    Float(f32),
    Bool(bool),
    String(Rc<String>),
    Void,
    Object(Rc<RefCell<dyn Object>>),
}

impl Encode for Value {
    fn encode<E: Encoder>(&self, encoder: &mut E) -> Result<(), EncodeError> {
        let helper = match self {
            Value::Int(i) => ValueSerde::Int(*i),
            Value::Float(f) => ValueSerde::Float(*f),
            Value::Bool(b) => ValueSerde::Bool(*b),
            Value::String(rc_str) => ValueSerde::String(rc_str.as_ref().clone()),
            Value::Void => ValueSerde::Void,
            Value::Object(obj_rc) => {
                let obj = obj_rc.borrow();
                // Tenta serializar o objeto de acordo com seu tipo
                if let Some(instance) = obj.as_any().downcast_ref::<Instance>() {
                    ValueSerde::Object(SerializableObject::Instance {
                        fields: instance.fields.clone(),
                    })
                } else if let Some(list) = obj.as_any().downcast_ref::<List>() {
                    ValueSerde::Object(SerializableObject::List {
                        elements: list.elements.clone(),
                    })
                } else {
                    return Err(EncodeError::Other(
                        "Unsupported object type for serialization",
                    ));
                }
            }
        };

        helper.encode(encoder)
    }
}

#[derive(Serialize, Deserialize, Encode)]
enum ValueSerde {
    Int(i32),
    Float(f32),
    Bool(bool),
    String(String),
    Void,
    Object(SerializableObject),
}

#[derive(Serialize, Deserialize, Encode)]
pub enum SerializableObject {
    Instance { fields: Vec<Value> },
    List { elements: Vec<Value> },
}

impl<'de> Deserialize<'de> for Value {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value_serde = ValueSerde::deserialize(deserializer)?;
        Ok(match value_serde {
            ValueSerde::Int(i) => Value::Int(i),
            ValueSerde::Float(f) => Value::Float(f),
            ValueSerde::Bool(b) => Value::Bool(b),
            ValueSerde::String(s) => Value::String(Rc::new(s)),
            ValueSerde::Void => Value::Void,
            ValueSerde::Object(obj) => match obj {
                SerializableObject::Instance { fields } => {
                    Value::Object(Rc::new(RefCell::new(Instance::new(fields))))
                }
                SerializableObject::List { elements } => {
                    Value::Object(Rc::new(RefCell::new(List::new(elements))))
                }
            },
        })
    }
}

impl Serialize for Value {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Value::Int(i) => ValueSerde::Int(*i).serialize(serializer),
            Value::Float(f) => ValueSerde::Float(*f).serialize(serializer),
            Value::Bool(b) => ValueSerde::Bool(*b).serialize(serializer),
            Value::String(s) => ValueSerde::String(s.as_ref().clone()).serialize(serializer),
            Value::Void => ValueSerde::Void.serialize(serializer),
            Value::Object(o) => {
                let obj = o.borrow();
                let serializable = if let Some(instance) = obj.as_any().downcast_ref::<Instance>() {
                    SerializableObject::Instance {
                        fields: instance.fields.clone(),
                    }
                } else if let Some(list) = obj.as_any().downcast_ref::<List>() {
                    SerializableObject::List {
                        elements: list.elements.clone(),
                    }
                } else {
                    return Err(serde::ser::Error::custom(
                        "Unsupported object type for serialization",
                    ));
                };
                ValueSerde::Object(serializable).serialize(serializer)
            }
        }
    }
}

pub enum ObjectColor {}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;
        match (self, other) {
            (Int(a), Int(b)) => a == b,
            (Float(a), Float(b)) => a == b,
            (Bool(a), Bool(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Void, Void) => true,
            (Object(a), Object(b)) => {
                let a_ref = a.borrow();
                let b_ref = b.borrow();
                a_ref.eq_object(&*b_ref)
            }
            _ => false,
        }
    }
}

impl Eq for Value {}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(v) => f.write_str(&format!("{v}")),
            Value::Int(v) => f.write_str(&format!("{v}")),
            Value::Float(v) => f.write_str(&format!("{v}")),
            Value::String(v) => f.write_str(&format!("{v}")),
            Value::Void => f.write_str("void"),
            Value::Object(obj_ptr) => f.write_str(&obj_ptr.borrow().to_string()),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(v) => f.write_str(&format!("{v}")),
            Value::Int(v) => f.write_str(&format!("{v}")),
            Value::Float(v) => f.write_str(&format!("{v}")),
            Value::String(v) => f.write_str(&format!("{v}")),
            Value::Void => f.write_str("void"),
            Value::Object(obj_ptr) => f.write_str(&obj_ptr.borrow().to_string()),
        }
    }
}

pub trait Object {
    fn as_any(&self) -> &dyn std::any::Any;
    fn as_any_mut(&mut self) -> &mut dyn std::any::Any;
    fn get_kind(&self) -> ObjectKind;
    fn type_name(&self) -> &'static str;
    fn to_string(&self) -> String;
    fn eq_object(&self, other: &dyn Object) -> bool;
    fn get_size(&self) -> usize;
    fn trace_references(&self, tracer: &mut dyn FnMut(&GcRef));
}

impl PartialEq for dyn Object {
    fn eq(&self, other: &Self) -> bool {
        self.eq_object(other)
    }
}

impl Eq for dyn Object {}

impl Debug for dyn Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{:?}", self.to_string()))
    }
}

#[derive(Serialize, Deserialize)]
pub struct Instance {
    pub fields: Vec<Value>,
}

impl Instance {
    pub fn new(fields: Vec<Value>) -> Self {
        Self { fields }
    }
}

impl Object for Instance {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn get_kind(&self) -> ObjectKind {
        ObjectKind::Obj
    }

    fn type_name(&self) -> &'static str {
        "object"
    }

    fn to_string(&self) -> String {
        format!("{:?}", self.fields)
    }

    fn eq_object(&self, other: &dyn Object) -> bool {
        if let Some(other_instance) = other.as_any().downcast_ref::<Instance>() {
            self.fields == other_instance.fields
        } else {
            false
        }
    }

    fn get_size(&self) -> usize {
        let mut size = std::mem::size_of::<Self>();
        size += self.fields.len() * std::mem::size_of::<Value>();

        for value in &self.fields {
            if let Value::Object(_) = value {
                todo!();
                //let obj = unsafe { gc_ref };
                //size += obj.borrow().get_size();
            }
        }

        size
    }

    fn trace_references(&self, _tracer: &mut dyn FnMut(&GcRef)) {
        for field in &self.fields {
            if let Value::Object(_) = field {
                todo!();
                //tracer(obj_ref);
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct List {
    pub elements: Vec<Value>,
}

impl List {
    pub fn new(elements: Vec<Value>) -> Self {
        Self { elements }
    }
}

impl Object for List {
    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn get_kind(&self) -> ObjectKind {
        ObjectKind::Obj
    }

    fn type_name(&self) -> &'static str {
        "list"
    }

    fn to_string(&self) -> String {
        format!("{:?}", self.elements)
    }

    fn eq_object(&self, other: &dyn Object) -> bool {
        if let Some(other_list) = other.as_any().downcast_ref::<List>() {
            self.elements == other_list.elements
        } else {
            false
        }
    }

    fn get_size(&self) -> usize {
        let mut size = std::mem::size_of::<Self>();
        size += self.elements.len() * std::mem::size_of::<Value>();

        for value in &self.elements {
            if let Value::Object(_) = value {
                todo!();
                //let obj = unsafe { gc_ref };
                //size += obj.borrow().get_size();
            }
        }

        size
    }

    fn trace_references(&self, _tracer: &mut dyn FnMut(&GcRef)) {
        for value in &self.elements {
            if let Value::Object(_) = value {
                todo!();
                //tracer(obj_ref);
            }
        }
    }
}

macro_rules! impl_object {
    ($type:ident, $kind:expr, $name:expr, $inner_type:ty) => {
        impl AsRaw<$inner_type> for $type {
            fn get_raw(&self) -> $inner_type {
                self.v.clone()
            }
        }

        impl Object for $type {
            fn as_any(&self) -> &dyn std::any::Any {
                self
            }

            fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
                self
            }

            fn get_kind(&self) -> ObjectKind {
                $kind
            }

            fn type_name(&self) -> &'static str {
                $name
            }

            fn to_string(&self) -> String {
                format!("{:?}", self.v)
            }
        }
    };
}
