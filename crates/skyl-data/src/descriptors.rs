use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt::Display,
    hash::Hash,
    rc::Rc,
};

#[derive(Debug, Clone)]
pub struct TypeDescriptor {
    pub name: String,
    pub id: u32,
    pub archetypes: HashSet<Archetype>,
    pub fields: HashMap<String, FieldDescriptor>,
    pub methods: HashMap<String, MethodDescriptor>,
}

impl PartialEq for TypeDescriptor {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for TypeDescriptor {}

impl Hash for TypeDescriptor {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.id.hash(state);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodParameter {
    pub name: String,
    pub kind: Rc<RefCell<TypeDescriptor>>,
}

impl MethodParameter {
    pub fn new(name: String, kind: Rc<RefCell<TypeDescriptor>>) -> Self {
        Self { name, kind }
    }
}

impl Hash for MethodParameter {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.kind.borrow().hash(state);
    }
}

#[derive(Debug, Clone)]
pub struct MethodDescriptor {
    pub name: String,
    pub params: Vec<MethodParameter>,
    pub arity: usize,
    pub owner_type_id: u32,
    pub return_kind_id: u32,
    pub is_native: bool,
}

impl MethodDescriptor {
    pub fn new(
        name: String,
        params: Vec<MethodParameter>,
        arity: usize,
        owner_type_id: u32,
        return_kind_id: u32,
        is_native: bool,
    ) -> Self {
        Self {
            name,
            params,
            arity,
            owner_type_id,
            return_kind_id,
            is_native,
        }
    }
}

impl Hash for MethodDescriptor {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.params.hash(state);
        self.arity.hash(state);
        self.owner_type_id.hash(state);
        self.return_kind_id.hash(state);
        self.is_native.hash(state);
    }
}

impl PartialEq for MethodDescriptor {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.params == other.params
            && self.owner_type_id == other.owner_type_id
            && self.return_kind_id == other.return_kind_id
            && self.is_native == other.is_native
    }
}

impl TypeDescriptor {
    pub fn new(
        name: String,
        archetypes: HashSet<Archetype>,
        fields: HashMap<String, FieldDescriptor>,
        id: u32,
    ) -> Self {
        Self {
            name,
            archetypes,
            fields,
            id,
            methods: HashMap::new(),
        }
    }

    pub fn from_type_decl(decl: TypeDecl) -> Self {
        Self {
            archetypes: decl.archetypes,
            fields: HashMap::new(),
            name: decl.name,
            id: decl.kind_id,
            methods: HashMap::new(),
        }
    }

    pub fn from_type_decl_with_fields(
        decl: TypeDecl,
        fields: HashMap<String, FieldDescriptor>,
    ) -> Self {
        Self {
            archetypes: decl.archetypes,
            fields,
            name: decl.name,
            id: decl.kind_id,
            methods: HashMap::new(),
        }
    }

    pub fn implements_archetype(&self, archetype: &Archetype) -> bool {
        self.archetypes.contains(archetype)
    }

    pub fn empty() -> TypeDescriptor {
        TypeDescriptor {
            name: String::new(),
            id: 0,
            archetypes: HashSet::new(),
            fields: HashMap::new(),
            methods: HashMap::new(),
        }
    }

    pub fn is_void(&self) -> bool {
        self.name == "void"
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldDescriptor {
    pub name: String,
    pub kind: Rc<RefCell<TypeDescriptor>>,
    pub id: u8,
}

impl FieldDescriptor {
    pub fn new(name: String, kind: Rc<RefCell<TypeDescriptor>>, id: u8) -> Self {
        Self { name, kind, id }
    }
}

#[derive(Clone, PartialEq, Hash, Eq)]
pub struct Archetype {
    pub name: String,
}

impl std::fmt::Debug for Archetype {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.name)
    }
}

impl Display for Archetype {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Archetype {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeDecl {
    pub name: String,
    pub kind_id: u32,
    pub archetypes: HashSet<Archetype>,
}

impl TypeDecl {
    pub fn new(name: String, kind_id: u32) -> Self {
        Self {
            name,
            kind_id,
            archetypes: HashSet::new(),
        }
    }

    pub fn implements_archetype(&self, arch: &Archetype) -> bool {
        self.archetypes.contains(arch)
    }

    pub fn add_archetype(&mut self, arch: Archetype) {
        self.archetypes.insert(arch);
    }
}
