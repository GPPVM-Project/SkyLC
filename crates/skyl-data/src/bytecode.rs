use crate::{NativeFunctionInfo, objects::Value};
use bincode::{
    Encode,
    config::{self, Configuration},
    enc::Encoder,
    encode_to_vec,
    error::{DecodeError, EncodeError},
};
use serde::{Deserialize, Serialize, ser::SerializeStruct};
use sha2::{Digest, Sha256};
use std::{
    collections::HashMap,
    fs::File,
    io::{Read, Write},
    path::Path,
    rc::Rc,
};

#[derive(Debug, Serialize, Deserialize, Encode)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn new(code: Vec<u8>, constants: Vec<Value>) -> Self {
        Self { code, constants }
    }
}

#[derive(Clone, Debug, Deserialize)]
pub struct VirtualFunction {
    pub id: u32,
    pub chunk: Rc<Chunk>,
}

impl Encode for VirtualFunction {
    fn encode<E: Encoder>(&self, encoder: &mut E) -> Result<(), EncodeError> {
        self.id.encode(encoder)?;
        self.chunk.as_ref().encode(encoder)?;
        Ok(())
    }
}

impl Serialize for VirtualFunction {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("VirtualFunction", 2)?;
        state.serialize_field("id", &self.id)?;
        state.serialize_field("chunk", &self.chunk.as_ref())?;
        state.end()
    }
}

impl VirtualFunction {
    pub fn new(id: u32, chunk: Rc<Chunk>) -> Self {
        Self { id, chunk }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, Encode)]
pub struct Bytecode {
    pub functions: HashMap<u32, VirtualFunction>,
    pub native_functions: HashMap<String, NativeFunctionInfo>,
    pub v_tables: HashMap<u32, Vec<VirtualFunction>>,
    pub main: Option<VirtualFunction>,
}

const BINCODE_CONFIG: Configuration = config::standard();

impl Bytecode {
    pub fn new(
        functions: HashMap<u32, VirtualFunction>,
        native_functions: HashMap<String, NativeFunctionInfo>,
        v_tables: HashMap<u32, Vec<VirtualFunction>>,
        main: Option<VirtualFunction>,
    ) -> Self {
        Self {
            functions,
            native_functions,
            v_tables,
            main,
        }
    }

    pub fn get_function(&self, function_id: u32) -> Rc<Chunk> {
        self.functions[&function_id].chunk.clone()
    }

    pub fn save_to_file<P: AsRef<Path>>(&self, path: P) -> Result<(), EncodeError> {
        let encoded: Vec<u8> = encode_to_vec(self, BINCODE_CONFIG)?;

        let mut hasher = Sha256::new();
        hasher.update(&encoded);
        let hash = hasher.finalize();

        let mut file = File::create(path).map_err(|e| EncodeError::Io { inner: e, index: 0 })?;
        file.write_all(&hash)
            .and_then(|_| file.write_all(&encoded))
            .map_err(|e| EncodeError::Io { inner: e, index: 0 })?;

        Ok(())
    }

    pub fn load_from_file<P: AsRef<Path>>(path: P) -> Result<Self, DecodeError> {
        let mut file =
            File::open(path).map_err(|e| DecodeError::OtherString(format!("IO Error: {e}")))?;
        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer)
            .map_err(|e| DecodeError::OtherString(format!("IO Error: {e}")))?;

        if buffer.len() < 32 {
            return Err(DecodeError::OtherString(
                "The file is too small to contain hash".to_string(),
            ));
        }

        let (hash_bytes, data_bytes) = buffer.split_at(32);

        let mut hasher = Sha256::new();
        hasher.update(data_bytes);
        let calculated_hash = hasher.finalize();

        if calculated_hash.as_slice() != hash_bytes {
            return Err(DecodeError::OtherString(
                "Hash mismatch - the  file may be corrupted".to_string(),
            ));
        }

        let bytecode: Result<(Bytecode, usize), DecodeError> =
            bincode::serde::decode_from_slice(data_bytes, BINCODE_CONFIG);

        match bytecode {
            Ok(b) => Ok(b.0),
            Err(e) => Err(e),
        }
    }
}
