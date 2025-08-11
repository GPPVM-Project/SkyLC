use base64::{engine::general_purpose::STANDARD, Engine};
use config::{Config, ConfigError};
use rand::RngCore;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct SkylConfig {
    pub bytecode: BytecodeGenConfig,
}

#[derive(Debug, Deserialize)]
pub struct BytecodeGenConfig {
    pub checksum_key: String,
}

pub fn load_config() -> Result<SkylConfig, ConfigError> {
    // let config = Config::builder()
    //     .add_source(config::File::with_name("config"))
    //     .build()?;

    let skyl_config: SkylConfig = SkylConfig {
        bytecode: BytecodeGenConfig {
            checksum_key: "lOEyPD/aMv631wE+jUdYMnM7qRdBJLzafS6ZRk6LwHg=".into(),
        },
    };
    Ok(skyl_config)
}

pub fn generate_base64_key() -> String {
    let mut key = [0u8; 32];
    rand::rng().fill_bytes(&mut key);
    STANDARD.encode(key)
}

pub fn decode_base64_key(key_str: &str) -> Result<[u8; 32], String> {
    let decoded = STANDARD
        .decode(key_str)
        .map_err(|e| format!("Invalid base64: {e}"))?;

    if decoded.len() != 32 {
        return Err(format!(
            "Invalid length: expected 32 bytes, got {}",
            decoded.len()
        ));
    }

    let mut key_bytes = [0u8; 32];
    key_bytes.copy_from_slice(&decoded);
    Ok(key_bytes)
}
