use std::{
    fmt::Display,
    fs,
    io::{self, Read},
    path::{Path, PathBuf},
};

static mut CURRENT_SOURCE_ID: u32 = 0;

pub fn read_file_without_bom(path: &str) -> io::Result<SourceFile> {
    let mut file = fs::File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    let content = if buffer.starts_with(&[0xef, 0xbb, 0xbf]) {
        String::from_utf8_lossy(&buffer[3..]).to_string()
    } else {
        String::from_utf8_lossy(&buffer).to_string()
    };

    let path_buf = PathBuf::from(path);
    let name = Path::new(path)
        .file_name()
        .map(|s| s.to_string_lossy().to_string())
        .unwrap_or_else(|| String::from("unknown"));

    let id = SourceFileID(unsafe { CURRENT_SOURCE_ID });
    unsafe { CURRENT_SOURCE_ID += 1 };

    Ok(SourceFile {
        id,
        content,
        name,
        path: path_buf,
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceFileID(pub u32);

impl Display for SourceFileID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(format!("SourceFileID({})", self.0).as_str())
    }
}

#[derive(Debug, Clone, Eq)]
pub struct SourceFile {
    pub id: SourceFileID,
    pub content: String,
    pub name: String,
    pub path: PathBuf,
}

impl PartialEq for SourceFile {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
