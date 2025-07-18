use std::{
    fs,
    io::{self, Read},
    path::{Path, PathBuf},
};

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

    Ok(SourceFile {
        content,
        name,
        path: path_buf,
    })
}

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub content: String,
    pub name: String,
    pub path: PathBuf,
}
