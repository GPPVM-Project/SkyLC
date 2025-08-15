use std::{cell::RefCell, path::PathBuf, rc::Rc};

use skyl_data::CompilerContext;

use crate::manifest::{Dependencies, Dependency};

pub fn setup_dependencies(ctx: Rc<RefCell<CompilerContext>>, dependencies: &Dependencies) {
    for (name, dependency) in &dependencies.deps {
        setup_dependency(ctx.clone(), dependency);
    }
}

fn setup_dependency(ctx: Rc<RefCell<CompilerContext>>, dependency: &Dependency) {
    match dependency {
        Dependency::Git { git, branch } => todo!(),
        Dependency::Path { path } => setup_path_dependency(ctx, path),
    }
}

fn setup_path_dependency(ctx: Rc<RefCell<CompilerContext>>, path: &str) {
    ctx.borrow_mut().add_dependency(PathBuf::from(path));
}
