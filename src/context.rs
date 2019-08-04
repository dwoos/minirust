use crate::ast::Identifier;
use std::collections::HashMap;
use std::fmt::Debug;

#[derive(Debug)]
pub struct Context<T>
where
    T: Debug + Clone,
{
    envs: Vec<HashMap<Identifier, T>>,
}

impl<T> Context<T>
where
    T: Debug + Clone,
{
    pub fn new() -> Self {
        Self {
            envs: vec![HashMap::new()],
        }
    }

    pub fn lookup(&self, id: &Identifier) -> Option<T> {
        for env in self.envs.iter().rev() {
            if let Some(ty) = env.get(&id) {
                return Some(ty.clone());
            }
        }
        None
    }

    pub fn set(&mut self, id: Identifier, ty: T) {
        let last_index = self.envs.len() - 1;
        self.envs[last_index].insert(id, ty);
    }

    pub fn push(&mut self) {
        self.envs.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        if self.envs.len() == 1 {
            panic!("Attempt to pop last frame");
        }
        self.envs.pop();
    }
}
