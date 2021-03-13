use crate::builtin;
use crate::object::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

/// 環境参照
pub type RefEnvironment = Rc<RefCell<Environment>>;

/// 環境のマップ
#[derive(Debug, PartialEq)]
pub struct Environment {
    map: HashMap<String, Object>,
    outer: Option<RefEnvironment>,
}

impl fmt::Display for Environment {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> fmt::Result {
        write!(fmt, "{{")?;

        write!(fmt, "map:")?;
        write!(fmt, "{{")?;
        let mut sep = "";

        for (k, v) in &self.map {
            write!(fmt, "{}{}:{}", sep, k, v)?;
            sep = ", ";
        }
        write!(fmt, "}},")?;

        write!(fmt, "outer:")?;
        match &self.outer {
            Some(outer) => write!(fmt, "{}", outer.borrow()),
            None => write!(fmt, "None"),
        }?;

        write!(fmt, "}}")
    }
}

/// 新しい環境作成
pub fn new_env(map: HashMap<String, Object>) -> RefEnvironment {
    Rc::new(RefCell::new(Environment { map, outer: None }))
}

/// 値の格納
pub fn set_value(env: &RefEnvironment, key: &str, value: Object) {
    env.borrow_mut().map.insert(key.to_string(), value);
}

/// 値の取得
pub fn get_value(env: &RefEnvironment, key: &str) -> Option<Object> {
    match env.borrow().map.get(key) {
        Some(v) => Some(v.clone()),
        // 外の環境を一階層再帰的に参照
        None => match &env.borrow().outer {
            Some(outer) => get_value(&outer, key),
            None => builtin::get(key),
        },
    }
}

/// この環境の外側に一階層環境を追加する.
pub fn add_outer(env: &RefEnvironment, outer_env: &RefEnvironment) {
    env.borrow_mut().outer = Some(outer_env.clone());
}

#[test]
fn test_get_get_set() {
    let e = new_env(HashMap::new());
    set_value(&e, "key_bool", Object::Bool(true));
    assert_eq!(get_value(&e, "key_bool").unwrap(), Object::Bool(true));

    set_value(&e, "key_int", Object::Int(1));
    assert_eq!(get_value(&e, "key_int").unwrap(), Object::Int(1));

    assert_eq!(get_value(&e, "key_none"), None);
}

#[test]
fn test_set_outer() {
    let mut g = HashMap::new();
    g.insert("TRUE".to_string(), Object::Bool(true));
    let global = new_env(g);

    let mut n = HashMap::new();
    n.insert("true".to_string(), Object::Bool(true));
    let local = new_env(n);
    add_outer(&local, &global);
    assert_eq!(get_value(&local, "TRUE").unwrap(), Object::Bool(true));
}
