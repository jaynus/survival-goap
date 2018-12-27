#![cfg_attr(feature="cargo-clippy", warn(clippy, clippy_correctness, clippy_style, clippy_pedantic, clippy_perf))]
#![feature(nll, crate_visibility_modifier, integer_atomics)]
#![warn(rust_2018_idioms)]

use std::sync::atomic::{Ordering, AtomicU32};
#[allow(unused_imports)]
use log::{info, trace, warn, error, debug};
#[allow(unused_imports)]
use fern;
use std::hash::{Hash, Hasher};


use specs::storage::{DenseVecStorage, UnprotectedStorage};

pub type Index = u32;
pub static INVALID_ID: u32 = std::u32::MAX;

pub const CONDITION_CAUSE_LIMIT: usize = 16;

pub trait Indexable {
    fn id(&self) -> Index;
}

enum ConditionType<'a> {
    Value(bool),
    Eval(&'a mut dyn Fn(&Condition<'a>, &Goap<'a>, ) -> bool),
}
impl<'a> std::fmt::Debug for ConditionType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConditionType::Value(v) => { write!(f, "{}", v) },
            ConditionType::Eval(_) => { write!(f, "Function") }
        }
    }
}
impl<'a> PartialEq for ConditionType<'a> {
    fn eq(&self, other: &ConditionType<'a>) -> bool {
        match self {
            ConditionType::Value(v) => {
                match other {
                    ConditionType::Value(v_other) => { v == v_other},
                    ConditionType::Eval(_) => { false }
                }
            },
            ConditionType::Eval(_) => { false }
        }
    }
}
impl<'a> Eq for ConditionType<'a> { }

pub struct Condition<'a> {
    id: Index,
    value: ConditionType<'a>,
}
impl<'a> Indexable for Condition<'a> {
    fn id(&self, ) -> Index { self.id }
}
impl<'a> Default for Condition<'a> {
    fn default() -> Self {
        Self {
            id: INVALID_ID,
            value: ConditionType::Value(false),
        }
    }
}
impl<'a> Condition<'a> {
    pub fn new(id: Index) -> Self {
        let mut selfie = Self::default();
        selfie.id = id;
        selfie
    }

    pub fn with_eval(mut self, f: &'a mut dyn Fn(&Condition<'a>, &Goap<'a>, ) -> bool) -> Self {
        self.value = ConditionType::Eval(f);
        self
    }

    pub fn eval(&self, goap: &Goap<'a>) -> bool {
        match &self.value {
            ConditionType::Value(v) => *v,
            ConditionType::Eval(f) => (*f)(self, goap),
        }
    }
}
impl<'a> Into<Index> for Condition<'a> {
    fn into(self) -> u32 {
        self.id
    }
}
impl<'a> Hash for Condition<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
impl<'a> std::fmt::Debug for Condition<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Condition [id={}, value={:?}]", self.id, self.value)
    }
}

pub struct Action<'a> {
    id: Index,
    conditions: [Option<&'a Condition<'a>>; CONDITION_CAUSE_LIMIT],
    conditions_count: usize,
    causes: [Option<&'a Condition<'a>>; CONDITION_CAUSE_LIMIT],
    causes_count: usize,
    cost_fn: Option<&'a mut dyn Fn(&Action<'_>, &Goap<'_>, ) -> u32>,
    effect_fn: Option<&'a mut dyn Fn(&Action<'_>, &Goap<'_>, )>,
}
impl<'a> Indexable for Action<'a> {
    fn id(&self, ) -> Index { self.id }
}
impl<'a> Default for Action<'a> {
    fn default() -> Self {
        Self {
            id: INVALID_ID,
            conditions: [None; CONDITION_CAUSE_LIMIT],
            conditions_count: 0,
            causes: [None; CONDITION_CAUSE_LIMIT],
            causes_count: 0,
            cost_fn: None,
            effect_fn: None,
        }
    }
}
impl<'a> Action<'a> {
    pub fn new(id: Index) -> Self {
        let mut selfie = Self::default();
        selfie.id = id;
        selfie
    }

    pub fn with_condition(mut self, condition: &'a Condition<'a>, ) -> Self {
        if self.conditions_count + 1 > CONDITION_CAUSE_LIMIT {
            panic!("Condition overflow");
        }

        self.conditions[self.conditions_count] = Some(condition);
        self.conditions_count += 1;

        self
    }
    pub fn with_conditions(mut self, src: &[Option<&'a Condition<'a>>], ) -> Self {
        if src.len() + self.conditions_count > CONDITION_CAUSE_LIMIT {
            panic!("Slice too big");
        }

        for n in self.conditions_count..src.len() {
            self.conditions[n] = src[n];
        }

        self.conditions_count += src.len();

        self
    }

    pub fn with_cause(mut self, condition: &'a Condition<'a>, ) -> Self {
        if self.causes_count + 1 > CONDITION_CAUSE_LIMIT {
            panic!("Condition overflow");
        }

        self.conditions[self.causes_count] = Some(condition);
        self.causes_count += 1;

        self
    }
    pub fn with_causes(mut self, src: &[Option<&'a Condition<'a>>], ) -> Self {
        if src.len() + self.causes_count > CONDITION_CAUSE_LIMIT {
            panic!("Slice too big");
        }

        for n in self.causes_count..src.len() {
            self.causes[n] = src[n];
        }

        self.causes_count += src.len();

        self
    }

    pub fn with_cost(mut self, costfn: &'a mut dyn Fn(&Action<'_>, &Goap<'_>, ) -> u32) -> Self {
        self.cost_fn = Some(costfn);
        self
    }

    pub fn with_effect(mut self, effectfn: &'a mut dyn Fn(&Action<'_>, &Goap<'_>, )) -> Self {
        self.effect_fn = Some(effectfn);
        self
    }
}
impl<'a> Into<Index> for Action<'a> {
    fn into(self) -> u32 {
        self.id
    }
}
impl<'a> Hash for Action<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
impl<'a> std::fmt::Debug for Action<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Action [ id={} ]", self.id())
    }
}


pub struct State {
    conditions: Vec<Index>,
}
impl State {
    pub fn new() -> Self {
        Self {
            conditions: Vec::new(),
        }
    }

    pub fn iter<'a>(&'a self, goap: &'a Goap<'a>, ) -> StateIter<'a> {
        StateIter::new(goap, self.conditions.iter())
    }

    pub fn with_conditions(mut self, conditions: &[&Condition<'_>], ) -> Self {
        conditions.iter().for_each(|c| { self.conditions.push(c.id()); });
        self
    }
    pub fn with_condition(mut self, condition: &Condition<'_>, ) -> Self {
        self.conditions.push(condition.id());
        self
    }

}

pub struct StateIter<'a,> {
    goap: &'a Goap<'a>,
    inner_iter: std::slice::Iter<'a, Index>,
}
impl<'a> StateIter<'a> {
    fn new(goap: &'a Goap<'a>, inner_iter: std::slice::Iter<'a, Index>,) -> Self {
        Self {
            goap,
            inner_iter,
        }
    }
}
impl<'a> Iterator for StateIter<'a> {
    type Item = &'a Condition<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner_iter.next() {
            Some(i) => self.goap.get_condition(*i),
            _ => None,
        }

    }
}

pub struct StateSnapshot {
    values: std::collections::BTreeMap<Index, bool>,
}
impl StateSnapshot {
    pub fn new<'a>(goap: &'a Goap<'a>, state: &'a State) -> Self {
        let mut snapshot = Self::default();

        state.iter(goap).for_each(|condition| {
            snapshot.values.insert(condition.id(), condition.eval(goap));
        });

        snapshot
    }
}
impl Default for StateSnapshot {
    fn default() -> Self {
        Self {
            values: std::collections::BTreeMap::new(),
        }
    }
}

pub struct Goap<'a> {
    id_increment: AtomicU32,
    names: DenseVecStorage<String>,
    actions: DenseVecStorage<Action<'a>>,
    conditions: DenseVecStorage<Condition<'a>>,
}
impl<'a> Default for Goap<'a> {
    fn default() -> Self {
        Self {
            id_increment: AtomicU32::new(0),
            names: DenseVecStorage::default(),
            actions: DenseVecStorage::default(),
            conditions: DenseVecStorage::default(),
        }
    }
}
impl<'a> Goap <'a>{
    pub fn new() -> Self {
        Self::default()
    }
    pub fn new_id(&mut self, ) -> Index {
        self.id_increment.fetch_add(1, Ordering::SeqCst)
    }

    pub fn new_condition(&mut self, name: &str, ) -> &Condition<'a> {
        let id = self.new_id();

        unsafe {
            self.names.insert(id, name.to_string());
            self.conditions.insert(id, Condition::new(id));
            self.conditions.get(id)
        }
    }
    pub fn new_action(&mut self, name: &str, ) -> &Action<'a> {
        let id = self.new_id();

        unsafe {
            self.names.insert(id, name.to_string());
            self.actions.insert(id, Action::new(id));
            self.actions.get(id)
        }
    }

    pub fn get_condition(&self, id: Index, ) -> Option<&Condition<'a>> {
        unsafe {
            Some(self.conditions.get(id))
        }
    }
    pub fn get_action(&self, id: Index, ) -> Option<&Condition<'a>> {
        unsafe {
            Some(self.conditions.get(id))
        }
    }
    pub fn get_name(&self, id: Index, ) -> Option<&str> {
        unsafe {
            Some(self.names.get(id).as_str())
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use fern;
    use std::sync::atomic::{AtomicBool, Ordering, ATOMIC_BOOL_INIT};
    static LOGGING_ENABLED: AtomicBool = ATOMIC_BOOL_INIT;

    #[test]
    fn test_state_iterator() {
        setup_logger();
        let mut goap = Goap::new();

        let state1 = State::new().with_conditions(&[
            goap.new_condition("balls1"),
        ]);

        let state2 = State::new()
            .with_condition(goap.new_condition("balls1"))
            .with_condition(goap.new_condition("balls2"))
            .with_condition(goap.new_condition("balls3"));

        let iter = state2.iter(&goap);
        iter.for_each(|condition| {
            trace!("c: {}", goap.get_name(condition.id()).unwrap());
        });

    }

   fn setup_logger() -> Result<(), fern::InitError> {
        use fern::colors::{Color, ColoredLevelConfig};
        // I'm used to Python's logging colors and format,
        // so let's do something like that.

        match LOGGING_ENABLED.load(Ordering::SeqCst) {
            false => {
                let colors = ColoredLevelConfig::default()
                    .info(Color::Green)
                    .debug(Color::BrightMagenta)
                    .trace(Color::BrightBlue);
                fern::Dispatch::new()
                    .format(move |out, message, record| {
                        out.finish(format_args!(
                            "[{}][{:<14}][{}] ({}:{}) - {}",
                            chrono::Local::now().format("%Y-%m-%d %H:%M:%S"),
                            colors.color(record.level()).to_string(),
                            record.target(),
                            record.file().unwrap_or("unknown"),
                            record.line().unwrap_or(0),
                            message
                        ))
                    })
                    // gfx_device_gl is very chatty on info loglevel, so
                    // filter that a bit more strictly.
                    .level(log::LevelFilter::Trace)
                    .chain(std::io::stdout())
                    .chain(std::fs::OpenOptions::new()
                        .write(true)
                        .create(true)
                        .truncate(true)
                        .open("debug.log")?)
                    .apply()?;

                LOGGING_ENABLED.swap(true, Ordering::SeqCst);

                Ok(())
            },
            true => Ok(()),
        }
    }
}