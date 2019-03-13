#![cfg_attr(feature="cargo-clippy", warn(clippy, clippy_correctness, clippy_style, clippy_pedantic, clippy_perf))]
#![feature(nll, crate_visibility_modifier, integer_atomics, associated_type_defaults)]
#![warn(rust_2018_idioms)]

use std::sync::atomic::{Ordering, AtomicU32, ATOMIC_U32_INIT};
#[allow(unused_imports)]
use log::{info, trace, warn, error, debug};
#[allow(unused_imports)]
use std::hash::{Hash, Hasher};
use std::collections::HashMap;

#[cfg(feature="serde")]
#[macro_use] extern crate serde;

pub type Index = u32;
pub const INVALID_ID: u32 = std::u32::MAX;

static ID_COUNTER: AtomicU32 = ATOMIC_U32_INIT;

pub trait Indexable {
    fn id(&self) -> Index;
    fn set_id(&mut self, id: Index) -> Index;
    fn name(&self) -> &str;
}

#[derive(Clone)]
#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
pub enum ConditionValue
{
    Value(bool),
    Script(String),
    #[cfg_attr(feature="serde", serde(skip_serializing, skip_deserializing))]
    Function(Box<fn(&Condition) -> bool>),
}
impl Default for ConditionValue {
    fn default() -> ConditionValue {
        ConditionValue::Value(false)
    }
}

#[derive(Default, Clone)]
#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
pub struct Condition
{
    #[cfg_attr(feature="serde", serde(default))]
    id: Index,
    name: String,
    value: ConditionValue,
}
impl Condition {
    pub fn new(name: &str,) -> Self {
        let mut selfie = Self::default();
        selfie.name = name.to_string();
        selfie.id = ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        selfie
    }

    pub fn with_value(mut self, value: ConditionValue) -> Self {
        self.value = value;
        self
    }

    pub fn eval(&self) -> bool {
        match &self.value {
            ConditionValue::Value(v) => *v,
            ConditionValue::Function(f) => f(self),
            ConditionValue::Script(_) => panic!("String hasn't been converted to a function!"),
        }
    }

    pub fn set(mut self, value: bool) -> Self {
        self.value = ConditionValue::Value(value);
        self
    }
}
impl Indexable for Condition {
    fn id(&self) -> Index { self.id }
    fn set_id(&mut self, id: Index) -> Index { self.id = id; id }
    fn name(&self) -> &str { self.name.as_str() }
}
impl Hash for Condition {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
impl PartialEq for Condition {
    fn eq(&self, other: &Condition) -> bool {
        self.id == other.id
    }
}
impl Eq for Condition {}

#[derive(Default, Clone)]
#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
pub struct Action {
    #[cfg_attr(feature="serde", serde(default))]
    id: Index,
    name: String,
    conditions: Vec<Condition>,
    causes: Vec<Condition>,

    #[cfg_attr(feature="serde", serde(skip_serializing, skip_deserializing))]
    cost_fn: Option<Box<fn(&Action) -> u32>>,
    #[cfg_attr(feature="serde", serde(skip_serializing, skip_deserializing))]
    effect_fn: Option<Box<fn(&Action)>>,
}
impl Action {
    pub fn new(name: &str,) -> Self {
        let mut selfie = Self::default();
        selfie.name = name.to_string();
        selfie.id = ID_COUNTER.fetch_add(1, Ordering::SeqCst);
        selfie
    }

    pub fn conditions(&self) -> &Vec<Condition> {
        &self.conditions
    }

    pub fn causes(&self) -> &Vec<Condition> {
        &self.causes
    }

    pub fn conditions_mut(&mut self) -> &mut Vec<Condition> {
        &mut self.conditions
    }

    pub fn causes_mut(&mut self) -> &mut Vec<Condition> {
        &mut self.causes
    }

    pub fn with_condition(mut self, condition: Condition) -> Self {
        self.conditions.push(condition);
        self
    }
    pub fn with_cause(mut self, condition: Condition) -> Self {
        self.causes.push(condition);
        self
    }

    pub fn cost(&self) -> u32 {
        match &self.cost_fn {
            Some(f) => f(self),
            None => 0,
        }
    }
    pub fn do_effect(&self) {
        match &self.effect_fn {
            Some(f) => f(self),
            None => {},
        }
    }

    pub fn with_cost(mut self, cost_fn: Option<fn(&Action) -> u32>) -> Self {
        self.cost_fn = match cost_fn {
            Some(f) => Some(Box::new(f)),
            None => None,
        };
        self
    }
    pub fn with_effect(mut self, effect_fn: Option<fn(&Action)>) -> Self {
        self.effect_fn = match effect_fn {
            Some(f) => Some(Box::new(f)),
            None => None,
        };
        self
    }

    pub fn can_apply(&self, state: &State) -> bool {
        for condition in &self.conditions {
            if let Some(state_condition) = state.conditions().get(&condition.id()) {
                if condition.eval() != state_condition.eval() {
                    return false;
                }
            } else {
                return false;
            }
        }

        true
    }

    pub fn apply(&self, state: &mut StateSnapshot) {
        for condition in &self.conditions {
            state.conditions_mut().insert(condition.id(), condition.eval());
        }
    }
}
impl Indexable for Action {
    fn id(&self) -> Index { self.id }
    fn set_id(&mut self, id: Index) -> Index { self.id = id; id}
    fn name(&self) -> &str { self.name.as_str() }
}
impl Hash for Action {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
impl PartialEq for Action {
    fn eq(&self, other: &Action) -> bool {
        self.id == other.id
    }
}
impl Eq for Action {}


#[derive(Default, Clone)]
#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
pub struct State {
    conditions: HashMap<Index, Condition>,
}
impl State {
    pub fn conditions(&self, ) -> &HashMap<Index, Condition> {
        &self.conditions
    }

    pub fn with_condition(mut self, condition: Condition) -> Self {
        self.add_condition(condition);
        self
    }
    pub fn add_condition(&mut self, condition: Condition) -> &mut Condition {
        let id = condition.id();
        self.conditions.insert(condition.id(), condition);
        self.conditions.get_mut(&id).unwrap()
    }

    pub fn get_by_name(&self, name: &str) -> Option<&Condition> {
        for (key, value) in &self.conditions {
            if value.name() == name {
                return Some(value)
            }
        }
        None
    }
    pub fn get(&self, id: Index, ) -> Option<&Condition> {
        self.conditions.get(&id)
    }

    pub fn fix_id<T>(&self, item: &mut T) -> Index
        where T: Indexable
    {
        match self.get_by_name(item.name()) {
            Some(existing) => { item.set_id(existing.id()); item.id() },
            None => { item.id() },
        }
    }
}
impl Hash for State {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.conditions.iter().for_each(|(key, value)| {
            key.hash(state);
            value.hash(state);
        });
    }
}
impl PartialEq for State {
    fn eq(&self, other: &State) -> bool {
        for (key, value) in &self.conditions {
            let other_value = other.conditions.get(key);
            if let Some(v) = other_value {
                if v != value {
                    return false;
                }
            } else {
                return false;
            }
        }

        true
    }
}

#[derive(Clone, Default)]
#[cfg_attr(feature="serde", derive(Serialize, Deserialize))]
pub struct StateSnapshot {
    conditions: HashMap<Index, bool>
}
impl StateSnapshot {
    pub fn new(state: &State) -> Self {
        let mut ret = Self::default();
        state.conditions().iter().for_each(|(key, value)| { ret.conditions.insert(*key, value.eval() ); });
        ret
    }

    pub fn diff(&self, other: &StateSnapshot) -> usize {
        let mut count: usize = 0;

        for (key, value) in &self.conditions {
            let other_value = other.conditions.get(key);
            if let Some(v) = other_value {
                if *v != *value {
                    count += 1;
                }
            } else {
                count += 1;
            }
        }

        count
    }

    pub fn conditions(&self, ) -> &HashMap<Index, bool> {
        &self.conditions
    }
    pub fn conditions_mut(&mut self, ) -> &mut HashMap<Index, bool> {
        &mut self.conditions
    }

    pub fn apply(mut self, action: &Action) -> Self {
        for condition in &action.causes {
            self.conditions_mut().insert(condition.id(), condition.eval());
        }
        self
    }

    pub fn can_apply(&self, action: &Action) -> bool {
        for condition in &action.conditions {
            if let Some(state_condition) = self.conditions().get(&condition.id()) {
                if condition.eval() != *state_condition {
                    return false;
                }
            } else {
                return false;
            }
        }

        true
    }
}
impl Hash for StateSnapshot {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.conditions.iter().for_each(|(key, value)| {
            key.hash(state);
            value.hash(state);
        });
    }
}
impl PartialEq for StateSnapshot {
    fn eq(&self, other: &StateSnapshot) -> bool {
        for (key, value) in &self.conditions {
            let other_value = other.conditions.get(key);
            if let Some(v) = other_value {
                if *v != *value {
                    return false;
                }
            } else {
                return false;
            }
        }

        true
    }
}
impl PartialEq<State> for StateSnapshot {
    fn eq(&self, other: &State) -> bool {
        for (key, value) in &self.conditions {
            let other_value = other.conditions.get(key);
            if let Some(v) = other_value {
                if v.eval() != *value {
                    return false;
                }
            } else {
                return false;
            }
        }

        true
    }
}

#[derive(Clone)]
struct PlanNode<'a> {
    state: StateSnapshot,
    action: Option<&'a Action>,
}
impl<'a> PlanNode<'a> {
    pub fn new(state: StateSnapshot, action: Option<&'a Action>) -> Self {
        trace!("PlanNode::new()");
        Self {
            state,
            action
        }
    }

    pub fn iterate(state: StateSnapshot, action: &'a Action) -> PlanNode<'a> {
        trace!("PlanNode::iterate()");
        PlanNode::new( state.apply(action), Some(action))
    }

    pub fn next_nodes(&self, actions: &[&'a Action]) -> Vec<(PlanNode<'a>, usize)> {
        trace!("PlanNode::next_nodes()");
        actions.iter().filter_map(|action| {
            match self.state.can_apply(action) {
                true => {
                    trace!("matching action; i={}", action.id());
                    Some((PlanNode::iterate(self.state.clone(), action), action.cost() as usize))
                },
                false => None,
            }
        }).collect()
    }

    pub fn diff(&self, other: &StateSnapshot) -> usize {
        trace!("PlanNode::diff() = {}", self.state.diff(other));
        self.state.diff(other)
    }
}
impl<'a> PartialEq for PlanNode<'a> {
    fn eq(&self, other: &PlanNode<'a>) -> bool {
        trace!("PlanNode::eq()");

        self.state == other.state
    }
}
impl<'a> Eq for PlanNode<'a> {}
impl<'a> Hash for PlanNode<'a> {
    fn hash<H>(&self, state: &mut H)
        where H: Hasher
    {
        if let Some(action) = self.action {
            action.hash(state);
        }

        self.state.hash(state);
    }
}

pub fn plan<'a>(initial_state: &'a StateSnapshot,
                goal_state: &StateSnapshot,
                allowed_actions: &'a [&'a Action])
                -> Option<Vec<&'a Action>> {
    // Builds our initial plan node.
    let start = PlanNode::new( initial_state.clone(), None);

    // Runs our search over the states graph.
    if let Some((plan, _)) = pathfinding::prelude::astar(&start,
                                                         |ref node| node.next_nodes(allowed_actions),
                                                         |ref node| node.diff(goal_state),
                                                         |ref node| node.state == *goal_state) {
        Some(plan.into_iter().skip(1).map(|ref node| node.action.unwrap()).collect())
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fern;
    use std::sync::atomic::{AtomicBool, Ordering, ATOMIC_BOOL_INIT};
    static LOGGING_ENABLED: AtomicBool = ATOMIC_BOOL_INIT;

    #[test]
    fn test_plan() {
        setup_logger();

        let initial_state = State::default()
            .with_condition(Condition::new("balls1"))
            .with_condition(Condition::new("balls2"))
            .with_condition(Condition::new("balls3"));

        let goal_state = State::default()
            .with_condition(initial_state.get_by_name("balls1").unwrap().clone().set(true))
            .with_condition(initial_state.get_by_name("balls2").unwrap().clone().set(true))
            .with_condition(initial_state.get_by_name("balls3").unwrap().clone().set(true));

        let action1 = Action::new("action1")
            .with_condition(initial_state.get_by_name("balls1").unwrap().clone().set(false))
            .with_cause(initial_state.get_by_name("balls1").unwrap().clone().set(true))
            .with_cause(initial_state.get_by_name("balls2").unwrap().clone().set(false))
            .with_cause(initial_state.get_by_name("balls3").unwrap().clone().set(false));

        let action2 = Action::new("action2")
            .with_condition(initial_state.get_by_name("balls1").unwrap().clone().set(true))
            .with_cause(initial_state.get_by_name("balls2").unwrap().clone().set(true));

        let action3 = Action::new("action3")
            .with_condition(initial_state.get_by_name("balls2").unwrap().clone().set(true))
            .with_cause(initial_state.get_by_name("balls3").unwrap().clone().set(true));

        let initial_snapshot = StateSnapshot::new(&initial_state);
        let goal_snapshot = StateSnapshot::new(&goal_state);
        let actions = [&action1, &action2, &action3];
        let the_plan = plan(&initial_snapshot, &goal_snapshot, &actions);

        let plan_ref = the_plan.as_ref().unwrap();
        assert_eq!(plan_ref.len(), 3);

        assert_eq!(plan_ref[0].name(), "action1");
        assert_eq!(plan_ref[1].name(), "action2");
        assert_eq!(plan_ref[2].name(), "action3");
    }

    #[test]
    #[cfg(feature="serde")]
    fn serde_test() {
        setup_logger();
        let initial_state = State::default()
            .with_condition(Condition::new("balls1"))
            .with_condition(Condition::new("balls2"))
            .with_condition(Condition::new("balls3"));

        let goal_state = State::default()
            .with_condition(initial_state.get_by_name("balls1").unwrap().clone().set(true))
            .with_condition(initial_state.get_by_name("balls2").unwrap().clone().set(true))
            .with_condition(initial_state.get_by_name("balls3").unwrap().clone().set(true));

        let action1 = Action::new("action1")
            .with_condition(initial_state.get_by_name("balls1").unwrap().clone().set(false))
            .with_cause(initial_state.get_by_name("balls1").unwrap().clone().set(true))
            .with_cause(initial_state.get_by_name("balls2").unwrap().clone().set(false))
            .with_cause(initial_state.get_by_name("balls3").unwrap().clone().set(false));

        let action2 = Action::new("action2")
            .with_condition(initial_state.get_by_name("balls1").unwrap().clone().set(true))
            .with_cause(initial_state.get_by_name("balls2").unwrap().clone().set(true));

        let action3 = Action::new("action3")
            .with_condition(initial_state.get_by_name("balls2").unwrap().clone().set(true))
            .with_cause(initial_state.get_by_name("balls3").unwrap().clone().set(true));

        let initial_snapshot = StateSnapshot::new(&initial_state);
        let goal_snapshot = StateSnapshot::new(&goal_state);
        let actions = [&action1, &action2, &action3];
        let the_plan = plan(&initial_snapshot, &goal_snapshot, &actions);

        let plan_ref = the_plan.as_ref().unwrap();

        let s_initial_state = serde_json::to_string_pretty(&initial_state).unwrap();
        trace!("--------------\ns_initial_state\n{}\n--------------", s_initial_state);

        let s_action = serde_json::to_string_pretty(&action1).unwrap();
        trace!("--------------\ns_action\n{}\n--------------", s_action);

        let s_initial_snapshot = serde_json::to_string_pretty(&initial_snapshot).unwrap();
        trace!("--------------\ns_initial_snapshot\n{}\n--------------", s_initial_snapshot);

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