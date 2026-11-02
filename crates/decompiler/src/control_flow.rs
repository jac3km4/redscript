use std::collections::{BTreeMap, BTreeSet};

use redscript_io::{CodeIter, Instr, Offset};

use crate::error::Error;
use crate::location::{Bounds, Location};

fn switch_target(pos: Location, offset: Offset) -> Result<Location, Error> {
    u32::from(pos)
        .checked_add_signed(i32::from(offset))
        .map(Location::from)
        .ok_or(Error::DetachedSwitchBodies)
}

pub(crate) fn seek(it: &mut CodeIter<'_, Offset>, target: Location) -> Result<(), Error> {
    while Location::from(it.virtual_offset()) < target {
        it.next().ok_or(Error::UnexpectedEndOfCode)?;
    }
    if Location::from(it.virtual_offset()) != target {
        return Err(Error::UnexpectedEndOfCode);
    }
    Ok(())
}

/// Dispatch order need not be source order: the game compiler links grouped
/// labels backwards, and nonfinal defaults use a separate dispatch table.
pub(crate) struct SwitchLayout {
    pub labels: Vec<(Location, Location)>,
    pub default: Option<Location>,
    pub bodies: Vec<(Location, Location)>,
    pub end: Location,
}

impl SwitchLayout {
    pub fn read(start: CodeIter<'_, Offset>, limit: Location) -> Result<Self, Error> {
        let origin = Location::from(start.virtual_offset());
        let mut it = start.clone();
        let Some(Instr::Switch(switch)) = it.next() else {
            return Err(Error::UnexpectedEndOfCode);
        };
        let mut next = switch_target(origin, switch.first_case())?;
        let mut visited = BTreeSet::new();
        let mut labels = Vec::new();
        let default_label;
        loop {
            if next == limit {
                default_label = None;
                break;
            }
            if next <= origin || next > limit || !visited.insert(next) {
                return Err(Error::DetachedSwitchBodies);
            }
            it = start.clone();
            seek(&mut it, next)?;
            match it.clone().next() {
                Some(Instr::SwitchLabel(label)) => {
                    let body = switch_target(next, label.body())?;
                    if body <= next || body > limit {
                        return Err(Error::DetachedSwitchBodies);
                    }
                    labels.push((next, body));
                    next = switch_target(next, label.next_case())?;
                }
                Some(Instr::SwitchDefault) => {
                    default_label = Some(next);
                    break;
                }
                _ => {
                    default_label = None;
                    break;
                }
            }
        }
        // Source order must preserve both dispatch evaluation and body
        // fallthrough. Shared-body labels can retain any dispatch order, but
        // moving between bodies backwards cannot be emitted as a plain switch.
        if labels.windows(2).any(|pair| pair[0].1 > pair[1].1) {
            return Err(Error::DetachedSwitchBodies);
        }
        let mut bodies: Vec<_> = labels.iter().map(|&(_, body)| body).collect();
        bodies.sort_unstable();
        bodies.dedup();
        let detached =
            default_label.is_some_and(|default| bodies.first().is_some_and(|&body| body > default));
        let default = if let Some(label) = default_label {
            it = start.clone();
            seek(&mut it, label)?;
            it.next();
            if detached {
                let pos = Location::from(it.virtual_offset());
                let Some(Instr::Jump(jump)) = it.next() else {
                    return Err(Error::DetachedSwitchBodies);
                };
                let target = switch_target(pos, jump.target())?;
                if target < Location::from(it.virtual_offset()) || target > limit {
                    return Err(Error::DetachedSwitchBodies);
                }
                Some(target)
            } else {
                Some(Location::from(it.virtual_offset()))
            }
        } else {
            None
        };

        // Inline cases end at the next physical label, not the next dispatch
        // link. Detached bodies instead end at the next body in source order.
        let mut regions = Vec::new();
        if detached {
            bodies.extend(default);
            bodies.sort_unstable();
            bodies.dedup();
            for pair in bodies.windows(2) {
                regions.push((pair[0], pair[1]));
            }
        } else {
            for &body in &bodies {
                let boundary = labels
                    .iter()
                    .map(|&(label, _)| label)
                    .chain(default_label)
                    .filter(|&label| label >= body)
                    .min()
                    .unwrap_or(next);
                regions.push((body, boundary));
            }
        }
        let mut exit = if default_label.is_none() {
            Some(next)
        } else {
            None
        };
        let scan_end = if detached {
            *bodies.last().ok_or(Error::DetachedSwitchBodies)?
        } else {
            default_label.unwrap_or(next)
        };
        // Breaks in earlier bodies provide the final body's boundary. Never
        // search past the final body's start without a known exit: unrelated
        // following statements could otherwise be mistaken for switch code.
        it = start.clone();
        seek(&mut it, bodies.first().copied().unwrap_or(scan_end))?;
        while Location::from(it.virtual_offset()) < exit.unwrap_or(scan_end).min(scan_end) {
            let pos = Location::from(it.virtual_offset());
            let nested_start = it.clone();
            let Some(instr) = it.next() else { break };
            if let Instr::Switch(_) = instr {
                let containing_end = regions
                    .iter()
                    .find(|&&(body, body_end)| body <= pos && pos < body_end)
                    .map(|&(_, body_end)| body_end)
                    .ok_or(Error::DetachedSwitchBodies)?;
                let nested = Self::read(nested_start, containing_end)?;
                seek(&mut it, nested.end)?;
            } else if let Instr::Jump(jump) = instr {
                let target = switch_target(pos, jump.target())?;
                let last = if detached {
                    bodies.last().copied().unwrap_or(next)
                } else {
                    scan_end
                };
                if (target > last || (!detached && target == last)) && target > pos {
                    exit = Some(exit.unwrap_or(target).max(target));
                }
            }
        }
        if detached && exit.is_none() {
            return Err(Error::DetachedSwitchBodies);
        }
        let end = exit.unwrap_or_else(|| default.unwrap_or(next));
        if detached {
            let last = *bodies.last().ok_or(Error::DetachedSwitchBodies)?;
            if end < last || end == Location::MAX {
                return Err(Error::DetachedSwitchBodies);
            }
            regions.push((last, end));
        } else if let Some(default) = default {
            regions.push((default, end));
        }
        if end > limit {
            return Err(Error::DetachedSwitchBodies);
        }
        Ok(Self {
            labels,
            default,
            bodies: regions,
            end,
        })
    }
}

pub fn build_control_flow(mut it: CodeIter<'_, Offset>) -> Result<ControlFlowBlock, Error> {
    ControlFlowBlock::default().build(&mut it, Location::MAX)
}

#[derive(Debug, Default)]
pub struct ControlFlowBlock {
    type_: BlockType,
    children: BTreeMap<Bounds, ControlFlowBlock>,
    entry: Option<Location>,
    exit: Option<Location>,
}

impl ControlFlowBlock {
    fn new(type_: BlockType) -> Self {
        Self {
            type_,
            children: BTreeMap::new(),
            entry: None,
            exit: None,
        }
    }

    fn case() -> Self {
        Self::new(BlockType::Case)
    }

    fn conditional() -> Self {
        Self::new(BlockType::Conditional)
    }

    pub fn entry(&self) -> Option<Location> {
        self.entry
    }

    pub fn exit(&self) -> Option<Location> {
        self.exit
    }

    pub fn type_(&self) -> BlockType {
        self.type_
    }

    fn with_entry(mut self, entry: Location) -> Self {
        self.entry = Some(entry);
        self
    }

    fn with_exit(mut self, exit: Location) -> Self {
        self.exit = Some(exit);
        self
    }

    pub fn get_block(&self, bounds: Bounds) -> &ControlFlowBlock {
        if let Some((b, parent)) = self
            .children
            .range(
                Bounds::new(Location::ZERO, bounds.end())
                    ..=Bounds::new(bounds.start(), Location::MAX),
            )
            .next_back()
            && b.contains(bounds)
        {
            parent.get_block(bounds)
        } else {
            self
        }
    }

    pub fn get_containing(&self, loc: Location) -> (Bounds, &ControlFlowBlock) {
        if let Some((&bounds, parent)) = self
            .children
            .range(Bounds::new(Location::ZERO, loc)..=Bounds::new(loc, Location::MAX))
            .next_back()
            && bounds.contains_location(loc)
        {
            let (nested_bounds, nested) = parent.get_containing(loc);
            if nested_bounds == Bounds::UNBOUNDED {
                (bounds, nested)
            } else {
                (nested_bounds, nested)
            }
        } else {
            (Bounds::UNBOUNDED, self)
        }
    }

    fn insert(&mut self, bounds: Bounds, block: ControlFlowBlock) {
        if let Some((b, parent)) = self
            .children
            .range_mut(
                Bounds::new(Location::ZERO, bounds.end())
                    ..Bounds::new(bounds.start(), Location::MAX),
            )
            .next_back()
            && b.contains(bounds)
        {
            parent.children.insert(bounds, block);
        } else {
            self.children.insert(bounds, block);
        }
    }

    fn build(
        self,
        it: &mut CodeIter<'_, Offset>,
        end: Location,
    ) -> Result<ControlFlowBlock, Error> {
        ControlFlowBuilder::new(self, it).consume(end)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BlockType {
    #[default]
    Block,
    While,
    Case,
    Conditional,
}

struct ControlFlowBuilder<'ctx, 'i> {
    block: ControlFlowBlock,
    it: &'ctx mut CodeIter<'i, Offset>,
}

impl<'ctx, 'i> ControlFlowBuilder<'ctx, 'i> {
    fn new(block: ControlFlowBlock, it: &'ctx mut CodeIter<'i, Offset>) -> Self {
        Self { block, it }
    }

    fn position(&self) -> Location {
        self.it.virtual_offset().into()
    }

    fn consume(mut self, end: Location) -> Result<ControlFlowBlock, Error> {
        let start = self.position();

        while self.position() < end {
            let pos = self.position();
            let switch_start = self.it.clone();
            let Some(instr) = self.it.next() else {
                break;
            };
            match instr {
                Instr::Switch(_) => {
                    let layout = SwitchLayout::read(switch_start.clone(), end)?;
                    for &(body, body_end) in &layout.bodies {
                        if body == body_end {
                            continue;
                        }
                        let mut body_it = switch_start.clone();
                        seek(&mut body_it, body)?;
                        let nested = ControlFlowBlock::case()
                            .with_exit(layout.end)
                            .build(&mut body_it, body_end)?;
                        self.block.insert(Bounds::new(body, body_end), nested);
                    }
                    *self.it = switch_start;
                    seek(self.it, layout.end)?;
                }
                Instr::Jump(jump) => {
                    if i32::from(jump.target()) == instr.virtual_size() as i32 {
                        // no-op jump
                        continue;
                    }
                    let target = pos + jump.target();
                    if target + instr.virtual_size() == start {
                        self.block.type_ = BlockType::While;
                        self.block.exit = Some(pos);
                    } else {
                        self.block.exit = Some(target);
                    }
                    return Ok(self.block);
                }
                Instr::JumpIfFalse(jump) => {
                    let target = pos + jump.target();
                    let nested = ControlFlowBlock::conditional()
                        .with_entry(pos)
                        .build(self.it, target)?;
                    let kind = nested.type_;
                    let exit = nested.exit;
                    self.block.insert(Bounds::new(pos, target), nested);

                    if kind == BlockType::Conditional
                        && let Some(target) = exit
                    {
                        while self.position() < target {
                            let offset = self.position();
                            let branches =
                                ControlFlowBlock::conditional().build(self.it, target)?;
                            if self.position() == offset {
                                return Err(Error::UnexpectedEndOfCode);
                            }
                            self.block.insert(Bounds::new(offset, target), branches);
                        }
                    }
                }
                _ => {}
            }
            if self.position() == end {
                break;
            }
        }

        Ok(self.block)
    }
}
