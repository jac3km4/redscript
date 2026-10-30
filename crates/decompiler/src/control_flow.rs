use std::collections::BTreeMap;

use redscript_io::{CodeIter, Instr, Offset};

use crate::error::Error;
use crate::location::{Bounds, Location};

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
            let Some(instr) = self.it.next() else {
                break;
            };
            match instr {
                Instr::SwitchLabel(label) => {
                    let body = pos + label.body();
                    let next = pos + label.next_case();
                    if body > next {
                        continue;
                    }
                    let nested = ControlFlowBlock::case()
                        .with_exit(next)
                        .build(self.it, next)?;
                    self.block.insert(Bounds::new(body, next), nested);
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
