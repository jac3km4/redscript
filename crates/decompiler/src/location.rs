use std::ops::Add;

use redscript_io::Offset;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location(u32);

impl Location {
    pub const MAX: Location = Location(u32::MAX);
    pub const ZERO: Location = Location(0);
}

impl From<u32> for Location {
    fn from(x: u32) -> Self {
        Self(x)
    }
}

impl From<Location> for u32 {
    fn from(x: Location) -> u32 {
        x.0
    }
}

impl Add<u16> for Location {
    type Output = Location;

    fn add(self, rhs: u16) -> Self::Output {
        Location(self.0 + u32::from(rhs))
    }
}

impl Add<Offset> for Location {
    type Output = Location;

    fn add(self, rhs: Offset) -> Self::Output {
        Location(
            self.0
                .checked_add_signed(i32::from(rhs))
                .expect("bytecode offset should not overflow"),
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Bounds {
    start: Location,
    end: Location,
}

impl Bounds {
    pub const UNBOUNDED: Bounds = Bounds {
        start: Location::ZERO,
        end: Location::MAX,
    };

    pub fn new(start: Location, end: Location) -> Self {
        Self { start, end }
    }

    pub fn start(&self) -> Location {
        self.start
    }

    pub fn end(&self) -> Location {
        self.end
    }

    pub fn contains(&self, bounds: Bounds) -> bool {
        self.start <= bounds.start && bounds.end <= self.end
    }

    pub fn contains_location(&self, loc: Location) -> bool {
        self.start <= loc && loc < self.end
    }
}
