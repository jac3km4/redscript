use std::ops::RangeTo;

use nom::error::ParseError;
use nom::{IResult, Offset, Slice};

/// Matches while open, close, or inner. Returns the matched range.
/// Input is matched, recursively descending, until open and close are balanced.
pub fn many_till_balanced1<I, O1, O2, E>(
    mut open: impl FnMut(I) -> IResult<I, O1, E>,
    mut inner: impl FnMut(I) -> IResult<I, I, E>,
    mut close: impl FnMut(I) -> IResult<I, O2, E>,
) -> impl FnMut(I) -> IResult<I, I, E>
where
    I: Clone + Offset + Slice<RangeTo<usize>>,
    E: ParseError<I>,
{
    move |start: I| {
        let mut open_count = 0usize;
        let mut close_count = 0usize;
        let mut rem = start.clone();
        let mut end = start.clone();
        loop {
            if let Ok((rem2, _)) = open(rem.clone()) {
                open_count += 1;
                rem = rem2;
            } else if let Ok((rem2, _)) = close(rem.clone()) {
                close_count += 1;
                rem = rem2;
            } else if let Ok((rem2, _)) = inner(rem.clone()) {
                rem = rem2;
            } else {
                break;
            }
            if open_count == close_count {
                end = rem.clone();
                break;
            }
        }
        let len = end.offset(&start);
        if len == 0 {
            Err(nom::Err::Error(E::from_error_kind(start, nom::error::ErrorKind::Many1)))
        } else {
            Ok((rem, start.slice(..len)))
        }
    }
}

pub fn delimited_list0<I, O1, O2, E>(
    mut open: impl FnMut(I) -> IResult<I, O1, E>,
    mut separator: impl FnMut(I) -> IResult<I, O1, E>,
    mut inner: impl FnMut(I) -> IResult<I, O2, E>,
    mut close: impl FnMut(I) -> IResult<I, O1, E>,
) -> impl FnMut(I) -> IResult<I, (O1, Vec<(O2, O1)>), E>
where
    I: Clone,
    E: ParseError<I>,
{
    move |is| {
        let (mut i, ss) = open(is)?;

        let mut parts = vec![];
        while let Ok((ip, ep)) = inner(i.clone()) {
            if let Ok((ip, sp)) = separator(ip.clone()) {
                parts.push((ep, sp));
                i = ip;
                continue;
            } else if let Ok((ip, se)) = close(ip) {
                parts.push((ep, se));
                i = ip;
                break;
            }
            // missing close match
            return Err(nom::Err::Error(E::from_error_kind(
                i,
                nom::error::ErrorKind::TagClosure,
            )));
        }
        return Ok((i, (ss, parts)));
    }
}
