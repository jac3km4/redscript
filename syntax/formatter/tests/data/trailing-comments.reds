// header comment, kept above the module
module Test.Trailing // module comment

import Foo.Bar // import comment

class Foo extends Bar { // class open
  let a: Int32; // field a
  let b: Int32 = 1; /* field b */

  func Run(x: Int32) -> Int32 { // fn open
    let y = x + 1; // let comment
    let z = 2; /* block */ // and line
    y += 1; // first
    // own line
    y += 2; // second
    if y > 1 { // if open
      y = 0; // inner
    } else { // else open
      y = 1;
    } // if close
    while y < 10 { // while open
      y += 1; // step
    } // while close
    let w = x /* mid */ + y;
    let p = (x + y) * (x - y) - (x - (y - 1)); // nested operators
    let q = (x * y) + ((x)); // redundant parentheses
    for i in [1, 2] { // for open
      Log("x"); // log
    } // for close
    switch y {
      case 1: // case one
        break; // done
      default:
        y = 2; // default body
    } // switch close
    return y; // result
  } // fn close

  func Short() = 1; // inline fn
} // class close

enum Color { // enum open
  Red, // red
  Green = 1, // green
} // enum close

enum Plain {
  One,
  Two
} // plain close

struct S {
  let v: Int32; // v
} // struct close

@addMethod(Foo)
func Extra() { // extra
  return; // bye
} // extra close

func Last() {} // last
