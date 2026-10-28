// Syntax accepted by redscript 0.5.x, parsed in the compatibility mode
/// a doc comment before the module
module MyMod.Compat

enum ESize {
  Small = 1l,
  Large = 10l,
}

/// A documented class
public class Holder extends IScriptable {
  /// a documented field
  public let values: array<Int32>= [];
  static public let count: Int32;

  @if(true)
  /// a doc comment after an annotation
  final public func Read(obj: ref<IScriptable>, flag: Bool) -> Int32 {
    /// a doc comment inside a body
    let total: Int32 = 007;
    /// between statements
    total += flag ? 1 : 2;
    let ratio: Float = .5;
    let min: Int32 = -2147483648;
    let ok: Bool = !obj as Holder.IsEmpty();
    let n: Int32 = obj as Holder.values[0];
    /// last in a body
    return total;
  }

  public func IsEmpty() -> Bool = ArraySize(this.values) == 0;
  /// last in a class
}
/// end of file
