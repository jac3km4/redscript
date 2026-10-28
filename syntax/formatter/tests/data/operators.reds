
func Test1() {
  let a = 1 * (2 + 3);
  let b = (4 + 5) * 6;
  let c = -7 + 8;
  let d = -(9 + 10);
}

func Test2() {
  let tru = true;
  let fal = false;
  let a = !tru && fal;
  let b = !(tru && fal);
  let c = -new Vector4(0, 0, 0, 0) + new Vector4(1, 1, 1, 1);
  let d = -(new Vector4(0, 0, 0, 0) + new Vector4(1, 1, 1, 1));
  let e = (new Damage() += 1.0).GetValue();
  let f = new Damage();
  let g = (fal as Damage).GetValue();
  let h = !tru ? tru : fal && fal;
  let i = (!tru ? tru : fal) && fal;
  let j = !(tru ? tru : fal) && fal;
  let k = !(tru ? tru : fal && fal);
}

func Test3() {
  let test = new TestClass();
  let allEqual = test.GetPositionX()
    == test.GetPositionX()
    && test.GetPositionY() == test.GetPositionY()
    && test.GetPositionZ() == test.GetPositionZ()
    && test.GetPositionW() == test.GetPositionW();
  let anyUnequal1 = test.GetPositionX()
    != test.GetPositionX()
    || test.GetPositionY() != test.GetPositionY()
    || test.GetPositionZ() != test.GetPositionZ()
    || test.GetPositionW() != test.GetPositionW();
  let anyUnequal2 = !(test.GetPositionX() == test.GetPositionX())
    || !(test.GetPositionY() == test.GetPositionY())
    || !(test.GetPositionZ() == test.GetPositionZ())
    || !(test.GetPositionW() == test.GetPositionW());
}

func Test4() {
  (new PlayerPuppet())
    .GetBumpComponent()
    .FindComponentByName(n"whatever")
    .GetEntity()
    .PrefetchAppearanceChange(n"whatever");
}

class TestClass {
  func GetPositionX() -> Float = 0;

  func GetPositionY() -> Float = 0;

  func GetPositionZ() -> Float = 0;

  func GetPositionW() -> Float = 0;
}
