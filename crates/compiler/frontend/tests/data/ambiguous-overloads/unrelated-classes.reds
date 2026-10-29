class First {}
class Second {}

func Choose(value: First) -> Int32 = 1;
func Choose(value: Second) -> Int32 = 2;


func UniqueCalls() {
  let _: Int32 = Choose(new First());
  let _: Int32 = Choose(new Second());

}

func AmbiguousCall() {
  let _: Int32 = Choose(null);
}
