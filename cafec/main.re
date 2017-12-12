open Pred;

let program = {|
func main() {
  0xF F F F
}
|};

let main = () => {
  open Spanned.Prelude;
  Parse.parse(program);
};

main();
