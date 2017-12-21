let rec print_indent = (indent) =>
  switch indent {
  | n when n <= 0 => ()
  | n =>
    print_string("  ");
    print_indent(n - 1);
  };
