void println(string to_print) {
  puts(to_print);
  puts("\n");
}

bool inf_or_equal(int a, int b) {
  if (a == b || a < b)
    return true;
  return false;
}

bool not(bool a) {
  if (a && true)
    return false;
  return true;
}

bool not_equal(int a, int b) { return not(a == b); }

bool sup(int a, int b) {
  bool result = inf_or_equal(a, b);
  return not(result);
}

bool sup_or_equal(int a, int b) {
  if (a == b || a > b)
    return true;
  return false;
}

void print_bool(bool a) {
  if (a)
    println("true");
  else
    println("false");
}
// Created cause i suck at assembly