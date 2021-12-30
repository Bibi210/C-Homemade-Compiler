typedef bool custom_bool_t;

void swap(custom_bool_t *a, custom_bool_t *b) {
  custom_bool_t temp = (*a);
  (*a) = (*b);
  (*b) = temp;
}
void main() {
  bool tr = true, fl = false;
  swap((&tr), (&fl));
  puts("Tr = ");
  print_bool(tr);
  puts("Fl = ");
  print_bool(fl);
}
