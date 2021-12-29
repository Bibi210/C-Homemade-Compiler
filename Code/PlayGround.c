void swap(bool *a, bool *b) {
  bool temp = (*a);
  (*a) = (*b);
  (*b) = temp;
}
void main() {
  print_bool(true);
}
