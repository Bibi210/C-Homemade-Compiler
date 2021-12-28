void _test(int *a) { (*a) = -37; }
void main() {
  int c = 42;
  int *d = (&c);
  _test((&c));
  puts("c = ");
  puti((*d));
  puts("\n");
}