const number = 185;
let n1 = 0;
let n2 = 1;

log("Fibonacci Series:");
log(n1); // print 0
log(n2); // print 1
let nextTerm = n1 + n2;

let i = 0;
while (i < number) {
  i = i + 1;
  log(nextTerm);

  n1 = n2;
  n2 = nextTerm;
  nextTerm = n1 + n2;
}
