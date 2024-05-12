const number = 185;
let n1 = 0;
let n2 = 1;

console.log("Fibonacci Series:");
console.log(n1); // print 0
console.log(n2); // print 1
let nextTerm = n1 + n2;

let i = 0;
while (i < number) {
  i = i + 1;
  console.log(nextTerm);

  n1 = n2;
  n2 = nextTerm;
  nextTerm = n1 + n2;
}
