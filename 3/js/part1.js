const fs = require("fs");
const text = fs.readFileSync("./map.txt").toString('utf-8');
const map = text.split("\n");
const cols = map[0].length;
const rows = map.length;

let trees = 0;
let x = 0;
let y = 0;

while (y < rows) {
  // console.log(`(${x}, ${y}) => ${map[y][x]}`);
  if (map[y][x] === '#') trees++;
  x = (x + 3) % (cols - 1);
  y++;
}

console.log(trees);
