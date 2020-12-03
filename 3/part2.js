const _ = require('lodash');
const fs = require("fs");
const text = fs.readFileSync("./map.txt").toString('utf-8');
const map = text.split("\n");
const cols = map[0].length;
const rows = map.length;

const slopes = [
  [1, 1],
  [3, 1],
  [5, 1],
  [7, 1],
  [1, 2],
];

const treesPerSlope = slopes.map(([mx, my]) => countTrees(mx, my));
// console.log(treesPerSlope);

var product = _.reduce(treesPerSlope, (product, n) => _.multiply(product, n), 1);
console.log(product);

function countTrees(mx, my) {
  let trees = 0;
  let x = 0;
  let y = 0;
  
  while (y < rows) {
    // console.log(`(${x}, ${y}) => ${map[y][x]}`);
    if (map[y][x] === '#') trees++;
    x = (x + mx) % (cols - 1);
    y += my;
  }

  return trees;
}