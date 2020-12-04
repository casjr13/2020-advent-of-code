const _ = require('lodash');
const passwords = require('./passwords.json');

const validPasswords = passwords.filter(line => {
  const {first, second, char, password} = parseLine(line);
  const a = password[first - 1] === char;
  const b = password[second - 1] === char;
  const valid = (a && !b) || (!a && b);
  // console.log(`f: ${first}, s: ${second}, c: ${char}, p: ${password}, a: ${password[first - 1]} == ${char}, b: ${password[second - 1]} == ${char}, valid: ${valid}`);
  return valid;
});

console.log(validPasswords.length);

function parseLine(line) {
  const split = line.split(' ');
  const pos = split[0].split('-');
  const first = parseInt(pos[0]);
  const second = parseInt(pos[1]);
  const char = split[1][0];
  const password = split[2];

  return {first, second, char, password};
}