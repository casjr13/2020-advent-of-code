const _ = require('lodash');
const passwords = require('./passwords.json');

const validPasswords = passwords.filter(line => {
  const {min, max, char, password} = parseLine(line);
  const fil = _.filter(password, c => c === char);
  const count = fil.length;
  let valid = count >= min && count <= max;
  return valid;
});

console.log(validPasswords.length);

function parseLine(line) {
  const split = line.split(' ');
  const minmax = split[0].split('-');
  const min = parseInt(minmax[0]);
  const max = parseInt(minmax[1]);
  const char = split[1][0];
  const password = split[2];

  return {min, max, char, password};
}