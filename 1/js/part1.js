const _ = require('lodash');
const list = require('./list.json');
list.forEach(a => {
  list.forEach(b => {
    var sum = a + b;
    if (sum === 2020) {
      console.log(a * b);
      return;
    }
  })
});
