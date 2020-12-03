const _ = require('lodash');
const list = require('./list.json');
list.forEach(a => {
  list.forEach(b => {
    list.forEach(c => {
      var sum = a + b + c;
      if (sum === 2020) {
        console.log(a * b * c);
        return;
      }
    })
  })
});
