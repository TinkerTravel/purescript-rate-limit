'use strict';

exports.setTimeout = function(millis) {
  return function(action) {
    return function() {
      setTimeout(action, millis);
    };
  };
};
