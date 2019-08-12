

exports.assert = function (message) {
  return function (success) {
    return function () {
      if (!success) throw new Error(message);
      return {};
    };
  };
};

