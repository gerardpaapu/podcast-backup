exports.setTimeoutImpl = function (n) {
  return function (onError, onSuccess) {
    var token = setTimeout(function () {
      onSuccess(null);
    }, n);

    return function (cancelError, onCancelerError, onCancelerSuccess) {
      cancelTimeout(token);
      onCancelerSuccess(null);
    };
  };
};
