var RSSParser = require('rss-parser');

exports.parseStringImpl = function (string) {
  return function () {
    var parser = new RSSParser();
    return parser.parseString(string);
  }
};
