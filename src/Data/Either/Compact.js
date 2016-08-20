'use strict';

exports.isFunction = function(x) {
  return x instanceof Function;
};

exports.isArray = function(x) {
  return x instanceof Array;
};

exports.isRecord = function(x) {
  return typeof x === 'object'
      && !exports.isFunction(x)
      && !exports.isArray(x);
};

exports.isNumber = function(x) {
  return typeof x === 'number';
};

exports.isString = function(x) {
  return typeof x === 'string';
};

exports.isBoolean = function(x) {
  return typeof x === 'boolean';
};
