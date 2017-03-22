var MostlyDom = require('mostly-dom')

exports.nodeListToArray = function (nodeList) {
  return Array.prototype.slice.call(nodeList)
}

exports.matches = function (selector) {
  return function (element) {
    return element.matches(selector.value0)
  }
}

exports.ensureMatches = function ensureMatches (selector) {
  if (!selector) { return always(always(true)) }

  return function (element) {
    return function (ev) {
      var target = ev.target

      for (; target && target !== element; target = target.parentElement) {
        if (target.matches(selector)) {
          return true
        }
      }

      return element.matches(selector)
    }
  }
}

exports.eventToElement = function (event) {
  return event.target
}

exports.isInScope = function (scope) {
  scope = scope.value0 || ''
  return function (element) {
    var isolate = element.getAttribute(MostlyDom.SCOPE_ATTRIBUTE)

    if (scope) {
      return isolate === scope
    }

    return !isolate
  }
}

exports.domEvent = function (eventType) {
  return function (useCapture) {
    return function (element) {
      function run (sink) {
        return function (scheduler) {
          function event (ev) {
            sink.event(scheduler.now())(ev)
          }

          element.addEventListener(eventType, event, useCapture)

          function dispose () {
            element.removeEventListener(eventType, event, useCapture)
          }

          return { dispose: dispose }
        }
      }

      return { run: run }
    }
  }
}

exports.sameElement = function (a) {
  return function (b) {
    return a === b
  }
}

function always (a) {
  return function () {
    return a
  }
}
