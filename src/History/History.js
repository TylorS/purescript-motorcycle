var Stream = require('../Control.Stream.Stream')

var inBrowser = typeof window !== 'undefined'

var history = inBrowser ? require('history/createBrowserHistory')() : require('history/createMemoryHistory')()

exports.createHistory = function (unit) {
  var subject = Stream.toHoldSubject(Stream.startWith(history.location)(Stream.never))

  history.listen(function (location) {
    Stream.subjectEvent(location)(subject)
  })

  function push (path) {
    return function () {
      history.push(path)
      return unit
    }
  }

  function replace (path) {
    return function () {
      history.replace(path)
      return unit
    }
  }

  function go (amount) {
    return function () {
      history.go(amount)
      return unit
    }
  }

  return {
    history: subject,
    push: push,
    replace: replace,
    go: go,
  }
}
