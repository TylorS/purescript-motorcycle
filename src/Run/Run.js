var stream = require('../Control.Stream.Stream')

exports.createSinks = function () {
  return new Proxy({}, {
    get: function get (target, property) {
      if (!target[property]) {
        target[property] = stream.toSubject(stream.never)
      }

      return stream.multicast(target[property])
    },

    has: function has () {
      return true
    }
  })
}

exports.createSubscriptions = function (sinks) {
  return function (sinkProxies) {
    return function () {
      Object.keys(sinks)
        .filter(function (sinkName) { return sinkProxies.hasOwnProperty(sinkName) })
        .forEach(function (sinkName) {
          var sink = sinks[sinkName]
          var sinkProxy = sinkProxies[sinkName]

          function event (time) {
            return function (value) {
              sinkProxy.event(time)(value)
            }
          }

          function end (time) {
            sinkProxy.end(time)
          }

          stream.subscribe(event)(end)(sink)()
        })

      function dispose () {
        sinkProxies.forEach(function (sinkProxy) {
          sinkProxy.end(Date.now())
        })
      }

      return { dispose: dispose }
    }
  }
}
