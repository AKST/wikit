
///
// MessageStore takes socket requests and attaches a timestamp to requests,
// and hands them back to callbacks that listen for those timestamps.
//
(function (exporter) {
  var MessageStore;

  MessageStore = function (url, protocols, onFatalError) {
    var socket, self;

    self = this;

    if (typeof protocols === 'undefined' || !protocols.length) {
      socket = new WebSocket(url);
    }
    else {
      socket = new WebSocket(url, protocols);
    }

    //
    // response listeners
    //
    this.__listeners = {};

    //
    // abstracts the need to wait for the socket 
    // to open before requests can be made.
    //
    this.__socket_promise = new RSVP.Promise(function (resolve, reject) {
      socket.onopen = function (event) {
        resolve(socket);
      };
    });

    //
    // handles requests
    //
    this.__socket_promise.then(function (socket) {
      socket.addEventListener("message", function (event) {
        var parsedData, timestamp, callback;

        parsedData = JSON.parse(event.data);
        timestamp  = parsedData.timestamp;   

        //
        // when no time stamp is present, this means
        // there was an error where it was impossible
        // to provide a timestamp, hence fatal.
        //
        if (typeof timestamp === 'undefined') {
          onFatalError(parsedData);
          return;
        }

        callback = self.__listeners[timestamp];
        callback(parsedData);

        //
        // prevent GC loitering
        //
        delete self.__listeners[timestamp];
      });
    });
  };

  MessageStore.create = function (url, protocols, onFatalError) {
    return new MessageStore(url, protocols, onFatalError);
  };
  
  MessageStore.prototype.send = function (request, callback) {
    // gets the time stamp
    var deepcopy, timestamp; 
    
    timestamp = (new Date()).valueOf();

    //
    // attach event listener
    //
    this.__listeners[timestamp] = callback;

    if (typeof request === 'object') {
      //
      // deep copy as we're mutating it's shape 
      //
      deepcopy = JSON.parse(JSON.stringify(request)); 
    }
    else if (typeof request === 'string') {
      //
      // all requests need to be serialised
      //
      deepcopy = JSON.parse(request);
    }

    //
    // attach timestamp so response can be recongised
    //
    deepcopy.timestamp = timestamp;

    this.__socket_promise.then(function (socket) {
      socket.send(JSON.stringify(deepcopy));
    });
  };

  MessageStore.prototype.addEventListener = function (event, callback) {
    this.__socket_promise.then(function (socket) {
      socket.addEventListener(event, callback);
    });
  };

  exporter(MessageStore); 

}(function (module) {
  if (typeof window !== 'undefined') {
    window.MessageStore = module;
  }
  else if (typeof process !== 'undefined') {
    module.exports = module
  }
  else {
    throw new Error("unknown export context");
  }
}));
