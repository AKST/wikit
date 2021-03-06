# Wikit [![travis-ci](https://travis-ci.org/AKST/wikit.svg)](https://travis-ci.org/AKST/wikit)

A intuitive & hopefully fast method of reviewing wikipedia article revisions. Some cool things
about this project.

- No JS Framework, a libraries based approach 
  - PS. I'm not against JS frameworks 
- No CSS Framework, only css libraries
- Uses cool HTML5 APIs
  - [Web Sockets](https://developer.mozilla.org/en/docs/WebSockets)
    - For fast revision retrivial 
  - [Web Workers](https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Using_web_workers)
    - To speed up parsing
  - [IndexedDB](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API)
    - For storing documents for offline use
  - [Cache Manifest](https://developer.mozilla.org/en-US/docs/Web/HTML/Using_the_application_cache)
    - For a fall back interface for offline use
- Written an obscure alt-js lang called PureScript
  - Haskell like staticly typed language designed with nice JS interope in mind
  - Originally choose it because I assumed it would something similar to parsec
  - This was actually the main reason I went with a libraries based approach

# Local Development

## Dependencies

- npm & node > 0.10 
- ghc > 7.8 & cabal

## Building

```
make init
make build
```

## Continious Builds (+ Tests)

Continious builds for the client code

```
make watch-client
```

Continious builds for the server code

```
make watch-server
```

## Running

To run the socket server

```
make serve-socket
```

To serve the static web files

```
make serve-client
```

## Testing

For running all tests

```
make test
```

For running client tests

```
make test-client
```

For runnning server tests

```
make test-server
```

## Debugging

You can use [__`wscat`__](https://www.npmjs.com/package/wscat), to test requests and 
responses of the socket server. In the likely event you event you don't have `wscat`,
you can install it like so.

```
npm install -g wscat
```

Then run it like so

```
make debug-server
```

