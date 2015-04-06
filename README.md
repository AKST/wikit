# Wikit [![travis-ci](https://travis-ci.org/AKST/wikit.svg)](https://travis-ci.org/AKST/wikit)

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

