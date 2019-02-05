# servant-avro

[![Build Status](https://travis-ci.org/doublecrowngaming/servant-avro.png)](https://travis-ci.org/doublecrowngaming/servant-avro)

# Servant integration for Avro

This package lets you communicate with clients using [Avro](https://github.com/haskell-works/avro).

It exposes an `Avro` content type, so that instead of having:

```
type API = "path" :> "to" :> "resource" :> Get '[JSON] a
```

you can say:

```
type API = "path" :> "to" :> "resource" :> Get '[Avro] a
```

or even:

```
type API = "path" :> "to" :> "resource" :> Get '[JSON, Avro] a
```

and of course, this will work for all HTTP verbs, and for request bodies, and anywhere else you use a content type in Servant.
