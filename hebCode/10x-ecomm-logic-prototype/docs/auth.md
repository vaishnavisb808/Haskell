## Authentication

The API uses jwt tokens signed using a key loaded from config to verify requests to all
endpoints the token must be included in the `Authorisation` header as a `Bearer` token.
The [servant-auth](https://hackage.haskell.org/package/servant-auth) package is used for implementing auth. 
