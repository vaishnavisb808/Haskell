# g10x-ecomm-logic-prototype
API to support markups and other e-commerce price logic


## Overview
Ecommerce price logic API is a service for maintaining logic that can be used for 
calculating the ecommerce price of products. Currently the only method used for 
conversion is markup - which calculates ecommerce price as a percentage variation 
from the store price. The API will enable it's user to create and modify logic that 
will apply to products effective from a certain date. Each logic is mapped to a UPC. 
The service also enables the user to specify exceptions to each logic where in an 
alternate rule gets applied  for the exception scenarios. Currently the service supports 
Store and Zone exceptions. The service maintains record of all logic that were applied, 
updates can only be made to future logic and at a given time a UPC is allowed to 
have only one future logic any updates will replace this logic.



## Package structure


```
EcomApi
  |
  |--Core/  - 
  |    |----Types.hs - types used throughout the application 
  |    |----Utils.hs -  utility functions
  |    |----Transformers.hs - conversion functions that convert from one type to another
  |    |----Config/ - 
  |           |-- Types.hs - types specifically used to load configuration information
  |           |-- Config.hs - functions asssociated with loading config                                                                             
  |
  |--API/
  |    |----	Types.hs - types used in request and response of API
  |    |----	Handler/
  |            |----	Viewlogic.hs - handler for view logic endpoint
  |            |----	ModifyLogic.hs - handler for modify logic endpoint
  |          Middleware/
  |            |----	Auth/
  |                    |----	Types - types used for authentication
  |                    |----	Basic - functions used for basic auth
  |                    |----	JWT - functions used for JWT auth
  |--Services/ - modules that interact with external entities
  |    |----Logger/
  |    |        |----	Logger.hs
  |    |        |----	Types.hs
  |    |-----Database/ - Everything associated with database
  |            |----	Types - types specifically used for database accessing or processing info from DB
  |            |----	Postgres - functions for accessing Postgres DB
  |               
  |--Server.hs - funtions to boot and run the server

```

## Developement setup
Docker is the prefered way to run the server and the database locally.  

`docker-compose build` will pull a `postgres` image and a `haskell` image and build the 
server.  
`docker-compose up` will launch two containers one with the server and the other with the 
database. The database server will also be avaialable at localhost:5432 and will use the
password configured using the `dbpasw` env var , this should be same as the `dbPass` key 
in config.json which is used by server to connect to the database.

## Authorisation
The API uses jwt tokens signed using a key loaded from config to verify requests to all
endpoints the token must be included in the `Authorisation` header as a `Bearer` token.
The [servant-auth](https://hackage.haskell.org/package/servant-auth) package is used for implementing auth. 

## Configuration

Configuration for the server are loaded either from a `config.json` file(if available) or
from an env var `CONFIG`(which is expected to be a json string). 
[Config.hs](src/EcomApi/Core/Config/Config.hs)


## Logging 
Formatting and writing of logs is done by [Logger](EcomApi/Services/Logger/Logger.hs) module
which has convinience functions for logging. Configuration like the max log level are loaded
on server start. The module supports 4 log levels `DEBUG, INFO, WARN, ERROR` and two
logtypes `FILE, CONSOLE`. Logtype also is configurable and is part of configuration loaded 
on server start.  
On server start a separate logger thread is spawned and a channel is created which is accessible 
all handlers so logging operation involves writing a logmessage along with it's loglevel into
the channel which is read by the logger functions and written to appropriate destinations
based on configuration.



