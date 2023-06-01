## Developement setup

Docker is the prefered way to run the server and the database locally.  

`docker-compose build` will pull a `postgres` image and a `haskell` image and build the 
server.  
`docker-compose up` will launch two containers one with the server and the other with the 
database. The database server will also be avaialable at localhost:5432 and will use the
password configured using the `dbpasw` env var , this should be same as the `dbPass` key 
in config.json which is used by server to connect to the database.
