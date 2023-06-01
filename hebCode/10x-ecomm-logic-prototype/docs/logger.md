## Logging 

Formatting and writing of logs is done by [Logger](../src/EcomApi/Services/Logger/Logger.hs) module
which has convinience functions for logging. Configuration like the max log level are loaded
on server start. The module supports 4 log levels `DEBUG, INFO, WARN, ERROR` and two
logtypes `FILE, CONSOLE`. Logtype also is configurable and is part of configuration loaded 
on server start.  
On server start a separate logger thread is spawned and a channel is created which is accessible 
all handlers so logging operation involves writing a logmessage along with it's loglevel into
the channel which is read by the logger functions and written to appropriate destinations
based on configuration.
