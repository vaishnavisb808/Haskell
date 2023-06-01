import System.IO
import Network.URI
main= do
   h <- connectTo "google.com" (portNumber 80)
   hputStr h "GET / HTTP /1.1\nHost:www.google.com\n\n"
   hGetcontents h