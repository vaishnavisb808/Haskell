import System.IO (stderr, Handle)
import System.Log.Logger (rootLoggerName, setHandlers, updateGlobalLogger,
                          Priority(INFO), Priority(WARNING), infoM, debugM,
                          warningM, errorM, setLevel)
import System.Log.Handler.Simple (fileHandler, streamHandler, GenericHandler)
import System.Log.Handler (setFormatter)
import System.Log.Formatter

main = do
    let logPath ="testinglog.hs"
    myStreamHandler <- streamHandler stderr INFO
    myFileHandler <- fileHandler logPath WARNING
    let myFileHandler' = withFormatter myFileHandler
    let myStreamHandler' = withFormatter myStreamHandler
    let log = rootLoggerName
    updateGlobalLogger log (setLevel INFO)
    updateGlobalLogger log (setHandlers [myFileHandler', myStreamHandler'])
    infoM log $ "Logging to " ++ logPath
    debugM log "Hello debug."
    infoM log "Hello info."
    warningM log "Hello warning."
    errorM log "Hello error."

withFormatter :: GenericHandler Handle -> GenericHandler Handle
withFormatter handler = setFormatter handler formatter
    -- http://hackage.haskell.org/packages/archive/hslogger/1.1.4/doc/html/System-Log-Formatter.html
    where formatter = simpleLogFormatter "[$time $loggername $prio] $msg"