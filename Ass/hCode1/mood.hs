data Mood = Happy|Sad deriving Show
fmood :: Mood->Mood
fmood Happy=Sad
fmood Sad=Happy
fcmood::Mood->IO()
fcmood Happy= putStrLn("Sad")
fcmood Sad= putStrLn("Happy")
