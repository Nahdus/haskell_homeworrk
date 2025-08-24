{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
import Log

-- I 5053 pci_id: con ing!
extractErrorCode :: String -> Int
extractErrorCode err = read (unwords (drop 1 (take 2 (words err))))

data InclusiveMessageType = UnknownType |
                            KnownType MessageType
                            deriving Show


-- checkErrorFormat

-- constructError::String -> InclusiveMessageType
-- constructError x = take 2 [words x]

isNumber :: String -> Bool
isNumber str = case reads str :: [(Double, String)] of
                [(_, "")] -> True
                _         -> False

isError::InclusiveMessageType -> Bool
isError (KnownType (Error _)) = True
isError _ = False

isUnknown::InclusiveMessageType -> Bool
isUnknown (UnknownType) = True
isUnknown _ = False


extractType:: String -> InclusiveMessageType
extractType x
    |take 1 (words x) == ["I"] = KnownType Info
    |take 1 (words x) == ["W"] = KnownType Warning
    |take 1 (words x) == ["E"] && isNumber (unwords (drop 1 (take 2 (words x)))) = KnownType (Error (extractErrorCode x))
    -- |take 1 (words x) == ["E"] = KnownType (Error (extractErrorCode x))
    |otherwise = UnknownType

extractTimeStamp:: String -> TimeStamp
extractTimeStamp inp
    |isError (extractType inp) = read (unwords (drop 2 (take 3 (words inp))))
extractTimeStamp inp = read (unwords (drop 1 (take 2 (words inp))))



extractMessage:: String -> String
extractMessage l = unwords (drop 2 (words l))

getKnownTypeFromInclusiveMessageType::InclusiveMessageType -> MessageType
getKnownTypeFromInclusiveMessageType (KnownType (Error x)) = Error x
getKnownTypeFromInclusiveMessageType (KnownType Info ) = Info
getKnownTypeFromInclusiveMessageType (KnownType Warning ) = Warning
getKnownTypeFromInclusiveMessageType (UnknownType) = Error 00


parseMessage :: String -> LogMessage
parseMessage msg
    |isUnknown (extractType msg) = Unknown msg
parseMessage msg = LogMessage (getKnownTypeFromInclusiveMessageType (extractType msg)) (extractTimeStamp msg) (extractMessage msg)


parseListlog :: [String] -> [LogMessage]
parseListlog [] = []
parseListlog (x:[]) = [parseMessage x]
parseListlog (x:y) = parseMessage x:parseListlog y

parse :: String -> [LogMessage]
parse filecontent = parseListlog (lines filecontent)

-- Leaf | Node MessageTree LogMessage MessageTree


-- insert::LogMessage -> MessageTree -> MessageTree
-- insert (LogMessage lmcode logtimestamp lmessage) ((Node treeleft (LogMessage treecod treetimestamp treemessage) treeright)|Leaf)
--     |logtimestamp>treetimestamp = (Node treeleft (LogMessage treecod treetimestamp treemessage) treeright) LogMessage Leaf
--     |logtimestamp<treetimestamp = Leaf LogMessage (Node treeleft (LogMessage treecod treetimestamp treemessage) treeright)

insert::LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert logmsg (Node lefttree (Unknown _) righttree) = Node lefttree logmsg righttree
insert (LogMessage lmcode logtimestamp _lmessage) ((Node b (LogMessage mtype timestamp _message) d))
    |logtimestamp > timestamp = Node (Node b (LogMessage mtype timestamp _message) d) (LogMessage lmcode logtimestamp _lmessage) Leaf
    |logtimestamp < timestamp = Node Leaf (LogMessage lmcode logtimestamp _lmessage) (Node b (LogMessage mtype timestamp _message) d)
insert (LogMessage lmcode logtimestamp _lmessage) Leaf
    |True = Node Leaf (LogMessage lmcode logtimestamp _lmessage) Leaf
insert lm mt = Node mt lm Leaf

    

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (lm:[]) = Node Leaf lm Leaf
build (lm1:(lm2)) = insert lm1 (build lm2)



exlm::LogMessage
exlm = parseMessage "I 2194 litt:le creature, and held out its arms and legs in all directions, 'just"
extree::MessageTree
extree = Node Leaf exlm Leaf


exlm2::LogMessage
exlm2 = parseMessage "E 47 1034 'What a pity it wouldn't stay!' sighed the Lory, as soon as it was quite"

extree2::MessageTree
extree2 = insert exlm2 extree

data IntList = Empty | Cons Int IntList




readFromFile::String->MessageTree
readFromFile fileContent = build (parse fileContent)

-- filecontent = "I 5053 pci_id: con ing!\n\
-- \   I 4681 ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)\n\
-- \   W 3654 e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled\n\
-- \   I 4076 verse.'\n\
-- \   I 4764 He trusts to you to set them free,\n\
-- \   I 858 your pocket?' he went on, turning to Alice.\n\
-- \   I 898 would be offended again.\n\
-- \   I 3753 pci 0x18fff steresocared, overne: 0000 (le wailan0: ressio0/derveld fory: alinpu-all)\n\
-- \   I 790 those long words, and, what's more, I don't believe you do either!' And\n\
-- \   I 3899 hastily.\n\
-- \   I 2194 little creature, and held out its arms and legs in all directions, 'just\n\
-- \   I 1447 she was terribly frightened all the time at the thought that it might be\n\
-- \   I 1147 began ordering people about like that!'\n\
-- \   I 3466 pci_hcd beed VRAM=2)"

-- op = readFromFile filecontent

testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse func n file = take n . func <$> readFile file

convertFileToTree::(String->MessageTree)
                    ->FilePath
                    ->IO MessageTree
convertFileToTree toTree path = toTree <$> readFile path

exampletree::IO MessageTree
exampletree = convertFileToTree readFromFile "error.log"