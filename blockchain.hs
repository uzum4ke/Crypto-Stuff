import Data.Sequence (Seq, (<|), empty)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Clock (getCurrentTime)
import Crypto.Hash (hashWith, SHA256 (..), Digest)
import Data.ByteString.Char8 as BC hiding (index, null, head, putStrLn)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Text.Encoding (encodeUtf8)
import Control.Monad (when)

data Transaction = Transaction {
    sender    :: ByteString,
    receiver  :: ByteString,
    amount    :: Integer,  -- Representing the amount as an integer for simplicity.
    signature :: ByteString  -- Typically, the signature would be complex to handle but simplified here.
} deriving (Show, Eq)

-- Define the structure of a block.
data Block = Block {
    index        :: Int,
    timestamp    :: POSIXTime,
    transactions  :: [Transaction],  -- A list of transactions included in the block.
    previousHash :: ByteString,  -- Use ByteString if hashes are manipulated or stored as binary data.
    nonce        :: Int
} deriving (Show)

-- Blockchain type
type Blockchain = Seq Block

hashBlock :: Block -> String
hashBlock block =
    let transactionData = mconcat [ BC.pack (show tx) | tx <- transactions block ]
        blockContent = BC.pack (show (index block)) `BC.append`
                       BC.pack (show (floor (utcTimeToPOSIXSeconds (posixSecondsToUTCTime (timestamp block))) :: Int)) `BC.append`
                       transactionData `BC.append`
                       previousHash block `BC.append`
                       BC.pack (show (nonce block))
        hashDigest :: Digest SHA256
        hashDigest = hashWith SHA256 blockContent
    in show hashDigest

-- Generate the genesis block.
genesisBlock :: IO Block
genesisBlock = do
    currentTime <- getCurrentTime
    let posixTime = utcTimeToPOSIXSeconds currentTime  -- Convert UTCTime to POSIXTime
    return Block {
        index = 0,
        timestamp = posixTime,
        transactions = [],
        previousHash = pack "0",
        nonce = 0
    }

addBlock :: Blockchain -> [Transaction] -> IO (Maybe Blockchain)
addBlock blockchain transactions
  | Seq.null blockchain = return Nothing
  | otherwise = do
      currentTime <- getCurrentTime
      let lastBlock = Seq.index blockchain 0 -- Get the first block (head) using Seq.index
          newBlock = Block {
              index = index lastBlock + 1,
              timestamp = utcTimeToPOSIXSeconds currentTime,
              transactions = transactions,
              previousHash = pack $ hashBlock lastBlock, -- Convert hash String to ByteString
              nonce = 0 -- Simplified version without proof of work
          }
      return $ Just (newBlock <| blockchain) -- Prepend the new block to the blockchain 

-- Helper function to pretty print the blockchain
prettyPrintBlockchain :: Blockchain -> IO ()
prettyPrintBlockchain blockchain = do
    putStrLn "Current Blockchain:"
    mapM_ printBlock (toList blockchain)
  where
    printBlock :: Block -> IO ()
    printBlock block = do
        putStrLn $ "Block Index: " ++ show (index block)
        putStrLn $ "Timestamp: " ++ show (posixSecondsToUTCTime $ timestamp block) ++ " UTC"
        putStrLn "Transactions:"
        mapM_ printTransaction (transactions block)
        putStrLn $ "Previous Hash: " ++ BC.unpack (previousHash block)
        putStrLn $ "Nonce: " ++ show (nonce block)
        putStrLn "----------------------------------------"

    printTransaction :: Transaction -> IO ()
    printTransaction tx = do
        putStrLn $ "  Sender: " ++ BC.unpack (sender tx)
        putStrLn $ "  Receiver: " ++ BC.unpack (receiver tx)
        putStrLn $ "  Amount: " ++ show (amount tx)
        putStrLn $ "  Signature: " ++ BC.unpack (signature tx)
        putStrLn "  ----------------------"
        
createDummyTransaction :: Int -> Transaction
createDummyTransaction n = Transaction {
    sender = pack $ "sender" ++ show n,
    receiver = pack $ "receiver" ++ show n,
    amount = fromIntegral (n * 100),
    signature = pack $ "signature" ++ show n
}

main :: IO ()
main = do
    genBlock <- genesisBlock
    let initialBlockchain = genBlock <| Seq.empty -- Initialize the blockchain with the genesis block
    
    let transactions1 = [createDummyTransaction 1, createDummyTransaction 2]
    let transactions2 = [createDummyTransaction 3, createDummyTransaction 4]
    let transactions3 = [createDummyTransaction 5, createDummyTransaction 6]

    -- Add the first block
    blockchain1 <- addBlock initialBlockchain transactions1
    case blockchain1 of
        Just chain1 -> do
            putStrLn "Blockchain after adding first block:"
            prettyPrintBlockchain chain1

            -- Add the second block
            blockchain2 <- addBlock chain1 transactions2
            case blockchain2 of
                Just chain2 -> do
                    putStrLn "Blockchain after adding second block:"
                    prettyPrintBlockchain chain2

                    -- Add the third block
                    blockchain3 <- addBlock chain2 transactions3
                    case blockchain3 of
                        Just chain3 -> do
                            putStrLn "Blockchain after adding third block:"
                            prettyPrintBlockchain chain3
                        Nothing -> putStrLn "Failed to add third block."
                Nothing -> putStrLn "Failed to add second block."
        Nothing -> putStrLn "Failed to add first block."
