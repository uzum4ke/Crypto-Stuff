
import Data.Sequence (Seq, (<|), empty)
import qualified Data.Sequence as Seq
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Clock (getCurrentTime)
import Crypto.Hash (hashWith, SHA256 (..), Digest)
import Data.ByteString.Char8 as BC hiding (index, null, head)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Text.Encoding (encodeUtf8)

-- Define the structure of a block.
data Block = Block {
    index        :: Int,
    timestamp    :: POSIXTime,
    blockData    :: ByteString,  -- Use ByteString for potential efficiency improvements.
    previousHash :: ByteString,  -- Use ByteString if hashes are manipulated or stored as binary data.
    nonce        :: Int
} deriving (Show)

-- Blockchain type
type Blockchain = Seq Block

-- Hashing function for a block using cryptonite
hashBlock :: Block -> String
hashBlock block =
    let blockContent = BC.pack (show (index block)) `BC.append`
                       BC.pack (show (floor (utcTimeToPOSIXSeconds (posixSecondsToUTCTime (timestamp block))) :: Int)) `BC.append`
                       blockData block `BC.append`
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
        blockData = pack "Genesis Block",
        previousHash = pack "0",
        nonce = 0
    }

-- Function to add a block to the blockchain.
addBlock :: Blockchain -> String -> IO Blockchain
addBlock blockchain newData = do
    currentTime <- getCurrentTime
    let lastBlock = if Seq.null blockchain
                    then error "Blockchain cannot be empty. Initialize with the genesis block first."
                    else Seq.index blockchain 0  -- Get the first block (head) using Seq.index
        newBlock = Block {
            index = index lastBlock + 1,
            timestamp = utcTimeToPOSIXSeconds currentTime,
            blockData = pack newData,  -- Convert String to ByteString
            previousHash = pack $ hashBlock lastBlock,  -- Convert hash String to ByteString
            nonce = 0  -- Simplified version without proof of work
        }
    return (newBlock <| blockchain)  -- Prepend the new block to the blockchain


-- Assuming the Block and Blockchain types, genesisBlock and addBlock functions are defined as previously discussed
main :: IO ()
main = do
    genBlock <- genesisBlock
    let initialBlockchain =  genBlock  <| Seq.empty -- Initialize the blockchain with the genesis block
    blockchain <- addBlock initialBlockchain "First actual block"
    print blockchain