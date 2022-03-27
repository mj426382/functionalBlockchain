module Blockchain where
import Control.Monad
import Data.Word

import Hashable32
import HashTree
import PPrint
import Utils

type Address = Hash
type Amount = Word32
coin :: Amount
coin = 1000
data Transaction = Tx
  { txFrom :: Address
  , txTo :: Address
  , txAmount :: Amount
  } deriving Show

instance Hashable Transaction where
  hash (Tx a b c) = hash [hash a, hash b, hash c]

data Block = Block { blockHdr :: BlockHeader, blockTxs ::  [Transaction]}

instance Show Block where
  show (Block hdr txs) = unlines (show hdr : map show txs)

instance Hashable Block where
  hash = hash . blockHdr

data BlockHeader = BlockHeader
  {
    parent :: Hash
  , coinbase :: Transaction
  , txroot :: Hash -- root of the Merkle tree
  , nonce :: Hash
  } deriving Show

instance Hashable BlockHeader where
  hash (BlockHeader p c r n) = hash [p,hash c, r, n]

difficulty = 5
blockReward = 50*coin
coinbaseTx miner = Tx {txFrom = 0, txTo = miner, txAmount = blockReward}

validNonce :: BlockHeader -> Bool
validNonce b = hash b `mod` 2^difficulty == 0

tx1 = Tx
  { txFrom = hash "Alice"
  , txTo = hash "Bob"
  , txAmount = 1*coin
  }

type Miner = Address
type Nonce = Word32

findNonce :: BlockHeader -> Hash
findNonce (BlockHeader pr t h n) = if validNonce (BlockHeader pr t h n) then n else findNonce (BlockHeader pr t h (n + 1))

mineBlock :: Miner -> Hash -> [Transaction] -> Block
mineBlock miner parent txs = do
  let minerTx = coinbaseTx miner
  let tHash = treeHash (buildTree (coinbaseTx miner:txs))
  let nonce = findNonce (BlockHeader parent minerTx tHash 0 )
  let blockHeader = BlockHeader parent minerTx tHash nonce
  Block blockHeader txs

genesis = block0
block0 = mineBlock (hash "Satoshi") 0 []
block1 = mineBlock (hash "Alice") (hash genesis) []
block2 = mineBlock (hash "Charlie") (hash block1) [tx1]
chain = [block2, block1, block0]

validChain :: [Block] -> Bool
validChain [] = True
validChain [b] = True
validChain (x : y : z) = isJust (verifyBlock x (hash y)) && validChain (y : z)

verifyChain :: [Block] -> Maybe Hash
verifyChain [] = Just 0
verifyChain [b] = Just (hash b)
verifyChain chain = do
  guard (validChain chain)
  return (hash (head chain))

verifyBlock :: Block -> Hash -> Maybe Hash
verifyBlock b@(Block hdr txs) parentHash = do
  guard (parent hdr == parentHash)
  guard (txroot hdr == treeHash (buildTree (coinbase hdr:txs)))
  guard (validNonce hdr)
  return (hash b)


data TransactionReceipt = TxReceipt
  {  txrBlock :: Hash, txrProof :: MerkleProof Transaction } deriving Show

validateReceipt :: TransactionReceipt -> BlockHeader -> Bool
validateReceipt r hdr = txrBlock r == hash hdr
                        && verifyProof (txroot hdr) (txrProof r)

mineTransactions :: Miner -> Hash -> [Transaction] -> (Block, [TransactionReceipt])
mineTransactions miner parent txs = do
  let block = mineBlock miner parent txs
  let allTxs = coinbaseTx miner:txs
  let receipts = mineTransactionReceipts (hash block) txs (buildTree allTxs)
  (block, receipts)

mineTransactionSingleReceipt :: Hash -> Transaction -> Tree Transaction -> [TransactionReceipt]
mineTransactionSingleReceipt blockHash tx Nil = [] -- just in case
mineTransactionSingleReceipt blockHash tx tr = mineTransactionReceiptWithoutProof blockHash (buildProof tx tr)

mineTransactionReceiptWithoutProof :: Hash -> Maybe (MerkleProof Transaction) -> [TransactionReceipt]
mineTransactionReceiptWithoutProof blockHash Nothing = [] -- just in case
mineTransactionReceiptWithoutProof blockHash (Just proof) = [TxReceipt blockHash proof]

mineTransactionReceipts :: Hash -> [Transaction] -> Tree Transaction -> [TransactionReceipt]
mineTransactionReceipts blockHash list t = map (\x -> head (mineTransactionSingleReceipt blockHash x t)) list


pprHeader :: BlockHeader -> ShowS
pprHeader self@(BlockHeader parent cb txroot nonce)
  = pprV [ p ("hash", VH $ hash self)
         , p ("parent", VH parent)
         , p ("miner", VH $ txTo cb)
         , p ("root", VH txroot)
         , p ("nonce", nonce)
         ]
  where
    nl = showString "\n"
    p :: Show a => (String, a) -> ShowS
    p = showsPair

pprBlock :: Block -> ShowS
pprBlock (Block header txs)
 = pprHeader header
 . showChar '\n'
 . pprTxs (coinbase header:txs)

pprTx :: Transaction -> ShowS
pprTx tx@(Tx from to amount)
  = pprH [ showString "Tx#"
         , showsHash (hash tx)
         , p ("from", VH from)
         , p ("to", VH to)
         , p ("amount", amount)
         ]
  where
    p :: Show a => (String, a) -> ShowS
    p = showsPair

pprTxs :: [Transaction] -> ShowS
pprTxs = pprV . map pprTx