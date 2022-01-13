module Curios.Compilation.RelocBuffer
  ( RelocBuffer (..)
  , relocEmpty
  , relocSingleton
  , relocPrependSize
  )
  where

import Curios.Compilation.Buffer (Buffer (..), unsigned)

import Curios.Compilation.Module
  ( SymIdx (..)
  , Offset
  , RelocType (..)
  , RelocEntry (..)
  )

data RelocBuffer =
  RelocBuffer Buffer [RelocEntry]

addOffset :: Offset -> RelocEntry -> RelocEntry
addOffset offset (RelocEntry relocType offset' symIdx) =
  RelocEntry relocType (offset + offset') symIdx

adjustRelocs :: Buffer -> [RelocEntry] -> [RelocEntry]
adjustRelocs (Buffer offset _) =
  map (addOffset offset)

instance Semigroup RelocBuffer where
  RelocBuffer buffer entries <> RelocBuffer buffer' entries' =
    RelocBuffer (buffer <> buffer') (entries ++ adjustRelocs buffer entries')

instance Monoid RelocBuffer where
  mempty = RelocBuffer mempty []
  mconcat = foldl (<>) mempty

relocEmpty :: Buffer -> RelocBuffer
relocEmpty buffer =
  RelocBuffer buffer []

relocSingleton :: Buffer -> RelocType -> SymIdx -> RelocBuffer
relocSingleton buffer relocType symIdx =
  RelocBuffer buffer [RelocEntry relocType 0 symIdx]

relocPrependSize :: RelocBuffer -> RelocBuffer
relocPrependSize relocBuffer =
  let RelocBuffer (Buffer size _) _ = relocBuffer
  in relocEmpty (unsigned size) <> relocBuffer 
