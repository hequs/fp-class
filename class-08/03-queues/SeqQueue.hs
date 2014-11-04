module SeqQueue (Queue, empty, enqueue, dequeue, isEmpty) where

import AbstractQueue
import qualified Data.Sequence as Seq

newtype Queue t = QueueImpl (Seq.Seq t)

instance AbstractQueue Queue where
  empty = QueueImpl Seq.empty

  isEmpty (QueueImpl s) = Seq.null s

  enqueue (QueueImpl s) x = QueueImpl (s Seq.|> x)

  dequeue (QueueImpl s) = (x, QueueImpl s')
    where
      (x Seq.:< s') = Seq.viewl s