module Capability.Random where

import Prelude
import Halogen (HalogenM)
import Control.Monad.Trans.Class (lift)

class
  Monad m <= Random m where
  shuffleArray :: forall a. Array a -> m (Array a)

instance randomHalogenM :: Random m => Random (HalogenM st act slots msg m) where
  shuffleArray = lift <<< shuffleArray
