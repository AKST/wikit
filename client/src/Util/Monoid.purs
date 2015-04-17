module Util.Monoid where

import Data.Monoid

concat :: forall m. (Monoid m) => [m] -> m
concat elems = impl mempty elems where
	impl acc (x:xs) = impl (acc <> x) xs
	impl acc [    ] = acc


