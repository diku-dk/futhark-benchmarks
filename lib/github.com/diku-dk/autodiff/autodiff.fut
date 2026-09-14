-- | Various utilities for performing AD.

import "onehot"

local def singular 'a (x: onehot.gen [1] a) = onehot.onehot x 0

-- | Compute the gradient of a scalar-valued function given a one-hot
-- generator for its result.
def grad_unit gen f x = vjp f x (singular gen)

-- | Convenience function for computing the gradient of an
-- 'f64'-valued differentiable function.
def grad32 = grad_unit onehot.f32

-- | Convenience function for computing the gradient of an
-- 'f64'-valued differentiable function.
def grad64 = grad_unit onehot.f64

-- | Compute the gradient of an arbitrary differentiable function
-- given a one-hot generator for its result.
def grad_rev gen f x = map (vjp f x) (onehots gen)
