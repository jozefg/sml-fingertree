## sml-fingertree

An implementation of efficient, persistent sequences backed by finger
trees in SML. Implementation is a little ugly due to lack of
polymorphic recursion.

The eventual goal is that `FINGERTREE` is equivalent to cmlib's
`SEQUENCE` allowing one to use this seamlessly with cmlib.
