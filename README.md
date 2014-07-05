
An extended Prelude for Haskell which imports a larger set of standard functions and types.

Because of name conflicts, and the impossibility of re-exporting modules qualified, 
the assumption is that this will be used in conjunction with some conventions for importing
some other qualified modules.  These are:

- import qualified Data.ByteString as B

