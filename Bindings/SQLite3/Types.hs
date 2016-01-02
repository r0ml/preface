{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}

module Bindings.SQLite3.Types (
    -- * Objects
    -- | <http://www.sqlite.org/c3ref/objlist.html>
    CDatabase,
    CStatement,
    CValue,
    CContext,
    CBlob,
    CBackup,

    -- * Enumerations

    -- ** Error
    SQLiteError(..),
    SQLColumnType(..),

    -- * Indices
    ParamIndex(..),
    ColumnIndex(..),
    ColumnCount,

    -- ** Indices (FFI)
    CParamIndex(..),
    CColumnIndex(..),
    CColumnCount,

    -- * Miscellaneous
    CNumBytes(..),
    CDestructor,
    c_SQLITE_TRANSIENT,
    c_SQLITE_UTF8,

    -- * Custom functions
    ArgCount(..),
    ArgIndex,
    CArgCount(..),
    c_SQLITE_DETERMINISTIC,

    -- * Conversion to and from FFI types
    FFIType(..),
) where

-- #include "sqlite3.h"

import Preface.Imports
import Foreign.C.Types
import Foreign.Ptr
import Preface.FFITemplates

-- Result code documentation copied from <http://www.sqlite.org/c3ref/c_abort.html>

data Error = ErrorOK                     -- ^ Successful result
           | ErrorError                  -- ^ SQL error or missing database
           | ErrorInternal               -- ^ Internal logic error in SQLite
           | ErrorPermission             -- ^ Access permission denied
           | ErrorAbort                  -- ^ Callback routine requested an abort
           | ErrorBusy                   -- ^ The database file is locked
           | ErrorLocked                 -- ^ A table in the database is locked
           | ErrorNoMemory               -- ^ A @malloc()@ failed
           | ErrorReadOnly               -- ^ Attempt to write a readonly database
           | ErrorInterrupt              -- ^ Operation terminated by @sqlite3_interrupt()@
           | ErrorIO                     -- ^ Some kind of disk I/O error occurred
           | ErrorCorrupt                -- ^ The database disk image is malformed
           | ErrorNotFound               -- ^ Unknown opcode in @sqlite3_file_control()@
           | ErrorFull                   -- ^ Insertion failed because database is full
           | ErrorCan'tOpen              -- ^ Unable to open the database file
           | ErrorProtocol               -- ^ Database lock protocol error
           | ErrorEmpty                  -- ^ Database is empty
           | ErrorSchema                 -- ^ The database schema changed
           | ErrorTooBig                 -- ^ String or BLOB exceeds size limit
           | ErrorConstraint             -- ^ Abort due to constraint violation
           | ErrorMismatch               -- ^ Data type mismatch
           | ErrorMisuse                 -- ^ Library used incorrectly
           | ErrorNoLargeFileSupport     -- ^ Uses OS features not supported on host
           | ErrorAuthorization          -- ^ Authorization denied
           | ErrorFormat                 -- ^ Auxiliary database format error
           | ErrorRange                  -- ^ 2nd parameter to sqlite3_bind out of range
           | ErrorNotADatabase           -- ^ File opened that is not a database file
           | ErrorRow                    -- ^ @sqlite3_step()@ has another row ready
           | ErrorDone                   -- ^ @sqlite3_step()@ has finished executing
             deriving (Eq, Show)

-- | <http://www.sqlite.org/c3ref/sqlite3.html>
--
-- @CDatabase@ = @sqlite3@
data CDatabase

-- | <http://www.sqlite.org/c3ref/stmt.html>
--
-- @CStatement@ = @sqlite3_stmt@
data CStatement

-- | <http://www.sqlite.org/c3ref/value.html>
--
-- @CValue@ = @sqlite3_value@
data CValue

-- | <http://www.sqlite.org/c3ref/context.html>
--
-- @CContext@ = @sqlite3_context@
data CContext

-- | <https://www.sqlite.org/c3ref/blob.html>
--
-- @CBlob@ = @sqlite3_blob@
data CBlob

-- | <https://www.sqlite.org/c3ref/backup.html>
--
-- @CBackup@ = @sqlite3_backup@
data CBackup

-- | Index of a parameter in a parameterized query.
-- Parameter indices start from 1.
--
-- When a query is 'Database.SQLite3.prepare'd, SQLite allocates an
-- array indexed from 1 to the highest parameter index.  For example:
--
-- >>Right stmt <- prepare conn "SELECT ?1, ?5, ?3, ?"
-- >>bindParameterCount stmt
-- >ParamIndex 6
--
-- This will allocate an array indexed from 1 to 6 (@?@ takes the highest
-- preceding index plus one).  The array is initialized with null values.
-- When you bind a parameter with 'Database.SQLite3.bindSQLData', it assigns a
-- new value to one of these indices.
--
-- See <http://www.sqlite.org/lang_expr.html#varparam> for the syntax of
-- parameter placeholders, and how parameter indices are assigned.
newtype ParamIndex = ParamIndex Int
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show ParamIndex where
    show (ParamIndex n) = show n

-- | Limit min/max bounds to fit into SQLite's native parameter ranges.
instance Bounded ParamIndex where
    minBound = ParamIndex (fromIntegral (minBound :: CInt))
    maxBound = ParamIndex (fromIntegral (maxBound :: CInt))

-- | Index of a column in a result set.  Column indices start from 0.
newtype ColumnIndex = ColumnIndex Int
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show ColumnIndex where
    show (ColumnIndex n) = show n

-- | Limit min/max bounds to fit into SQLite's native parameter ranges.
instance Bounded ColumnIndex where
    minBound = ColumnIndex (fromIntegral (minBound :: CInt))
    maxBound = ColumnIndex (fromIntegral (maxBound :: CInt))

-- | Number of columns in a result set.
type ColumnCount = ColumnIndex

newtype CParamIndex = CParamIndex CInt
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show CParamIndex where
    show (CParamIndex n) = show n

newtype CColumnIndex = CColumnIndex CInt
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show CColumnIndex where
    show (CColumnIndex n) = show n

type CColumnCount = CColumnIndex

newtype CNumBytes = CNumBytes CInt
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

-- | <http://www.sqlite.org/c3ref/c_static.html>
--
-- @Ptr CDestructor@ = @sqlite3_destructor_type@
data CDestructor

-- | Tells SQLite3 to make its own private copy of the data
c_SQLITE_TRANSIENT :: Ptr CDestructor
c_SQLITE_TRANSIENT = intPtrToPtr (-1)

c_SQLITE_UTF8 :: CInt
c_SQLITE_UTF8 = 1 -- #{const SQLITE_UTF8}


-- | Number of arguments of a user defined SQL function.
newtype ArgCount = ArgCount Int
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show ArgCount where
    show (ArgCount n) = show n

instance Bounded ArgCount where
    minBound = ArgCount 0
    maxBound = ArgCount 6 -- (#{const SQLITE_LIMIT_FUNCTION_ARG})

-- | Index of an argument to a custom function. Indices start from 0.
type ArgIndex = ArgCount

newtype CArgCount = CArgCount CInt
    deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | This just shows the underlying integer, without the data constructor.
instance Show CArgCount where
    show (CArgCount n) = show n

instance Bounded CArgCount where
    minBound = CArgCount (-1)
    maxBound = CArgCount 6 -- #{const SQLITE_LIMIT_FUNCTION_ARG}

-- | Tells SQLite3 that the defined custom SQL function is deterministic.
c_SQLITE_DETERMINISTIC :: CInt
c_SQLITE_DETERMINISTIC = 0x800 -- #{const SQLITE_DETERMINISTIC}

[enum|SQLiteError OK Error Internal Permission
    Abort Busy Locked NoMemory ReadOnly Interrupt 
    IO Corrupt NotFound Full CantOpen Protocol
    Empty Schema TooBig Constraint Mismatch
    Misuse NoLargeFileSupport Authorization Format
    Range NotADatabase Notice Warning Row 100 Done |]

[enum|SQLColumnType Integer 1 Float Text Blob Null |]
------------------------------------------------------------------------
-- Conversion to and from FFI types

-- | The "Database.SQLite3" and "Database.SQLite3.Direct" modules use
-- higher-level representations of some types than those used in the
-- FFI signatures ("Database.SQLite3.Bindings").  This typeclass
-- helps with the conversions.
class FFIType public ffi | public -> ffi, ffi -> public where
    toFFI   :: public -> ffi
    fromFFI :: ffi -> public

instance FFIType ParamIndex CParamIndex where
    toFFI (ParamIndex n) = CParamIndex (fromIntegral n)
    fromFFI (CParamIndex n) = ParamIndex (fromIntegral n)

instance FFIType ColumnIndex CColumnIndex where
    toFFI (ColumnIndex n) = CColumnIndex (fromIntegral n)
    fromFFI (CColumnIndex n) = ColumnIndex (fromIntegral n)

instance FFIType ArgCount CArgCount where
    toFFI (ArgCount n)  = CArgCount (fromIntegral n)
    fromFFI (CArgCount n) = ArgCount (fromIntegral n)
