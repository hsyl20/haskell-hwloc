{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Hwloc
   ( getApiVersion
   , CpuSet (..)
   , NodeSet (..)
   , ObjectType (..)
   , CacheType (..)
   , BridgeType (..)
   , OSDeviceType (..)
   , compareTypes
   , PageType(..)
   , MemoryObject (..)
   , Object (..)
   , CacheAttribute (..)
   , GroupAttribute (..)
   , PCIDeviceAttribute (..)
   , BridgeAttribute (..)
   , OSDeviceAttribute (..)
   , Attribute (..)
   , Distance (..)
   , initTopology
   , loadTopology
   , destroyTopology
   , dupTopology
   , checkTopology
   , getTopologyDepth
   , getTypeDepth
   , TypeDepth (..)
   , CpuBindFlag (..)
   , fromFlags
   , setCpuBind
   , getCpuBind
   , setProcCpuBind
   , getProcCpuBind
   , setThreadCpuBind
   , getThreadCpuBind
   , getLastCpuLocation
   , getProcLastCpuLocation
   , MemBindPolicy (..)
   , MemBindFlag (..)
   , setMemBindNodeSet
   , setMemBind
   , getMemBindNodeSet
   , getMemBind
   , setProcMemBindNodeSet
   , setProcMemBind
   , getProcMemBindNodeSet
   , getProcMemBind
   , setAreaMemBindNodeSet
   , setAreaMemBind
   , getAreaMemBindNodeSet
   , getAreaMemBind
   , alloc
   , allocMemBindNodeSet
   , allocMemBind
   , free
   , setPid
   , setSynthetic
   , setXml
   , setXmlBuffer
   , TopologyFlag (..)
   , setFlags
   , getFlags
   , isThisSystem
   , DiscoverySupport (..)
   , CpuBindSupport (..)
   , MemBindSupport (..)
   , TopologySupport (..)
   , getSupport
   , ignoreType
   , ignoreTypeKeepStructure
   , ignoreAllKeepStructure
   , setDistanceMatrix
   , setUserData
   , getUserData
   , RestrictFlag (..)
   , restrict
--   , insertMiscObject
--   , allocGroupObject
--   , insertGroupObject
--   , addSetsFromOtherObject
   , getObject
   , peekObject
   )
where

import Data.Word
import Data.Int
import Data.Bits
import Data.Vector
import Data.Map (Map)
import qualified Data.Map as Map

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types (CSize(..))
import Foreign.C.String (CString, peekCString)
import Foreign.CStorable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc (alloca)
import GHC.Generics (Generic)

import System.Hwloc.Bitmap (Bitmap (..))

-- | Indicate at runtime which hwloc API version was used at build time.
foreign import ccall "hwloc_get_api_version" getApiVersion :: Word

--  \defgroup hwlocality_object_sets Object Sets (hwloc_cpuset_t and hwloc_nodeset_t)
-- 
-- Hwloc uses bitmaps to represent two distinct kinds of object sets:
-- CPU sets (::hwloc_cpuset_t) and NUMA node sets (::hwloc_nodeset_t).
-- These types are both typedefs to a common back end type
-- (::hwloc_bitmap_t), and therefore all the hwloc bitmap functions
-- are applicable to both ::hwloc_cpuset_t and ::hwloc_nodeset_t (see
-- \ref hwlocality_bitmap).
-- 
-- The rationale for having two different types is that even though
-- the actions one wants to perform on these types are the same (e.g.,
-- enable and disable individual items in the set/mask), they're used
-- in very different contexts: one for specifying which processors to
-- use and one for specifying which NUMA nodes to use.  Hence, the
-- name difference is really just to reflect the intent of where the
-- type is used.
-- 

-- | A CPU set is a bitmap whose bits are set according to CPU
-- physical OS indexes.
-- 
-- It may be consulted and modified with the bitmap API as any
-- ::hwloc_bitmap_t (see hwloc/bitmap.h).
-- 
-- Each bit may be converted into a PU object using
-- hwloc_get_pu_obj_by_os_index().
-- 
newtype CpuSet = CpuSet Bitmap deriving (Show,Storable)

-- | A node set is a bitmap whose bits are set according to NUMA
-- memory node physical OS indexes.
-- 
-- It may be consulted and modified with the bitmap API as any
-- ::hwloc_bitmap_t (see hwloc/bitmap.h).
-- Each bit may be converted into a NUMA node object using
-- hwloc_get_numanode_obj_by_os_index().
-- 
-- When binding memory on a system without any NUMA node,
-- the single main memory bank is considered as NUMA node #0.
-- 
-- See also \ref hwlocality_helper_nodeset_convert.
-- 
newtype NodeSet = NodeSet Bitmap deriving (Show,Storable)



-- | Type of topology object.
-- 
-- \note Do not rely on the ordering or completeness of the values as new ones
-- may be defined in the future!  If you need to compare types, use
-- hwloc_compare_types() instead.
-- 
data ObjectType
   = ObjectTypeSystem      -- ^ Whole system (may be a cluster of machines).  
                           -- The whole system that is accessible to hwloc.  
                           -- That may comprise several machines in SSI systems.

   | ObjectTypeMachine     -- ^ Machine.  
                           -- The typical root object type.  
                           -- A set of processors and memory with cache coherency.

   | ObjectTypeNumaNode    -- ^ NUMA node.  
                           -- A set of processors around memory which the
                           -- processors can directly access.  There is always
                           -- at one such object in the topology even if the
                           -- machine is not NUMA.

   | ObjectTypePackage     -- ^ Physical package, what goes into a socket.  
                           -- In the physical meaning, i.e. that you can add or remove physically.

   | ObjectTypeCore        -- ^ Core.  
                           -- A computation unit (may be shared by several logical processors).

   | ObjectTypePU          -- ^ Processing Unit, or (Logical) Processor.
                           -- An execution unit (may share a core with some
                           -- other logical processors, e.g. in the case of an
                           -- SMT core).
                           -- Objects of this kind are always reported and can
                           -- thus be used as fallback when others are not.
           
   | ObjectTypeCacheL1     -- ^ Level 1 Data (or Unified) Cache
   | ObjectTypeCacheL2     -- ^ Level 2 Data (or Unified) Cache
   | ObjectTypeCacheL3     -- ^ Level 3 Data (or Unified) Cache
   | ObjectTypeCacheL4     -- ^ Level 4 Data (or Unified) Cache
   | ObjectTypeCacheL5     -- ^ Level 5 Data (or Unified) Cache
   | ObjectTypeICacheL1    -- ^ Level 1 instruction Cache, only when the ::HWLOC_TOPOLOGY_FLAG_ICACHES topology flag is set
   | ObjectTypeICacheL2    -- ^ Level 2 instruction Cache, only when the ::HWLOC_TOPOLOGY_FLAG_ICACHES topology flag is set
   | ObjectTypeICacheL3    -- ^ Level 3 instruction Cache, only when the ::HWLOC_TOPOLOGY_FLAG_ICACHES topology flag is set

   | ObjectTypeGroup       -- ^ Group objects.
                           -- Objects which do not fit in the above but are
                           -- detected by hwloc and are useful to take into
                           -- account for affinity. For instance, some operating systems
                           -- expose their arbitrary processors aggregation this
                           -- way.  And hwloc may insert such objects to group
                           -- NUMA nodes according to their distances.
           
                           -- These objects are ignored when they do not bring
                           -- any structure.
           
   | ObjectTypeMisc        -- ^ Miscellaneous objects.
                           -- Objects without particular meaning, that can e.g. be
                           -- added by the application for its own use, or by hwloc
                           -- for miscellaneous objects such as MemoryModule (DIMMs).
                           -- These objects are not listed in the main children list,
                           -- but rather in the dedicated misc children list.
                           -- Misc objects may only have Misc objects as children,
                           -- and those are in the dedicated misc children list as well.
                           -- Misc objects have NULL CPU and node sets.

   | ObjectTypeBridge      -- ^ Bridge.
                           -- Any bridge that connects the host or an I/O bus,
                           -- to another I/O bus.
                           -- They are not added to the topology unless I/O discovery
                           -- is enabled with hwloc_topology_set_flags().
                           -- I/O objects are not listed in the main children list,
                           -- but rather in the dedicated io children list.
                           -- I/O objects have NULL CPU and node sets.

   | ObjectTypePCIDevice   -- ^ PCI device.
                           -- They are not added to the topology unless I/O discovery
                           -- is enabled with hwloc_topology_set_flags().
                           -- I/O objects are not listed in the main children list,
                           -- but rather in the dedicated io children list.
                           -- I/O objects have NULL CPU and node sets.

   | ObjectTypeOSDevice    -- ^ Operating system device.
                           -- They are not added to the topology unless I/O discovery
                           -- is enabled with hwloc_topology_set_flags().
                           -- I/O objects are not listed in the main children list,
                           -- but rather in the dedicated io children list.
                           -- I/O objects have NULL CPU and node sets.
   deriving (Show,Eq,Enum)
           
-- | Cache type
data CacheType
   = CacheTypeUnified      -- ^ Unified cache
   | CacheTypeData         -- ^ Data cache
   | CacheTypeInstruction  -- ^ Instruction cache.
                           -- Only used when the ::HWLOC_TOPOLOGY_FLAG_ICACHES topology flag is set
   deriving (Show,Eq,Enum)

instance CStorable CacheType where
   cAlignment _ = 4
   cSizeOf    _ = 4
   cPeek ptr    = toEnum . fromIntegral <$> (peek (castPtr ptr) :: IO Word32)
   cPoke        = undefined


-- | Type of one side (upstream or downstream) of an I/O bridge
data BridgeType
   = BridgeTypeHost        -- ^ Host-side of a bridge, only possible upstream
   | BridgeTypePCI         -- ^ PCI-side of a bridge
   deriving (Show,Eq,Enum)

instance CStorable BridgeType where
   cAlignment _ = 4
   cSizeOf    _ = 4
   cPeek ptr    = toEnum . fromIntegral <$> (peek (castPtr ptr) :: IO Word32)
   cPoke        = undefined

-- | Type of a OS device
data OSDeviceType
   = OSDeviceTypeBlock        -- ^ Operating system block device. For instance "sda" on Linux.
   | OSDeviceTypeGPU          -- ^ Operating system GPU device. For instance
                              -- ":0.0" for a GL display, "card0" for a Linux DRM device.
   | OSDeviceTypeNetwork      -- ^ Operating system network device. For instance the "eth0" interface on Linux.
   | OSDeviceTypeOpenFabrics  -- ^ Operating system openfabrics device. For instance the "mlx4_0" InfiniBand HCA device on Linux.
   | OSDeviceTypeDMA          -- ^ Operating system dma engine device. For instance the "dma0chan0" DMA channel on Linux.
   | OSDeviceTypeCoProc       -- ^ Operating system co-processor device.
                              -- For instance "mic0" for a Xeon Phi (MIC) on Linux,
                              -- "opencl0d0" for a OpenCL device,
                              -- "cuda0" for a CUDA device. */
   deriving (Show,Eq,Enum)

instance CStorable OSDeviceType where
   cAlignment _ = 4
   cSizeOf    _ = 4
   cPeek ptr    = toEnum . fromIntegral <$> (peek (castPtr ptr) :: IO Word32)
   cPoke        = undefined

-- | Compare the depth of two object types
-- 
-- Types shouldn't be compared as they are, since newer ones may be added in
-- the future.  This function returns less than, equal to, or greater than zero
-- respectively if \p type1 objects usually include \p type2 objects, are the
-- same as \p type2 objects, or are included in \p type2 objects. If the types
-- can not be compared (because neither is usually contained in the other),
-- ::HWLOC_TYPE_UNORDERED is returned.  Object types containing CPUs can always
-- be compared (usually, a system contains machines which contain nodes which
-- contain packages which contain caches, which contain cores, which contain
-- processors).
-- 
-- \note ::HWLOC_OBJ_PU will always be the deepest.
-- \note This does not mean that the actual topology will respect that order:
-- e.g. as of today cores may also contain caches, and packages may also contain
-- nodes. This is thus just to be seen as a fallback comparison method.
-- 
compareTypes :: ObjectType -> ObjectType -> Maybe Ordering
compareTypes obj1 obj2 = 
   case compareTypes' (fromEnum obj1) (fromEnum obj2) of
      x | x == maxBound -> Nothing
        | x < 0         -> Just LT
        | x > 0         -> Just GT
        | otherwise     -> Just EQ
      

foreign import ccall "hwloc_compare_types" compareTypes' :: Int -> Int -> Int

-- | Memory page type
data PageType = PageType
   { pageTypeSize    :: Word64   -- ^ Size of pages
   , pageTypeCount   :: Word64   -- ^ Number of pages of this size
   }
   deriving (Show,Eq,Generic)

instance CStorable PageType
instance Storable PageType where
   sizeOf    = cSizeOf
   alignment = cAlignment
   poke      = cPoke
   peek      = cPeek


-- | Object memory
data MemoryObject = MemoryObject
   { totalSize :: Word64      -- ^ Total memory (in bytes) in this object and its children
   , localSize :: Word64      -- ^ Local memory (in bytes)
   , pageTypes :: [PageType]  -- ^ Array of local memory page types
   }
   deriving (Show)

roundTo :: Int -> Int -> Int
roundTo x y = x + (y - (x `mod` y))

instance Storable MemoryObject where
   alignment _ = 8
   sizeOf    _ = (16 + sizeOf (undefined :: Word32) + sizeOf (undefined :: Ptr ())) `roundTo` 8
   peek ptr    = do
      ts <- peekByteOff ptr 0
      ls <- peekByteOff ptr 8
      (n :: Word)  <- peekByteOff ptr 16
      addr <- peekByteOff ptr (16 + sizeOf (undefined :: Word))
      typs <- peekArray (fromIntegral n) addr
      return $ MemoryObject ts ls typs

   poke _ = undefined



-- | Structure of a topology object
-- 
-- Applications must not modify any field except hwloc_obj.userdata.
-- 
data Object = Object
   { objectType      :: ObjectType        -- ^ Type of object
   , objectOsIndex   :: Word32            -- ^ OS-provided physical index number.
                                          -- It is not guaranteed unique across the entire machine,
                                          -- except for PUs and NUMA nodes.

   , objectName      :: String            -- ^ Object description if any
   , objectMemory    :: MemoryObject      -- ^ Memory attributes

   , objectAttribute :: Maybe Attribute   -- ^ Object type-specific Attributes,

   , objectDepth     :: Word32            -- ^ Vertical index in the hierarchy.
                                          -- If the topology is symmetric, this is equal to the
                                          -- parent depth plus one, and also equal to the number
                                          -- of parent/child links from the root object to here.

   , objectLogicalIndex :: Word32         -- ^ Horizontal index in the whole list of similar objects,
                                          -- hence guaranteed unique across the entire machine.
                                          -- Could be a "cousin_rank" since it's the rank within the "cousin" list below

   , objectNextCousin :: Ptr Object       -- ^ Next object of same type and depth
   , objectPreviousCousin :: Ptr Object   -- ^ Previous object of same type and depth

   , objectParent :: Ptr Object           -- ^ Parent, \c NULL if root (system object)
   , objectSiblingRank :: Word32          -- ^ Index in parent's \c children[]
                                          -- array. Or the index in parent's I/O or Misc children list
  
   , objectNextSibling :: Ptr Object      -- ^ Next object below the same parent
   , objectPreviousSibling :: Ptr Object  -- ^ Previous object below the same parent

   , objectChildren :: [Ptr Object]       -- ^ Children
   
   , objectSymetricSubTree :: Int32       -- ^ Set if the subtree of normal objects below this object is symmetric,
                                          -- which means all children and their children have identical subtrees.
                                          -- I/O and Misc children are ignored.
                                          -- 
                                          -- If set in the topology root object, lstopo may export the topology
                                          -- as a synthetic string.

   , objectIOArity :: Word32
   , objectIOFirstChild :: Ptr Object     -- ^ IO first child

   , objectMiscArity :: Word32
   , objectMiscChildren :: Ptr Object     -- ^ Mist first child

   , objectCpuSet :: CpuSet               -- ^ CPUs covered by this object
                                          -- 
                                          -- This is the set of CPUs for which there are PU objects in the topology
                                          -- under this object, i.e. which are known to be physically contained in this
                                          -- object and known how (the children path between this object and the PU
                                          -- objects).
                                          -- 
                                          -- If the ::HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM configuration flag is set,
                                          -- some of these CPUs may not be allowed for binding, see allowed_cpuset.
                                          -- 
                                          -- All objects have non-NULL CPU and node sets except Misc and I/O objects.
                                          --
                                          -- Its value must not be changed, Bitmap.dup must be used instead.

   , objectCompleteCpuSet :: CpuSet       -- ^ The complete CPU set of logical processors of this object,
                                          -- 
                                          -- This may include not only the same as the cpuset field, but also the CPUs for
                                          -- which topology information is unknown or incomplete, the offlines CPUS, and
                                          -- the CPUs that are ignored when the ::HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM flag
                                          -- is not set.
                                          -- Thus no corresponding PU object may be found in the topology, because the
                                          -- precise position is undefined. It is however known that it would be somewhere
                                          -- under this object.
                                          -- 
                                          -- Its value must not be changed, hwloc_bitmap_dup() must be used instead.

   , objectAllowedCpuSet :: CpuSet        -- ^ The CPU set of allowed logical processors
                                          -- 
                                          -- This includes the CPUs contained in this object which are allowed for
                                          -- binding, i.e. passing them to the hwloc binding functions should not return
                                          -- permission errors.  This is usually restricted by administration rules.
                                          -- 
                                          -- If the ::HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM configuration flag is set,
                                          -- allowed_cpuset may be smaller than cpuset. Otherwise they are identical.
                                          -- 
                                          -- Its value must not be changed, hwloc_bitmap_dup() must be used instead.


   , objectNodeSet :: NodeSet             -- ^ NUMA nodes covered by this object or containing this object
                                          -- 
                                          -- This is the set of NUMA nodes for which there are NODE objects in the
                                          -- topology under or above this object, i.e. which are known to be physically
                                          -- contained in this object or containing it and known how (the children path
                                          -- between this object and the NODE objects).
                                          -- 
                                          -- In the end, these nodes are those that are close to the current object.
                                          -- 
                                          -- If the ::HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM configuration flag is set,
                                          -- some of these nodes may not be allowed for allocation, see allowed_nodeset.
                                          -- 
                                          -- If there are no NUMA nodes in the machine, all the memory is close to this
                                          -- object, so only the first bit may be set in \p nodeset.
                                          -- 
                                          -- All objects have non-NULL CPU and node sets except Misc and I/O objects.
                                          -- 
                                          -- Its value must not be changed, hwloc_bitmap_dup() must be used instead.

   , objectCompleteNodeSet :: NodeSet     -- ^ The complete NUMA node set of this object,
                                          -- 
                                          -- This may include not only the same as the nodeset field, but also the NUMA
                                          -- nodes for which topology information is unknown or incomplete, the offlines
                                          -- nodes, and the nodes that are ignored when the ::HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM
                                          -- flag is not set.
                                          -- Thus no corresponding NODE object may be found in the topology, because the
                                          -- precise position is undefined. It is however known that it would be
                                          -- somewhere under this object.
                                          -- 
                                          -- If there are no NUMA nodes in the machine, all the memory is close to this
                                          -- object, so only the first bit is set in \p complete_nodeset.
                                          -- 
                                          -- Its value must not be changed, hwloc_bitmap_dup() must be used instead.

   , objectAllowedNodeSet :: NodeSet      -- ^ The set of allowed NUMA memory nodes
                                          -- 
                                          -- This includes the NUMA memory nodes contained in this object which are
                                          -- allowed for memory allocation, i.e. passing them to NUMA node-directed
                                          -- memory allocation should not return permission errors. This is usually
                                          -- restricted by administration rules.
                                          -- 
                                          -- If the ::HWLOC_TOPOLOGY_FLAG_WHOLE_SYSTEM configuration flag is set,
                                          -- allowed_nodeset may be smaller than nodeset. Otherwise they are identical.
                                          -- 
                                          -- If there are no NUMA nodes in the machine, all the memory is close to this
                                          -- object, so only the first bit may be set in \p allowed_nodeset.
                                          -- 
                                          -- Its value must not be changed, hwloc_bitmap_dup() must be used instead.

   , objectDistances :: [Ptr Distance]    -- ^ Distances between all objects at same depth below this object
   , objectInfo      :: Map String String -- ^ Array of stringified info type=name

   , objectUserData :: Ptr ()             -- ^ Application-given private data pointer,
                                          -- initialized to \c NULL, use it as you wish.
                                          -- See hwloc_topology_set_userdata_export_callback() in hwloc/export.h
                                          -- if you wish to export this field to XML. */
   }
   deriving (Show)

data Info = Info String String

instance Storable Info where
   alignment _ = 8
   sizeOf _    = 2 * sizeOf (undefined :: Ptr ())
   poke        = undefined
   peek ptr    = do
      nptr <- peekByteOff ptr 0
      vptr <- peekByteOff ptr (sizeOf (undefined :: Ptr ()))
      Info <$> peekCString nptr <*> peekCString vptr


peekObject :: Ptr Object -> IO Object
peekObject ptr = do
   typ     <- toEnum . fromIntegral <$> (peekByteOff ptr 0 :: IO Word32)
   osindex <- peekByteOff ptr 4
   nameptr <- peekByteOff ptr 8
   name    <- if nameptr == nullPtr
                  then return ""
                  else peekCString nameptr
   memobj  <- peekByteOff ptr 16
   ptrattr <- peekByteOff ptr 48  :: IO (Ptr ())

   attr <- case typ of
      ObjectTypeSystem     -> return Nothing
      ObjectTypeMachine    -> return Nothing
      ObjectTypeNumaNode   -> return Nothing
      ObjectTypePackage    -> return Nothing
      ObjectTypeCore       -> return Nothing
      ObjectTypePU         -> return Nothing
      ObjectTypeCacheL1    -> Just . AttributeCache     <$> peek (castPtr ptrattr)
      ObjectTypeCacheL2    -> Just . AttributeCache     <$> peek (castPtr ptrattr)
      ObjectTypeCacheL3    -> Just . AttributeCache     <$> peek (castPtr ptrattr)
      ObjectTypeCacheL4    -> Just . AttributeCache     <$> peek (castPtr ptrattr)
      ObjectTypeCacheL5    -> Just . AttributeCache     <$> peek (castPtr ptrattr)
      ObjectTypeICacheL1   -> Just . AttributeCache     <$> peek (castPtr ptrattr)
      ObjectTypeICacheL2   -> Just . AttributeCache     <$> peek (castPtr ptrattr)
      ObjectTypeICacheL3   -> Just . AttributeCache     <$> peek (castPtr ptrattr)
      ObjectTypeGroup      -> Just . AttributeGroup     <$> peek (castPtr ptrattr)
      ObjectTypeMisc       -> return Nothing
      ObjectTypeBridge     -> Just . AttributeBridge    <$> peek (castPtr ptrattr)
      ObjectTypePCIDevice  -> Just . AttributePCIDevice <$> peek (castPtr ptrattr)
      ObjectTypeOSDevice   -> Just . AttributeOsDevice  <$> peek (castPtr ptrattr)
      

   depth    <- peekByteOff ptr 56
   logindex <- peekByteOff ptr 60

   nextcousin  <- peekByteOff ptr 64
   prevcousin  <- peekByteOff ptr 72
   parent      <- peekByteOff ptr 80
   sibrank     <- peekByteOff ptr 88
   nextsibling <- peekByteOff ptr 96
   prevsibling <- peekByteOff ptr 104

   arity       <- peekByteOff ptr 112
   childrenPtr <- peekByteOff ptr 120
   children    <- peekArray arity childrenPtr
   -- skip first child and last child
   -- firstchild <- peekByteOff ptr 128
   -- lastchild  <- peekByteOff ptr 136

   symsub  <- peekByteOff ptr 144
   ioarity <- peekByteOff ptr 148
   iofchild <- peekByteOff ptr 152

   miscarity <- peekByteOff ptr 160
   miscfchild <- peekByteOff ptr 168

   cpuset    <- peekByteOff ptr 176
   ccpuset   <- peekByteOff ptr 184
   acpuset   <- peekByteOff ptr 192
   nodeset   <- peekByteOff ptr 200
   cnodeset  <- peekByteOff ptr 208
   anodeset  <- peekByteOff ptr 216

   distPtr   <- peekByteOff ptr 224
   distCount <- peekByteOff ptr 232
   distances <- peekArray distCount distPtr

   infoPtr   <- peekByteOff ptr 240
   infoCount <- peekByteOff ptr 248

   let f (Info x y) = (x,y)
   infos     <- Map.fromList . fmap f <$> peekArray infoCount infoPtr

   userdata <- peekByteOff ptr 256



   return $ Object 
      typ
      osindex
      name
      memobj
      attr
      depth
      logindex
      prevcousin
      nextcousin
      parent
      sibrank
      nextsibling
      prevsibling
      children
      symsub
      ioarity
      iofchild
      miscarity
      miscfchild
      cpuset
      ccpuset
      acpuset
      nodeset
      cnodeset
      anodeset
      distances
      infos
      userdata


-- | Cache-specific Object Attributes
data CacheAttribute = CacheAttribute
   { cacheAttributeSize          :: Word64   -- ^ Size of cache in bytes
   , cacheAttributeDepth         :: Word32   -- ^ Depth of cache (e.g., L1, L2, ...etc.)
   , cacheAttributeLineSize      :: Word32   -- ^ Cache-line size in bytes. 0 if unknown
   , cacheAttributeAssociativity :: Int32    -- ^ Ways of associativity, -1 if fully associative, 0 if unknown
   , cacheAttributeType          :: CacheType -- ^ Cache type
   }
   deriving (Show,Generic)

instance CStorable CacheAttribute
instance Storable CacheAttribute where
   alignment = cAlignment
   sizeOf    = cSizeOf
   peek      = cPeek
   poke      = cPoke
   

-- | Group-specific Object Attributes
data GroupAttribute = GroupAttribute
   { groupAttributeDepth         :: Word32     -- ^ Depth of group object
   }
   deriving (Show,Generic)

instance CStorable GroupAttribute
instance Storable GroupAttribute where
   alignment = cAlignment
   sizeOf    = cSizeOf
   peek      = cPeek
   poke      = cPoke

-- | PCI Device specific Object Attributes
data PCIDeviceAttribute = PCIDeviceAttribute
   { pciDeviceAttributeDomain       :: Word16
   , pciDeviceAttributeBus          :: Word8
   , pciDeviceAttributeDevice       :: Word8
   , pciDeviceAttributeFunc         :: Word8
   , pciDeviceAttributeClassId      :: Word16
   , pciDeviceAttributeVendorId     :: Word16
   , pciDeviceAttributeDeviceId     :: Word16
   , pciDeviceAttributeSubVendorId  :: Word16
   , pciDeviceAttributeSubDeviceId  :: Word16
   , pciDeviceAttributeRevision     :: Word8
   , pciDeviceAttributeLinkSpeed    :: Float    -- ^ in GB/s
   }
   deriving (Show,Generic)

instance CStorable PCIDeviceAttribute
instance Storable PCIDeviceAttribute where
   alignment = cAlignment
   sizeOf    = cSizeOf
   peek      = cPeek
   poke      = cPoke

-- | Bridge specific Object Attributes
data BridgeAttribute = BridgeAttribute
   { bridgeAttributeUpstreamPCI                 :: PCIDeviceAttribute
   , bridgeAttributeUpstreamType                :: BridgeType
   , bridgeAttributeDownstreamPCIDomain         :: Word16
   , bridgeAttributeDownstreamPCISecondaryBus   :: Word8
   , bridgeAttributeDownstreamPCISubordinateBus :: Word8
   , bridgeAttributeDownstreamType              :: BridgeType
   , bridgeAttributeDepth                       :: Word32
   }
   deriving (Show,Generic)

instance CStorable BridgeAttribute
instance Storable BridgeAttribute where
   alignment = cAlignment
   sizeOf    = cSizeOf
   peek      = cPeek
   poke      = cPoke

-- | OS Device specific Object Attributes
data OSDeviceAttribute = OSDeviceAttribute
   { osDeviceAttributeType :: OSDeviceType
   }
   deriving (Show,Generic)

instance CStorable OSDeviceAttribute
instance Storable OSDeviceAttribute where
   alignment = cAlignment
   sizeOf    = cSizeOf
   peek      = cPeek
   poke      = cPoke

-- | Object type-specific Attributes
data Attribute
   = AttributeCache CacheAttribute
   | AttributeGroup GroupAttribute
   | AttributePCIDevice PCIDeviceAttribute
   | AttributeBridge BridgeAttribute
   | AttributeOsDevice OSDeviceAttribute
   deriving (Show)

-- | Distances between objects
-- 
-- One object may contain a distance structure describing distances
-- between all its descendants at a given relative depth. If the
-- containing object is the root object of the topology, then the
-- distances are available for all objects in the machine.
-- 
-- If the \p latency pointer is not \c NULL, the pointed array contains
-- memory latencies (non-zero values), see below.
-- 
-- In the future, some other types of distances may be considered.
-- In these cases, \p latency may be \c NULL.
-- 
data Distance = Distance
   { distanceRelativeDepth :: Word     -- ^ Relative depth of the considered objects
                                       -- below the object containing this distance information.
   , distanceLatencies :: Maybe (Vector Float) -- ^ Matrix of latencies between objects, stored as a one-dimension array.
                                               -- May be \c NULL if the distances considered here are not latencies.
                                               -- 
                                               -- Unless defined by the user, this currently contains latencies
                                               -- between NUMA nodes (as reported in the System Locality Distance Information Table
                                               -- (SLIT) in the ACPI specification), which may or may not be accurate.
                                               -- It corresponds to the latency for accessing the memory of one node
                                               -- from a core in another node.
                                               -- 
                                               -- Values are normalized to get 1.0 as the minimal value in the matrix.
                                               -- Latency from i-th to j-th object is stored in slot i*nbobjs+j.
   , distanceMaxLatency :: Float       -- ^ The maximal value in the latency matrix
   , distanceBaseLatency :: Float      -- ^ The multiplier that should be applied to latency matrix
                                       -- to retrieve the original OS-provided latencies.
                                       -- Usually 10 on Linux since ACPI SLIT uses 10 for local latency.
   }
   deriving (Show)



newtype Topology = Topology (Ptr ())

-- | Allocate a topology context.
initTopology :: IO (Maybe Topology)
initTopology = alloca $ \ptr -> do
   t <- initTopology' ptr
   case t of
      0 -> Just . Topology <$> peek ptr
      _ -> return Nothing

foreign import ccall "hwloc_topology_init" initTopology' :: Ptr (Ptr ()) -> IO Int


-- | Build the actual topology
-- 
-- Build the actual topology once initialized with hwloc_topology_init() and
-- tuned with \ref hwlocality_configuration and \ref hwlocality_setsource routines.
-- No other routine may be called earlier using this topology context.
-- 
-- \param topology is the topology to be loaded with objects.
-- 
-- \return 0 on success, -1 on error.
-- 
-- \note On failure, the topology is reinitialized. It should be either
-- destroyed with hwloc_topology_destroy() or configured and loaded again.
-- 
-- \note This function may be called only once per topology.
-- 
-- \sa hwlocality_configuration and hwlocality_setsource
-- 
foreign import ccall "hwloc_topology_load" loadTopology :: Topology -> IO Int

-- | Terminate and free a topology context
-- 
-- \param topology is the topology to be freed
-- 
foreign import ccall "hwloc_topology_destroy" destroyTopology :: Topology -> IO ()

-- | Duplicate a topology.
-- 
-- The entire topology structure as well as its objects
-- are duplicated into a new one.
-- 
-- This is useful for keeping a backup while modifying a topology.
-- 
dupTopology :: Topology -> IO (Maybe Topology)
dupTopology t = alloca $ \ptr -> do
   r <- dupTopology' ptr t
   case r of
      0 -> Just . Topology <$> peek ptr
      _ -> return Nothing

foreign import ccall "hwloc_topology_dup" dupTopology' :: Ptr (Ptr ()) -> Topology -> IO Int


-- | Run internal checks on a topology structure
-- 
-- The program aborts if an inconsistency is detected in the given topology.
-- 
-- \param topology is the topology to be checked
-- 
-- \note This routine is only useful to developers.
-- 
-- \note The input topology should have been previously loaded with
-- hwloc_topology_load().
-- 
foreign import ccall "hwloc_topology_check" checkTopology :: Topology -> IO ()


-- | Get the depth of the hierarchical tree of objects.
-- 
-- This is the depth of ::HWLOC_OBJ_PU objects plus one.
-- 
foreign import ccall "hwloc_topology_get_depth" getTopologyDepth :: Topology -> IO Word

-- | Returns the depth of objects of type \p type.
-- 
-- If no object of this type is present on the underlying architecture, or if
-- the OS doesn't provide this kind of information, the function returns
-- ::HWLOC_TYPE_DEPTH_UNKNOWN.
-- 
-- If type is absent but a similar type is acceptable, see also
-- hwloc_get_type_or_below_depth() and hwloc_get_type_or_above_depth().
-- 
-- If ::HWLOC_OBJ_GROUP is given, the function may return ::HWLOC_TYPE_DEPTH_MULTIPLE
-- if multiple levels of Groups exist.
-- 
-- If an I/O object type is given, the function returns a virtual value
-- because I/O objects are stored in special levels that are not CPU-related.
-- This virtual depth may be passed to other hwloc functions such as
-- hwloc_get_obj_by_depth() but it should not be considered as an actual
-- depth by the application. In particular, it should not be compared with
-- any other object depth or with the entire topology depth.
-- 
getTypeDepth :: Topology -> ObjectType -> IO TypeDepth
getTypeDepth topo typ = do
   r <- getTypeDepth' topo (fromEnum typ)
   return $ case r of
      -1 -> TypeDepthUnknown
      -2 -> TypeDepthMultiple
      -3 -> TypeDepthBridge
      -4 -> TypeDepthPCIDevice
      -5 -> TypeDepthOSDevice
      -6 -> TypeDepthMisc
      _  -> TypeDepth (fromIntegral r)

foreign import ccall "hwloc_get_type_depth" getTypeDepth' :: Topology -> Int -> IO Int

data TypeDepth
   = TypeDepth Word
   | TypeDepthUnknown      -- ^ No object of given type exists in the topology
   | TypeDepthMultiple     -- ^ Objects of given type exist at different depth in the topology (only for Groups)
   | TypeDepthBridge       -- ^ Virtual depth for bridge object level
   | TypeDepthPCIDevice    -- ^ Virtual depth for PCI device object level
   | TypeDepthOSDevice     -- ^ Virtual depth for software device object level
   | TypeDepthMisc         -- ^ Virtual depth for Misc object
   deriving (Show,Eq)

-- CPU binding
-- 
-- It is often useful to call hwloc_bitmap_singlify() first so that a single CPU
-- remains in the set. This way, the process will not even migrate between
-- different CPUs inside the given set.
-- Some operating systems also only support that kind of binding.
-- 
-- Some operating systems do not provide all hwloc-supported
-- mechanisms to bind processes, threads, etc.
-- hwloc_topology_get_support() may be used to query about the actual CPU
-- binding support in the currently used operating system.
-- 
-- When the requested binding operation is not available and the
-- ::HWLOC_CPUBIND_STRICT flag was passed, the function returns -1.
-- \p errno is set to \c ENOSYS when it is not possible to bind the requested kind of object
-- processes/threads. errno is set to \c EXDEV when the requested cpuset
-- can not be enforced (e.g. some systems only allow one CPU, and some
-- other systems only allow one NUMA node).
-- 
-- If ::HWLOC_CPUBIND_STRICT was not passed, the function may fail as well,
-- or the operating system may use a slightly different operation
-- (with side-effects, smaller binding set, etc.)
-- when the requested operation is not exactly supported.
-- 
-- The most portable version that should be preferred over the others,
-- whenever possible, is the following one which just binds the current program,
-- assuming it is single-threaded:
-- 
-- \code
-- hwloc_set_cpubind(topology, set, 0),
-- \endcode
-- 
-- If the program may be multithreaded, the following one should be preferred
-- to only bind the current thread:
-- 
-- \code
-- hwloc_set_cpubind(topology, set, HWLOC_CPUBIND_THREAD),
-- \endcode
-- 
-- \sa Some example codes are available under doc/examples/ in the source tree.
-- 
-- \note To unbind, just call the binding function with either a full cpuset or
-- a cpuset equal to the system cpuset.
-- 
-- \note On some operating systems, CPU binding may have effects on memory binding, see
-- ::HWLOC_CPUBIND_NOMEMBIND
-- 
-- \note Running lstopo --top or hwloc-ps can be a very convenient tool to check
-- how binding actually happened.
-- @{
-- 

-- | Process/Thread binding flags.
-- 
-- These bit flags can be used to refine the binding policy.
-- 
-- The default (0) is to bind the current process, assumed to be
-- single-threaded, in a non-strict way.  This is the most portable
-- way to bind as all operating systems usually provide it.
-- 
-- \note Not all systems support all kinds of binding.  See the
-- "Detailed Description" section of \ref hwlocality_cpubinding for a
-- description of errors that can occur.
-- 
data CpuBindFlag
   = CpuBindProcess  -- ^ Bind all threads of the current (possibly) multithreaded process.
   | CpuBindThread   -- ^ Bind current thread of current process.
   | CpuBindStrict   -- ^ Request for strict binding from the OS.
                     -- 
                     -- By default, when the designated CPUs are all busy while other
                     -- CPUs are idle, operating systems may execute the thread/process
                     -- on those other CPUs instead of the designated CPUs, to let them
                     -- progress anyway.  Strict binding means that the thread/process
                     -- will _never_ execute on other cpus than the designated CPUs, even
                     -- when those are busy with other tasks and other CPUs are idle.
                     -- 
                     -- \note Depending on the operating system, strict binding may not
                     -- be possible (e.g., the OS does not implement it) or not allowed
                     -- (e.g., for an administrative reasons), and the function will fail
                     -- in that case.
                     -- 
                     -- When retrieving the binding of a process, this flag checks
                     -- whether all its threads  actually have the same binding. If the
                     -- flag is not given, the binding of each thread will be
                     -- accumulated.
                     -- 
                     -- \note This flag is meaningless when retrieving the binding of a
                     -- thread.
                     -- \hideinitializer
                     -- 
   | CpuBindNoMemBind   -- ^ Avoid any effect on memory binding
                        -- 
                        -- On some operating systems, some CPU binding function would also
                        -- bind the memory on the corresponding NUMA node.  It is often not
                        -- a problem for the application, but if it is, setting this flag
                        -- will make hwloc avoid using OS functions that would also bind
                        -- memory.  This will however reduce the support of CPU bindings,
                        -- i.e. potentially return -1 with errno set to ENOSYS in some
                        -- cases.
                        -- 
                        -- This flag is only meaningful when used with functions that set
                        -- the CPU binding.  It is ignored when used with functions that get
                        -- CPU binding information.
                        -- \hideinitializer
                        -- 
   deriving (Show,Eq,Enum)

fromFlags :: Enum a => [a] -> Word
fromFlags = rec 0
   where
      rec v [] = v
      rec v (x:xs) = rec (v .|. (1 `shiftL` fromIntegral (fromEnum x))) xs

-- | Bind current process or thread on cpus given in physical bitmap \p set.
-- 
-- \return -1 with errno set to ENOSYS if the action is not supported
-- \return -1 with errno set to EXDEV if the binding cannot be enforced
-- 
setCpuBind :: Topology -> CpuSet -> [CpuBindFlag] -> IO Int
setCpuBind topo cpuset flags = setCpuBind' topo cpuset (fromFlags flags)

foreign import ccall "hwloc_set_cpubind" setCpuBind' :: Topology -> CpuSet -> Word -> IO Int


-- | Get current process or thread binding.
-- 
-- Writes into \p set the physical cpuset which the process or thread (according to \e
-- flags) was last bound to.
-- 
getCpuBind :: Topology -> CpuSet -> [CpuBindFlag] -> IO Int
getCpuBind topo cpuset flags = getCpuBind' topo cpuset (fromFlags flags)

foreign import ccall "hwloc_get_cpubind" getCpuBind' :: Topology -> CpuSet -> Word -> IO Int



-- | Bind a process \p pid on cpus given in physical bitmap \p set.
-- 
-- \note \p hwloc_pid_t is \p pid_t on Unix platforms,
-- and \p HANDLE on native Windows platforms.
-- 
-- \note As a special case on Linux, if a tid (thread ID) is supplied
-- instead of a pid (process ID) and ::HWLOC_CPUBIND_THREAD is passed in flags,
-- the binding is applied to that specific thread.
-- 
-- \note On non-Linux systems, ::HWLOC_CPUBIND_THREAD can not be used in \p flags.
-- 
setProcCpuBind :: Topology -> Word -> CpuSet -> [CpuBindFlag] -> IO Int
setProcCpuBind topo pid cpuset flags = setProcCpuBind' topo pid cpuset (fromFlags flags)

foreign import ccall "hwloc_set_proc_cpubind" setProcCpuBind' :: Topology -> Word -> CpuSet -> Word -> IO Int

-- | Get the current physical binding of process \p pid.
-- 
-- \note \p hwloc_pid_t is \p pid_t on Unix platforms,
-- and \p HANDLE on native Windows platforms.
-- 
-- \note As a special case on Linux, if a tid (thread ID) is supplied
-- instead of a pid (process ID) and HWLOC_CPUBIND_THREAD is passed in flags,
-- the binding for that specific thread is returned.
-- 
-- \note On non-Linux systems, HWLOC_CPUBIND_THREAD can not be used in \p flags.
-- 
getProcCpuBind :: Topology -> Word -> CpuSet -> [CpuBindFlag] -> IO Int
getProcCpuBind topo pid cpuset flags = getProcCpuBind' topo pid cpuset (fromFlags flags)

foreign import ccall "hwloc_get_proc_cpubind" getProcCpuBind' :: Topology -> Word -> CpuSet -> Word -> IO Int

-- | Bind a thread \p thread on cpus given in physical bitmap \p set.
-- 
-- \note \p hwloc_thread_t is \p pthread_t on Unix platforms,
-- and \p HANDLE on native Windows platforms.
-- 
-- \note ::HWLOC_CPUBIND_PROCESS can not be used in \p flags.
-- 
setThreadCpuBind :: Topology -> Word -> CpuSet -> [CpuBindFlag] -> IO Int
setThreadCpuBind topo tid cpuset flags = setThreadCpuBind' topo tid cpuset (fromFlags flags)

foreign import ccall "hwloc_set_thread_cpubind" setThreadCpuBind' :: Topology -> Word -> CpuSet -> Word -> IO Int


-- | Get the current physical binding of thread \p tid.
-- 
-- \note \p hwloc_thread_t is \p pthread_t on Unix platforms,
-- and \p HANDLE on native Windows platforms.
-- 
-- \note ::HWLOC_CPUBIND_PROCESS can not be used in \p flags.
-- 
getThreadCpuBind :: Topology -> Word -> CpuSet -> [CpuBindFlag] -> IO Int
getThreadCpuBind topo tid cpuset flags = getThreadCpuBind' topo tid cpuset (fromFlags flags)

foreign import ccall "hwloc_get_thread_cpubind" getThreadCpuBind' :: Topology -> Word -> CpuSet -> Word -> IO Int

-- | Get the last physical CPU where the current process or thread ran.
-- 
-- The operating system may move some tasks from one processor
-- to another at any time according to their binding,
-- so this function may return something that is already
-- outdated.
-- 
-- \p flags can include either ::HWLOC_CPUBIND_PROCESS or ::HWLOC_CPUBIND_THREAD to
-- specify whether the query should be for the whole process (union of all CPUs
-- on which all threads are running), or only the current thread. If the
-- process is single-threaded, flags can be set to zero to let hwloc use
-- whichever method is available on the underlying OS.
-- 
getLastCpuLocation :: Topology -> CpuSet -> [CpuBindFlag] -> IO Int
getLastCpuLocation topo cpuset flags = getLastCpuLocation' topo cpuset (fromFlags flags)

foreign import ccall "hwloc_get_last_cpu_location" getLastCpuLocation' :: Topology -> CpuSet -> Word -> IO Int

-- | Get the last physical CPU where a process ran.
-- 
-- The operating system may move some tasks from one processor
-- to another at any time according to their binding,
-- so this function may return something that is already
-- outdated.
-- 
-- \note \p hwloc_pid_t is \p pid_t on Unix platforms,
-- and \p HANDLE on native Windows platforms.
-- 
-- \note As a special case on Linux, if a tid (thread ID) is supplied
-- instead of a pid (process ID) and ::HWLOC_CPUBIND_THREAD is passed in flags,
-- the last CPU location of that specific thread is returned.
-- 
-- \note On non-Linux systems, ::HWLOC_CPUBIND_THREAD can not be used in \p flags.
-- 
getProcLastCpuLocation :: Topology -> Word -> CpuSet -> [CpuBindFlag] -> IO Int
getProcLastCpuLocation topo pid cpuset flags = getProcLastCpuLocation' topo pid cpuset (fromFlags flags)

foreign import ccall "hwloc_get_proc_last_cpu_location" getProcLastCpuLocation' :: Topology -> Word -> CpuSet -> Word -> IO Int


-- Memory binding
-- 
-- Memory binding can be done three ways:
-- 
-- - explicit memory allocation thanks to hwloc_alloc_membind() and friends:
--   the binding will have effect on the memory allocated by these functions.
-- - implicit memory binding through binding policy: hwloc_set_membind() and
--   friends only define the current policy of the process, which will be
--   applied to the subsequent calls to malloc() and friends.
-- - migration of existing memory ranges, thanks to hwloc_set_area_membind()
--   and friends, which move already-allocated data.
-- 
-- Not all operating systems support all three ways.
-- hwloc_topology_get_support() may be used to query about the actual memory
-- binding support in the currently used operating system.
-- 
-- When the requested binding operation is not available and the
-- ::HWLOC_MEMBIND_STRICT flag was passed, the function returns -1.
-- \p errno will be set to \c ENOSYS when the system does support
-- the specified action or policy
-- (e.g., some systems only allow binding memory on a per-thread
-- basis, whereas other systems only allow binding memory for all
-- threads in a process).
-- \p errno will be set to EXDEV when the requested cpuset can not be enforced
-- (e.g., some systems only allow binding memory to a single NUMA node).
-- 
-- If ::HWLOC_MEMBIND_STRICT was not passed, the function may fail as well,
-- or the operating system may use a slightly different operation
-- (with side-effects, smaller binding set, etc.)
-- when the requested operation is not exactly supported.
-- 
-- The most portable form that should be preferred over the others
-- whenever possible is as follows.
-- It allocates some memory hopefully bound to the specified set.
-- To do so, hwloc will possibly have to change the current memory
-- binding policy in order to actually get the memory bound, if the OS
-- does not provide any other way to simply allocate bound memory
-- without changing the policy for all allocations. That is the
-- difference with hwloc_alloc_membind(), which will never change the
-- current memory binding policy.
-- 
-- \code
-- hwloc_alloc_membind_policy(topology, size, set,
--                            HWLOC_MEMBIND_BIND, 0);
-- \endcode
-- 
-- Each hwloc memory binding function is available in two forms: one
-- that takes a CPU set argument and another that takes a NUMA memory
-- node set argument (see \ref hwlocality_object_sets and \ref
-- hwlocality_bitmap for a discussion of CPU sets and NUMA memory node
-- sets).  The names of the latter form end with _nodeset.  It is also
-- possible to convert between CPU set and node set using
-- hwloc_cpuset_to_nodeset() or hwloc_cpuset_from_nodeset().
-- 
-- \sa Some example codes are available under doc/examples/ in the source tree.
-- 
-- \note On some operating systems, memory binding affects the CPU
-- binding; see ::HWLOC_MEMBIND_NOCPUBIND
-- @{
-- 

-- | Memory binding policy.
-- 
-- These constants can be used to choose the binding policy.  Only one policy can
-- be used at a time (i.e., the values cannot be OR'ed together).
-- 
-- Not all systems support all kinds of binding.
-- hwloc_topology_get_support() may be used to query about the actual memory
-- binding policy support in the currently used operating system.
-- See the "Detailed Description" section of \ref hwlocality_membinding
-- for a description of errors that can occur.
-- 
data MemBindPolicy
   = MemBindDefault     -- ^ Reset the memory allocation policy to the system default.
                        -- Depending on the operating system, this may correspond to
                        -- ::HWLOC_MEMBIND_FIRSTTOUCH (Linux),
                        -- or ::HWLOC_MEMBIND_BIND (AIX, HP-UX, Solaris, Windows).
   | MemBindFirstTouch  -- ^ Allocate memory
                        -- but do not immediately bind it to a specific locality. Instead,
                        -- each page in the allocation is bound only when it is first
                        -- touched. Pages are individually bound to the local NUMA node of
                        -- the first thread that touches it. If there is not enough memory
                        -- on the node, allocation may be done in the specified cpuset
                        -- before allocating on other nodes.
   | MemBinbBind        -- ^ Allocate memory on the specified nodes.
   
   | MemBindInterleave  -- ^ Allocate memory on the given nodes in an interleaved
                        -- / round-robin manner.  The precise layout of the memory across
                        -- multiple NUMA nodes is OS/system specific. Interleaving can be
                        -- useful when threads distributed across the specified NUMA nodes
                        -- will all be accessing the whole memory range concurrently, since
                        -- the interleave will then balance the memory references.
   
   | MemBindNextTouch   -- ^ For each page bound with this policy, by next time
                        -- it is touched (and next time only), it is moved from its current
                        -- location to the local NUMA node of the thread where the memory
                        -- reference occurred (if it needs to be moved at all).
   
   deriving (Show,Eq,Enum)


-- | Memory binding flags.
-- 
-- These flags can be used to refine the binding policy.
-- All flags can be logically OR'ed together with the exception of
-- ::HWLOC_MEMBIND_PROCESS and ::HWLOC_MEMBIND_THREAD;
-- these two flags are mutually exclusive.
-- 
-- Not all systems support all kinds of binding.
-- hwloc_topology_get_support() may be used to query about the actual memory
-- binding support in the currently used operating system.
-- See the "Detailed Description" section of \ref hwlocality_membinding
-- for a description of errors that can occur.
-- 
data MemBindFlag
   = MemBindProcess     -- ^ Set policy for all threads of the specified (possibly
                        -- multithreaded) process.  This flag is mutually exclusive with
                        -- ::HWLOC_MEMBIND_THREAD.

   | MemBindThread      -- ^ Set policy for a specific thread of the current process.
                        -- This flag is mutually exclusive with ::HWLOC_MEMBIND_PROCESS.

   | MemBindStrict      -- ^ Request strict binding from the OS.  The function will fail if
                        -- the binding can not be guaranteed / completely enforced.
                        -- 
                        -- This flag has slightly different meanings depending on which
                        -- function it is used with.
                        -- \hideinitializer  */
   
   | MemBindMigrate     -- ^ Migrate existing allocated memory.  If the memory cannot
                        -- be migrated and the ::HWLOC_MEMBIND_STRICT flag is passed, an error
                        -- will be returned.

   | MemBindNoCPuBind   -- ^ Avoid any effect on CPU binding.
                        -- 
                        -- On some operating systems, some underlying memory binding
                        -- functions also bind the application to the corresponding CPU(s).
                        -- Using this flag will cause hwloc to avoid using OS functions that
                        -- could potentially affect CPU bindings.  Note, however, that using
                        -- NOCPUBIND may reduce hwloc's overall memory binding
                        -- support. Specifically: some of hwloc's memory binding functions
                        -- may fail with errno set to ENOSYS when used with NOCPUBIND.
   deriving (Show,Eq,Enum) 


-- | Set the default memory binding policy of the current
-- process or thread to prefer the NUMA node(s) specified by physical \p nodeset
-- 
-- If neither ::HWLOC_MEMBIND_PROCESS nor ::HWLOC_MEMBIND_THREAD is
-- specified, the current process is assumed to be single-threaded.
-- This is the most portable form as it permits hwloc to use either
-- process-based OS functions or thread-based OS functions, depending
-- on which are available.
-- 
-- \return -1 with errno set to ENOSYS if the action is not supported
-- \return -1 with errno set to EXDEV if the binding cannot be enforced
-- 
setMemBindNodeSet :: Topology -> NodeSet -> MemBindPolicy -> [MemBindFlag] -> IO Int
setMemBindNodeSet topo ns pol flags = setMemBindNodeSet' topo ns (fromEnum pol) (fromFlags flags)

foreign import ccall "hwloc_set_membind_nodeset" setMemBindNodeSet' :: Topology -> NodeSet -> Int -> Word -> IO Int

-- | Set the default memory binding policy of the current
-- process or thread to prefer the NUMA node(s) near the specified physical \p
-- cpuset
-- 
-- If neither ::HWLOC_MEMBIND_PROCESS nor ::HWLOC_MEMBIND_THREAD is
-- specified, the current process is assumed to be single-threaded.
-- This is the most portable form as it permits hwloc to use either
-- process-based OS functions or thread-based OS functions, depending
-- on which are available.
-- 
-- \return -1 with errno set to ENOSYS if the action is not supported
-- \return -1 with errno set to EXDEV if the binding cannot be enforced
-- 
setMemBind :: Topology -> CpuSet -> MemBindPolicy -> [MemBindFlag] -> IO Int
setMemBind topo cs pol flags = setMemBind' topo cs (fromEnum pol) (fromFlags flags)

foreign import ccall "hwloc_set_membind" setMemBind' :: Topology -> CpuSet -> Int -> Word -> IO Int

-- | Query the default memory binding policy and physical locality of the
-- current process or thread.
-- 
-- This function has two output parameters: \p nodeset and \p policy.
-- The values returned in these parameters depend on both the \p flags
-- passed in and the current memory binding policies and nodesets in
-- the queried target.
-- 
-- Passing the ::HWLOC_MEMBIND_PROCESS flag specifies that the query
-- target is the current policies and nodesets for all the threads in
-- the current process.  Passing ::HWLOC_MEMBIND_THREAD specifies that
-- the query target is the current policy and nodeset for only the
-- thread invoking this function.
-- 
-- If neither of these flags are passed (which is the most portable
-- method), the process is assumed to be single threaded.  This allows
-- hwloc to use either process-based OS functions or thread-based OS
-- functions, depending on which are available.
-- 
-- ::HWLOC_MEMBIND_STRICT is only meaningful when ::HWLOC_MEMBIND_PROCESS
-- is also specified.  In this case, hwloc will check the default
-- memory policies and nodesets for all threads in the process.  If
-- they are not identical, -1 is returned and errno is set to EXDEV.
-- If they are identical, the values are returned in \p nodeset and \p
-- policy.
-- 
-- Otherwise, if ::HWLOC_MEMBIND_PROCESS is specified (and
-- ::HWLOC_MEMBIND_STRICT is \em not specified), \p nodeset is set to
-- the logical OR of all threads' default nodeset.  If all threads'
-- default policies are the same, \p policy is set to that policy.  If
-- they are different, \p policy is set to ::HWLOC_MEMBIND_MIXED.
-- 
-- In the ::HWLOC_MEMBIND_THREAD case (or when neither
-- ::HWLOC_MEMBIND_PROCESS or ::HWLOC_MEMBIND_THREAD is specified), there
-- is only one nodeset and policy; they are returned in \p nodeset and
-- \p policy, respectively.
-- 
-- If any other flags are specified, -1 is returned and errno is set
-- to EINVAL.
-- 
getMemBindNodeSet :: Topology -> NodeSet -> [MemBindFlag] -> IO (Int,MemBindPolicy)
getMemBindNodeSet topo ns flags = alloca $ \ptr -> do
   r   <- getMemBindNodeSet' topo ns ptr (fromFlags flags)
   pol <- toEnum <$> peek ptr
   return (r,pol)

foreign import ccall "hwloc_get_membind_nodeset" getMemBindNodeSet' :: Topology -> NodeSet -> Ptr Int -> Word -> IO Int

-- | Query the default memory binding policy and physical locality of the
-- current process or thread (the locality is returned in \p cpuset as
-- CPUs near the locality's actual NUMA node(s)).
-- 
-- This function has two output parameters: \p cpuset and \p policy.
-- The values returned in these parameters depend on both the \p flags
-- passed in and the current memory binding policies and nodesets in
-- the queried target.
-- 
-- Passing the ::HWLOC_MEMBIND_PROCESS flag specifies that the query
-- target is the current policies and nodesets for all the threads in
-- the current process.  Passing ::HWLOC_MEMBIND_THREAD specifies that
-- the query target is the current policy and nodeset for only the
-- thread invoking this function.
-- 
-- If neither of these flags are passed (which is the most portable
-- method), the process is assumed to be single threaded.  This allows
-- hwloc to use either process-based OS functions or thread-based OS
-- functions, depending on which are available.
-- 
-- ::HWLOC_MEMBIND_STRICT is only meaningful when ::HWLOC_MEMBIND_PROCESS
-- is also specified.  In this case, hwloc will check the default
-- memory policies and nodesets for all threads in the process.  If
-- they are not identical, -1 is returned and errno is set to EXDEV.
-- If they are identical, the policy is returned in \p policy.  \p
-- cpuset is set to the union of CPUs near the NUMA node(s) in the
-- nodeset.
-- 
-- Otherwise, if ::HWLOC_MEMBIND_PROCESS is specified (and
-- ::HWLOC_MEMBIND_STRICT is \em not specified), the default nodeset
-- from each thread is logically OR'ed together.  \p cpuset is set to
-- the union of CPUs near the NUMA node(s) in the resulting nodeset.
-- If all threads' default policies are the same, \p policy is set to
-- that policy.  If they are different, \p policy is set to
-- ::HWLOC_MEMBIND_MIXED.
-- 
-- In the ::HWLOC_MEMBIND_THREAD case (or when neither
-- ::HWLOC_MEMBIND_PROCESS or ::HWLOC_MEMBIND_THREAD is specified), there
-- is only one nodeset and policy.  The policy is returned in \p
-- policy; \p cpuset is set to the union of CPUs near the NUMA node(s)
-- in the \p nodeset.
-- 
-- If any other flags are specified, -1 is returned and errno is set
-- to EINVAL.
-- 
getMemBind :: Topology -> CpuSet -> [MemBindFlag] -> IO (Int,MemBindPolicy)
getMemBind topo cs flags = alloca $ \ptr -> do
   r   <- getMemBind' topo cs ptr (fromFlags flags)
   pol <- toEnum <$> peek ptr
   return (r,pol)

foreign import ccall "hwloc_get_membind" getMemBind' :: Topology -> CpuSet -> Ptr Int -> Word -> IO Int

-- | Set the default memory binding policy of the specified
-- process to prefer the NUMA node(s) specified by physical \p nodeset
-- 
-- \return -1 with errno set to ENOSYS if the action is not supported
-- \return -1 with errno set to EXDEV if the binding cannot be enforced
-- 
-- \note \p hwloc_pid_t is \p pid_t on Unix platforms,
-- and \p HANDLE on native Windows platforms.
-- 
setProcMemBindNodeSet :: Topology -> Word -> NodeSet -> MemBindPolicy -> [MemBindFlag] -> IO Int
setProcMemBindNodeSet topo pid ns pol flags = setProcMemBindNodeSet' topo pid ns (fromEnum pol) (fromFlags flags)

foreign import ccall "hwloc_set_proc_membind_nodeset" setProcMemBindNodeSet' :: Topology -> Word -> NodeSet -> Int -> Word -> IO Int

-- | Set the default memory binding policy of the specified
-- process to prefer the NUMA node(s) near the specified physical \p cpuset
-- 
-- \return -1 with errno set to ENOSYS if the action is not supported
-- \return -1 with errno set to EXDEV if the binding cannot be enforced
-- 
-- \note \p hwloc_pid_t is \p pid_t on Unix platforms,
-- and \p HANDLE on native Windows platforms.
-- 
setProcMemBind :: Topology -> Word -> CpuSet -> MemBindPolicy -> [MemBindFlag] -> IO Int
setProcMemBind topo pid cs pol flags = setProcMemBind' topo pid cs (fromEnum pol) (fromFlags flags)

foreign import ccall "hwloc_set_proc_membind" setProcMemBind' :: Topology -> Word -> CpuSet -> Int -> Word -> IO Int

-- | Query the default memory binding policy and physical locality of the
-- specified process.
-- 
-- This function has two output parameters: \p nodeset and \p policy.
-- The values returned in these parameters depend on both the \p flags
-- passed in and the current memory binding policies and nodesets in
-- the queried target.
-- 
-- Passing the ::HWLOC_MEMBIND_PROCESS flag specifies that the query
-- target is the current policies and nodesets for all the threads in
-- the specified process.  If ::HWLOC_MEMBIND_PROCESS is not specified
-- (which is the most portable method), the process is assumed to be
-- single threaded.  This allows hwloc to use either process-based OS
-- functions or thread-based OS functions, depending on which are
-- available.
-- 
-- Note that it does not make sense to pass ::HWLOC_MEMBIND_THREAD to
-- this function.
-- 
-- If ::HWLOC_MEMBIND_STRICT is specified, hwloc will check the default
-- memory policies and nodesets for all threads in the specified
-- process.  If they are not identical, -1 is returned and errno is
-- set to EXDEV.  If they are identical, the values are returned in \p
-- nodeset and \p policy.
-- 
-- Otherwise, \p nodeset is set to the logical OR of all threads'
-- default nodeset.  If all threads' default policies are the same, \p
-- policy is set to that policy.  If they are different, \p policy is
-- set to ::HWLOC_MEMBIND_MIXED.
-- 
-- If any other flags are specified, -1 is returned and errno is set
-- to EINVAL.
-- 
-- \note \p hwloc_pid_t is \p pid_t on Unix platforms,
-- and \p HANDLE on native Windows platforms.
-- 
getProcMemBindNodeSet :: Topology -> Word -> NodeSet -> [MemBindFlag] -> IO (Int,MemBindPolicy)
getProcMemBindNodeSet topo pid ns flags = alloca $ \ptr -> do
   r   <- getProcMemBindNodeSet' topo pid ns ptr (fromFlags flags)
   pol <- toEnum <$> peek ptr
   return (r,pol)

foreign import ccall "hwloc_get_proc_membind_nodeset" getProcMemBindNodeSet' :: Topology -> Word -> NodeSet -> Ptr Int -> Word -> IO Int

-- | Query the default memory binding policy and physical locality of the
-- specified process (the locality is returned in \p cpuset as CPUs
-- near the locality's actual NUMA node(s)).
-- 
-- This function has two output parameters: \p cpuset and \p policy.
-- The values returned in these parameters depend on both the \p flags
-- passed in and the current memory binding policies and nodesets in
-- the queried target.
-- 
-- Passing the ::HWLOC_MEMBIND_PROCESS flag specifies that the query
-- target is the current policies and nodesets for all the threads in
-- the specified process.  If ::HWLOC_MEMBIND_PROCESS is not specified
-- (which is the most portable method), the process is assumed to be
-- single threaded.  This allows hwloc to use either process-based OS
-- functions or thread-based OS functions, depending on which are
-- available.
-- 
-- Note that it does not make sense to pass ::HWLOC_MEMBIND_THREAD to
-- this function.
-- 
-- If ::HWLOC_MEMBIND_STRICT is specified, hwloc will check the default
-- memory policies and nodesets for all threads in the specified
-- process.  If they are not identical, -1 is returned and errno is
-- set to EXDEV.  If they are identical, the policy is returned in \p
-- policy.  \p cpuset is set to the union of CPUs near the NUMA
-- node(s) in the nodeset.
-- 
-- Otherwise, the default nodeset from each thread is logically OR'ed
-- together.  \p cpuset is set to the union of CPUs near the NUMA
-- node(s) in the resulting nodeset.  If all threads' default policies
-- are the same, \p policy is set to that policy.  If they are
-- different, \p policy is set to ::HWLOC_MEMBIND_MIXED.
-- 
-- If any other flags are specified, -1 is returned and errno is set
-- to EINVAL.
-- 
-- \note \p hwloc_pid_t is \p pid_t on Unix platforms,
-- and \p HANDLE on native Windows platforms.
-- 
getProcMemBind :: Topology -> Word -> CpuSet -> [MemBindFlag] -> IO (Int,MemBindPolicy)
getProcMemBind topo pid cs flags = alloca $ \ptr -> do
   r   <- getProcMemBind' topo pid cs ptr (fromFlags flags)
   pol <- toEnum <$> peek ptr
   return (r,pol)

foreign import ccall "hwloc_get_proc_membind" getProcMemBind' :: Topology -> Word -> CpuSet -> Ptr Int -> Word -> IO Int

-- | Bind the already-allocated memory identified by (addr, len)
-- to the NUMA node(s) in physical \p nodeset.
-- 
-- \return -1 with errno set to ENOSYS if the action is not supported
-- \return -1 with errno set to EXDEV if the binding cannot be enforced
-- 
setAreaMemBindNodeSet :: Topology -> Ptr a -> CSize -> NodeSet -> MemBindPolicy -> [MemBindFlag] -> IO Int
setAreaMemBindNodeSet topo addr len nodeset policy flags = setAreaMemBindNodeSet' topo addr len nodeset (fromEnum policy) (fromFlags flags)

foreign import ccall "hwloc_set_area_membind_nodeset" setAreaMemBindNodeSet' :: Topology -> Ptr a -> CSize -> NodeSet -> Int -> Word -> IO Int

-- | Bind the already-allocated memory identified by (addr, len)
-- to the NUMA node(s) near physical \p cpuset.
-- 
-- \return -1 with errno set to ENOSYS if the action is not supported
-- \return -1 with errno set to EXDEV if the binding cannot be enforced
-- 
setAreaMemBind :: Topology -> Ptr a -> CSize -> CpuSet -> MemBindPolicy -> [MemBindFlag] -> IO Int
setAreaMemBind topo addr len cpuset policy flags = setAreaMemBind' topo addr len cpuset (fromEnum policy) (fromFlags flags)

foreign import ccall "hwloc_set_area_membind" setAreaMemBind' :: Topology -> Ptr a -> CSize -> CpuSet -> Int -> Word -> IO Int

-- | Query the physical NUMA node(s) and binding policy of the memory
-- identified by (\p addr, \p len ).
-- 
-- This function has two output parameters: \p nodeset and \p policy.
-- The values returned in these parameters depend on both the \p flags
-- passed in and the memory binding policies and nodesets of the pages
-- in the address range.
-- 
-- If ::HWLOC_MEMBIND_STRICT is specified, the target pages are first
-- checked to see if they all have the same memory binding policy and
-- nodeset.  If they do not, -1 is returned and errno is set to EXDEV.
-- If they are identical across all pages, the nodeset and policy are
-- returned in \p nodeset and \p policy, respectively.
-- 
-- If ::HWLOC_MEMBIND_STRICT is not specified, \p nodeset is set to the
-- union of all NUMA node(s) containing pages in the address range.
-- If all pages in the target have the same policy, it is returned in
-- \p policy.  Otherwise, \p policy is set to ::HWLOC_MEMBIND_MIXED.
-- 
-- If any other flags are specified, -1 is returned and errno is set
-- to EINVAL.
-- 
getAreaMemBindNodeSet :: Topology -> Ptr a -> CSize -> NodeSet -> [MemBindFlag] -> IO (Int,MemBindPolicy)
getAreaMemBindNodeSet topo addr len nodeset flags = alloca $ \ptr -> do
   r   <- getAreaMemBindNodeSet' topo addr len nodeset ptr (fromFlags flags)
   pol <- toEnum <$> peek ptr
   return (r,pol)

foreign import ccall "hwloc_get_area_membind_nodeset" getAreaMemBindNodeSet' :: Topology -> Ptr a -> CSize -> NodeSet -> Ptr Int -> Word -> IO Int


-- | Query the CPUs near the physical NUMA node(s) and binding policy of
-- the memory identified by (\p addr, \p len ).
-- 
-- This function has two output parameters: \p cpuset and \p policy.
-- The values returned in these parameters depend on both the \p flags
-- passed in and the memory binding policies and nodesets of the pages
-- in the address range.
-- 
-- If ::HWLOC_MEMBIND_STRICT is specified, the target pages are first
-- checked to see if they all have the same memory binding policy and
-- nodeset.  If they do not, -1 is returned and errno is set to EXDEV.
-- If they are identical across all pages, the policy is returned in
-- \p policy.  \p cpuset is set to the union of CPUs near the NUMA
-- node(s) in the nodeset.
-- 
-- If ::HWLOC_MEMBIND_STRICT is not specified, the union of all NUMA
-- node(s) containing pages in the address range is calculated.  \p
-- cpuset is then set to the CPUs near the NUMA node(s) in this union.
-- If all pages in the target have the same policy, it is returned in
-- \p policy.  Otherwise, \p policy is set to ::HWLOC_MEMBIND_MIXED.
-- 
-- If any other flags are specified, -1 is returned and errno is set
-- to EINVAL.
-- 
getAreaMemBind :: Topology -> Ptr a -> CSize -> CpuSet -> [MemBindFlag] -> IO (Int,MemBindPolicy)
getAreaMemBind topo addr len cpuset flags = alloca $ \ptr -> do
   r   <- getAreaMemBind' topo addr len cpuset ptr (fromFlags flags)
   pol <- toEnum <$> peek ptr
   return (r,pol)

foreign import ccall "hwloc_get_area_membind" getAreaMemBind' :: Topology -> Ptr a -> CSize -> CpuSet -> Ptr Int -> Word -> IO Int

-- | Allocate some memory
-- 
-- This is equivalent to malloc(), except that it tries to allocate
-- page-aligned memory from the OS.
-- 
-- \note The allocated memory should be freed with hwloc_free().
-- 
foreign import ccall "hwloc_alloc" alloc :: Topology -> CSize -> IO (Ptr a)

-- | Allocate some memory on the given physical nodeset \p nodeset
-- 
-- \return NULL with errno set to ENOSYS if the action is not supported
-- and ::HWLOC_MEMBIND_STRICT is given
-- \return NULL with errno set to EXDEV if the binding cannot be enforced
-- and ::HWLOC_MEMBIND_STRICT is given
-- \return NULL with errno set to ENOMEM if the memory allocation failed
-- even before trying to bind.
-- 
-- \note The allocated memory should be freed with hwloc_free().
-- 
allocMemBindNodeSet :: Topology -> CSize -> NodeSet -> MemBindPolicy -> [MemBindFlag] -> IO (Ptr a)
allocMemBindNodeSet topo len nodeset pol flags = allocMemBindNodeSet' topo len nodeset (fromEnum pol) (fromFlags flags)

foreign import ccall "hwloc_alloc_membind_nodeset" allocMemBindNodeSet' :: Topology -> CSize -> NodeSet -> Int -> Word -> IO (Ptr a)

-- | Allocate some memory on memory nodes near the given physical cpuset \p cpuset
-- 
-- \return NULL with errno set to ENOSYS if the action is not supported
-- and ::HWLOC_MEMBIND_STRICT is given
-- \return NULL with errno set to EXDEV if the binding cannot be enforced
-- and ::HWLOC_MEMBIND_STRICT is given
-- \return NULL with errno set to ENOMEM if the memory allocation failed
-- even before trying to bind.
-- 
-- \note The allocated memory should be freed with hwloc_free().
-- 
allocMemBind :: Topology -> CSize -> CpuSet -> MemBindPolicy -> [MemBindFlag] -> IO (Ptr a)
allocMemBind topo len cpuset pol flags = allocMemBind' topo len cpuset (fromEnum pol) (fromFlags flags)

foreign import ccall "hwloc_alloc_membind" allocMemBind' :: Topology -> CSize -> CpuSet -> Int -> Word -> IO (Ptr a)

-- | Free memory that was previously allocated by hwloc_alloc()
-- or hwloc_alloc_membind().
-- 
foreign import ccall "hwloc_free" free :: Topology -> Ptr a -> CSize -> IO Int


--  Changing the Source of Topology Discovery
-- 
-- If none of the functions below is called, the default is to detect all the objects
-- of the machine that the caller is allowed to access.
-- 
-- This default behavior may also be modified through environment variables
-- if the application did not modify it already.
-- Setting HWLOC_XMLFILE in the environment enforces the discovery from a XML
-- file as if hwloc_topology_set_xml() had been called.
-- Setting HWLOC_SYNTHETIC enforces a synthetic topology as if
-- hwloc_topology_set_synthetic() had been called.
-- Setting HWLOC_FSROOT switches to reading the topology from the specified Linux
-- filesystem root.
-- 
-- Finally, HWLOC_THISSYSTEM enforces the return value of
-- hwloc_topology_is_thissystem().
-- 

-- | Change which process the topology is viewed from.
-- 
-- On some systems, processes may have different views of the machine, for
-- instance the set of allowed CPUs. By default, hwloc exposes the view from
-- the current process. Calling hwloc_topology_set_pid() permits to make it
-- expose the topology of the machine from the point of view of another
-- process.
-- 
-- \note \p hwloc_pid_t is \p pid_t on Unix platforms,
-- and \p HANDLE on native Windows platforms.
-- 
-- \note -1 is returned and errno is set to ENOSYS on platforms that do not
-- support this feature.
-- 
foreign import ccall "hwloc_topology_set_pid" setPid :: Topology -> Word -> IO Int

-- | Enable synthetic topology.
-- 
-- Gather topology information from the given \p description,
-- a space-separated string of <type:number> describing
-- the object type and arity at each level.
-- All types may be omitted (space-separated string of numbers) so that
-- hwloc chooses all types according to usual topologies.
-- See also the \ref synthetic.
-- 
-- Setting the environment variable HWLOC_SYNTHETIC
-- may also result in this behavior.
-- 
-- If \p description was properly parsed and describes a valid topology
-- configuration, this function returns 0.
-- Otherwise -1 is returned and errno is set to EINVAL.
-- 
-- Note that this function does not actually load topology
-- information; it just tells hwloc where to load it from.  You'll
-- still need to invoke hwloc_topology_load() to actually load the
-- topology information.
-- 
-- \note For convenience, this backend provides empty binding hooks which just
-- return success.
-- 
-- \note On success, the synthetic component replaces the previously enabled
-- component (if any), but the topology is not actually modified until
-- hwloc_topology_load().
-- 
foreign import ccall "hwloc_topology_set_synthetic" setSynthetic :: Topology -> CString -> IO Int

-- | Enable XML-file based topology.
-- 
-- Gather topology information from the XML file given at \p xmlpath.
-- Setting the environment variable HWLOC_XMLFILE may also result in this behavior.
-- This file may have been generated earlier with hwloc_topology_export_xml() in hwloc/export.h,
-- or lstopo file.xml.
-- 
-- Note that this function does not actually load topology
-- information; it just tells hwloc where to load it from.  You'll
-- still need to invoke hwloc_topology_load() to actually load the
-- topology information.
-- 
-- \return -1 with errno set to EINVAL on failure to read the XML file.
-- 
-- \note See also hwloc_topology_set_userdata_import_callback()
-- for importing application-specific object userdata.
-- 
-- \note For convenience, this backend provides empty binding hooks which just
-- return success.  To have hwloc still actually call OS-specific hooks, the
-- ::HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM has to be set to assert that the loaded
-- file is really the underlying system.
-- 
-- \note On success, the XML component replaces the previously enabled
-- component (if any), but the topology is not actually modified until
-- hwloc_topology_load().
-- 
foreign import ccall "hwloc_topology_set_xml" setXml :: Topology -> CString -> IO Int

-- | Enable XML based topology using a memory buffer (instead of
-- a file, as with hwloc_topology_set_xml()).
-- 
-- Gather topology information from the XML memory buffer given at \p
-- buffer and of length \p size.  This buffer may have been filled
-- earlier with hwloc_topology_export_xmlbuffer() in hwloc/export.h.
-- 
-- Note that this function does not actually load topology
-- information; it just tells hwloc where to load it from.  You'll
-- still need to invoke hwloc_topology_load() to actually load the
-- topology information.
-- 
-- \return -1 with errno set to EINVAL on failure to read the XML buffer.
-- 
-- \note See also hwloc_topology_set_userdata_import_callback()
-- for importing application-specific object userdata.
-- 
-- \note For convenience, this backend provides empty binding hooks which just
-- return success.  To have hwloc still actually call OS-specific hooks, the
-- ::HWLOC_TOPOLOGY_FLAG_IS_THISSYSTEM has to be set to assert that the loaded
-- file is really the underlying system.
-- 
-- \note On success, the XML component replaces the previously enabled
-- component (if any), but the topology is not actually modified until
-- hwloc_topology_load().
-- 
foreign import ccall "hwloc_topology_set_xmlbuffer" setXmlBuffer :: Topology -> CString -> Int -> IO Int



--  Topology Detection Configuration and Query
-- 
-- Several functions can optionally be called between hwloc_topology_init() and
-- hwloc_topology_load() to configure how the detection should be performed,
-- e.g. to ignore some objects types, define a synthetic topology, etc.
-- 

-- | Flags to be set onto a topology context before load.
-- 
-- Flags should be given to hwloc_topology_set_flags().
-- They may also be returned by hwloc_topology_get_flags().
-- 
data TopologyFlag
   = TopologyFlagWholeSystem     -- ^ Detect the whole system, ignore reservations.
                                 -- 
                                 -- Gather all resources, even if some were disabled by the administrator.
                                 -- For instance, ignore Linux Cgroup/Cpusets and gather all processors and memory nodes.
                                 -- 
                                 -- When this flag is not set, PUs that are disallowed are not added to the topology.
                                 -- Parent objects (package, core, cache, etc.) are added only if some of their children are allowed.
                                 -- NUMA nodes are always added but their available memory is set to 0 when disallowed.
                                 -- 
                                 -- When this flag is set, each object has allowed_cpuset <= cpuset <= complete_cpuset.
                                 -- Otherwise allowed_cpuset = cpuset <= complete_cpuset.
                                 -- The same applies to nodesets.

   | TopologyFlagIsThisSystem    -- ^ Assume that the selected backend provides the topology for the
                                 -- system on which we are running.
                                 -- 
                                 -- This forces hwloc_topology_is_thissystem() to return 1, i.e. makes hwloc assume that
                                 -- the selected backend provides the topology for the system on which we are running,
                                 -- even if it is not the OS-specific backend but the XML backend for instance.
                                 -- This means making the binding functions actually call the OS-specific
                                 -- system calls and really do binding, while the XML backend would otherwise
                                 -- provide empty hooks just returning success.
                                 -- 
                                 -- Setting the environment variable HWLOC_THISSYSTEM may also result in the
                                 -- same behavior.
                                 -- 
                                 -- This can be used for efficiency reasons to first detect the topology once,
                                 -- save it to an XML file, and quickly reload it later through the XML
                                 -- backend, but still having binding functions actually do bind.

   | TopologyFlagIoDevices       -- ^ Detect PCI devices.
                                 -- 
                                 -- By default, I/O devices are ignored. This flag enables I/O device
                                 -- detection using the pci backend. Only the common PCI devices (GPUs,
                                 -- NICs, block devices, ...) and host bridges (objects that connect the host
                                 -- objects to an I/O subsystem) will be added to the topology.
                                 -- Uncommon devices and other bridges (such as PCI-to-PCI bridges) will be
                                 -- ignored.
   
   | TopologyFlagIoBridges       -- ^ Detect PCI bridges.
                                 -- 
                                 -- This flag should be combined with ::HWLOC_TOPOLOGY_FLAG_IO_DEVICES to enable
                                 -- the detection of both common devices and of all useful bridges (bridges that
                                 -- have at least one device behind them).

   | TopologyFlagWholeIO         -- ^ Detect the whole PCI hierarchy.
                                 -- 
                                 -- This flag enables detection of all I/O devices (even the uncommon ones
                                 -- such as DMA channels) and bridges (even those that have no device behind
                                 -- them) using the pci backend.
                                 -- Additionally it also enables MemoryModule misc objects.
                                 -- This implies ::HWLOC_TOPOLOGY_FLAG_IO_DEVICES.

   | TopologyFlagICaches         -- ^ Detect instruction caches.
                                 -- 
                                 -- This flag enables detection of Instruction caches,
                                 -- instead of only Data and Unified caches.
   deriving (Show,Eq,Enum)

-- | Set OR'ed flags to non-yet-loaded topology.
-- 
-- Set a OR'ed set of ::hwloc_topology_flags_e onto a topology that was not yet loaded.
-- 
-- If this function is called multiple times, the last invokation will erase
-- and replace the set of flags that was previously set.
-- 
-- The flags set in a topology may be retrieved with hwloc_topology_get_flags()
-- 
setFlags :: Topology -> [TopologyFlag] -> IO Int
setFlags topo flags = setFlags' topo (fromFlags flags)

foreign import ccall "hwloc_topology_set_flags" setFlags' :: Topology -> Word -> IO Int


-- | Get OR'ed flags of a topology.
-- 
-- Get the OR'ed set of ::hwloc_topology_flags_e of a topology.
-- 
-- \return the flags previously set with hwloc_topology_set_flags().
-- 
foreign import ccall "hwloc_topology_get_flags" getFlags :: Topology -> IO Word

-- | Does the topology context come from this system?
-- 
-- \return 1 if this topology context was built using the system
-- running this program.
-- \return 0 instead (for instance if using another file-system root,
-- a XML topology file, or a synthetic topology).
-- 
foreign import ccall "hwloc_topology_is_thissystem" isThisSystem :: Topology -> IO Int

-- | Flags describing actual discovery support for this topology. */
data DiscoverySupport = DiscoverySupport
   { discoverySupportPU :: Word8    -- ^ Detecting the number of PU objects is supported
   }
   deriving (Show)

-- | Flags describing actual PU binding support for this topology. */
data CpuBindSupport = CpuBindSupport
   { cpuBindSupportSetCurrentProc   :: Word8    -- ^ Binding the whole current process is supported
   , cpuBindSupportGetCurrentProc   :: Word8    -- ^ Getting the binding of the whole current process is supported
   , cpuBindSupportSetProc          :: Word8    -- ^ Binding a whole given process is supported
   , cpuBindSupportGetProc          :: Word8    -- ^ Getting the binding of a whole given process is supported
   , cpuBindSupportSetCurrentThread :: Word8    -- ^ Binding the current thread only is supported
   , cpuBindSupportGetCurrentThread :: Word8    -- ^ Getting the binding of the current thread only is supported
   , cpuBindSupportSetThread        :: Word8    -- ^ Binding a given thread only is supported
   , cpuBindSupportGetThread        :: Word8    -- ^ Getting the binding of a given thread only is supported
   , cpuBindSupportGetCurrentProcLastLocation  :: Word8    -- ^ Getting the last processors where the whole current process ran is supported
   , cpuBindSupportGetProcLastLocation :: Word8  -- ^ Getting the last processors where a whole process ran is supported
   , cpuBindSupportGetThreadLastLocation :: Word8 -- ^ Getting the last processors where the current thread ran is supported
   }
   deriving (Show)

-- | Flags describing actual memory binding support for this topology
data MemBindSupport = MemBindSupport
   { memBindSupportSetCurrentProc   :: Word8    -- ^ Binding the whole current process is supported
   , memBindSupportGetCurrentProc   :: Word8    -- ^ Getting the binding of the whole current process is supported
   , memBindSupportSetProc          :: Word8    -- ^ Binding a whole given process is supported
   , memBindSupportGetProc          :: Word8    -- ^ Getting the binding of a whole given process is supported
   , memBindSupportSetThread        :: Word8    -- ^ Binding the current thread only is supported
   , memBindSupportGetThread        :: Word8    -- ^ Getting the binding of the current thread only is supported
   , memBindSupportSetArea          :: Word8    -- ^ Binding a given memory area is supported
   , memBindSupportGetArea          :: Word8    -- ^ Getting the binding of a given memory area is supported
   , memBindSupportAlloc            :: Word8    -- ^ Allocating a bound memory area is supported
   , memBindSupportFirstTouch       :: Word8    -- ^ First-touch policy is supported
   , memBindSupportBind             :: Word8    -- ^ Bind policy is supported
   , memBindSupportInterleave       :: Word8    -- ^ Interleave policy is supported
   , memBindSupportNextTouch        :: Word8    -- ^ Next-touch migration policy is supported
   , memBindSupportMigrate          :: Word8    -- ^ Migration flag is supported
   }
   deriving (Show)

-- | Set of flags describing actual support for this topology.
-- 
-- This is retrieved with hwloc_topology_get_support() and will be valid until
-- the topology object is destroyed.  Note: the values are correct only after
-- discovery.
-- 
data TopologySupport = TopologySupport
   { topologySupportDiscovery :: DiscoverySupport
   , topologySupportCpuBund   :: CpuBindSupport
   , topologySupportMemBind   :: MemBindSupport
   }
   deriving (Show)

-- | Retrieve the topology support
foreign import ccall "hwloc_topology_get_support" getSupport :: Topology -> IO (Ptr TopologySupport)

-- | Ignore an object type.
-- 
-- Ignore all objects from the given type.
-- The bottom-level type ::HWLOC_OBJ_PU and the ::HWLOC_OBJ_NUMANODE level may not be ignored.
-- The top-level object of the hierarchy will never be ignored, even if this function
-- succeeds.
-- I/O objects may not be ignored, topology flags should be used to configure
-- their discovery instead.
-- 
ignoreType :: Topology -> ObjectType -> IO Int
ignoreType topo typ = ignoreType' topo (fromEnum typ)

foreign import ccall "hwloc_topology_ignore_type" ignoreType' :: Topology -> Int -> IO Int


-- | Ignore an object type if it does not bring any structure.
-- 
-- Ignore a level from the given type as long as it does not bring any structure:
-- Each object of the level should have a single child or be the only child of its parent.
-- The bottom-level type ::HWLOC_OBJ_PU and the ::HWLOC_OBJ_NUMANODE level may not be ignored.
-- I/O objects may not be ignored, topology flags should be used to configure
-- their discovery instead.
-- Group levels are always ignored if they do not bring any structure
-- since they are designed to add structure to the topology.
-- Misc objects cannot be ignored based on the structure since they are only annotations
-- outside of the main topology structure.
-- 
foreign import ccall "hwloc_topology_ignore_type_keep_structure" ignoreTypeKeepStructure' :: Topology -> Int -> IO Int

ignoreTypeKeepStructure :: Topology -> ObjectType -> IO Int
ignoreTypeKeepStructure topo typ = ignoreTypeKeepStructure' topo (fromEnum typ)

-- | Ignore all objects that do not bring any structure.
-- 
-- Ignore all levels that do not bring any structure:
-- This is equivalent to calling hwloc_topology_ignore_type_keep_structure()
-- for all object types.
-- 
foreign import ccall "hwloc_topology_ignore_all_keep_structure" ignoreAllKeepStructure :: Topology -> IO Int

-- | Provide a distance matrix.
-- 
-- Provide the matrix of distances between a set of objects of the given type.
-- The set may or may not contain all the existing objects of this type.
-- The objects are specified by their OS/physical index in the \p os_index
-- array. The \p distances matrix follows the same order.
-- The distance from object i to object j in the i*nbobjs+j.
-- 
-- A single latency matrix may be defined for each type.
-- If another distance matrix already exists for the given type,
-- either because the user specified it or because the OS offers it,
-- it will be replaced by the given one.
-- If \p nbobjs is \c 0, \p os_index is \c NULL and \p distances is \c NULL,
-- the existing distance matrix for the given type is removed.
-- 
-- \note Distance matrices are ignored in multi-node topologies.
-- 
foreign import ccall "hwloc_topology_set_distance_matrix" setDistanceMatrix' :: Topology -> Int -> Word -> Ptr Word -> Ptr Float -> IO Int

setDistanceMatrix :: Topology -> ObjectType -> Word -> Ptr Word -> Ptr Float -> IO Int
setDistanceMatrix topo typ n k v = setDistanceMatrix' topo (fromEnum typ) n k v

-- | Set the topology-specific userdata pointer.
-- 
-- Each topology may store one application-given private data pointer.
-- It is initialized to \c NULL.
-- hwloc will never modify it.
-- 
-- Use it as you wish, after hwloc_topology_init() and until hwloc_topolog_destroy().
-- 
-- This pointer is not exported to XML.
-- 
foreign import ccall "hwloc_topology_set_userdata" setUserData :: Topology -> Ptr a -> IO ()

-- | Retrieve the topology-specific userdata pointer.
-- 
-- Retrieve the application-given private data pointer that was
-- previously set with hwloc_topology_set_userdata().
-- 
foreign import ccall "hwloc_topology_get_userdata" getUserData :: Topology -> IO (Ptr a)



-- | Flags to be given to hwloc_topology_restrict()
data RestrictFlag
   = RestrictFlagAdapDistances      -- ^ Adapt distance matrices according to objects being removed during restriction.
                                    -- If this flag is not set, distance matrices are removed.

   | RestrictFlagAdaptMisc          -- ^ Move Misc objects to ancestors if their parents are removed during restriction.
                                    -- If this flag is not set, Misc objects are removed when their parents are removed.

   | RestrictFlagAdaptIO            -- ^ Move I/O objects to ancestors if their parents are removed during restriction.
                                    -- If this flag is not set, I/O devices and bridges are removed when their parents are removed.
   deriving (Show,Eq,Enum)

-- | Restrict the topology to the given CPU set.
-- 
-- Topology \p topology is modified so as to remove all objects that
-- are not included (or partially included) in the CPU set \p cpuset.
-- All objects CPU and node sets are restricted accordingly.
-- 
-- \p flags is a OR'ed set of ::hwloc_restrict_flags_e.
-- 
-- \note This call may not be reverted by restricting back to a larger
-- cpuset. Once dropped during restriction, objects may not be brought
-- back, except by loading another topology with hwloc_topology_load().
-- 
-- \return 0 on success.
-- 
-- \return -1 with errno set to EINVAL if the input cpuset is invalid.
-- The topology is not modified in this case.
-- 
-- \return -1 with errno set to ENOMEM on failure to allocate internal data.
-- The topology is reinitialized in this case. It should be either
-- destroyed with hwloc_topology_destroy() or configured and loaded again.
-- 
foreign import ccall "hwloc_topology_restrict" restrict :: Topology -> CpuSet -> Word -> IO (Ptr a)

-- | Add a MISC object as a leaf of the topology
-- 
-- A new MISC object will be created and inserted into the topology at the
-- position given by parent. It is appended to the list of existing Misc children,
-- without ever adding any intermediate hierarchy level. This is useful for
-- annotating the topology without actually changing the hierarchy.
-- 
-- \p name will be copied to the setup the new object attributes.
-- However, the new leaf object will not have any \p cpuset.
-- 
-- \return the newly-created object
-- 
-- \note If \p name contains some non-printable characters, they will
-- be dropped when exporting to XML, see hwloc_topology_export_xml() in hwloc/export.h.
-- 
--foreign import ccall "hwloc_topology_insert_misc_object" insertMiscObject :: Topology -> Ptr Object -> CString -> IO (Ptr Object)

-- | Allocate a Group object to insert later with hwloc_topology_insert_group_object().
-- 
-- This function returns a new Group object.
-- The caller should (at least) initialize its sets before inserting the object.
-- See hwloc_topology_insert_group_object().
-- 
-- Custom name/value info pairs may be added with hwloc_obj_add_info() after
-- insertion. For instance the Type info key allows to display something else
-- than "Group" as the type name for this object in lstopo.
-- 
-- It is recommended not to set any other object attribute before insertion,
-- since the Group may get discarded during insertion.
-- 
-- The object will be destroyed if passed to hwloc_topology_insert_group_object()
-- without any set defined.
-- 
--foreign import ccall "hwloc_topology_alloc_group_object" allocGroupObject :: Topology -> IO (Ptr Object)

-- | Add more structure to the topology by adding an intermediate Group
-- 
-- The caller should first allocate a new Group object with hwloc_topology_alloc_group_object().
-- Then it must initialize some of its sets to specify the final location of the Group
-- in the topology.
-- Then the object can be passed to this function for actual insertion in the topology.
-- 
-- Either the cpuset or nodeset field (or both, if compatible) may be used to do so.
-- If inserting with respect to the complete topology (including disallowed, offline
-- or unknown object), complete_cpuset and/or complete_nodeset may be used instead.
-- It grouping several objects, hwloc_obj_add_other_obj_sets() is an easy way to
-- build the Group sets iteratively.
-- 
-- \return The inserted object if it was properly inserted.
-- 
-- \return An existing object if the Group was discarded because the topology already
-- contained an object at the same location (the Group did not add any locality information).
-- Any name/info key pair set before inserting is appended to the existing object.
-- 
-- \return \c NULL if the insertion failed because of conflicting sets in topology tree.
-- 
-- \return \c NULL if Group objects are always ignored in the topology.
-- 
-- \return \c NULL if the object was discarded because no set was initialized in the Group
-- before insert, or all of them were empty.
-- 
--foreign import ccall "hwloc_topology_insert_group_object" insertGroupObject :: Topology -> Ptr Object -> IO (Ptr Object)

-- | Setup object cpusets/nodesets by OR'ing another object's sets.
-- 
-- For each defined cpuset or nodeset in \p src, allocate the corresponding set
-- in \p dst and add \p src to it by OR'ing sets.
-- 
-- This function is convenient between hwloc_topology_alloc_group_object()
-- and hwloc_topology_insert_group_object(). It builds the sets of the new Group
-- that will be inserted as a new intermediate parent of several objects.
-- 
--foreign import ccall "hwloc_obj_add_other_obj_sets" addSetsFromOtherObject :: Ptr Object -> Ptr Object -> IO Int

-- | Returns the topology object at logical index \p idx from depth \p depth
foreign import ccall "hwloc_get_obj_by_depth" getObject' :: Topology -> Word -> Word -> IO (Ptr Object)

getObject :: Topology -> Word -> Word -> IO Object
getObject topo depth idx = peekObject =<< getObject' topo depth idx

