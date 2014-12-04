{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module SoundwaveProtos.Snapshot (Snapshot(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified SoundwaveProtos.Datum as SoundwaveProtos (Datum)
 
data Snapshot = Snapshot{dat :: !(P'.Seq SoundwaveProtos.Datum)}
              deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Snapshot where
  mergeAppend (Snapshot x'1) (Snapshot y'1) = Snapshot (P'.mergeAppend x'1 y'1)
 
instance P'.Default Snapshot where
  defaultValue = Snapshot P'.defaultValue
 
instance P'.Wire Snapshot where
  wireSize ft' self'@(Snapshot x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 11 x'1)
  wirePut ft' self'@(Snapshot x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutRep 10 11 x'1
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{dat = P'.append (dat old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Snapshot) Snapshot where
  getVal m' f' = f' m'
 
instance P'.GPB Snapshot
 
instance P'.ReflectDescriptor Snapshot where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".soundwave.Snapshot\", haskellPrefix = [], parentModule = [MName \"SoundwaveProtos\"], baseName = MName \"Snapshot\"}, descFilePath = [\"SoundwaveProtos\",\"Snapshot.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".soundwave.Snapshot.dat\", haskellPrefix' = [], parentModule' = [MName \"SoundwaveProtos\",MName \"Snapshot\"], baseName' = FName \"dat\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".soundwave.Datum\", haskellPrefix = [], parentModule = [MName \"SoundwaveProtos\"], baseName = MName \"Datum\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"