### Generated by rprotoc. DO NOT EDIT!
### <proto file: datum.proto>
# package soundwave;
# 
# option java_package = "in.michaelrbernste.soundwave";
# option java_outer_classname = "SoundwaveProtos";
# 
# message Datum {
#   required string name = 1;
#   repeated Value vector = 2;
# }
# 
# message Value {
#   required int32 key = 1;
#   required int32 value = 2;
# }

require 'protobuf/message/message'
require 'protobuf/message/enum'
require 'protobuf/message/service'
require 'protobuf/message/extend'

module Soundwave
  ::Protobuf::OPTIONS[:"java_package"] = "in.michaelrbernste.soundwave"
  ::Protobuf::OPTIONS[:"java_outer_classname"] = "SoundwaveProtos"
  class Datum < ::Protobuf::Message
    defined_in __FILE__
    required :string, :name, 1
    repeated :Value, :vector, 2
  end
  class Value < ::Protobuf::Message
    defined_in __FILE__
    required :int32, :key, 1
    required :int32, :value, 2
  end
end