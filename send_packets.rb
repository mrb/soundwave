require 'socket'
require './datum.pb'

def frame_datum(datum)
  datum_string = datum.serialize_to_string
  datum_length = [0, 0, 0, datum_string.length].pack("C*")
  "#{datum_length}#{datum_string}"
end

datum = Soundwave::Datum.new(vector: [Soundwave::Value.new(key:1,value:8)], name: "foo")
framed_datum = frame_datum(datum)

socket = UDPSocket.new
socket.connect("127.0.0.1",1514)
socket.send framed_datum, 0
