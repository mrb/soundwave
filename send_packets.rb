require 'socket'
require './datum.pb'

def frame_datum(datum)
  datum_string = datum.serialize_to_string
  datum_length = [0, 0, 0, datum_string.length].pack("C*")
  "#{datum_length}#{datum_string}"
end

def rand_name
  "#{(rand*100000000).to_i}"
end

def rand_val
  (rand*10000).to_i
end

def rand_valp
  Soundwave::Value.new(key: rand_val, value: rand_val)
end

def send_random_data(socket)
  100.times {
    datum = Soundwave::Datum.new(vector: [ rand_valp, rand_valp, rand_valp, rand_valp ], name: "foo")
    
    framed_datum = frame_datum(datum)
    
    socket.send framed_datum, 0
  }
end

def send_query(socket)
  v = Soundwave::Value.new({})
  datum = Soundwave::Datum.new(vector: [], name: "foo%")
  framed_datum = frame_datum datum
  socket.send framed_datum, 0
end

socket = UDPSocket.new
socket.connect("127.0.0.1",1514)
send_query(socket)
