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

socket = UDPSocket.new
socket.connect("127.0.0.1",1514)

100.times {
  datum = Soundwave::Datum.new(
    vector: [ rand_valp, rand_valp, rand_valp, rand_valp ],
    name: rand_name)
  
  framed_datum = frame_datum(datum)
  
  socket.send framed_datum, 0
  p socket.recv 1024
}

def send_query(socket,name)
  v = Soundwave::Value.new({})
  datum = Soundwave::Datum.new(vector: [rand_valp], name: name)
  framed_datum = frame_datum datum
  socket.send framed_datum, 0
  p socket.recv 1024
end

def send_update(socket,name)
  v = Soundwave::Value.new({})
  datum = Soundwave::Datum.new(vector: [rand_valp, rand_valp, rand_valp], name: name)
  framed_datum = frame_datum datum
  socket.send framed_datum, 0
  p socket.recv 1024
end
