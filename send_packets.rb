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

def start
  socket = UDPSocket.new
  socket.connect("127.0.0.1",1514)
  send_query(socket)
  p socket.recv(1024)
}

def send_query(socket,name)
  v = Soundwave::Value.new({})
  datum = Soundwave::Datum.new(vector: [], name: name)
  framed_datum = frame_datum datum
  socket.send framed_datum, 0
  resp = socket.recv 1024
  Soundwave::Datum.new.parse_from_string resp
end

def send_update(socket,name)
  v = Soundwave::Value.new({})
  datum = Soundwave::Datum.new(vector: [rand_valp, rand_valp, rand_valp], name: name)
  framed_datum = frame_datum datum
  socket.send framed_datum, 0
  resp = socket.recv 1024
  Soundwave::Datum.new.parse_from_string resp
end

def max_update(socket, name, value)
  v = Soundwave::Value.new({})
  datum = Soundwave::Datum.new(vector: [value], name: name)
  framed_datum = frame_datum datum
  socket.send framed_datum, 0
  resp = socket.recv 1024
  Soundwave::Datum.new.parse_from_string resp
end
