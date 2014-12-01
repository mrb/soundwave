require 'socket'
require './datum.pb'

def frame_pb(pb)
  pb_string = pb.serialize_to_string
  pb_length = [0, 0, 0, pb_string.length].pack("C*")
  "#{pb_length}#{pb_string}"
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

def send_query(socket, name)
  v = Soundwave::Value.new({})
  datum = Soundwave::Datum.new(vector: [], name: name)
  request = Soundwave::Request.new(request: datum)
  framed_request = frame_pb request
  socket.send framed_request, 0
  resp = socket.recv 1024
  Soundwave::Response.new.parse_from_string resp
end

def send_update(socket, name)
  v = Soundwave::Value.new({})
  datum = Soundwave::Datum.new(vector: [rand_valp, rand_valp, rand_valp], name: name)
  request = Soundwave::Request.new(request: datum)
  framed_request = frame_pb request
  socket.send framed_request, 0
  resp = socket.recv 2048
  Soundwave::Response.new.parse_from_string resp
end

def max_update(socket, name, value=rand_valp)
  v = Soundwave::Value.new({})
  datum = Soundwave::Datum.new(vector: [value], name: name)
  request = Soundwave::Request.new(request: datum)
  framed_request = frame_pb request
  socket.send framed_request, 0
  resp = socket.recv 2048
  Soundwave::Response.new.parse_from_string resp
end

socket = UDPSocket.new
socket.connect("127.0.0.1",1514)
send_update(socket,"z")
