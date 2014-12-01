require 'socket'
require './datum.pb'

class Soundwave::Client
  attr_accessor :socket

  def initialize(socket=UDPSocket.new,host="127.0.0.1",port=1514)
    @socket = socket
    @host = host
    @port = port
  end

  def connect
    @socket.connect(@host, @port)
  end

  def update(vector, name)
    connect
    datum = Soundwave::Datum.new(vector: vector, name: name)
    socket.send frame_pb(request(datum)), 0
    rcv
  end

  def query(name)
    connect
    datum = Soundwave::Datum.new(vector: [], name: name)
    socket.send frame_pb(request(datum)), 0
    rcv
  end

private
  def frame_pb(pb)
    pb_string = pb.serialize_to_string
    pb_length = [0, 0, 0, pb_string.length].pack("C*")
    "#{pb_length}#{pb_string}"
  end

  def rcv
    resp = socket.recv(1024)
    Soundwave::Response.new.parse_from_string resp
  end

  def request(datum)
    Soundwave::Request.new(request: datum)
  end
end

class Soundwave::Helpers
  def self.rand_name
    "#{(rand*100000000).to_i}"
  end

  def self.rand_val
    (rand*10000).to_i
  end

  def self.rand_valp
    Soundwave::Value.new(key: rand_val, value: rand_val)
  end
end

client = Soundwave::Client.new; client.update([Soundwave::Value.new(key:1865, value: 9000)], "foo")
