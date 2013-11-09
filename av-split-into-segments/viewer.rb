require 'mongoid'
require 'sinatra'
require 'thread'
set :logging, true
Mongoid.configure { |config| config.master = Mongo::Connection.new("127.0.0.1".db("video") }
class Segments
  include Mongoid::Document
  field :data
  field :duration, type: Float
  field :stamp, type: Integer
  field :url, type: String
  index :stamp
end
t = Thread.new do |t| 
  while true
    puts "deleted : " + Segments.where(:stamp.lt => (Time.new.to_i - 300)).delete.to_s
    sleep 1
  end
end

Segments.create_indexes

get '/segment/:id-:seq.ts' do |id,seq|
  content_type 'video/h264'
        Segments.find(id).data.to_s rescue ""
end
get '/list.m3u8' do 
  erb :list
end
get '/list-:port.m3u8' do |port|
  content_type 'octet/stream'

  s = []
  s << "#EXTM3U"
  segments = Segments.asc(:stamp).where(:url => /:#{port}/)
  
  f = segments.first
  duration = 0.0
  segments.each { |x| duration += x.duration }
  c = (segments.count > 0) ? segments.count : 1
  duration = duration / c
  s << "#EXT-X-MEDIA-SEQUENCE:#{f.stamp}" if f
  s << "#EXT-X-TARGETDURATION:#{duration.to_i}"
  segments.each do |segment|
    s << "#EXTINF:#{segment.duration.to_i}, simple"
    s << "http://localhost:4567/segment/#{segment.id}-#{segment.stamp}.ts"
    puts "#{segment.stamp}"
  end
  # s << "#EXT-X-ENDLIST"
  p s
  s.join("\n")
end

get '/play' do
  erb :play
end

__END__
@@play
<html>
<body>
<h1> Test for simple WebM Live streaming </h1>
<video src="http://localhost:4567/list-8500.m3u8" type="video/h264" autoplay="autoplay" controls="controls" height="360" width="640"></video>
<video src="http://localhost:4567/list-8501.m3u8" type="video/h264" autoplay="autoplay" controls="controls" height="360" width="640"></video>
<video src="http://localhost:4567/list-8502.m3u8" type="video/h264" autoplay="autoplay" controls="controls" height="360" width="640"></video>
</body>
</html>

@@list
#EXTM3U
#EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=10240000
http://localhost:4567/list-8500.m3u8
#EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=5120000
http://localhost:4567/list-8501.m3u8
#EXT-X-STREAM-INF:PROGRAM-ID=1,BANDWIDTH=2560000
http://localhost:4567/list-8502.m3u8
#EXT-X-ENDLIST


