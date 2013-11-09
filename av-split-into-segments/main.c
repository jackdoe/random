#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <stdint.h>
#include <pthread.h>
#include <unistd.h>
#include <libavformat/avformat.h>
#include "mongo.h"
/*
/Applications/VLC.app/Contents/MacOS/VLC -I rc test.avi --sout '#duplicate{dst="transcode{vcodec=h264,vb=1024,acodec=mpga,ab=256}:standard{mux=ts,dst=localhost:8500,access=udp}",dst="transcode{vcodec=h264,vb=512,acodec=mpga,ab=128}:standard{mux=ts,dst=localhost:8501,access=udp}",dst="transcode{vcodec=h264,vb=256,acodec=mpga,ab=64}:standard{mux=ts,dst=localhost:8502,access=udp}"}'
*/
#ifndef MIN
    #define MIN(a,b) (((a)<(b))?(a):(b))
#endif

#define D(fmt, arg...) printf(fmt " - [%s():%s:%d]\n", ##arg,__func__,__FILE__,__LINE__);

#define SAYX(fmt, arg...)   \
do {                        \
    D(fmt,##arg);           \
    exit(1);                \
} while(0);


#define COPY(a,b,len) bcopy((a),(b),len)
#define SEGMENT_DURATION 5.0
#define BUFFERSIZE 3276800

typedef uint64_t u64;
typedef uint32_t u32;
typedef uint8_t u8;
struct _b {
    u8 *value;
    u64 pos;
    u64 len;
};
typedef struct _b blob;
struct _segment {
    blob *frame;
    double duration;
    double prev, current;
    int reload;
    char *url;
    pthread_mutex_t lock;
    mongo conn[1];
};


blob * new_blob(u64 len);
blob * set_blob(blob *b, u8 *data, u64 len);
blob * reset_blob(blob *b);
void destroy_blob(blob *b);

blob * reset_blob(blob *b) {
    b->pos = 0;
    return b;
}
blob * set_blob(blob *b, u8 *data, u64 len) {
    u64 copy_len = MIN(len,(b->len - b->pos));
    COPY(data,(b->value + b->pos),copy_len);
    b->pos += copy_len;
    return b;
}
blob * append_blob(blob *b, u8 *data, u64 len) {
    if ((b->pos + len) > b->len) {
        blob *m = new_blob(b->pos + len + 1);
        set_blob(m,b->value,b->pos);
        set_blob(m,data,len);
        destroy_blob(b);
        return m;
    } else {
        set_blob(b,data,len);
        return b;
    }
}
void destroy_blob(blob *b) {
    free(b->value);
    free(b);
}
blob * new_blob(u64 len) {
    assert(len > 0);
    blob *b = malloc(sizeof(*b));
    assert(b);
    b->value = malloc(sizeof(*b->value) * len);
    assert(b->value);
    b->len = len;
    b->pos = 0;
    return b;
}
int io_callback(void *opaque, uint8_t *buf, int buf_size) {
    struct _segment *s = (struct _segment *) opaque;
    pthread_mutex_lock(&s->lock);
    s->frame = append_blob(s->frame,buf,buf_size);
    if (s->reload) {
        bson *b = bson_malloc(sizeof(bson));
        bson_init(b);
        bson_append_binary(b, "data",BSON_BIN_BINARY, (char *) s->frame->value,s->frame->pos);
        bson_append_int(b, "stamp", time(NULL));
        bson_append_string(b,"url",s->url);
        bson_append_double(b, "duration", s->duration);
        bson_finish(b);
        mongo_insert(s->conn,"video.segments",b);
        bson_print(b);
        bson_destroy(b);
        bson_free(b);
        s->frame = reset_blob(s->frame);
        s->reload = 0;
    }
    pthread_mutex_unlock(&s->lock);
    return buf_size;
}
static AVStream *add_output_stream(AVFormatContext *output_format_context, AVStream *input_stream) {
    AVCodecContext *input_codec_context;
    AVCodecContext *output_codec_context;
    AVStream *output_stream;

    output_stream = avformat_new_stream(output_format_context, 0);
    if (!output_stream)
        SAYX("Could not allocate stream");

    input_codec_context = input_stream->codec;
    output_codec_context = output_stream->codec;

    output_codec_context->codec_id = input_codec_context->codec_id;
    output_codec_context->codec_type = input_codec_context->codec_type;
    output_codec_context->codec_tag = input_codec_context->codec_tag;
    output_codec_context->bit_rate = input_codec_context->bit_rate;
    output_codec_context->extradata = input_codec_context->extradata;
    output_codec_context->extradata_size = input_codec_context->extradata_size;

    if(av_q2d(input_codec_context->time_base) * input_codec_context->ticks_per_frame > av_q2d(input_stream->time_base) && av_q2d(input_stream->time_base) < 1.0/1000) {
        output_codec_context->time_base = input_codec_context->time_base;
        output_codec_context->time_base.num *= input_codec_context->ticks_per_frame;
    } else {
        output_codec_context->time_base = input_stream->time_base;
    }

    switch (input_codec_context->codec_type) {
    case AVMEDIA_TYPE_AUDIO:
        output_codec_context->channel_layout = input_codec_context->channel_layout;
        output_codec_context->sample_rate = input_codec_context->sample_rate;
        output_codec_context->channels = input_codec_context->channels;
        output_codec_context->frame_size = input_codec_context->frame_size;
        if ((input_codec_context->block_align == 1 && input_codec_context->codec_id == CODEC_ID_MP3) || input_codec_context->codec_id == CODEC_ID_AC3)
            output_codec_context->block_align = 0;
        else
            output_codec_context->block_align = input_codec_context->block_align;
        break;
    case AVMEDIA_TYPE_VIDEO:
        output_codec_context->pix_fmt = input_codec_context->pix_fmt;
        output_codec_context->width = input_codec_context->width;
        output_codec_context->height = input_codec_context->height;
        output_codec_context->has_b_frames = input_codec_context->has_b_frames;

        if (output_format_context->oformat->flags & AVFMT_GLOBALHEADER)
            output_codec_context->flags |= CODEC_FLAG_GLOBAL_HEADER;
        break;
    default:
        break;
    }

    return output_stream;
}
int process(char *url) {
    D("starting: %s",url);
    AVInputFormat *ifmt;
    AVOutputFormat *ofmt;
    AVFormatContext *ic = NULL, *oc = NULL;
    AVStream *video_st = NULL, *audio_st = NULL;
    AVCodec *codec;
    int video_index = -1;
    int audio_index = -1;
    u32 i;

    struct _segment segment;
    bzero(&segment,sizeof(segment));
    segment.frame = new_blob(1);
    segment.url = url;
    pthread_mutex_init(&segment.lock,NULL);
    if (mongo_connect( segment.conn, "192.168.0.1", 27017) != MONGO_OK)
        SAYX("error connecting");


    av_register_all();
    avformat_network_init();
    av_log_set_level(AV_LOG_DEBUG);

    ifmt = av_find_input_format("mpegts");
    if (!ifmt) 
        SAYX("Could not find MPEG-TS demuxer");

    if (avformat_open_input(&ic, segment.url, ifmt, NULL) != 0) 
        SAYX("Could not open input file, make sure it is an mpegts file");

    if (avformat_find_stream_info(ic,NULL) < 0)
        SAYX("Could not read stream information");

    ofmt = av_guess_format("mpegts", NULL, NULL);
    if (!ofmt)
        SAYX("Could not find MPEG-TS muxer, in av_guess_format()");

    oc = avformat_alloc_context();
  u8 *buffer = av_malloc(BUFFERSIZE);
  if (!buffer)
    SAYX("couldnt create buffer");
    oc->pb = avio_alloc_context(buffer, BUFFERSIZE, 0, &segment, NULL, io_callback, NULL);
    if (!oc)
        SAYX("Could not allocated output context");

  oc->oformat = ofmt;

    for (i = 0; i < ic->nb_streams && (video_index < 0 || audio_index < 0); i++) {
        switch (ic->streams[i]->codec->codec_type) {
        case AVMEDIA_TYPE_VIDEO:
            video_index = i;
            ic->streams[i]->discard = AVDISCARD_NONE;
            video_st = add_output_stream(oc, ic->streams[i]);
            break;
        case AVMEDIA_TYPE_AUDIO:
            audio_index = i;
            ic->streams[i]->discard = AVDISCARD_NONE;
            audio_st = add_output_stream(oc, ic->streams[i]);
            break;
        default:
            ic->streams[i]->discard = AVDISCARD_ALL;
            break;
        }
    }
    ic->flags |= AVFMT_FLAG_IGNDTS;
    av_dump_format(oc, 0, segment.url, 1);

    if (video_st) {
        codec = avcodec_find_decoder(video_st->codec->codec_id);
        if (!codec)
            D("Could not find video decoder %x, key frames will not be honored\n", video_st->codec->codec_id);
        if (avcodec_open2(video_st->codec, codec,NULL) < 0)
            D("Could not open video decoder, key frames will not be honored\n");
    }
    AVPacket packet;
    if (avformat_write_header(oc, NULL) < 0)
        SAYX("failed to write header");
    for (;;) {
        if (av_read_frame(ic, &packet) != 0)
            break;

        if (packet.stream_index == video_index && (packet.flags & AV_PKT_FLAG_KEY)) {
            segment.current = (packet.pts * av_q2d(video_st->time_base));
            segment.duration = segment.current - segment.prev;
            if (segment.duration > SEGMENT_DURATION) {
                segment.reload = 1;
                // if (avformat_write_header(oc, NULL) < 0)
                //  SAYX("failed to write header");
                segment.prev = segment.current;
            }
        }

        av_interleaved_write_frame(oc, &packet);
        av_free_packet(&packet);
    }

    if (video_st)
        avcodec_close(video_st->codec);
    if (audio_st)   
        avcodec_close(audio_st->codec);

    for(i = 0; i < oc->nb_streams; i++) {
        av_freep(&oc->streams[i]->codec);
        av_freep(&oc->streams[i]);
    }
    mongo_destroy(segment.conn);
    av_free(buffer);
    av_free(oc);
    pthread_mutex_destroy(&segment.lock);
    return 0;
}

int main(void) {
    pthread_t unused;
    char *c[] = {"udp://localhost:8500","udp://localhost:8501","udp://localhost:8502"};
    int i;
    for (i=0; i <sizeof(c)/sizeof(c[0]); i++)
        pthread_create (&unused, NULL, (void *) &process, (void *) c[i]);

    for (;;) {
        sleep(1);
    }
    return 0;
}
