
obj sereal_to_obj(obj *s);
static obj s_default_reader(obj *s, u8 tag);
static obj s_read_zigzag(obj *s, u8 tag);
static obj s_read_varint(obj *s, u8 tag);
static obj s_read_small_positive_int(obj *s, u8 tag);
static obj s_read_small_negative_int(obj *s, u8 tag);
static obj s_read_array(obj *s,u8 tag);
static obj s_read_arrayref(obj *s, u8 tag);
static obj s_read_hash(obj *s, u8 tag);
static obj s_read_hashref(obj *s, u8 tag);
static obj s_read_string(obj *s,u8 t);
static obj s_read_nil(obj *s, u8 tag);
static obj s_read_true(obj *s, u8 tag);
static obj s_read_false(obj *s, u8 tag);
static obj s_read_pad(obj *s, u8 tag);
static obj s_read_extend(obj *s, u8 tag);

static obj (*READERS[256])(obj *, u8) = {
    s_read_small_positive_int,                      // 0    SRL_HDR_POS_LOW
    s_read_small_positive_int,                      // 1 
    s_read_small_positive_int,                      // 2
    s_read_small_positive_int,                      // 3
    s_read_small_positive_int,                      // 4
    s_read_small_positive_int,                      // 5
    s_read_small_positive_int,                      // 6
    s_read_small_positive_int,                      // 7
    s_read_small_positive_int,                      // 8
    s_read_small_positive_int,                      // 9
    s_read_small_positive_int,                      // 10
    s_read_small_positive_int,                      // 11
    s_read_small_positive_int,                      // 12
    s_read_small_positive_int,                      // 13
    s_read_small_positive_int,                      // 14
    s_read_small_positive_int,                      // 15   SRL_HDR_POS_HIGH
    s_read_small_negative_int,                      // 16   SRL_HDR_NEG_LOW
    s_read_small_negative_int,                      // 17
    s_read_small_negative_int,                      // 18
    s_read_small_negative_int,                      // 19
    s_read_small_negative_int,                      // 20
    s_read_small_negative_int,                      // 21
    s_read_small_negative_int,                      // 22
    s_read_small_negative_int,                      // 23
    s_read_small_negative_int,                      // 24
    s_read_small_negative_int,                      // 25
    s_read_small_negative_int,                      // 26
    s_read_small_negative_int,                      // 27
    s_read_small_negative_int,                      // 28
    s_read_small_negative_int,                      // 29
    s_read_small_negative_int,                      // 30
    s_read_small_negative_int,                      // 31   SRL_HDR_NEG_HIGH
    s_read_varint,                                  // 32   SRL_HDR_VARINT
    s_read_zigzag,                                  // 33   SRL_HDR_ZIGZAG
    s_default_reader,                               // 34   SRL_HDR_FLOAT
    s_default_reader,                               // 35   SRL_HDR_DOUBLE
    s_default_reader,                               // 36   SRL_HDR_LONG_DOUBLE
    s_read_nil,                                     // 37   SRL_HDR_UNDEF
    s_read_string,                                  // 38   SRL_HDR_BINARY
    s_default_reader,                               // 39   SRL_HDR_STR_UTF8
    s_read_pad,                                     // 40   SRL_HDR_REFN
    s_default_reader,                               // 41   SRL_HDR_REFP
    s_read_hash,                                    // 42   SRL_HDR_HASH
    s_read_array,                                   // 43   SRL_HDR_ARRAY
    s_default_reader,                               // 44   SRL_HDR_OBJECT
    s_default_reader,                               // 45   SRL_HDR_OBJECTV
    s_default_reader,                               // 46   SRL_HDR_ALIAS
    s_default_reader,                               // 47   SRL_HDR_COPY
    s_default_reader,                               // 48   SRL_HDR_WEAKEN
    s_default_reader,                               // 49   SRL_HDR_REGEXP
    s_default_reader,                               // 50   OBJECT_FREEZE
    s_default_reader,                               // 51   OBJECTV_FREEZE
    s_default_reader,                               // 52
    s_default_reader,                               // 53
    s_default_reader,                               // 54
    s_default_reader,                               // 55
    s_default_reader,                               // 56
    s_default_reader,                               // 57   SRL_HDR_RESERVED_HIGH
    s_read_false,                                   // 58   SRL_HDR_FALSE
    s_read_true,                                    // 59   SRL_HDR_TRUE
    s_default_reader,                               // 60   SRL_HDR_MANY
    s_default_reader,                               // 61   SRL_HDR_PACKET_START
    s_read_extend,                                  // 62   SRL_HDR_EXTEND
    s_read_pad,                                     // 63   SRL_HDR_PAD
    s_read_arrayref,                                // 64   SRL_HDR_ARRAYREF_LOW
    s_read_arrayref,                                // 65
    s_read_arrayref,                                // 66
    s_read_arrayref,                                // 67
    s_read_arrayref,                                // 68
    s_read_arrayref,                                // 69
    s_read_arrayref,                                // 70
    s_read_arrayref,                                // 71
    s_read_arrayref,                                // 72
    s_read_arrayref,                                // 73
    s_read_arrayref,                                // 74
    s_read_arrayref,                                // 75
    s_read_arrayref,                                // 76
    s_read_arrayref,                                // 77
    s_read_arrayref,                                // 78
    s_read_arrayref,                                // 79   SRL_HDR_ARRAYREF_HIGH
    s_read_hashref,                                 // 80   SRL_HDR_HASHREF_LOW
    s_read_hashref,                                 // 81
    s_read_hashref,                                 // 82
    s_read_hashref,                                 // 83
    s_read_hashref,                                 // 84
    s_read_hashref,                                 // 85
    s_read_hashref,                                 // 86
    s_read_hashref,                                 // 87
    s_read_hashref,                                 // 88
    s_read_hashref,                                 // 89
    s_read_hashref,                                 // 90
    s_read_hashref,                                 // 91
    s_read_hashref,                                 // 92
    s_read_hashref,                                 // 93
    s_read_hashref,                                 // 94
    s_read_hashref,                                 // 95   SRL_HDR_HASHREF_HIGH
    s_read_string,                                  // 96   SRL_HDR_SHORT_BINARY_LOW
    s_read_string,                                  // 97
    s_read_string,                                  // 98
    s_read_string,                                  // 99
    s_read_string,                                  // 100
    s_read_string,                                  // 101
    s_read_string,                                  // 102
    s_read_string,                                  // 103
    s_read_string,                                  // 104
    s_read_string,                                  // 105
    s_read_string,                                  // 106
    s_read_string,                                  // 107
    s_read_string,                                  // 108
    s_read_string,                                  // 109
    s_read_string,                                  // 110
    s_read_string,                                  // 111
    s_read_string,                                  // 112
    s_read_string,                                  // 113
    s_read_string,                                  // 114
    s_read_string,                                  // 115
    s_read_string,                                  // 116
    s_read_string,                                  // 117
    s_read_string,                                  // 118
    s_read_string,                                  // 119
    s_read_string,                                  // 120
    s_read_string,                                  // 121
    s_read_string,                                  // 122
    s_read_string,                                  // 123
    s_read_string,                                  // 124
    s_read_string,                                  // 125
    s_read_string,                                  // 126
    s_read_string,                                  // 127
    s_default_reader,                               // 128  SRL_HDR_TRACK_FLAG
    s_default_reader,                               // 129
    s_default_reader,                               // 130
    s_default_reader,                               // 131
    s_default_reader,                               // 132
    s_default_reader,                               // 133
    s_default_reader,                               // 134
    s_default_reader,                               // 135
    s_default_reader,                               // 136
    s_default_reader,                               // 137
    s_default_reader,                               // 138
    s_default_reader,                               // 139
    s_default_reader,                               // 140
    s_default_reader,                               // 141
    s_default_reader,                               // 142
    s_default_reader,                               // 143
    s_default_reader,                               // 144
    s_default_reader,                               // 145
    s_default_reader,                               // 146
    s_default_reader,                               // 147
    s_default_reader,                               // 148
    s_default_reader,                               // 149
    s_default_reader,                               // 150
    s_default_reader,                               // 151
    s_default_reader,                               // 152
    s_default_reader,                               // 153
    s_default_reader,                               // 154
    s_default_reader,                               // 155
    s_default_reader,                               // 156
    s_default_reader,                               // 157
    s_default_reader,                               // 158
    s_default_reader,                               // 159
    s_default_reader,                               // 160
    s_default_reader,                               // 161
    s_default_reader,                               // 162
    s_default_reader,                               // 163
    s_default_reader,                               // 164
    s_default_reader,                               // 165
    s_default_reader,                               // 166
    s_default_reader,                               // 167
    s_default_reader,                               // 168
    s_default_reader,                               // 169
    s_default_reader,                               // 170
    s_default_reader,                               // 171
    s_default_reader,                               // 172
    s_default_reader,                               // 173
    s_default_reader,                               // 174
    s_default_reader,                               // 175
    s_default_reader,                               // 176
    s_default_reader,                               // 177
    s_default_reader,                               // 178
    s_default_reader,                               // 179
    s_default_reader,                               // 180
    s_default_reader,                               // 181
    s_default_reader,                               // 182
    s_default_reader,                               // 183
    s_default_reader,                               // 184
    s_default_reader,                               // 185
    s_default_reader,                               // 186
    s_default_reader,                               // 187
    s_default_reader,                               // 188
    s_default_reader,                               // 189
    s_default_reader,                               // 190
    s_default_reader,                               // 191
    s_default_reader,                               // 192
    s_default_reader,                               // 193
    s_default_reader,                               // 194
    s_default_reader,                               // 195
    s_default_reader,                               // 196
    s_default_reader,                               // 197
    s_default_reader,                               // 198
    s_default_reader,                               // 199
    s_default_reader,                               // 200
    s_default_reader,                               // 201
    s_default_reader,                               // 202
    s_default_reader,                               // 203
    s_default_reader,                               // 204
    s_default_reader,                               // 205
    s_default_reader,                               // 206
    s_default_reader,                               // 207
    s_default_reader,                               // 208
    s_default_reader,                               // 209
    s_default_reader,                               // 210
    s_default_reader,                               // 211
    s_default_reader,                               // 212
    s_default_reader,                               // 213
    s_default_reader,                               // 214
    s_default_reader,                               // 215
    s_default_reader,                               // 216
    s_default_reader,                               // 217
    s_default_reader,                               // 218
    s_default_reader,                               // 219
    s_default_reader,                               // 220
    s_default_reader,                               // 221
    s_default_reader,                               // 222
    s_default_reader,                               // 223
    s_default_reader,                               // 224
    s_default_reader,                               // 225
    s_default_reader,                               // 226
    s_default_reader,                               // 227
    s_default_reader,                               // 228
    s_default_reader,                               // 229
    s_default_reader,                               // 230
    s_default_reader,                               // 231
    s_default_reader,                               // 232
    s_default_reader,                               // 233
    s_default_reader,                               // 234
    s_default_reader,                               // 235
    s_default_reader,                               // 236
    s_default_reader,                               // 237
    s_default_reader,                               // 238
    s_default_reader,                               // 239
    s_default_reader,                               // 240
    s_default_reader,                               // 241
    s_default_reader,                               // 242
    s_default_reader,                               // 243
    s_default_reader,                               // 244
    s_default_reader,                               // 245
    s_default_reader,                               // 246
    s_default_reader,                               // 247
    s_default_reader,                               // 248
    s_default_reader,                               // 249
    s_default_reader,                               // 250
    s_default_reader,                               // 251
    s_default_reader,                               // 252
    s_default_reader,                               // 253
    s_default_reader,                               // 254
    s_default_reader                                // 255
};
