; Prints values in various ways to output,
; including numbers and strings.

newline = DTE_NEWLINE

zp_byte print_temp_

.ifdef NEED_PRINT_REG

; Prints indicated register to console as two hex
; chars and space
; Preserved: A, X, Y, flags
print_a:
	php
	pha
print_reg_:
	jsr print_hex
	lda #' '	;DTE_CHARMAP
	jsr print_char_
	pla
	plp
	rts

print_x:
	php
	pha
	txa
	jmp print_reg_

print_y:
	php
	pha
	tya
	jmp print_reg_

print_p:
	php
	pha
	php
	pla
	jmp print_reg_

print_s:
	php
	pha
	txa
	tsx
	inx
	inx
	inx
	inx
	jsr print_x
	tax
	pla
	plp
	rts

.endif

; Prints A as two hex characters, NO space after
; Preserved: A, X, Y
print_hex:
.ifdef NEED_CRC_FUN
	jsr update_crc
.endif	
	pha
	lsr a
	lsr a
	lsr a
	lsr a
	jsr print_hex_nibble
	pla
	
	pha
	and #$0F
	jsr print_hex_nibble
	pla
	rts
	
print_hex_nibble:
    @z = '0'	;DTE_CHARMAP
    ;empty line in between to prevent the two lines being joined
    @a = 'A'	;DTE_CHARMAP
    .if @a <> @z+10
	cmp #10
	blt @digit
	adc #(@a-@z-11) ;+1 since carry is set
@digit:
    .else
	clc
    .endif
	adc #@z
	jmp print_char_


; Prints character and updates checksum UNLESS
; it's a newline.
; Preserved: A, X, Y
print_char:
.ifdef NEED_CRC_FUN
	cmp #DTE_NEWLINE
	beq :+
	jsr update_crc
:
.endif
        pha
	jsr print_char_
	pla
	rts

; Prints space. Does NOT update checksum.
; Preserved: A, X, Y
print_space:
	pha
	lda #' '	;DTE_CHARMAP
	jsr print_char_
	pla
	rts


; Advances to next line. Does NOT update checksum.
; Preserved: A, X, Y
print_newline:
	pha
	lda #DTE_NEWLINE
	jsr print_char_
	pla
	rts


; Prints string
; Preserved: A, X, Y
.macro print_str_no_nul s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,s91,s92,s93,s94,s95,s96,s97,s98,s99,s100,s101,s102,s103,s104,s105,s106,s107,s108,s109,s110,s111,s112,s113,s114,s115,s116,s117,s118,s119,s120,s121,s122,s123,s124,s125,s126,s127,s128,s129,s130,s131,s132,s133,s134,s135,s136,s137,s138,s139,s140,s141,s142,s143,s144,s145,s146,s147,s148,s149,s150,s151,s152,s153,s154,s155,s156,s157,s158,s159,s160,s161,s162,s163,s164,s165,s166,s167,s168,s169,s170,s171,s172,s173,s174,s175,s176,s177,s178,s179,s180,s181,s182,s183,s184,s185,s186,s187,s188,s189,s190,s191,s192,s193,s194,s195,s196,s197,s198,s199,s200,s201,s202,s203,s204,s205,s206,s207,s208,s209,s210,s211,s212,s213,s214,s215,s216,s217,s218,s219,s220,s221,s222,s223,s224,s225,s226,s227,s228,s229,s230,s231,s232,s233,s234,s235,s236,s237,s238,s239,s240,s241,s242,s243,s244,s245,s246,s247,s248,s249,s250,s251,s252,s253,s254,s255,s256,s257,s258,s259,s260,s261,s262,s263,s264,s265,s266,s267,s268,s269,s270,s271,s272,s273,s274,s275,s276,s277,s278,s279,s280,s281,s282,s283,s284,s285,s286,s287,s288,s289,s290,s291,s292,s293,s294,s295,s296,s297,s298,s299,s300
	jsr print_str_
	.byte s
.ifnblank s2
.byte s2
.endif
.ifnblank s3
.byte s3
.endif
.ifnblank s4
.byte s4
.endif
.ifnblank s5
.byte s5
.endif
.ifnblank s6
.byte s6
.endif
.ifnblank s7
.byte s7
.endif
.ifnblank s8
.byte s8
.endif
.ifnblank s9
.byte s9
.endif
.ifnblank s10
.byte s10
.endif
.ifnblank s11
.byte s11
.endif
.ifnblank s12
.byte s12
.endif
.ifnblank s13
.byte s13
.endif
.ifnblank s14
.byte s14
.endif
.ifnblank s15
.byte s15
.endif
.ifnblank s16
.byte s16
.endif
.ifnblank s17
.byte s17
.endif
.ifnblank s18
.byte s18
.endif
.ifnblank s19
.byte s19
.endif
.ifnblank s20
.byte s20
.endif
.ifnblank s21
.byte s21
.endif
.ifnblank s22
.byte s22
.endif
.ifnblank s23
.byte s23
.endif
.ifnblank s24
.byte s24
.endif
.ifnblank s25
.byte s25
.endif
.ifnblank s26
.byte s26
.endif
.ifnblank s27
.byte s27
.endif
.ifnblank s28
.byte s28
.endif
.ifnblank s29
.byte s29
.endif
.ifnblank s30
.byte s30
.endif
.ifnblank s31
.byte s31
.endif
.ifnblank s32
.byte s32
.endif
.ifnblank s33
.byte s33
.endif
.ifnblank s34
.byte s34
.endif
.ifnblank s35
.byte s35
.endif
.ifnblank s36
.byte s36
.endif
.ifnblank s37
.byte s37
.endif
.ifnblank s38
.byte s38
.endif
.ifnblank s39
.byte s39
.endif
.ifnblank s40
.byte s40
.endif
.ifnblank s41
.byte s41
.endif
.ifnblank s42
.byte s42
.endif
.ifnblank s43
.byte s43
.endif
.ifnblank s44
.byte s44
.endif
.ifnblank s45
.byte s45
.endif
.ifnblank s46
.byte s46
.endif
.ifnblank s47
.byte s47
.endif
.ifnblank s48
.byte s48
.endif
.ifnblank s49
.byte s49
.endif
.ifnblank s50
.byte s50
.endif
.ifnblank s51
.byte s51
.endif
.ifnblank s52
.byte s52
.endif
.ifnblank s53
.byte s53
.endif
.ifnblank s54
.byte s54
.endif
.ifnblank s55
.byte s55
.endif
.ifnblank s56
.byte s56
.endif
.ifnblank s57
.byte s57
.endif
.ifnblank s58
.byte s58
.endif
.ifnblank s59
.byte s59
.endif
.ifnblank s60
.byte s60
.endif
.ifnblank s61
.byte s61
.endif
.ifnblank s62
.byte s62
.endif
.ifnblank s63
.byte s63
.endif
.ifnblank s64
.byte s64
.endif
.ifnblank s65
.byte s65
.endif
.ifnblank s66
.byte s66
.endif
.ifnblank s67
.byte s67
.endif
.ifnblank s68
.byte s68
.endif
.ifnblank s69
.byte s69
.endif
.ifnblank s70
.byte s70
.endif
.ifnblank s71
.byte s71
.endif
.ifnblank s72
.byte s72
.endif
.ifnblank s73
.byte s73
.endif
.ifnblank s74
.byte s74
.endif
.ifnblank s75
.byte s75
.endif
.ifnblank s76
.byte s76
.endif
.ifnblank s77
.byte s77
.endif
.ifnblank s78
.byte s78
.endif
.ifnblank s79
.byte s79
.endif
.ifnblank s80
.byte s80
.endif
.ifnblank s81
.byte s81
.endif
.ifnblank s82
.byte s82
.endif
.ifnblank s83
.byte s83
.endif
.ifnblank s84
.byte s84
.endif
.ifnblank s85
.byte s85
.endif
.ifnblank s86
.byte s86
.endif
.ifnblank s87
.byte s87
.endif
.ifnblank s88
.byte s88
.endif
.ifnblank s89
.byte s89
.endif
.ifnblank s90
.byte s90
.endif
.ifnblank s91
.byte s91
.endif
.ifnblank s92
.byte s92
.endif
.ifnblank s93
.byte s93
.endif
.ifnblank s94
.byte s94
.endif
.ifnblank s95
.byte s95
.endif
.ifnblank s96
.byte s96
.endif
.ifnblank s97
.byte s97
.endif
.ifnblank s98
.byte s98
.endif
.ifnblank s99
.byte s99
.endif
.ifnblank s100
.byte s100
.endif
.ifnblank s101
.byte s101
.endif
.ifnblank s102
.byte s102
.endif
.ifnblank s103
.byte s103
.endif
.ifnblank s104
.byte s104
.endif
.ifnblank s105
.byte s105
.endif
.ifnblank s106
.byte s106
.endif
.ifnblank s107
.byte s107
.endif
.ifnblank s108
.byte s108
.endif
.ifnblank s109
.byte s109
.endif
.ifnblank s110
.byte s110
.endif
.ifnblank s111
.byte s111
.endif
.ifnblank s112
.byte s112
.endif
.ifnblank s113
.byte s113
.endif
.ifnblank s114
.byte s114
.endif
.ifnblank s115
.byte s115
.endif
.ifnblank s116
.byte s116
.endif
.ifnblank s117
.byte s117
.endif
.ifnblank s118
.byte s118
.endif
.ifnblank s119
.byte s119
.endif
.ifnblank s120
.byte s120
.endif
.ifnblank s121
.byte s121
.endif
.ifnblank s122
.byte s122
.endif
.ifnblank s123
.byte s123
.endif
.ifnblank s124
.byte s124
.endif
.ifnblank s125
.byte s125
.endif
.ifnblank s126
.byte s126
.endif
.ifnblank s127
.byte s127
.endif
.ifnblank s128
.byte s128
.endif
.ifnblank s129
.byte s129
.endif
.ifnblank s130
.byte s130
.endif
.ifnblank s131
.byte s131
.endif
.ifnblank s132
.byte s132
.endif
.ifnblank s133
.byte s133
.endif
.ifnblank s134
.byte s134
.endif
.ifnblank s135
.byte s135
.endif
.ifnblank s136
.byte s136
.endif
.ifnblank s137
.byte s137
.endif
.ifnblank s138
.byte s138
.endif
.ifnblank s139
.byte s139
.endif
.ifnblank s140
.byte s140
.endif
.ifnblank s141
.byte s141
.endif
.ifnblank s142
.byte s142
.endif
.ifnblank s143
.byte s143
.endif
.ifnblank s144
.byte s144
.endif
.ifnblank s145
.byte s145
.endif
.ifnblank s146
.byte s146
.endif
.ifnblank s147
.byte s147
.endif
.ifnblank s148
.byte s148
.endif
.ifnblank s149
.byte s149
.endif
.ifnblank s150
.byte s150
.endif
.ifnblank s151
.byte s151
.endif
.ifnblank s152
.byte s152
.endif
.ifnblank s153
.byte s153
.endif
.ifnblank s154
.byte s154
.endif
.ifnblank s155
.byte s155
.endif
.ifnblank s156
.byte s156
.endif
.ifnblank s157
.byte s157
.endif
.ifnblank s158
.byte s158
.endif
.ifnblank s159
.byte s159
.endif
.ifnblank s160
.byte s160
.endif
.ifnblank s161
.byte s161
.endif
.ifnblank s162
.byte s162
.endif
.ifnblank s163
.byte s163
.endif
.ifnblank s164
.byte s164
.endif
.ifnblank s165
.byte s165
.endif
.ifnblank s166
.byte s166
.endif
.ifnblank s167
.byte s167
.endif
.ifnblank s168
.byte s168
.endif
.ifnblank s169
.byte s169
.endif
.ifnblank s170
.byte s170
.endif
.ifnblank s171
.byte s171
.endif
.ifnblank s172
.byte s172
.endif
.ifnblank s173
.byte s173
.endif
.ifnblank s174
.byte s174
.endif
.ifnblank s175
.byte s175
.endif
.ifnblank s176
.byte s176
.endif
.ifnblank s177
.byte s177
.endif
.ifnblank s178
.byte s178
.endif
.ifnblank s179
.byte s179
.endif
.ifnblank s180
.byte s180
.endif
.ifnblank s181
.byte s181
.endif
.ifnblank s182
.byte s182
.endif
.ifnblank s183
.byte s183
.endif
.ifnblank s184
.byte s184
.endif
.ifnblank s185
.byte s185
.endif
.ifnblank s186
.byte s186
.endif
.ifnblank s187
.byte s187
.endif
.ifnblank s188
.byte s188
.endif
.ifnblank s189
.byte s189
.endif
.ifnblank s190
.byte s190
.endif
.ifnblank s191
.byte s191
.endif
.ifnblank s192
.byte s192
.endif
.ifnblank s193
.byte s193
.endif
.ifnblank s194
.byte s194
.endif
.ifnblank s195
.byte s195
.endif
.ifnblank s196
.byte s196
.endif
.ifnblank s197
.byte s197
.endif
.ifnblank s198
.byte s198
.endif
.ifnblank s199
.byte s199
.endif
.ifnblank s200
.byte s200
.endif
.ifnblank s201
.byte s201
.endif
.ifnblank s202
.byte s202
.endif
.ifnblank s203
.byte s203
.endif
.ifnblank s204
.byte s204
.endif
.ifnblank s205
.byte s205
.endif
.ifnblank s206
.byte s206
.endif
.ifnblank s207
.byte s207
.endif
.ifnblank s208
.byte s208
.endif
.ifnblank s209
.byte s209
.endif
.ifnblank s210
.byte s210
.endif
.ifnblank s211
.byte s211
.endif
.ifnblank s212
.byte s212
.endif
.ifnblank s213
.byte s213
.endif
.ifnblank s214
.byte s214
.endif
.ifnblank s215
.byte s215
.endif
.ifnblank s216
.byte s216
.endif
.ifnblank s217
.byte s217
.endif
.ifnblank s218
.byte s218
.endif
.ifnblank s219
.byte s219
.endif
.ifnblank s220
.byte s220
.endif
.ifnblank s221
.byte s221
.endif
.ifnblank s222
.byte s222
.endif
.ifnblank s223
.byte s223
.endif
.ifnblank s224
.byte s224
.endif
.ifnblank s225
.byte s225
.endif
.ifnblank s226
.byte s226
.endif
.ifnblank s227
.byte s227
.endif
.ifnblank s228
.byte s228
.endif
.ifnblank s229
.byte s229
.endif
.ifnblank s230
.byte s230
.endif
.ifnblank s231
.byte s231
.endif
.ifnblank s232
.byte s232
.endif
.ifnblank s233
.byte s233
.endif
.ifnblank s234
.byte s234
.endif
.ifnblank s235
.byte s235
.endif
.ifnblank s236
.byte s236
.endif
.ifnblank s237
.byte s237
.endif
.ifnblank s238
.byte s238
.endif
.ifnblank s239
.byte s239
.endif
.ifnblank s240
.byte s240
.endif
.ifnblank s241
.byte s241
.endif
.ifnblank s242
.byte s242
.endif
.ifnblank s243
.byte s243
.endif
.ifnblank s244
.byte s244
.endif
.ifnblank s245
.byte s245
.endif
.ifnblank s246
.byte s246
.endif
.ifnblank s247
.byte s247
.endif
.ifnblank s248
.byte s248
.endif
.ifnblank s249
.byte s249
.endif
.ifnblank s250
.byte s250
.endif
.ifnblank s251
.byte s251
.endif
.ifnblank s252
.byte s252
.endif
.ifnblank s253
.byte s253
.endif
.ifnblank s254
.byte s254
.endif
.ifnblank s255
.byte s255
.endif
.ifnblank s256
.byte s256
.endif
.ifnblank s257
.byte s257
.endif
.ifnblank s258
.byte s258
.endif
.ifnblank s259
.byte s259
.endif
.ifnblank s260
.byte s260
.endif
.ifnblank s261
.byte s261
.endif
.ifnblank s262
.byte s262
.endif
.ifnblank s263
.byte s263
.endif
.ifnblank s264
.byte s264
.endif
.ifnblank s265
.byte s265
.endif
.ifnblank s266
.byte s266
.endif
.ifnblank s267
.byte s267
.endif
.ifnblank s268
.byte s268
.endif
.ifnblank s269
.byte s269
.endif
.ifnblank s270
.byte s270
.endif
.ifnblank s271
.byte s271
.endif
.ifnblank s272
.byte s272
.endif
.ifnblank s273
.byte s273
.endif
.ifnblank s274
.byte s274
.endif
.ifnblank s275
.byte s275
.endif
.ifnblank s276
.byte s276
.endif
.ifnblank s277
.byte s277
.endif
.ifnblank s278
.byte s278
.endif
.ifnblank s279
.byte s279
.endif
.ifnblank s280
.byte s280
.endif
.ifnblank s281
.byte s281
.endif
.ifnblank s282
.byte s282
.endif
.ifnblank s283
.byte s283
.endif
.ifnblank s284
.byte s284
.endif
.ifnblank s285
.byte s285
.endif
.ifnblank s286
.byte s286
.endif
.ifnblank s287
.byte s287
.endif
.ifnblank s288
.byte s288
.endif
.ifnblank s289
.byte s289
.endif
.ifnblank s290
.byte s290
.endif
.ifnblank s291
.byte s291
.endif
.ifnblank s292
.byte s292
.endif
.ifnblank s293
.byte s293
.endif
.ifnblank s294
.byte s294
.endif
.ifnblank s295
.byte s295
.endif
.ifnblank s296
.byte s296
.endif
.ifnblank s297
.byte s297
.endif
.ifnblank s298
.byte s298
.endif
.ifnblank s299
.byte s299
.endif
.ifnblank s300
.byte s300
.endif
.endmacro

.macro print_str s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,s91,s92,s93,s94,s95,s96,s97,s98,s99,s100,s101,s102,s103,s104,s105,s106,s107,s108,s109,s110,s111,s112,s113,s114,s115,s116,s117,s118,s119,s120,s121,s122,s123,s124,s125,s126,s127,s128,s129,s130,s131,s132,s133,s134,s135,s136,s137,s138,s139,s140,s141,s142,s143,s144,s145,s146,s147,s148,s149,s150,s151,s152,s153,s154,s155,s156,s157,s158,s159,s160,s161,s162,s163,s164,s165,s166,s167,s168,s169,s170,s171,s172,s173,s174,s175,s176,s177,s178,s179,s180,s181,s182,s183,s184,s185,s186,s187,s188,s189,s190,s191,s192,s193,s194,s195,s196,s197,s198,s199,s200,s201,s202,s203,s204,s205,s206,s207,s208,s209,s210,s211,s212,s213,s214,s215,s216,s217,s218,s219,s220,s221,s222,s223,s224,s225,s226,s227,s228,s229,s230,s231,s232,s233,s234,s235,s236,s237,s238,s239,s240,s241,s242,s243,s244,s245,s246,s247,s248,s249,s250,s251,s252,s253,s254,s255,s256,s257,s258,s259,s260,s261,s262,s263,s264,s265,s266,s267,s268,s269,s270,s271,s272,s273,s274,s275,s276,s277,s278,s279,s280,s281,s282,s283,s284,s285,s286,s287,s288,s289,s290,s291,s292,s293,s294,s295,s296,s297,s298,s299
	print_str_no_nul s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,s91,s92,s93,s94,s95,s96,s97,s98,s99,s100,s101,s102,s103,s104,s105,s106,s107,s108,s109,s110,s111,s112,s113,s114,s115,s116,s117,s118,s119,s120,s121,s122,s123,s124,s125,s126,s127,s128,s129,s130,s131,s132,s133,s134,s135,s136,s137,s138,s139,s140,s141,s142,s143,s144,s145,s146,s147,s148,s149,s150,s151,s152,s153,s154,s155,s156,s157,s158,s159,s160,s161,s162,s163,s164,s165,s166,s167,s168,s169,s170,s171,s172,s173,s174,s175,s176,s177,s178,s179,s180,s181,s182,s183,s184,s185,s186,s187,s188,s189,s190,s191,s192,s193,s194,s195,s196,s197,s198,s199,s200,s201,s202,s203,s204,s205,s206,s207,s208,s209,s210,s211,s212,s213,s214,s215,s216,s217,s218,s219,s220,s221,s222,s223,s224,s225,s226,s227,s228,s229,s230,s231,s232,s233,s234,s235,s236,s237,s238,s239,s240,s241,s242,s243,s244,s245,s246,s247,s248,s249,s250,s251,s252,s253,s254,s255,s256,s257,s258,s259,s260,s261,s262,s263,s264,s265,s266,s267,s268,s269,s270,s271,s272,s273,s274,s275,s276,s277,s278,s279,s280,s281,s282,s283,s284,s285,s286,s287,s288,s289,s290,s291,s292,s293,s294,s295,s296,s297,s298,s299,0
.endmacro

.macro print_str_and_ret s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,s91,s92,s93,s94,s95,s96,s97,s98,s99,s100,s101,s102,s103,s104,s105,s106,s107,s108,s109,s110,s111,s112,s113,s114,s115,s116,s117,s118,s119,s120,s121,s122,s123,s124,s125,s126,s127,s128,s129,s130,s131,s132,s133,s134,s135,s136,s137,s138,s139,s140,s141,s142,s143,s144,s145,s146,s147,s148,s149,s150,s151,s152,s153,s154,s155,s156,s157,s158,s159,s160,s161,s162,s163,s164,s165,s166,s167,s168,s169,s170,s171,s172,s173,s174,s175,s176,s177,s178,s179,s180,s181,s182,s183,s184,s185,s186,s187,s188,s189,s190,s191,s192,s193,s194,s195,s196,s197,s198,s199,s200,s201,s202,s203,s204,s205,s206,s207,s208,s209,s210,s211,s212,s213,s214,s215,s216,s217,s218,s219,s220,s221,s222,s223,s224,s225,s226,s227,s228,s229,s230,s231,s232,s233,s234,s235,s236,s237,s238,s239,s240,s241,s242,s243,s244,s245,s246,s247,s248,s249,s250,s251,s252,s253,s254,s255,s256,s257,s258,s259,s260,s261,s262,s263,s264,s265,s266,s267,s268,s269,s270,s271,s272,s273,s274,s275,s276,s277,s278,s279,s280,s281,s282,s283,s284,s285,s286,s287,s288,s289,s290,s291,s292,s293,s294,s295,s296,s297,s298,s299
	print_str s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,s91,s92,s93,s94,s95,s96,s97,s98,s99,s100,s101,s102,s103,s104,s105,s106,s107,s108,s109,s110,s111,s112,s113,s114,s115,s116,s117,s118,s119,s120,s121,s122,s123,s124,s125,s126,s127,s128,s129,s130,s131,s132,s133,s134,s135,s136,s137,s138,s139,s140,s141,s142,s143,s144,s145,s146,s147,s148,s149,s150,s151,s152,s153,s154,s155,s156,s157,s158,s159,s160,s161,s162,s163,s164,s165,s166,s167,s168,s169,s170,s171,s172,s173,s174,s175,s176,s177,s178,s179,s180,s181,s182,s183,s184,s185,s186,s187,s188,s189,s190,s191,s192,s193,s194,s195,s196,s197,s198,s199,s200,s201,s202,s203,s204,s205,s206,s207,s208,s209,s210,s211,s212,s213,s214,s215,s216,s217,s218,s219,s220,s221,s222,s223,s224,s225,s226,s227,s228,s229,s230,s231,s232,s233,s234,s235,s236,s237,s238,s239,s240,s241,s242,s243,s244,s245,s246,s247,s248,s249,s250,s251,s252,s253,s254,s255,s256,s257,s258,s259,s260,s261,s262,s263,s264,s265,s266,s267,s268,s269,s270,s271,s272,s273,s274,s275,s276,s277,s278,s279,s280,s281,s282,s283,s284,s285,s286,s287,s288,s289,s290,s291,s292,s293,s294,s295,s296,s297,s298,s299
	rts
.endmacro
.macro print_str_no_nul_and_ret s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,s91,s92,s93,s94,s95,s96,s97,s98,s99,s100,s101,s102,s103,s104,s105,s106,s107,s108,s109,s110,s111,s112,s113,s114,s115,s116,s117,s118,s119,s120,s121,s122,s123,s124,s125,s126,s127,s128,s129,s130,s131,s132,s133,s134,s135,s136,s137,s138,s139,s140,s141,s142,s143,s144,s145,s146,s147,s148,s149,s150,s151,s152,s153,s154,s155,s156,s157,s158,s159,s160,s161,s162,s163,s164,s165,s166,s167,s168,s169,s170,s171,s172,s173,s174,s175,s176,s177,s178,s179,s180,s181,s182,s183,s184,s185,s186,s187,s188,s189,s190,s191,s192,s193,s194,s195,s196,s197,s198,s199,s200,s201,s202,s203,s204,s205,s206,s207,s208,s209,s210,s211,s212,s213,s214,s215,s216,s217,s218,s219,s220,s221,s222,s223,s224,s225,s226,s227,s228,s229,s230,s231,s232,s233,s234,s235,s236,s237,s238,s239,s240,s241,s242,s243,s244,s245,s246,s247,s248,s249,s250,s251,s252,s253,s254,s255,s256,s257,s258,s259,s260,s261,s262,s263,s264,s265,s266,s267,s268,s269,s270,s271,s272,s273,s274,s275,s276,s277,s278,s279,s280,s281,s282,s283,s284,s285,s286,s287,s288,s289,s290,s291,s292,s293,s294,s295,s296,s297,s298,s299
	print_str_no_nul s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,s91,s92,s93,s94,s95,s96,s97,s98,s99,s100,s101,s102,s103,s104,s105,s106,s107,s108,s109,s110,s111,s112,s113,s114,s115,s116,s117,s118,s119,s120,s121,s122,s123,s124,s125,s126,s127,s128,s129,s130,s131,s132,s133,s134,s135,s136,s137,s138,s139,s140,s141,s142,s143,s144,s145,s146,s147,s148,s149,s150,s151,s152,s153,s154,s155,s156,s157,s158,s159,s160,s161,s162,s163,s164,s165,s166,s167,s168,s169,s170,s171,s172,s173,s174,s175,s176,s177,s178,s179,s180,s181,s182,s183,s184,s185,s186,s187,s188,s189,s190,s191,s192,s193,s194,s195,s196,s197,s198,s199,s200,s201,s202,s203,s204,s205,s206,s207,s208,s209,s210,s211,s212,s213,s214,s215,s216,s217,s218,s219,s220,s221,s222,s223,s224,s225,s226,s227,s228,s229,s230,s231,s232,s233,s234,s235,s236,s237,s238,s239,s240,s241,s242,s243,s244,s245,s246,s247,s248,s249,s250,s251,s252,s253,s254,s255,s256,s257,s258,s259,s260,s261,s262,s263,s264,s265,s266,s267,s268,s269,s270,s271,s272,s273,s274,s275,s276,s277,s278,s279,s280,s281,s282,s283,s284,s285,s286,s287,s288,s289,s290,s291,s292,s293,s294,s295,s296,s297,s298,s299
	rts
.endmacro

.macro print_ext_str s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,s91,s92,s93,s94,s95,s96,s97,s98,s99,s100,s101,s102,s103,s104,s105,s106,s107,s108,s109,s110,s111,s112,s113,s114,s115,s116,s117,s118,s119,s120,s121,s122,s123,s124,s125,s126,s127,s128,s129,s130,s131,s132,s133,s134,s135,s136,s137,s138,s139,s140,s141,s142,s143,s144,s145,s146,s147,s148,s149,s150,s151,s152,s153,s154,s155,s156,s157,s158,s159,s160,s161,s162,s163,s164,s165,s166,s167,s168,s169,s170,s171,s172,s173,s174,s175,s176,s177,s178,s179,s180,s181,s182,s183,s184,s185,s186,s187,s188,s189,s190,s191,s192,s193,s194,s195,s196,s197,s198,s199,s200,s201,s202,s203,s204,s205,s206,s207,s208,s209,s210,s211,s212,s213,s214,s215,s216,s217,s218,s219,s220,s221,s222,s223,s224,s225,s226,s227,s228,s229,s230,s231,s232,s233,s234,s235,s236,s237,s238,s239,s240,s241,s242,s243,s244,s245,s246,s247,s248,s249,s250,s251,s252,s253,s254,s255,s256,s257,s258,s259,s260,s261,s262,s263,s264,s265,s266,s267,s268,s269,s270,s271,s272,s273,s274,s275,s276,s277,s278,s279,s280,s281,s282,s283,s284,s285,s286,s287,s288,s289,s290,s291,s292,s293,s294,s295,s296,s297,s298,s299
	.local @Addr
	jsr @Addr
	seg_data "RODATA",{@Addr: print_str_and_ret s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,s91,s92,s93,s94,s95,s96,s97,s98,s99,s100,s101,s102,s103,s104,s105,s106,s107,s108,s109,s110,s111,s112,s113,s114,s115,s116,s117,s118,s119,s120,s121,s122,s123,s124,s125,s126,s127,s128,s129,s130,s131,s132,s133,s134,s135,s136,s137,s138,s139,s140,s141,s142,s143,s144,s145,s146,s147,s148,s149,s150,s151,s152,s153,s154,s155,s156,s157,s158,s159,s160,s161,s162,s163,s164,s165,s166,s167,s168,s169,s170,s171,s172,s173,s174,s175,s176,s177,s178,s179,s180,s181,s182,s183,s184,s185,s186,s187,s188,s189,s190,s191,s192,s193,s194,s195,s196,s197,s198,s199,s200,s201,s202,s203,s204,s205,s206,s207,s208,s209,s210,s211,s212,s213,s214,s215,s216,s217,s218,s219,s220,s221,s222,s223,s224,s225,s226,s227,s228,s229,s230,s231,s232,s233,s234,s235,s236,s237,s238,s239,s240,s241,s242,s243,s244,s245,s246,s247,s248,s249,s250,s251,s252,s253,s254,s255,s256,s257,s258,s259,s260,s261,s262,s263,s264,s265,s266,s267,s268,s269,s270,s271,s272,s273,s274,s275,s276,s277,s278,s279,s280,s281,s282,s283,s284,s285,s286,s287,s288,s289,s290,s291,s292,s293,s294,s295,s296,s297,s298,s299}
.endmacro
.macro print_ext_str_flush s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,s91,s92,s93,s94,s95,s96,s97,s98,s99,s100,s101,s102,s103,s104,s105,s106,s107,s108,s109,s110,s111,s112,s113,s114,s115,s116,s117,s118,s119,s120,s121,s122,s123,s124,s125,s126,s127,s128,s129,s130,s131,s132,s133,s134,s135,s136,s137,s138,s139,s140,s141,s142,s143,s144,s145,s146,s147,s148,s149,s150,s151,s152,s153,s154,s155,s156,s157,s158,s159,s160,s161,s162,s163,s164,s165,s166,s167,s168,s169,s170,s171,s172,s173,s174,s175,s176,s177,s178,s179,s180,s181,s182,s183,s184,s185,s186,s187,s188,s189,s190,s191,s192,s193,s194,s195,s196,s197,s198,s199,s200,s201,s202,s203,s204,s205,s206,s207,s208,s209,s210,s211,s212,s213,s214,s215,s216,s217,s218,s219,s220,s221,s222,s223,s224,s225,s226,s227,s228,s229,s230,s231,s232,s233,s234,s235,s236,s237,s238,s239,s240,s241,s242,s243,s244,s245,s246,s247,s248,s249,s250,s251,s252,s253,s254,s255,s256,s257,s258,s259,s260,s261,s262,s263,s264,s265,s266,s267,s268,s269,s270,s271,s272,s273,s274,s275,s276,s277,s278,s279,s280,s281,s282,s283,s284,s285,s286,s287,s288,s289,s290,s291,s292,s293,s294,s295,s296,s297,s298,s299
	.local @Addr
	jsr @Addr
	.pushseg
	.segment "RODATA"
	 @Addr:
	 print_str s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,s91,s92,s93,s94,s95,s96,s97,s98,s99,s100,s101,s102,s103,s104,s105,s106,s107,s108,s109,s110,s111,s112,s113,s114,s115,s116,s117,s118,s119,s120,s121,s122,s123,s124,s125,s126,s127,s128,s129,s130,s131,s132,s133,s134,s135,s136,s137,s138,s139,s140,s141,s142,s143,s144,s145,s146,s147,s148,s149,s150,s151,s152,s153,s154,s155,s156,s157,s158,s159,s160,s161,s162,s163,s164,s165,s166,s167,s168,s169,s170,s171,s172,s173,s174,s175,s176,s177,s178,s179,s180,s181,s182,s183,s184,s185,s186,s187,s188,s189,s190,s191,s192,s193,s194,s195,s196,s197,s198,s199,s200,s201,s202,s203,s204,s205,s206,s207,s208,s209,s210,s211,s212,s213,s214,s215,s216,s217,s218,s219,s220,s221,s222,s223,s224,s225,s226,s227,s228,s229,s230,s231,s232,s233,s234,s235,s236,s237,s238,s239,s240,s241,s242,s243,s244,s245,s246,s247,s248,s249,s250,s251,s252,s253,s254,s255,s256,s257,s258,s259,s260,s261,s262,s263,s264,s265,s266,s267,s268,s269,s270,s271,s272,s273,s274,s275,s276,s277,s278,s279,s280,s281,s282,s283,s284,s285,s286,s287,s288,s289,s290,s291,s292,s293,s294,s295,s296,s297,s298,s299
	 jmp console_flush
	.popseg
.endmacro
.macro print_ext_str_flush_rts s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,s91,s92,s93,s94,s95,s96,s97,s98,s99,s100,s101,s102,s103,s104,s105,s106,s107,s108,s109,s110,s111,s112,s113,s114,s115,s116,s117,s118,s119,s120,s121,s122,s123,s124,s125,s126,s127,s128,s129,s130,s131,s132,s133,s134,s135,s136,s137,s138,s139,s140,s141,s142,s143,s144,s145,s146,s147,s148,s149,s150,s151,s152,s153,s154,s155,s156,s157,s158,s159,s160,s161,s162,s163,s164,s165,s166,s167,s168,s169,s170,s171,s172,s173,s174,s175,s176,s177,s178,s179,s180,s181,s182,s183,s184,s185,s186,s187,s188,s189,s190,s191,s192,s193,s194,s195,s196,s197,s198,s199,s200,s201,s202,s203,s204,s205,s206,s207,s208,s209,s210,s211,s212,s213,s214,s215,s216,s217,s218,s219,s220,s221,s222,s223,s224,s225,s226,s227,s228,s229,s230,s231,s232,s233,s234,s235,s236,s237,s238,s239,s240,s241,s242,s243,s244,s245,s246,s247,s248,s249,s250,s251,s252,s253,s254,s255,s256,s257,s258,s259,s260,s261,s262,s263,s264,s265,s266,s267,s268,s269,s270,s271,s272,s273,s274,s275,s276,s277,s278,s279,s280,s281,s282,s283,s284,s285,s286,s287,s288,s289,s290,s291,s292,s293,s294,s295,s296,s297,s298,s299
	.local @Addr
	jmp @Addr
	.pushseg
	.segment "RODATA"
	 @Addr:
	 print_str s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,s91,s92,s93,s94,s95,s96,s97,s98,s99,s100,s101,s102,s103,s104,s105,s106,s107,s108,s109,s110,s111,s112,s113,s114,s115,s116,s117,s118,s119,s120,s121,s122,s123,s124,s125,s126,s127,s128,s129,s130,s131,s132,s133,s134,s135,s136,s137,s138,s139,s140,s141,s142,s143,s144,s145,s146,s147,s148,s149,s150,s151,s152,s153,s154,s155,s156,s157,s158,s159,s160,s161,s162,s163,s164,s165,s166,s167,s168,s169,s170,s171,s172,s173,s174,s175,s176,s177,s178,s179,s180,s181,s182,s183,s184,s185,s186,s187,s188,s189,s190,s191,s192,s193,s194,s195,s196,s197,s198,s199,s200,s201,s202,s203,s204,s205,s206,s207,s208,s209,s210,s211,s212,s213,s214,s215,s216,s217,s218,s219,s220,s221,s222,s223,s224,s225,s226,s227,s228,s229,s230,s231,s232,s233,s234,s235,s236,s237,s238,s239,s240,s241,s242,s243,s244,s245,s246,s247,s248,s249,s250,s251,s252,s253,s254,s255,s256,s257,s258,s259,s260,s261,s262,s263,s264,s265,s266,s267,s268,s269,s270,s271,s272,s273,s274,s275,s276,s277,s278,s279,s280,s281,s282,s283,s284,s285,s286,s287,s288,s289,s290,s291,s292,s293,s294,s295,s296,s297,s298,s299
	 jmp console_flush
	.popseg
.endmacro

.macro print_ext_str_no_nul s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,s91,s92,s93,s94,s95,s96,s97,s98,s99,s100,s101,s102,s103,s104,s105,s106,s107,s108,s109,s110,s111,s112,s113,s114,s115,s116,s117,s118,s119,s120,s121,s122,s123,s124,s125,s126,s127,s128,s129,s130,s131,s132,s133,s134,s135,s136,s137,s138,s139,s140,s141,s142,s143,s144,s145,s146,s147,s148,s149,s150,s151,s152,s153,s154,s155,s156,s157,s158,s159,s160,s161,s162,s163,s164,s165,s166,s167,s168,s169,s170,s171,s172,s173,s174,s175,s176,s177,s178,s179,s180,s181,s182,s183,s184,s185,s186,s187,s188,s189,s190,s191,s192,s193,s194,s195,s196,s197,s198,s199,s200,s201,s202,s203,s204,s205,s206,s207,s208,s209,s210,s211,s212,s213,s214,s215,s216,s217,s218,s219,s220,s221,s222,s223,s224,s225,s226,s227,s228,s229,s230,s231,s232,s233,s234,s235,s236,s237,s238,s239,s240,s241,s242,s243,s244,s245,s246,s247,s248,s249,s250,s251,s252,s253,s254,s255,s256,s257,s258,s259,s260,s261,s262,s263,s264,s265,s266,s267,s268,s269,s270,s271,s272,s273,s274,s275,s276,s277,s278,s279,s280,s281,s282,s283,s284,s285,s286,s287,s288,s289,s290,s291,s292,s293,s294,s295,s296,s297,s298,s299
	.local @Addr
	jsr @Addr
	seg_data "RODATA",{@Addr: print_str_no_nul_and_ret s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,s91,s92,s93,s94,s95,s96,s97,s98,s99,s100,s101,s102,s103,s104,s105,s106,s107,s108,s109,s110,s111,s112,s113,s114,s115,s116,s117,s118,s119,s120,s121,s122,s123,s124,s125,s126,s127,s128,s129,s130,s131,s132,s133,s134,s135,s136,s137,s138,s139,s140,s141,s142,s143,s144,s145,s146,s147,s148,s149,s150,s151,s152,s153,s154,s155,s156,s157,s158,s159,s160,s161,s162,s163,s164,s165,s166,s167,s168,s169,s170,s171,s172,s173,s174,s175,s176,s177,s178,s179,s180,s181,s182,s183,s184,s185,s186,s187,s188,s189,s190,s191,s192,s193,s194,s195,s196,s197,s198,s199,s200,s201,s202,s203,s204,s205,s206,s207,s208,s209,s210,s211,s212,s213,s214,s215,s216,s217,s218,s219,s220,s221,s222,s223,s224,s225,s226,s227,s228,s229,s230,s231,s232,s233,s234,s235,s236,s237,s238,s239,s240,s241,s242,s243,s244,s245,s246,s247,s248,s249,s250,s251,s252,s253,s254,s255,s256,s257,s258,s259,s260,s261,s262,s263,s264,s265,s266,s267,s268,s269,s270,s271,s272,s273,s274,s275,s276,s277,s278,s279,s280,s281,s282,s283,s284,s285,s286,s287,s288,s289,s290,s291,s292,s293,s294,s295,s296,s297,s298,s299}
.endmacro
.macro print_ext_str_no_nul_flush s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,s91,s92,s93,s94,s95,s96,s97,s98,s99,s100,s101,s102,s103,s104,s105,s106,s107,s108,s109,s110,s111,s112,s113,s114,s115,s116,s117,s118,s119,s120,s121,s122,s123,s124,s125,s126,s127,s128,s129,s130,s131,s132,s133,s134,s135,s136,s137,s138,s139,s140,s141,s142,s143,s144,s145,s146,s147,s148,s149,s150,s151,s152,s153,s154,s155,s156,s157,s158,s159,s160,s161,s162,s163,s164,s165,s166,s167,s168,s169,s170,s171,s172,s173,s174,s175,s176,s177,s178,s179,s180,s181,s182,s183,s184,s185,s186,s187,s188,s189,s190,s191,s192,s193,s194,s195,s196,s197,s198,s199,s200,s201,s202,s203,s204,s205,s206,s207,s208,s209,s210,s211,s212,s213,s214,s215,s216,s217,s218,s219,s220,s221,s222,s223,s224,s225,s226,s227,s228,s229,s230,s231,s232,s233,s234,s235,s236,s237,s238,s239,s240,s241,s242,s243,s244,s245,s246,s247,s248,s249,s250,s251,s252,s253,s254,s255,s256,s257,s258,s259,s260,s261,s262,s263,s264,s265,s266,s267,s268,s269,s270,s271,s272,s273,s274,s275,s276,s277,s278,s279,s280,s281,s282,s283,s284,s285,s286,s287,s288,s289,s290,s291,s292,s293,s294,s295,s296,s297,s298,s299
	.local @Addr
	jsr @Addr
	.pushseg
	.segment "RODATA"
	 @Addr:
	 print_str_no_nul s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,s91,s92,s93,s94,s95,s96,s97,s98,s99,s100,s101,s102,s103,s104,s105,s106,s107,s108,s109,s110,s111,s112,s113,s114,s115,s116,s117,s118,s119,s120,s121,s122,s123,s124,s125,s126,s127,s128,s129,s130,s131,s132,s133,s134,s135,s136,s137,s138,s139,s140,s141,s142,s143,s144,s145,s146,s147,s148,s149,s150,s151,s152,s153,s154,s155,s156,s157,s158,s159,s160,s161,s162,s163,s164,s165,s166,s167,s168,s169,s170,s171,s172,s173,s174,s175,s176,s177,s178,s179,s180,s181,s182,s183,s184,s185,s186,s187,s188,s189,s190,s191,s192,s193,s194,s195,s196,s197,s198,s199,s200,s201,s202,s203,s204,s205,s206,s207,s208,s209,s210,s211,s212,s213,s214,s215,s216,s217,s218,s219,s220,s221,s222,s223,s224,s225,s226,s227,s228,s229,s230,s231,s232,s233,s234,s235,s236,s237,s238,s239,s240,s241,s242,s243,s244,s245,s246,s247,s248,s249,s250,s251,s252,s253,s254,s255,s256,s257,s258,s259,s260,s261,s262,s263,s264,s265,s266,s267,s268,s269,s270,s271,s272,s273,s274,s275,s276,s277,s278,s279,s280,s281,s282,s283,s284,s285,s286,s287,s288,s289,s290,s291,s292,s293,s294,s295,s296,s297,s298,s299
	 jmp console_flush
	.popseg
.endmacro
.macro print_ext_str_no_nul_flush_rts s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,s91,s92,s93,s94,s95,s96,s97,s98,s99,s100,s101,s102,s103,s104,s105,s106,s107,s108,s109,s110,s111,s112,s113,s114,s115,s116,s117,s118,s119,s120,s121,s122,s123,s124,s125,s126,s127,s128,s129,s130,s131,s132,s133,s134,s135,s136,s137,s138,s139,s140,s141,s142,s143,s144,s145,s146,s147,s148,s149,s150,s151,s152,s153,s154,s155,s156,s157,s158,s159,s160,s161,s162,s163,s164,s165,s166,s167,s168,s169,s170,s171,s172,s173,s174,s175,s176,s177,s178,s179,s180,s181,s182,s183,s184,s185,s186,s187,s188,s189,s190,s191,s192,s193,s194,s195,s196,s197,s198,s199,s200,s201,s202,s203,s204,s205,s206,s207,s208,s209,s210,s211,s212,s213,s214,s215,s216,s217,s218,s219,s220,s221,s222,s223,s224,s225,s226,s227,s228,s229,s230,s231,s232,s233,s234,s235,s236,s237,s238,s239,s240,s241,s242,s243,s244,s245,s246,s247,s248,s249,s250,s251,s252,s253,s254,s255,s256,s257,s258,s259,s260,s261,s262,s263,s264,s265,s266,s267,s268,s269,s270,s271,s272,s273,s274,s275,s276,s277,s278,s279,s280,s281,s282,s283,s284,s285,s286,s287,s288,s289,s290,s291,s292,s293,s294,s295,s296,s297,s298,s299
	.local @Addr
	jmp @Addr
	.pushseg
	.segment "RODATA"
	 @Addr:
	 print_str_no_nul s,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12,s13,s14,s15,s16,s17,s18,s19,s20,s21,s22,s23,s24,s25,s26,s27,s28,s29,s30,s31,s32,s33,s34,s35,s36,s37,s38,s39,s40,s41,s42,s43,s44,s45,s46,s47,s48,s49,s50,s51,s52,s53,s54,s55,s56,s57,s58,s59,s60,s61,s62,s63,s64,s65,s66,s67,s68,s69,s70,s71,s72,s73,s74,s75,s76,s77,s78,s79,s80,s81,s82,s83,s84,s85,s86,s87,s88,s89,s90,s91,s92,s93,s94,s95,s96,s97,s98,s99,s100,s101,s102,s103,s104,s105,s106,s107,s108,s109,s110,s111,s112,s113,s114,s115,s116,s117,s118,s119,s120,s121,s122,s123,s124,s125,s126,s127,s128,s129,s130,s131,s132,s133,s134,s135,s136,s137,s138,s139,s140,s141,s142,s143,s144,s145,s146,s147,s148,s149,s150,s151,s152,s153,s154,s155,s156,s157,s158,s159,s160,s161,s162,s163,s164,s165,s166,s167,s168,s169,s170,s171,s172,s173,s174,s175,s176,s177,s178,s179,s180,s181,s182,s183,s184,s185,s186,s187,s188,s189,s190,s191,s192,s193,s194,s195,s196,s197,s198,s199,s200,s201,s202,s203,s204,s205,s206,s207,s208,s209,s210,s211,s212,s213,s214,s215,s216,s217,s218,s219,s220,s221,s222,s223,s224,s225,s226,s227,s228,s229,s230,s231,s232,s233,s234,s235,s236,s237,s238,s239,s240,s241,s242,s243,s244,s245,s246,s247,s248,s249,s250,s251,s252,s253,s254,s255,s256,s257,s258,s259,s260,s261,s262,s263,s264,s265,s266,s267,s268,s269,s270,s271,s272,s273,s274,s275,s276,s277,s278,s279,s280,s281,s282,s283,s284,s285,s286,s287,s288,s289,s290,s291,s292,s293,s294,s295,s296,s297,s298,s299
	 jmp console_flush
	.popseg
.endmacro




print_str_:
	sta print_temp_
	
	pla
	sta addr
	pla
	sta addr+1
	
	jsr inc_addr
	jsr print_str_addr
	
	lda print_temp_
	jmp (addr)


; Prints string at addr and leaves addr pointing to
; byte AFTER zero terminator.
;
; Naive algorithm description in pseudo-C would be:
;
;     Print:
;         for(End = false; End == false; )
;         {
;             a = next_byte();
;             Process(a);
;         }
;     Process:
;         if(a == 0) { End = true; }
;         else if(a == 0xFF) { addr = next_word(); }
;         else if(a >= DTE_BEGIN && a <= DTE_END)
;         {
;             Process(DTE_TABLE0(a-DTE_BEGIN));
;             Process(DTE_TABLE1(a-DTE_BEGIN));
;         }
;         else { putchar(a); }
;
; For optimization, first, the second Process() recursion
; was converted into a tail-call.
;
;     Print:
;         for(End = false; End == false; )
;         {
;             a = next_byte();
;             Process(a);
;         }
;     Process:
;         for(;;)
;         {
;             if(a == 0) { End = true; break; }
;             if(a == 0xFF) { addr = next_word(); break; }
;             if(a >= DTE_BEGIN && a <= DTE_END)
;             {
;                 Process(DTE_TABLE0(a-DTE_BEGIN)); // Process the first byte
;                 a = DTE_TABLE1(a-DTE_BEGIN); // Then process the second byte
;             }
;             else { putchar(a); break; }
;         }
;
; As next part of optimization, the recursion was converted into a value stack:
;
;     Print:
;         for(End = false; End == false; )
;         {
;             if(stack.empty())
;                 a = next_byte();
;             else
;                 a = stack.pop(); // Now handle the next deferred byte. (LIFO)
;             for(;;)
;             {
;                 if(a == 0) { End = true; break; }
;                 if(a == 0xFF) { addr = next_word(); break; }
;                 if(a >= DTE_BEGIN && a <= DTE_END)
;                 {
;                     stack.push( DTE_TABLE1(a-DTE_BEGIN) ); // Process the second byte later
;                     a = DTE_TABLE0(a-DTE_BEGIN); // Process the first byte now 
;                 }
;                 else { putchar(a); break; }
;             }
;         }
;
; This assembler function just uses the hardware stack. Using a value stack
; instead of a function call stack means that a recursion of depth 20 takes
; only 20 bytes of stack space rather than 40 bytes.
; Note that a 0 occurring in the middle of all recursions can exit the
; function before the stack is empty. Rationally, no DTE pair has 0 as
; the first byte.
;
; Preserved: A, X, Y
print_str_addr:
	zp_byte @s_level
	pha ; save A
	tya
	pha ; save Y
	txa
	pha ; save X
	tsx
	stx @s_level ; determine S for outermost recursion level
@loop:	jsr @next_byte
@process_char:
	beq @end
	cmp #DTE_BEGIN
	bcs @maybe_dte
.if DTE_END+1 = $FF
    @process_non_dte_byte:
	cmp #$FF
	beq @changed
.endif
	jsr print_char
@done_byte:
	tsx
	cpx @s_level ; determine recursion level
	bcs @loop
	; If stuff was pushed to stack, pop byte
	; before continuing with addr-reading loop.
	pla
	bcc @process_char ;always taken.
@maybe_dte:
	cmp #DTE_END+1
	.if DTE_END+1 = $FF
	beq @changed
	.else
	bcs @process_non_dte_byte
	.endif
	tay
	lda DTE_TABLE1-DTE_BEGIN,y
	pha               ; put the second char to stack.
	lda DTE_TABLE0-DTE_BEGIN,y
	bne @process_char ; process first char (which is never zero)
@changed:
	; Jump to another string.
	; The next two bytes will be the new address.
	jsr @next_byte
	pha
	 jsr @next_byte
	 sta addr+1
	pla
	sta addr+0
	jmp @done_byte
@next_byte:
	ldy #0
	lda (addr),y
	jsr inc_addr
	ora #0
	rts
@end:
	pla
	tax
	pla
	tay
	pla
	rts

; Increments 16-bit value in addr.
; Preserved: A, X, Y
inc_addr:
	inc addr
	beq :+
	rts
:       inc addr+1
	rts

.ifdef NEED_PRINT_DEC16

.pushseg
.segment "RODATA"
	; >= 60000 ? (EA60)
	; >= 50000 ? (C350)
	; >= 40000 ? (9C40)
	; >= 30000 ? (7530)
	; >= 20000 ? (4E20)
	; >= 10000 ? (2710)
digit10000_hi: .byte $00,$27,$4E,$75,$9C,$C3,$EA
digit10000_lo: .byte $00,$10,$20,$30,$40,$50,$60
	; >= 9000 ? (2328 (hex))
	; >= 8000 ? (1F40 (hex))
	; >= 7000 ? (1B58 (hex))
	; >= 6000 ? (1770 (hex))
	; >= 5000 ? (1388 (hex))
	; >= 4000 ? (FA0 (hex))
	; >= 3000 ? (BB8 (hex))
	; >= 2000 ? (7D0 (hex))
	; >= 1000 ? (3E8 (hex))
digit1000_hi: .byte $00,$03,$07,$0B,$0F,$13,$17,$1B,$1F,$23
digit1000_lo: .byte $00,$E8,$D0,$B8,$A0,$88,$70,$58,$40,$28
; >= 900 ? (384 (hex))
; >= 800 ? (320 (hex))
; >= 700 ? (2BC (hex))
; >= 600 ? (258 (hex))
; >= 500 ? (1F4 (hex))
; >= 400 ? (190 (hex))
; >= 300 ? (12C (hex))
; >= 200 ? (C8 (hex))
; >= 100 ? (64 (hex))
digit100_hi: .byte $00,$00,$00,$01,$01,$01,$02,$02,$03,$03
digit100_lo: .byte $00,$64,$C8,$2C,$90,$F4,$58,$BC,$20,$84
.popseg

.macro dec16_comparew table_hi, table_lo
	.local @lt
	cmp table_hi,y
	bcc @lt
	bne @lt ; only test the lo-part if hi-part is equal
	pha
	 txa
	 cmp table_lo,y
	pla
@lt:
.endmacro
.macro do_digit table_hi, table_lo
	pha
	 ; print Y as digit; put X in A and do SEC for subtraction
	 jsr @print_dec16_helper
	 sbc table_lo,y
	 tax
	pla
	sbc table_hi,y
.endmacro

; Prints A:X as 2-5 digit decimal value, NO space after.
; A = high 8 bits, X = low 8 bits.
print_dec16:
	ora #0
	beq @less_than_256

	ldy #6
	sty print_temp_

	; TODO: Use binary search?
:	dec16_comparew digit10000_hi,digit10000_lo
	bcs @got10000
	dey
	bne :-
	;cpy print_temp_
	;beq @got10000
@cont_1000:
	ldy #9
:	dec16_comparew digit1000_hi,digit1000_lo
	bcs @got1000
	dey
	bne :-		; Y = 0.
	cpy print_temp_ ; zero print_temp_ = print zero-digits
	beq @got1000
@cont_100:
	ldy #9
:	dec16_comparew digit100_hi,digit100_lo
	bcs @got100
	dey
	bne :-
	cpy print_temp_
	beq @got100
@got10000:
	do_digit digit10000_hi,digit10000_lo
	; value is now 0000..9999
	ldy #0
	sty print_temp_
	beq @cont_1000
@got1000:
	do_digit digit1000_hi,digit1000_lo
	; value is now 000..999
	ldy #0
	sty print_temp_
	beq @cont_100
@got100:
	do_digit digit100_hi,digit100_lo
	; value is now 00..99
	txa
	jmp print_dec_00_99
@less_than_256:
	txa
	jmp print_dec
@print_dec16_helper:
	 tya
	 jsr print_digit
	 txa
	 sec
	rts

.endif


; Prints A as 2-3 digit decimal value, NO space after.
; Preserved: Y
print_dec:
	; Hundreds
	cmp #10
	blt print_digit
	cmp #100
	blt print_dec_00_99
	ldx #'0'-1	;DTE_CHARMAP
:       inx
	sbc #100
	bge :-
	adc #100
	jsr print_char_x
	
	; Tens
print_dec_00_99:
	sec
	ldx #'0'-1	;DTE_CHARMAP
:       inx
	sbc #10
	bge :-
	adc #10
	jsr print_char_x
	; Ones
print_digit:
	ora #'0'	;DTE_CHARMAP
	jmp print_char
	; Print a single digit
print_char_x:
	pha
	txa
	jsr print_char
	pla
	rts

; Prints one of two characters based on condition.
; SEC; print_cc bcs,'C','-' prints 'C'.
; Preserved: A, X, Y, flags
.macro print_cc cond,yes,no
	; Avoids labels since they're not local
	; to macros in ca65.
	php
	pha
	cond *+6
	lda #no
	bne *+4
	lda #yes
	jsr print_char
	pla
	plp
.endmacro

