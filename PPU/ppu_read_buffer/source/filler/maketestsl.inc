%if 0
MakeTestSL:
	mov edi, NTSCline
	cld
	%macro mmm 2
	 mov ecx, %1
	 call %2 + 4
	%endmacro

    mmm 15*8, .w0

    mmm 64, .w0
    mmm 8, .w1
    mmm 1, .w2
	xor si,si
.loo3:
    mmm 6, .w3
    mmm 6, .w2
	inc si
	cmp si,9
	jb .loo3
    mmm 6, .w3
    mmm 5, .w2
    mmm 8, .w1
    mmm 104, .w0
    mmm 1, .w1
	xor si,si
.loo26:
    mmm 6, .w4
    mmm 6, .w1
	inc si
	cmp si,7
	jb .loo26
    mmm 6, .w4
    mmm 5, .w1
	xor si,si
.loo42:
    mmm 6, .w4
    mmm 6, .w1
	inc si
	cmp si,10
	jb .loo42
    mmm 6, .w4
    mmm 5, .w1
	xor si,si
.loo64:
    mmm 6, .w4
    mmm 6, .w1
	inc si
	cmp si,9
	jb .loo64
    mmm 6, .w4
    mmm 5, .w1
	xor si,si
.loo84:
    mmm 6, .w4
    mmm 6, .w1
	inc si
	cmp si,10
	jb .loo84
    mmm 6, .w4
    mmm 5, .w1
	xor si,si
.loo106:
    mmm 6, .w4
    mmm 6, .w1
	inc si
	cmp si,10
	jb .loo106
    mmm 5, .w4
	xor si,si
.loo127:
    mmm 6, .w1
    mmm 6, .w4
	inc si
	cmp si,10
	jb .loo127
    mmm 6, .w1
    mmm 5, .w4
	xor si,si
.loo149:
    mmm 6, .w1
    mmm 6, .w4
	inc si
	cmp si,9
	jb .loo149
    mmm 6, .w1
    mmm 5, .w4
	xor si,si
.loo169:
    mmm 6, .w1
    mmm 6, .w4
	inc si
	cmp si,10
	jb .loo169
    mmm 5, .w1
	xor si,si
.loo190:
    mmm 6, .w4
    mmm 6, .w1
	inc si
	cmp si,10
	jb .loo190
    mmm 6, .w4
    mmm 5, .w1
	xor si,si
.loo212:
    mmm 6, .w4
    mmm 6, .w1
	inc si
	cmp si,9
	jb .loo212
    mmm 6, .w4
    mmm 5, .w1
	xor si,si
.loo232:
    mmm 6, .w4
    mmm 6, .w1
	inc si
	cmp si,10
	jb .loo232
    mmm 5, .w4
	xor si,si
.loo253:
    mmm 6, .w1
    mmm 6, .w4
	inc si
	cmp si,8
	jb .loo253
    mmm 104, .w0
    mmm 8, .w1
    mmm 4, .w3
	xor si,si
.loo272:
    mmm 6, .w2
    mmm 6, .w3
	inc si
	cmp si,9
	jb .loo272
    mmm 6, .w2
    mmm 2, .w3
    mmm 8, .w1
    mmm 56, .w0

    mmm 11*8, .w0
ret
.w0: dd 0.033241
	mov eax, [.w0]
	rep stosd
	ret
.w1: dd 0.083333
	mov eax, [.w1]
	rep stosd
	ret
.w2: dd 0.057018
	mov eax, [.w2]
	rep stosd
	ret
.w3: dd 0.000000
	mov eax, [.w3]
	rep stosd
	ret
.w4: dd 0.025623
	mov eax, [.w4]
	rep stosd
%endif
	ret

