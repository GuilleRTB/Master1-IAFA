	.meta source "\"autos/add2.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 9 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	invoke 5, 20, 0
	add r19, r20, r21
	invoke 5, 21, 6
	set r5, r19
	set r16, r5
	add r15, r16, r17
	invoke 5, 17, 2
	add r14, r15, r18
	set r18, r5
	set r6, r14
	set r10, r5
	add r9, r10, r11
	set r11, r6
	add r8, r9, r12
	invoke 5, 12, 6
	add r7, r8, r13
	invoke 5, 13, 0
	invoke 4, 7, 0
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
