	.meta source "\"autos/add1.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 9 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	invoke 5, 18, 0
	add r17, r18, r19
	invoke 5, 19, 6
	set r17, r5
	set r15, r5
	add r14, r15, r16
	invoke 5, 16, 2
	set r14, r6
	invoke 5, 12, 5
	add r11, r12, r13
	set r13, r5
	set r11, r7
	set r9, r6
	add r8, r9, r10
	set r10, r7
	invoke 4, 8, 0
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
