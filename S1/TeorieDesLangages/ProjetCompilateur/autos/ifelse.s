	.meta source "\"autos/ifelse.auto\""
	.meta fields "[{ \"name\": \"\", \"num\": 0, \"lo\": 0, \"hi\": 1 }]"
	invoke 1, 2, 3
	seti r4, #1
	seti r0, #0
L0:
	seti r1, #0
L1:
	invoke 3, 0, 1
	invoke 5, 23, 6
	set r23, r5
	invoke 5, 22, 2
	set r22, r6
L11:
	set r18, r5
	set r19, r6
	goto_eq L18, r19, r11
	goto L12
	seti r20, #1
	invoke 4, 20, 0
	goto L13
L12:
	seti r21, #0
	invoke 4, 21, 0
L13:
L2:
	set r8, r5
	add r7, r8, r9
	set r9, r6
	seti r10, #2
	goto_ne L7, r10, r2
	goto L3
L5:
	invoke 5, 11, 3
	invoke 5, 12, 5
	goto_lt L11, r12, r5
	goto L6
	seti r13, #2
	invoke 4, 13, 0
	goto L7
L6:
	seti r14, #0
	invoke 4, 14, 0
L7:
	goto L4
L3:
L8:
	invoke 5, 15, 0
	seti r16, #0
	goto_eq L15, r16, r8
	goto L9
	seti r17, #1
	invoke 4, 17, 0
	goto L10
L9:
L10:
L4:
	add r1, r1, r4
	goto_lt L1, r1, r3
	add r0, r0, r4
	goto_lt L0, r0, r2
	stop
