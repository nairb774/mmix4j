# 1 "main.cc"
! mmixal:= 8H LOC Data_Section
	.text ! mmixal:= 9H LOC 8B
	.p2align 2
	LOC @+(4-@)&3
	.global cpIntArray
cpIntArray	IS @
	SETL $4,0
	JMP L:2
L:3	IS @
	LDT $3,$1,$4
	STTU $3,$0,$4
	SUBU $2,$2,1
	SLU $2,$2,32
	SR $2,$2,32
	ADDU $4,$4,4
L:2	IS @
	PBP $2,L:3
	POP 0,0

	.section	.rodata
	.p2align 2
	LOC @+(4-@)&3
LC:0	IS @
	BYTE "Enter integer ",#5b,"%d to terminate",#5d," : ",#0
	.p2align 2
	LOC @+(4-@)&3
LC:1	IS @
	BYTE "%d",#0
	.p2align 2
	LOC @+(4-@)&3
LC:2	IS @
	BYTE "array is full",#0
	.text ! mmixal:= 9H LOC 8B
	.p2align 2
	LOC @+(4-@)&3
	.global getIntArray
getIntArray	IS @
	SUBU $254,$254,8
	GET $9,rJ
	SET $3,$0
	SETL $0,0
	GETA $8,printf
	ADDU $4,$254,4
	GETA $7,scanf
	GETA $6,puts
	SET $5,$1
L:11	IS @
	GETA $11,LC:0
	SET $12,$2
	PUSHGO $10,$8,0
	GETA $11,LC:1
	SET $12,$4
	PUSHGO $10,$7,0
	PUT rJ,$9
	LDT $11,$4,0
	SLU $10,$11,32
	SR $10,$10,32
	CMP $10,$10,$2
	BZ $10,L:7
	SLU $10,$0,32
	SR $10,$10,32
	CMP $10,$10,$1
	PBNZ $10,L:8
	GETA $11,LC:2
	PUSHGO $10,$6,0
	SET $0,$5
	JMP L:11
L:8	IS @
	SLU $10,$0,32
	SR $10,$10,32
	SLU $10,$10,2
	STTU $11,$3,$10
	ADDU $0,$0,1
	JMP L:11
L:7	IS @
	ADDU $254,$254,8
	POP 1,0

	.section	.rodata
	.p2align 2
	LOC @+(4-@)&3
LC:3	IS @
	BYTE #9,"%d ",#0
	.text ! mmixal:= 9H LOC 8B
	.p2align 2
	LOC @+(4-@)&3
	.global printIntArray
printIntArray	IS @
	GET $4,rJ
	SETL $2,0
	GETA $7,printf
	SETL $6,#5
	GETA $5,putchar
	JMP L:14
L:16	IS @
	ADDU $3,$2,1
	SET $2,$3
	LDT $8,$0,0
	SLU $11,$8,32
	GETA $10,LC:3
	SR $11,$11,32
	PUSHGO $9,$7,0
	SLU $9,$3,32
	SR $9,$9,32
	SET $3,$9
	SET $8,$6
	NEGU $9,0,$8
	CSN $8,$8,$9
	NEGU $255,0,$3
	CSN $3,$3,$255
	DIVU $3,$3,$8
	GET $9,rR
	NEGU $8,0,$9
	CSNN $9,$255,$8
	PBNZ $9,L:15
	SETL $10,#a
	PUSHGO $9,$5,0
L:15	IS @
	ADDU $0,$0,4
L:14	IS @
	SLU $9,$2,32
	SR $9,$9,32
	CMP $9,$9,$1
	PBN $9,L:16
	SETL $10,#a
	PUSHJ $9,putchar
	PUT rJ,$4
	POP 0,0

	.section	.rodata
	.p2align 2
	LOC @+(4-@)&3
LC:4	IS @
	BYTE "Read the x array:",#0
	.p2align 2
	LOC @+(4-@)&3
LC:5	IS @
	BYTE "The x array is:",#0
	.p2align 2
	LOC @+(4-@)&3
LC:6	IS @
	BYTE "Read the y array:",#0
	.p2align 2
	LOC @+(4-@)&3
LC:7	IS @
	BYTE "The y array is:",#0
	.p2align 2
	LOC @+(4-@)&3
LC:8	IS @
	BYTE "Printing x after having copied 4 elements",#a,"from y starting at"
	BYTE " y",#5b,"3",#5d," into x starting at x",#5b,"2",#5d,#0
	.text ! mmixal:= 9H LOC 8B
	.p2align 2
	LOC @+(4-@)&3
	.global main
main	IS @
	SUBU $254,$254,64
	GET $6,rJ
	GETA $2,puts
	GETA $8,LC:4
	PUSHGO $7,$2,0
	ADDU $0,$254,32
	GETA $5,getIntArray
	SET $8,$0
	SETL $9,#8
	SETL $10,0
	PUSHGO $7,$5,0
	SET $4,$7
	GETA $8,LC:5
	PUSHGO $7,$2,0
	SLU $9,$4,32
	GETA $3,printIntArray
	SET $8,$0
	SR $9,$9,32
	PUSHGO $7,$3,0
	GETA $8,LC:6
	PUSHGO $7,$2,0
	SET $8,$254
	SETL $9,#8
	SETL $10,0
	PUSHGO $7,$5,0
	SET $5,$7
	GETA $8,LC:7
	PUSHGO $7,$2,0
	SLU $9,$5,32
	SET $8,$254
	SR $9,$9,32
	PUSHGO $7,$3,0
	LDT $5,$254,12
	STTU $5,$0,8
	LDT $5,$254,16
	STTU $5,$0,12
	LDT $5,$254,20
	STTU $5,$0,16
	LDT $1,$254,24
	STTU $1,$0,20
	GETA $8,LC:8
	PUSHGO $7,$2,0
	SLU $4,$4,32
	SET $8,$0
	SR $9,$4,32
	PUSHGO $7,$3,0
	PUT rJ,$6
	ADDU $254,$254,64
	POP 1,0

	.data ! mmixal:= 8H LOC 9B
