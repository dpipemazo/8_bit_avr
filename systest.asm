; This is the testcode for Homework #5.  It is in AVR assembly so
; that it will be easy to generate the actual testcode.  It was assembled
; targetting an ATMega16.
;
;
; Revision History
;     5/11/00  Glen George	Initial revision (from 4/23/00 version of 
;                               alutest.asm and 5/11/00 version of
;                               memtest.asm).
;     5/13/00  Glen George	Fixed mistakes in BST instructions (wrong
;                               registers).  Fixed some flag and register
;                               value problems.
;     7/27/00  Glen George	Added instructions for Homework #5 (jumps,
;                               calls, etc.).
;     5/16/04  Glen George	Added more testing and updated comments.


Start:					; start of the test code


					;OpA OpB Res    Flags
	BCLR	0			; XX  XX  --  -------0
	BCLR	7			; XX  XX  --  0------0
	BCLR	4			; XX  XX  --  0--0---0
	BCLR	3			; XX  XX  --  0--00--0
	BCLR	1			; XX  XX  --  0--00-00
	BCLR	5			; XX  XX  --  0-000-00
	BCLR	2			; XX  XX  --  0-000000
	BCLR	6			; XX  XX  --  00000000

					;OpA OpB Res    Flags
	BSET	4			; XX  XX  --  00010000
	BSET	1			; XX  XX  --  00010010
	BSET	7			; XX  XX  --  10010010
	BSET	0			; XX  XX  --  10010011
	BSET	2			; XX  XX  --  10010111
	BSET	6			; XX  XX  --  11010111
	BSET	5			; XX  XX  --  11110111
	BSET	3			; XX  XX  --  11111111

	LDI	R16, 0			; need a bunch of 0 values
	MOV	R0, R16
	MOV	R1, R16
	MOV	R2, R16
	MOV	R3, R16
	MOV	R4, R16
	MOV	R5, R16
	MOV	R6, R16
	MOV	R7, R16

					;OpA OpB Res    Flags
	BLD	R0, 7			; 00  XX  80  11111111
	BLD	R1, 3			; 00  XX  08  11111111
	BLD	R2, 1			; 00  XX  02  11111111
	BLD	R3, 6			; 00  XX  40  11111111
	BLD	R4, 0			; 00  XX  01  11111111
	BLD	R5, 5			; 00  XX  20  11111111
	BLD	R6, 4			; 00  XX  10  11111111
	BLD	R7, 2			; 00  XX  04  11111111

	LDI	R20, 0xDF		; initialize for BST
	MOV	R8, R20
	LDI	R20, 0x04
	MOV	R9, R20
	LDI	R20, 0x7F
	MOV	R10, R20
	LDI	R20, 0x01
	MOV	R11, R20
	LDI	R20, 0xFD
	MOV	R12, R20
	LDI	R20, 0x40
	MOV	R13, R20
	LDI	R20, 0xF7
	MOV	R14, R20
	LDI	R20, 0x10
	MOV	R15, R20
					;OpA OpB Res    Flags
	BST	R8, 5			; DF  XX  --  10111111
	BST	R9, 2			; 04  XX  --  11111111
	BST	R10, 7			; 7F  XX  --  10111111
	BST	R11, 0			; 01  XX  --  11111111
	BST	R12, 1			; FD  XX  --  10111111
	BST	R13, 6			; 40  XX  --  11111111
	BST	R14, 3			; F7  XX  --  10111111
	BST	R15, 4			; 10  XX  --  11111111

	LDI	R16, 0xFF		; initialize for ALU ops
	LDI	R17, 0xFF
	LDI	R18, 0
	LDI	R19, 0x70
	LDI	R20, 0
	LDI	R21, 0x7E
	LDI	R22, 0x7E
	LDI	R23, 0x80
	LDI	R24, 0x45
	LDI	R25, 0x80
	LDI	R26, 0xF0
	LDI	R27, 0xFF
	LDI	R28, 0x55
	LDI	R29, 0xAA
	LDI	R30, 0x70
	LDI	R31, 0x3F

					;OpA OpB Res    Flags
	ADC	R16, R17		; FF  FF  FF  11110101
	ST 	X, R16			; Check Store = FF
	ADC	R16, R18		; FF  00  00  11100011
	ST 	X, R16			; Check Store = 00
	ADC	R18, R17		; 00  FF  00  11100011
	ST 	X, R18			; Check Store = 00
	ADC	R25, R21		; 80  7E  FF  11010100
	ST 	X, R25			; Check Store = FF
	ADC	R22, R0			; 7E  80  FE  11010100
	ST 	X, R22			; Check Store = FE

					;OpA OpB Res    Flags
	ADD	R25, R21		; FF  7E  7D  11100001
	ST 	X, R25			; Check Store = 7D
	ADD	R16, R17		; 00  FF  FF  11010100
	ST 	X, R16			; Check Store = FF
	ADD	R18, R20		; 00  00  00  11000010
	ST 	X, R18			; Check Store = 00
	ADD	R23, R0			; 80  80  00  11011011
	ST 	X, R23			; Check Store = 00

					;OpA OpB Res    Flags
	ADIW    R24, 0x0		; 45  00  45  --------
					; 7D  XX  7D  11000000
	ST 	X, R24			; Check Store = 45
	ST 	X, R25			; Check Store = 7D
	ADIW    R26, 0x10	       	; F0  10  00  --------
					; FF  XX  00  11011011
	ST 	X, R26			; Check Store = 00
	ST 	X, R27			; Check Store = 00
 
					;OpA OpB Res    Flags
	AND	R17, R28		; FF  55  55  11000001
	ST 	X, R17			; Check Store = 55
	AND	R17, R29		; 55  AA  00  11000011
	ST 	X, R17			; Check Store = 00
	AND	R0, R31			; 80  3F  00  11010101
	ST 	X, R0			; Check Store = 00
	BLD	R0, 7			; 00  XX  80  11111111
	

					;OpA OpB Res    Flags
	ANDI	R28, 0xFF		; 55  FF  55  11000001
	ST 	X, R28			; Check Store = 55
	ANDI	R16, 0xFF		; FF  FF  FF  11010101
	ST 	X, R16			; Check Store = FF
	ANDI	R29, 0xFF		; AA  FF  AA  11010101
	ST 	X, R29			; Check Store = AA

					;OpA OpB Res    Flags
	ASR	R16			; FF  XX  FF  11010101
	ST 	X, R16			; Check Store = FF
	ASR	R27			; 00  XX  00  11000010
	ST 	X, R27			; Check Store = 00
	ASR	R30			; 70  XX  38  11000000
	ST 	X, R30			; Check Store = 38
	ASR	R29			; AA  XX  D5  11001100
	ST 	X, R29			; Check Store = D5

					;OpA OpB Res    Flags
	COM	R16			; FF  XX  00  11000011
	ST 	X, R16			; Check Store = 00
	COM	R16			; 00  XX  FF  11010101
	ST 	X, R16			; Check Store = FF
	COM	R28			; 55  XX  AA  11010101
	ST 	X, R28			; Check Store = AA
	COM	R28			; AA  XX  55  11000001
	ST 	X, R28			; Check Store = 55

					;OpA OpB Res    Flags
	CP	R17, R16		; 00  FF  --  11100001
	CP	R21, R26		; 7E  FF  --  11001101
	CP	R31, R3			; 3F  40  --  11010101

					;OpA OpB Res    Flags
	CPC	R17, R16		; 00  FF  --  11000010
	CPC	R21, R17		; 7E  00  --  11000000
	CPC	R17, R21		; 00  7E  --  11110101
	CPC	R21, R16		; 7E  FF  --  11100001

	LDI	R30, 0x40
	LDI	R31, 0x7F
					;OpA OpB Res    Flags
	CPI	R17, 0x7F		; 00  7F  --  11101101
	CPI	R30, 0x70 		; 40  70  --  11010101
	CPI	R31, 0xA0		; 7F  A0  --  11001101

	MOV	R30, R19
					;OpA OpB Res    Flags
	DEC	R17			; 00  XX  FF  11010101
	ST 	X, R17			; Check Store = FF
	DEC	R0			; 80  XX  7F  11011001
	ST 	X, R0			; Check Store = 7F
	DEC	R30			; 70  XX  6F  11000001
	ST 	X, R30			; Check Store = 6F
	DEC	R17			; FF  XX  FE  11010101
	ST 	X, R17			; Check Store = FE

	MOV	R17, R28
					;OpA OpB Res    Flags
	EOR	R17, R29		; 55  AA  FF  11010101
	ST 	X, R17			; Check Store = FF
	EOR	R17, R28		; FF  55  AA  11010101
	ST 	X, R17			; Check Store = AA
	EOR	R18, R17		; 00  AA  AA  11010101
	ST 	X, R18			; Check Store = AA
	EOR	R18, R16		; AA  FF  55  11000001
	ST 	X, R18			; Check Store = 55
	EOR	R24, R24		; 45  45  00  11000011
	ST 	X, R24			; Check Store = 00

					;OpA OpB Res    Flags
	INC	R24			; 00  XX  01  11000001
	ST 	X, R24			; Check Store = 01
	INC	R22			; FE  XX  FF  11000011
	ST 	X, R23			; Check Store = FF
	INC	R22			; FF  XX  00  11000011
	ST 	X, R23			; Check Store = 00
	INC	R0			; 7F  XX  80  11001101
	ST 	X, R0			; Check Store = 80

	DEC	R26
	LDI	R31, 0x80
					;OpA OpB Res    Flags
	LSR	R26			; FF  XX  7F  11011001
	ST 	X, R26			; Check Store = 7F
	LSR	R30			; 70  XX  38  11000000
	ST 	X, R30			; Check Store = 38
	LSR	R20			; 00  XX  00  11000010
	ST 	X, R20			; Check Store = 00
	LSR	R31			; 80  XX  40  11000000
	ST 	X, R31			; Check Store = 40

					;OpA OpB Res    Flags
	NEG	R16			; FF  XX  01  11100001
	ST 	X, R16			; Check Store = 01
	NEG	R22			; 00  XX  00  11000010
	ST 	X, R22			; Check Store = 00
	NEG	R0			; 80  XX  80  11001101
	ST 	X, R0			; Check Store = 80
	NEG	R21			; 7E  XX  82  11110101
	ST 	X, R21			; Check Store = 82

					;OpA OpB Res    Flags
	OR	R18, R17		; 55  AA  FF  11110101
	ST 	X, R18			; Check Store = FF
	OR	R18, R28		; FF  55  FF  11110101
	ST 	X, R18			; Check Store = FF
	OR	R22, R17		; 00  AA  AA  11110101
	ST 	X, R22			; Check Store = AA

					;OpA OpB Res    Flags
	ORI	R17, 0xFF		; AA  FF  FF  11110101
	ST 	X, R17			; Check Store = FF
	ORI	R25, 0x7D		; 7D  7D  7D  11100001
	ST 	X, R25			; Check Store = 7D

					;OpA OpB Res    Flags
	ROR	R17			; FF  XX  FF  11110101
	ST 	X, R17			; Check Store = FF
	ROR	R19			; 70  XX  B8  11001100
	ST 	X, R19			; Check Store = B8
	ROR	R16			; 01  XX  00  11011011
	ST 	X, R16			; Check Store = 00
	ROR	R0 			; 80  XX  C0  11001100
	ST 	X, R0			; Check Store = C0
	ROR	R16			; 00  XX  00  11000010
	ST 	X, R16			; Check Store = 00

	LDI	R31, 0x50
					;OpA OpB Res    Flags
	SBC	R16, R17		; 00  FF  01  11100001
	ST 	X, R16			; Check Store = 01
	SBCI	R31, 0x70		; 50  70  DF  11101101
	ST 	X, R31			; Check Store = DF
	SBC	R10, R20		; 7F  00  7E  11000000
	ST 	X, R10			; Check Store = 7E

	LDI	R25, 0x7F
	LDI	R24, 0x71
					;OpA OpB Res    Flags
	SBCI	R26, 0x7F		; 00  7F  80  11110101
	ST 	X, R26			; Check Store = 80
	SBC	R25, R17		; 7F  FF  7F  11100001
	ST 	X, R25			; Check Store = 7F
	SBCI	R24, 0xA0		; 71  A0  D0  11001101
	ST 	X, R24			; Check Store = D0

	LDI	R24, 0x0D
	LDI	R25, 0
					;OpA OpB Res    Flags
	SBIW    R24, 0x10		; 0D  10  FD  --------
					; 00  XX  FF  11010101
	ST 	X, R24			; Check Store = FD
	ST 	X, R25			; Check Store = FF
	SBIW    R24, 0x0		; FD  00  FD  --------
					; FF  XX  FF  11010100
	ST 	X, R24			; Check Store = FD
	ST 	X, R25			; Check Store = FF

	LDI	R30, 0x7F
	LDI	R31, 0x7F
					;OpA OpB Res    Flags
	SUB	R16, R17		; 01  FF  02  11100001
	ST 	X, R16			; Check Store = 02
	SUB	R30, R17		; 7F  FF  80  11001101
	ST 	X, R30			; Check Store = 80
	SUB	R31, R20		; 7F  00  7F  11000000
	ST 	X, R31			; Check Store = 7F

	LDI	R30, 0x50
	LDI	R31, 0x71
					;OpA OpB Res    Flags
	SUBI	R20, 0x7F		; 00  7F  81  11110101
	ST 	X, R20			; Check Store = 81
	SUBI	R30, 0x70		; 50  70  E0  11010101
	ST 	X, R30			; Check Store = E0
	SUBI	R31, 0xA0		; 71  A0  D1  11001101
	ST 	X, R31			; Check Store = D1

					;OpA OpB Res    Flags
	SWAP	R21			; 82  XX  28  11001101
	ST 	X, R21			; Check Store = 28
	SWAP	R10			; 7E  XX  E7  11001101
	ST 	X, R10			; Check Store = E7
	SWAP	R27			; 00  XX  00  11001101
	ST 	X, R27			; Check Store = 00


	PUSH	R26			; store address registers Push = 80
	PUSH	R27			; Push = 00
	PUSH	R28			; Push = 55
	PUSH	R29			; Push = D5
	PUSH	R30			; Push = E0
	PUSH	R31			; Push = D1

	POP	R0			; pop back into a couple other regs
	POP	R1

					; setup addresses for writing
	LDI	R27, 0xFF		; X = FFFF
	LDI	R26, 0xFF
	LDI	R29, 0xFF		; Y = FFC0
	LDI	R28, 0xC0
	LDI	R31, 0x00		; Z = 0080
	LDI	R30, 0x80

	STS	 0x5555, R0		; write    @ 5555
	STS	 0xAAAA, R1		; write    @ AAAA

	ST	 X, R2			; write    @ FFFF
	ST	-X, R3			; write    @ FFFE
	ST	X+, R4			; write    @ FFFE
	ST	X+, R5			; write    @ FFFF
	ST	 X, R6			; write    @ 0000

	ST	Y+, R7			; write    @ FFC0
	ST	 Y, R8			; write    @ FFC1
	ST	-Y, R9			; write    @ FFC0
	ST	 Y, R10			; write    @ FFC0
	STD	Y + 60, R11		; write    @ FFFC
	STD	Y + 2, R12		; write    @ FFC2
	STD	Y + 22, R13		; write    @ FFD6
	STD	Y + 1, R14		; write    @ FFC1

	ST	Z+, R15			; write    @ 0080
	ST	 Z, R16			; write    @ 0081
	ST	-Z, R17			; write    @ 0080
	ST	 Z, R18			; write    @ 0080
	STD	Z + 30, R19		; write    @ 009E
	STD	Z + 1, R20		; write    @ 0081
	STD	Z + 63, R21		; write    @ 00BF
	STD	Z + 32, R22		; write    @ 00A0

					; setup another address for writing
	LDI	R29, 0xFF		; Y = FFE0
	LDI	R28, 0xE0

	ST	-Y, R23			; write    @ FFDF
	ST	Y+, R24			; write    @ FFDF
	STD	Y + 63, R25		; write    @ 001F


					;setup new addresses for reading
	LDI	R27, 0			; X = 0
	LDI	R26, 0
	LDI	R29, 0xFF		; Y = FFFF
	LDI	R28, 0xFF
	LDI	R31, 0xFF		; Z = FFC0
	LDI	R30, 0xC0

	LDS	R0, 0xAAAA		; read     @ AAAA
	LDS	R1, 0x5555		; read     @ 5555

	LD	R7, X			; read     @ 0000
	LD	R9, -X			; read     @ FFFF
	LD	R20, X+			; read     @ FFFF
	LD	R21, X			; read     @ 0000

	LD	R6, Y+			; read     @ FFFF
	LD	R23, Y			; read     @ 0000
	LD	R22, -Y			; read     @ FFFF
	LDD	R15, Y + 30		; read     @ 001D

	LD	R4, Z+			; read     @ FFC0
	LD	R13, Z			; read     @ FFC1
	LD	R2, -Z			; read     @ FFC0
	LDD	R17, Z + 60		; read     @ FFFC


TestJumps:				; test unconditional jumping

	JMP	JumpTest		; just test jumping
BackRJump:
	LDI	R22, 0x5A
	LDI	R23, 0x5A
	ST 	X, R23			; 2) Check Store = 5A
	RJMP	ForwardRJump		; test a forward RJMP
JumpTest:
	LDI	R24, 0xA5
	ST 	X, R24			; 1) Check Store = A5
	RJMP	BackRJump		; test a backward RJMP
ForwardRJump:
	LDI	R30, LOW(IndirJump)	; finally test an indirect jump
	LDI	R31, HIGH(IndirJump)
	LDI	R16, 0x42
	ST  	X, R16			; 3) Check Store = 42
	IJMP
	LDI	R27, 0			; should skip these instructions
	LDI	R28, 0
	ST 	X, R16			; Should not execute
IndirJump:
	LDI	R16, 0xFF
	ST  	X, R16			; 4) Check Store = FF

TestCalls:				; test subroutine calls
	CALL	Subr1			; direct subroutine call
	RCALL	Subr1			; relative direct subroutine call
	LDI	R30, LOW(Subr1)
	LDI	R31, HIGH(Subr1)
	ICALL				; indirect subroutine call


TestBranches:				; test some conditional branches
	CP	R28, R27
	BRLO	Branch1			; should branch: 0x7F U< 0xFF
	LDI	R16, 0xF1
	ST  	X, R16			; X) Fail if we read this (F1)
	JMP	TestBranches
Branch1:
	LDI	R16, 0xB0
	ST  	X, R16			; 8) Check Store = B0
	BRLT	TestBranches		; should not branch: 0x7F S> 0xFF
	LDI	R16, 0xB1
	ST  	X, R16			; 9) Check Store = B1
	BREQ	TestBranches		; should not branch: 0x7F != 0xFF
	LDI	R16, 0xB2
	ST  	X, R16			; 10) Check Store = B2
	BRNE	Branch2			; should branch: 0x7F != 0xFF
	LDI	R16, 0xF2
	ST  	X, R16			; X) Fail if we read this (F2)
	JMP	TestBranches
Branch2:
	LDI	R16, 0xB3
	ST  	X, R16			; 11) Check Store = B3
	LDI	R21, 0x69
	ADD	R21, R21
	BRHC	TestBranches		; should not branch (HC is set)
	LDI	R16, 0xB4
	ST  	X, R16			; 12) Check Store = B4
	OR	R27, R27		; this is a negative number
	BRMI	Branch3			; should take the branch
	LDI	R16, 0xF3
	ST  	X, R16			; X) Fail if we read this (F3)
	JMP	TestBranches
Branch3:
	LDI	R16, 0xB5
	ST  	X, R16			; 13) Check Store = B5
	OR	R28, R28		; this is a positive number
	BRMI	TestBranches		; should not take the branch
	BRPL	Branch4			; now should take it
	LDI	R16, 0xF4
	ST  	X, R16			; X) Fail if we read this (F4)
	JMP	TestBranches
Branch4:
	LDI	R16, 0xB6
	ST  	X, R16			; 14) Check Store = B6
	OR	R27, R27		; this is a negative number
	BRPL	TestBranches		; should not take the branch
	SUB	R28, R27		; this generates an overflow
	BRVS	Branch5			; so should take the branch
	LDI	R16, 0xF5
	ST  	X, R16			; X) Fail if we read this (F5)
	JMP	TestBranches
Branch5:
	LDI	R16, 0xB7
	ST  	X, R16			; 15) Check Store = B7
	DEC	R28			; 80 - 1 -> 7F => overflow
	BRVC	TestBranches		; should not take the branch
	CPI	R27, 1			; -1 < 1
	BRGE	TestBranches		; so should not take the branch
	CLI				; clear interrupt flag
	BRIE	TestBranches		; so should not take the branch
	CALL	SubrI			; call subroutine that ends with RETI
	BRID	TestBranches		; RETI set I flag, dont branch
	BST	R30, 1			; set the T flag
	BRTC	TestBranches		; so should not branch
	BST	R30, 3			; now clear the T flag
	BRTS	Branch5 		; and still should not branch
	ADD	R30, R30		; R30 is now 0xCC (no carry)
	BRSH	Branch6			; so should take the branch
	JMP	TestBranches
Branch6:
	LDI	R16, 0xB8
	ST  	X, R16			; 17) Check Store = B8
	ADD	R30, R30		; should set the carry and half carry
	BRSH	Branch5 		; should not take branch
	BRHS	TestSkips		; but should take this one
	LDI	R16, 0xF6
	ST  	X, R16			; X) Fail if we read this (F6)
	JMP	TestBranches


TestSkips:				; test skip instructions
	LDI	R16, 0xB9
	ST  	X, R16			; 18) Check Store = B9
	CPSE	R22, R23		; skip a 1 byte instruction
	RJMP    TestSkips
	CPSE	R22, R23		; skip a 2 byte instruction
	JMP	TestSkips
	CPSE	R22, R24		; dont skip
	LDI	R22, 0x80
	SBRC	R22, 6			; should skip a 1 byte instruction
	LDI	R22, 0xFF
	SBRC	R22, 3			; should skip a 2 byte instruction
	JMP	TestSkips
	SBRC	R22, 7			; dont skip
	LDI	R22, 0xA5
	SBRS	R22, 0			; should skip a 1 byte instruction
	LDI	R22, 0
	SBRS	R22, 5			; should skip a 2 byte instruction
	JMP	TestSkips
	SBRS	R22, 1			; dont skip
	JMP	End			; end test over

End:
	LDI	R16, 0x42
	ST  	X, R16			; 19) Check Store = 42
	JMP 	Start

Subr1:					; the subroutine
	LDI	R27, 0xFF
	LDI	R28, 0x7F
	LDI	R29, 0

	LDI	R16, 0xA5
	ST  	X, R16			; 5/6) Check Store = A5
	RET


SubrI:					; subroutine ending with RETI
	LDI	R25, 0xFF
	LDI	R26, 0x7F
	LDI	R30, 0x66

	LDI	R16, 0x36
	ST  	X, R16			; 7/16) Check Store = 36
	RETI
