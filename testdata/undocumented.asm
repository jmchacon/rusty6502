	ORG $C000
  TESTLOC EQU $1300
  VAL EQU $55

  ;; These aren't comprehensive WRT flags. Mostly checking the flag state that should correspond to final A or documented side effects.
	PHA
	LDA #$71
	ALR #VAL
	BEQ * ; Check Z is clear
	BCC * ; Check C is set
	BMI * ; Check N is clear
	CMP #$28 ; Verify expected value
	BNE *	; Loop if bad
	ANC #$20
	BEQ *	; Check Z is clear
	BCS * ; Make sure C cleared
	BMI * ; Check N is clear
	CMP #$20 ; Make sure the right value
	BNE *   ; Loop if bad
	SEC   ; Reset for other opcode variant
	ANC #$40
	BNE * 	; Check Z is set
	BCS *	; Make sure C cleared
	BMI *	; Check N is clear
	CLD	; Tests for ARR in non decimal first
	SEC	; Set carry so it gets rotated in.
	CLV	; And overflow
	LDA #$C1
	ARR #$55
	BEQ * ; Check Z is clear
	BCS * ; Make sure C cleared
	BPL * ; N should be set
	BVC * ; V should be set
	CMP #$A0 ; Verify expected value
	CLC	; Clear up front since should set this time.
	LDA #$C1
	ARR #$C5
	BEQ * ; Check Z is clear
	BCC * ; Check C got set
	BMI * ; Check N is clear
	BVS * ; Check V is clear
	CMP #$60 ; Verify expected value
	BNE *   ; Loop if bad
	SED	; Decimal version check for ARR
	SEC
	CLV
	LDA #$C5
	ARR #$55
	BEQ * ; Check Z is clear
	BCS * ; Make sure C cleared
	BPL * ; N should be set
	BVC * ; V should be set
	CMP #$A8 ; Should be different in decimal mode
	BNE *   ; Loop if bad
	CLC	; Another pass where we check C,!N,!Z,!V
	LDA #$C5
	ARR #$D5
	BEQ * ; Check Z is clear
	BCC * ; Check C got set
	BMI * ; Check N is clear
	BVS * ; Check V is clear
	CMP #$C8 ; Verify expected value (both halves did fixups).
	BNE *   ; Loop if bad
	TXA
	PHA
	LDA #$B1
	LDX #$F1
	XAA #$55
	BEQ * ; Check Z is clear
	BCC * ; Check C is still set
	BMI * ; Check N is clear
	BVS * ; Check V is still clear
	CMP #$51
	BNE * ; Loop if bad
	TYA
	PHA
	LDY #$FF ; Counter for iterations of OAL
START	LDA #$B1
	LDX #$F1
	OAL #$55
	CMP #$51 ; We ran XAA
	BEQ CONT ; Do the DEY
	CMP #$11 ; Did OAL (A&#) -> A,X
	BNE *	 ; Loop if neither test matched.
	CPX #$11
	BNE *	  ; Loop if neither test matched.
CONT	DEY
	BNE START ; Not done
	LDA #$F0
	STA $FF
	LDA #$12
	STA $00 ; Setup (d),y for FF to point at 12F0
 	STA TESTLOC ; The final addr + Y. Put 12 there for now.
	LDA #$B5
	LDX #$D3
	LDY #$10
	AHX ($FF),Y
	LDA TESTLOC
	CMP #$10
	BNE *
	LDA #$FF
	STA TESTLOC ; Reset for 2nd call using absolute,Y
	LDA #$B5
	AHX $12F0,Y
	LDA TESTLOC
	CMP #$10
	BNE *
	TSX ; Need to save S since LAS/TAS will change it, but can't use stack
	STX $01
	LDA #$FF
	STA TESTLOC ; Reset for TAS
	LDX #$D3	 ; Reset X as before
	LDA #$B5
	TAS $12F0,Y
	LDA TESTLOC
	CMP #$10
	BNE *
	TSX
	CPX #$91 ; What S changed to. Can't compare against stashed value, stack might be that?
	BNE *
	TXA
	AND #$10
	STA $02 ; precompute expected value from LAS for all regs since it's S&val (from 1300) and we know S
	LDA #$FF
	LDX #$FF
	LAS $12F0,Y
	CMP $02
	BNE * ; Check A
	CPX $02
	BNE * ; Check X
	TSX
	CPX $02
	BNE * ; Check S
	LDX $01
	TXS ; Restore S
	LDA #$FF
	STA TESTLOC ; Reset for SHY
	LDA #$B5
	LDX #$10
	LDY #$D3 ; Use same values as before but swap regs
	SHY $12F0,X
	LDA TESTLOC
	CMP #$10
	BNE *
	LDA #$FF
	STA TESTLOC ; Reset for SHX
	LDA #$B5
	LDX #$D3
	LDY #$10 ; Use same values as before but swap regs
	SHX $12F0,Y
	LDA TESTLOC
	CMP #$10
	BNE *
	BEQ * ; We're done
