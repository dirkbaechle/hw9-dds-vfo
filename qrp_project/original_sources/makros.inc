LD1      MACRO     DEST,OP        ; DEST:=OP
         MOVF      OP,W
         MOVWF     DEST
         ENDM

MOVLF	MACRO	KL,DEST
	MOVLW	KL
	MOVWF	DEST
	ENDM
;
ADD1     MACRO     DEST,OP        ; DEST:=DEST+OP
         MOVF      OP,W
         ADDWF     DEST
         ENDM
;
SUB1     MACRO     DEST,OP        ; DEST:=DEST-OP
         MOVF      OP,W
         SUBWF     DEST
         ENDM
;
NSUB1    MACRO     DEST,OP        ; DEST:=OP-DEST
         MOVF      DEST,W
         SUBWF     OP,W
         MOVWF     DEST
         ENDM
;
CMP1     MACRO     DEST,OP        ; W:=DEST-OP
         MOVF      OP,W
         SUBWF     DEST,W
         ENDM
;
LDK1     MACRO     DEST,KL        ; DEST:=KL
         MOVLW     KL
         MOVWF     DEST
         ENDM
;
ADDK1    MACRO     DEST,KL        ; DEST:=DEST+KL
         MOVLW     KL
         ADDWF     DEST
         ENDM
;
SUBK1    MACRO     DEST,KL        ; DEST:=DEST-KL
         MOVLW     KL
         SUBWF     DEST
         ENDM
;
NSUBK1   MACRO     DEST,KL        ; DEST:=KL-DEST
         MOVLW     DEST,W
         SUBLW     DEST            ;
         ENDM
;
CMPK1    MACRO     DEST,KL        ; W:= DEST-W
         MOVLW     KL
         SUBWF     DEST,W
         ENDM
;
CLR2     MACRO     DEST
         CLRF      DEST
         CLRF      DEST+1
         ENDM

LD2      MACRO     DEST,OP
         MOVF      OP+1,W
         MOVWF     DEST+1
         MOVF      OP,W
         MOVWF     DEST
         ENDM
;
DEC2     MACRO     DEST
         DECF      DEST
         INCF      DEST,W
	 SKPNZ
         DECF      DEST+1
         SKPNZ
	 MOVF	   DEST,F
	 ENDM

DEC4     MACRO	DEST
	LOCAL	DEC41
         DECF	DEST
         INCF	DEST,W
         SKPZ
	goto	DEC41
         DECF	DEST+1
         INCF	DEST+1,W
         SKPZ
	goto	DEC41
         DECF	DEST+2
         INCF	DEST+2,W
         SKPNZ
         DECF	DEST+3
DEC41:
         ENDM

DEC5     MACRO	DEST
	LOCAL	DEC51
         DECF	DEST
         INCF	DEST,W
         SKPZ
	goto	DEC51
         DECF	DEST+1
         INCF	DEST+1,W
         SKPZ
	goto	DEC51
         DECF	DEST+2
         INCF	DEST+2,W
         SKPZ
	goto	DEC51
         DECF	DEST+3
         INCF	DEST+3,W
         SKPNZ
         DECF	DEST+4
DEC51:
         ENDM
;
INC2     MACRO     DEST
         INCF      DEST
         SKPNZ
         INCF      DEST+1
         ENDM

INC3     MACRO     DEST
	 INC2      DEST
         SKPNZ
         INCF      DEST+2
         ENDM

INC4     MACRO     DEST
	 INC3      DEST
         SKPNZ
         INCF      DEST+3
         ENDM

INC5     MACRO     DEST
	 INC4      DEST
         SKPNZ
         INCF      DEST+4
         ENDM

;
ADD2     MACRO     DEST,OP
         MOVF      OP,W
         ADDWF     DEST
         MOVF      OP+1,W
         SKPNC
         INCFSZ    OP+1,W
         ADDWF     DEST+1
         ENDM
;
SUB2     MACRO     DEST,OP        ; Low Byte first
         MOVF      OP,W
         SUBWF     DEST
         MOVF      OP+1,W
         SKPC
         INCFSZ    OP+1,W
         SUBWF     DEST+1
         ENDM

NEG1	MACRO	DEST
	COMF	DEST
	INCF	DEST
	ENDM

NEG2	MACRO	DEST
	COMF	DEST
	COMF	DEST+1
	INC2	DEST
	ENDM

NEG3	MACRO	DEST
	COMF	DEST
	COMF	DEST+1
	COMF	DEST+2
	INC3	DEST
	ENDM

NEG4	MACRO	DEST
	COMF	DEST
	COMF	DEST+1
	COMF	DEST+2
	COMF	DEST+3
	INC4	DEST
	ENDM

NSUB2    MACRO     DEST,OP        ; Low Byte first
         MOVF      DEST,W
         SUBWF     OP,W
         MOVWF     DEST
         MOVF      DEST+1,W
         SKPC
         INCFSZ    DEST+1,W
         SUBWF     OP+1,W
         MOVWF     DEST+1
         ENDM
;
CMP2     MACRO     DEST,OP        ; Low Byte first
         LOCAL     CEND
         MOVF      OP+1,W
         SUBWF     DEST+1,W
         SKPZ
         GOTO      CEND
         MOVF      OP,W
         SUBWF     DEST,W
CEND     NOP
         ENDM

LDK2     MACRO     DEST,KH,KL
         MOVLW     KH
         MOVWF     DEST+1
         MOVLW     KL
         MOVWF     DEST
         ENDM
;
LDK4     MACRO     DEST,KH1,KL1,KH2,KL2
         MOVLW     KH1
         MOVWF     DEST+3
         MOVLW     KL1
         MOVWF     DEST+2
         MOVLW     KH2
         MOVWF     DEST+1
         MOVLW     KL2
         MOVWF     DEST
         ENDM
;
RL2      MACRO     DEST
         RLF       DEST
         RLF       DEST+1
         ENDM
;
RR2      MACRO     DEST
         RRF       DEST+1
         RRF       DEST
         ENDM
;
CLR3     MACRO     DEST
         CLRF      DEST
         CLRF      DEST+1
         CLRF      DEST+2
         ENDM

LD3      MACRO     DEST,OP
         LD2       DEST,OP
         MOVF      OP+2,W
         MOVWF     DEST+2
         ENDM

LD4      MACRO     DEST,OP
         LD3       DEST,OP
         MOVF      OP+3,W
         MOVWF     DEST+3
         ENDM

LD5      MACRO     DEST,OP
         LD4       DEST,OP
         MOVF      OP+4,W
         MOVWF     DEST+4
         ENDM

LD6      MACRO     DEST,OP
         LD5       DEST,OP
         MOVF      OP+5,W
         MOVWF     DEST+5
         ENDM

LD7      MACRO     DEST,OP
         LD6       DEST,OP
         MOVF      OP+6,W
         MOVWF     DEST+6
         ENDM

LD8      MACRO     DEST,OP
         LD7       DEST,OP
         MOVF      OP+7,W
         MOVWF     DEST+7
         ENDM

;
ADD3     MACRO     DEST,OP
         ADD2      DEST,OP
         MOVF      OP+2,W
         SKPNC
         INCFSZ    OP+2,W
         ADDWF     DEST+2
         ENDM
;
ADD4     MACRO     DEST,OP
         ADD3      DEST,OP
         MOVF      OP+3,W
         SKPNC
         INCFSZ    OP+3,W
         ADDWF     DEST+3
         ENDM
;
SUB3     MACRO     DEST,OP        ; Low Byte first
         SUB2      DEST,OP
         MOVF      OP+2,W
         SKPC
         INCFSZ    OP+2,W
         SUBWF     DEST+2
         ENDM
;
SUB4     MACRO     DEST,OP        ; Low Byte first
	 SUB3	   DEST,OP
         MOVF      OP+3,W
         SKPC
         INCFSZ    OP+3,W
         SUBWF     DEST+3
         ENDM
;
SUB5     MACRO     DEST,OP        ; Low Byte first
	 SUB4	   DEST,OP
         MOVF      OP+4,W
         SKPC
         INCFSZ    OP+4,W
         SUBWF     DEST+4
         ENDM
;
SUB6     MACRO     DEST,OP        ; Low Byte first
	 SUB5	   DEST,OP
         MOVF      OP+5,W
         SKPC
         INCFSZ    OP+5,W
         SUBWF     DEST+5
         ENDM
;
SUB7     MACRO     DEST,OP        ; Low Byte first
	 SUB6	   DEST,OP
         MOVF      OP+6,W
         SKPC
         INCFSZ    OP+6,W
         SUBWF     DEST+6
         ENDM
;
SUB8     MACRO     DEST,OP        ; Low Byte first
	 SUB7	   DEST,OP
         MOVF      OP+7,W
         SKPC
         INCFSZ    OP+7,W
         SUBWF     DEST+7
         ENDM
;
NSUB3    MACRO     DEST,OP        ; Low Byte first
         NSUB2     DEST,OP
         MOVF      DEST+2,W
         SKPC
         INCFSZ    DEST+2,W
         SUBWF     OP+2,W
         MOVWF     DEST+2
         ENDM
;
CMP3     MACRO     DEST,OP
         LOCAL     C2END
	 MOVF	   OP+2,W
	 SUBWF	   DEST+2,W
	 SKPZ
	 GOTO	   C2END
         CMP2      DEST,OP
C2END    NOP
         ENDM

CMP4     MACRO     DEST,OP
         LOCAL     C3END
	 MOVF	   OP+3,W
	 SUBWF	   DEST+3,W
	 SKPZ
	 GOTO	   C3END
         CMP3      DEST,OP
C3END    NOP
         ENDM

CMP5     MACRO     DEST,OP
         LOCAL     C4END
	 MOVF	   OP+4,W
	 SUBWF	   DEST+4,W
	 SKPZ
	 GOTO	   C4END
         CMP4      DEST,OP
C4END    NOP
         ENDM

CMP6     MACRO     DEST,OP
         LOCAL     C5END
	 MOVF	   OP+5,W
	 SUBWF	   DEST+5,W
	 SKPZ
	 GOTO	   C5END
         CMP5      DEST,OP
C5END    NOP
         ENDM

CMP7     MACRO     DEST,OP
         LOCAL     C6END
	 MOVF	   OP+6,W
	 SUBWF	   DEST+6,W
	 SKPZ
	 GOTO	   C6END
         CMP6      DEST,OP
C6END    NOP
         ENDM

CMP8     MACRO     DEST,OP
         LOCAL     C7END
	 MOVF	   OP+7,W
	 SUBWF	   DEST+7,W
	 SKPZ
	 GOTO	   C7END
         CMP7      DEST,OP
C7END    NOP
         ENDM

RL3      MACRO     DEST
         RLF       DEST
         RLF       DEST+1
         RLF       DEST+2
         ENDM
;
RR3      MACRO     DEST
         RRF       DEST+2
         RRF       DEST+1
         RRF       DEST
         ENDM
;
CLR4     MACRO     DEST
         CLRF       DEST
         CLRF       DEST+1
         CLRF       DEST+2
         CLRF       DEST+3
         ENDM
;
CLR8     MACRO     DEST
         CLRF       DEST
         CLRF       DEST+1
         CLRF       DEST+2
         CLRF       DEST+3
         CLRF       DEST+4
         CLRF       DEST+5
         CLRF       DEST+6
         CLRF       DEST+7
         ENDM

RL4      MACRO     DEST
         RLF       DEST
         RLF       DEST+1
         RLF       DEST+2
         RLF       DEST+3
         ENDM
;
RL8      MACRO     DEST
         RLF       DEST
         RLF       DEST+1
         RLF       DEST+2
         RLF       DEST+3
         RLF       DEST+4
         RLF       DEST+5
         RLF       DEST+6
         RLF       DEST+7
         ENDM

RR4      MACRO     DEST
         RRF       DEST+3
         RRF       DEST+2
         RRF       DEST+1
         RRF       DEST
         ENDM
;
RR8      MACRO     DEST
         RRF       DEST+7
         RRF       DEST+6
         RRF       DEST+5
         RRF       DEST+4
         RRF       DEST+3
         RRF       DEST+2
         RRF       DEST+1
         RRF       DEST
         ENDM

ADDK4    MACRO     DEST,KH2,KL2,KH1,KL1
         MOVLW     KL1
         ADDWF     DEST
         SKPNC
         INCF      DEST+1
         MOVLW     KH1
         ADDWF     DEST+1
         SKPNC
         INCF      DEST+2
         MOVLW     KL2
         ADDWF     DEST+2
         SKPNC
         INCF      DEST+3
         MOVLW     KH2
         ADDWF     DEST+3
         ENDM

BMUL10	MACRO	DEST
	clrf	DEST+1
	movf	DEST,W
	clrc
	RL2	DEST
	clrc
	RL2	DEST
	addwf	DEST
	SKPNC
	incf	DEST+1,F
	clrc
	RL2	DEST
	ENDM
