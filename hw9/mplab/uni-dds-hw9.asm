;##############################################################################
;##############################################################################

;;;;;;; Assembler directives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       list  P=16f876, F=INHX32, C=160, N=0, ST=ON, MM=ON, X=ON

#include        p16f876.inc             ; processor specific variable definitions
#include        makros.inc

        __CONFIG   _CP_OFF & _LVP_OFF & _WDT_OFF & _PWRTE_ON & _HS_OSC

;#define                HWtest          ;Testaufbau Moskita DL4JAL
#define         HW2             ;Leiterplatte fuer Peter
;#define                debug

LCDStr: MACRO   var1
        movlw   HIGH var1       ;Highanteil der Adresse laden
        movwf   lcdtextadrh     ;und in den Ram fuer alle Baenke laden
        movlw   LOW var1        ;die untersten 8 Bit in W laden
        call    LCDString       ;und String ausgeben
        ENDM


;==========================================================
; Variablen:
;==========================================================
                        cblock  70h     ;Bankunabhaengiger Bereich

        w_temp:1                        ;70h variable used for context saving
        status_temp:1                   ;71h variable used for context saving
        pclath_temp:1                   ;72h
        fsr_temp:1                      ;73h
        lcdtextadrh:1                   ;74h Bankunabhaengiger Bereich
        option_mirror:1                 ;75h
        LCDByte:1                       ;76h Zwischenspeicher fuer LCD-Ausgabe
        LCDByte2:1                      ;77h Zwischenspeicher fuer Stringausgabe
        data_ee_addr:1                  ;78h Zwischenspeicher interne Eepromadr
        data_ee_data:1                  ;79h Zwischenspeicher interne Eepromdaten
        utemp:4                         ;7ah,7bh,7ch,7dh
                        endc

                        cblock     020h
        bank0:0
        ramanfang:0

;Variable die fast immer gebraucht werden
;die ersten Ramzellen werden vom Eeprom ueberladen
        flag1:1
        ddskonst:4
        zwischenfrequenz:4
        frequenza:4     ;Frequenz VFO-A
        frequenzb:4     ;Frequenz VFO-B
        xtal80:4        ;Frequenz 80m-XTAL (HW9-Mode)
        
        bconst:4        ; Band-Konstante (BandKonst - VFO = Anzeigefrequenz)
        band:1          ; Band selektion fuer Display
        flag2:1
        flag3:1
        pointer1:1       ;Zeiger fuer indirekt
        pointer2:1       ;Zeiger fuer indirekt
        delay_counter1:1        ;Zellen fuer Delay
        delay_counter2:1
        delay_counter3:1
        schleife:3
        tastennummer:1  ;
        ebene:1 ;Menuebene
        step:1  ;Schrittweite
        impulse:1       ;Drehimpulse
        temp:10 ;temporaere Speicher
        tempkonst1:8    ;DDS rechenspeicher
        tempkonst2:8    ;nicht trennen da gemeinsam clr

        zs1:1
        zs2:1
        zs3:1
        idlecounter:1   ;Zaehler fuer Leerdurchlaeufe ohne Funktion
        uvor:1
        urueck:1

        ramende:0       ;Merker fuer Ramende
        endc

        cblock  0A0h
        bank1:0
        swr:3
        ukeyer:4
        keyergeschw:2
        anst:1
        agc:2
        wert:0
        wertl:1
        werth:1
        s_konst1:2
        s_konst2:2
        s_wert:1
        tempbank1:12
        ddsbinrx:4      ;binaerwert fuer DDS laden Empfangsfrequenz
        ddsbintx:4      ;binaerwert fuer DDS laden Sendefrequenz
        ddsword:2
        ddsdword:4
        agcmerk:3       ;Zwischenspeicher fuer verzoegerte AGC
        bandmerk:2      ;Zwischenspeicher fuer Bandauswahl
        endc
;------------------------------------------------------------------------------
                        org     120h
bank2:
;------------------------------------------------------------------------------
                        org     1A0h
bank3:
;------------------------------------------------------------------------------


;Flagdefinitionen
#define blauto          flag1,0         ;Merkbit Lichtautomatik
#define bkeyer          flag1,1         ;1= keyer ein; 0= Keyer aus
#define blight          flag1,2         ;1= licht ein; 0= licht aus
#define brit            flag1,3         ;1=Rit ein
#define bzfablage       flag1,4         ;ZF Ablage
#define bddsohnezf      flag1,5         ;ZF Ablage beim Senden
#define bhw9            flag1,6         ;Merkbit HW9-Mode
#define bmenu           flag1,7         ;Merkbit fuer Menu
#define bidle           flag2,0         ;Merkbit fuer keine Funktion
#define bpunkt          flag2,1         ;Merkbit Punkt
#define bstrich         flag2,2         ;Merkbit Strich
#define bzs1ausein      flag2,3         ;Merkbit Zeit1 gestartet
#define bzs1einaus      flag2,4         ;Merkbit Zeit1 abgelaufen
#define bsenderein      flag2,5         ;Merkbit fuer Sender ein
#define bkeyeranz       flag2,6         ;Merkbit
#define LCDr            flag2,7         ;Statusspeicher fuer LCD beim Busylesen
#define bvfo            flag3,0         ;0=VFO_A  1=VFO_B
#define bddsneu         flag3,1         ;DDS neu errechnen und laden
#define blcdneu         flag3,2         ;LCD neu anzeugen
#define bzfdirekt       flag3,3         ;ZF ausgeben im SETUP
#define bdds6_075mhz    flag3,4         ;6,75 MHz ausgeben im SETUP bei DDS-Konstande
#define bxit            flag3,5         ;Merkbit fuer XIT
#define bnull           flag3,6         ;Fuehrende Null unterdruecken

;=========================================================================
;Namen der I/O Leitungen
;=========================================================================

;port a
#define         light           PORTA,4h        ;

;port b
#define         drehtakt        PORTB,0H        ;
#define         drehdir         PORTB,1H        ;

        ifdef   HW2                     ;Fertige Leiterplatte QRPeter
        messg   "Hardware LP QRPeter"

#define         ddsdaten        PORTB,4h        ;
#define         ddsclk          PORTB,3h        ;
#define         ddsfsyn         PORTB,2H        ;

        endif

        ifdef   HWtest                  ;Testaufbau von DL4JAL
        messg   "Hardware LP zum testen von DL4JAL"

#define         ddsdaten        PORTB,2h        ;
#define         ddsclk          PORTB,3h        ;
#define         ddsfsyn         PORTB,4H        ;

        endif


#define         taste           PORTB,5H        ;
#define         punktpin        PORTB,6H        ;
#define         strichpin       PORTB,7H        ;

;port c
LCDPort equ             PORTC           ;Port fuer LCDAusgabe
#define a_LCD_E         PORTC,0h        ;Enable
#define a_LCD_RW        PORTC,1h        ;R/W Pin
#define a_LCD_RS        PORTC,2h        ;RS Pin an LCD
#define senderein       PORTC,3h        ;Sender tasten
#define ea_LCD_D4       PORTC,4h        ;Daten von LCD
#define ea_LCD_D5       PORTC,5h        ;Daten von LCD
#define ea_LCD_D6       PORTC,6h        ;Daten von LCD
#define ea_LCD_D7       PORTC,7h        ;Daten von LCD

;=========================================================================
;Konstanten fuer die PIC-Konfiguration
;=========================================================================
d_option_reg:           equ     b'01000000'

;       bit 7: RBPU: PORTB Pull-up Enable bit
;               1 = PORTB pull-ups are disabled
;               0 = PORTB pull-ups are enabled by individual port latch values
;       bit 6: INTEDG: Interrupt Edge Select bit
;               1 = Interrupt on rising edge of RB0/INT pin
;               0 = Interrupt on falling edge of RB0/INT pin
;       bit 5: T0CS: TMR0 Clock Source Select bit
;               1 = Transition on RA4/T0CKI pin
;               0 = Internal instruction cycle clock (CLKOUT)
;       bit 4: T0SE: TMR0 Source Edge Select bit
;               1 = Increment on high-to-low transition on RA4/T0CKI pin
;               0 = Increment on low-to-high transition on RA4/T0CKI pin
;       bit 3: PSA: Prescaler Assignment bit
;               1 = Prescaler is assigned to the WDT
;               0 = Prescaler is assigned to the Timer0 module
;       bit 2-0: PS<2:0>: Prescaler Rate Select bits

d_port_a:               equ     00h
d_port_b:               equ     00h
d_port_c:               equ     00h
d_tmr0:                 equ     b'00000000'
                                                ;0=Ausgang
                                                ;1=Eingang
d_trisa:                equ     b'00101111'     ;Funktion siehe Definitionen
d_trisb:                equ     b'11100011'
d_trisc:                equ     b'00000000'
d_LCD_lesen:            equ     b'10000000'     ;zum Lesen aus LCD
d_LCD_schreiben:        equ     b'00000000'


d_intcon:               equ     b'11010000'     ;global interrupt enable
;bit 7: GIE: Global Interrupt Enable bit
;       1 = Enables all un-masked interrupts
;       0 = Disables all interrupts
;bit 6: PEIE: Peripheral Interrupt Enable bit
;       1 = Enables all un-masked peripheral interrupts
;       0 = Disables all peripheral interrupts
;bit 5: T0IE: TMR0 Overflow Interrupt Enable bit
;       1 = Enables the TMR0 interrupt
;       0 = Disables the TMR0 interrupt
;bit 4: INTE: RB0/INT External Interrupt Enable bit
;       1 = Enables the RB0/INT external interrupt
;       0 = Disables the RB0/INT external interrupt
;bit 3: RBIE: RB Port Change Interrupt Enable bit
;       1 = Enables the RB port change interrupt
;       0 = Disables the RB port change interrupt
;bit 2: T0IF: TMR0 Overflow Interrupt Flag bit
;       1 = TMR0 register has overflowed (must be cleared in software)
;       0 = TMR0 register did not overflow
;bit 1: INTF: RB0/INT External Interrupt Flag bit
;       1 = The RB0/INT external interrupt occurred (must be cleared in software)
;       0 = The RB0/INT external interrupt did not occur
;bit 0: RBIF: RB Port Change Interrupt Flag bit
;       1 = At least one of the RB<7:4> pins changed state (must be cleared in software)
;       0 = None of the RB<7:4> pins have changed state

d_pie1:                 equ     b'00000011'     ;

;bit 7: Reserved: Always maintain this bit clear
;bit 6: ADIE: A/D Converter Interrupt Enable bit
;       1 = Enables the A/D converter interrupt
;       0 = Disables the A/D converter interrupt
;bit 5-4: Reserved: Always maintain this bit clear
;bit 3: SSPIE: Synchronous Serial Port Interrupt Enable bit
;       1 = Enables the SSP interrupt
;       0 = Disables the SSP interrupt
;bit 2: CCP1IE: CCP1 Interrupt Enable bit
;       1 = Enables the CCP1 interrupt
;       0 = Disables the CCP1 interrupt
;bit 1: TMR2IE: TMR2 to PR2 Match Interrupt Enable bit
;       1 = Enables the TMR2 to PR2 match interrupt
;       0 = Disables the TMR2 to PR2 match interrupt
;bit 0: TMR1IE: TMR1 Overflow Interrupt Enable bit
;       1 = Enables the TMR1 overflow interrupt
;       0 = Disables the TMR1 overflow interrupt

d_t1con:                equ     b'00000001'     ;

;bit 7-6: Unimplemented: Read as '0'
;bit 5-4: T1CKPS<1:0>: Timer1 Input Clock Prescale Select bits
;       11 = 1:8 Prescale value
;       10 = 1:4 Prescale value
;       01 = 1:2 Prescale value
;       00 = 1:1 Prescale value
;bit 3: T1OSCEN: Timer1 Oscillator Enable Control bit
;       1 = Oscillator is enabled
;       0 = Oscillator is shut off (The oscillator inverter is turned off to eliminate power drain)
;bit 2: T1SYNC: Timer1 External Clock Input Synchronization Control bit
;       TMR1CS = 1
;       1 = Do not synchronize external clock input
;       0 = Synchronize external clock input
;       TMR1CS = 0
;       This bit is ignored. Timer1 uses the internal clock when TMR1CS = 0.
;bit 1: TMR1CS: Timer1 Clock Source Select bit
;       1 = External clock from pin RC0/T1OSO/T1CKI (on the rising edge)
;       0 = Internal clock (FOSC/4)
;bit 0: TMR1ON: Timer1 On bit
;       1 = Enables Timer1
;       0 = Stops Timer1

d_t2con:                equ     b'00000001'     ;

;bit 7: Unimplemented: Read as '0'
;bit 6-3: TOUTPS3:TOUTPS0: Timer2 Output Postscale Select bits
;       0000 = 1:1 Postscale
;       0001 = 1:2 Postscale
;       0010 = 1:3 Postscale
;       1111 = 1:16 Postscale
;bit 2: TMR2ON: Timer2 On bit
;       1 = Timer2 is on
;       0 = Timer2 is off
;bit 1-0: T2CKPS1:T2CKPS0: Timer2 Clock Prescale Select bits
;       00 = Prescaler is 1
;       01 = Prescaler is 4
;       1x = Prescaler is 16

tmr1hconst:             equ     0d9H
tmr1lconst:             equ     0h

tmr1word                equ     .9984

tmr1hconst:             equ     HIGH (0 - tmr1word)
tmr1lconst:             equ     LOW  (0 - tmr1word)

tmr2const:              equ     0H

d_pie2:                 equ     b'00010000'     ;eewrite interrupt enable

;bit 7: Unimplemented: Read as '0'
;bit 6: Reserved: Always maintain this bit clear
;bit 5: Unimplemented: Read as '0'
;bit 4: EEIE: EEPROM Write Operation Interrupt Enable
;       1 = Enable EE Write Interrupt
;       0 = Disable EE Write Interrupt
;bit 3: BCLIE: Bus Collision Interrupt Enable
;       1 = Enable Bus Collision Interrupt
;       0 = Disable Bus Collision Interrupt
;bit 2-1: Unimplemented: Read as '0'
;bit 0: Reserved: Always maintain this bit clear

d_adcon1:               equ     b'00000010'     ;

;bit 7 ADFM: A/D Result Format Select bit
;       1 = Right justified. Most Significant bits of ADRESH are read as  0
;       0 = Left justified. 6 Least Significant bits of ADRESL are read as  0 i.
;bit 6-4 Unimplemented: Read as '0'
;bit 3-0 PCFG3:PCFG0: A/D Port Configuration Control bits:
;PCFG0  AN7(1)RE2       AN6(1)RE1       AN5(1)RE0       AN4RA5  AN3RA3  AN2RA2  AN1RA1  AN0RA0
;0000   A               A               A               A       A       A       A       A
;0001   A               A               A               A       VREF+   A       A       A
;0010   D               D               D               A       A       A       A       A
;0011   D               D               D               A       VREF+   A       A       A
;0100   D               D               D               D       A       D       A       A
;0101   D               D               D               D       VREF+   D       A       A
;011x   D               D               D               D       D       D       D       D
;1000   A               A               A               A       VREF+   VREF-   A       A
;1001   D               D               A               A       A       A       A       A
;1010   D               D               A               A       VREF+   A       A       A
;1011   D               D               A               A       VREF+   VREF-   A       A
;1100   D               D               D               A       VREF+   VREF-   A       A
;1101   D               D               D               D       VREF+   VREF-   A       A
;1110   D               D               D               D       D       D       D       A
;1111   D               D               D               D       VREF+   VREF-   D       A

err1:                   equ     0h
fnull:                  equ     1h
tonw                    equ     2h
nodisp:                 equ     3h
di:                     equ     7               ; input
do:                     equ     6               ; output

speicher1:              equ     20h             ;Speicherplatz 1
speicher2:              equ     80h             ;Speicherplatz 2
sp_lang:                equ     5ah             ;maximale Anzahl der Zeichen in den Speichern

x0vvein                 equ     .0
x0vvaus                 equ     .4

;=========================================================================
; EEPROM Zellen
;=========================================================================
        org 2100h

eflag1          de      0ffh                  ; Konfig-Word (Keyer/Light/VFO-ZF/HW9-Mode)

edds            de      0ffh,0ffh,0ffh,0ffh   ; DDS-Konstante
ezf             de      0ffh,0ffh,0ffh,0ffh   ; ZF
evfo:
estarta         de      0ffh,0ffh,0ffh,0ffh   ; Startfrequenz VFO A
estartb         de      0ffh,0ffh,0ffh,0ffh   ; Startfrequenz VFO B
extal80         de      0ffh,0ffh,0ffh,0ffh   ; Frequenz des 80m-XTAL (HW9-Mode)

escana          de      0ffh,0ffh,0ffh,0ffh   ; Startfrequenz für Scan
escanb          de      0ffh,0ffh,0ffh,0ffh   ; Endfrequenz für Scan

es_konst1       de      0ffh,0ffh             ; S-Meter Konstanten
es_konst2       de      0ffh,0ffh

;=========================================================================
; Hardware initialisieren
;=========================================================================

        org     0
PAGE0:
start:
        goto    haupt
;=========================================================================
;ISR
;=========================================================================
        org     4
isr:
        movwf   w_temp          ; W retten
        swapf   STATUS,W        ; Flags nach W holen
        clrf    STATUS          ; Bank 0 einschalten
        movwf   status_temp             ; Flags retten
        movf    PCLATH,W
        movwf   pclath_temp
        clrf    PCLATH
        bcf     STATUS, IRP
        movf    FSR,W
        movwf   fsr_temp

        btfss   INTCON,INTF     ;RB0 Interrupt durch PICbus
        goto    ir1
        BANKSEL OPTION_REG
        movf    OPTION_REG,W
        BANKSEL PORTA
        movwf   option_mirror
        btfss   option_mirror,6
        goto    ir20
        btfsc   drehdir         ;Drehrichtung
        goto    ir2
        decf    impulse,F
        goto    ir21
ir2:
        incf    impulse,F       ;
        goto    ir21
ir20:
        btfss   drehdir         ;Drehrichtung
        goto    ir202
        decf    impulse,F
        goto    ir21
ir202:
        incf    impulse,F       ;
ir21:
        btfss   option_mirror,6
        goto    ir22
        BANKSEL OPTION_REG
        bcf     OPTION_REG,6
        BANKSEL PORTA
        goto    ir23
ir22:
        BANKSEL OPTION_REG
        bsf     OPTION_REG,6
        BANKSEL PORTA
ir23:
        bcf     INTCON,INTF     ;Bit wieder bereit
ir1:                            ;Timer 1 ist fuer Zeitablaeufe im 10mSek-takt
        btfss   PIR1,TMR1IF     ;Timer 1 10mSek vorbei
        goto    ir3
        bcf     PIR1,TMR1IF     ;Timer 1 Bit loeschen
        movf    zs1,F           ;Zeitschleife1 testen
        btfss   STATUS,Z        ;? Test ob 0
        decf    zs1,F           ;0 um 1 decrementieren
        movf    zs2,F           ;Zeitschleife2 testen
        btfss   STATUS,Z        ;? Test ob 0
        decf    zs2,F           ;0 um 1 decrementieren

        movlw   tmr1hconst      ;timer neu laden
        movwf   TMR1H
        movlw   tmr1lconst
        movwf   TMR1L
ir3:                            ;Timer 2 ist fuer Punktzeit
        btfss   PIR1,TMR2IF
        goto    ir4
        bcf     PIR1,TMR2IF     ;Timer 2 Bit loeschen
        movf    zs3,F
        btfss   STATUS,Z
        decf    zs3,F
ir4:
        btfss   INTCON,T0IF
        goto    ir6
        bcf     INTCON,T0IF
ir6:
        btfsc   PIR2,EEIF       ;? EEWrite inter.
        bcf     PIR2,EEIF       ;1 Bit loeschen

;Restaurieren der Register
iend:
        movf    fsr_temp,W
        movwf   FSR
        movf    pclath_temp,W
        movwf   PCLATH
        swapf   status_temp,w   ; gerettete Flags in W
        movwf   STATUS          ; Flags restaurieren
        swapf   w_temp,f        ; W restaurieren
        swapf   w_temp,w        ; W restaurieren
        retfie
        
;==============================================================================
;Funktion:      Wandlung BCDZahl unteres Nibbel in ASCIIzeichen
;Eingang:       BCD in W unteres Nibbel
;Ausgang:       entsprechendes Ascii-Zeichen in W
;------------------------------------------------------------------------------
BCDToASCII:
        andlw   B'00001111'     ;sicherheitshalber nur unteres Nibbel
        addwf   PCL             ;zum Programmcounter addieren
        retlw   '0'             ;und mit entsprechenden Ascii zurueck
        retlw   '1'
        retlw   '2'
        retlw   '3'
        retlw   '4'
        retlw   '5'
        retlw   '6'
        retlw   '7'
        retlw   '8'
        retlw   '9'
        retlw   'A'
        retlw   'B'
        retlw   'C'
        retlw   'D'
        retlw   'E'
        retlw   'F'
;=========================================================================
u2bpm:
        andlw   B'00001111'     ;sicherheitshalber nur unteres Nibbel
        addwf   PCL             ;zum Programmcounter addieren
        retlw   .190            ;
        retlw   .180            ;
        retlw   .170            ;
        retlw   .160            ;
        retlw   .150            ;
        retlw   .140            ;
        retlw   .130            ;
        retlw   .120            ;
        retlw   .110            ;
        retlw   .100            ;
        retlw   .90             ;
        retlw   .80             ;
        retlw   .70             ;
        retlw   .60             ;
        retlw   .50             ;
        retlw   .40             ;
;=========================================================================
;Funktion       Keyergeschwindigkeit 9-40 BpM in Werte umsetzen die die
;               Zeitschleife entsprechend steuern
;Eingang        wertebereich 0-FFh vom DA wandler
;Ausgang        Zahl der Schleifenwerte multipliziert mit 1msek
;

ukey2msek:
        BANKSEL bank1
        movf   keyergeschw+1,W
        BANKSEL bank0
        movwf   schleife
        rrf     schleife,F
        rrf     schleife,F
        rrf     schleife,W
        andlw   B'00011111'
        addwf   PCL
        retlw   .133    ;9
        retlw   .120    ;10
        retlw   .109    ;11 BpM
        retlw   .100    ;12 BpM
        retlw   .92     ;13 BpM
        retlw   .86     ;14 BpM
        retlw   .80     ;15 BpM
        retlw   .75     ;16 BpM
        retlw   .71     ;17 BpM
        retlw   .67     ;18 BpM
        retlw   .63     ;19 BpM
        retlw   .60     ;20 BpM
        retlw   .57     ;21 BpM
        retlw   .55     ;22 BpM
        retlw   .52     ;23 BpM
        retlw   .50     ;24 BpM
        retlw   .48     ;25 BpM
        retlw   .46     ;26 BpM
        retlw   .44     ;27 BpM
        retlw   .43     ;28 BpM
        retlw   .41     ;29 BpM
        retlw   .40     ;30 BpM
        retlw   .39     ;31 BpM
        retlw   .38     ;32 BpM
        retlw   .36     ;33 BpM
        retlw   .35     ;34 BpM
        retlw   .34     ;35 BpM
        retlw   .33     ;36 BpM
        retlw   .32     ;37 BpM
        retlw   .32     ;38 BpM
        retlw   .31     ;39 BpM
        retlw   .30     ;40 BpM
;=========================================================================
;Funktion       Keyergeschwindigkeit 9-40 BpM in Werte umsetzen die die
;               Zeitschleife entsprechend steuern
;Eingang        wertebereich 0-FFh vom DA wandler
;Ausgang        HEX Ziffer zur Anzeige
;

ukey2anz:
        movwf   schleife
        rrf     schleife,F
        rrf     schleife,F
        rrf     schleife,W
        andlw   B'00011111'
        addwf   PCL
        retlw   9h
        retlw   10h
        retlw   11h
        retlw   12h
        retlw   13h
        retlw   14h
        retlw   15h
        retlw   16h
        retlw   17h
        retlw   18h
        retlw   19h
        retlw   20h
        retlw   21h
        retlw   22h
        retlw   23h
        retlw   24h
        retlw   25h
        retlw   26h
        retlw   27h
        retlw   28h
        retlw   29h
        retlw   30h
        retlw   31h
        retlw   32h
        retlw   33h
        retlw   34h
        retlw   35h
        retlw   36h
        retlw   37h
        retlw   38h
        retlw   39h
        retlw   40h

;==============================================================================
MenuStart:
        clrf    impulse         ;Impulse loeschen
        movlw   1               ;Tastennummer 1 voreinstellen
        movwf   tastennummer    ;
        movlw   .10             ;Zeitschleife aufbauen 500 mSek
        movwf   schleife        ;
MenuStart1:                     ;
        call    Tastegedrueckt  ;SCHLEIFE(1)
        SKPC                    ;
        goto    MenuStart2      ;  break --> Taste nicht gedrueckt Tastennummer 1
        call    t50mSek         ;            Display unveraendert
        decfsz  schleife,F      ;ENDE(1) nach 500 msek Taste gedrueckt
        goto    MenuStart1      ;
        call    LCDDisplayClear ;Display loeschen
        LCDStr  text5           ;"Menu" im Display
MenuStart3:
        movlw   2               ;nach 500 mSek Tastennummer 2
        movwf   tastennummer    ;
        call    Tastegedrueckt  ;SCHLEIFE(2)
        SKPNC
        goto    MenuStart3      ;ENDE(2) keine Taste gedrueckt
MenuStart2:
        return
;==============================================================================
;Abfrage ob Taste gedrueckt ist
;Ausgang        C=1 Taste gedrueckt
;               C=0 Taste nicht gedrueckt
Tastegedrueckt:
        bsf     STATUS,C
        btfsc   taste
        bcf     STATUS,C
        return

;=========================================================================
;Initialisierung des uC
;=========================================================================
init:
        clrf        STATUS      ; Flags loeschen

        BANKSEL TRISA                   ; Registerbank 80h..0AFh
        movlw   d_option_reg
        movwf   OPTION_REG
        movlw   d_trisa
        movwf   TRISA
        movlw   d_trisb
        movwf   TRISB
        movlw   d_trisc
        movwf   TRISC
        movlw   d_adcon1
        movwf   ADCON1
        movlw   d_pie1
        movwf   PIE1
        movlw   d_pie2
        movwf   PIE2
        BANKSEL PORTA                   ; Registerbank 0..2FH

        movlw   d_intcon
        movwf   INTCON
        movlw   d_port_a
        movwf   PORTA
        movlw   d_port_b
        movwf   PORTB
        movlw   d_port_c
        movwf   PORTC
        movlw   d_t1con
        movwf   T1CON
        movlw   d_t2con
        movwf   T2CON

        movlw   ramanfang       ;Ram loeschen
        movwf   FSR             ;fsr fuer indirekte Adressierung laden
ramclr1
        clrf    INDF            ;Fileregister indirekt loeschen
        movlw   ramende         ;Ramende in Akku
        subwf   FSR,W           ;Test ob Ende erreicht
        btfsc   STATUS,Z        ;Auswertung des Zerroflags
        goto    anf1            ;Ende erreicht
        incf    FSR,F           ;Ende nicht erreicht
        goto    ramclr1         ;wieder von vorn
anf1:
        movlw   tmr1hconst      ;Timer1 Zeit laden
        movwf   TMR1H
        movlw   tmr1lconst
        movwf   TMR1L
        BANKSEL bank1
        movlw   1
        movwf   agcmerk+2       ;Anzeigewert auf 1 setzen
        clrf    agcmerk         ;
        clrf    agcmerk+1       ;
        BANKSEL bank0
        return

;=========================================================================
;Zeitschleife
;1 Einheit= 1 mSek
;Einsprung bei DELAY wird mit Wert in W gerechnet
;=========================================================================
t1Sek:
        movlw   .250
        call    DELAY
        movlw   .250
        call    DELAY
        movlw   .250
        call    DELAY
t250mSek:
        movlw   .250
        goto    DELAY
t100mSek:
        movlw   .100
        goto    DELAY
t50mSek:
        movlw   .50
        goto    DELAY
t10mSek:
        movlw   .10
        goto    DELAY
t5mSek:
        movlw   .5
        goto    DELAY
t3mSek:
        movlw   .3
        goto    DELAY
t1mSek:
        movlw   .1
DELAY:
        ifdef   debug                   ;Zeitschleife verkuerzen
        return                          ;beim Simulieren
        endif

        movwf   delay_counter1
        incf    delay_counter1,F        ;min 1 auswerten
DELAY_1:
        DECFSZ  delay_counter1,F        ; dec F + result in F
        GOTO    DELAY_2                 ; if F=0: skip next instr.
        return

DELAY_2:
        MOVLW   .19
        MOVWF   delay_counter2
DELAY_3:
        DECFSZ  delay_counter2,F
        GOTO    DELAY_4

        GOTO    DELAY_1
DELAY_4:
        MOVLW   .16
        MOVWF   delay_counter3
DELAY_5:
        DECFSZ  delay_counter3,F
        GOTO    DELAY_5
        GOTO    DELAY_3

;=========================================================================
;Initialisierung der LCD Anzeige
;
;==============================================================================
;Function Set: 4 bit Datenbreite; 2 Zeilen
ib1     equ     B'00100000'     ;Function set 1. nibel 4.Bitmodus
ib2     equ     B'10000000'     ;Function set 2. nibel
;==============================================================================
;Entry Mode Set: increment, display shift
; d7 d6 d5 d4 d3 d2 d1 d0
;  0  0  0  0  0  1 I/D S
; I/D 1= increment      0= decrement
; S   1= display shift  0= display freeze
ib3     equ     B'00000110'     ;Bit1=I/D, Bit0=S
;==============================================================================
;Display on/off control: display on, cursor off , cursor not blink
; d7 d6 d5 d4 d3 d2 d1 d0
;  0  0  0  0  1  D  C  B
; D     1= display on           0= display off
; C     1= cursor on            0= cursor off
; B     1= cursor blink         0= cursor not blink
ib4     equ     B'00001100'     ;Bit2=D, Bit1=C, Bit0=B
ib4_off equ     B'00001000'     ;Bit2=D, Bit1=C, Bit0=B
;==============================================================================
;Cursor Display shift: display shift, right shift
; d7 d6 d5 d4 d3  d2 d1 d0
;  0  0  0  1 S/C F/L *  *
; S/C   1= display shift        0= cursor move
; F/L   1= right shift          0= left shift
ib5     equ     B'00010100'     ;Bit3=S/C, Bit2=R/L
;==============================================================================
LCDInit:
        bcf     a_LCD_E         ;Enable auf LOW setzen

        movlw   .15             ;15 mSek warten bevor 1. Byte geladen wird
        call    DELAY

        movlw   ib1             ;Funktion setzen
        call    LCDAusgabe
        movlw   ib2
        call    LCDAusgabe

        movlw   .5              ;5 mSek warten
        call    DELAY

        movlw   ib1             ;Funktion setzen
        call    LCDAusgabe
        movlw   ib2
        call    LCDAusgabe

        movlw   .1              ;1 mSek warten
        call    DELAY

        movlw   ib3             ;LCD entsprechend einstellen
        call    LCDCom
        movlw   ib4
        call    LCDCom
        movlw   ib5
        call    LCDCom

        call    LCDDisplayClear ;LCD loeschen + Cursor auf null
        return

;=========================================================================
;Ausgabe eines Char auf dem Display
;RS  = 1
;R/W = 0

LCDChar:
        movwf   LCDByte         ;Byte merken
        call    LCDRdy          ;ob LCD bereit
        movf    LCDByte,w       ;
        andlw   B'11110000'     ;oberes Nibbel verwenden
        iorlw   B'00000100'     ;RS hinzuschalten
        call    LCDAusgabe      ;zur LCD schicken
        swapf   LCDByte,w       ;unteres Nibbel laden vom gemerkten Byte
        andlw   B'11110000'     ;und zur LCD-Ausgabe vorbereiten
        iorlw   B'00000100'     ;RS hinzuschalten
        goto    LCDAusgabe      ;call einsparen und an LCD ausgeben
;------------------------------------------------------------------------------
;Ausgabe eines LCD-Commandos zur Steuerung der LCD
;RS  = 0
;R/W = 0

LCDCom:
        movwf   LCDByte         ;Byte merken
        call    LCDRdy          ;ist LCD bereit
        movf    LCDByte,w       ;zuerst oberes Nibbel
        andlw   B'11110000'
        call    LCDAusgabe      ;ausgeben
        swapf   LCDByte,w       ;und dann unteres Nibbel
        andlw   B'11110000'     ;ausgeben
;------------------------------------------------------------------------------
LCDAusgabe:
        btfsc   LCDPort,3       ;? test ob Bit gesetzt
        iorlw   B'00001000'     ;1 Bit nicht veraendern
        movwf   LCDPort         ;an PORT anlegen
;------------------------------------------------------------------------------
Enable:
        bsf     a_LCD_E         ;LH Flanke uebernimmt LCD die 4 Bit
        bcf     a_LCD_E
        return
;------------------------------------------------------------------------------
;lesen des Statusbits der LCD-anzeige
;BS  = 0
;R/W = 1

LCDRdy:
        ifdef   debug                   ;Zeitschleife verkuerzen
        return                          ;beim Simulieren
        endif

        bsf     LCDPort,7
        BANKSEL TRISC
        movlw   d_LCD_lesen     ;Port vorbereiten zum Lesen
        movwf   TRISC
        BANKSEL PORTC

        bcf     a_LCD_RS
        bsf     a_LCD_RW
LCDRdy1:
        bcf     LCDr
        bsf     a_LCD_E
        nop
        btfss   LCDPort,7
        bsf     LCDr
        bcf     a_LCD_E
        nop
        bsf     a_LCD_E
        nop
        bcf     a_LCD_E
        btfss   LCDr
        goto    LCDRdy1

        BANKSEL TRISC
        movlw   d_LCD_schreiben ;Port wieder zurueck
        movwf   TRISC
        BANKSEL PORTC
        bcf     a_LCD_RW

        return
;------------------------------------------------------------------------------
LCDDisplayClear:
        movlw   B'00000001'
        goto    LCDCom
;------------------------------------------------------------------------------
LCDCursorHome:
        movlw   B'00000010'
        goto    LCDCom
;------------------------------------------------------------------------------
LCDCursorZeile1:
        movlw   B'10000000'
        goto    LCDCom
;------------------------------------------------------------------------------
LCDCursorZeile2:
        movlw   B'11000000'
        goto    LCDCom
;------------------------------------------------------------------------------
LCDPos:
        iorlw   B'10000000'
        goto    LCDCom
;------------------------------------------------------------------------------
LCDString:
        BANKSEL EEADR
        movwf   EEADR           ;Uebergabe der LOW Adr von W nach EEADR
        movf    lcdtextadrh,W   ;Uebergabe der High Adr an EEADRH
        movwf   EEADRH
        BANKSEL EECON1
        bsf     EECON1, EEPGD   ;lesen aus dem Programmspeicher

LCDString3:
        bcf     INTCON,GIE
        btfsc   INTCON,GIE
        goto    LCDString3
        bsf     EECON1, RD      ;lesen
        nop                     ;nach 2 Takten sind Daten verfuegbar
        nop
        bsf     INTCON,GIE
        BANKSEL EEADR
        movf    EEDATA, W       ;Byte holen
        BANKSEL PORTC
        call    LCDPos          ;1. Byte ist die Position in der LCD
        BANKSEL EEADR
        incf    EEADR,F         ;naechstes Byte
        SKPNZ
        incf    EEADRH,F        ;Ueberlauf behandeln
LCDString1:
        BANKSEL EECON1
        bsf     EECON1, EEPGD   ;und wieder Lesen aus dem Programmspeicher
LCDString2:
        bcf     INTCON,GIE
        btfsc   INTCON,GIE
        goto    LCDString2
        bsf     EECON1, RD
        nop
        nop
        bsf     INTCON,GIE
        BANKSEL EEADR
        movf    EEDATA, W
        BANKSEL PORTC
        andlw   B'11111111'     ;letztes Byte ist 0
        btfsc   STATUS, Z       ;? letztes Byte erreicht
        return                  ;1 Ende

        call    LCDChar         ;0 Ausgabe auf LCD
        BANKSEL EEADR
        incf    EEADR,F         ;  naechste Adresse
        SKPNZ
        incf    EEADRH,F        ;  und Ueberlauf behandeln
        goto    LCDString1
;------------------------------------------------------------------------------
LCDSpace:
        movlw   ' '
        goto    LCDChar
;------------------------------------------------------------------------------
LCDHEX:
        movwf   LCDByte2
        swapf   LCDByte2,W
        call    BCDToASCII
        call    LCDChar
        movf    LCDByte2,W
        call    BCDToASCII
        goto    LCDChar

;------------------------------------------------------------------------------
haupt:
        call    init            ;MC initialisieren
        ifdef   debug
        MOVLF   0x80,temp+4
        MOVLF   0xee,temp+5
        MOVLF   0x36,temp+6
        MOVLF   0x00,temp+7
        MOVLF   0x40,zwischenfrequenz
        MOVLF   0x4b,zwischenfrequenz+1
        MOVLF   0x4c,zwischenfrequenz+2
        MOVLF   0x00,zwischenfrequenz+3
        call    subzf
        endif
        movlw   .1              ; Aktuelles Band als Standard
        movwf   band            ; auf 40m setzen...
        PAGESEL PAGE3           ; DBTODO vllt. routine bandconst auf page0 verlagern?
        call    bandconst
        PAGESEL PAGE0
        call    dds_init        ;DDS initialisieren
        bsf     light
        call    LCDInit         ;LCD initialisieren
        LCDStr  textc1
        LCDStr  textc2
        movlw   .7
        movwf   schleife
haupt0_2:
        call    t250mSek
        movlw   '.'
        call    LCDChar
        decfsz  schleife,F
        goto    haupt0_2
        call    eeladen         ;Grunddaten aus Eeprom laden
        incf    flag1,W         ;? Urladung
        btfss   STATUS,Z
        goto    haupt0_4
        movlw   .2
        movwf   schleife
        LCDStr  text14
        LCDStr  text15
        call    t1Sek
haupt0_5:
        movlw   ib4_off
        call    LCDCom
        call    t1Sek
        movlw   ib4
        call    LCDCom
        call    t1Sek
        decfsz  schleife
        goto    haupt0_5
        PAGESEL PAGE2
        goto    smdefault_1     ;1 Urladung
haupt0_4
        btfsc   taste
        goto    haupt0_1
        call    LCDDisplayClear ;LCD loeschen + Cursor auf null
        LCDStr  textc3
        LCDStr  textc4
        call    t1Sek
        call    t1Sek
        call    t1Sek
        call    t1Sek
        call    t1Sek
haupt0_3:
        btfss   taste
        goto    haupt0_3
        PAGESEL PAGE2
        goto    msetup
        PAGESEL PAGE0
haupt0_1:
        call    LCDDisplayClear ;LCD loeschen + Cursor auf null
        btfss   blight          ;? Dauerlicht im Eeprom
        bcf     light           ;1 Licht nicht wieder ausschalten
        call    zs1setzen2sek   ;Timer 2Sek starten
        bsf     bzs1ausein      ;Funktion Timer ein->aus aktivieren
        clrf    ebene           ;Menueebene 0 einschalten
        LDK1    step,2          ;Abstimmschritte=2 (50Hz)
        LDK1    idlecounter,.16 ;Idleschleife initialisieren (16 Durchlauefe)
        bsf     blcdneu         ;LCD anzeigen
        bsf     bddsneu         ;Startfrequenz einstellen
haupt1:
        bsf     bidle           ;Bit fuer keine Funktion setzen
        movf    impulse,F       ;? Drehimpulse angefallen
        SKPNZ
        goto    haupt5
        bcf     bidle           ;1 Bit fuer keine Funktion loeschen
        call    zs1setzen2sek
        movf    impulse,W       ;

        btfss   bhw9            ;? Reverted tuning/IF direction
        goto    haupt2a

;
; Auswerten des Tunings/Impulse in umgekehrte Richtung
;
        btfsc   impulse,7       ;  ? positive Impulse
        goto    haupt3r
        clrf    impulse         ;  1 wieder Vorbereiten fuer Interrupt
        andlw   B'01111111'     ;    < 127
        movwf   temp+9
haupt2r:                        ;    SCHLEIFE
        call    stepsub         ;      step subtrahieren
        decfsz  temp+9,F        ;    ENDE Impulse=0
        goto    haupt2r
        goto    haupt6
haupt3r:
        comf    impulse,W       ;  0 komplementaer bilden
        clrf    impulse         ;    wieder vorbereiten fuer Interrupt
        addlw   1               ;    komplementaer -> negation
        movwf   temp+9
haupt4r:                        ;    SCHLEIFE
        call    stepadd         ;      step addieren
        decfsz  temp+9,F        ;    ENDE Impulse=0
        goto    haupt4r
        goto haupt6

;
; Auswerten des Tunings/Impulse in "normaler" Richtung
;
haupt2a:
        btfsc   impulse,7       ;  ? positive Impulse
        goto    haupt3
        clrf    impulse         ;  1 wieder Vorbereiten fuer Interrupt
        andlw   B'01111111'     ;    < 127
        movwf   temp+9
haupt2:                         ;    SCHLEIFE
        call    stepadd         ;      step addieren
        decfsz  temp+9,F        ;    ENDE Impulse=0
        goto    haupt2
        goto    haupt6
haupt3:
        comf    impulse,W       ;  0 komplementaer bilden
        clrf    impulse         ;    wieder vorbereiten fuer Interrupt
        addlw   1               ;    komplementaer -> negation
        movwf   temp+9
haupt4:                         ;    SCHLEIFE
        call    stepsub         ;      step subtrahieren
        decfsz  temp+9,F        ;    ENDE Impulse=0
        goto    haupt4

haupt6:
        bsf     bddsneu         ;  DDS neu berechnen
        bsf     blcdneu         ;  LCD neu anzeigen
haupt5:
        btfss   blcdneu         ;? LCD neu anzeigen Zeile1
        goto    haupt70
        bcf     bidle           ;1 Bit fuer keine Funktion loeschen
        call    LCDAnzeigeZ1    ;  LCD neu anzeigen Zeile1
        call    LCDAnzeigeZ2    ;  LCD neu anzeigen Zeile2
        bcf     blcdneu         ;  BIT loeschen LCD neuanzeige
haupt70:
        btfss   bddsneu         ;? DDS neu berechnen
        goto    haupt8
        bcf     bidle           ;1 Bit fuer keine Funktion loeschen
        call    ddsbinausrechnen;  DDS neu ausrechnen
        btfsc   senderein       ;  ? Sender aus
        goto    haupt701        ;
        call    t1mSek          ;
        call    rx_to_dds       ;  1 Empfangsfrequenz laden
        goto    haupt702        ;
haupt701:                       ;
        call    tx_to_dds       ;  1 Sendefrequenz laden
haupt702:                       ;
        bcf     bddsneu         ;  bit loeschen
haupt8:                         ;
        btfsc   taste           ;? Taste gedrueckt
        goto    haupt9          ;
        bcf     bidle           ;1 Bit fuer keine Funktion loeschen
        btfsc   blauto          ;  ? Lichtautomatik an
        bsf     light           ;  1 Licht ein
        call    MenuStart       ;  Entprellung abfragen
        movf    tastennummer,W  ;  ? Tastennummer
        xorlw   1               ;
        SKPZ                    ;
        goto    haupt8_1        ;  1 Taste kurz gedrueckt
haupt0001:
        incf    step,F          ;    Abstimmschritte aendern
        movf    step,W          ;    ? grobsten Schritt erreicht
        xorlw   .4              ;
        SKPZ                    ;
        goto    haupt8_2        ;
haupt8_5:                       ;
        movlw   1               ;    1 wieder feinsten Schritt
        movwf   step            ;
haupt8_2:                       ;
        bsf     blcdneu         ;    neue LCDAnzeige
        goto    haupt8_3        ;
haupt8_1:
        PAGESEL PAGE2           ;
        call    Menuauswertung  ;  2 Taste lang gedrueckt
        PAGESEL PAGE0           ;    Menueinsprung
haupt8_3:
        btfsc   blauto          ;  ? Lichtautomatik an
        call    zs1setzen2sek   ;  1 Lichtautomatik starten
haupt9:
        btfss   bkeyer          ;? keyer ein
        goto    haupt91         ;
        btfss   punktpin        ;1 ? Punktpaddle
        bsf     bpunkt          ;  1 merken
        btfss   strichpin       ;  ? Strichpaddle
        bsf     bstrich         ;  1 merken
        btfss   bpunkt          ;  ? Punkt gemerkt
        goto    haupt10
        bcf     bidle           ;  1 Bit fuer keine Funktion loeschen
        call    tx_to_dds       ;    Sendefrequenz einstellen
        bsf     senderein       ;    Sender einschalten
        call    punkt           ;    Punktdauer warten
        bcf     senderein       ;    Sender ausschalten
        call    t1mSek          ;
        call    rx_to_dds       ;    Empfangsfrequenz einstellen
        call    punkt           ;    Punktdauer warten
        bcf     bpunkt          ;    gemerkten Punkt loeschen
haupt10:
        btfss   bstrich         ;  ? Strich gemerkt
        goto    haupt11
        bcf     bidle           ;  1 Bit fuer keine Funktion loeschen
        call    tx_to_dds       ;    Sender einschalten
        bsf     senderein       ;    Sender einschalten
        call    punkt           ;    Punktdauer warten
        call    punkt           ;    Punktdauer warten
        call    punkt           ;    Punktdauer warten
        bcf     senderein       ;    Sender ausschalten
        call    t1mSek          ;
        call    rx_to_dds       ;    Empfangsfrequenz einstellen
        call    punkt           ;    Punktdauer warten
        bcf     bstrich         ;    gemerkten Strich loeschen
        goto    haupt93
haupt91:
        btfsc   punktpin        ;0 ? Handtaste Taste gedrueckt
        goto    haupt92
        btfsc   senderein       ;  1 ? Sender noch aus
        goto    haupt93
        bcf     bidle           ;    1 Bit fuer keine Funktion loeschen
        call    tx_to_dds       ;      Sender einschalten
        bsf     senderein       ;      Sender einschalten
        goto    haupt93
haupt92:
        btfss   senderein       ;  0 ? Sender noch an
        goto    haupt93
        call    t1mSek          ;
        call    rx_to_dds       ;    1 Sendefrequenz ausgeben
        bcf     senderein       ;      Sender ausschalten
haupt93:
haupt11:
        movf    zs1,F           ;? Timer 2 Sekunden aktiv
        SKPNZ
        goto    haupt11_1
        btfss   bzs1ausein      ;1 ? Funktion aus -> ein schon ausgefuehrt
        goto    haupt12
        btfsc   blauto          ;  1 ? Autolicht
        bsf     light           ;    1 Licht ein
        bcf     bzs1ausein      ;    Funktion aus -> ein deaktivieren
        bsf     bzs1einaus      ;    Funktion ein -> aus aktivieren
        bsf     blcdneu         ;    LCD neu Anzeigen
        goto    haupt12
haupt11_1:
        btfss   bzs1einaus      ;0 ? Funktion ein -> aus schon ausgefuehrt
        goto    haupt12
        btfsc   blauto          ;  1 ? Autolicht
        bcf     light           ;    1 Licht aus
        bsf     bzs1ausein      ;    Funktion aus -> ein aktivieren
        bcf     bzs1einaus      ;    Funktion ein -> aus deaktivieren
        bsf     blcdneu         ;    LCD neu Anzeigen
haupt12:
        btfss   bkeyeranz       ;? Keyergeschw. anzeigen
        goto    haupt13
        call    zs1setzen2sek   ;1 Timer 2 Sek setzen
        call    LCDAnzeigeZ1    ;  LCD neu anzeigen Zeile1
        call    LCDAnzeigeZ2    ;  LCD neu anzeigen Zeile2
        bcf     bidle           ;  kein idle mehr (Funktionsbit)
        bcf     bkeyeranz       ;  bit loeschen
haupt13:
        btfss   bidle           ;? keine Funktion ausgefuehrt
        goto    haupt80         ;
        movf    idlecounter,W   ;1 alle Nebenfunktionen beginnen hier
        xorlw   .1              ;  ? Idlefunktion 1 (Zeitspalte 1)
        SKPZ
        goto    haupt801        ;
        PAGESEL PAGE2
        call    AnzeigeBand     ;  1 Bandauswahl anzeigen
        PAGESEL PAGE0
haupt801:
        movf    idlecounter,W   ;
        xorlw   .2              ;  2 Idlefunktion 2 (Zeitspalte 2)
        SKPZ
        goto    haupt802        ;
        PAGESEL PAGE2
        call    AnzeigeAGC      ;    S-Meter anzeigen
        PAGESEL PAGE0
haupt802:
        movf    idlecounter,W   ;
        xorlw   .3              ;  3 Idlefunktion 3 (Zeitspalte 3)
        SKPZ
        goto    haupt803        ;
        BANKSEL bank1
        RL2     ukeyer+2        ;    Summe von UKeyer x 16
        RL2     ukeyer+2        ;    weil nur 16 Idledurchlaeufe
        RL2     ukeyer+2        ;    wir brauchen 256 zum auswerten
        RL2     ukeyer+2        ;    (Durchschnittserrechnung)
        LD1     ukeyer, ukeyer+3;    Summe von UKeyer holen
        movf    keyergeschw,W   ;    gemerkte Ukeyer holen
        subwf   ukeyer+3        ;    und vergleichen
        SKPNZ                   ;    ? Werte sind ungleich
        goto    haupt803_1
        LD1     ukeyer+3,ukeyer ;
        incf    keyergeschw,W   ;      oder keyergeschw +1
        subwf   ukeyer+3        ;
        SKPNZ                   ;
        goto    haupt803_1      ;
        LD1     ukeyer+3,ukeyer ;
        decf    keyergeschw,W   ;      oder keyergeschw -1
        subwf   ukeyer+3        ;
        SKPNZ                   ;
        goto    haupt803_1      ;
        BANKSEL bank0           ;    1 Keyergeschw neu anzeigen
        bsf     bkeyeranz
        BANKSEL bank1
        LD1     keyergeschw, ukeyer;   und neuen Wert merken
haupt803_1:
        CLR4    ukeyer          ;    Summenwert loeschen
        BANKSEL bank0
haupt803:
        decfsz  idlecounter,F   ;  Idlefunktion + 1
        goto    haupt899        ;  ? 16x Idle
        movlw   .16             ;  1 wieder von vorn
        movwf   idlecounter
haupt899:
        call    UmessKeyerPoti  ;  Keyerpoti messen
        BANKSEL bank1
        movwf   ukeyer          ;  als word speichern
        clrf    ukeyer+1        ;  hoeherwertiges byte loeschen
        ADD2    ukeyer+2, ukeyer;  und als word addieren
        BANKSEL bank0
haupt80:
haupt99:
        goto    haupt1          ;und wieder von vorn

;==============================================================================
;Funktion       Anzeige der Frequenz auf LCD Zeile
;Eingang        Frequenz im Speicher
;Ausgang        LCD

LCDAnzeigeZ2:
        btfss   bkeyer
        goto    LCDAnZ202
        btfss   bkeyeranz
        goto    LCDAnZ202
        LCDStr  text4
        call    UmessKeyerPoti
        call    ukey2anz
        call    LCDHEX
        call    LCDSpace
        call    LCDSpace
        call    LCDSpace
        return

LCDAnZ202:
        movlw   40H
        call    LCDPos          ;Cursor bewegen

        btfss   bxit            ;? xit ein
        goto    LCDAnZ203       ;
        movlw   'x'             ;1 Zeile 2 RX Frequenz
        goto    LCDAnZ201       ;
LCDAnZ203:
        movlw   'A'             ;
        btfss   bvfo            ;? VFO B aktiv
        movlw   'B'             ;1 B anzeigen
        btfss   brit            ;? Rit aktiv
        goto    LCDAnZ201
        movlw   'a'             ;1 ? VFO B aktiv
        btfss   bvfo            ;
        movlw   'b'             ;  1 B anzeigen
LCDAnZ201:
        call    LCDChar         ;anzeigen
        btfss   bvfo            ;? Frequenz B an
        goto    LCDLoadB
        goto    LCDLoadA
LCDAnzeigeZ1:
        clrw                    ;1. Position Zeile 1
        call    LCDPos          ;Cursor bewegen

        btfss   bxit            ;
        goto    LCDAnZ102       ;
        movlw   'r'             ;
        goto    LCDAnZ101       ;
LCDAnZ102:
        movlw   'A'             ;
        btfsc   bvfo            ;? VFO B aktiv
        movlw   'B'             ;1 B anzeigen
        btfss   brit
        goto    LCDAnZ101
        movlw   'a'             ;
        btfsc   bvfo            ;? VFO B aktiv
        movlw   'b'             ;1 B anzeigen
LCDAnZ101:
        call    LCDChar         ;anzeigen
        btfsc   bvfo            ;? Frequenz B aus
        goto    LCDLoadB
        goto    LCDLoadA
LCDLoadB:
        LD4     tempkonst1, frequenzb   ;1 VFO B temporär kopieren
        goto LCDfkorr
LCDLoadA:
        LD4     tempkonst1, frequenza   ;1 VFO A temporär kopieren
LCDfkorr:
        btfss   bhw9
        goto    LCDAnzeige

        movlw   tempkonst1              ; Von bisheriger Frequenz
        movwf   pointer1                ; die Bandkonstante abziehen
        movlw   bconst                  ; und das Ergebnis
        movwf   pointer2                ; wieder speichern...
        PAGESEL PAGE0
        call    cleartemp4
        call    bcdsub4
        PAGESEL PAGE2
LCDAnzeige:
        movlw   tempkonst1+3
        movwf   FSR
        bsf     bnull           ;BIT fuehrende Null setzen
        swapf   INDF,W
        call    LCDfrqbyte      ;oberes Nibbel
        movf    INDF,W
        call    LCDfrqbyte      ;unteres Nibbel
        movlw   '.'             ;Punkt zur besseren Lesbarkeit
        call    LCDChar
        decf    FSR             ;naechstes Byte
        swapf   INDF,W          ;oberes Nibbel
        call    LCDfrqbyte
        movf    step,W          ;? 1MHz schritte
        xorlw   4               ;
        btfsc   STATUS,Z        ;
        goto    LCDAnz01
        movf    INDF,W          ;unteres Nibbel
        call    LCDfrqbyte
        decf    FSR             ;naechstes Byte
        swapf   INDF,W          ;oberes Nibbel
        call    LCDfrqbyte
        movlw   '.'             ;Punkt zur besseren Lesbarkeit
        call    LCDChar
        movf    step,W          ;? 1kHz schritte
        xorlw   3               ;
        btfsc   STATUS,Z        ;
        goto    LCDAnz1
        movf    INDF,W          ;0 weiter mit der Anzeige
        call    LCDfrqbyte
        movf    step,W          ;? 50Hz Schritte
        xorlw   2
        btfsc   STATUS,Z
        goto    LCDAnz2
        decf    FSR             ;0 weiter mit der Anzeige
        swapf   INDF,W
        call    LCDfrqbyte
        movf    step,W
        xorlw   1               ;? 10Hz Schritte
        btfsc   STATUS,Z
        goto    LCDAnz3
        movf    INDF,W          ;0 1Hz anzeigen
        call    LCDfrqbyte
        goto    LCDAnz9
LCDAnz01:
        movlw   ' '
        call    LCDChar
        movlw   ' '
        call    LCDChar
        movlw   ' '
        call    LCDChar
LCDAnz1:                        ;3 Leerzeichen ausgeben
        movlw   ' '
        call    LCDChar
LCDAnz2:
        movlw   ' '             ;2 Leerzeichen ausgeben
        call    LCDChar
LCDAnz3:
        movlw   ' '             ;1 Leerzeichen ausgeben
        call    LCDChar
LCDAnz9:                        ;kein Leerzeichen ausgeben
        return
;==============================================================================
;Funktion       eine Zahl auf LCD ausgeben Fuehrende 0 als Leerzeichen

LCDfrqbyte:
        andlw   B'00001111'     ;unteres Nibbel filtern
        btfss   STATUS,Z        ;? Zeichen = 0
        goto    LCDfrq3
        btfss   bnull           ;1 ? fuehrende Null
        goto    LCDfrq1
        movlw   ' '             ;  1 Leerzeichen ausgeben
        goto    LCDfrq4
LCDfrq1:
LCDfrq3:
        iorlw   0x30            ;
        bcf     bnull
LCDfrq4
        goto    LCDChar

;=========================================================================
band2hex:
        andlw   B'00000111'     ;sicherheitshalber nur bits 0-3
        addwf   PCL             ;zum Programmcounter addieren
        retlw   .128            ; 0 -> 0x80
        retlw   .48             ; 1 -> 0x40
        retlw   .36             ; 2 -> 0x30
        retlw   .32             ; 3 -> 0x20
        retlw   .23             ; 4 -> 0x17
        retlw   .21             ; 5 -> 0x15
        retlw   .18             ; 6 -> 0x12
        retlw   .16             ; 7 -> 0x10

LCDBand:
        movf    band,W
        call    band2hex
        call    LCDHEX
        LCDStr  tbm             ;"m" im Display hinzufuegen

;==============================================================================
;Abfragen Tastenstatus mit Entprellung
;Ausgang        Nummer der Taste in tastennummer
;               1-2 sind gueltige Tasten 0= ungueltig

tastcounter     equ     .10     ;Anzahl der Eingabesequenzen die geprueft werden
                                ;auf Gleichheit
; DBTODO erweitern auf zusaetzliche Tasten
tastaturstatus:
        movlw   tastcounter
        movwf   schleife        ;zwischenspeichern
Tastaturst1:                    ;SCHLEIFE(1)
        movf    PORTB,W         ;Tastenzustand einlesen
        andlw   B'00100000'     ;filtern
        movwf   schleife+1      ;merken
        call    t1mSek          ;1 mSek warten
        movf    PORTB,W         ;erneut einlesen
        andlw   B'00100000'
        xorwf   schleife+1,W    ;und vergleichen
        btfss   STATUS,Z        ;? war die Eingabe gleich
        goto    tastaturstatus  ;0 nein wieder von ganz vorn
        decfsz  schleife        ;1 gueltig Zaehler dec
        goto    Tastaturst1     ;? gueltige Eingabenanzahl erreicht
        movf    schleife+1,W    ;1 Eingabe auswerten
        andlw   B'00100000'
        btfss   STATUS,Z
        goto    Tastaturst2
        movlw   1
        movwf   tastennummer    ;1 abspeichern
        bsf     STATUS,C
        goto    Tastaturst99    ;ENDE
Tastaturst2:
        movlw   0               ;keine gueltige Tastennummer
        movwf   tastennummer    ;Tastennummer 0 abspeichern
        bcf     STATUS,C
Tastaturst99:
        return
        
;==============================================================================
;Funktion       loeschen des Tempram alle 10 Byte oder 4 untersten Byte

cleartemp:
        clrf    temp+4
        clrf    temp+5
        clrf    temp+6
        clrf    temp+7
        clrf    temp+8
        clrf    temp+9
cleartemp4:
        clrf    temp
        clrf    temp+1
        clrf    temp+2
        clrf    temp+3
        return
        
;------------------------------------------------------------------------------
;Funktion       add des eingestellten Schritt zur Frequenz
;               1kHz, 100Hz, 10Hz oder 1Hz

stepadd:
        movf    step,W          ;? step
        btfsc   STATUS,Z        ;0 Schritt = 1 Hz
        goto    bcdadd1
        xorlw   1
        btfsc   STATUS,Z
        goto    bcdadd10        ;1 Schritt = 10 Hz
        movf    step,W
        xorlw   2
        btfsc   STATUS,Z
        goto    bcdadd50        ;2 Schritt = 50 Hz
        movf    step,W          ;? step
        xorlw   3
        btfsc   STATUS,Z
        goto    bcdadd1000      ;3 Schritt = 1000 Hz
        movf    step,W          ;? step
        xorlw   4
        btfsc   STATUS,Z
        goto    bcdadd100k      ;4 Schritt = 100kHz
        return
;------------------------------------------------------------------------------
bcdadd100k:
        call    fpointerladen
        call    cleartemp4
        movlw   10h
        movwf   temp+2
        goto    bcdadd4
;------------------------------------------------------------------------------
bcdadd1000:
        call    fpointerladen
        call    cleartemp4
        movlw   10h
        movwf   temp+1
        goto    bcdadd4
;------------------------------------------------------------------------------
bcdadd100:
        call    fpointerladen
        call    cleartemp4
        movlw   .1
        movwf   temp+1
        goto    bcdadd4
;------------------------------------------------------------------------------
bcdadd50:
        call    fpointerladen
        call    cleartemp4
        movlw   50H
        movwf   temp
        goto    bcdadd4
;------------------------------------------------------------------------------
bcdadd10:
        call    fpointerladen
        call    cleartemp4
        movlw   10H
        movwf   temp
        goto    bcdadd4
;------------------------------------------------------------------------------
bcdadd1:
        call    fpointerladen
        call    cleartemp4
        incf    temp,F
;------------------------------------------------------------------------------
bcdadd4:
        movlw   4               ;4 byte add
        movwf   temp+4
bcdadd41:
        movf    pointer1,W      ;pointer1 ins FSR laden
        movwf   FSR
        movf    INDF,W
        movwf   temp+5          ;in temp+5 zwischenspeichern
        movf    pointer2,W      ;pointer2 ins FSR laden
        movwf   FSR
        movf    INDF,W
        addwf   temp+5,F        ;und beide werte addieren
        movlw   6
        addwf   temp+5,W
        btfss   STATUS,DC       ;? BCD korrektur low durchfuehren
        goto    bcdadd42
        movwf   temp+5          ;1 ja
bcdadd42:
        movlw   60H             ;? BCD korrektur high durchfueren
        addwf   temp+5,W
        btfss   STATUS,C
        goto    bcdadd43
        movwf   temp+5          ;1 ja
        movf    pointer1,W
        movwf   FSR
        incf    FSR,F
        incf    INDF,F          ;  ueberlauf addieren
        decf    FSR,F
bcdadd43:
        movf    pointer1,W      ;ergebnis in pointer1 laden
        movwf   FSR
        movf    temp+5,W
        movwf   INDF
        incf    pointer1
        incf    pointer2
        decfsz  temp+4
        goto    bcdadd41
        return
;==============================================================================
;Funktion       sub des eingestellten Schritt zur Frequenz
;               1kHz, 50Hz, 10Hz oder 1Hz

stepsub:
        movf    step,W
        btfsc   STATUS,Z
        goto    bcdsub1
        xorlw   1
        btfsc   STATUS,Z
        goto    bcdsub10
        movf    step,W
        xorlw   2
        btfsc   STATUS,Z
        goto    bcdsub50
        movf    step,W
        xorlw   3
        btfsc   STATUS,Z
        goto    bcdsub1000
        movf    step,W
        xorlw   4
        btfsc   STATUS,Z
        goto    bcdsub100k
        return
;------------------------------------------------------------------------------
bcdsub100k:
        call    fpointerladen
        call    cleartemp4
        movlw   10h
        movwf   temp+2
        goto    bcdsub4
;------------------------------------------------------------------------------
bcdsub1000:
        call    fpointerladen
        call    cleartemp4
        movlw   10h
        movwf   temp+1
        goto    bcdsub4
;------------------------------------------------------------------------------
bcdsub100:
        call    fpointerladen
        call    cleartemp4
        movlw   .1
        movwf   temp+1
        goto    bcdsub4
;------------------------------------------------------------------------------
bcdsub50:
        call    fpointerladen
        call    cleartemp4
        movlw   50H
        movwf   temp
        goto    bcdsub4
;------------------------------------------------------------------------------
bcdsub10:
        call    fpointerladen
        call    cleartemp4
        movlw   10H
        movwf   temp
        goto    bcdsub4
;------------------------------------------------------------------------------
bcdsub1:
        call    fpointerladen
        call    cleartemp4
        incf    temp,F
;------------------------------------------------------------------------------
bcdsub4:
        movlw   4
        movwf   temp+4
bcdsub41:
        movf    pointer1,W
        movwf   FSR
        movf    INDF,W
        movwf   temp+5          ;
        movf    pointer2,W
        movwf   FSR
        movf    INDF,W
        subwf   temp+5,F
        rlf     temp+6,F        ;Carry merken
        btfsc   STATUS,DC
        goto    bcdsub42
        movlw   6
        subwf   temp+5,F
bcdsub42:
        btfsc   temp+6,C
        goto    bcdsub43
        movlw   60H
        subwf   temp+5,F
        incf    FSR,F
        incf    INDF,F
        decf    FSR,F
bcdsub43:
        movf    pointer1,W
        movwf   FSR
        movf    temp+5,W
        movwf   INDF
        incf    pointer1
        incf    pointer2
        decfsz  temp+4
        goto    bcdsub41
        return

bcdtobin:
        call    cleartemp       ;temp loeschen
        movf    pointer1,W      ;4 byte nach temp laden
        movwf   FSR             ;A oder B VFO
        movf    INDF,W
        movwf   temp
        incf    FSR,F
        movf    INDF,W
        movwf   temp+1
        incf    FSR,F
        movf    INDF,W
        movwf   temp+2
        incf    FSR,F
        movf    INDF,W
        movwf   temp+3
        movlw   D'32'           ;32 bit bcd in bin umwandeln
        movwf   temp+8

bcdtobin1:
        bcf     STATUS,C
        rrf     temp+3
        rrf     temp+2
        rrf     temp+1
        rrf     temp
        rrf     temp+7
        rrf     temp+6
        rrf     temp+5
        rrf     temp+4
        movlw   temp
        movwf   FSR
        movlw   4
        movwf   temp+9
bcdtobin3:
        btfss   INDF,7
        goto    bcdtobin4
        movlw   30H
        subwf   INDF,F
bcdtobin4:
        btfss   INDF,3
        goto    bcdtobin5
        movlw   3
        subwf   INDF,F
bcdtobin5:
        incf    FSR,F
        decfsz  temp+9,F
        goto    bcdtobin3
        decfsz  temp+8,F
        goto    bcdtobin1
        return
        
;------------------------------------------------------------------------------
addzf:                                  ;
        LD4     temp, zwischenfrequenz  ;
        ADD4    temp+4, temp            ;
        return                          ;
;------------------------------------------------------------------------------
subzf:                                  ;
        LD4     temp, zwischenfrequenz  ;
        SUB4    temp+4, temp            ;
        btfss   temp+7,7                ;? negativ
        return                          ;
        NEG4    temp+4                  ;
        return                          ;

;------------------------------------------------------------------------------
;Funktion       Multiplikation der Frequenz mit der 1Hz DDS-Konst
;Ausgang        binwert steht in tempkonst2+4

muldds:                         ;
        movf    ddskonst,W      ;Konstande 4Byte in temp laden
        movwf   tempkonst1      ;
        movf    ddskonst+1,W    ;
        movwf   tempkonst1+1    ;
        movf    ddskonst+2,W    ;
        movwf   tempkonst1+2    ;
        movf    ddskonst+3,W    ;
        movwf   tempkonst1+3    ;
        movlw   D'32'           ;
        movwf   temp+8          ;
muldds1:                        ;
        movlw   tempkonst1      ;schleife (1)
        movwf   pointer1        ;  pointer1 laden
        movlw   tempkonst2      ;
        movwf   pointer2        ;  pointer2 laden
        clrf    temp+1          ;  Ueberlauf loeschen
        bcf     STATUS,C        ;  Carry loeschen
        rrf     temp+7          ;  Faktor1 nach rechts schieben
        rrf     temp+6          ;
        rrf     temp+5          ;
        rrf     temp+4          ;
        btfss   STATUS,C        ;  ? Carry = 1 nach RR
        goto    muldds2         ;
        movlw   8               ;  1 Schleife laden mit 8
        movwf   temp+2          ;
muldds5:                        ;
        movf    pointer1,W      ;  schleife (2)
        movwf   FSR             ;    1 Pointer laden
        movf    INDF,W          ;    wert1 holen
        movwf   temp            ;    zwischenspeichern
        movf    pointer2,W      ;    Pointer2 laden
        movwf   FSR             ;    ins indirekte Register
        movf    temp+1,W        ;    gemerketen Ueberlauf holen
        clrf    temp+1          ;    und loeschen
        addwf   INDF,F          ;    zum Ergebnis addieren
        btfss   STATUS,C        ;    ? Ueberlauf
        goto    muldds4         ;
        movlw   1               ;    1 neuen Ueberlauf
        movwf   temp+1          ;      merken
muldds4:                        ;
        movf    temp,W          ;    Wert2 = Wert2 + Wert1
        addwf   INDF,F          ;
        btfss   STATUS,C        ;    ? Ueberlauf
        goto    muldds3         ;
        movlw   1               ;    1 neuen Ueberlauf
        movwf   temp+1          ;      merken
muldds3:                        ;
        incf    pointer1        ;
        incf    pointer2        ;
        decfsz  temp+2          ;  ende (2)
        goto    muldds5         ;
muldds2:                        ;
        bcf     STATUS,C        ;  Carry loeschen
        rlf     tempkonst1      ;  Wert1 nach links schieben
        rlf     tempkonst1+1    ;  Wert1 = 2 * Wert1
        rlf     tempkonst1+2    ;
        rlf     tempkonst1+3    ;
        rlf     tempkonst1+4    ;
        rlf     tempkonst1+5    ;
        rlf     tempkonst1+6    ;
        rlf     tempkonst1+7    ;
                                ;
        decfsz  temp+8,F        ;ende (1) 32 mal durchlaufen
        goto    muldds1         ;
        return                  ;
;------------------------------------------------------------------------------
;Funktion       Ram fuer Multiplikation loeschen

mulramclr:
        movlw   temp            ;Ram loeschen
        movwf   FSR             ;fsr fuer indirekte Adressierung laden
mulramclr1
        clrf    INDF            ;Fileregister indirekt loeschen
        movlw   tempkonst2+7    ;Ramende in Akku
        subwf   FSR,w           ;Test ob Ende erreicht
        btfsc   STATUS,Z        ;Auswertung des Zerroflags
        goto    mulramclr2      ;Ende erreicht
        incf    FSR,f           ;Ende nicht erreicht
        goto    mulramclr1      ;wieder von vorn
mulramclr2:
        return
;-----------------------------------------------------------------------
;Funktion       laden der entsprechenden Ramadr in den Pointerram

fpointerladenddstx:
        btfsc   brit                    ;? Rit aktiv
        goto    fpl03                   ;1 Zeile 2 = tx -> break
        btfsc   bxit                    ;0 ? xit aktiv
        goto    fpl03                   ;  1 Zeile 2 = tx -> break
        goto    fpl01                   ;Zeile 1 = tx

fpointerladen:                          ;Laden fuer Abstimmung
        movlw   temp                    ;
        movwf   pointer2                ;
        btfsc   bxit                    ;? xit
        goto    fpl03                   ;1 Zeile 2 abstimmen
fpointerladendds:                       ;Laden fuer RX
fpl01:                                  ;
        movlw   frequenza               ;Zeile 1 = rx
        btfsc   bvfo                    ;
        movlw   frequenzb               ;
        goto    fpl02                   ;
fpl03:                                  ;
        movlw   frequenza               ;Zeile 2 = tx
        btfss   bvfo                    ;
        movlw   frequenzb               ;
fpl02:                                  ;
        movwf   pointer1                ;
        return                          ;
;-----------------------------------------------------------------------
ddsbinausrechnen:
                                        ;RX Frequenz ausrechnen
        call    mulramclr               ;Ram loeschen
        call    fpointerladendds        ;entsprechenden VFO laden
        call    bcdtobin                ;BCD to BIN wandeln
        btfss   bzfablage               ;? VFO-ZF
        call    subzf                   ;1 ZF binaer subdrahieren
        btfsc   bzfablage               ;? VFO+ZF
        call    addzf                   ;1 ZF binaer addieren
        call    muldds                  ;Multiplikation mit 1Hz Konstande
        LD4     utemp,tempkonst2+3      ;
        BANKSEL bank1                   ;
        LD4     ddsbinrx,utemp          ;
        BANKSEL bank0                   ;
                                        ;TX Frequenz ausrechnen
        call    mulramclr               ;Ram loeschen
        call    fpointerladenddstx      ;entsprechenden VFO laden
        call    bcdtobin                ;BCD to BIN wandeln
        btfsc   bddsohnezf              ;? ZF Ablage beim Senden
        goto    ddsbinausr01            ;
        btfss   bzfablage               ;1 ? VFO-ZF
        call    subzf                   ;  1 ZF binaer subdrahieren
        btfsc   bzfablage               ;  ? VFO+ZF
        call    addzf                   ;  1 ZF binaer addieren
ddsbinausr01:                           ;
        call    muldds                  ;Multiplikation mit 1Hz Konstande
        LD4     utemp,tempkonst2+3
        BANKSEL bank1
        LD4     ddsbintx,utemp
        BANKSEL bank0
        return
;------------------------------------------------------------------------------
tx_to_dds:                              ;TXbin_speicher in den DDS laden
        BANKSEL bank1
        LD4     ddsdword,ddsbintx
        goto    frq28bit_to_dds
rx_to_dds:                              ;RXbin_speicher in den DDS laden
        BANKSEL bank1
        LD4     ddsdword,ddsbinrx
frq28bit_to_dds:
        BANKSEL bank0
        call    control_to_dds
        BANKSEL bank1
        LD2     ddsword,ddsdword
        bcf     ddsword+1,7
        bsf     ddsword+1,6
        BANKSEL bank0
        call    word_to_dds
        BANKSEL bank1
        RL4     ddsdword
        RL4     ddsdword
        LD2     ddsword,ddsdword+2
        bcf     ddsword+1,7
        bsf     ddsword+1,6
        BANKSEL bank0
        call    word_to_dds
        return
;------------------------------------------------------------------------------
dds_init:
        BANKSEL bank0
        call    control_to_dds
        BANKSEL bank1
        movlw   B'01000100'
        movwf   ddsword+1
        movlw   B'10111011'
        movwf   ddsword
        BANKSEL bank0
        call    word_to_dds
        BANKSEL bank1
        movlw   B'01000111'
        movwf   ddsword+1
        movlw   B'10101110'
        movwf   ddsword
        BANKSEL bank0
        call    word_to_dds
        return
;-----------------------------------------------------------------------------
control_to_dds:
        BANKSEL bank1
        movlw   B'00100000'
        movwf   ddsword+1
        movlw   B'00000000'
        movwf   ddsword
;-----------------------------------------------------------------------------
word_to_dds:
        BANKSEL bank0
        bcf     ddsfsyn
        BANKSEL bank1
        movf    ddsword+1,W
        BANKSEL bank0
        call    byte_to_dds
        BANKSEL bank1
        movf    ddsword,W
        BANKSEL bank0
        call    byte_to_dds
        bsf     ddsfsyn
        return
;-----------------------------------------------------------------------------
byte_to_dds:
        movwf   temp
        movlw   .8
        movwf   schleife
bytetodds01:
        rlf     temp,F
        SKPC
        bcf     ddsdaten
        SKPNC
        bsf     ddsdaten
        bcf     ddsclk
        bsf     ddsclk
        decfsz  schleife,F
        goto    bytetodds01
        return
        
;==============================================================================
kanalRA1:       equ     b'00001000'
kanalRA2:       equ     b'00010000'
kanalRA3:       equ     b'00011000'
kanalRA5:       equ     b'00100000'

Uagc:
        BANKSEL bank1
        bsf     ADCON1,7                ;auf 10 bit schalten
        BANKSEL bank0
        movlw   0x41 | kanalRA5         ;anal. kanal RA5
        call    Umess
        BANKSEL bank1
        movwf   werth
        movf    ADRESL,W
        movwf   wertl
        bcf     ADCON1,7                ;auf 8 bit schalten
        BANKSEL bank0
        return

UBatt:
        movlw   0x41 | kanalRA3         ;anal. kanal RA3
        goto    Umess
UPowerRueck:
        movlw   0x41 | kanalRA2         ;anal. kanal RA2
        goto    Umess
UPowerVor:
        movlw   0x41 | kanalRA1         ;anal. kanal RA1
        goto    Umess                   ;Keyerpoti
UmessKeyerPoti:
Umess0:
        movlw   0x41                    ; anal. kanal RA0
Umess:
        movwf   ADCON0                  ; ... als Messeingang setzen
        movlw   1
        call    DELAY
        bsf     ADCON0,2                ;Start Messung
ubme1:
        btfsc   ADCON0,2                ;warten bis Messung beendet
        goto    ubme1                   ;Ergebnis in adresh und adresl
        movf    ADRESH,W                ;Ergebnis in W
        return
;==============================================================================
;Funktion       Anpassen der Potikurve durch Multiplikation
;Eingang        Register W
;Ausgang        Register W
;Register       temp+9, temp+8, temp+6, temp+7

ubatt_mul:
        movwf   temp+9
        movlw   .154            ;Multplizieren mit 154
        movwf   temp+8
        movf    temp+9,W
        clrf    temp+9
        bsf     temp+9,3
        CLR2    temp+6
um1:
        bcf     STATUS,C
        RL2     temp+6
        RLF     temp+8
        SKPC
        goto    um2
        addwf   temp+6
        SKPNC
        incf    temp+7
um2:
        decfsz  temp+9
        goto    um1

        movlw   .0              ;Addieren mit 0
        bcf     STATUS,C
        addwf   temp+7,W

        return
        
;==============================================================================
;Funktion       HEX zu BCD wandeln
;Eingang        HEX in W
;Ausgang        BCD in temp+6 2 Byte lang
;Register       temp+5, temp+6, temp+7, temp+8, tem+9

HEX2BCD:
        movwf   temp+8
        movlw   .8
        movwf   temp+9
        clrf    temp+6
        clrf    temp+7
htb1:
        movf    temp+6,W
        andlw   B'00001111'
        movwf   temp+5
        movlw   .5
        subwf   temp+5,W
        btfss   STATUS,C
        goto    htb2
        movlw   .3
        addwf   temp+6,F
htb2:
        movf    temp+6,W
        andlw   B'11110000'
        movwf   temp+5
        movlw   50h
        subwf   temp+5,W
        btfss   STATUS,C
        goto    htb3
        movlw   30h
        addwf   temp+6,F
htb3:
        bcf     STATUS,C
        rlf     temp+8,F
        rlf     temp+6,F
        rlf     temp+7,F
        decfsz  temp+9,F
        goto    htb1
        movf    temp+6,W
        return
        
;==============================================================================
;Funktion       Dauer eines Punktes warten. Wartezeit wird 1mSek-Einheiten
;               und dem Timer2 gebildet. Waehrend der Wartezeit wird beim
;               Senden die Ausgangsleistung geregelt. Weiter Funktion ist
;               das abfragen der Paddel fuer Punkt oder Strichspeicher

d_pr2h  equ     .242    ;Timer2 Period Register
d_pr2l  equ     .242    ;Timer2 Period Register

punkt:
        ifdef   debug
        return
        endif
        call    UmessKeyerPoti  ;Keyergeschw vom Poti lesen
        BANKSEL bank1
        movwf   keyergeschw+1   ;Keyergeschwindigkeit mit Potiwert laden
        BANKSEL bank0
        call    ukey2msek       ;aus Potiwert umrechnen in mSek
        movwf   zs3             ;Timer setzen
        movlw   d_pr2h
        btfss   senderein       ;? Sender eingeschalten
        movlw   d_pr2l          ;0 kuerzere Pause
        BANKSEL PR2
        nop
        movwf   PR2
        BANKSEL PORTA
        bsf     T2CON,2         ;Timer starten ueber Interrupt
punkt3:                         ;SCHLEIFE
        btfss   punktpin
        bsf     bpunkt
        btfss   strichpin
        bsf     bstrich
        movf    zs3,F
        SKPZ
        goto    punkt3
punkt5:
        bcf     T2CON,2
        return
;==============================================================================
;Funktion       Anpassen der Potikurve durch Multiplikation
;Eingang        Register W
;Ausgang        Register W
;Register       temp+9, temp+8, temp+6, temp+7

keyermul:
        movwf   temp+9
        movlw   .42
        movwf   temp+8
        movf    temp+9,W
        clrf    temp+9
        bsf     temp+9,3
        CLR2    temp+6
kum1:
        bcf     STATUS,C
        RL2     temp+6
        RLF     temp+8
        SKPC
        goto    kum2
        addwf   temp+6
        SKPNC
        incf    temp+7
kum2:
        decfsz  temp+9
        goto    kum1

        movlw   .9
        bcf     STATUS,C
        addwf   temp+7,W

        return
        
;==============================================================================
zs1setzen2sek:
        movlw   .200
        movwf   zs1
        return

;=========================================================================
;Schreiben in Eeprom
;Eingang:       Addr in data_ee_addr
;               Daten in data_ee_data
;Ausgang:       Daten im Eeprom intern
;=========================================================================
ewrite:
        BANKSEL bank3                   ;
ew2:                                    ;SCHLEIFE (1)
        btfsc   EECON1,WR               ;  schreibbit abfragen
        goto    ew2                     ;ENDE(1) vorheriger Schreibvorgang beendet
        BANKSEL bank0           ;
        movf    data_ee_addr,w          ;Adresse zum EE-Register
        BANKSEL bank2                   ;
        movwf   EEADR                   ;data memory address to write
        BANKSEL bank0                   ;Daten zum EEregister
        movf    data_ee_data,w          ;
        BANKSEL bank2                   ;
        movwf   EEDATA                  ;data memory value to write
        BANKSEL bank3
        bcf     EECON1, EEPGD           ;point to data memory
        bsf     EECON1, WREN            ;enable writes
ew1:                                    ;SCHLEIFE (2)
        bcf     INTCON,GIE              ;  disable interrupts
        btfsc   INTCON,GIE              ;
        goto    ew1                     ;ENDE (2) Interrupt wirklich gesperrt
        movlw   55h                     ;
        movwf   EECON2                  ;write 55h
        movlw   0aah                    ;
        movwf   EECON2                  ;write aah
        bsf     EECON1,WR               ;set wr bit to begin write
ew3:                                    ;SCHLEIFE (2)
        btfsc   EECON1,WR               ;  schreibbit abfragen
        goto    ew3                     ;ENDE(2) Schreibvorgang beendet
        bsf     INTCON, GIE             ;enable interrupts
        bcf     EECON1, WREN            ;disable writes
        BANKSEL bank0
        return
;=========================================================================
;Lesen von Eeprom
;Eingang:       Addr in data_ee_addr
;Ausgang:       Daten in w und in data_ee_data
;=========================================================================
eread:
        movf    data_ee_addr,w
        BANKSEL EEADR
        movwf   EEADR
        BANKSEL EECON1
        bcf     EECON1, EEPGD
        bsf     EECON1, RD
        BANKSEL EEDATA
        movf    EEDATA,W
        BANKSEL PORTA
        return
;=========================================================================
eeladen:
        movlw   .21
        movwf   schleife
        movlw   flag1
        movwf   FSR
        clrf    data_ee_addr
        call    leread
        movlw   .4
        movwf   schleife
        movlw   utemp
        movwf   FSR
        movlw   low(es_konst1)
        movwf   data_ee_addr
        call    leread
        BANKSEL bank1
        LD4     s_konst1,utemp
        BANKSEL bank0
        return
leread:
eel01:
        call    eread
        movwf   INDF
        incf    FSR,F
        incf    data_ee_addr,F
        decfsz  schleife
        goto    eel01
        return
;=========================================================================
lewrite:
ewl01:
        movf    INDF,W
        movwf   data_ee_data
        call    ewrite
        incf    FSR,F
        incf    data_ee_addr,F
        decfsz  schleife
        goto    ewl01
        return
;=========================================================================

;##############################################################################
endepage0:
        if endepage0 > 800h
        error   "PAGE0 ueberschritten"
        endif
        org     800h
PAGE1:
;##############################################################################

;-------------------------------------------------------------------------------
u2pwr:
        andlw   B'00111111'
        addwf   PCL
        retlw   0       ;0
        retlw   0       ;1
        retlw   0       ;2
        retlw   01h     ;3
        retlw   01h     ;4
        retlw   01h     ;5
        retlw   02h     ;6
        retlw   03h     ;7
        retlw   03h     ;8
        retlw   04h     ;9
        retlw   05h     ;a
        retlw   06h     ;b
        retlw   06h     ;c
        retlw   07h     ;d
        retlw   08h     ;e
        retlw   09h     ;f
        retlw   10h     ;10
        retlw   11h     ;11
        retlw   12h     ;12
        retlw   14h     ;13
        retlw   15h     ;14
        retlw   17h     ;15
        retlw   18h     ;16
        retlw   19h     ;17
        retlw   20h     ;18
        retlw   22h     ;19
        retlw   24h     ;1a
        retlw   25h     ;1b
        retlw   27h     ;1c
        retlw   30h     ;1d
        retlw   32h     ;1e
        retlw   34h     ;1f
        retlw   35h     ;20
        retlw   37h     ;21
        retlw   39h     ;22
        retlw   40h     ;23
        retlw   43h     ;24
        retlw   45h     ;25
        retlw   47h     ;26
        retlw   50h     ;27
        retlw   52h     ;28
        retlw   54h     ;29
        retlw   56h     ;2a
        retlw   58h     ;2b
        retlw   60h     ;2c
        retlw   63h     ;2d
        retlw   65h     ;2e
        retlw   67h     ;2f
        retlw   69h     ;30
        retlw   72h     ;31
        retlw   74h     ;32
        retlw   76h     ;33
        retlw   78h     ;34
        retlw   81h     ;35
        retlw   83h     ;36
        retlw   85h     ;37
        retlw   87h     ;38
        retlw   90h     ;39
        retlw   92h     ;3a
        retlw   94h     ;3b
        retlw   96h     ;3c
        retlw   99h     ;3d
        retlw   99h     ;3e
        retlw   99h     ;3f

;------------------------------------------------------------------------------
textc2:         DT      40h,"V2.1 init",0
textc4:         DT      40h,"V2.1   27.09.21",0

textc1:         DT      0,  "DL-QRP-AG UniDDS",0
textc3:         DT      0,  "DDS-VFO(c)DL4JAL",0

;Test fuer Display 4x20

;textc5:         DT      14h,"DDS-VFO(c)DL4JALxxx3",0
;textc6:         DT      54h,"DDS-VFO(c)DL4JALxxx4",0

text3:          DT      0,  "SWR: ",0
text4:          DT      40H,"WpM: ",0
text5:          DT      0  ,"Menu",0
text6:          DT      40H,"scanning",0
text7:          DT      40H,"break   ",0
text8:          DT      .9 ,         "save   ",0
text9:          DT      .9 ,         "cancel ",0
text10:         DT      0  ,"S=0  0 uV   (ok)",0
text11:         DT      0  ,"S=9 50 uV   (ok)",0
text12:         DT      40H,"   VFO+ZF       ",0
text13:         DT      40H,"   VFO-ZF       ",0
text14:         DT      0  ,"first init      ",0
text15:         DT      40h,"please input nr!",0
text16:         DT      0  ,"tuning          ",0

texton:         DT      40H,"   on           ",0
textoff:        DT      40H,"   off          ",0

tbreak:         DT      0,  " 0 break        ",0
tvfoab:         DT      0,  " 1 VFO A/B      ",0
tband:          DT      0,  " 2 Band         ",0
tlight:         DT      0,  " 3 light on/off ",0
tlightauto:     DT      0,  " 4 light auto   ",0
tscan:          DT      0,  " 5 scan         ",0
tsetup:         DT      0,  " 6 SETUP        ",0
tkeyer:         DT      0,  " 7 keyer        ",0
ttune:          DT      0,  " 8 tune         ",0
txit1k:         DT      0,  " 9 xit 1k       ",0
txit2k:         DT      0,  "10 xit 2k       ",0
trit:           DT      0,  "11 rit          ",0

stbreak:        DT      0,  " 0 Setup break  ",0
stddskonst:     DT      0,  " 1 DDS-Takt     ",0
stvfoa          DT      0,  " 2 VFO-A (Hz)   ",0
stvfob          DT      0,  " 3 VFO-B (Hz)   ",0
stzf            DT      0,  " 4 ZF (Hz)      ",0
stscanbeg       DT      0,  " 5 scan-begin   ",0
stscanend       DT      0,  " 6 scan-end     ",0
stsmeter        DT      0,  " 7 S-Meter eich.",0
stvfozf         DT      0,  " 8 VFO +/- ZF   ",0
sttxzf          DT      0,  " 9 TX +/- ZF    ",0
sthw9           DT      0,  "10 HW9-Mode     ",0
stdefault       DT      0,  "11 def. config  ",0

tbbreak:        DT      0,  " 0 Band break   ",0
tb80m:          DT      0,  " 1 80m          ",0
tb40m:          DT      0,  " 2 40m          ",0
tb30m:          DT      0,  " 3 30m          ",0
tb20m:          DT      0,  " 4 20m          ",0
tb17m:          DT      0,  " 5 17m          ",0
tb15m:          DT      0,  " 6 15m          ",0
tb12m:          DT      0,  " 7 12m          ",0
tb10m:          DT      0,  " 8 10m          ",0
tbm:            DT      0  ,"m",0

;==============================================================================

;##############################################################################
endepage1:
        if endepage1 > 1000h
        error   "PAGE1 ueberschritten"
        endif
        org     1000h
PAGE2:
;##############################################################################

;------------------------------------------------------------------------------
smenuanzeige:
        andlw   B'00001111'
        addwf   PCL
        goto    smtbreak
        goto    smtddskonst
        goto    smtvfoa
        goto    smtvfob
        goto    smtzf
        goto    smtscanbeg
        goto    smtscanend
        goto    smtsmeter
        goto    smtvfozf
        goto    smttxzf 
        goto    smthw9  
        goto    smtdefault
        goto    mtbreak
        goto    mtbreak
        goto    mtbreak
        goto    mtbreak
;------------------------------------------------------------------------------
menuanzeige:
        andlw   B'00001111'
        addwf   PCL
        goto    mtbreak
        goto    mtvfoab
        goto    mtband
        goto    mtlight
        goto    mtlightauto
        goto    mtscan
        goto    mtsetup
        goto    mtkeyer
        goto    mttune
        goto    mtxit1k
        goto    mtxit2k
        goto    mtrit
        goto    mtbreak
        goto    mtbreak
        goto    mtbreak
        goto    mtbreak
        goto    mtbreak
        goto    mtbreak
;------------------------------------------------------------------------------
bmenuanzeige:
        andlw   B'00001111'
        addwf   PCL
        goto    tbbreak
        goto    tb80m
        goto    tb40m
        goto    tb30m
        goto    tb20m
        goto    tb17m
        goto    tb15m
        goto    tb12m
        goto    tb10m
        goto    tbbreak 
        goto    tbbreak 
        goto    tbbreak
        goto    tbbreak
        goto    tbbreak
        goto    tbbreak
        goto    tbbreak
;------------------------------------------------------------------------------
smenuausfuehren:
        andlw   B'00001111'
        addwf   PCL
        goto    smbreak
        goto    smddskonst
        goto    smvfoa
        goto    smvfob
        goto    smzf
        goto    smscanbeg
        goto    smscanend
        goto    smsmeter
        goto    smvfozf
        goto    smtxzf
        goto    smhw9
        goto    smdefault_1
        goto    mbreak
        goto    mbreak
        goto    mbreak
        goto    mbreak
;------------------------------------------------------------------------------
menuausfuehren:
        andlw   B'00001111'
        addwf   PCL
        goto    mbreak
        goto    mvfoab
        goto    mrband
        goto    mlight
        goto    mlightauto
        goto    mscan
        goto    msetup
        goto    mkeyer
        goto    mtune
        goto    mxit1k
        goto    mxit2k
        goto    mrit
        goto    mbreak
        goto    mbreak
        goto    mbreak
        goto    mbreak
        goto    mbreak
        goto    mbreak
;------------------------------------------------------------------------------
uagc2s:
        andlw   B'00001111'
        addwf   PCL
        retlw   '0'     ;0
        retlw   '1'     ;1
        retlw   '2'     ;2
        retlw   '3'     ;3
        retlw   '4'     ;4
        retlw   '5'     ;5
        retlw   '6'     ;6
        retlw   '7'     ;7
        retlw   '8'     ;8
        retlw   '9'     ;9
        retlw   '+'     ;10
        retlw   '+'     ;11
        retlw   '+'     ;12
        retlw   '+'     ;13
        retlw   '+'     ;14
        retlw   '+'     ;15
;------------------------------------------------------------------------------
smsmeter:
        PAGESEL PAGE3
        goto    P3smsmeter
P2smsmeter:
        goto    smbreak
;--------------------------------
maxebene        equ     .11     ;
;::::::::::::::::::::::::::::::::
Menuauswertung:                 ;
        clrf    impulse         ;impulse = 0
        PAGESEL PAGE0           ;
        call    LCDDisplayClear ;LCD loeschen
        PAGESEL PAGE2           ;
mausw01:                        ;SCHLEIFE(1)
        PAGESEL PAGE0           ;
        call    tastaturstatus  ;? Taste gedrueckt
        PAGESEL PAGE2           ;
        SKPNC                   ;
        goto    mausw01         ;ENDE(1) warten bis Taste nicht gedrueckt
        movf    ebene,W         ;Menu
        call    menuanzeige     ;Anzeigen
mausw02:                        ;SCHLEIFE(2)
        movf    impulse,W       ;  ? Impulse angefallen
        SKPNZ                   ;
        goto    mausw04         ;
        PAGESEL PAGE0           ;
        call    zs1setzen2sek   ;  1 Timer 2Sek setzen
        PAGESEL PAGE2           ;
        btfsc   impulse,7       ;    ? Vorwaertz
        goto    mausw05         ;
        incf    ebene,F         ;    1 ebene += 1
        movf    ebene,W         ;
        xorlw   maxebene+1      ;      ? max erreicht
        SKPNZ                   ;
        clrf    ebene           ;      1 von 0 beginnen
        goto    mausw06         ;
mausw05:                        ;
        decf    ebene,F         ;    0 eben -= 1
        movf    ebene,W         ;
        xorlw   0ffh            ;      ? min erreicht
        SKPZ                    ;
        goto    mausw06         ;
        movlw   maxebene        ;      1 bei max beginnen
        movwf   ebene           ;
mausw06:                        ;
        PAGESEL PAGE0           ;
        call    LCDDisplayClear ;    neu anzeigen
        PAGESEL PAGE2           ;
        movf    ebene,W         ;
        call    menuanzeige     ;
        clrf    impulse         ;    Impulse loeschen
mausw04:                        ;
        PAGESEL PAGE0           ;
        call    tastaturstatus  ;    Taste abfragen
        PAGESEL PAGE2           ;
        SKPNC                   ;
        goto    mausw03         ;
        btfsc   strichpin       ;
        goto    mausw02         ;ENDE(2) Taste gedrueckt, Strich gedrueckt
        PAGESEL PAGE0           ;
        LCDStr  text16          ;"tuning"
        PAGESEL PAGE2           ;
mausw10:                        ;SCHLEIFE(4)
        btfss   strichpin       ;
        goto    mausw10         ;
        PAGESEL PAGE0           ;
        call    t100mSek        ;
        PAGESEL PAGE2           ;
        btfss   strichpin       ;
        goto    mausw10         ;ENDE(4) kein Strich
        call    Dauerton        ;
        goto    mausw11         ;
mausw03:                        ;SCHLEIFE(3)
        PAGESEL PAGE0           ;
        call    tastaturstatus  ;
        PAGESEL PAGE2           ;
        SKPNC                   ;
        goto    mausw03         ;ENDE(3) Taste nicht gedrueckt
        movf    ebene,W         ;
        call    menuausfuehren  ;
mausw11:                        ;
        BANKSEL bank1           ;
        movlw   0ffh            ;
        movwf   agcmerk+2       ;neuanzeige agc simulieren
        BANKSEL bank0           ;
        return                  ;
;--------------------------------
mflagsp:
        movf    flag1,W
        movwf   data_ee_data
        movlw   low(eflag1)
        movwf   data_ee_addr
        PAGESEL PAGE0
        call    ewrite
        PAGESEL PAGE2
mbreak:
        PAGESEL PAGE0
        call    LCDDisplayClear
        PAGESEL PAGE2
        bsf     blcdneu
        bsf     bddsneu         ;DDS neu ansteuern
        BANKSEL bank1
        movlw   0ffh
        movwf   agcmerk+2       ;neuanzeige agc simulieren
        BANKSEL bank0
        return
mvfoab:
        btfss   bvfo
        goto    mvfoab1
        bcf     bvfo
        goto    mvfoab2
mvfoab1:
        bsf     bvfo
mvfoab2:
        goto    mbreak
;------------------------------------------------------------------------------
mrit:
        bcf     bxit            ;xit aus
        btfss   brit            ;? rit on
        goto    mrit1
        bcf     brit            ;1 rit aus
        btfsc   bvfo            ;  ? VFOa aktiv
        goto    mrit4
        call    vfob2vfoa       ;  1 VFOa = VFOb(tx-frequenz)
        goto    mrit2
mrit4:
        call    vfoa2vfob       ;  1 VFOb = VFOa(tx-frequenz)
        goto    mrit2
mrit1:
        bsf     brit            ;0 rit ein
        btfss   bvfo            ;  ? VFOb aktiv
        goto    mrit3
        call    vfob2vfoa       ;  1 VFOb = VFOa
        goto    mrit2
mrit3:
        call    vfoa2vfob       ;  0 VFOa = VFOb
mrit2:
        goto    mbreak
;------------------------------------------------------------------------------

mkeyer:
        btfss   bkeyer
        goto    mkeyer1
        bcf     bkeyer
        goto    mkeyer2
mkeyer1:
        bsf     bkeyer
mkeyer2
        goto    mflagsp
;------------------------------------------------------------------------------
; Redirektion fuer Band-Menue auf Page 3
mrband:
        PAGESEL PAGE3
        call    mband
        PAGESEL PAGE2
        goto    mbreak
;------------------------------------------------------------------------------
mlight:
        btfsc   blight
        goto    mlight1
        bsf     blight
        bsf     light
        bcf     blauto
        goto    mflagsp
mlight1:
        bcf     blight
        bcf     light
        bsf     blauto
        goto    mflagsp
;------------------------------------------------------------------------------
mtune:
        call    Dauerton
        goto    mbreak
;==============================================================================
vfoa2vfob:
        LD4     frequenzb,frequenza
        return
;==============================================================================
vfob2vfoa:
        LD4     frequenza,frequenzb
        return
;==============================================================================
mxit1k:
        btfsc   bxit
        goto    xit1k03
        btfss   bvfo            ;? VFOb aktiv
        goto    xit1k01
        call    vfob2vfoa       ;1 VFOa=VFOb
        goto    xit1k02
xit1k01:
        call    vfoa2vfob       ;0 VFOb=VFOa
xit1k02:
        bcf     brit            ;rit aus
        bsf     bxit            ;xit ein
        movlw   temp
        movwf   pointer2
        movlw   frequenza
        btfss   bvfo
        movlw   frequenzb
        movwf   pointer1
        PAGESEL PAGE0
        call    cleartemp4
        movlw   10h
        movwf   temp+1
        call    bcdadd4
        PAGESEL PAGE2
        goto    xit1k04
xit1k03:
        bcf     bxit
xit1k04:
        goto    mbreak
;------------------------------------------------------------------------------
mxit2k:
        btfsc   bxit
        goto    xit2k03
        btfss   bvfo
        goto    xit2k01
        call    vfob2vfoa
        goto    xit2k02
xit2k01:
        call    vfoa2vfob
xit2k02:
        bcf     brit
        bsf     bxit
        movlw   temp
        movwf   pointer2
        movlw   frequenza
        btfss   bvfo
        movlw   frequenzb
        movwf   pointer1
        PAGESEL PAGE0
        call    cleartemp4
        movlw   20h
        movwf   temp+1
        call    bcdadd4
        PAGESEL PAGE2
        goto    xit2k04
xit2k03:
        bcf     bxit
xit2k04:
        goto    mbreak
;------------------------------------------------------------------------------
mlightauto:
        btfss   blauto
        goto    mlightauto1
        bcf     blauto
        bcf     light
        bcf     blight
        goto    mlightauto2
mlightauto1:
        PAGESEL PAGE0
        call    zs1setzen2sek
        PAGESEL PAGE2
        bsf     blauto
        bcf     blight
        bsf     light
mlightauto2:
        goto    mflagsp
;------------------------------------------------------------------------------
smtxzf:
        btfss   bddsohnezf
        goto    smtxzf01
        bcf     bddsohnezf
        goto    mflagsp
smtxzf01:
        bsf     bddsohnezf
        goto    mflagsp
;------------------------------------------------------------------------------
smhw9:
        btfss   bhw9
        goto    smhw901
        bcf     bhw9
        goto    mflagsp
smhw901:
        bsf     bhw9
        goto    mflagsp
;------------------------------------------------------------------------------
smvfozf:
        btfss   bzfablage
        goto    smvfozf01
        bcf     bzfablage
        goto    mflagsp
smvfozf01:
        bsf     bzfablage
        goto    mflagsp
;------------------------------------------------------------------------------
smbreak:
        PAGESEL PAGE0
        call    LCDDisplayClear
        goto    start
;------------------------------------------------------------------------------
smddskonst:
        movlw   low(edds)
        bsf     bdds6_075mhz    ;Ausgabe 6,075 MHz
        goto    smedit_hex
;------------------------------------------------------------------------------
smvfoa:
        movlw   low(estarta)
        goto    smedit_bcd
;------------------------------------------------------------------------------
smvfob:
        movlw   low(estartb)
        goto    smedit_bcd
;------------------------------------------------------------------------------
smzf:
        movlw   low(ezf)
        bsf     bzfdirekt       ;ZF-Direktausgabe aktiv
        goto    smedit_hex_bcd
;------------------------------------------------------------------------------
smscanbeg:
        movlw   low(escana)
        goto    smedit_bcd
;------------------------------------------------------------------------------
smscanend:
        movlw   low(escanb)
        goto    smedit_bcd
;------------------------------------------------------------------------------
smdefault_1:
        PAGESEL PAGE0
        call    LCDDisplayClear
        LCDStr  stdefault
        call    t1Sek
        PAGESEL PAGE2
        movlw   b'00001111'
        movwf   temp+8
        call    smedit
        PAGESEL PAGE3
        call    smdefault
        PAGESEL PAGE2
;------------------------------------------------------------------------------
;FUNKTION       editieren von 4-Stelligen HEX oder BCD-werten
;EINGANG        temp+7 Adresse im Eeprom
;               temp+8,0 Bit 1 = BCD; Bit 0 = HEX;
;               temp+8,1 Eingabe HEX Wandlung HEX/BCD BCD/HEX Ergebnis HEX
;               temp+8,2 Bit 1 = kein lesen/schreiben Eeprom
;               temp+8,3 Bit 1 = beginnt an Pos LOW mit edit
;AUSGANG        temp = 1 4 Byte werden in Eeprom zurueckgespeichert
;               temp = 2 keine Speicherung

smedit_hex:
        clrf    temp+8
        goto    smedit
smedit_bcd:
        clrf    temp+8
        bsf     temp+8,0
        goto    smedit
smedit_hex_bcd:
        clrf    temp+8
        bsf     temp+8,0
        bsf     temp+8,1

smedit:
        movwf   temp+7
        movwf   data_ee_addr
        clrf    temp
        CLR4    temp+1
        btfsc   temp+8,2        ;? Lesen aus Eeprom
        goto    smedit01_1
        movlw   .4              ;1 lesen aus dem Eeprom
        movwf   schleife        ;
        movlw   temp+1          ;
        movwf   FSR             ;
        PAGESEL PAGE0           ;
        call    leread          ;
        PAGESEL PAGE2           ;
smedit01_1:
        btfsc   temp+8,1
        call    hex2bcd
        clrf    temp+5
        btfsc   temp+8,3        ;? Editposition aendern
        bsf     temp+5,1        ;1 ja Pos = 2
        call    SAnzeige
        clrf    impulse
smedit01:
        btfsc   taste
        goto    smedit03
smedit04:
        PAGESEL PAGE0
        call    tastaturstatus
        PAGESEL PAGE2
        SKPNC
        goto    smedit04
        movf    temp,W          ;? Cursor nach rechts
        SKPNZ                   ;
        goto    smedit06        ;
        movf    temp,W          ;0 ? save
        xorlw   .1              ;
        SKPZ                    ;
        goto    smedit07        ;
        btfsc   temp+8,1        ;  1 ? bcd2hex
        call    bcd2hex         ;    1 wandeln
        btfsc   temp+8,2        ;    ? speichern in Eeprom
        goto    smedit07_1      ;
        movlw   .4              ;    1 4 Byte   
        movwf   schleife        ;
        movlw   temp+1          ;      Quelle
        movwf   FSR             ;
        movf    temp+7,W        ;      Ziel
        movwf   data_ee_addr    ;
        PAGESEL PAGE0           ;
        call    lewrite         ;      speichern
        PAGESEL PAGE2           ;
smedit07_1:                     ;
        goto    smedit09        ;      ------> break
smedit07:                       ;
        movf    temp,W          ;
        xorlw   .2              ;  ? cancel
        SKPZ                    ;
        goto    smedit08        ;
        btfsc   temp+8,1        ;  1 ? bcd2hex 
        call    bcd2hex         ;    1 wandeln
        goto    smedit09        ;      ------> break
smedit08:                       ;
smedit06:                       ;
        incf    temp+5,F        ;1 Cursor + 1
        movf    temp+5,W        ;
        xorlw   .5              ;  ? Cursor = 5
        SKPNZ                   ;
        clrf    temp+5          ;  1 Cursor = 0
        call    SAnzeige        ;    alles Anzeigen
smedit03:
        movf    impulse,F       ;  ? Impulse
        SKPNZ
        goto    smedit01        ; 
        movlw   temp+4          ;
        movwf   FSR             ;
        movf    temp+5,W        ;
        subwf   FSR             ;
        btfsc   impulse,7       ;
        goto    smedit10        ;
        movlw   .1              ;
        addwf   INDF,F          ;
        btfss   temp+8,0        ;
        goto    smedit10        ;
;dezimalkorrektur
        movf    INDF,W          ;
        movwf   temp+10         ;
        movlw   6               ;
        addwf   temp+10,W       ;
        btfss   STATUS,DC       ;? BCD korrektur low durchfuehren
        goto    smedit12        ;
        movwf   INDF            ;1 ja
smedit12:                       ;
        movf    INDF,W          ;
        movwf   temp+10         ;
        movlw   60H             ;? BCD korrektur high durchfueren
        addwf   temp+10,W       ;
        btfss   STATUS,C        ;
        goto    smedit13        ;
        movwf   INDF            ;1 ja
smedit13:                       ;
smedit10:                       ;
        btfss   impulse,7       ;
        goto    smedit11        ;
        movlw   .1              ;
        subwf   INDF,F          ;
        btfss   temp+8,0        ;
        goto    smedit11        ;
;dezimalkorrektur               ;
        rlf     temp+9,F        ;Carry merken
        btfsc   STATUS,DC       ;
        goto    smedit14        ;
        movlw   6               ;
        subwf   INDF,F          ;
smedit14:                       ;
        btfsc   temp+9,0        ;
        goto    smedit15        ;
        movlw   60H             ;
        subwf   INDF,F          ;
smedit15:                       ;
smedit11:                       ;
        movf    temp,W          ;
        xorlw   .3              ;
        SKPNZ                   ;
        clrf    temp            ;
        movf    temp,W          ;
        xorlw   0ffh            ;
        SKPZ                    ;
        goto    smedit05        ;
        movlw   .2              ;
        movwf   temp            ;
smedit05:                       ;
        call    SAnzeige        ;
        clrf    impulse         ;
        goto    smedit01        ;
smedit09:                       ;
        return                  ;
;------------------------------------------------------------------------------
SAnzeige:
        LD4     utemp,temp              ;temp 8 Byte sichern in Bank1
        BANKSEL bank1
        LD4     tempbank1,utemp
        BANKSEL bank0
        LD4     utemp,temp+4
        BANKSEL bank1
        LD4     tempbank1+4,utemp
        BANKSEL bank0
        LD4     utemp,temp+8
        BANKSEL bank1
        LD4     tempbank1+8,utemp
        BANKSEL bank0

        btfss   bdds6_075mhz
        goto    sanz01_2
        LD4     ddskonst,temp+1
        LDK4    frequenza,6h,07h,50h,0  ;  6,075 Mhz "Deutsche Welle" in Europa
        goto    sanz01_3
sanz01_2:
        btfss   bzfdirekt
        goto    sanz01_1
        LD4     frequenza,temp+1        ;1 Frequenz A mit ZF laden
sanz01_3:
        bsf     bddsohnezf              ;  keine ZF-Berechnung
        bcf     bvfo                    ;  VFO A
        PAGESEL PAGE0
        call    ddsbinausrechnen        ;  DDS neu ausrechnen
        call    tx_to_dds               ;  Sendefrequenz laden
        PAGESEL PAGE2
sanz01_1:
        
        BANKSEL bank1                   ;8 Byte wieder aus Bank1 holen 
        LD4     utemp,tempbank1
        BANKSEL bank0
        LD4     temp,utemp
        BANKSEL bank1
        LD4     utemp,tempbank1+4
        BANKSEL bank0
        LD4     temp+4,utemp
        BANKSEL bank1
        LD4     utemp,tempbank1+8
        BANKSEL bank0
        LD4     temp+8,utemp

        movlw   temp+4
        movwf   FSR
        movlw   .4
        movwf   schleife
        PAGESEL PAGE0
        call    LCDDisplayClear
        movlw   B'00001111'
        call    LCDCom
        PAGESEL PAGE2
sanz01:
        movf    INDF,W
        PAGESEL PAGE0
        call    LCDHEX
        PAGESEL PAGE2
        decf    FSR,F
        decfsz  schleife
        goto    sanz01
        movf    temp,W
        SKPNZ
        goto    sanz02
        xorlw   1
        SKPZ
        goto    sanz03
        PAGESEL PAGE0
        LCDStr  text8
        PAGESEL PAGE2
sanz03:
        movf    temp,W
        xorlw   2
        SKPZ
        goto    sanz04
        PAGESEL PAGE0
        LCDStr  text9
        PAGESEL PAGE2
sanz04:
sanz02:
        LD1     temp+6, temp+5
        clrc
        rlf     temp+6,F
        incf    temp+6,W
        iorlw   40h
        PAGESEL PAGE0
        call    LCDPos
        PAGESEL PAGE2
        return
;------------------------------------------------------------------------------
smtbreak:
        PAGESEL PAGE0
        LCDStr  stbreak
        PAGESEL PAGE2
        return

smtddskonst
        PAGESEL PAGE0
        LCDStr  stddskonst
        PAGESEL PAGE2
        return

smtvfoa
        PAGESEL PAGE0
        LCDStr  stvfoa
        PAGESEL PAGE2
        return

smtvfob
        PAGESEL PAGE0
        LCDStr  stvfob
        PAGESEL PAGE2
        return

smtzf
        PAGESEL PAGE0
        LCDStr  stzf
        PAGESEL PAGE2
        return

smtscanbeg
        PAGESEL PAGE0
        LCDStr  stscanbeg
        PAGESEL PAGE2
        return

smtscanend
        PAGESEL PAGE0
        LCDStr  stscanend
        PAGESEL PAGE2
        return

smtsmeter
        PAGESEL PAGE0
        LCDStr  stsmeter
        PAGESEL PAGE2
        return

smtvfozf
        PAGESEL PAGE0
        LCDStr  stvfozf
        PAGESEL PAGE2
        btfss   bzfablage
        goto    smtvfozf01
        PAGESEL PAGE0
        LCDStr  text13
        PAGESEL PAGE2
        return
smtvfozf01:
        PAGESEL PAGE0
        LCDStr  text12
        PAGESEL PAGE2
        return

smttxzf
        PAGESEL PAGE0
        LCDStr  sttxzf
        PAGESEL PAGE2
        btfsc   bddsohnezf
        goto    smttxzf01
        PAGESEL PAGE0
        LCDStr  textoff
        PAGESEL PAGE2
        return
smttxzf01:
        PAGESEL PAGE0
        LCDStr  texton
        PAGESEL PAGE2
        return
smthw9:
        PAGESEL PAGE0
        LCDStr  sthw9
        PAGESEL PAGE2
        btfss   bhw9
        goto    smthw901
        PAGESEL PAGE0
        LCDStr  textoff
        PAGESEL PAGE2
        return
smthw901:
        PAGESEL PAGE0
        LCDStr  texton
        PAGESEL PAGE2
        return

smtdefault
        PAGESEL PAGE0
        LCDStr  stdefault
        PAGESEL PAGE2
        return

mton:
        PAGESEL PAGE0
        LCDStr  texton
        PAGESEL PAGE2
        return

mtoff:
        PAGESEL PAGE0
        LCDStr  textoff
        PAGESEL PAGE2
        return

mtbreak:
        PAGESEL PAGE0
        LCDStr  tbreak
        PAGESEL PAGE2
        return

mtscan:
        PAGESEL PAGE0
        LCDStr  tscan
        PAGESEL PAGE2
        return

mtsetup:
        PAGESEL PAGE0
        LCDStr  tsetup
        PAGESEL PAGE2
        return

mtkeyer:
        btfss   bkeyer
        goto    mtkeyer1
        call    mtoff
        goto    mtkeyer2
mtkeyer1:
        call    mton
mtkeyer2:
        PAGESEL PAGE0
        LCDStr  tkeyer
        PAGESEL PAGE2
        return

mtrit:
        btfss   brit
        goto    mtrit1
        call    mtoff
        goto    mtrit2
mtrit1:
        call    mton
mtrit2:
        PAGESEL PAGE0
        LCDStr  trit
        PAGESEL PAGE2
        return

mtband:
        PAGESEL PAGE0
        LCDStr  tband
        PAGESEL PAGE2
        return

mtvfoab:
        PAGESEL PAGE0
        LCDStr  tvfoab
        PAGESEL PAGE2
        return

mtlight:
        btfss   blight
        goto    mtlight1
        call    mtoff
        goto    mtlight2
mtlight1:
        call    mton
mtlight2:
        PAGESEL PAGE0
        LCDStr  tlight
        PAGESEL PAGE2
        return

mtlightauto:
        btfss   blauto
        goto    mtlightauto1
        call    mtoff
        goto    mtlightauto2
mtlightauto1:
        call    mton
mtlightauto2:
        PAGESEL PAGE0
        LCDStr  tlightauto
        PAGESEL PAGE2
        return

mttune:
        PAGESEL PAGE0
        LCDStr  ttune
        PAGESEL PAGE2
        return
;------------------------------------------------------------------------------
mtxit1k:
        btfss   bxit
        goto    mtxit1k1
        call    mtoff
        goto    mtxit1k2
mtxit1k1:
        call    mton
mtxit1k2:
        PAGESEL PAGE0
        LCDStr  txit1k
        PAGESEL PAGE2
        return
;------------------------------------------------------------------------------
mtxit2k:
        btfss   bxit
        goto    mtxit2k1
        call    mtoff
        goto    mtxit2k2
mtxit2k1:
        call    mton
mtxit2k2:
        PAGESEL PAGE0
        LCDStr  txit2k
        PAGESEL PAGE2
        return

;==============================================================================
AnzeigeAGC:
        PAGESEL PAGE0           ;
        call    Uagc            ;AGC-Spannung messen
        PAGESEL PAGE2           ;
        call    getswert        ;swert errechnen
        BANKSEL bank1           ;
        movwf   agcmerk+1       ;swert merken
        subwf   agcmerk,W       ;vergleichen mit dem letzten wert
        BANKSEL bank0           ;? AGC neu > AGC alt
        SKPNC                   ;
        goto    AAGC01          ;
        movlw   .100            ;1 Timer neu starten
        movwf   zs2             ;
        BANKSEL bank1           ;
        movf    agcmerk+1,W     ;  neuer wert wird groesster wert
        movwf   agcmerk         ;
        BANKSEL bank0           ;
AAGC01:                         ;
        movf    zs2,F           ;? Timer 1 Sek abgelaufen
        SKPZ                    ;
        goto    AAGC02          ;
        BANKSEL bank1           ;1 neuster wert wird aktuell
        movf    agcmerk+1,W     ;
        movwf   agcmerk         ;
        BANKSEL bank0           ;
AAGC02:                         ;
        BANKSEL bank1           ;
        movf    agcmerk,W       ;? letzer angezeigte wert != neuster wert
        subwf   agcmerk+2,W     ;
        BANKSEL bank0           ;
        SKPNZ                   ;
        goto    AAGC03          ;0 break --> zum Ende
        PAGESEL PAGE0           ;1 AGC neu anzeigen
        movlw   .14 | 40h       ;
        call    LCDPos          ;
        movlw   'S'             ;
        call    LCDChar         ;
        PAGESEL PAGE2           ;
        BANKSEL bank1           ;
        movf    agcmerk,W       ;
        movwf   agcmerk+2       ;
        BANKSEL bank0           ;
        call    uagc2s          ;
        PAGESEL PAGE0           ;
        call    LCDChar         ;
        PAGESEL PAGE2           ;
AAGC03:
        return

;==============================================================================
AnzeigeBand:
        BANKSEL bank0           ;
        movf    band,W
        BANKSEL bank1
        movwf   bandmerk        ;
        subwf   bandmerk+1,W    ;? alter wert != neuer wert
        BANKSEL bank0           ;
        SKPNZ                   ;
        goto    az03            ;0 --> break zum ende
        PAGESEL PAGE0           ;1 neu anzeigen
        movlw   .11             ;  pos in LCD
        call    LCDPos          ;
        BANKSEL bank1           ;
        movf    bandmerk,W      ;  neuen wert merken
        movwf   bandmerk+1      ;
        BANKSEL bank0           ;
        PAGESEL PAGE0           ;  1 Leerzeichen anstelle 0
        call    LCDBand         ;
        PAGESEL PAGE2           ;
az03:
        return                  ;fertig
        
;==============================================================================
Dauerton:
        PAGESEL PAGE0           ;
        call    tx_to_dds       ;
        bsf     senderein       ;
        call    LCDDisplayClear ;
        PAGESEL PAGE2           ;
Dauer01:                        ;
        PAGESEL PAGE0           ;
        call    tastaturstatus  ;
        PAGESEL PAGE2           ;
        SKPNC                   ;
        goto    Dauer02         ;
        btfss   strichpin       ;
        goto    Dauer02         ;
        btfss   punktpin        ;
        goto    Dauer02         ;
        PAGESEL PAGE0           ;
        call    UPowerVor       ;
        movwf   uvor            ;
        call    UPowerRueck     ;
        movwf   urueck          ;
        PAGESEL PAGE2           ;

        call    getswr          ;  SWR errechnen
        PAGESEL PAGE0           ;
        LCDStr  text3           ;  "SWR: "
        PAGESEL PAGE2           ;
        BANKSEL bank1           ;
        movf    swr+2,W         ;  Ganzahliges Ergebnis holen
        BANKSEL bank0           ;
        iorlw   30h             ;
        PAGESEL PAGE0           ;
        call    LCDChar         ;  und anzeigen
        movlw   '.'             ;  Dezimalpunk anzeigen
        call    LCDChar         ;
        BANKSEL bank1           ;
        movf    swr+1,W         ;  Nachkommastelle anzeigen
        BANKSEL bank0           ;
        iorlw   30h             ;
        call    LCDChar         ;

        call    LCDSpace        ;
        movlw   'P'             ;      Leistung anzeigen
        call    LCDChar         ;
        movlw   ':'             ;
        call    LCDChar         ;
        PAGESEL PAGE2           ;
        movf    uvor,W          ;      aus Vorlauf errechnen
        movwf   temp            ;
        clrc                    ;
        rrf     temp,F          ;
        rrf     temp,W          ;
        PAGESEL PAGE1           ;
        call    u2pwr           ;
        movwf   temp            ;
        swapf   temp,W          ;
        andlw   0fh             ;
        iorlw   30h             ;
        PAGESEL PAGE0           ;
        call    LCDChar         ;
        movlw   '.'             ;
        call    LCDChar         ;
        movf    temp,W          ;
        andlw   0fh             ;
        iorlw   30h             ;
        call    LCDChar         ;
        movlw   'W'             ;      P:X.X W
        call    LCDChar         ;
        PAGESEL PAGE2           ;

Dauer03:
        movlw   40h             ;  Bargraph anzeigen in Zeile 2
        PAGESEL PAGE0           ;
        call    LCDPos          ;
        PAGESEL PAGE2           ;
        BANKSEL bank1           ;
        movf    swr,W           ;
        BANKSEL bank0           ;
        movwf   schleife        ;  Balken anzeigen
        movlw   .16             ;
        movwf   schleife+1      ;
Dauer04:                        ;  SCHLEIFE(1)
        movlw   0ffh            ;    volles schwarzes Zeichen
        decf    schleife,F      ;
        btfsc   schleife,7      ;    ? noch Balken anzeigen
        movlw   ' '             ;    0 Leerzeichen
        PAGESEL PAGE0           ;
        call    LCDChar         ;    anzeigen
        PAGESEL PAGE2           ;
        decfsz  schleife+1,F    ;
        goto    Dauer04         ;  ENDE(1) temp+3 = 0
        goto    Dauer01         ;
Dauer02:                        ;
        PAGESEL PAGE0           ;
        call    tastaturstatus  ;
        PAGESEL PAGE2           ;
        SKPNC                   ;
        goto    Dauer02         ;
        PAGESEL PAGE0           ;
        bcf     senderein       ;
        call    t1mSek          ;
        call    rx_to_dds       ;
        call    LCDDisplayClear ;
        PAGESEL PAGE2           ;
        bsf     blcdneu         ;
        return                  ;
        
;==============================================================================
;Funktion       Ausrechnen SWR aus uvor und urueck
;Register       temp, temp+1, temp+2, temp+3, temp+4, temp+5, uvor, urueck, swr x3
;Eingang        uvor, urueck
;Ausgang        swr = Bargraphlaenge, swr+1-swr+2 SWR in Dezimalform

getswr:
        PAGESEL PAGE0
        call    cleartemp       ;temp loeschen
        PAGESEL PAGE2
        movf    uvor,W          ;temp = Uvor - Urueck
        btfsc   STATUS,Z        ;? test auf Uvor = 0
        goto    getswr4         ;1 break zur extremwertkorrektur
        movwf   temp
        movf    urueck,W
        movwf   temp+2
        SUB2    temp, temp+2
        btfss   STATUS,C        ;? Uvor < Urueck
        goto    getswr3         ;1 break zur Extremwertkorrektur
        movf    temp,F          ;? Uvor - Urueck = 0
        btfsc   STATUS,Z        ;
        goto    getswr3         ;1 break zur Extremwertkorrektur
        movf    uvor,W
        movwf   temp+4
        ADD2    temp+2, temp+4  ;temp+2 = Uvor + Urueck
        clrf    temp+5
        decf    temp+5,F
getswr1:                        ;SCHLEIFE(1)
        incf    temp+5,F        ;  Ziffer vor dem Komma durch Subtraktion
        SUB2    temp+2, temp    ;  errechnen
        btfsc   STATUS,C
        goto    getswr1         ;ENDE(1) temp+2 < 0 in temp+5 = Ergebnis vor dem Komma
        ADD2    temp+2, temp    ;0 wurde unterschritten wieder korrigieren
        BMUL10  temp+2          ;Rest mit 10 multiplizieren
        clrf    temp+4          ;Nachkommastelle ermitteln
        decf    temp+4,F        ;Schleifenvorbereitung
getswr2:                        ;SCHLEIFE(2)
        incf    temp+4,F        ;  und nachkommastelle errechnen
        SUB2    temp+2, temp    ;
        btfsc   STATUS,C        ;
        goto    getswr2         ;ENDE(1) temp+2 < 0 in temp+4 = Ergebnis
        ADD2    temp+2, temp    ;Korrektur wenn < 0
        movlw   0ah             ;? test ob SWR > 9,9
        subwf   temp+5,W        ;
        btfss   STATUS,C        ;
        goto    getswr5         ;
getswr3:
        movlw   9               ;1 Anzeige auf 9,9 einstellen
        movwf   temp+4
        movwf   temp+5
        goto    getswr5
getswr4:
        clrf    temp+4          ;Extremwert 0,0 einstellen
        clrf    temp+5
        clrf    temp+3          ;BarGrahp = 0
        goto    getswr8
getswr5:                        ;BarGraph ermitteln
        clrf    temp+3          ;BarGraph = 0
        movlw   2               ;? SWR < 2.0
        subwf   temp+5,W        ;
        btfss   STATUS,C        ;
        goto    getswr6         ;
        decf    temp+5,W        ;0 BarGraph = 9 + (SWR(vorm Komma) - 1)
        movwf   temp+3          ;
        movlw   9               ;
        addwf   temp+3,F        ;
        goto    getswr7
getswr6:
        movf    temp+4,W        ;1 BarGraph = SWR(Nachkomma)
        movwf   temp+3          ;
getswr7:                        ;
        movlw   .18             ;? BarGraph > 18
        subwf   temp+3,W
        btfsc   STATUS,C
        decf    temp+3,F
getswr8:
        movf    temp+3,W
        BANKSEL bank1
        movwf   swr             ;Bargraph
        BANKSEL bank0
        movf    temp+4,W
        BANKSEL bank1
        movwf   swr+1           ;SWR
        BANKSEL bank0
        movf    temp+5,W
        BANKSEL bank1
        movwf   swr+2
        BANKSEL bank0
        return

;==============================================================================
mscan:
        clrf    impulse         ;keine Drehgeberimpulse
        btfsc   blauto          ;? Lichtautomatik
        bcf     light           ;1 Licht aus
        LDK1    step,2          ;Step 50 Hz
        bcf     bvfo            ;VFO A
        movlw   low(escana)     ;Anfangs und
        movwf   data_ee_addr    ;Endfrequenz
        movlw   .8              ;lesen
        movwf   schleife
        movlw   frequenza
        movwf   FSR
        PAGESEL PAGE0
        call    leread
        PAGESEL PAGE2
        call    AnzeigeAGC      ;S-Meter anzeigen
        PAGESEL PAGE0
        LCDStr  text6           ;Zeile2 "scanning"
        PAGESEL PAGE2
        BANKSEL bank1
        LD2     agc, wert       ;S-Wert merken
        BANKSEL bank0
scan01:
        PAGESEL PAGE0           ;SCHLEIFE(1)
        call    LCDAnzeigeZ1    ;  LCD neu anzeigen Zeile1
        btfss   bhw9            ; Im HW9-Mode subtrahieren, anstatt
        goto    scan01add       ; die steps zu addieren...
        call    stepsub         ;  step abziehen
        goto    scan01stp
scan01add
        call    stepadd         ;  step addieren
scan01stp:
        call    ddsbinausrechnen;  DDS neu ausrechnen
        call    t1mSek          ;
        call    rx_to_dds       ;  Empfangsfrequenz laden
        PAGESEL PAGE2
        call    AnzeigeAGC      ;  S_Wert anzeigen
        BANKSEL bank1
        CMP2    agc, wert       ;  ? Spannunganstieg
        BANKSEL bank0
        SKPNC
        goto    scan02
        BANKSEL bank1
        incf    anst,F          ;  1 anstieg+1
        BANKSEL bank0
        goto    scan03
scan02:                         ;  0 anstieg = 0
        BANKSEL bank1
        clrf    anst
        BANKSEL bank0
scan03:
        BANKSEL bank1           ;  ? Anstieg > 3
        CMPK1   anst,3
        BANKSEL bank0
        SKPC
        goto    scan04
        btfsc   blauto          ;  1 ? Lichtautomatik
        bsf     light           ;    1 Licht ein
        movlw   .40             ;  4 Sekunden
        movwf   schleife+2
scan05:
        call    AnzeigeAGC      ;  SCHLEIFE(2)
        BANKSEL bank1
        LD2     agc, wert       ;    S_wert merken
        BANKSEL bank0
        PAGESEL PAGE0
        call    t100mSek        ;    Zeit verstreichen
        PAGESEL PAGE2
        movf    impulse,F       ;
        btfss   STATUS,Z        ;
        goto    scan98          ;    break -> ende
        btfss   punktpin        ;
        goto    scan98          ;    break -> ende
        btfss   strichpin       ;
        goto    scan98          ;    break -> ende
        btfss   taste           ;
        goto    scan98          ;    break -> ende
        decfsz  schleife+2,F
        goto    scan05          ;  ENDE(2) 4 Sekunden

        BANKSEL bank1
        clrf    anst            ;  Anstieg = 0
        BANKSEL bank0
        btfsc   blauto          ;  ? Lichtautomatik
        bcf     light           ;  1 Licht aus
scan04:
        BANKSEL bank1
        LD2     agc, wert       ;
        BANKSEL bank0
        movf    impulse,F       ;
        btfss   STATUS,Z        ;
        goto    scan98          ;    break -> ende
        btfss   punktpin        ;
        goto    scan98          ;    break -> ende
        btfss   strichpin       ;
        goto    scan98          ;    break -> ende
        btfss   taste           ;
        goto    scan98          ;    break -> ende
        CMP4    frequenza, frequenzb;
        SKPC
        goto    scan01          ;ENDE(1) break
        goto    mscan
scan98:
        btfsc   blauto
        bsf     light
        PAGESEL PAGE0
        LCDStr  text7           ;
        PAGESEL PAGE2
scan99:
        btfss   punktpin        ;
        goto    scan99
        btfss   strichpin       ;
        goto    scan99
        btfss   taste           ;
        goto    scan99
        btfsc   blauto
        bcf     light
        clrf    impulse         ;Impulse loeschen
        return

;==============================================================================
msetup:

smaxebene       equ     .11

        clrf    ebene
        clrf    impulse
        PAGESEL PAGE0
        call    LCDDisplayClear
        PAGESEL PAGE2
msausw01:
        PAGESEL PAGE0
        call    tastaturstatus
        PAGESEL PAGE2
        SKPNC
        goto    msausw01
        movf    ebene,W
        call    smenuanzeige
msausw02:
        movf    impulse,W       ;? Impulse angefallen
        SKPNZ
        goto    msausw04
        PAGESEL PAGE0
        call    zs1setzen2sek
        PAGESEL PAGE2
        btfsc   impulse,7       ;1 ? Vorwaertz
        goto    msausw05
        incf    ebene,F         ;  1
        movf    ebene,W
        xorlw   smaxebene+1     ;
        SKPNZ
        clrf    ebene
        goto    msausw06
msausw05:
        decf    ebene,F
        movf    ebene,W
        xorlw   0ffh
        SKPZ
        goto    msausw06
        movlw   smaxebene
        movwf   ebene
msausw06:
        PAGESEL PAGE0
        call    LCDDisplayClear
        PAGESEL PAGE2
        movf    ebene,W
        call    smenuanzeige
        clrf    impulse
msausw04:
        PAGESEL PAGE0
        call    tastaturstatus
        PAGESEL PAGE2
        SKPC
        goto    msausw02
msausw03:
        PAGESEL PAGE0
        call    tastaturstatus
        PAGESEL PAGE2
        SKPNC
        goto    msausw03
        movf    ebene,W
        call    smenuausfuehren
        goto    msetup
        

;==============================================================================
;Funktion       HEX zu BCD wandeln

hex2bcd:
        LD4     tempkonst1, temp+1
        movlw   .8 * 4
        movwf   schleife
        CLR4    temp+1
hextb1:
        movlw   temp+1
        movwf   FSR
        movlw   4
        movwf   schleife+1
hextb4:
        movf    INDF,W
        andlw   B'00001111'
        movwf   temp+5
        movlw   .5
        subwf   temp+5,W
        btfss   STATUS,C
        goto    hextb2
        movlw   .3
        addwf   INDF,F
hextb2:
        movf    INDF,W
        andlw   B'11110000'
        movwf   temp+5
        movlw   50h
        subwf   temp+5,W
        btfss   STATUS,C
        goto    hextb3
        movlw   30h
        addwf   INDF,F
hextb3:
        incf    FSR,F
        decfsz  schleife+1
        goto    hextb4
        bcf     STATUS,C
        RL4     tempkonst1
        RL4     temp+1
        decfsz  schleife,F
        goto    hextb1
        return

;==============================================================================
bcd2hex:
        LD4     tempkonst1, temp+1
        CLR4    temp+1
        movlw   .8 * 4          ;32 bit bcd in bin umwandeln
        movwf   schleife+1
bcd2hex1:
        bcf     STATUS,C
        RR4     tempkonst1
        RR4     temp+1
        movlw   tempkonst1
        movwf   FSR
        movlw   4
        movwf   schleife
bcd2hex3:
        btfss   INDF,7
        goto    bcd2hex4
        movlw   30H
        subwf   INDF,F
bcd2hex4:
        btfss   INDF,3
        goto    bcd2hex5
        movlw   3
        subwf   INDF,F
bcd2hex5:
        incf    FSR,F
        decfsz  schleife,F
        goto    bcd2hex3
        decfsz  schleife+1,F
        goto    bcd2hex1
        return
        
;==============================================================================
;Funktion       dividieren von 2 2Byte-Werten
;Eingang        divisor, divident
;Ausgang        ergebnis, rest im divident
;Register       schleife, divisor, divident

divisor         equ     temp+4
divident        equ     temp+2
ergebnis        equ     temp

divb2:
        clrf    schleife
        CLR2    ergebnis
divb21:
        incf    schleife
        clrc
        btfsc   divisor+1,7
        goto    divb23
        RL2     divisor
        CMP2    divident,divisor
        SKPNC
        goto    divb21
        CLR2    ergebnis
divb22:
        clrc
        RR2     divisor
divb23:
        CMP2    divident,divisor
        SKPC
        goto    divb24
        SUB2    divident,divisor
divb24:
        RL2     ergebnis
        decfsz  schleife
        goto    divb22
        return
        
;------------------------------------------------------------------------------
;Funktion       Mulitplikation von 2 Faktoren 2 Byte lang
;Eingang        faktor1, faktor2
;Ausgang        ergebnis
;Register       pointer1, pointer2, schleife, schleife+1, zw, ueberlauf

faktor1:        equ     temp+2
faktor2:        equ     temp+4
ergebnis:       equ     temp
zw              equ     temp+6
ueberlauf       equ     temp+7

mulb2:
        CLR2    ergebnis
        movlw   .16             ;Schleife laden mit 32 bit
        movwf   schleife
mulb21:
        movlw   faktor2         ;schleife (1)
        movwf   pointer1        ;  pointer1 = faktor2
        movlw   ergebnis
        movwf   pointer2        ;  pointer2 = faktor1
        clrf    ueberlauf       ;  Ueberlauf loeschen
        bcf     STATUS,C        ;  Carry loeschen
        RR2     faktor1
        btfss   STATUS,C        ;  ? Carry = 1 nach RR
        goto    mulb22          ;
        movlw   2               ;  1 Schleife laden mit 8
        movwf   schleife+1
mulb25:
        movf    pointer1,W      ;  schleife (2)
        movwf   FSR             ;    1 Pointer laden
        movf    INDF,W          ;    wert1 holen
        movwf   zw              ;    zwischenspeichern
        movf    pointer2,W      ;    Pointer2 laden
        movwf   FSR             ;    ins indirekte Register
        movf    ueberlauf,W     ;    gemerketen Ueberlauf holen
        clrf    ueberlauf       ;    und loeschen
        addwf   INDF,F          ;    zum Ergebnis addieren
        btfss   STATUS,C        ;    ? Ueberlauf
        goto    mulb24
        movlw   1               ;    1 neuen Ueberlauf
        movwf   ueberlauf       ;      merken
mulb24:
        movf    zw,W            ;    Wert2 = Wert2 + Wert1
        addwf   INDF,F          ;    zum Ergebnis addieren
        btfss   STATUS,C        ;    ? Ueberlauf
        goto    mulb23
        movlw   1               ;    1 neuen Ueberlauf
        movwf   ueberlauf       ;      merken
mulb23:
        incf    pointer1
        incf    pointer2
        decfsz  schleife+1      ;  ende (2)
        goto    mulb25
mulb22:
        bcf     STATUS,C        ;  Carry loeschen
        RL2     faktor2
        decfsz  schleife,F      ;ende (1) 32 mal durchlaufen
        goto    mulb21
        return


;------------------------------------------------------------------------------
getswert:
        PAGESEL PAGE0           ;
        call    Uagc            ;AGC Spannung messen
        PAGESEL PAGE2
        BANKSEL bank1
        LD2     utemp,wert      ;Wert zum Rechnen holen
        BANKSEL bank0
        LD2     temp+2,utemp
        BANKSEL bank1
        LD2     utemp,s_konst1  ;s_konst1 subtrahieren
        BANKSEL bank0
        LD2     temp+4,utemp
        CMP2    temp+2,temp+4   ;? wert > konst1
        SKPC
        goto    gsw01
        SUB2    temp+2,temp+4   ;1
        LDK2    temp+4,0,.10    ;  Ergebnis mit 10 multiplizieren
        call    mulb2
        LD2     temp+2,temp
        BANKSEL bank1
        LD2     utemp,s_konst2
        BANKSEL bank0
        LD2     temp+4,utemp
        movf    temp+5,F        ;  ? s_konst2 = 0
        btfss   STATUS,Z
        goto    gsw03
        movf    temp+4,F
        btfsc   STATUS,Z
        goto    gsw02           ;  1 --> break Ergebnis = 0fH = unendlich
gsw03:
        call    divb2           ;  0 Ergebnis durch s_konst2 dividieren
        movf    temp,W
        andlw   0xf0            ;  ? Ergebnis groesser 0fh
        SKPZ
        goto    gsw02
        movf    temp,W
        movf    temp+1,F        ;  0 ? Ergebnis groesser 0ffh
        SKPZ
gsw02:
        movlw   0x0f            ;    1 Ergebnis = 0fh = max
        return
gsw01:
        clrw                    ;0 Ergebnis = 0 = min
        return

;##############################################################################
endepage2:
        if endepage2 > 1800h
        error   "PAGE2 ueberschritten"
        endif
        org     1800h
PAGE3:
;##############################################################################

nr2vfoH:
        addwf   PCL             ;zum Programmcounter addieren
        retlw   HIGH vfo0
        retlw   HIGH vfo1
        retlw   HIGH vfo2
        retlw   HIGH vfo3
        retlw   HIGH vfo4
        retlw   HIGH vfo5
        retlw   HIGH vfo6
        retlw   HIGH vfo7
        retlw   HIGH vfohw9
;------------------------------------------------------------------------------
nr2vfoL:
        addwf   PCL             ;zum Programmcounter addieren
        retlw   LOW vfo0
        retlw   LOW vfo1
        retlw   LOW vfo2
        retlw   LOW vfo3
        retlw   LOW vfo4
        retlw   LOW vfo5
        retlw   LOW vfo6
        retlw   LOW vfo7
        retlw   LOW vfohw9
;------------------------------------------------------------------------------
nr2ddsL:
        addwf   PCL             ;zum Programmcounter addieren
        retlw   LOW dds0
        retlw   LOW dds1
;------------------------------------------------------------------------------
nr2ddsH:
        addwf   PCL             ;zum Programmcounter addieren
        retlw   HIGH dds0
        retlw   HIGH dds1
;------------------------------------------------------------------------------
nr2zfH:
        addwf   PCL             ;zum Programmcounter addieren
        retlw   HIGH zf0
        retlw   HIGH zf1
        retlw   HIGH zf2
        retlw   HIGH zf2
;------------------------------------------------------------------------------
nr2zfL:
        addwf   PCL             ;zum Programmcounter addieren
        retlw   LOW zf0
        retlw   LOW zf1
        retlw   LOW zf2
        retlw   LOW zf2
;------------------------------------------------------------------------------
konst           de      .3,0                    ;S_Meter konstande 1
                de      .80,0                   ;S_meter konstande 2
;------------------------------------------------------------------------------
; Starts at address 0x3050
;------------------------------------------------------------------------------
vfo0:           de      0x00,0x00,0x81,0x01     ;Startfrequenz
                de      0x00,0x00,0x81,0x01     ;Startfrequenz
                de      0x00,0x00,0x81,0x01     ;Startfrequenz fuer scan
                de      0x00,0x50,0x84,0x01     ;Endfrequenz fuer scan
;------------------------------------------------------------------------------
vfo1:           de      0x00,0x00,0x56,0x03     ;Startfrequenz
                de      0x00,0x00,0x56,0x03     ;Startfrequenz
                de      0x00,0x00,0x50,0x03     ;Startfrequenz fuer scan
                de      0x00,0x00,0x57,0x03     ;Endfrequenz fuer scan
;------------------------------------------------------------------------------
vfo2:           de      0x00,0x00,0x03,0x07     ;Startfrequenz
                de      0x00,0x00,0x03,0x07     ;Startfrequenz
                de      0x00,0x00,0x00,0x07     ;Startfrequenz fuer scan
                de      0x00,0x50,0x03,0x07     ;Endfrequenz fuer scan
;------------------------------------------------------------------------------
vfo3:           de      0x00,0x60,0x11,0x10     ;Startfrequenz
                de      0x00,0x60,0x11,0x10     ;Startfrequenz
                de      0x00,0x00,0x0a,0x10     ;Startfrequenz fuer scan
                de      0x00,0x50,0x12,0x11     ;Endfrequenz fuer scan
;------------------------------------------------------------------------------
vfo4:           de      0x00,0x00,0x06,0x14     ;Startfrequenz
                de      0x00,0x00,0x06,0x14     ;Startfrequenz
                de      0x00,0x00,0x00,0x14     ;Startfrequenz fuer scan
                de      0x00,0x50,0x06,0x14     ;Endfrequenz fuer scan
;------------------------------------------------------------------------------
vfo5:           de      0x00,0x60,0x09,0x18     ;Startfrequenz
                de      0x00,0x60,0x09,0x18     ;Startfrequenz
                de      0x00,0x80,0x06,0x18     ;Startfrequenz fuer scan
                de      0x00,0x00,0x13,0x18     ;Endfrequenz fuer scan
;------------------------------------------------------------------------------
vfo6:           de      0x00,0x00,0x06,0x21     ;Startfrequenz
                de      0x00,0x00,0x06,0x21     ;Startfrequenz
                de      0x00,0x00,0x00,0x21     ;Startfrequenz fuer scan
                de      0x00,0x50,0x06,0x21     ;Endfrequenz fuer scan
;------------------------------------------------------------------------------
vfo7:           de      0x00,0x00,0x06,0x28     ;Startfrequenz
                de      0x00,0x00,0x06,0x28     ;Startfrequenz
                de      0x00,0x00,0x00,0x28     ;Startfrequenz fuer scan
                de      0x00,0x50,0x06,0x28     ;Endfrequenz fuer scan
;------------------------------------------------------------------------------
vfohw9:         de      0xc4,0x8a,0x5b,0x00     ;Startfrequenz            5.9993MHz 0x005B8AC4
                de      0xc4,0x8a,0x5b,0x00     ;Startfrequenz            5.9993MHz 0x005B8AC4
                de      0xc4,0x8a,0x5b,0x00     ;Startfrequenz fuer scan  5.9993MHz 0x005B8AC4
                de      0x34,0xba,0x57,0x00     ;Endfrequenz fuer scan    5.7493MHz 0x0057BA34
;------------------------------------------------------------------------------
zf0:            de      0x18,0x05,0x3d,0x00     ;ZF binaer 3,999 Mhz
;------------------------------------------------------------------------------
zf1:            de      0xec,0xbe,0x86,0x00     ;ZF binaer 8,8307 Mhz
;------------------------------------------------------------------------------
zf2:            de      0x44,0xfd,0x4a,0x00     ;ZF binaer 4,9145 Mhz
;------------------------------------------------------------------------------
dds0:           de      0xb8,0x63,0x5e,0x05     ;DDS konstande 50Mhz
;------------------------------------------------------------------------------
dds1:           de      0xc0,0x4f,0x2f,0x0b     ;DDS konstande 24Mhz
;------------------------------------------------------------------------------
xtal80m:        de      0x90,0xb1,0x17,0x01     ; xtal 80m = 18.33MHz 0x0117B190
;------------------------------------------------------------------------------
xtald:          de      0x00,0x00,0x00,0x00     ; Diff QuarzFreq 80m -> 80m = 0MHz
                de      0xe0,0x67,0x35,0x00     ; " -> 40m =  3.50MHz 0x003567E0
                de      0xa0,0x2e,0x63,0x00     ; " -> 30m =  6.50MHz 0x00632EA0
                de      0xa0,0x37,0xa0,0x00     ; " -> 20m = 10.50MHz 0x00A037A0
                de      0xa0,0x40,0xdd,0x00     ; " -> 17m = 14.50MHz 0x00DD40A0
                de      0x60,0x07,0x0b,0x01     ; " -> 15m = 17.50MHz 0x010B0760
                de      0xd0,0x3f,0x44,0x01     ; " -> 12m = 21.25MHz 0x01443FD0
                de      0x20,0xd7,0x75,0x01     ; " -> 10m = 24.50MHz 0x0175D720

;Auswertung der Urladung
;------------------------------
;0      = 160m
;1      = 80m
;2      = 40m
;3      = 30m
;4      = 20m
;5      = 17m
;6      = 15m
;7      = 10m
;------------------------------
;0      = VFO - ZF
;8      = VFO + ZF
;------------------------------
;0      = TX mit ZF-berechnung
;16     = TX direkt ohne ZF
;------------------------------
;0      = DDS-Takt 50MHz
;32     = DDS-Takt 24MHz
;------------------------------
;0      = ZF 3,999 MHz
;64     = ZF 8,8307 MHz -> switch to HW9 mode
;128    = ZF 4,9145 MHz
;------------------------------

smdefault:
        LD2     temp+8,temp+1   ;2 Kopien da temp+1 gebraucht wird
        LD2     temp+6,temp+1   ;2 Byte kopieren fuer die Zukunft
; Auswertung VFO-Frequenzen
        movf    temp+6,W        ;low byte holen
        btfsc   W,6             ;IF = 8.8307? (HW9-Mode?)
        goto    smsethw9
        andlw   b'00000111'     ;bit 0-2 auswerten
        movwf   temp+6          ;merken
        goto    smsetvfo
smsethw9:
        movlw   .8              ; VFOs für HW9 direkt anwählen (idx=8) 
        movwf   temp+6          ;merken
smsetvfo:
        call    nr2vfoL         ;lowteil der ROM-Adresse
        movwf   temp+1          
        movf    temp+6,W        ;highteil der ROM-Adresse
        call    nr2vfoH
        movwf   temp+2          ;16 Byte
        movlw   .16
        movwf   temp
        movlw   evfo            ;Eepromadresse
        movwf   temp+3
        call    rom2eeprom      ;und umspeichern
; Auswertung DDS-Konstante
        LD1     temp+6,temp+8   ;byte neu holen
        rrf     temp+6,F        ;bit 5 in
        rrf     temp+6,F        ;richtung bit 0
        rrf     temp+6,F        ;schieben
        rrf     temp+6,F
        rrf     temp+6,W
        andlw   b'00000001'     ;bit 0 auswerten
        movwf   temp+6          ;byte merken
        call    nr2ddsL         ;low adresse im rom
        movwf   temp+1
        movf    temp+6,W
        call    nr2ddsH         ;highadresse im rom
        movwf   temp+2
        movlw   .4              ;4 byte
        movwf   temp
        movlw   edds            ;DDSkonstande Adresse im Eeprom
        movwf   temp+3
        call    rom2eeprom      ;und umspeichern
; Auswertung ZF
        LD1     temp+6,temp+8   ; byte neu holen
        rlf     temp+6,F        ;bit 7 und 6 nach links zum bit 0
        rlf     temp+6,F        ;schieben
        rlf     temp+6,W        ;
        andlw   b'00000011'     ;bit 0 und 1 selektieren
        movwf   temp+6          ;byte merken
        call    nr2zfL          ;lowadresse im Rom
        movwf   temp+1
        movf    temp+6,W
        call    nr2zfH          ;highadresse im rom
        movwf   temp+2
        movlw   .4              ;4 Byte
        movwf   temp
        movlw   ezf             ;ZF-Adresse im Eeprom
        movwf   temp+3
        call    rom2eeprom      ;und umspeichern
; Auswertung Keyer/Light/ZF-VFO/HW9-Mode
        movlw   b'00000011'     ;Flags fuer Keyer und lightauto 
        movwf   temp            ;Flagbyte vorbereiten
        btfsc   temp+8,3        ;? ZF-Ablage
        bsf     temp,4          ;1 VFO+ZF
        btfsc   temp+8,4        ;? TX direkt    
        bsf     temp,5          ;1 TX ohne ZF-Ablage
        btfsc   temp+8,6        ;? IF=8.8307 -> HW9-Mode        
        bsf     temp,6          ;1 TX ohne ZF-Ablage
        movwf   data_ee_data    ;Byte Vorbereiten zum speichern
        clrf    data_ee_addr    ;Adresse 0 im Eeprom
        PAGESEL PAGE0
        call    ewrite          ;abspeichern
        movlw   es_konst1       ;S-Meterkonstande 1
        movwf   data_ee_addr    ;
        movlw   .3              ;
        movwf   data_ee_data    ;
        call    ewrite          ;
        incf    data_ee_addr,F  ;
        clrf    data_ee_data    ;
        call    ewrite          ;
        incf    data_ee_addr,F  ;
        movlw   .80             ;
        movwf   data_ee_data    ;
        call    ewrite          ;
        incf    data_ee_addr,F  ;
        clrf    data_ee_data    ;
        call    ewrite          ;
        goto    start           ;und BG neu starten
        
;------------------------------------------------------------------------------
;Funktion       Laden von Daten von ROM in den EEPROM
;Eingang        temp= Anzahl der Bytes
;               temp+1= LOW Adresse im ROM
;               temp+2= HIGH Adresse im ROM
;               temp+3= Adresse im EEPROM
;Ausgang        Daten befinden sich im EEPROM
;Register       temp, temp+1, temp+2, temp+3, W, data_ee_addr, data_ee_data

rom2eeprom:
        movf    temp+3,W
        movwf   data_ee_addr            ;im Eeprom mit Adr 0 beginnend
        movf    temp+1,W                ;
        BANKSEL EEADR
        movwf   EEADR                   ;in Register laden
        BANKSEL bank0   
        movf    temp+2,W
        BANKSEL EEADRH
        movwf   EEADRH                  ;in Register laden
eepromneu1:
        BANKSEL EECON1                  ;SCHLEIFE(1)
        bsf     EECON1, EEPGD           ;  lesen aus dem Programmspeicher
        bcf     INTCON,GIE              ;  Interrupt sperren
        btfsc   INTCON,GIE              ;  warten bis gesperrt
        goto    eepromneu1
        bsf     EECON1, RD              ;  lesen im Rom
        nop
        nop
        bsf     INTCON,GIE              ;  Interrupt wieder freigeben
        BANKSEL EEDATA
        movf    EEDATA, W               ;  daten in W
        BANKSEL data_ee_data
        movwf   data_ee_data            ;  in Eepromdaten laden
        BANKSEL PORTA
        PAGESEL PAGE0
        call    ewrite                  ;  und abspeichern
        PAGESEL PAGE3
        BANKSEL data_ee_addr
        incf    data_ee_addr,F          ;  Eeprom addr + 1
        BANKSEL bank0
        incfsz  temp+1,F                ;  naechste Adresse im Rom LOW
        goto    eepromneu2
        BANKSEL EEADRH                  ;  LOW Ueberlauf HIGH Addr + 1
        incf    EEADRH,F                ;
        BANKSEL bank0                   ;
eepromneu2:
        movf    temp+1,W                ;
        BANKSEL EEADR                   ;
        movwf   EEADR                   ;
        BANKSEL bank0                   ;
        decfsz  temp,F                  ;ENDE(1) 
        goto    eepromneu1              ;
        return
        
;------------------------------------------------------------------------------
P3smsmeter:
        PAGESEL PAGE0           ;
        LCDStr  text10          ;"S0"
        PAGESEL PAGE3           ;
smsm01:                         ;SCHLEIFE(1)
        PAGESEL PAGE0           ;
        call    tastaturstatus  ;
        PAGESEL PAGE3           ;
        SKPNC                   ;
        goto    smsm01          ;ENDE(1) Taste nicht mehr gedrueckt
smsm02:                         ;
        PAGESEL PAGE0           ;SCHLEIFE(2)
        call    Uagc            ;  AGC Spannung messen
        PAGESEL PAGE3           ;
        BANKSEL bank1           ;
        LD2     utemp,wert      ;  Wert holen
        BANKSEL bank0           ;
        LD2     temp,utemp      ;
        movlw   40h             ;
        PAGESEL PAGE0           ;
        call    LCDPos          ;
        PAGESEL PAGE3           ;
        movf    temp+1,W        ;  und Anzeigen
        PAGESEL PAGE0           ;
        call    LCDHEX          ;
        PAGESEL PAGE3           ;
        movf    temp,W          ;
        PAGESEL PAGE0           ;
        call    LCDHEX          ;
        PAGESEL PAGE3           ;
        PAGESEL PAGE0           ;
        call    tastaturstatus  ;
        PAGESEL PAGE3           ;
        SKPC                    ;
        goto    smsm02          ;ENDE(2) Taste gedrueckt
        BANKSEL bank1           ;
        LD2     utemp,wert      ;Wert holen
        LD2     s_konst1,wert   ;und speichern im Ram
        BANKSEL bank0           ;
        movlw   .2              ;
        movwf   schleife        ;
        movlw   utemp           ;
        movwf   FSR             ;
        movlw   low(es_konst1)  ;
        movwf   data_ee_addr    ;
        PAGESEL PAGE0           ;
        call    lewrite         ;und in den Eeprom speichern
        PAGESEL PAGE3           ;

        PAGESEL PAGE0           ;
        LCDStr  text11          ;"S9"
        PAGESEL PAGE3           ;
smsm03:                         ;SCHLEIFE(3)
        PAGESEL PAGE0           ;
        call    tastaturstatus  ;
        PAGESEL PAGE3           ;
        SKPNC                   ;
        goto    smsm03          ;ENDE(3) Taste nicht gedrueckt
smsm04:                         ;
        PAGESEL PAGE0           ;SCHLEIFE(4)
        call    Uagc            ;  AGC-Spannung messen
        BANKSEL bank1           ;
        LD2     utemp,wert      ;
        BANKSEL bank0           ;
        LD2     temp,utemp      ;
        movlw   40h             ;
        call    LCDPos          ;
        movf    temp+1,W        ;
        call    LCDHEX          ;  und Anzeigen
        movf    temp,W          ;
        call    LCDHEX          ;
        call    tastaturstatus  ;
        PAGESEL PAGE3           ;
        SKPC                    ;
        goto    smsm04          ;ENDE(4) Taste gedrueckt
        movlw   40h             ;
        PAGESEL PAGE0           ;
        call    LCDPos          ;
        BANKSEL bank1           ;
        LD2     utemp,wert      ;Wert holen
        LD2     utemp+2,s_konst1;Konstande 1 holen
        BANKSEL bank0           ;
        LD2     temp+2,utemp    ;
        LD2     temp+4,utemp+2  ;
        SUB2    temp+2,temp+4   ;Konstande2 = Wert - Konstande1
        movf    temp+3,W        ;
        call    LCDHEX          ;und Anzeigen
        movf    temp+2,W        ;
        call    LCDHEX          ;
        LDK2    temp+4,0,.10    ;
        PAGESEL PAGE2           ;
        call    mulb2           ;Konstande2 = Konstande2 * 10
        PAGESEL PAGE3           ;
        movf    temp+1,W        ;
        PAGESEL PAGE0           ;
        call    LCDHEX          ;und Anzeigen
        movf    temp,W          ;
        call    LCDHEX          ;
        PAGESEL PAGE3           ;
        LD2     temp+2,temp     ;
        LDK2    temp+4,0,.9     ;
        PAGESEL PAGE2           ;
        call    divb2           ;Konstande2 = Konstande2 / 9
        PAGESEL PAGE3           ;
        movf    temp+1,W        ;
        PAGESEL PAGE0           ;
        call    LCDHEX          ;und Anzeigen
        movf    temp,W          ;
        call    LCDHEX          ;
        movlw   .2              ;
        movwf   schleife        ;
        movlw   temp            ;
        movwf   FSR             ;
        movlw   low(es_konst2)  ;
        movwf   data_ee_addr    ;
        call    lewrite         ;Konstande2 in Eeprom speichern
        PAGESEL PAGE2           ;
        goto    P2smsmeter      ;wieder zurueck in PAGE2 springen
;------------------------------------------------------------------------------

;==============================================================================
mband:

bmaxebene       equ     .8

        movf    band,W            ; Initial das aktuelle Band setzen
        incf    W,0
        movwf   ebene
        clrf    impulse           ; Impulse zuruecksetzen
        PAGESEL PAGE0
        call    LCDDisplayClear
        PAGESEL PAGE3
mbausw01:
        PAGESEL PAGE0
        call    tastaturstatus
        PAGESEL PAGE3
        SKPNC
        goto    mbausw01
        movf    ebene,W
        PAGESEL PAGE2
        call    bmenuanzeige
        PAGESEL PAGE3
mbausw02:
        movf    impulse,W       ;? Impulse angefallen
        SKPNZ
        goto    mbausw04
        PAGESEL PAGE0
        call    zs1setzen2sek
        PAGESEL PAGE3
        btfsc   impulse,7       ;1 ? Vorwaertz
        goto    mbausw05
        incf    ebene,F         ;  1
        movf    ebene,W
        xorlw   bmaxebene+1     ;
        SKPNZ
        clrf    ebene
        goto    mbausw06
mbausw05:
        decf    ebene,F
        movf    ebene,W
        xorlw   0ffh
        SKPZ
        goto    mbausw06
        movlw   bmaxebene
        movwf   ebene
mbausw06:
        PAGESEL PAGE0
        call    LCDDisplayClear
        PAGESEL PAGE3
        movf    ebene,W
        PAGESEL PAGE2
        call    bmenuanzeige
        PAGESEL PAGE3
        clrf    impulse
mbausw04:
        PAGESEL PAGE0
        call    tastaturstatus
        PAGESEL PAGE3
        SKPC
        goto    mbausw02
mbausw03:
        PAGESEL PAGE0
        call    tastaturstatus
        PAGESEL PAGE3
        SKPNC
        goto    mbausw03
        movf    ebene,W
        addlw   0x00           ; wenn ebene == 0 stoppe submenu
        btfss   STATUS,Z
        goto    mbstore
        return
mbstore:
        decf    W,0            ; ebene - 1 als neue bandauswahl speichern
        movwf   band
        call    bandconst
        goto    mband
        

bandconst:
        movlw   .4              ; 4 Bytes lesen
        movwf   temp
        movlw   low(xtald)      ;lowteil der ROM-Adresse merken
        movwf   temp+1          
        movlw   high(xtald)     ;highteil der ROM-Adresse merken
        movwf   temp+2          ;

        movf    band,W          ; Aktuelle Band-Selektion
        movwf   temp+3          ; mit 4 multiplizieren
        rlf     temp+3,f        ; und die LOW-Adresse
        rlf     temp+3,f        ; entsprechend erhöhen.
        rlf     temp+3,f        ; Warnung: Aktuelle Annahme dass
        movf    temp+3,W        ; es zu keinem WrapAround der
        addwf   temp+1,f        ; LOW-Adresse kommt! Ggf. in HEX-File überpruefen!!!

        movlw   bconst          ; Indir. Adressierung auf bconst
        movwf   FSR             ; als Start-Adresse für Ziel setzen
rom2temp:
        movf    temp+1,W                ; Low+High Adresse im ROM setzen
        BANKSEL EEADR
        movwf   EEADR                   ;in Register laden
        BANKSEL bank0   
        movf    temp+2,W
        BANKSEL EEADRH
        movwf   EEADRH                  ;in Register laden
romnext1:
        BANKSEL EECON1                  ;SCHLEIFE(1)
        bsf     EECON1, EEPGD           ;  lesen aus dem Programmspeicher
        bcf     INTCON,GIE              ;  Interrupt sperren
        btfsc   INTCON,GIE              ;  warten bis gesperrt
        goto    romnext1
        bsf     EECON1, RD              ;  lesen im Rom
        nop
        nop
        bsf     INTCON,GIE              ;  Interrupt wieder freigeben
        BANKSEL EEDATA
        movf    EEDATA, W               ;  daten in W
        BANKSEL bank0
        movwf   INDF                    ; Byte in Indir. Ziel speichern
        incf    FSR, f                  ; Ziel-Adresse weiterzaehlen
        incfsz  temp+1,F                ;  naechste Adresse im Rom LOW
        goto    romnext2
        BANKSEL EEADRH                  ;  LOW Ueberlauf HIGH Addr + 1
        incf    EEADRH,F                ;
        BANKSEL bank0                   ;
romnext2:
        movf    temp+1,W                ;
        BANKSEL EEADR                   ;
        movwf   EEADR                   ;
        BANKSEL bank0                   ;
        decfsz  temp,F                  ;ENDE(1) 
        goto    romnext1                ;
        clrf    FSR                     ; Indir. Adressierung zuruecksetzen

        
        movlw   bconst                  ; Zu Banddiff (aus ROM) die Frequenz
        movwf   pointer1                ; des 80m-XTALs addieren
        movlw   xtal80                  ; und das Ergebnis
        movwf   pointer2                ; wieder in bconst speichern...
        PAGESEL PAGE0
        call    cleartemp4
        call    bcdadd4
        PAGESEL PAGE2

        movlw   bconst                  ; Von bisheriger Konstante
        movwf   pointer1                ; die IF abziehen
        movlw   zwischenfrequenz        ; und das Ergebnis
        movwf   pointer2                ; wieder in bconst speichern...
        PAGESEL PAGE0
        call    cleartemp4
        call    bcdsub4
        PAGESEL PAGE2

        return
        
;==============================================================================
        nop
        nop
        END


