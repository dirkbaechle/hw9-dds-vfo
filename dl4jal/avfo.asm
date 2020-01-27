 ;##############################################################################                          
 ;DDS-VFO fuer alle Baender.
 ;
 ;16.6.2008
 ;(c) DL4JAL
 ;##############################################################################
 ;History
 ;13.7.2008
 ;HW zum testen aufgebaut
 ;30.7 Interrupt Timer2 aktiviert fuer Keyer
 ;03.01.09 Setup erweitert alle ZF
 ;23.07.09 Umstellung von PIC18F452 auf PIC18F4520
 ;          Version auf 1.02 erhoeht
 ;          Variantenumschaltung konstruiert
 ;
 ;01.09.09 neu V1.03
 ;Fehler in Routine "ad9951_frq32bit_to_dds" Zeile 3673
 ;
 ;02.09.09 neu V1.04
 ;Fehler bei Funktion 100kHz; geloest
 ;SW: Setup 23,24 hinzugekommen. Speichern und Abrufen der Konfiguration im Flash
 ;es gibt 4 Speicherplaetze fuer die Konfiguration und die Default-Konfiguration
 ;Setup 24 kann auch durch PowerON+Taste1 aufgerufen werden
 ;
 ;04.09.09 neu V1.05
 ;Eeprom Auffrischungsroutine hinzugefuegt;
 ;wurde MT aktiviert kann jetzt mit jeder Taste deaktiviert werden ohne eine
 ;Funktion auszuloesen;
 ;
 ;06.09.09 neu V1.06
 ;bei txpermzf==0 nur bei CW und CWr keine ZF-Berechnung, alles ander ist bei TX mit ZF
 ;bei ZF 9MHz VFO unterhalb und RX 3,5MHz schwingt VFO auf 5,5MHz, das wird jetzt erkannt
 ; RX-Frq 3,5 minus ZF 9,0 = (-)5,5MHz ;wird in Absolutwert gewandelt
 ;BUG in MODE-Erkennung bei RX-ZF=VFO
 ;
 ;23.09.09 neu V1.07
 ;BUG im Setup ZF-Zuordnung pro Mode
 ;
 ;08.11.09 neu V1.08
 ;aendern Mode DIGr in FM: Varinate 3 und 23
 ;beim Einschalten von der RIT werden VFO-Freqeuenzen gemerkt und beim Ausschalten
 ;wieder hergestellt.
 ;Wird VFOx4 aktiv wird kein Mode im Display dargestellt sondern "MHz"
 ;Keyerpotiabfrage bei Handtastung abschalten
 ;
 ;;;;;;; Assembler directives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;          list   P=18f452, F=INHX32, C=160, N=0, ST=ON, MM=ON, X=ON

 ;------------------------------------------------------------------------------
 ;bytevariante: equ      .1      ;Variante PIC18F452 englisch
 ;bytevariante: equ      .2      ;Variante PIC18F452 deutsch
 ;bytevariante: equ      .3      ;Variante PIC18F452 deutsch FM an Stelle DIr
 ;bytevariante: equ      .21     ;Variante PIC18F4520 englisch
 ;bytevariante: equ      .22     ;Variante PIC18F4520 deutsch
 bytevariante:   equ     .23     ;Variante PIC18F4520 deutsch FM an Stelle DIr
 ;------------------------------------------------------------------------------

 ;------------------------------------------------------------------------------
         if      bytevariante >= .21     ;alle Varianten mit PIC18F4520
 ;------------------------------------------------------------------------------
 #include        p18f4520.inc
 ;------------------------------------------------------------------------------
         endif
 ;------------------------------------------------------------------------------
 ;------------------------------------------------------------------------------
         if      bytevariante < .21      ;alle Varianten mit PIC18F452
 ;------------------------------------------------------------------------------
 #include        p18f452.inc
 ;------------------------------------------------------------------------------
         endif
 ;------------------------------------------------------------------------------

 #include           makros_pic18.inc
 ;#define                   mathetest
 ;#define                   debug           ;debuglauf auf dem PC
 ;#define                   test1

 ;------------------------------------------------------------------------------
         if      bytevariante < .21      ;alle Varianten mit PIC18F452
;------------------------------------------------------------------------------
         __CONFIG _CONFIG1H, _HS_OSC_1H                                 ;HS oscillator
;          __CONFIG _CONFIG2L, _PWRT_ON_2L & _BOR_ON_2L & _BORV_42_2L   ;Reset
         __CONFIG _CONFIG2L, _PWRT_OFF_2L & _BOR_OFF_2L & _BORV_42_2L   ;Reset
         __CONFIG _CONFIG2H, _WDT_OFF_2H                                ;Watchdog timer disabled
          __CONFIG _CONFIG3H, _CCP2MX_ON_3H                             ;CCP2 to RC1 (rather than  to RB3)
          __CONFIG _CONFIG4L, _LVP_OFF_4L                               ;RB5 enabled for I/O
;------------------------------------------------------------------------------
          endif
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
        if      bytevariante >= .21     ;alle Varianten mit PIC18F4520
;------------------------------------------------------------------------------
    __CONFIG _CONFIG1H, _OSC_HS_1H & _FCMEN_OFF_1H & _IESO_OFF_1H
    __CONFIG _CONFIG2L, _PWRT_OFF_2L & _BOREN_OFF_2L & _BORV_3_2L
    __CONFIG _CONFIG2H, _WDT_OFF_2H & _WDTPS_1_2H
    __CONFIG _CONFIG3H, _CCP2MX_PORTC_3H & _PBADEN_OFF_3H & _LPT1OSC_OFF_3H & _MCLRE_OFF_3H
    __CONFIG _CONFIG4L, _STVREN_OFF_4L & _LVP_OFF_4L & _XINST_OFF_4L & _DEBUG_OFF_4L
    __CONFIG _CONFIG5L, _CP0_OFF_5L & _CP1_OFF_5L & _CP2_OFF_5L & _CP3_OFF_5L
    __CONFIG _CONFIG5H, _CPB_OFF_5H & _CPD_OFF_5H
    __CONFIG _CONFIG6L, _WRT0_OFF_6L & _WRT1_OFF_6L & _WRT2_OFF_6L & _WRT3_OFF_6L
    __CONFIG _CONFIG6H, _WRTC_OFF_6H & _WRTB_OFF_6H & _WRTD_OFF_6H
    __CONFIG _CONFIG7L, _EBTR0_OFF_7L & _EBTR1_OFF_7L & _EBTR2_OFF_7L & _EBTR3_OFF_7L
    __CONFIG _CONFIG7H, _EBTRB_OFF_7H
;------------------------------------------------------------------------------
        endif
;------------------------------------------------------------------------------


;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
var     udata_acs 0x000                         ;Beginning of Access RAM

W_TEMP:                 res     1       ;variable used for context saving
STATUS_TEMP:            res     1       ;variable used for context saving
BSR_TEMP:               res     1       ;variable used for context saving
FSR0L_TEMP:             res     1       ;variable used for context saving
FSR0H_TEMP:             res     1       ;variable used for context saving
data_ee_addr:           res     1       ;Zwischenspeicher interne Eepromadr
data_ee_data:           res     1       ;Zwischenspeicher interne Eepromdaten
LCDByte:                res     1       ;Zwischenspeicher fuer LCD-Ausgabe
LCDByte2:               res     1       ;Zwischenspeicher fuer Stringausgabe

ramanfang:              res     0       ;
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;dieser Bereich haengt zusammen im Eeprom Beginn
ddskonst:               res     5       ;Quarzoszikonstande
step:                   res     1       ;Schrittweite 0=1Hz 1=10Hz 2=50Hz 3=1000Hz
band:                   res     1       ;Band 160 - 2m = 0 - 11
flag1:                  res     1       ;Verschiedene Flags die mit im Eeprom gespeichert werden
ddstype                 res     1       ;ddstype 0=ad9833/34 1=ad9850 2=ad9851 3=ad9951x4 4=ad9951x5
                                        ;5=ad9951x20 6=ad9951x1
stimer                  res     1       ;Timer fuer verzoegert Sender AUS
;dieser Bereich haengt zusammen im Eeprom Ende
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
zwischenfrequenz:       res     4       ;Frequenz ZF
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;dieser Bereich haengt zusammen im Eeprom Beginn
frequenza:              res     5       ;Frequenz VFO-A
frequenzb:              res     5       ;Frequenz VFO-B
modeA:                  res     1       ;
modeB:                  res     1       ;
lcdoffset:              res     5       ;Displayoffset
;dieser Bereich haengt zusammen im Eeprom Ende
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
flag2:                  res     1       ;Verschiedene Flags
flag3:                  res     1       ;Verschiedene Flags
flag4:                  res     1       ;Verschiedene Flags
flag5:                  res     1       ;Verschiedene Flags
bandneu:                res     1       ;Band 160 - 2m = 0 - 11
mode:                   res     1       ;Mode Zwischenspeicher
modeneu:                res     1       ;neu eingestellter Mode

CounterA:               res     1       ;Zellen fuer Delay
CounterB:               res     1       ;Zellen fuer Delay
CounterC:               res     1       ;Zellen fuer Delay

tastennummer:           res     1       ;Nummer der gedrueckten Taste
 impulse:                res     1       ;Drehimpulse                                                        
 ;..............................................................................
 ergebnis:               res     0       ;ergebnis fuer mathe-funktion
 temp:                   res     D'10'   ;temporaere Speicher
 op1:                    res     0       ;
 minuend:                res     0       ;minuend fuer mathe-funktion
 summand1:               res     0       ;summand fuer mathe-funktion
 divident:               res     0       ;divident fuer mathe-funktion
 faktor1:                res     0       ;faktor1 fuer mathe-funktion
 tempkonst1:             res     8       ;rechenspeicher
 op2:                    res     0       ;
 subtrahend:             res     0       ;subtrahend fuer mathe-funktion
 summand2:               res     0       ;summand fuer mathe-funktion
 divisor:                res     0       ;divisor fuer mathe-funktion
 faktor2:                res     0       ;faktor2 fuer mathe-funktion
 tempkonst2:             res     8       ;nicht trennen da gemeinsam clr
 ;..............................................................................
 ebene:                  res     1       ;Nummer der Menueebene
 zs1:                    res     1       ;Zeitschleife 1
 zs2:                    res     2       ;Zeitschleife 2 Sender AUS
 zs3:                    res     1       ;Zeitschleife fuer CW (Punktlaenge im Interr.)
 keyergeschw:            res     2       ;Keyergeschwindigkeit
 schleife:               res     4       ;fuer schleifen

 ddsbinrx:               res     4       ;binaerwert fr DDS laden Empfangsfrequenz
 ddsbintx:               res     4       ;binaerwert fr DDS laden Sendefrequenz
 tmr0const               res     1       ;Constande fuer Mithoerton
 ubatt:                  res     2       ;Batteriespannung
 uvor:                   res     2       ;gemessene Vorlaufleistung 0..3ff
 urueck:                 res     2       ;gemessene Ruechlaufleistung 0..3ff
 urit:                   res     2       ;gemessene Spannung am RIT-Poti
 usmeter:                res     3       ;gemessene Spannung S-Meter
 ukeyer:                 res     4       ;Keyerspannungen

 swr:                    res     3       ;Zwischenspeicher fuer SWR u. BarGraph
 tempindex:              res     1       ;Index fuer Tabellenroutinen
 zw:                     res     1       ;zwischenspeicher
 string:                 res     D'12'   ;fuer Stringausgabe
 laenge:                 res     1       ;fuer Stringausgabe
 komma:                  res     1       ;fuer Stringausgabe
 kuerzen:                res     1       ;fuer Stringausgabe
 mess:                   res     0       ;Ergebnis der Spannungsmessung
 messl:                  res     1       ;L-Teil
 messh:                  res     1       ;H:Teil
 zeitschlitz:            res     1       ;Zaehler fuer die Zeitschlitze
 sr0                     res     1       ;Register fuer AD9951
 ddsword:                res     2       ;DDS-Word
 ddsdword:               res     4       ;DDS-DoppelWord
 setupnr:                res     1       ;Zaehler SETUP
 stemp                   res     1       ;fuer SETUP
 stemp1                  res     5       ;fuer SETUP
 mddskonst               res     5       ;fuer SETUP
 swrmerke                res     2       ;das SWR merken
 bargraph                res     3       ;Bytes fuer Bargarphanzeige
 flpointer               res     3       ;Pointer fuer Flash schreiben
 frqmerk:                res     .12     ;Frequenz Mode merken bei RIT
 ;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 ;dieser Bereich haengt zusammen im Eeprom Beginn
 xsm                     res     2       ;X Konstande fuer S-Meter
 ysm                     res     2       ;Y Konstande fuer S-Meter
 mithoerton:             res     1       ;Mithoerton fuer ADD oder SUB zur DDS-Frequ.
 ;dieser Bereich haengt zusammen im Eeprom Ende
 ;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 FSRL_rs232_write        res     1       ;LOW in Ringbuffer RS232 schreiben
 FSRH_rs232_write        res     1       ;HIGH in Ringbuffer RS232 schreiben
 FSRL_rs232_read         res     1       ;LOW aus Ringbuffer RS232 lesen
 FSRH_rs232_read         res     1       ;HIGH aus Ringbuffer RS232 lesen
 CAT_in_byte             res     1       ;Merker fuer Byte aus rs232buffer
 catbefehl               res     .10     ;Befehlsspeicher
 endecatbefehl           res     0       ;ende markieren
 ;..............................................................................
 ramende:                res     0       ;Merker fuer Ramende im Bankbereich 0

 var3                    udata   0x300   ;Zwischenbuffer fuer Flashspeichern
 flbuffer                res     .256    ;

 var4                    udata   0x400   ;BSR RAM
 txbuffer                res     0xff    ;

 var5                    udata   0x500   ;BSR RAM
start_CAT_buffer:         res        0xf0   ;RX/TX Ring-Buffer
end_CAT_buffer:           res        0      ;Buffer ende

;;;;;;; Definitionen ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;; Definitionen ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;FLAG1 wird mit gespeichert im Eeprom
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#define blcdsmeter      flag1,0         ;Anstelle Frequenz das S-Meter anzeigen
#define bbeleuchtung    flag1,1         ;bit fuer Beleuchtung
#define blauto          flag1,2         ;Lichautomatik
#define b1hzanz         flag1,3         ;1Hz Umschaltung moeglich
#define bkeyerein       flag1,4         ;Keyer ein/aus
#define btxpermzf       flag1,5         ;Beim Senden keine ZF addieren oder subtrahieren
#define bbandmode       flag1,6         ;Aktive Bandumschaltung mit RS232 ausgang
#define bvfox4          flag1,7         ;VFO x 4 fuer I/Q-Mischer
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;FLAG1 wird mit gespeichert im Eeprom
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#define   bddsneu         flag2,0           ;1=DDS neu ausrechnen und laden
#define   bmenu           flag2,1           ;Menuanzeige
#define   tonenable       flag2,2           ;Tonenable
#define   LCDr            flag2,3           ;Statusspeicher fuer LCD beim Busylesen
#define   bvfo            flag2,4           ;0=frequenza 1=frequenzb
#define   bnull           flag2,5           ;Bit fuer fuehrende Null auf LCD
#define   btaste          flag2,6           ;Bit fuer Taste wurde gedrueckt
#define   bzs1ausein      flag2,7           ;Merkbit Zeit1 gestartet

#define   bzs1einaus      flag3,0           ;Merkbit Zeit1 abgelaufen
#define   bpunkt          flag3,1           ;ob Keyerpunkt gedrueckt war
#define   bstrich         flag3,2           ;ob Keyerstrich gedrueckt war
#define   brit            flag3,3           ;fuer Rit
#define   bnofunktion     flag3,4           ;Bit fuer wichtige Funktion
#define   block           flag3,5           ;fuer LOCK
#define   blcdneu         flag3,6           ;1=frequenz zeile1 neu anzeigen
#define   ubattneu        flag3,7           ;Batteriespannung neu anzeigen

#define   bkeyeranz       flag4,0           ;Keyergeschw. anzeigen
#define   bohnezf         flag4,1           ;ohne ZF
#define   bminuszf        flag4,2           ;Flag fuer ZF subtrahieren
#define   bergebisminus   flag4,3           ;Flag Ergebnis ist Minus
#define   bintsperren     flag4,4           ;Interrupts sperren
#define   bkein_mt        flag4,5           ;keinen Mithoerton


;==============================================================================
;Namen der I/O Leitungen
;==============================================================================

#define   e_DrehgeberC    PORTB,0H          ;Drehgebertakt
#define   e_strichpin     PORTB,1H          ;Keyer
#define   e_punktpin      PORTB,2H          ;Keyer
#define   e_DrehgeberD    PORTB,3H          ;Drehgeberrichtung

#define   e_Taste1        PORTB,4h          ;Taste1
#define   e_Taste2        PORTB,5h          ;Taste2
#define   e_Taste3        PORTB,6h          ;Taste3
#define   e_Taste4        PORTB,7h          ;Taste4

LCDTris   equ             TRISD             ;Einstellung fuer Port LCD
LCDPort   equ             PORTD             ;Port fuer LCDAusgabe
#define   a_LCD_RS        PORTC,3h          ;RS Pin an LCD
#define   a_LCD_RW        PORTC,4h          ;R/W Pin
#define   a_LCD_E         PORTD,3h          ;Enable
#define   ea_LCD_D4       PORTD,4h          ;Daten von LCD
#define   ea_LCD_D5       PORTD,5h          ;Daten von LCD
#define   ea_LCD_D6       PORTD,6h          ;Daten von LCD
#define   ea_LCD_D7       PORTD,7h          ;Daten von LCD

#define a_senderein       PORTD,0h          ;Sender einschalten
#define a_tastungein      PORTD,1h          ;Sender Tastung

#define a_licht           PORTD,2h          ;LCD Hintergrundbeleuchtung

#define a_mton            PORTA,4H          ;Mithoerton

 #define   a_fsync         PORTC,0h        ;DDS_BG                                                        
 #define   a_sclk          PORTC,1h        ;DDS_BG
 #define   a_sdata         PORTC,2h        ;DDS_BG
 #define   a_reset         PORTC,5h        ;DDS_BG

 ;=========================================================================
 ; EEPROM Zellen
 ;=========================================================================

 ;---------------------------------------------
 ;Adressberechnungen fuer das laufende Programm
 ;---------------------------------------------

 ddskonstlaenge          equ     5
 zflaenge                equ     4       ;ZF Bereich -2.147.483.648 Hz bis 2.147.483.647 Hz
 frequenzlaengeband      equ     4       ;Frequenz Bereich -2.147.483.648 Hz bis 2.147.483.647 Hz
 lcdoffsetlaenge         equ     5       ;LCD-Offset Bereich -549.755.813.888 Hz bis 549.755.813.8 87 Hz
 modelaenge              equ     1
 steplaenge              equ     1
 bandlaenge              equ     1
 flaglaenge              equ     1
 ddstypelaenge           equ     1
 zflagelaenge            equ     2
 smlaenge                equ     2
 ;-----------------------------------------------------------------------------------
 ;Festlegung der Datenlaengen im Eeprom
 grunddaten      equ     ddskonstlaenge + steplaenge + bandlaenge + flaglaenge + ddstypelaenge + 1  + (4*zflaenge)
 bandgeslaenge   equ     (2*frequenzlaengeband) + modelaenge + modelaenge
 trvbandlaenge   equ     (2*frequenzlaengeband) + lcdoffsetlaenge + modelaenge + modelaenge + 1
 ;------------------------------------------------------------------------------
 ;Festlegung der Adressen im Eeprom
 addrbeginn              equ     0
 addrddskonst            equ     addrbeginn                      ;0 1 2 3 4
 addrstep                equ     addrbeginn + ddskonstlaenge     ;5
 addrband                equ     addrstep + steplaenge           ;6
 addrflag                equ     addrband + bandlaenge           ;7
 addrddstype             equ     addrflag + flaglaenge           ;8 9
 addrzfcw                equ     addrddstype + ddstypelaenge + 1 ;A B C D    Ausgleich zu geraden Zahl
 addrzfcwr               equ     addrzfcw + zflaenge             ;E F 10 11
 addrzflsb               equ     addrzfcwr + zflaenge            ;12 13 14 15
 addrzfusb               equ     addrzflsb + zflaenge            ;16 17 18 19
 addrzfdig               equ     addrzfusb + zflaenge            ;1a 1b 1c 1d
 addrzfdigr              equ     addrzfdig + zflaenge            ;1e 1f 20 21
 addrxsm                 equ     addrzfdigr + zflaenge           ;22 23
 addrysm                 equ     addrxsm + smlaenge              ;24 25

 ;13 Baender
 addrbandbeginn            equ     0x28
 addrmess                  equ     addrbandbeginn   +   (0*bandgeslaenge)
 addr160m                  equ     addrbandbeginn   +   (1*bandgeslaenge)
 addr80m                   equ     addrbandbeginn   +   (2*bandgeslaenge)
 addr60m                   equ     addrbandbeginn   +   (3*bandgeslaenge)
 addr40m                   equ     addrbandbeginn   +   (4*bandgeslaenge)
 addr30m                   equ     addrbandbeginn   +   (5*bandgeslaenge)
 addr20m                   equ     addrbandbeginn   +   (6*bandgeslaenge)
 addr17m                   equ     addrbandbeginn   +   (7*bandgeslaenge)
 addr15m                   equ     addrbandbeginn   +   (8*bandgeslaenge)
 addr12m                   equ     addrbandbeginn   +   (9*bandgeslaenge)
 addr10m                   equ     addrbandbeginn   +   (.10*bandgeslaenge)
 addr6m                    equ     addrbandbeginn   +   (.11*bandgeslaenge)
 addr2m                    equ     addrbandbeginn   +   (.12*bandgeslaenge)

 ;5 Transverterbaender
 transverterbeginn       equ     addrbandbeginn + (.13*bandgeslaenge)
 addrtrvband1            equ     transverterbeginn + (0*trvbandlaenge)
 addrtrvband2            equ     transverterbeginn + (1*trvbandlaenge)
 addrtrvband3            equ     transverterbeginn + (2*trvbandlaenge)
 addrtrvband4            equ     transverterbeginn + (3*trvbandlaenge)
 addrtrvband5            equ     transverterbeginn + (4*trvbandlaenge)
 ;==============================================================================
 d_t0con                 equ     b'11000100'

 ;bit 7 TMR0ON:   Timer0 On/Off Control
         ;bit 1   = Enables Timer0
         ;bit 0   = Stops Timer0
 ;bit 6 T08BIT:   Timer0 8-bit/16-bit Control
         ;bit 1   = Timer0 is configured as an 8-bit timer/counter
        ;bit 0 = Timer0 is configured as a 16-bit timer/counter
;bit 5 T0CS: Timer0 Clock Source Select
        ;bit 1 = Transition on T0CKI pin
        ;bit 0 = Internal instruction cycle clock (CLKO)
;bit 4 T0SE: Timer0 Source Edge Select
        ;bit 1 = Increment on high-to-low transition on T0CKI pin
        ;bit 0 = Increment on low-to-high transition on T0CKI pin
;bit 3 PSA: Timer0 Prescaler Assignment
        ;bit 1 = TImer0 prescaler is NOT assigned. Timer0 clock input bypasses prescaler.
        ;bit 0 = Timer0 prescaler is assigned. Timer0 clock input comes from prescaler output.
;bit 2-0 T0PS2:T0PS0: Timer0 Prescaler Select
        ;bits 111 = 1:256 prescale value
        ;110 = 1:128 prescale value
        ;101 = 1:64 prescale value
        ;100 = 1:32 prescale value
        ;011 = 1:16 prescale value
        ;010 = 1:8 prescale value
        ;001 = 1:4 prescale value
        ;000 = 1:2 prescale value
;******************************************************************************

;d_t1con                equ     b'00010001'
d_t1con          equ    b'00000001'

;       R/W-0 R/W-0    R/W-0    R/W-0  R/W-0 R/W-0 R/W-0
;       RD16 ‚M-^@M-^T T1CKPS1 T1CKPS0 T1OSCEN T1SYNC TMR1CS TMR1ON
;
;bit 7 RD16: 16-bit Read/Write Mode Enable bit
;       1 = Enables register Read/Write of Timer1 in one 16-bit operation
;       0 = Enables register Read/Write of Timer1 in two 8-bit operations
;bit 6 Unimplemented: Read as '0'
;bit 5-4 T1CKPS1:T1CKPS0: Timer1 Input Clock Prescale Select bits
;       11 = 1:8 Prescale value
;       10 = 1:4 Prescale value
;       01 = 1:2 Prescale value
;       00 = 1:1 Prescale value
;bit 3 T1OSCEN: Timer1 Oscillator Enable bit
;       1 = Timer1 Oscillator is enabled
;       0 = Timer1 Oscillator is shut-off
;The oscillator inverter and feedback resistor are turned off to eliminate power drain.
;bit 2 T1SYNC: Timer1 External Clock Input Synchronization Select bit
;When TMR1CS = 1:
;1 = Do not synchronize external clock input
;0 = Synchronize external clock input
;When TMR1CS = 0:
;This bit is ignored. Timer1 uses the internal clock when TMR1CS = 0.
;bit 1 TMR1CS: Timer1 Clock Source Select bit
;1 = External clock from pin RC0/T1OSO/T13CKI (on the rising edge)
;0 = Internal clock (FOSC/4)
;bit 0 TMR1ON: Timer1 On bit
;1 = Enables Timer1
;0 = Stops Timer1
;******************************************************************************
;T2CON: TIMER2 CONTROL REGISTER

d_t2con          equ     B'00100001'
;Postscale = 5   Anpassung an 20MHz
;Prescaler = 4

;bit 7 Unimplemented: Read as '0'
;bit 6-3 TOUTPS3:TOUTPS0: Timer2 Output Postscale Select bits
; 0000 = 1:1 Postscale
; 0001 = 1:2 Postscale
; 0010 = 1:3 Postscale
; 0011 = 1:4 Postscale
; 0100 = 1:5 Postscale
;   .
;   .
;   .
; 1111 = 1:16 Postscale
;bit 2 TMR2ON: Timer2 On bit
; 1 = Timer2 is on
; 0 = Timer2 is off
;bit 1-0 T2CKPS1:T2CKPS0: Timer2 Clock Prescale Select bits
; 00 = Prescaler is 1
; 01 = Prescaler is 4
;   .
;   .
;   .
; 1x = Prescaler is 16
 ;                                                                                                        
 ;******************************************************************************
 d_t3con         equ     B'00000000'

 ;T3CON: TIMER3 CONTROL REGISTER
 ;R/W-0 R/W-0 R/W-0 R/W-0 R/W-0 R/W-0 R/W-0 R/W-0
 ;RD16 T3CCP2 T3CKPS1 T3CKPS0 T3CCP1 T3SYNC TMR3CS TMR3ON
 ;
 ;bit 7 RD16: 16-bit Read/Write Mode Enable bit
 ; 1 = Enables register Read/Write of Timer3 in one 16-bit operation
 ; 0 = Enables register Read/Write of Timer3 in two 8-bit operations
 ;bit 6-3 T3CCP2:T3CCP1: Timer3 and Timer1 to CCPx Enable bits
 ; 1x = Timer3 is the clock source for compare/capture CCP modules
 ; 01 = Timer3 is the clock source for compare/capture of CCP2,
 ; Timer1 is the clock source for compare/capture of CCP1
 ; 00 = Timer1 is the clock source for compare/capture CCP modules
 ;bit 5-4 T3CKPS1:T3CKPS0: Timer3 Input Clock Prescale Select bits
 ; 11 = 1:8 Prescale value
 ; 10 = 1:4 Prescale value
 ; 01 = 1:2 Prescale value
 ; 00 = 1:1 Prescale value
 ;bit 2 T3SYNC: Timer3 External Clock Input Synchronization Control bit
 ; (Not usable if the system clock comes from Timer1/Timer3)
 ; When TMR3CS = 1:
 ; 1 = Do not synchronize external clock input
 ; 0 = Synchronize external clock input
 ; When TMR3CS = 0:
 ; This bit is ignored. Timer3 uses the internal clock when TMR3CS = 0.
 ;bit 1 TMR3CS: Timer3 Clock Source Select bit
 ; 1 = External clock input from Timer1 oscillator or T1CKI
 ; (on the rising edge after the first falling edge)
 ; 0 = Internal clock (FOSC/4)
 ;bit 0 TMR3ON: Timer3 On bit
 ; 1 = Enables Timer3
 ; 0 = Stops Timer3
 ;******************************************************************************

 d_intcon        equ     B'11110000'

 ;bit 7 GIE/GIEH: Global Interrupt Enable bit
 ;       When IPEN = 0:
 ;       1 = Enables all unmasked interrupts
 ;       0 = Disables all interrupts
 ;       When IPEN = 1:
 ;       1 = Enables all high priority interrupts
 ;       0 = Disables all interrupts
 ;bit 6 PEIE/GIEL: Peripheral Interrupt Enable bit
 ;       When IPEN = 0:
 ;       1 = Enables all unmasked peripheral interrupts
 ;       0 = Disables all peripheral interrupts
 ;       When IPEN = 1:
 ;       1 = Enables all low priority peripheral interrupts
 ;       0 = Disables all low priority peripheral interrupts
 ;bit 5 TMR0IE: TMR0 Overflow Interrupt Enable bit
 ;       1 = Enables the TMR0 overflow interrupt
 ;       0 = Disables the TMR0 overflow interrupt
 ;bit 4 INT0IE: INT0 External Interrupt Enable bit
 ;       1 = Enables the INT0 external interrupt
 ;       0 = Disables the INT0 external interrupt
 ;bit 3 RBIE: RB Port Change Interrupt Enable bit
 ;       1 = Enables the RB port change interrupt
 ;       0 = Disables the RB port change interrupt
 ;bit 2 TMR0IF: TMR0 Overflow Interrupt Flag bit
 ;       1 = TMR0 register has overflowed (must be cleared in software)
 ;       0 = TMR0 register did not overflow
 ;bit 1 INT0IF: INT0 External Interrupt Flag bit
 ;       1 = The INT0 external interrupt occurred (must be cleared in software)
 ;       0 = The INT0 external interrupt did not occur
 ;bit 0 RBIF: RB Port Change Interrupt Flag bit
 ;       1 = At least one of the RB7:RB4 pins changed state (must be cleared in software)
 ;       0 = None of the RB7:RB4 pins have changed state
 ;******************************************************************************

 d_intcon2       equ     B'00000000'

 ;bit 7 RBPU: PORTB Pull-up Enable bit
 ;       1 = All PORTB pull-ups are disabled
 ;       0 = PORTB pull-ups are enabled by individual port latch values
 ;bit 6 INTEDG0:External Interrupt0 Edge Select bit
 ;       1 = Interrupt on rising edge
;       0 = Interrupt on falling edge
;bit 5 INTEDG1: External Interrupt1 Edge Select bit
;       1 = Interrupt on rising edge
;       0 = Interrupt on falling edge
;bit 4 INTEDG2: External Interrupt2 Edge Select bit
;       1 = Interrupt on rising edge
;       0 = Interrupt on falling edge
;bit 3 Unimplemented: Read as '0'
;bit 2 TMR0IP: TMR0 Overflow Interrupt Priority bit
;       1 = High priority
;       0 = Low priority
;bit 1 Unimplemented: Read as '0'
;bit 0 RBIP: RB Port Change Interrupt Priority bit
;       1 = High priority
;       0 = Low priority
;******************************************************************************

d_intcon3:     equ     B'00000000'

;bit 7 INT2IP: INT2 External Interrupt Priority bit
;       1 = High priority
;       0 = Low priority
;bit 6 INT1IP: INT1 External Interrupt Priority bit
;       1 = High priority
;       0 = Low priority
;bit 5 Unimplemented: Read as '0'
;bit 4 INT2IE: INT2 External Interrupt Enable bit
;       1 = Enables the INT2 external interrupt
;       0 = Disables the INT2 external interrupt
;bit 3 INT1IE: INT1 External Interrupt Enable bit
;       1 = Enables the INT1 external interrupt
;       0 = Disables the INT1 external interrupt
;bit 2 Unimplemented: Read as '0'
;bit 1 INT2IF: INT2 External Interrupt Flag bit
;       1 = The INT2 external interrupt occurred (must be cleared in software)
;       0 = The INT2 external interrupt did not occur
;bit 0 INT1IF: INT1 External Interrupt Flag bit
;       1 = The INT1 external interrupt occurred (must be cleared in software)
;       0 = The INT1 external interrupt did not occur
;******************************************************************************

d_rcon:        equ     B'00000000'

;bit 7 IPEN: Interrupt Priority Enable bit
;       1 = Enable priority levels on interrupts
;       0 = Disable priority levels on interrupts (16CXXX Compatibility mode)
;bit 6-5 Unimplemented: Read as '0'
;bit 4 RI: RESET Instruction Flag bit For details of bit operation, see Register 4-3
;bit 3 TO: Watchdog Time-out Flag bit For details of bit operation, see Register 4-3
;bit 2 PD: Power-down Detection Flag bit For details of bit operation, see Register 4-3
;bit 1 POR: Power-on Reset Status bit For details of bit operation, see Register 4-3
;bit 0 BOR: Brown-out Reset Status bit For details of bit operation, see Register 4-3
;******************************************************************************
;------------------------------------------------------------------------------
        if      bytevariante < .21      ;alle Varianten mit PIC18F452
;------------------------------------------------------------------------------
d_adcon1:               equ     b'10000000'      ;
;------------------------------------------------------------------------------
        endif
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
        if      bytevariante >= .21     ;alle Varianten mit PIC18F4520
;------------------------------------------------------------------------------
d_adcon1:               equ     b'00000111'      ;
d_adcon2:               equ     b'10000110'      ;
;------------------------------------------------------------------------------
        endif
;------------------------------------------------------------------------------

;bit 7 ADFM: A/D Result Format Select bit
;       1 = Right justified. 6 Most Significant bits of ADRESH are read as 0.
;       0 = Left justified. 6 Least Significant bits of ADRESL are read as 0.
;bit 6-4 Unimplemented: Read as '0'
;bit 3-0 PCFG3:PCFG0: A/D Port Configuration Control bits:
;******************************************************************************

d_pie1:                equ     b'00100011'

;bit 7 PSPIE(1): Parallel Slave Port Read/Write Interrupt Enable bit
;       1 = Enables the PSP read/write interrupt
 ;       0 = Disables the PSP read/write interrupt                                                      
 ;bit 6 ADIE: A/D Converter Interrupt Enable bit
 ;       1 = Enables the A/D interrupt
 ;       0 = Disables the A/D interrupt
 ;bit 5 RCIE: USART Receive Interrupt Enable bit
 ;       1 = Enables the USART receive interrupt
 ;       0 = Disables the USART receive interrupt
 ;bit 4 TXIE: USART Transmit Interrupt Enable bit
 ;       1 = Enables the USART transmit interrupt
 ;       0 = Disables the USART transmit interrupt
 ;bit 3 SSPIE: Master Synchronous Serial Port Interrupt Enable bit
 ;       1 = Enables the MSSP interrupt
 ;       0 = Disables the MSSP interrupt
 ;bit 2 CCP1IE: CCP1 Interrupt Enable bit
 ;       1 = Enables the CCP1 interrupt
 ;       0 = Disables the CCP1 interrupt
 ;bit 1 TMR2IE: TMR2 to PR2 Match Interrupt Enable bit
 ;       1 = Enables the TMR2 to PR2 match interrupt
 ;       0 = Disables the TMR2 to PR2 match interrupt
 ;bit 0 TMR1IE: TMR1 Overflow Interrupt Enable bit
 ;       1 = Enables the TMR1 overflow interrupt
 ;       0 = Disables the TMR1 overflow interrupt
 ;******************************************************************************
 d_pie2                  equ     b'00000010'

 ;bit 7-5 Unimplemented: Read as '0'
 ;bit 4 EEIE: Data EEPROM/FLASH Write Operation Interrupt Enable bit
 ;       1 = Enabled
 ;       0 = Disabled
 ;bit 3 BCLIE: Bus Collision Interrupt Enable bit
 ;       1 = Enabled
 ;       0 = Disabled
 ;bit 2 LVDIE: Low Voltage Detect Interrupt Enable bit
 ;       1 = Enabled
 ;       0 = Disabled
 ;bit 1 TMR3IE: TMR3 Overflow Interrupt Enable bit
 ;       1 = Enables the TMR3 overflow interrupt
 ;       0 = Disables the TMR3 overflow interrupt
 ;bit 0 CCP2IE: CCP2 Interrupt Enable bit
 ;       1 = Enables the CCP2 interrupt
 ;       0 = Disables the CCP2 interrupt
 ;******************************************************************************
 d_ipr1                  equ     b'00000000'

 ;bit 7 PSPIP(1): Parallel Slave Port Read/Write Interrupt Priority bit
 ;       1 = High priority
 ;       0 = Low priority
 ;bit 6 ADIP: A/D Converter Interrupt Priority bit
 ;       1 = High priority
 ;       0 = Low priority
 ;bit 5 RCIP: USART Receive Interrupt Priority bit
 ;       1 = High priority
 ;       0 = Low priority
 ;bit 4 TXIP: USART Transmit Interrupt Priority bit
 ;       1 = High priority
 ;       0 = Low priority
 ;bit 3 SSPIP: Master Synchronous Serial Port Interrupt Priority bit
 ;       1 = High priority
 ;       0 = Low priority
 ;bit 2 CCP1IP: CCP1 Interrupt Priority bit
 ;       1 = High priority
 ;       0 = Low priority
 ;bit 1 TMR2IP: TMR2 to PR2 Match Interrupt Priority bit
 ;       1 = High priority
 ;       0 = Low priority
 ;bit 0 TMR1IP: TMR1 Overflow Interrupt Priority bit
 ;       1 = High priority
 ;       0 = Low priority
 ;******************************************************************************
 d_ipr2                   equ    b'00000000'

 ;bit   7-5 Unimplemented: Read as '0'
 ;bit   4 EEIP: Data EEPROM/FLASH Write Operation Interrupt Priority bit
 ;         1 = High priority
 ;         0 = Low priority
 ;bit   3 BCLIP: Bus Collision Interrupt Priority bit
 ;         1 = High priority
 ;         0 = Low priority
 ;bit   2 LVDIP: Low Voltage Detect Interrupt Priority bit
 ;         1 = High priority
;        0 = Low priority
;bit 1 TMR3IP: TMR3 Overflow Interrupt Priority bit
;        1 = High priority
;        0 = Low priority
;bit 0 CCP2IP: CCP2 Interrupt Priority bit
;        1 = High priority
;        0 = Low priority
;##############################################################################
;CONSTANDEN
;##############################################################################
;Daten fuer Timer1 = verschiedene Zeittimer
;Eine Zeiteinheit betraegt 10 mSek
;------------------------------------------------------------------------------
pictakt                  equ    .18432          ;in kHz
tmr1word                 equ    (pictakt * .5) / .4
tmr1lconst               equ    LOW (0-tmr1word)
tmr1hconst               equ    HIGH (0-tmr1word)
;------------------------------------------------------------------------------
;Daten fuer Timer0 = Mithoerton
;------------------------------------------------------------------------------
tonhoehe                 equ    .68
tmr0wert                 equ    pictakt * .100 / (.8 * .32)
d_tmr0lconst             equ    LOW (0-tmr0wert)
;------------------------------------------------------------------------------
d_trisa:                  equ   b'00101111'      ;RA0,1,2,3,5 analoge Eingaenge
d_trisb:                  equ   b'11111111'      ;Grundeinstellung PORTB
d_trisc:                  equ   b'10000000'      ;
d_trisd:                  equ   b'00000000'      ;
d_trise:                  equ   b'00000111'      ;Port E analoge Eingaenge
d_porta                  equ    b'00101111'     ;
d_portb                  equ    b'11111111'     ;
d_portc                  equ    b'00100111'     ;
d_portd                  equ    b'00000000'     ;
d_porte                  equ    b'00000111'     ;

modecw                  equ     1               ;
modecwr                 equ     2               ;
modelsb                 equ     3               ;
modeusb                 equ     4               ;
modedig                 equ     5               ;
modedigr                equ     6               ;
;##############################################################################
;##############################################################################
;;;;;;; Vectors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        org     0x0000                  ;Reset vector
        bra     main
        org     0x0008                  ;High priority interrupt vector
        goto    interrupt1              ;Trap
        org     0x0018                  ;Low priority interrupt vector
        goto    interrupt2              ;Trap
;------------------------------------------------------------------------------
;Funktion:      Wandlung BCDZahl unteres Nibbel in ASCIIzeichen
;Eingang:       BCD in W unteres Nibbel
;Ausgang:       entsprechendes Ascii-Zeichen in W
;==============================================================================
bcd2ascii_1b:
        andlw   B'00001111'
        movwf   tempindex
        movlw   high(dbcd2hex)
        movwf   TBLPTRH
        movlw   low(dbcd2hex)
indexlesen:
        movwf   TBLPTRL
        movf    tempindex,W
        addwf   TBLPTRL,F
        btfsc   STATUS,C
        incf    TBLPTRH,F
        tblrd*
        movf    TABLAT,W
        return
dbcd2hex:
        db      "0123456789ABCDEF"
;------------------------------------------------------------------------------
getBandadr:
        movf    band,W
        andlw   B'00011111'
        movwf   tempindex
        movlw   high(dbandadr)
        movwf   TBLPTRH
         movlw   low(dbandadr)                                                                            
         bra     indexlesen
 dbandadr:
         db      addrmess, addr160m, addr80m, addr60m
         db      addr40m, addr30m, addr20m, addr17m
         db      addr15m, addr12m, addr10m, addr6m
         db      addr2m, addrtrvband1, addrtrvband2, addrtrvband3
         db      addrtrvband4, addrtrvband5
 ;------------------------------------------------------------------------------
 getZFadr:
         andlw   B'00000111'
         movwf   tempindex
         movlw   high(dzfadr)
         movwf   TBLPTRH
         movlw   low(dzfadr)
         bra     indexlesen
 dzfadr:
         db      addrzfcw, addrzfcwr, addrzflsb, addrzfusb, addrzfdig, addrzfdigr

 ukey2msek:
         movf    keyergeschw+1,W
         movwf    tempindex
         clrc
         rrcf    tempindex,F
         rrcf    tempindex,F
         rrcf    tempindex,W
         andlw   B'00011111'
         movwf   tempindex
         movlw   high(keyertab)
         movwf   TBLPTRH
         movlw   low(keyertab)
         bra     indexlesen

 keyertab:       db      .133,   .120, .109, .100, .92, .86, .80, .75, .71, .67
                 db      .63,    .60, .57, .55, .52, .50, .48, .46, .44, .43
                 db      .41,    .40, .39, .38, .36, .35, .34, .33, .32, .32
                 db      .31,    .30

 ;==============================================================================
 ;;;;;;; Mainline program ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;Konstanden
 maxmenu:        equ     .4      ;4 Menuepunkte
 maxmenumitband: equ     .12     ;12 Menuepunkte
 version:        equ     .100    ;Version 1.00
 kxsm:           equ     200h    ;auch mess S0
 kysm:           equ     100h    ;auch mess S9


 main:
         rcall   init            ;Initialize everything
         bsf     a_reset         ;
         call    rs232init       ;
 ;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 ;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 ;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 ;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         ifdef   mathetest
         MOVLF   1,band
         MOVLF   .4,step
         call    umschalten_step
         endif
 ;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 ;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 ;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         rcall   LCDInit         ;Initialisierung LCD
 ;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 ;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 ;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 ;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         ifdef   mathetest
         rcall   mtausrechnen    ;Mithoerton ausrechnen
         endif
 ;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 ;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 ;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
 ;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
         bsf     blcdneu         ;LCD neu anzeigen
         bsf     bddsneu         ;DDS einstellen
         MOVLF   .1,ebene        ;Menugrundstellung
         MOVLF   .16,zeitschlitz ;Zeitschlitz einstellen
        MOVLF   .255,data_ee_addr;
        call    eread           ;
;........................................
;        movlw  0
;        call   LCDPos
;        movf   data_ee_data,W          ;
;        call   LCDHEX                  ;
;        call   t1Sek                   ;
;        goto   $
;........................................
        movf    data_ee_data,W ;
        sublw   version         ;? richtige Version
        bnz     main002         ;0 --> Break Urladen
        rcall   Tastegedrueckt ;? Taste gedrueckt
        bnc     main00          ;
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;Laderoutine Konfiguration. Der Eeprom wird mit den Banddaten programmiert
;Power ON + Taste 1 ganz links
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        rcall   Tastaturstatus ;1 ? Tasttaturabfrage Taste 1
        movlw   1               ;
        subwf   tastennummer,W ;
        bnz     main003
        call    readsicherung   ; letzte Sicherung einlesen
        rcall   mtausrechnen    ; Mithoerton ausrechnen
        rcall   zs1laden        ; Timer 1Sek laden
        goto    flash2ee        ;
main003:
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;Urladeroutine. Der Eeprom wird mit den Banddaten programmiert
;Power ON + Taste 4 ganz rechts
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        rcall   Tastaturstatus ;1 ? Tasttaturabfrage Taste 4
        movlw   4               ;
        subwf   tastennummer,W ;
        bnz     main001         ;
main002:
        rcall   LCDDisplayClear ; 1 Taste 4 Power ON behandeln
        LCDStrp text1po         ;    Text Ladebeginn
        call    urladung        ;    Urladung Eeprom beschreiben
        rcall   LCDDisplayClear ;    Text Laden beendet
        LCDStrp text2po         ;
        rcall   t1Sek           ;    1 Sekunde warten
        rcall   quittungsdton   ;    Quittungston doppelt
main001:                        ;SCHLEIFE(1)
        rcall   Tastegedrueckt ; ? Taste gedrueckt
        bc      main001         ;ENDE(1) Taste nicht gedrueck
        goto    main            ;Neustart
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;------------------------------------------------------------------------------
main00:                         ;
        LCDStrp qtext28         ;Text "Eeprom refresh "
        clrf    schleife        ;256 Byte
        clrf    data_ee_addr    ;Adresse 0 im Eeprom
main000:                        ;SCHLEIFE(1)
        call    eread           ; Byte lesen
        call    ewrite          ; wieder schreiben
        incf    data_ee_addr,F ; Adresse+1
        decfsz schleife,F       ;
        bra     main000         ;ENDE(1) 256 Byte aufgefrischt
        call    t500mSek        ;ein bischen warten
        call    readsicherung   ;letzte Sicherung einlesen
        rcall   mtausrechnen    ;Mithoerton ausrechnen
        rcall   quittungston    ;Quittungston ausgeben
        movff   band,bandneu    ;Bandumschaltung gueltig machen
        call    ebandread       ;und Werte lesen sonst stehen im Ram falsche Werte
        btfsc   bbandmode       ;? aktive Umschaltung mit Tasten fuer Mode und Band
        bra     main00a         ;
        call    ubandmessen     ;0 Messen welches Band eingestellt ist
        call    umodemessen     ; Messen welcher Mode eingestellt ist
main00a:                        ;
        call    bandrs232       ;
        call    moders232       ;
        call    bandrs232       ;
        call    moders232       ;
        call    ddsinit         ;DDS init
        btfsc   blauto          ;? Lichautomatik
        bsf     a_licht         ;1 Hintergrundbeleuchtung ein
        btfsc   bbeleuchtung    ;? Dauerlicht
        bsf     a_licht         ;1 Hintergrundbeleuchtung ein
          MOVLF   1,step          ;10Hz Anzeige einstellen                                               
          btfsc   b1hzanz         ;? 1Hz Anzeige
          clrf    step            ;1 1Hz Anzeige einstellen
          rcall   zs1laden        ;Timer 1Sek laden
main01:                           ;SCHLEIFE(1)
        btfsc     a_tastungein    ; ? Sendertastung AUS
        bra       main0011        ;
        btfsc     a_senderein     ; 1 ? Sender noch EIN
        rcall     test_s_timerende;    1 test ob die Zeit abgelaufen ist
main0011:                         ;
        bsf       bnofunktion     ; BIT setzen keine Funktion durchgefuehrt
        movf      impulse,F       ; ? Impulse angefallen
        bz        main02          ;
        btfsc     blauto          ; 1 ? Lichautomatik
        bsf       a_licht         ;    1 Hintergrundbeleuchtung ein
        bcf       bnofunktion     ;    BIT loeschen keine Funktion durchgefuehrt
        rcall     zs1laden        ;    Timer 1Sek laden
        movf      impulse,W       ;    Impulse holen
        btfss     bmenu           ;    ? Menue aktiv
        bra       main014         ;
        btfsc     impulse,7       ;    1 ? positive Drehrichtung
        bra       main015         ;
        andlw     B'01111100'     ;      1 ? Steps reduzieren/Steps angefallen
        bz        main02          ;
        incf      ebene,F         ;        1 Ebene +1
        movlw     maxmenu + 1     ;          ? maximale Ebene erreicht
        btfsc     bbandmode       ;
        movlw     maxmenumitband + 1;        ? maximale Ebene erreicht
        subwf     ebene,W         ;
        bnz       main016         ;
        MOVLF     .1,ebene        ;          1 Ebene = 1
main016:                          ;
        bra       main017         ;
main015:                          ;
        movff     impulse,schleife+3;    0 negative Drehrichtung
        negf      schleife+3      ;        Impulse merken da nicht reduziert
        movf      schleife+3,W    ;        und negieren
        andlw     B'11111100'     ;        ? Steps reduzieren/Steps angefallen
        bz        main02          ;
        decf      ebene,F         ;        1 Ebene -1
        incf      ebene,W         ;          ? Ebene < 0
        bnz       main017         ;
        movlw     maxmenu         ;
        btfsc     bbandmode       ;
        movlw     maxmenumitband ;
        movwf     ebene           ;          1 Ebene == maxmenu
main017:                          ;
        bsf       blcdneu         ;
        clrf      impulse         ;      Impulse loeschen
        bra       main02          ;
main014:                          ;
        btfsc     impulse,7       ;    0 ? positive Impulse
        bra       main010         ;
        clrf      impulse         ;      1 wieder Vorbereiten fuer Interrupt
        andlw     B'01111111'     ;        reduzieren < 127
        movwf     schleife+3      ;
main011:                          ;        SCHLEIFE(1)
        btfss     block           ;          ? kein LOCK
        rcall     stepadd         ;          1 step addieren
        decfsz    schleife+3,F    ;
        bra       main011         ;        ENDE(1) Impulse==0 LOCK
        bra       main013         ;
main010:                          ;
        negf      impulse         ;        komplementaer bilden
        movf      impulse,W       ;        in WREG laden
        clrf      impulse         ;        wieder fuer Interrupt vorbereiten
        movwf     schleife+3      ;        Schleifenzaehler laden
main012:                          ;        SCHLEIFE(1)
        btfss     block           ;          ? kein LOCK
        rcall     stepsub         ;          1 step subtrahieren
        decfsz    schleife+3,F    ;
        bra       main012         ;        ENDE(1) Impulse=0
main013:
        bsf     blcdneu         ;
        bsf     bddsneu         ;      Frequenz neu berechnen
;..............................................................................
main02:
        btfss   blcdneu         ; ? LCD neu anzeigen beide Zeilen
        goto    main03          ;
        bcf     bnofunktion     ; 1 BIT loeschen keine Funktion durchgefuehrt
        call    LCDAnzeigeZ1    ;    LCD neu anzeigen Zeile1
        call    LCDAnzeigeZ2    ;    LCD neu anzeigen Zeile2
        bcf     blcdneu         ;    BIT loeschen LCD neuanzeige
;..............................................................................
main03:
;..............................................................................
main04:
        btfss   bddsneu         ; ? DDS neu berechnen
        bra     main05          ;
        bcf     bnofunktion     ; 1 BIT loeschen keine Funktion durchgefuehrt
        rcall   ddsbinausrechnen;    BINs neu ausrechnen tx + rx
        rcall   bin2ddsrx       ;    RX-Frequenz laden
        bcf     bddsneu         ;    bit loeschen
;..............................................................................
main05:
        rcall   Tastegedrueckt ; ? Taste gedrueckt
        btfss   btaste          ;
        bra     main06          ;
        btfsc   blauto          ; 1 ? Lichautomatik
        bsf     a_licht         ;    1 Licht ein
        bcf     bnofunktion     ;    1 BIT loeschen keine Funktion durchgefuehrt
        rcall   zs1laden        ;      Timer 1Sek laden
        btfss   tonenable       ;    ? MT aktiv
        bra     main0502        ;
        bcf     tonenable       ;    1 MT aus
main0501:                       ;
        call    Tastegedrueckt ;       SCHLEIFE(2)
        bc      main0501        ;      ENDE(2) keine Taste gedrueckt
        bra     main06          ;      --> break
main0502:                       ;
        rcall   Tastaturstatus ;     0 ? Tasttaturabfrage
        movlw   1               ;
        subwf   tastennummer,W ;
        bnz     main051         ;
        call    taste1behandlung;      1 Taste 1 behandeln
main051:                        ;
        movlw   2               ;
        subwf   tastennummer,W ;
        bnz     main052         ;
        call    taste2behandlung;      2 Taste 2 behandeln
main052:                        ;
        movlw   3               ;
        subwf   tastennummer,W ;
        bnz     main053         ;
        call    taste3behandlung;      3 Taste 3 behandeln
main053:                        ;
        movlw   4               ;
        subwf   tastennummer,W ;
        bnz     main054         ;
        call    taste4behandlung;      4 Taste 4 behandeln
main054:                        ;
;..............................................................................
main06:                         ;
        btfss   bkeyeranz       ; ? Keyergeschw neu anzeigen
        bra     main07          ;
        bcf     bnofunktion     ; 1 Idle-Funktion loeschen
        bsf     blcdneu         ;
;..............................................................................
main07:                         ;
        bcf     bkein_mt        ; Mithoerton erlauben
        movlw   modecw          ; ? CW oder CWr und Keyer ein
        subwf   mode,W          ;
        bz      main074         ;
        movlw   modecwr         ;
        subwf   mode,W          ;
        bz      main074         ;
        bsf     bkein_mt        ;    kein CW oder CWr Mithoerton nicht erlauben
        bra     main071         ;
main074:                        ;
        btfss   bkeyerein       ; 1 ? Keyer aktiv
        bra     main071         ;
        btfsc   e_punktpin      ;    1 ? keyer gedrueckt punkt
        bra     main0741        ;
        btfss   bmenu           ;      1 ? Menu ein
        bra     main0742        ;
        bsf     tonenable       ;        1 Mithoerton ein
        call    swrmessen       ;          SWR messen
        bcf     tonenable       ;          Mithoerton aus
        bra     main0741        ;
main0742:                       ;
        bsf     bpunkt          ;        0 punkt merken                                                  
main0741:                       ;
        btfss   e_strichpin     ;      ? keyer gedrueckt strich
        bsf     bstrich         ;      1 strich merken
        btfss   bpunkt          ;      ? wurde ein Punkt gespeichert
        bra     main08          ;
        bcf     bnofunktion     ;      1 Idle-Funktion loeschen
        rcall   bin2ddstx       ;        Sendefrequenz einstellen
        bsf     a_senderein     ;        Sender einschalten
        bsf     tonenable       ;        Mithoerton ein
        bsf     a_tastungein    ;        Sender Tastung ein
        call    punkt           ;        Punktdauer warten
        bcf     a_tastungein    ;        Sender Tastung aus
        rcall   sendertimerein ;         SenderAusVerzoegerung
        bcf     tonenable       ;        Mithoerton aus
        call    punkt           ;        Punktdauer warten
        bcf     bpunkt          ;        gemerkten Punkt loeschen
        bra     main99          ;        --> break zum Schleifenende
main071:                        ;
        btfsc   e_punktpin      ;    0 ? Handtaste Taste gedrueckt
        bra     main072         ;
        btfsc   a_tastungein    ;      1 ? Sendertastung noch aus
        bra     main0711        ;
        bcf     bnofunktion     ;        1 Bit fuer keine Funktion loeschen
        rcall   bin2ddstx       ;          Sendefrequenz einstellen
        bsf     a_senderein     ;          Sender einschalten
        bsf     a_tastungein    ;          Sender Tastung EIN
        btfss   bkein_mt        ;          ? ist Mithoerton erlaubt
        bsf     tonenable       ;          1 Mithoerton ein
main0711:
        rcall   sendertimerein ;         SenderAusVerzoegerung
        bra     main99          ;
main072:                        ;
        bcf     a_tastungein    ;        0 Sender Tastung aus
        bcf     tonenable       ;          Mithoerton aus
main073:                        ;
;..............................................................................
main08:                         ;
        movlw   modecw          ; ? CW oder CWr und Keyer ein
        subwf   mode,W          ;
        bz      main082         ;
        movlw   modecwr         ;
        subwf   mode,W          ;
        bnz     main081         ;
main082:
        btfss   bkeyerein       ;
        bra     main081         ;
        btfss   bstrich         ; 1 ? wurde ein Strich gespeichert
        bra     main09          ;
        bcf     bnofunktion     ;    1 Idle-Funktion loeschen
testaaa:
        rcall   bin2ddstx       ;      Sendefrequenz einschalten
        bsf     a_senderein     ;      Sender einschalten
        bsf     tonenable       ;      Mithoerton ein
        bsf     a_tastungein    ;      Sender Tastung EIN
        call    punkt           ;      Punktdauer warten
        call    punkt           ;      Punktdauer warten
        call    punkt           ;      Punktdauer warten
        bcf     a_tastungein    ;      Sender Tastung AUS
        rcall   sendertimerein ;       SenderAusVerzoegerung
        rcall   test_s_timerende;
        bcf     tonenable       ;      Mithoerton aus
        call    punkt           ;      Punktdauer warten
        bcf     bstrich         ;      gemerkten Punkt loeschen
        bra     main99          ;      --> break zum Schleifenende
main081:                        ; 0 kein Break bei Handtastung
;..............................................................................
main09:
        movf    zs1,F           ; ? Timer 2 Sekunden aktiv
        bz      main09_1
        btfss   bzs1ausein      ; 1 ? Funktion aus -> ein noch nicht ausgefuehrt
        bra     main11          ;
        btfsc   bbeleuchtung    ;    1 ? Dauerlicht
        bsf     a_licht         ;      1 Licht ein
        bcf     bzs1ausein      ;      Funktion aus -> ein deaktivieren
        bsf     bzs1einaus      ;      Funktion ein -> aus aktivieren
        bsf     blcdneu         ;      LCD neu Anzeigen
        bra     main10
main09_1:
        btfss   bzs1einaus      ; 0 ? Funktion ein -> aus noch nicht ausgefuehrt
        bra     main11          ;
        btfsc   blauto          ;    1 ? Autolicht
        bcf     a_licht         ;      1 Licht aus
        btfsc   bbeleuchtung    ;      ? Dauerlicht
        bsf     a_licht         ;      1 Licht ein
        bcf     bkeyeranz       ;      Keyeranzeige in LCD nicht anzeigen
        bcf     bzs1einaus      ;      Funktion ein -> aus deaktivieren
        bsf     blcdneu         ;      LCD neu Anzeigen
;..............................................................................
main10:
        bcf     bnofunktion     ; wichtige Funktion ausgefuehrt
main11:
;..............................................................................
main80:                         ;
        btfss   bnofunktion     ; ? keine wichtigen Funktionen angefallen
        bra     main99          ; 0 ---> sofort wieder von Vorn alle wichtigen
                                ;    Funktionen abklappern
;..............................................................................
        movlw   .1              ;
        subwf   zeitschlitz,W   ; 1 ? Zeitschlitz
        bnz     main801         ;      1.Zeitschlitz Bandumschaltung
        btfss   bbandmode       ;    1 ? Bandumschaltung ueber A/D Eingang
        call    ubandmessen     ;      1 feststellen ob Band umgeschalten wurde am A/D eingang
        movf    band,W          ;      ? neues Band
        subwf   bandneu,W       ;
        bz      main801         ;
        btfss   bbandmode       ;      1 ? Bandumschaltung ueber A/D Eingang
        rcall   quittungston    ;        1 Quittungston
        rcall   LCDDisplayClear ;        Display loeschen
        movf    bandneu,W       ;        an Zeilenanfang
        rcall   LCDHEX          ;        Bandnummer in Hex
        LCDStrp text3           ;        Text Bandwechsel zeigen
        call    ebandwrite      ;        altes Band speichern
        movff   bandneu,band    ;        neues Band aktivieren
        call    ebandread       ;        Banddaten lesen
        rcall   t1Sek           ;        Zeit verstreichen lassen
        bsf     blcdneu         ;        LCD muesste mal neu angezeigt werden
        bsf     bddsneu         ;        DDS neu ausrechnen
        rcall   zs1laden        ;        Timer 1Sek laden
        btfsc   blauto          ;        ? Lichautomatik
         bsf    a_licht         ;        1 Licht ein
        MOVLF   1,step          ;        10Hz Schrittweite
        btfsc   bbandmode       ;        ? Bandumschaltung aktiv umschalten
        call    bandrs232       ;        1 Band auf RS232 ausgeben
;..............................................................................
main801:                        ;
        movlw   .2              ;    2 2.Zeitschlitz Mode-Aenderung
        subwf   zeitschlitz,W   ;
        bnz     main802         ;
        btfss   bbandmode       ;      ? Modeumschaltung ueber A/D Eingang
        call    umodemessen     ;      1 Feststellen des Modes
        movf    band,F          ;      ? Band == 0 (Messgenerator)
        bnz     main8022        ;
        clrf    modeneu         ;      1 unbedingt auf MHz schalten
main8022:                       ;
        movf    modeneu,W       ;
        subwf   mode,W          ;      ? ZF-Aenderung
        bz      main8021        ;
        movff   modeneu,mode    ;      1 neu ZF aktivieren
        call    ezfread         ;        neue Mode-ZF einlesen
        rcall   zs1laden        ;        Timer 1Sek laden
        btfsc   blauto          ;        ? Lichautomatik
        bsf     a_licht         ;        1 Hintergrundbeleuchtung ein
        bsf     bddsneu         ;        DDS neu ausrechnen
        btfsc   bbandmode       ;        ? BandModeumschaltung aktiv
        call    moders232       ;        1 Mode auf RS232 ausgeben
main8021:                       ;
;..............................................................................
main802:                        ;
        movlw   .3              ;    3 3.Zeitschlitz S-Meteranzeige
        subwf   zeitschlitz,W   ;
        bnz     main803         ;
        btfss   blcdsmeter      ;      ? BIT S-Meter auf LCD anzeigen
        bra     main803         ;
        call    smeterausw      ;      1 S-Meter ablesen
        CMP1    usmeter+1,usmeter;       ? neue Messung != alte Messung
        bz      main803         ;
        movff   usmeter,usmeter+1;       1 Messung speichern
        bsf     blcdneu         ;          und LCD neu anzeigen
;..............................................................................
main803:                         ;                                                                      
        movlw   .4               ;    4 4.Zeitschlitz Keyer-Geschwindigkeit
        subwf   zeitschlitz,W    ;
        bnz     main804          ;
        btfss   bkeyerein        ;      ? Keyer aktiv
        bra     main804          ;
        RL2     ukeyer+2         ;      1 Summe von UKeyer x 16
        RL2     ukeyer+2         ;        weil nur 16 Idledurchlaeufe
        RL2     ukeyer+2         ;        wir brauchen 256 zum auswerten
        RL2     ukeyer+2         ;        (Durchschnittserrechnung)
        movff   ukeyer+3,ukeyer ;          Summe von UKeyer holen
        movf    keyergeschw,W    ;        gemerkte Ukeyer holen
        subwf   ukeyer+3,F       ;        und vergleichen
        bz      main804_1        ;        ? aenderung
         movff  ukeyer,ukeyer+3 ;
         incf   keyergeschw,W    ;           oder keyergeschw +1
        subwf   ukeyer+3,F       ;
         bz     main804_1        ;
         movff  ukeyer,ukeyer+3 ;
         decf   keyergeschw,W    ;           oder keyergeschw -1
         subwf  ukeyer+3,F       ;
         bz     main804_1        ;
        btfsc   blauto           ;        1 ? Lichautomatik
        bsf     a_licht          ;           1 Hintergrundbeleuchtung ein
        bsf     bkeyeranz        ;           Keyergeschw neu anzeigen
        rcall   zs1laden         ;           Timer 1Sek laden
        movff   ukeyer,keyergeschw;          und neuen Wert merken
main804_1:                       ;
        CLR4    ukeyer           ;        Summenwert loeschen
;..............................................................................
main804:
        movlw   .5               ;    5 5.Zeitschlitz
        subwf   zeitschlitz,W    ;
        bnz     main805          ;
        call    rxbuffer         ;      lesen aus dem RS232-Buffer
;        bnc    main805          ;
;        movwf  catbefehl        ;
;        call   LCDDisplayClear ;
;        movf   CAT_in_byte,W    ;
;        call   LCDHEX           ;
main8041:
;        call   rxbuffer         ;      lesen aus dem RS232-Buffer
;        bnc    main8042         ;
;        call   LCDHEX           ;
;        bra    main8041         ;
main8042:                        ;
;        bra    main8042         ;
;..............................................................................
main805:
        movlw   .6               ;    6 6.Zeitschlitz
        subwf   zeitschlitz,W    ;
        bnz     main806          ;
;..............................................................................
main806:
main98:                          ;    ### Funktionen ohne Zeitschlitz ###
                                 ;    Neue Summe fuer Keyerpoti bilden
                                 ;    jeder Zeitschlitz wird eine Messung add.
        btfss   bkeyerein        ;    ? Keyer aktiv
        bra     main982          ;
        call    UmessKeyerPoti ;      1 Keyerpoti messen
        movff   messh,ukeyer     ;      als byte speichern
        clrf    ukeyer+1         ;      hoeherwertiges byte loeschen
        ADD2    ukeyer+2, ukeyer;       und als word addieren
main982:                         ;
        decfsz zeitschlitz,F     ;    zeitschlitz-1
        bra     main981          ;    ? zeitschlitz == 0
        MOVLF   .16,zeitschlitz ;     1 zeitschlitz wieder max
main981:                         ;
main99:                          ;
        bra     main01           ;ENDE(1) endlos
;-------------------------------------------------------------------------
sendertimerein:                          ;
        MOVLF   tmr1lconst,TMR1L         ;timer1 neu laden
        MOVLF   tmr1hconst,TMR1H         ;
        movff   stimer,zs2               ;Sendertimer laden
        return                           ;
;-------------------------------------------------------------------------
test_s_timerende:                ;
        movf    zs2,F            ; ? Timer Sender AUS != 0
        bnz     ts01             ;
         bcf      a_senderein   ; 1 Sender ausschalten
         rcall    bin2ddsrx     ;    Empfangsfrequenz einstellen
ts01:                           ;
        return                  ;
;=========================================================================
;serieller Empfang von 8 Bit an RC7
;=========================================================================
rs232rx:
        ifdef debug
        return
        endif
rs232rx01:
        btfss   PIR1,RCIF       ;test ob Daten
        bra     rs232rx01       ;..wenn nein, dann warten
        movf    RCREG,W         ;Daten ins W-Register
        return
;=========================================================================
;Senden von 8 Bit an RC6
;=========================================================================
rs232tx:
        ifdef debug
        return
        endif
rs232tx01:
        btfss   PIR1,TXIF       ;..und ob der Sendepuffer leer ist
        bra     rs232tx01       ;
        movwf   TXREG           ;Sende Byte
        return
;==============================================================================
;Funktion       Dauer eines Punktes warten. Wartezeit wird 1mSek-Einheiten
;               und dem Timer2 gebildet. Waehrend der Wartezeit wird beim
;               Senden die Ausgangsleistung geregelt. Weiter Funktion ist
;               das abfragen der Paddel fuer Punkt oder Strichspeicher
;------------------------------------------------------------------------------
d_pr2h          equ     .242    ;Timer2 Period Register
d_pr2l          equ     .242    ;Timer2 Period Register

punkt:
         ifdef    debug
         return
         endif

         call     UmessKeyerPoti;Keyergeschw vom Poti lesen
         rcall    ukey2msek     ;aus Potiwert umrechnen in mSek
         movwf    zs3           ;Timer setzen
         MOVLF    d_pr2h,PR2    ;Timer 2 laden
         bsf      T2CON,TMR2ON  ;Timer 2 starten
punkt3:                         ;SCHLEIFE
        btfss   e_punktpin      ; Keyer Punkt aktiv
        bsf     bpunkt          ; Punkt merken
        btfss   e_strichpin     ; Keyer Strich aktiv
        bsf     bstrich         ; Strich merken
        btfss   a_tastungein    ;
        rcall   test_s_timerende;
        movf    zs3,F           ; ? Timer abgelaufen
        bnz     punkt3          ;ENDE Timer abgelaufen
punkt5:                         ;
        bcf     T2CON,TMR2ON    ;Timer 2 aus
        return                  ;
;==============================================================================
;Abfrage ob Taste 1-4 gedrueckt ist
;Ausgang        Taste gedrueckt: Bit btaste=1; Carry=1
;               Taste nicht gedrueckt: Bit btaste=0; Carry=0
;------------------------------------------------------------------------------
Tastegedrueckt:
        bcf     btaste
        movf    PORTB,W         ;Tastenzustand einlesen
        andlw   B'11110000'     ;filtern
        xorlw   B'11110000'     ;? ist eine Taste gedrueckt
        bz      tasteged01
        bsf     btaste          ;1 BIT setzen
        bsf     STATUS,C        ; und C setzen
        bra     tasteged02
tasteged01:
        bcf     STATUS,C
tasteged02:
        return
;==============================================================================
;Zeitschleife
;1 Einheit= 1 mSek
;Einsprung bei DELAY wird mit Wert in W gerechnet                                                        
;-------------------------------------------------------------------------
t5Sek:                          ;5 Sekunden
        rcall   t2Sek           ;3 + 2Sek.
        rcall   t1Sek           ;
t2Sek:                          ;2 Sekunden
        rcall   t1Sek           ;1 + 1Sek
t1Sek:                          ;1 Sekunde
        rcall   t250mSek        ;4 x 250mSek
        rcall   t250mSek        ;
t500mSek:                       ;500 mSekunde
        rcall   t250mSek        ;2 x 250mSek
t250mSek:                       ;250 mSekunden
        movlw   .250            ;
        bra     DELAY           ;
t200mSek:                       ;200 mSekunden
        movlw   .200            ;
        bra     DELAY           ;
t100mSek:                       ;100 mSekunden
        movlw   .100            ;
        bra     DELAY           ;
t50mSek:                        ;50 mSekunden
        movlw   .50             ;
        bra     DELAY           ;
t10mSek:                        ;10 mSekunden
        movlw   .10             ;10 mSek
        bra     DELAY           ;
t1mSek:                         ;1 mSekunde
        movlw   1               ;1 mSek
DELAY:                          ;
; Delay code generated by PiKloops (Mi 2008-Jul-09 08:37:15)
; Time Delay = 0.00100000s with Osc = 20.00000000MHz

       ifdef    debug          ;Zeitschleife verkuerzen
       return                  ;beim Simulieren
       endif

        movwf   CounterC        ;dazu von DL4JAL Sp. fuer mSek
delay_0.001_sec                 ;
        movlw   D'7'            ;
        movwf   CounterB        ;
        movlw   D'125'          ;
        movwf   CounterA        ;
delay_0.001_sec_loop            ;
        decfsz CounterA,1       ;
        bra     delay_0.001_sec_loop
        decfsz CounterB,1       ;
        bra     delay_0.001_sec_loop
        decfsz CounterC,F       ;
        bra     delay_0.001_sec ;
        return                  ;
;===============================================================================
quittungsdton:
        bsf     tonenable
        rcall   t50mSek
        bcf     tonenable
        rcall   t50mSek
quittungston:
        bsf     tonenable
        rcall   t50mSek
        bcf     tonenable
        bcf     a_senderein     ;sender aus
        bcf     a_tastungein    ;tastung des Traegers aus
        return

;===============================================================================
;Initialisierung der LCD Anzeige
;
;==============================================================================
;Function Set: 4 bit Datenbreite; 2 Zeilen
ib1     equ     B'00100000'      ;Function set 1. nibel 4.Bitmodus
ib2     equ     B'10000000'      ;Function set 2. nibel
;==============================================================================
;Entry Mode Set: increment, display shift
; d7 d6 d5 d4 d3 d2 d1 d0
; 0 0 0 0 0 1 I/D S
; I/D 1= increment      0= decrement
; S   1= display shift 0= display freeze
ib3     equ     B'00000110'      ;Bit1=I/D, Bit0=S
;==============================================================================
;Display on/off control: display on, cursor off , cursor not blink
; d7 d6 d5 d4 d3 d2 d1 d0
; 0 0 0 0 1 D C B
; D      1= display on           0= display off
; C      1= cursor on            0= cursor off
; B      1= cursor blink         0= cursor not blink
ib4      equ     B'00001100'     ;Bit2=D, Bit1=C, Bit0=B
;==============================================================================
;Cursor Display shift: display shift, right shift
; d7 d6 d5 d4 d3 d2 d1 d0
; 0 0 0 1 S/C F/L * *
; S/C    1= display shift        0= cursor move
; F/L    1= right shift          0= left shift
ib5      equ     B'00010100'     ;Bit3=S/C, Bit2=R/L
;==============================================================================
LCDInit:
        bcf      a_LCD_E         ;Enable auf LOW setzen
        bcf      a_LCD_RS        ;RS ausschalten
        bcf      a_LCD_RW        ;RW ausschalten
        rcall    t50mSek         ;min 15 mSek warten bevor 1. Byte geladen wird
        movlw    ib1             ;Funktion setzen
        rcall    LCDAusgabe
        movlw    ib2
        rcall    LCDAusgabe
        rcall    t10mSek         ;min 5 mSek warten
        movlw    ib1             ;Funktion setzen
        rcall    LCDAusgabe
        movlw    ib2
        rcall    LCDAusgabe
        rcall    t1mSek          ;1 mSek warten
        movlw    ib3             ;LCD entsprechend einstellen
        rcall    LCDCom
        movlw    ib4
        rcall    LCDCom
        movlw    ib5
        rcall    LCDCom
        rcall    LCDDefSonderzeichen;Sonderzeichen in die LCD laden
        rcall    LCDDisplayClear ;LCD loeschen + Cursor auf null
        LCDStrp text1            ;(c) ausgeben
        LCDStrp text2
        rcall    t2Sek
        rcall    LCDDisplayClear ;LCD loeschen + Cursor auf null
        bsf      a_licht         ;
        return
;==============================================================================
;Ausgabe eines Char auf dem Display
;RS = 1
;R/W = 0
;------------------------------------------------------------------------------
LCDChar:
        movwf    LCDByte         ;Byte merken
        rcall    LCDRdy          ;ob LCD bereit
        movf     LCDByte,w       ;
        bsf      a_LCD_RS        ;RS einschalten
        bcf      a_LCD_RW        ;RW ausschalten
        bcf      a_LCD_E         ;Enable ausschalten
        andlw    B'11110000'     ;oberes Nibbel verwenden
        rcall    LCDAusgabe      ;zur LCD schicken
        swapf    LCDByte,w       ;unteres Nibbel laden vom gemerkten Byte
        andlw    B'11110000'     ;und zur LCD-Ausgabe vorbereiten
        bra      LCDAusgabe      ;call einsparen und an LCD ausgeben
;##############################################################################
;Funktion        LCD-Commandos zum definieren eines Sonderzeichens
;Eingang         Adresse 0 bis 7 im W Register
;                Bytes des Sonderzeichen im Programmspeicher TABLAT
;Ausgang         Zeichen sind in LCD gespeichert
;------------------------------------------------------------------------------
LCDDefine:
        movwf    LCDByte         ;Byte merken
        rlncf    LCDByte,F       ;x 8 multiplizieren
        rlncf    LCDByte,F       ;
        rlncf    LCDByte,F       ;
        rcall    LCDRdy          ;ist LCD bereit
        movf     LCDByte,w       ;zuerst oberes Nibbel
        iorlw    0x40            ;Zeichenspeicher schreiben
        rcall    LCDCom          ;LCD Zeichenadresse festlegen
        MOVLF    8,schleife      ;8 Byte in LCD speichern
LCDDef01:                        ;SCHLEIFE(1)
        tblrd    *+              ; Programmspeicher lesen und INC
        movf     TABLAT,W        ; Lesergebnis in W
        rcall   LCDChar         ; und in LCD Speichern                                                  
        decfsz schleife,F       ;
        bra     LCDDef01        ;END(1) 8 Byte geschrieben
        return                  ;
;------------------------------------------------------------------------------
LCDDefSonderzeichen:
        POINT   TabelleSonderzeichen; Sonderzeichentabelle in TABPOINTER
        MOVLF   8,schleife+1    ;8 Sonderzeichen in LCD erzeugen
        clrf    schleife+2      ;Beginn bei Adresse 0
LCDDefS01:                      ;SCHLEIFE(1)
        movf    schleife+2,W    ; Adresse ins W
        rcall   LCDDefine       ; und Darstellung in LCD schreiben 8 Byte
        incf    schleife+2,F    ; Adresse + 1
        decfsz schleife+1,F     ;
        bra     LCDDefS01       ;ENDE(1) 8 komplette Chardarstellungen
        return                  ;
;------------------------------------------------------------------------------
;Sonderzeichentabelle
TabelleSonderzeichen:
Sonderzeichen1:
        DB      0,0,0,0x10,0x10,0,0,0                    ;|
Sonderzeichen2:
        DB      0,0,0,0x14,0x14,0,0,0                    ;||
Sonderzeichen3:
        DB      0,0,0,0x15,0x15,0,0,0                    ;|||
Sonderzeichen4:
        DB      0,0x01,0x01,0x15,0x15,0x01,0x01,0
Sonderzeichen5:
        DB      0,0x10,0x10,0x10,0x10,0x10,0x10,0                        ;|
Sonderzeichen6:
        DB      0,0x14,0x14,0x14,0x14,0x14,0x14,0                        ;||
Sonderzeichen7:
        DB      0,0x15,0x15,0x15,0x15,0x15,0x15,0                        ;|||
Sonderzeichen8:
        DB      0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
;------------------------------------------------------------------------------
;Ausgabe eines LCD-Commandos zur Steuerung der LCD
;RS = 0
;R/W = 0
;------------------------------------------------------------------------------
LCDCom:
        movwf   LCDByte         ;Byte merken
        rcall   LCDRdy          ;ist LCD bereit
        movf    LCDByte,W       ;zuerst oberes Nibbel
        andlw   B'11110000'
        bcf     a_LCD_RS        ;RS ausschalten
        bcf     a_LCD_RW        ;RW ausschalten
        bcf     a_LCD_E         ;Enable ausschalten
        rcall   LCDAusgabe      ;ausgeben
        swapf   LCDByte,W       ;und dann unteres Nibbel
        andlw   B'11110000'     ;ausgeben
;------------------------------------------------------------------------------
LCDAusgabe:
        btfsc   LCDPort,0       ;? test ob Bit gesetzt
        iorlw   B'00000001'     ;1 Bit nicht veraendern
        btfsc   LCDPort,1       ;? test ob Bit gesetzt
        iorlw   B'00000010'     ;1 Bit nicht veraendern
        btfsc   LCDPort,2       ;? test ob Bit gesetzt
        iorlw   B'00000100'     ;1 Bit nicht veraendern
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
;BS = 0
;R/W = 1
;------------------------------------------------------------------------------
LCDRdy:
        ifdef   debug           ;Zeitschleife verkuerzen
        return                  ;beim Simulieren
        endif

       bsf     LCDPort,7
       bsf     LCDTris,7       ;Port vorbereiten zum Lesen

        bcf     a_LCD_RS        ;Status lesen aktivieren
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
        bra     LCDRdy1
        bcf     LCDTris,7       ;Port wieder zurueck
        bcf     a_LCD_RW
        return
;------------------------------------------------------------------------------
LCDDisplayClear:
        movlw   B'00000001'
        bra     LCDCom
;------------------------------------------------------------------------------
LCDCursorHome:
        movlw   B'00000010'
        bra     LCDCom
;------------------------------------------------------------------------------
LCDCursorZeile1:
        movlw   B'10000000'
        bra     LCDCom
;------------------------------------------------------------------------------
LCDCursorZeile2:
        movlw   B'11000000'
        bra     LCDCom
;------------------------------------------------------------------------------
LCDPos:
        iorlw   B'10000000'
        bra     LCDCom
;------------------------------------------------------------------------------
LCDSpace:
        movlw   ' '
        bra     LCDChar
;------------------------------------------------------------------------------
LCDSpacel:
        movwf   schleife
LCDSpacel01:
        rcall   LCDSpace
        decfsz schleife,F
        bra     LCDSpacel01
        return
;------------------------------------------------------------------------------
LCDHEX:
        movwf   LCDByte2
        swapf   LCDByte2,W
        rcall   bcd2ascii_1b
        rcall   LCDChar
        movf    LCDByte2,W
        rcall   bcd2ascii_1b
        bra     LCDChar
;------------------------------------------------------------------------------
LCDStringp:                     ;Zuerst Position festlegen
        tblrd   *+              ;als erstes LCD-Position laden
        movf    TABLAT,W        ;und an LCD die Cursorposition
        rcall   LCDPos          ;geben
LCDString:                      ;Dann String ausgeben
LCDStr02:                       ;SCHLEIFE(1)
        tblrd   *+              ; Zeichen aus Code holen
        movf    TABLAT,W        ; Zeichen
        bz      LCDStr01
        rcall   LCDChar         ; an LCD ausgeben
        bra     LCDStr02        ;ENDE(1) wenn Zeichen = 0
LCDStr01:
        return
;------------------------------------------------------------------------------
rs232String:                    ;Dann String ausgeben
rs232Str02:                     ;SCHLEIFE(1)
        tblrd   *+              ; Zeichen aus Code holen
        movf    TABLAT,W        ; Zeichen
        bz      rs232Str01
        rcall   rs232tx         ; an LCD ausgeben
        bra     rs232Str02      ;ENDE(1) wenn Zeichen = 0                                                  
rs232Str01:
        return
;;;;;;; Initial subroutine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This subroutine performs all initializations of variables and registers.

init:
         MOVLF  d_adcon1,ADCON1         ;Enable PORTA & PORTE digital I/O pins
;------------------------------------------------------------------------------
        if      bytevariante >= .21     ;alle Varianten mit PIC18F4520
;------------------------------------------------------------------------------
         MOVLF  d_adcon2,ADCON2         ;
;------------------------------------------------------------------------------
        endif
;------------------------------------------------------------------------------
         MOVLF  d_trisa,TRISA           ;Set I/O for PORTA
         MOVLF  d_trisb,TRISB           ;Set I/O for PORTB
         MOVLF  d_trisc,TRISC           ;Set I/0 for PORTC
         MOVLF  d_trisd,TRISD           ;Set I/O for PORTD
         MOVLF  d_trise,TRISE           ;Set I/O for PORTE
        MOVLF   d_rcon,RCON
        MOVLF   d_intcon,INTCON
        MOVLF   d_intcon2,INTCON2
        MOVLF   d_intcon3,INTCON3
        MOVLF   d_pie1,PIE1
        MOVLF   d_pie2,PIE2
        MOVLF   d_t1con,T1CON
        MOVLF   d_t2con,T2CON
        MOVLF   d_t3con,T3CON
        MOVLF   d_porta,PORTA
        MOVLF   d_portb,PORTB
        MOVLF   d_portc,PORTC
        MOVLF   d_portd,PORTD
        MOVLF   d_porte,PORTE
        MOVLF   0xff,schleife
        lfsr    0,0
init1:
        clrf    POSTINC0                ;RAM komplett loeschen
        decfsz schleife,F               ;
        bra     init1                   ;
        btfsc   blauto                  ;
        bsf     a_licht                 ;
        btfsc   bbeleuchtung            ;
        bsf     a_licht                 ;
        MOVLF   tmr1lconst,TMR1L        ;timer1 neu laden
        MOVLF   tmr1hconst,TMR1H        ;
        goto    zs1laden                ;
;------------------------------------------------------------------------------
mtausrechnen:
        rcall   math_ramclr             ;Tonhoehe ausrechnen
        LDK2    faktor1, HIGH tmr0wert, LOW tmr0wert
        movff   mithoerton, faktor2     ;
        rcall   div5b                   ;
        rcall   neg5ergebnis            ;
        movff   ergebnis,tmr0const      ;
        MOVLF   tmr0const,TMR0L         ;
        MOVLF   d_t0con,T0CON           ;
        return
;------------------------------------------------------------------------------
zs1laden:
        MOVLF   .200,zs1                ;genau 2 Sekunden Einheit = 10 mSek
        bsf     bzs1ausein              ;
        return
;==============================================================================
;Abfragen Tastenstatus mit Entprellung
;Ausgang        Nummer der Taste in tastennummer
;               1-4 sind gueltige Tasten 0= ungueltig

tastcounter       equ     .40     ;Anzahl der Eingabesequenzen die geprueft werden
                                  ;auf Gleichheit

Tastaturstatus:
        MOVLF     tastcounter,schleife    ;zwischenspeichern
Tastaturst1:
        movf      PORTB,W         ;Tastenzustand einlesen
        andlw     B'11110000'     ;filtern
        movwf     schleife+1      ;merken
        rcall     t1mSek          ;1 mSek warten
        movf      PORTB,W         ;erneut einlesen
        andlw   B'11110000'     ;filtern
        subwf   schleife+1,W    ;? war die Eingabe gleich
        bnz     Tastaturstatus ;0 nein wieder von ganz vorn
        decfsz schleife,F       ;1 gueltig Zaehler dec
        bra     Tastaturst1     ;? gueltige Eingabenanzahl erreicht
        movlw   0               ;keine gueltige Tastennummer
        btfss   schleife+1,7    ;? taste 1
        movlw   1               ;1 Nr 1
        btfss   schleife+1,6    ;? taste 2
        movlw   2               ;1 Nr 2
        btfss   schleife+1,5    ;? taste 3
        movlw   3               ;1 Nr 3
        btfss   schleife+1,4    ;? taste 4
        movlw   4               ;1 Nr 4
        movwf   tastennummer    ;Tastennummer abspeichern
        return
;==============================================================================
;Ausrechnen der RX und TX DDS-Werte
;..............................................................................
ddsbinausrechnen:
                                         ;RX Frequenz ausrechnen
        rcall   math_ramclr             ;Ram loeschen
        rcall   fpointerladendds         ;entsprechenden VFO laden
        lfsr    1,faktor1               ;Frequenz 5 Byte
        MOVLF   5,schleife              ;
ddsaus01:                               ;SCHLEIFE(1)
        movf    POSTINC0,W              ; nach Faktor1 laden
        movwf   POSTINC1                ;
        decfsz schleife,F               ;
        bra     ddsaus01                ;ENDE(1) nach 5 byte

        movf     band,F                  ;?   Messmodus
        bz       ddsaus00                ;1   ---> ZF nicht beruecksichtigen
        btfss    bohnezf                 ;0   ? ohne ZF
        rcall    zfbehandlung            ;    0 Zf binaer addieren
        btfsc    bvfox4                  ;    ? VFO x 4
        rcall    vfox4                   ;    1 mit 4 multiplizieren
ddsaus00:
        movff    ddskonst,faktor2        ;Konstande 5Byte in Faktor2 laden
        movff    ddskonst+1,faktor2+1
        movff    ddskonst+2,faktor2+2
        movff    ddskonst+3,faktor2+3
        movff    ddskonst+4,faktor2+4

        rcall    mul5b                   ;Multiplikation mit 1Hz Konstande
        movff    ergebnis+4,ddsbinrx     ;Ergebnis / 2 hoch 32 teilen
        movff    ergebnis+5,ddsbinrx+1   ;das bedeutet das die ersten 4 Byte
        movff    ergebnis+6,ddsbinrx+2   ;hinter dem Komma sind
        movff    ergebnis+7,ddsbinrx+3   ;
                                         ;TX Frequenz ausrechnen
        rcall    math_ramclr             ;Ram loeschen
        rcall    fpointerladenddstx      ;entsprechenden VFO laden
        lfsr     1,faktor1               ;
        MOVLF    5,schleife              ;
ddsaus02:                                ;SCHLEIFE(1)
        movf     POSTINC0,W              ; nach Faktor1 laden
        movwf    POSTINC1                ;
        decfsz   schleife,F              ;
        bra      ddsaus02                ;ENDE(1) nach 5 byte

        movf     band,F                  ;? kein Messmodus
        bz       ddsaus03                ;
        btfsc    bohnezf                 ;1 ? alles mit ZF
        bra      ddsaus04                ;
        movlw    modecw                  ;
        subwf    mode,W                  ; 1 ? kein CW oder CWr
        bz       ddsaus021               ;
        movlw    modecwr                 ;
        subwf    mode,W                  ; 1 ? kein CW oder CWr
        bz       ddsaus021               ;
        rcall    zfbehandlung            ;      1 ZF add. oder sub.
        bra      ddsaus04                ;
ddsaus021:                               ;
        btfsc    btxpermzf               ;    1 ? ZF permanent
        rcall    zfbehandlung            ;      1 ZF add. oder sub.
ddsaus04:                                ;
        btfsc    bvfox4                  ; ? VFO x 4
        rcall    vfox4                   ; 1 mit 4 multiplizieren
ddsaus03:                                ;
        movff    ddskonst,faktor2        ;Konstande 5Byte in Faktor2 laden
        movff   ddskonst+1,faktor2+1    ;                                                               
        movff   ddskonst+2,faktor2+2    ;
        movff   ddskonst+3,faktor2+3    ;
        movff   ddskonst+4,faktor2+4    ;

         rcall  mul5b                   ;Multiplikation mit 1Hz Konstande
         movff  ergebnis+4,ddsbintx     ;Ergebnis / 2 hoch 32 teilen
         movff  ergebnis+5,ddsbintx+1   ;das bedeutet das die ersten 4 Byte
         movff  ergebnis+6,ddsbintx+2   ;hinter dem komma sind
         movff  ergebnis+7,ddsbintx+3   ;
         return                         ;
;------------------------------------------------------------------------------
vfox4:                                  ;
         clrc                           ;
        RL5     faktor1                 ;
        RL5     faktor1                 ;
        return                          ;
;------------------------------------------------------------------------------
bin2ddsrx:
        goto    rx2dds
;------------------------------------------------------------------------------
bin2ddstx:
        goto    tx2dds
;==============================================================================
;Funktion       Ram fuer Multiplikation loeschen
;Ausgang        ergebniss, faktor1, faktor2 geloescht
;Register       LFSR0, schleife
;------------------------------------------------------------------------------

math_ramclr:
        MOVLF   .10+.8+.8,schleife      ;laenge aller beteiligten variablen
        lfsr    0,ergebnis              ;ergebnis ist die erste Variable
math_ramclr01:                          ;SCHLEIF(1)
        clrf    POSTINC0                ; byte loeschen
        decfsz schleife,F               ;
        bra     math_ramclr01           ;ENDE(1) alle Math-Variablen loeschen
        return
;------------------------------------------------------------------------------
;Funktion       laden der entsprechenden Ramadr in den Pointerram

fpointerladen:
        lfsr    1,temp
fpointerladendds:
        lfsr    0,frequenza
        btfsc   bvfo
        lfsr    0,frequenzb
        return

fpointerladenddstx:
        btfss   brit             ;? Rit aktiv
        bra     fpointerladendds;0 normalen Pointer laden
        lfsr    0,frequenza      ;1 bei Rit deaktivierten VFO laden beim Senden
        btfss   bvfo
        lfsr    0,frequenzb
        return
;------------------------------------------------------------------------------
bcdtobin:
        rcall   cleartemp                ;temp loeschen
        movff   POSTINC0,temp
        movff   POSTINC0,temp+1
        movff   POSTINC0,temp+2
        movff   POSTINC0,temp+3
        MOVLF   D'32',schleife+1         ;32 bit bcd in bin umwandeln

bcdtobin1:
        bcf     STATUS,C
        rrcf    temp+3,F
        rrcf    temp+2,F
        rrcf    temp+1,F
        rrcf    temp,F
        rrcf    temp+7,F
        rrcf    temp+6,F
        rrcf    temp+5,F
        rrcf    temp+4,F
        lfsr    0,temp
        MOVLF   4,schleife
bcdtobin3:
        btfss   INDF0,7
        bra     bcdtobin4
        movlw   30H
        subwf   INDF0,F
bcdtobin4:
        btfss   INDF0,3
        bra     bcdtobin5
        movlw   3
        subwf   INDF0,F
bcdtobin5:
        movf    POSTINC0,F
        decfsz schleife,F
        bra     bcdtobin3
        decfsz schleife+1,F
        bra     bcdtobin1
        return
;------------------------------------------------------------------------------
;Funktion       ZF zur Frequenz addieren oder subtrahieren
zfbehandlung:
        btfsc   bminuszf                        ;? ZF addieren
        bra     zfb01                           ;
        ADD4    faktor1, zwischenfrequenz       ;1 FREQ = FREQ + ZF
        bra     zfb99                           ;
zfb01:                                          ;
        SUB4    faktor1, zwischenfrequenz       ;0 FREQ = FREQ - ZF
zfb99:
        btfss   faktor1+3,7                     ;negative Zahl
        return

        NEG4    faktor1                         ;
        return                                  ;
;------------------------------------------------------------------------------
;Funktion       Multiplikation der Frequenz mit der 1Hz DDS-Konst
;Ausgang        binwert steht in tempkonst2+4

muldds:
        movff    ddskonst,tempkonst1     ;Konstande 5Byte in temp laden
        movff    ddskonst+1,tempkonst1+1
        movff    ddskonst+2,tempkonst1+2
        movff    ddskonst+3,tempkonst1+3
        movff    ddskonst+4,tempkonst1+4
        MOVLF    D'32',schleife+1
muldds1:
        lfsr     0,tempkonst1     ;schleife (1)
        lfsr     1,tempkonst2
        clrf     temp+1           ;   Ueberlauf loeschen
        bcf      STATUS,C         ;   Carry loeschen
        rrcf     temp+7,F         ;   Faktor1 nach rechts schieben
        rrcf     temp+6,F
        rrcf     temp+5,F
        rrcf     temp+4,F
        btfss    STATUS,C         ;   ? Carry = 1 nach RR
        bra      muldds2          ;
        MOVLF    8,schleife       ;   1 Schleife laden mit 8
        lfsr     0,tempkonst1
        lfsr     1,tempkonst2
muldds5:                          ;   schleife (2)
        movff    POSTINC0,temp    ;     wert1 holen zwischenspeichern
        movf     temp+1,W         ;     gemerketen Ueberlauf holen
        clrf     temp+1           ;     und loeschen
        addwf    INDF1,F          ;     zum Ergebnis addieren
        bnc      muldds4          ;     ? Ueberlauf
        movlw    1                ;     1 neuen Ueberlauf
        movwf    temp+1           ;       merken
muldds4:
        movf     temp,W           ;     Wert2 = Wert2 + Wert1
        addwf    POSTINC1,F
        bnc      muldds3          ;    ? Ueberlauf
        movlw    1                ;    1 neuen Ueberlauf
        movwf    temp+1           ;      merken
muldds3:
        decfsz   schleife,F       ;   ende (2)
        bra      muldds5
muldds2:
        bcf      STATUS,C         ;   Carry loeschen
        rlcf     tempkonst1,F     ;   Wert1 nach links schieben
        rlcf     tempkonst1+1,F   ;   Wert1 = 2 * Wert1
        rlcf     tempkonst1+2,F
        rlcf     tempkonst1+3,F
        rlcf     tempkonst1+4,F
        rlcf     tempkonst1+5,F
        rlcf     tempkonst1+6,F
        rlcf     tempkonst1+7,F
                                                                                                        
        decfsz schleife+1,F     ;ende (1) 32 mal durchlaufen
        bra     muldds1
        return
;------------------------------------------------------------------------------
;Funktion       loeschen des Tempram alle 10 Byte oder 4 untersten Byte
cleartemp:
        clrf    temp+9
        clrf    temp+8
        clrf    temp+7
        clrf    temp+6
        clrf    temp+5
cleartemp5:
        clrf    temp+4
cleartemp4:
        clrf    temp+3
        clrf    temp+2
        clrf    temp+1
        clrf    temp
        return
;------------------------------------------------------------------------------
;Funktion       add des eingestellten Schritt zur Frequenz
;               100kHz, 1kHz, 100Hz, 10Hz oder 1Hz
stepadd:
        rcall   cleartemp5
        rcall   fpointerladen
        movf    step,W
        bz      add1
        movlw   1
        subwf   step,W
        bz      add10
        movlw   2
        subwf   step,w
        bz      add50
        movlw   3
        subwf   step,w
        bz      add1000
        movlw   4
        subwf   step,w
        bz      add100kHz
        return

add100kHz:
        MOVLF   0xa0,temp
        MOVLF   0x86,temp+1
        MOVLF   0x01,temp+2
        bra     add5b
;------------------------------------------------------------------------------
add1000:
        LDK2    temp,0x03,0xe8 ;Step 1000Hz
        bra     add5b
;------------------------------------------------------------------------------
add50:
        LDK1    temp,.50        ;Step 50Hz
        bra     add5b
;------------------------------------------------------------------------------
add10:
        LDK1    temp,.10        ;Step 10Hz
        bra     add5b
;------------------------------------------------------------------------------
add1:
        incf    temp,F          ;Step 1Hz
        bra     add5b
;==============================================================================
;Funktion       sub des eingestellten Schritt zur Frequenz
;               100kHz, 1kHz, 50Hz, 10Hz oder 1Hz
stepsub:
        rcall   cleartemp5
        rcall   fpointerladen
        movf    step,W
        bz      sub1
        movlw   1
        subwf   step,W
        bz      sub10
        movlw   2
        subwf   step,W
        bz      sub50
        movlw   3
        subwf   step,W
        bz      sub1000
        movlw   4
        subwf   step,W
        bz      sub100kHz
        return
;------------------------------------------------------------------------------
sub100kHz:
        MOVLF   0xa0,temp
        MOVLF   0x86,temp+1
        MOVLF   0x01,temp+2
        bra     sub5b
;------------------------------------------------------------------------------
sub1000:
        LDK2    temp,0x03,0xe8
        bra     sub5b
;------------------------------------------------------------------------------
sub50:
        LDK1    temp,.50
        bra     sub5b
;------------------------------------------------------------------------------
sub10:
        LDK1    temp,.10
        bra     sub5b
;------------------------------------------------------------------------------
sub1:
        incf    temp,F
        bra     sub5b
;------------------------------------------------------------------------------
;==============================================================================
;Funktion       Subtraktion von 5 Byte; Differenz = Minuend - Subtrahend
;Eingang        LFSR0 = Minuend; LFSR1 = Subtrahend
;Ausgang        Differenz in LFSR0
;Register       schleife; LFSR0,1
;------------------------------------------------------------------------------
sub5b:
        MOVLF   .5,schleife     ;5 Byte
        clrf    STATUS,C        ;Carry loeschen
sub5b1:                         ;SCHLEIFE(1)
        movf    INDF1,W         ; Byte aus temp holen
        btfss   STATUS,C        ; ? Ueberlauf
        incfsz INDF1,W          ; 1 ? Byte 0 (Byte eins hoeher)
        subwf   INDF0,F         ;    0 subtrahieren
        movf    POSTINC0,F      ; pointer+1
        movf    POSTINC1,F      ; pointer+1
        decfsz schleife,F       ;
        bra     sub5b1          ;ENDE(1) schleife == 0
        return
;==============================================================================
;Funktion       Addition von 5 Byte; Summe = Summand1 - Summand2
;Eingang        LFSR0 = Summand1; LFSR1 = Summand2
;Ausgang        Summe in LFSR0
;Register       schleife; LFSR0,1
;------------------------------------------------------------------------------
add5b:
        movf    POSTINC1,W      ;addieren
        addwf   POSTINC0,F      ;ohne Carry
        MOVLF   4,schleife      ;weitere 4 Byte
add5b1:                         ;SCHLEIFE(1)
        movf    POSTINC1,W      ; addieren
        addwfc POSTINC0,F       ; mit Carry
        decfsz schleife,F       ;
        bra     add5b1          ;ENDE(1) 5 Byte addiert
        return
;==============================================================================
;Funktion       Multiplikation mit 18Fxxx XByte
;Eingang        faktor1, faktor2 je XByte, laenge in mlaenge
;Ausgang        ergebnis 2 x XByte
;Register       lfsr0, lfsr1, lfsr2 schleife, schleife+1
;------------------------------------------------------------------------------
mlaenge equ     .5

mul5b:
mulxb:
          MOVLF    mlaenge * .2,schleife   ;ergebnis loeschen
          lfsr     0,ergebnis              ;
mulx07:                                    ;SCHLEIFE(1)
          clrf     POSTINC0                ; loeschen
          decfsz   schleife,F              ;
          bra      mulx07                  ;ENDE(1)ergebnis geloescht
          bcf      bergebisminus           ;Ergebnis ist positiv
          btfsc    faktor1+mlaenge-1,7     ;? ist faktor1 minus
          rcall    neg5op1               ;Ergebnisvorzeichen+op1 umkehren                                  
          btfsc    faktor2+mlaenge-1,7   ;? ist faktor2 minus
          rcall    neg5op2               ;Ergebnisvorzeichen+op2 umkehren
          MOVLF    mlaenge,schleife      ;8 Byte schleife
          lfsr     0,faktor2             ;lfsr0 = faktor2
          lfsr     2,ergebnis            ;lfsr2 = ergebnis
mulx01:                                  ;SCHLEIFE(1)
          MOVLF    mlaenge,schleife+1    ; 8 Byte schleife
          movf     schleife,W
          subwf    schleife+1,F          ;
          lfsr     1,faktor1             ;   lfsr1= faktor1
          lfsr     2,ergebnis            ;   lfsr2 = ergebnis
          movf     schleife+1,F          ;   ? Test ob schleife+1 == 0
          bz       mulx04                ;
mulx03:                                  ;   0 SCHLEIFE(5)
          movf     POSTINC2,F            ;       LFSR2 + 1
          decfsz   schleife+1,F          ;     ENDE(5) bis LFSR2 wieder an richtiger Stelle
          bra      mulx03                ;
mulx04:
          MOVLF    mlaenge,schleife+1   ; 8 Byte schleife
mulx02:                                 ; SCHLEIFE(2)
         MOVLF  mlaenge-2,schleife+2    ;    6 Byte schleife
         movf   INDF0,W                 ;    faktor2 holen
         mulwf  POSTINC1                ;    mit faktor1 multiplizieren
         movf   PRODL,W                 ;    produkt LOW holen und
         addwf  POSTINC2,F              ;    zum ergebniss addieren
         movf   PRODH,W                 ;    produkt HIGH holen und
         addwfc POSTINC2,F              ;    zum ergebnis mit Carry addieren
mulx05:                                 ;    SCHLEIFE(3)
         clrf   WREG                    ;      nur Carry
         addwfc POSTINC2,F              ;      addieren
         decfsz schleife+2,F            ;    ENDE(3) wenn alle ergebnisbytes behandelt
         bra    mulx05                  ;
         MOVLF  mlaenge-1,schleife+2    ;    7 Byte schleife fuer Korrektur LFSR2
mulx06:                                 ;    SCHLEIFE(4)
         movf   POSTDEC2,F              ;      Pointer LFSR 2 - 1
         decfsz schleife+2,F            ;      wieder zurueck korrigieren
         bra    mulx06                  ;    ENDE(4)
         decfsz schleife+1,F            ; ENDE(2) 8 Byte faktor2 bearbeitet
         bra    mulx02                  ;
         movf   POSTINC0,F              ; pointer faktor2 + 1
         decfsz schleife,F              ;ENDE(1)
         bra    mulx01                  ;
         btfsc  bergebisminus           ;? ist das ergebnis minus
         rcall  neg5ergebnis            ;1 minus setzen
         return                         ;
;------------------------------------------------------------------------------
neg5ergebnis:
         NEG5   ergebnis                ;
         bcf    bergebisminus           ;Ergebnisvorzeichen loeschen
         return
;------------------------------------------------------------------------------
neg5op1:                                ;
         NEG5   op1                     ;
         bra    neg5op2_1               ;
;------------------------------------------------------------------------------
neg5op2:                                ;
         NEG5   op2                     ;
neg5op2_1:                              ;
        btg     bergebisminus           ;Ergebnisvorzeichen umkehren
        return                          ;
;==============================================================================
;Funktion       dividieren von 2 x 5 Byte-Werten divident / divisor
;Eingang        divisor, divident
;Ausgang        ergebnis, rest im divident
;Register       schleife, divisor, divident
;------------------------------------------------------------------------------
div5b:
        bcf     bergebisminus           ;Ergebnis ist positiv
        btfsc   divident+mlaenge-1,7    ;? ist divisor minus
        rcall   neg5op1                 ;Ergebnisvorzeichen+op1 umkehren
        btfsc   divisor+mlaenge-1,7     ;? ist divident minus
        rcall   neg5op2                 ;Ergebnisvorzeichen+op2 umkehren
        bcf     divisor+mlaenge-1,7     ;Operator Vorzeichen loeschen
        bcf     divident+mlaenge-1,7    ;Operator Vorzeichen loeschen
        clrf    schleife                ;schleife = 0
        CLR5    ergebnis                ;ergebnis loeschen
divb51:                                 ;
        incf    schleife,F              ;schleife + 1
        clrc                            ;Carry loeschen
        btfsc   divisor+4,7               ;
        bra     divb53                    ;
        RL5     divisor                   ;
        CMP5    divident,divisor          ;
        bc      divb51                    ;
        CLR5    ergebnis                  ;
divb52:                                   ;SCHLEIFE(1)
         clrc                             ;
         RR5    divisor                   ;
divb53:                                   ;
         CMP5   divident,divisor          ;
         bnc    divb54                    ;
         SUB5   divident,divisor          ;
divb54:                                   ;
        RL5     ergebnis                  ;
        decfsz schleife,F                 ;
        bra     divb52                    ;ENDE(1)
        btfsc   bergebisminus             ;? ist das ergebnis minus
        rcall   neg5ergebnis              ;1 minus setzen
        return                            ;
;==============================================================================
;Funktion       ergebnis nach faktor1 copieren
;Eingang        ergebnis 5 Byte
;Ausgang        ergebnis in faktor1
;Register       faktor1, ergebnis
;------------------------------------------------------------------------------
ergebnis2faktor1_5b:
        movff   ergebnis+4,faktor1+4
        movff   ergebnis+3,faktor1+3
        movff   ergebnis+2,faktor1+2
        movff   ergebnis+1,faktor1+1
        movff   ergebnis,faktor1
        return
;==============================================================================
;Funktion       ergebnis nach faktor2 copieren
;Eingang        ergebnis 5 Byte
;Ausgang        ergebnis in faktor2
;Register       faktor2, ergebnis
;------------------------------------------------------------------------------
ergebnis2faktor2_5b:
        movff   ergebnis+4,faktor2+4
        movff   ergebnis+3,faktor2+3
        movff   ergebnis+2,faktor2+2
        movff   ergebnis+1,faktor2+1
        movff   ergebnis,faktor2
        return
;==============================================================================
;Funktion       HEX zu BCD wandeln
;Eingang        faktor1 5 Byte
;Ausgang        ergebnis max 6 Byte
;Register       faktor1, ergebnis, zw, schleife, schleife+1
;------------------------------------------------------------------------------
hex2bcd5b:
        CLR4    ergebnis        ;ergebnis loeschen
        CLR2    ergebnis+4      ;
        MOVLF   .40, schleife   ;40 Durchlaeufe fuers Bit
htb1:                            ;SCHLEIFE(1)
        MOVLF   .6, schleife+1 ; 6 Byte Ergebnis
        lfsr    0,ergebnis      ; LFSR0 = ergebnis
htb11:                           ; SCHLEIFE(2)
        movf    INDF0,W         ;     Ergebnis Byte holen
        andlw   B'00001111'     ;     untersten Bit
        movwf   zw              ;     zwischenspeichern
        movlw   .5              ;     5
        subwf   zw,W            ;     subtrahieren
        btfss   STATUS,C        ;     ? Ueberlauf
        bra     htb2            ;
        movlw   .3              ;     1 3
        addwf   INDF0,F         ;        addiern
htb2:
        movf    INDF0,W         ;     Ergebnis Byte holen
        andlw   B'11110000'     ;     obersten Bit
        movwf   zw              ;     zwischenspeichern
        movlw   50h             ;     50
        subwf   zw,W            ;     subtrahieren
        btfss   STATUS,C        ;     ? Ueberlauf
        bra     htb3            ;
        movlw   30h             ;     1 0x30
        addwf   INDF0,F         ;        addieren
htb3:
        movf    PREINC0,F       ;    Byte eins weiter                                                   
        decfsz schleife+1,F     ; ENDE(2) 6 Byte bearbeitet
        bra     htb11           ;
        clrc                    ; carry loeschen
        RL5     faktor1         ; faktor1 -> ergebnis schieben
        RL6     ergebnis        ;
        decfsz schleife,F       ;
        bra     htb1            ;ENDE(1) nach 40 Bit
        return                  ;fertig
;==============================================================================
;Funktion       Ausgabe des Strings
;Eingang        String in string;
;               Laenge (2..12) = Anzahl der Ziffern
;               Komma (0..12) = Stelle des Kommas von rechts 0 = kein Komma
;               Kuerzen (0..) = Anzahl der zu kuerzenden Ziffern von rechts
;Ausgang        Alle Ziffern mit Komma ohne fueherende 0 sind auf LCD
;Register       string, schleife, komma, laenge
;------------------------------------------------------------------------------
printf:
        lfsr    0,string        ;hier beginnt der String
        MOVLF   .12,schleife    ;bestehend aus 12 Char
        movf    laenge,W        ;laengezahl errechnen
        subwf   schleife,W      ;
        movwf   laenge          ;
        movf    komma,W         ;Kommazahl errechnen
        subwf   schleife,W      ;
        movwf   komma           ;
        bcf     bnull           ;beginnen mit keiner Nullausgabe
        movf    kuerzen,W       ;letzten Stellen nicht
        subwf   schleife,F      ;ausgeben
printf2:                        ;SCHLEIFE(1)
        decf    komma,F         ; ? Komma == 1 Stelle davor
        btfsc   STATUS,Z        ;
        bsf     bnull           ; 1 richtige NULL ausgeben
        incf    komma,F         ; Komma wieder korrigieren
        movf    komma,F         ; ? Kommaausgabe
        bnz     printf3         ;
        movlw   ','             ; 1 Komma schreiben
        rcall   LCDChar         ;    in LCD
printf3:
        movf    POSTINC0,W      ; Char holen
        movwf   schleife+1      ; und zwischenspeichern
        movf    laenge,F        ;
        bnz     printf4         ;
        btfsc   bnull           ; ? Space statt NULL ausgeben
        bra     printf6         ;
        movlw   '0'             ; 0 0 ausgeben
        subwf   schleife+1,W    ;
        bnz     printf6         ;
        MOVLF   ' ',schleife+1 ;
        bra     printf7         ;
printf6:                        ;
        bsf     bnull           ;
printf7:                        ;
        movf    schleife+1,W    ;
        rcall   LCDChar         ;
printf4:                        ;
        movf    laenge,F        ;
        bz      printf5         ;
        decf    laenge,F        ;
printf5:                        ;
        decf    komma,F         ;
        decfsz schleife,F       ;ENDE(1)
        bra     printf2         ;
        movf    kuerzen,F       ;
        bz      printf8         ;
        movf    kuerzen,W       ;
        rcall   LCDSpacel       ;Leerzeichen ausgeben
printf8
        return
;==============================================================================
;Funktion       Wandeln BCD-Format in String
;Eingang:       gepacktes BCD-Format in ergebnis(6 Byte)
;Ausgang:       String in string(12)
;Register:      schleife, string, ergebnis
;------------------------------------------------------------------------------
bcd2char6b:
        CLR4    string
        CLR4    string+4
        CLR4    string+8
          lfsr     0,string
          lfsr     1,ergebnis+5
          MOVLF    6,schleife
b2c401:
          swapf    INDF1,W
          andlw    b'00001111'
          rcall    charwrite
          movf     POSTDEC1,W
          rcall    charwrite
          decfsz   schleife,F
          bra      b2c401
          return

charwrite:
        andlw   b'00001111'
        iorlw   30h
        movwf   POSTINC0
        return
;------------------------------------------------------------------------------
;Funktion       eine Zahl auf LCD ausgeben Fuehrende 0 als Leerzeichen
;------------------------------------------------------------------------------
LCDfrqbyte:
        andlw   B'00001111'     ;unteres Nibbel filetern
        bnz     LCDfrq3         ;? Zahl NULL
        btfss   bnull           ;1 ? fuehrende Null
        bra     LCDfrq1
        movlw   ' '             ; 1 Leerzeichen ausgeben
        bra     LCDfrq4
LCDfrq1:
LCDfrq3:
        iorlw   0x30            ;
        bcf     bnull
LCDfrq4
        bra     LCDChar
;==============================================================================
;Funktion       Anzeige der Frequenz auf LCD Zeile
;Eingang        Frequenz im Speicher
;Ausgang        LCD
;------------------------------------------------------------------------------
FunktionPos:    equ      .11

LCDAnzeigeZ2:
        btfsc      bmenu            ;? Menu aktiv
        bra        menuanzeige      ;1 zur Menuanzeige
        btfsc      bkeyeranz        ;? Keyergeschw. anzeigen
        bra        LCDAnzKeyer      ;1 zur Anzeige
        movf       band,F           ;? Messmodus
        bz         LCDAnZ200        ;1 ---> Zur Frequenzanzeige
        btfsc      blcdsmeter       ;0 ? S-Meter aktiv
        bra        smanzeige        ; 1 S-Meter auf Zeile 2
LCDAnZ200:                          ;
        movlw       40H             ;Anfang Zeile 2
        rcall       LCDPos          ;Cursor bewegen
        movlw       'A'             ;
        btfss       bvfo            ;? VFO B aktiv
        movlw       'B'             ;1 B anzeigen
        btfss       brit            ;? Rit aktiv
        bra        LCDAnZ201        ;
        movlw       'a'             ;1 ? VFO B aktiv
        btfss       bvfo            ;
        movlw       'b'             ; 1 B anzeigen
LCDAnZ201:                          ;
        rcall       LCDChar         ;anzeigen
        btfsc       bvfo            ;
        bra         LCDAnZ203       ;
LCDAnZ202:                          ;
        LD5         faktor1,frequenzb;
        bra         LCDAnZ204       ;
LCDAnZ203:                          ;
        LD5         faktor1,frequenza;
LCDAnZ204:                          ;
        rcall       LCDAnz          ;
        movlw       .3              ;
        bra         LCDSpacel       ;noch 3 Leerzeichen

LCDAnzeigeZ1:
        clrf       WREG            ;1. Position Zeile 1
        call       LCDPos          ;Cursor bewegen
        movlw      'A'             ;
        btfsc      bvfo            ;? VFO B aktiv
        movlw   'B'             ;1 B anzeigen                                                           
        btfss   brit            ;
        bra     LCDAnZ101       ;
        movlw   'a'             ;
        btfsc   bvfo            ;? VFO B aktiv
        movlw   'b'             ;1 B anzeigen
LCDAnZ101:                      ;
        call    LCDChar         ;anzeigen
        btfsc   bvfo            ;
        bra     LCDAnZ102       ;
LCDAnZ103:
        LD5     faktor1,frequenza
        bra     LCDAnZ104
LCDAnZ102:
        LD5     faktor1,frequenzb
LCDAnZ104:
        rcall   LCDAnz
        goto    LCDAnzMode
;------------------------------------------------------------------------------
LCDAnz:
        lfsr    0,faktor1       ;
        lfsr    1,lcdoffset     ;
        rcall   add5b           ;
        rcall   hex2bcd5b       ;
        rcall   bcd2char6b      ;
        MOVLF   .11,laenge      ;
        MOVLF   .6,komma        ;
        movff   step,kuerzen    ;
        movlw   4               ;
        subwf   step,W          ;
        bnz     LCDAnz01        ;
        MOVLF   .5,kuerzen      ;
LCDAnz01:
        goto    printf          ;
;------------------------------------------------------------------------------
LCDAnzMode:                     ;
        btfsc   bvfox4          ;
        bra     LCDAnzM01       ;
        movf    mode,F          ;
        bnz     LCDAnzM104      ;
LCDAnzM01:                      ;
        LCDStrp tmode0          ;
        return                  ;
LCDAnzM104:                     ;
        movlw   1               ;
        subwf   mode,W          ;
        bnz     LCDAnzM105      ;
        LCDStrp tmode1          ;
        return                  ;
LCDAnzM105:                     ;
        movlw   2               ;
        subwf   mode,W          ;
        bnz     LCDAnzM106      ;
        LCDStrp tmode2          ;
        return                  ;
LCDAnzM106:                     ;
        movlw   3
        subwf   mode,W
        bnz     LCDAnzM107
        LCDStrp tmode3
        return
LCDAnzM107:
        movlw   4
        subwf   mode,W
        bnz     LCDAnzM108
        LCDStrp tmode4
        return
LCDAnzM108:
        movlw   5
        subwf   mode,W
        bnz     LCDAnzM109
        LCDStrp tmode5
        return
LCDAnzM109:
        movlw   6
        subwf   mode,W
        bnz     LCDAnzM110
        LCDStrp tmode6
        return
LCDAnzM110:
        return
;------------------------------------------------------------------------------
LCDAnzKeyer:                            ;
        movf    band,F                  ;? Messender
        btfsc   STATUS,Z                ;
        return                          ;1 keine Anzeige
        LCDStrp text5                   ;0 "WpM: "
        call    UmessKeyerPoti          ; Spannung messen
        movwf   schleife+3              ; U / 8
        rrcf    schleife+3,F            ;
        rrcf    schleife+3,F            ;
        rrcf    schleife+3,W            ;
        andlw   B'00011111'             ;
        addlw   .9                      ; geringste Geschwindigkeit addieren 0Volt
        movwf   schleife+3              ;
        rcall   math_ramclr             ;
        movff   schleife+3,faktor1      ; berechen
        rcall   hex2bcd5b               ; hex2bcd
        movf    ergebnis,W              ;
        call    LCDHEX                  ; und anzeigen
        movlw   .9                      ;
        goto    LCDSpacel               ; 9 Leerzeichen folgen
;==============================================================================
xsmladen:                               ;
        movff   xsm,faktor2             ;laden des X-Wertes in Faktor 2
        movff   xsm+1,faktor2+1         ;
        btfss   xsm+1,7                 ;? test ob negativ
        bra     xsmladen01              ;
        comf    faktor2+2,F             ;1 X-Werte negativ
        comf    faktor2+3,F             ; darstellen
        comf    faktor2+4,F             ;
xsmladen01:                             ;
        return                          ;
;------------------------------------------------------------------------------
ysmladen:                               ;
        CLR5    summand1                ;
        movff   ysm,summand1+2          ;
        movff   ysm+1,summand1+3        ;
        btfsc   ysm+1,7                 ;
        comf    summand1+4,F            ;
        return                          ;
;------------------------------------------------------------------------------
smeterausw:                             ;
        rcall   umesssmeter             ;Spannung messen
smeterausw01:                           ;
        rcall   math_ramclr             ;Ram loeschen math operationen
        LD2     faktor1,mess            ;faktor1 = Messergebnis
        rcall   xsmladen                ;XSM Laden mit Vorzeichen
        rcall   mul5b                   ;Ergebnis = Messung * X-Wert
        rcall   ysmladen                ;YSM Laden mit Vorzeichen
        ADD5    ergebnis,summand1       ;Ergebnis = Ergebnis + Y-Wert
;       LCDHEX4Z2        ergebnis       ;
;       call    t1Sek                   ;
        movlw   d'60'                   ;
        subwf   ergebnis+2,W            ;
        bnc     smeterausw02            ;
        clrf    ergebnis+2              ;
        bra     smeterausw03            ;
smeterausw02:                           ;
        movlw   d'31'                   ;
        subwf   ergebnis+2,W            ;
        bnc     smeterausw03            ;
        MOVLF   d'30',ergebnis+2        ;
smeterausw03:                           ;
        movff   ergebnis+2,usmeter      ;ergebnis speichern fuer Bargraph
        movff   ergebnis+2,usmeter+2    ;ergebnis speichern fuer S-Wert
        return                          ;
;------------------------------------------------------------------------------
smanzeige:                              ;
        rcall   smeterausw              ;
        CMP2    usmeter+1,usmeter       ;? neue Messung != alte Messung
        bz      smanzeige01             ;
        movff   usmeter,usmeter+1       ;1 Messung speichern
smanzeige01:                            ;
        rcall   math_ramclr             ;alle Operanten loeschen
        movff   usmeter+1,divident      ;alte Messung in Divident laden
        MOVLF   .3,divisor              ;Ergebnis = Bargrah / 3
        rcall   div5b                   ;
        MOVLF   .8,schleife+3           ;Schleife3 = 8
        movf    ergebnis,W              ;
         subwf  schleife+3,F            ;Schleife3 = Schleife3 - Ergebnis                               
         movlw  0x40                    ;Zeilenanfang 2. Zeile
         call   LCDPos                  ;
         MOVLF  .6,ergebnis+1           ;Grafische markierung fuer S9
smanz03:                                ;SCHLEIFE(1)
         movf   ergebnis,F              ; ? ergebnis == 0
         bz     smanz05                 ;
         decfsz ergebnis+1,F            ; 0 grafische Markierung S9 - 1
         bra    smanz032                ;
         movlw  .3                      ;    ? grafische Markierung erreicht
         bra    smanz031                ;    1 langes Zeichen laden
smanz032:                               ;
         movlw  .2                      ;    0 kleines Zeichen laden
smanz031:                               ;
         call   LCDChar                 ;    und anzeigen
         decfsz ergebnis,F              ;
         bra    smanz03                 ;ENDE(1)
smanz05:                                ;
         movf   divident,F              ;? Divident == 0
         bnz    smanz02                 ;
         incf   schleife+3,F            ;1 Schleife3 = Schleife3 + 1
         bra    smanz06                 ;
smanz02:                                ;
         decf   divident,W              ;0 Divident = Divident -1
         call   LCDChar                 ; als Zeichen darstellen
smanz06:                                ;
         incf   schleife+3,F            ;Schleife3 = Schleife3 + 1
         movf   schleife+3,W            ;? Schleife3 != 0
         bz     smanz07                 ;
         call   LCDSpacel               ;1 als Leezeichen darstellen
smanz07:                                ;
         movlw  'S'                     ;"S" Darstellen
         call   LCDChar                 ;
         rcall  math_ramclr             ;operanten loeschen
         clrc                           ;
         rrcf   usmeter+2,F             ;usmeter2 = usmeter2 / 2
         movff  usmeter+2,faktor1       ;faktor1 = usmeter
         movlw  .10                     ;
         subwf  usmeter+2,W             ;usmeter2 = usmeter2 - 10
         bc     smanz08                 ;? S-Wert < 10
         rcall  hex2bcd5b               ;1 faktor1 in BCD wandeln
         rcall  bcd2char6b              ; in Char wandeln
         MOVLF  .1,laenge               ; 1 stellige Anzeige
         MOVLF  .0,komma                ; 0 Stelle hintern Komma
         clrf   kuerzen                 ; 0 Stellen kuerzen
         rcall  printf                  ; und anzeigen
         bra    smanz09                 ;
smanz08:                                ;
        movlw   '+'                     ;0 ein + darstellen
        call    LCDChar                 ;
smanz09:                                ;
        movlw   4                       ;eventuelle Zeichen rechts
        goto    LCDSpacel               ;ueberschreiben mit Leerzeichen
;==============================================================================
ubattanzeige:
        call    math_ramclr             ;
        call    LCDDisplayClear         ;Display loeschen
        rcall   umessbatt               ;Spannung messen
        movff   ADRESL,faktor2          ;ermittelte Spannung laden
        movff   ADRESH,faktor2+1        ;
;       LCDHEX2Z1       faktor2 ;
;       call    t5Sek

       MOVLF   HIGH(.9805),faktor1+1   ;mit Faktor multiplizieren
       MOVLF   LOW(.9805),faktor1      ;
       call    mul5b                   ;
;      LCDHEX4Z1       ergebnis        ;
;      call    t5Sek

       CLR5    faktor1                 ;
       movff   ergebnis+2,faktor1      ;
       btfsc   ergebnis+1,7            ;? niederwertige Stelle > 7fh
       incf    faktor1,F                       ;1 aufrunden
       rcall   hex2bcd5b               ;HEX in BCD wandeln
       rcall   bcd2char6b              ;BCD in Char wandeln
       MOVLF   .3,laenge               ;3 stellige Anzeige
       MOVLF   .1,komma                ;1 Stelle hintern Komma
       clrf    kuerzen                 ;von rechts nichts kuerzen
       movlw   .3                      ;Position Zeile 1 4.Zeichen
       call    LCDPos                  ;setzen
        rcall   printf                  ;Formatiert ausgeben
        LCDStrp text4                   ;und "Volt" dahinter schreiben
        goto    t2Sek                   ;2 Sekunden anzeigen
;==============================================================================
;Konstande fuer die Berechnung, welches Band eingestellt wurde
anzband         equ     .13     ;mess,160,80,60,40,30,20,17,15,12,10,6,2m
anztransv       equ     .5      ;5 Transverterbaender

bdiv    equ     (0x3ff * .100) / (anzband + anztransv - 1)
bshift equ      0x3ff / ((anzband + anztransv - 1) * 2)
ubandmessen:
        call    math_ramclr             ;mathe-speicher loeschen
        rcall   umessband               ;Spannung messen
        movff   ADRESL,faktor1          ;umspeichern Ergebnis
        movff   ADRESH,faktor1+1        ;
        MOVLF   LOW(bshift),faktor2     ;
        MOVLF   HIGH(bshift),faktor2+1 ;
        ADD2    faktor1,faktor2         ;
        CLR5    faktor2                 ;
        MOVLF   .100,faktor2            ;Multiplizieren mit 100
        rcall   mul5b                   ;
        rcall   ergebnis2faktor1_5b     ;Ergebnis weiter verwenden
        LDK2    faktor2,HIGH(bdiv),LOW(bdiv);Dividieren durch konst
        rcall   div5b                   ;
        movff   ergebnis,bandneu        ;Ergebnis ist eine Bandnummer je nach angelegter
        return                          ;Spannung
;==============================================================================
;Konstande fuer die Berechnung, welcher Mode eingestellt wurde
anzmode         equ     .6              ;cw,lsb,usb,cwr,dig,digr

bmdiv   equ     (0x3ff * .100) / (anzmode - 1)
bmshift equ     0x3ff / ((anzmode - 1) * 2)

umodemessen:
        call    math_ramclr             ;mathe-speicher loeschen
        rcall   umessmode               ;Spannung messen
        movff   ADRESL,faktor1          ;umspeichern Ergebnis
        movff   ADRESH,faktor1+1        ;
        MOVLF   LOW(bmshift),faktor2    ;
        MOVLF   HIGH(bmshift),faktor2+1 ;
        ADD2    faktor1,faktor2         ;
        CLR5    faktor2                 ;
        MOVLF   .100,faktor2            ;Multiplizieren mit 100
        rcall   mul5b                   ;
        rcall   ergebnis2faktor1_5b     ;Ergebnis weiter verwenden
        LDK2    faktor2,HIGH(bmdiv),LOW(bmdiv)
                                        ;Dividieren durch konst
        rcall   div5b                   ;
        incf    ergebnis,W              ;Ergebnis ist eine Modenummer je nach angelegter
        movwf   modeneu                 ;Spannung ausser Nummer 0
        return                          ;
;==============================================================================
UmessKeyerPoti:
;------------------------------------------------------------------------------
        if      bytevariante >= .21     ;alle Varianten mit PIC18F4520
;------------------------------------------------------------------------------
        bcf     ADCON2,ADFM
;------------------------------------------------------------------------------
        endif
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
        if      bytevariante < .21      ;alle Varianten mit PIC18F452
;------------------------------------------------------------------------------
        bcf     ADCON1,ADFM
;------------------------------------------------------------------------------
        endif
;------------------------------------------------------------------------------
        rcall   umesskeyer
        movf    messh,W
        movwf   keyergeschw+1
;------------------------------------------------------------------------------
        if      bytevariante >= .21     ;alle Varianten mit PIC18F4520
;------------------------------------------------------------------------------
        bsf     ADCON2,ADFM
;------------------------------------------------------------------------------
        endif
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
        if      bytevariante < .21      ;alle Varianten mit PIC18F452
;------------------------------------------------------------------------------
        bsf     ADCON1,ADFM                                                                             
;------------------------------------------------------------------------------
        endif
;------------------------------------------------------------------------------
        return
;==============================================================================
;Batteriespannung ermitteln Spannungsteiler = 6,8k / 3,3K

ubatterielesen:
        rcall   umessbatt                ;
        movff   ADRESL,ubatt     ;
        movff   ADRESH,ubatt+1 ;
        bsf     ubattneu         ;Spannung neu anzeigen einleiten
        return
;==============================================================================
;------------------------------------------------------------------------------
        if      bytevariante >= .21      ;alle Varianten mit PIC18F4520
;------------------------------------------------------------------------------
adst            equ      0x01
kanalRA0        equ      adst | b'00000000'
kanalRA1        equ      adst | b'00000100'
kanalRA2        equ      adst | b'00001000'
kanalRA3        equ      adst | b'00001100'
kanalRA5        equ      adst | b'00010000'
kanalRE0        equ      adst | b'00010100'
kanalRE1        equ      adst | b'00011000'
kanalRE2        equ      adst | b'00011100'
#define         adfrei ADCON0,1
;------------------------------------------------------------------------------
        endif
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
        if      bytevariante < .21       ;alle Varianten mit PIC18F452
;------------------------------------------------------------------------------
adst            equ      0xc1
kanalRA0        equ      adst | b'00000000'
kanalRA1        equ      adst | b'00001000'
kanalRA2        equ      adst | b'00010000'
kanalRA3        equ      adst | b'00011000'
kanalRA5        equ      adst | b'00100000'
kanalRE0        equ      adst | b'00101000'
kanalRE1        equ      adst | b'00110000'
kanalRE2        equ      adst | b'00111000'
#define         adfrei ADCON0,2
;------------------------------------------------------------------------------
        endif
;------------------------------------------------------------------------------

umessmode:
umess8:
        movlw   kanalRE2                ;anal. kanal RE2
        bra     umess
umesssmeter:
umess7:
        movlw   kanalRE1                ;anal. kanal RE1
        bra     umess
umessband:
umess6:
        movlw   kanalRE0                ;anal. kanal RE0
        bra     umess
umessrit:
umess5:
        movlw   kanalRA5                ;anal. kanal RA5
        bra     umess
umessrueck:
umess3:
        movlw   kanalRA3                ;anal. kanal RA3
        bra     umess
umessvor:
umess2:
        movlw   kanalRA2                ;anal. kanal RA2
        bra     umess
umesskeyer:
umess1:
        movlw   kanalRA1               ;anal. kanal RA1
        bra     umess                  ;Keyerpoti
umessbatt:
umess0:
        movlw   kanalRA0                ; anal. kanal RA0
umess:
         movwf    ADCON0               ; ... als Messeingang setzen
         MOVLF    .100,schleife        ;
ubme2:
         decfsz   schleife,F           ;
         bra      ubme2                ;
         bsf      adfrei               ;Start Messung
ubme1:
        btfsc   adfrei                  ;warten bis Messung beendet
        bra     ubme1                   ;Ergebnis in adresh und adresl
        movff   ADRESH,messh            ;
        movff   ADRESL,messl            ;
        return
;-------------------------------------------------------------------------
;Funktion zum umrechnen des ermittelten Spannungswertes in passenden Hexwert
;z.B. 10,2 Volt = 102 = 66h
;Trick: es wird mit mulkonst multipliziert und der Ueberlauf ins naechste
;Byte ist das Divisionsergebnis
;eingang        Wert in W
;ausgang        Wert in W
;register       temp, temp+1,

mulkonst:         equ     .110

umrechnen1:
        movwf   temp            ;
        movlw   mulkonst        ;
        mulwf   temp            ;
        movff   PRODL,temp      ;
        movff   PRODH,temp+1    ;
        btfsc   temp,7          ;? test ob aufrunden niederwertiges Byte>7f
        incf    temp+1,F        ;1 hoeherwertiges Byte + 1
        movf    temp+1,W        ;Ergebnis in W
        return                  ;
;==============================================================================
menuanzeige:                    ;
        movlw   .1              ;
        subwf   ebene,W         ;
        bnz     manz01          ;
        LCDStrp menu1           ;
manz01:                         ;
        movlw   .2              ;
        subwf   ebene,W         ;
        bnz     manz02          ;
        LCDStrp menu2           ;
manz02:                         ;
        movlw   .3              ;
        subwf   ebene,W         ;
        bnz     manz03          ;
        LCDStrp menu3           ;
manz03:                         ;
        movlw   .4              ;
        subwf   ebene,W         ;
        bnz     manz04          ;
        LCDStrp menu4           ;
manz04:                         ;
        movlw   .5              ;
        subwf   ebene,W         ;
        bnz     manz05          ;
        LCDStrp menu5           ;
manz05:                         ;
        movlw   .6              ;
        subwf   ebene,W         ;
        bnz     manz06          ;
        LCDStrp menu6           ;
manz06:                         ;
        movlw   .7              ;
        subwf   ebene,W         ;
        bnz     manz07          ;
        LCDStrp menu7           ;
manz07:                         ;
        movlw   .8              ;
        subwf   ebene,W         ;
        bnz     manz08          ;
        LCDStrp menu8           ;
manz08:                         ;
        movlw   .9              ;
        subwf   ebene,W         ;
        bnz     manz09          ;
        LCDStrp menu9           ;
manz09:                         ;
       movlw     .10            ;                                                                       
       subwf     ebene,W        ;
       bnz       manz10         ;
       LCDStrp   menu10         ;
manz10:                         ;
        movlw   .11             ;
        subwf   ebene,W         ;
        bnz     manz11          ;
        LCDStrp menu11          ;
manz11:                         ;
        movlw   .12             ;
        subwf   ebene,W         ;
        bnz     manz12          ;
        LCDStrp menu12          ;
manz12:                         ;
        return                  ;
;==============================================================================
taste1behandlung:               ;
        bcf     block           ;eventuell Entriegeln
        clrf    impulse         ;Impulse loeschen
        call    quittungston    ;Tastaturquittung
        btg     bmenu           ;Menu ein oder aus
        bsf     blcdneu         ;
        call    LCDDisplayClear ;
        bra     t4b_99          ;ende
;------------------------------------------------------------------------------
taste2behandlung:               ;
        rcall   MenuStart       ;
        movlw   .1              ;
        subwf   ebene,W         ;
        bnz     t2b_1           ;
        rcall   umschalten_vfo ;VFOa/VFOb
t2b_1:                          ;
        movlw   .2              ;
        subwf   ebene,W         ;
        bnz     t2b_2           ;
        rcall   BgleichA        ;VFOa=VFOb
t2b_2:                          ;
        movlw   .3              ;
        subwf   ebene,W         ;
        bnz     t2b_3           ;
        rcall   AgleichB        ;VFOb=VFOa
t2b_3:                          ;
        movlw   .4              ;
        subwf   ebene,W         ;
        bnz     t2b_4           ;
        call    swrmessen       ;SWR messen und darstellen
t2b_4:                          ;
        movlw   .5              ;
        subwf   ebene,W         ;
        bnz     t2b_5           ;
        MOVLF   modecw,modeneu ;CW
t2b_5:                          ;
        movlw   .6              ;
        subwf   ebene,W         ;
        bnz     t2b_6           ;
        MOVLF   modecwr,modeneu ;CWr
t2b_6:                          ;
        movlw   .7              ;
        subwf   ebene,W         ;
        bnz     t2b_7           ;
        MOVLF   0,bandneu       ;mess
t2b_7:                          ;
        movlw   .8              ;
        subwf   ebene,W         ;
        bnz     t2b_8           ;
        MOVLF   .3,bandneu      ;60m
t2b_8:                          ;
        movlw   .9              ;
        subwf   ebene,W         ;
        bnz     t2b_9           ;
        MOVLF   .6,bandneu      ;20m
t2b_9:                          ;
        movlw   .10             ;
        subwf   ebene,W         ;
        bnz     t2b_10          ;
        MOVLF   .9,bandneu      ;12m
t2b_10:                         ;
        movlw   .11             ;
        subwf   ebene,W         ;
       bnz     t2b_11           ;
       MOVLF   .12,bandneu      ;2m
t2b_11:                         ;
        movlw   .12             ;
        subwf   ebene,W         ;
        bnz     t2b_12          ;
        MOVLF   .15,bandneu     ;tr3
t2b_12:                         ;
        bra     t4b_98          ;
;------------------------------------------------------------------------------
taste3behandlung:               ;
        rcall   MenuStart       ;
        movlw   .1              ;
        subwf   ebene,W         ;
        bnz     t3b_1           ;
        rcall   umschalten_step ;Step aendern
t3b_1:                          ;
        movlw   .2              ;
        subwf   ebene,W         ;
        bnz     t3b_2           ;
        rcall   mithoerton_ein_aus;Spot Mithoerton ein/aus
t3b_2:                          ;
        movlw   .3              ;
        subwf   ebene,W         ;
        bnz     t3b_3           ;
        rcall   ebandwrite      ;Bandeinstellung sichern
        rcall   writesicherung ;Einstellung sichern
t3b_3:                          ;
        movlw   .4              ;
        subwf   ebene,W         ;
        bnz     t3b_4           ;
        rcall   SETUP           ;SETUP
t3b_4:                          ;
        movlw   .5              ;
        subwf   ebene,W         ;
        bnz     t3b_5           ;
        MOVLF   modelsb,modeneu ;LSB
t3b_5:
        movlw   .6              ;
        subwf   ebene,W         ;
        bnz     t3b_6           ;
        MOVLF   modedig,modeneu ;DIG
t3b_6:                          ;
        movlw   .7              ;
        subwf   ebene,W         ;
        bnz     t3b_7           ;
        MOVLF   1,bandneu       ;160m
t3b_7:                          ;
        movlw   .8              ;
        subwf   ebene,W         ;
        bnz     t3b_8           ;
        MOVLF   4,bandneu       ;40m
t3b_8:                          ;
        movlw   .9              ;
        subwf   ebene,W         ;
        bnz     t3b_9           ;
        MOVLF   7,bandneu       ;17m
t3b_9:                          ;
        movlw   .10             ;
        subwf   ebene,W         ;
        bnz     t3b_10          ;
        MOVLF   .10,bandneu     ;10m
t3b_10:                         ;
        movlw   .11             ;
        subwf   ebene,W         ;
        bnz     t3b_11          ;
        MOVLF   .13,bandneu     ;tr1
t3b_11:                         ;
        movlw   .12             ;
        subwf   ebene,W         ;
        bnz     t3b_12          ;
        MOVLF   .16,bandneu     ;tr4
t3b_12:                         ;
        bra     t4b_98          ;
;------------------------------------------------------------------------------
taste4behandlung:               ;
        rcall   MenuStart       ;
        movlw   .1              ;
        subwf   ebene,W         ;
        bnz     t4b_1           ;
         rcall   funktionrit   ;                                                                        
t4b_1:                         ;
        movlw   .2             ;
        subwf   ebene,W        ;
        bnz     t4b_2          ;
        movlw   .4             ;
        subwf   step,W         ;
        bz      t4b_101        ;
        MOVLF   4,step         ;
        bsf     blcdneu        ;
        bra     t4b_2          ;
t4b_101:
        rcall   umschalten_step ;Step aendern
t4b_2:                          ;
        movlw   .3              ;
        subwf   ebene,W         ;
        bnz     t4b_3           ;
        btg     block           ;Lock ein
t4b_3:                          ;
        movlw   .4              ;
        subwf   ebene,W         ;
        bnz     t4b_4           ;
        rcall   ubattanzeige    ;Anzeige Batteriespannung 2 Sekunden
        bra     menureset       ;
t4b_4:                          ;
        movlw   .5              ;
        subwf   ebene,W         ;
        bnz     t4b_5           ;
        MOVLF   modeusb,modeneu ;USB
t4b_5:                          ;
        movlw   .6              ;
        subwf   ebene,W         ;
        bnz     t4b_6           ;
        MOVLF   modedigr,modeneu;DIGr
t4b_6:                          ;
        movlw   .7              ;
        subwf   ebene,W         ;
        bnz     t4b_7           ;
        MOVLF   2,bandneu       ;80m
t4b_7:                          ;
        movlw   .8              ;
        subwf   ebene,W         ;
        bnz     t4b_8           ;
        MOVLF   5,bandneu       ;30m
t4b_8:                          ;
        movlw   .9              ;
        subwf   ebene,W         ;
        bnz     t4b_9           ;
        MOVLF   8,bandneu       ;15m
t4b_9:                          ;
        movlw   .10             ;
        subwf   ebene,W         ;
        bnz     t4b_10          ;
        MOVLF   .11,bandneu     ;6m
t4b_10:                         ;
        movlw   .11             ;
        subwf   ebene,W         ;
        bnz     t4b_11          ;
        MOVLF   .14,bandneu     ;tr2
t4b_11:                         ;
        movlw   .12             ;
        subwf   ebene,W         ;
        bnz     t4b_12          ;
        MOVLF   .17,bandneu     ;tr5
t4b_12:                         ;
t4b_98:                         ;
menuende:                       ;
        clrf    tastennummer    ;keine Taste war aktiv deshalb loeschen
        MOVLF   1,ebene         ;wieder auf Menue 1
        bcf     bmenu           ;Menu ausschalten
t4b_99:                         ;SCHLEIFE(1)
        call    Tastegedrueckt ;
        bc      t4b_99          ;ENDE(1) keine Taste gedrueckt
        return                  ;
;------------------------------------------------------------------------------
funktionrit:                    ;
        btfsc   brit            ;? RIT aus
        bra     frit01          ;
        rcall   kopie_rit_ein   ;
        btfsc   bvfo            ;1 ? VFOa
       bra     frit02           ;
       rcall   AgleichB         ; 1 VFOa nach VFOb kopieren
       bra     frit01           ;
frit02:                         ;
        rcall   BgleichA        ; 0 VFOb nach VFOa kopieren
frit01:                         ;
        rcall   kopie_rit_aus   ;
        btg     brit            ;RIT umschalten
        bsf     blcdneu         ;Anzeige neu
        bsf     bddsneu         ;DDS neu berechnen
        return                  ;
;------------------------------------------------------------------------------
kopie_rit_aus:                  ;
        lfsr    1,frequenza     ;
        lfsr    2,frqmerk       ;
        bra     kr02            ;
kopie_rit_ein:                  ;
        lfsr    2,frequenza     ;
        lfsr    1,frqmerk       ;
kr02:                           ;
        MOVLF   .12,schleife    ;
kr01:                           ;
        movff   POSTINC2,POSTINC1;
        decfsz schleife,F       ;
        bra     kr01            ;
        return                  ;
;------------------------------------------------------------------------------
AgleichB:
        movff   frequenza,frequenzb
        movff   frequenza+1,frequenzb+1
        movff   frequenza+2,frequenzb+2
        movff   frequenza+3,frequenzb+3
        movff   frequenza+4,frequenzb+4
        bra     menureset
;------------------------------------------------------------------------------
BgleichA:
        movff   frequenzb,frequenza
        movff   frequenzb+1,frequenza+1
        movff   frequenzb+2,frequenza+2
        movff   frequenzb+3,frequenza+3
        movff   frequenzb+4,frequenza+4
        bra     menureset
;------------------------------------------------------------------------------
mithoerton_ein_aus:             ;
        btg     tonenable       ;
        btfss   tonenable       ;
        return                  ;
        MOVLF   1,step          ;10Hz Schrittweite
        bsf     blcdneu         ;
        return                  ;
;------------------------------------------------------------------------------
umschalten_step:                ;
        clrf    impulse         ;
        movf    band,F          ;? Messmodus
        bnz     umstep01        ;
        incf    step,F          ;1 step + 1
        movlw   5               ; ? oberer Bereich ueberschritten
        subwf   step,W          ;
        bnc     umstep_1        ;
        clrf    step            ; 1 wieder von vorn
        bra     umstep_1        ;
umstep01:
        incf    step,F          ;0 step + 1
        movlw   4               ; ? step > 4
        subwf   step,W          ;
        bnc     umstep_1        ;
        clrf    step            ; 1 wieder mit 0 beginnen
        btfss   b1hzanz         ;    ? keine 1Hz Anzeige
        incf    step,F          ;    1 mit 1 beginnen
umstep_1:                       ;
        bsf     blcdneu         ;LCD neu anzeigen
        return                  ;
;------------------------------------------------------------------------------
umschalten_vfo:                 ;
        rcall   setmode         ;
        btg     bvfo            ;
        rcall   getmode         ;
        bsf     bddsneu         ;
        bsf     blcdneu         ;
        return                  ;
;==============================================================================                         
MenuStart:                       ;
         clrf   impulse          ;Impulse loeschen
         call   quittungston     ;Tastaturquittung
         movlw  .1               ;
         subwf  ebene,W          ;? Ebene 1
         bnz    MenuStart2       ;
         movlw  .5               ;1 Zeitschleife aufbauen
         movwf  schleife         ;
MenuStart1:                      ;
         call   Tastegedrueckt ; SCHLEIFE(1)
         bnc    MenuStart2       ;    break --> Taste nicht gedrueckt
         call   t50mSek          ;
         decfsz schleife,F       ; ENDE(1) nach 250 msek Taste gedrueckt
         bra    MenuStart1       ;
         call   quittungsdton    ; 250 msek gedrueckt
         call   t50mSek          ; doppelter Quittungston
         MOVLF  2,ebene          ;
;        incf   ebene,F          ; auf Ebene 2 schalten
MenuStart3:                      ;
         call   Tastegedrueckt ; SCHLEIFE(2)
         bc     MenuStart3       ; ENDE(2) keine Taste gedrueckt
MenuStart2:                      ;
         movf   ebene,W          ;Ebene laden
         return                  ;
;=========================================================================
;ZF in Eeprom schreiben
;------------------------------------------------------------------------------
ezfwrite:                                ;
        decf    mode,W                   ;
        call    getZFadr                 ;ZF Adresse im Eeprom bestimmen
        movwf   data_ee_addr             ;Ziel im Eeprom
        lfsr    0,zwischenfrequenz       ;Quelle im Ram
        MOVLF   4,schleife               ;
        goto    lewrite                  ;0 ZF in Eeprom schreiben
;=========================================================================
;ZF aus Eeprom lesen
;------------------------------------------------------------------------------
ezfread:                                 ;
        decf    mode,W                   ;
        call    getZFadr                 ;
        movwf   data_ee_addr             ;
        lfsr    0,zwischenfrequenz       ;Eintrittspunkt im Ram
        MOVLF   4,schleife               ;
        movf    mode,F                   ;? mode == 0
        bnz     ezfread01                ;
        CLR4    zwischenfrequenz         ;1 keine ZF
        return                           ;
ezfread01:                               ;
        goto    leread                   ;0 ZF aus Eeprom lesen
;=========================================================================
;Bandumschaltungen
;------------------------------------------------------------------------------
bandaddrset:                    ;
        call    getBandadr      ;lesen der adr im Eeprom Bandbezogen
        movwf   data_ee_addr    ;Vorbereiten zum Eepromlesen adr festlegen
        lfsr    0,frequenza     ;Eintrittspunkt im Ram
bandaddrset1:                   ;
        MOVLF   4,schleife      ;10 Byte Frequenz AB mode AB
        return                  ;
;------------------------------------------------------------------------------
ebandread:                      ;
        CLR5    lcdoffset       ;Vorsichthalber Offset loeschen
        rcall   bandaddrset     ;Adressen und Zaehler laden
        rcall   leread          ;Frequenz A lesen
        movf    POSTINC0,F      ;Ram + 1
        rcall   bandaddrset1    ;Zaehler laden
        rcall   leread          ;Frequenz B lesen
        movf    POSTINC0,F      ;Ram + 1
        MOVLF   2,schleife      ;Zaehler laden
        rcall   leread          ;mode AB lesen
        movlw   .13             ;ab 13 ist das Band ein Transverter
        subwf   band,W          ;? Transverter
        bnc     ebandread1      ;
        MOVLF   5,schleife      ;1 noch 5 Byte Displayshift
        rcall   leread          ;Displayshift lesen
ebandread1:
        bcf     bminuszf        ;RX = RX + ZF
        btfsc   modeA,7         ;? Bit gesetzt
        bsf     bminuszf        ;1 RX = RX - ZF
        bcf     modeA,7         ;unbedingt BIT loeschen
        rcall   getmode         ;mode auswerten
        return                  ;
;------------------------------------------------------------------------------
ebandwrite:                     ;
        rcall   setmode         ;Mode in entsprechenden VFO abspeichern
        btfsc   bminuszf        ;? RX = RX -ZF
        bsf     modeA,7         ;1 minus ZF merken
        rcall   bandaddrset     ;Adressen und Zaehler laden
        rcall   lewrite         ;FrequenzA schreiben
        movf    POSTINC0,F      ;Ram + 1
        rcall   bandaddrset1    ;Zaehler laden
        rcall   lewrite         ;Frequenz B schreiben
        movf    POSTINC0,F      ;Ram + 1
        MOVLF   2,schleife      ;Zaehler laden
        rcall   lewrite         ;mode AB schreiben
        movlw   .13             ;ab 13 ist das Band ein Transverter
        subwf   band,W          ;? Transverter
        bnc     ebw01           ;
        MOVLF   5,schleife      ;1 noch 5 Byte Displayshift
        rcall   lewrite         ; Displayshift schreiben
ebw01:                          ;
        return                  ;
;------------------------------------------------------------------------------
lewrite:                        ;
lew01:                          ;SCHLEIFE
        movf    POSTINC0,W      ; Byte aus Indexregister
        movwf   data_ee_data    ; in Eepromdaten laden
        rcall   ewrite          ; und abspeichern
        incf    data_ee_addr,F ; eepromadr+1
        decfsz schleife,F       ;ENDE schleife=0
        bra     lew01           ;
        return                  ;
;------------------------------------------------------------------------------
leread:                         ;
ler01:                          ;SCHLEIFE
        rcall   eread           ; Byte aus Eeprom lesen
        movwf   POSTINC0        ; indirekt abspeichern
        incf    data_ee_addr,F ; eepromadr+1
        decfsz schleife,F       ;ENDE schleife=0
        bra     ler01           ;
        return                  ;
;------------------------------------------------------------------------------
menureset:                      ;
        movlw   1               ;
        movwf   ebene           ;
        bcf     bmenu           ;
        bsf     blcdneu         ;
        goto    LCDDisplayClear ;
;=========================================================================
;Funktion       lesen der DDSKONST,STEP-Einstellung,Band und versch. Flags
;Register       lfsr0, schleife
;-------------------------------------------------------------------------
readsicherung:                           ;
        clrf    data_ee_addr            ;Beginn mit 0 im Eeprom
        MOVLF   .10,schleife            ;10 Byte aus Eeprom lesen
        lfsr    0,ddskonst              ;Im Ram ab ddskonst
        rcall   reads01                 ;Lesen

        MOVLF   addrxsm,data_ee_addr    ;ab Adresse addrsxm sxm,sym,mithoerton
        MOVLF   .5,schleife             ;noch 5 Byte
        lfsr    0,xsm                   ;Im Ram ab sxm
reads01:                                ;SCHLEIFE(1)
        rcall   eread                   ; Lesen
        movwf   POSTINC0                ;
        incf    data_ee_addr,F          ; eeadr++
        decfsz schleife,F               ;
        bra     reads01                 ;ENDE(1)
        return                          ;
;==============================================================================
;Funktion       schreiben der DDSKONST,STEP-Einstellung,Band und versch. Flags
;Register       lfsr0, schleife
;------------------------------------------------------------------------------
writesicherung:                         ;
        clrf    data_ee_addr            ;Ab Adr 0 im Eeprom
        MOVLF   .10,schleife            ;10 Byte
        lfsr    0,ddskonst              ;im Ram ab ddskonst
        rcall   writes01                ;Schreiben

       MOVLF   addrxsm,data_ee_addr     ;Ab Adr addrsmx im Eeprom
        MOVLF   .5,schleife             ;5 Byte                                                             
        lfsr    0,xsm                   ;im Ram ab sxm
writes01:                               ;
        movf    POSTINC0,W              ;Schreiben
        movwf   data_ee_data            ;
        rcall   ewrite                  ;
        incf    data_ee_addr,F          ;
        decfsz schleife,F               ;
        bra     writes01                ;
        return                          ;
;##############################################################################
;##############################################################################
;##############################################################################
urladung:                               ;
        POINT   mgrunddatenanf          ;
urladungerweitert:
        bcf     INTCON,GIE              ;
        bsf     bintsperren             ;
        clrf    schleife                ;
        clrf    data_ee_addr            ;
urladung01:                             ;
        tblrd   *+                      ;
        movf    TABLAT,W                ;
        movwf   data_ee_data            ;
;........................................
;       movlw   0
;       call    LCDPos
;       movf    data_ee_data,W          ;
;       call    LCDHEX                  ;
;       goto    $
;........................................
        rcall   ewrite                  ;
        incf    data_ee_addr,F          ;
        decfsz schleife,F               ;
        bra     urladung01              ;
        goto    0                       ;
;------------------------------------------------------------------------------
;==============================================================================
;Schreiben in Eeprom
;Eingang:       Addr in data_ee_addr
;               Daten in data_ee_data
;Ausgang:       Daten im Eeprom intern
;==============================================================================
ewrite:                                 ;SCHLEIFE(1)
        btfsc   EECON1,WR               ; WR abfragen ob vorheriger WR fertig
        bra     ewrite                  ;ENDE(1) wr geloescht
        movf    data_ee_addr,W          ;Adresse holen
        movwf   EEADR                   ;
        movf    data_ee_data,W          ;Daten holen
        movwf   EEDATA
        bcf     EECON1, EEPGD           ;to Datamemory
        bcf     EECON1, CFGS            ;Access program FLASH or Data EEPROM memory
        bsf     EECON1, WREN            ;enable writes
ew1:
        bcf     INTCON,GIE              ;disable interrupts
        btfsc   INTCON,GIE              ;
        bra     ew1                     ;
        movlw   55h                     ;
        movwf   EECON2                  ;write 55h
        movlw   0aah                    ;
        movwf   EECON2                  ;write aah
        bsf     EECON1,WR               ;set wr bit to begin write
        btfss   bintsperren             ;? Ist Interrupt dauerhaft gesperrt
        bsf     INTCON, GIE             ;0 enable interrupts
        bcf     EECON1, WREN            ;disable writes
        return                          ;
;==============================================================================
;Lesen von Eeprom
;Eingang:       Addr in data_ee_addr
;Ausgang:       Daten in w und in data_ee_data
;==============================================================================
eread:                                  ;SCHLEIFE(1)
        btfsc   EECON1,WR               ; WR abfragen ob vorheriger WR fertig
        bra     eread                   ;ENDE(1) wr geloescht
        movf    data_ee_addr,W          ;Adresse laden
        movwf   EEADR                   ;
        bcf     EECON1,EEPGD            ;von Datamemory
        bcf     EECON1,CFGS             ;Access program FLASH or Data EEPROM memory
        bsf     EECON1,RD               ;enable READ
        movf    EEDATA,W                ;Daten holen
         movwf  data_ee_data            ;und speichern
         return                         ;
;------------------------------------------------------------------------------
;Lesen/Schreiben des Modes je nach Band und umspeichern in eine gemeinsame Variable
getmode:                                ;
         movf   modeA,W                 ;
         btfsc  bvfo                    ;
         movf   modeB,W                 ;
         movwf  modeneu                 ;Modeumschaltung vormerken
         return                         ;
setmode:                                ;
        btfsc   bvfo                    ;
        movff   mode,modeB              ;
        btfss   bvfo                    ;
        movff   mode,modeA              ;
        return                          ;
;##############################################################################
ddsinit:
        movf    ddstype,F
        bz      ad9833_ad9834_init      ;DDSTYPE == 0
        movlw   1
        subwf   ddstype,W
        bz      ad9850_init             ;DDSTYPE == 1
        movlw   2
        subwf   ddstype,W
        bz      ad9851_init             ;DDSTYPE == 2
        movlw   3
        subwf   ddstype,W
        bz      ad9951_1_init           ;DDSTYPE == 3
        movlw   4
        subwf   ddstype,W
        bz      ad9951_2_init           ;DDSTYPE == 4
        movlw   5
        subwf   ddstype,W
        bz      ad9951_3_init           ;DDSTYPE == 5
        movlw   6
        subwf   ddstype,W
        bz      ad9951_4_init           ;DDSTYPE == 6
        return
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;AD9850 und AD9851 init
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
ad9850_init:
ad9851_init:
        return
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;AD9850 und AD9851 init ende
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;AD9833 und AD9834 init
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
ad9833_ad9834_init:
        rcall   ad9833_ad9834_control_to_dds
        MOVLF   B'01000100',ddsword+1
        MOVLF   B'10111011',ddsword
        rcall   ad9833_ad9834_word_to_dds
        MOVLF   B'01000111',ddsword+1
        MOVLF   B'10101110',ddsword
        goto    ad9833_ad9834_word_to_dds
;-----------------------------------------------------------------------------
ad9833_ad9834_control_to_dds:
        MOVLF   B'00100000',ddsword+1
        MOVLF   B'00000000',ddsword
;-----------------------------------------------------------------------------
ad9833_ad9834_word_to_dds:
        bcf     a_fsync
        movf    ddsword+1,W
        rcall   ad9833_ad9834_byte_to_dds
        movf    ddsword,W
        rcall   ad9833_ad9834_byte_to_dds
        bsf     a_fsync
        return
;-----------------------------------------------------------------------------
ad9833_ad9834_byte_to_dds:
        movwf   temp
        MOVLF   .8,schleife
ad9833_ad9834_bytetodds01:
        rlcf    temp,F
        SKPC
        bcf     a_sdata
        SKPNC                                                                                            
        bsf     a_sdata
        bcf     a_sclk
        bsf     a_sclk
        decfsz schleife,F
        bra     ad9833_ad9834_bytetodds01
        return
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;AD9833 und AD9834 init ende
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;AD9951 init
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
ad9951_1_init:
        MOVLF   b'00100100',temp;Faktor 4
        bra     ad9951_init
ad9951_2_init:
        MOVLF   b'00101100',temp;Faktor 5
        bra     ad9951_init
ad9951_3_init:
        MOVLF   b'10100100',temp;LSB des cfr2 Registers
                                ;Faktor 20 ( 0x14)
        bra     ad9951_init
ad9951_4_init:
        MOVLF   b'00000100',temp;LSB des cfr2 Registers
                                ;ohne PLL
ad9951_init:
        bcf     a_sclk          ;
        bcf     a_fsync         ;
        bcf     a_sdata         ;
        bcf     a_reset         ;
        MOVLF   01h,sr0         ;Adresse des cfr2 Registers
        rcall   seroutDDS       ;
        clrf    sr0             ;0
        rcall   seroutDDS       ;
        clrf    sr0             ;0
        rcall   seroutDDS       ;
        movff   temp,sr0        ;
        rcall   seroutDDS       ;
        bsf     a_fsync         ;
        bcf     a_fsync         ;
        return
;------------------------------------------------------------------------------
seroutDDS:
        MOVLF   .8,schleife     ;
rot:
        rlcf    sr0,F           ;
        btfsc   STATUS,C        ;
        bsf     a_sdata         ;
        bsf     a_sclk          ;
        bcf     a_sclk          ;
        bcf     a_sdata         ;
        decfsz schleife,F       ;
        bra     rot             ;
        return                  ;
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;AD9951 init Ende
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;INIT Ende
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
rx2dds:                                         ;TXbin_speicher in den DDS laden
        LD4     ddsdword,ddsbinrx
frq2dds:
        movf    ddstype,F                       ;DDSType 0
        bz      ad9833_ad9834_frq28bit_to_dds
        movlw   1
        subwf   ddstype,W                       ;DDSType 1
        bz      ad9850_frq32bit_to_dds
        movlw   2
        subwf   ddstype,W                       ;DDSType 2
        bz      ad9851_frq32bit_to_dds
        bra     ad9951_frq32bit_to_dds          ;DDSType >= 3
        return
;..............................................................................
tx2dds:                                         ;RXbin_speicher in den DDS laden
         LD4    ddsdword,ddsbintx
         bra    frq2dds
;..............................................................................
ad9833_ad9834_frq28bit_to_dds:
        rcall   ad9833_ad9834_control_to_dds
        LD2     ddsword,ddsdword
        bcf     ddsword+1,7
        bsf     ddsword+1,6
        rcall   ad9833_ad9834_word_to_dds
        RL4     ddsdword
        RL4     ddsdword
        LD2     ddsword,ddsdword+2
        bcf     ddsword+1,7
        bsf     ddsword+1,6
        goto    ad9833_ad9834_word_to_dds
;..............................................................................
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;AD9851
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
ad9851_frq32bit_to_dds:
        clrf    temp+3          ;=null
        bsf     temp+3,0        ;AD9851 Oszi x 6
        bra     bintodds3       ;
;..............................................................................
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;AD9850
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
ad9850_frq32bit_to_dds:
        clrf    temp+3                   ;5. Frequenzbyte setzen
bintodds3:
        movff   ddsdword,temp+2          ; byte holen
        rcall   bittodds                 ; und zum DDS ausgeben
        movff   ddsdword+1,temp+2        ; byte holen
        rcall   bittodds                 ; und zum DDS ausgeben
        movff   ddsdword+2,temp+2        ; byte holen
        rcall   bittodds                 ; und zum DDS ausgeben
        movff   ddsdword+3,temp+2        ; byte holen
        rcall   bittodds                 ; und zum DDS ausgeben
        movff   temp+3,temp+2            ;5. Frequenzbyte holen
        rcall   bittodds                 ;5.Byte als 0 laden
        bsf     a_fsync                  ;fallende Flanke = Frequenzausgabe
        bcf     a_fsync                  ;
        return
;------------------------------------------------------------------------------
;Funktion       1 Byte in DDS laden
bittodds:
        MOVLF   8,schleife
bittodds1:                       ;SCHLEIFE(1)
        rrcf    temp+2,F        ; Bit nach rechts herausschieben
        btfss   STATUS,C        ; ? Bit = 1
        bra     bittodds2
        bsf     a_sdata         ; 1 datenbit setzen
        bra     bittodds3
bittodds2:
        bcf     a_sdata         ; 0 datenbit loeschen
bittodds3:
        bsf     a_sclk          ; fallende Flanke
        bcf     a_sclk          ; ist Uebernahme
        decfsz schleife,F       ;ENDE(1) nach 8 Durchlaeufen
        bra     bittodds1
        return
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;AD9951
;::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
ad9951_frq32bit_to_dds:
        movlw   04h             ;Adresse des Frequenzregisters
        movwf   sr0             ;
        rcall   seroutDDS       ;
        movff   ddsdword+3,sr0 ;
        rcall   seroutDDS       ;
        movff   ddsdword+2,sr0 ;
        rcall   seroutDDS       ;
        movff   ddsdword+1,sr0 ;
        rcall   seroutDDS       ;
        movff   ddsdword,sr0     ;
        rcall   seroutDDS       ;
        bsf     a_fsync         ;
        bcf     a_fsync         ;
        return                  ;
;##############################################################################
;    DDS Bereich zuende
;##############################################################################
;##############################################################################
;##############################################################################                         
setupnrmax      equ     D'25'           ;

SETUP:                                  ;
         bsf    a_licht                 ;
         call   LCDDisplayClear         ;
SETUP00:                                ;SCHLEIFE(1)
         call   Tastegedrueckt          ; ? Taste gedrueckt
         bc     SETUP00                 ;ENDE(1) Taste nicht mehr gedrueckt
         clrf   impulse                 ;
         MOVLF  1,setupnr               ;
SETUP01:                                ;
         rcall  LCDsetup                ;anzeige des ersten Menues
SETUP02:                                ;
         btfsc  impulse,7               ;? positive Drehrichtung
         bra    SETUP03                 ;
         movf   impulse,W               ;
         andlw  B'01111100'             ;1 ? Steps reduzieren/Steps angefallen
         bz     SETUP04                 ;
         incf   setupnr,F               ; 1 Ebene +1
         movlw  setupnrmax + 1          ;    ? maximale Ebene erreicht
         subwf  setupnr,W               ;
         bnz    SETUP05                 ;
         MOVLF  1,setupnr               ;    1 Ebene = 1
SETUP05:                                ;
         bra    SETUP06                 ;
SETUP03:                                ;
         movff  impulse,schleife+3      ;0 negative Drehrichtung
         negf   schleife+3              ; Impulse merken da nicht reduziert
         movf   schleife+3,W            ; und negieren
         andlw  B'11111100'             ; ? Steps reduzieren/Steps angefallen
         bz     SETUP04                 ;
         decf   setupnr,F               ; 1 Ebene -1
         incf   setupnr,W               ;    ? Ebene < 0
         bnz    SETUP06                 ;
         MOVLF  setupnrmax,setupnr      ;    1 Ebene == maxmenu
SETUP06:                                ;
         rcall  LCDsetup                ;anzeige des ersten Menues
         clrf   impulse                 ;
SETUP04:                                ;
        call    Tastegedrueckt          ;? Taste gedrueckt
        bnc     SETUP02                 ;
        call    quittungston            ;1 Quittungston ausgeben
        call    Tastaturstatus          ; ? Tasttaturabfrage Taste 1
        movlw   1                       ;
        subwf   tastennummer,W          ;
        bnz     SETUP07                 ;
        rcall   csetup                  ;
        goto    0                       ;Neustart
SETUP07:                                ;
        movlw   4                       ;
        subwf   tastennummer,W          ;
        bnz     SETUP04                 ;
        goto    menuende                ;Abbruch --> zurueck in Taskschleife
;..............................................................................
LCDsetup:                       ;
        LCDStrp stext           ;
        clrf    WREG            ;
        call    LCDPos          ;
        movlw   .1              ;
        subwf   setupnr,W       ;
        bnz     LCDsetup01      ;
        LCDStr stext1           ;
        btfss   blcdsmeter      ;
        rcall   lcd0            ;
        btfsc   blcdsmeter      ;
        rcall   lcd1            ;
        return                  ;
LCDsetup01:                     ;
        movlw   .2              ;
        subwf   setupnr,W       ;
        bnz     LCDsetup02      ;
        LCDStr stext2           ;
        btfss   blauto          ;
        rcall   lcd0            ;
        btfsc   blauto          ;
        rcall   lcd1            ;
        return                  ;
LCDsetup02:                     ;
        movlw   .3              ;
        subwf    setupnr,W      ;
        bnz      LCDsetup03     ;
        LCDStr   stext3         ;
        btfss    bbeleuchtung   ;
        rcall    lcd0           ;
        btfsc    bbeleuchtung   ;
        rcall    lcd1           ;
        return                  ;
LCDsetup03:                     ;
        movlw    .4             ;
        subwf    setupnr,W      ;
        bnz      LCDsetup04     ;
        LCDStr   stext4         ;
        btfss    b1hzanz        ;
        rcall    lcd0           ;
        btfsc    b1hzanz        ;
        rcall    lcd1           ;
        return                  ;
LCDsetup04:                     ;
        movlw    .5             ;
        subwf    setupnr,W      ;
        bnz      LCDsetup05     ;
        LCDStr   stext5         ;
        btfss    bkeyerein      ;
        rcall    lcd0           ;
        btfsc    bkeyerein      ;
        rcall    lcd1           ;
        return                  ;
LCDsetup05:                     ;
        movlw    .6             ;
        subwf    setupnr,W      ;
        bnz      LCDsetup051    ;
        LCDStr   stext6         ;
        btfss    btxpermzf      ;
        rcall    lcd0           ;
        btfsc    btxpermzf      ;
        rcall    lcd1           ;
        return                  ;
LCDsetup051:                    ;
        movlw    .7             ;
        subwf    setupnr,W      ;
        bnz      LCDsetup052    ;
        LCDStr   stext7         ;
        btfss    bminuszf       ;
        rcall    lcd0           ;
        btfsc    bminuszf       ;
        rcall    lcd1           ;
        return                  ;
LCDsetup052:                    ;
        movlw    .8             ;
        subwf    setupnr,W      ;
        bnz      LCDsetup053    ;
        LCDStr   stext7_1       ;
        btfss    bbandmode      ;
        rcall    lcd0           ;
        btfsc    bbandmode      ;
        rcall    lcd1           ;
        return                  ;
LCDsetup053:                    ;
        movlw    .9             ;
        subwf    setupnr,W      ;
        bnz      LCDsetup054    ;
        LCDStr   stext7_2       ;
        btfss    bvfox4         ;
        rcall    lcd0           ;
        btfsc    bvfox4         ;
        rcall    lcd1           ;
        return                  ;
LCDsetup054:                    ;
        movlw    .10            ;
        subwf    setupnr,W      ;
        bnz      LCDsetup06     ;
        LCDStr   stext8         ;
        return                  ;
LCDsetup06:                     ;
        movlw    .11            ;
        subwf    setupnr,W      ;
        bnz      LCDsetup07     ;
        LCDStr   stext9         ;
        return                  ;
LCDsetup07:                    ;                                      
        movlw    .12           ;
        subwf    setupnr,W     ;
        bnz      LCDsetup071   ;
        LCDStr   stext9_1          ;
        return                 ;
LCDsetup071:                   ;
        movlw    .13           ;
        subwf    setupnr,W     ;
        bnz      LCDsetup08    ;
        LCDStr   stext10       ;
        return                 ;
LCDsetup08:                    ;
        movlw    .14           ;
        subwf    setupnr,W     ;
        bnz      LCDsetup09    ;
        LCDStr   stext11       ;
        return                 ;
LCDsetup09:                    ;
        movlw    .15           ;
        subwf    setupnr,W     ;
        bnz      LCDsetup10    ;
        LCDStr   stext12       ;
        return                 ;
LCDsetup10:                    ;
        movlw    .16           ;
        subwf    setupnr,W     ;
        bnz      LCDsetup101   ;
        LCDStr   stext12_1         ;
        return                 ;
LCDsetup101:                   ;
        movlw    .17           ;
        subwf    setupnr,W     ;
        bnz      LCDsetup11    ;
        LCDStr   stext13       ;
        goto     type99        ;
LCDsetup11:                    ;
        movlw    .18           ;
        subwf    setupnr,W     ;
        bnz      LCDsetup12    ;
        LCDStr   stext14       ;
        return                 ;
LCDsetup12:                    ;
        movlw    .19           ;
        subwf    setupnr,W     ;
        bnz      LCDsetup13    ;
        LCDStr   stext15       ;
        return                 ;
LCDsetup13:                    ;
        movlw    .20           ;
        subwf    setupnr,W     ;
        bnz      LCDsetup14    ;
        LCDStr   stext15_1     ;
        return                 ;
LCDsetup14:                    ;
        movlw    .21           ;
        subwf    setupnr,W     ;
        bnz      LCDsetup141   ;
        LCDStr   stext15_2     ;
        return                 ;
LCDsetup141:                   ;
        movlw    .22           ;
        subwf    setupnr,W     ;
        bnz      LCDsetup15    ;
        LCDStr   stext16       ;
        return                 ;
LCDsetup15:                    ;
        movlw    .23           ;
        subwf    setupnr,W     ;
        bnz      LCDsetup16    ;
        LCDStr   stext17       ;
        return                 ;
LCDsetup16:                    ;
        movlw    .24           ;
        subwf    setupnr,W     ;
        bnz      LCDsetup17    ;
        LCDStr   stext17_1     ;
        return                 ;
LCDsetup17:                    ;
        movlw    .25           ;
        subwf   setupnr,W       ;
        bnz     LCDsetup18      ;
        LCDStr stext17_2        ;
        return                  ;
LCDsetup18:                     ;
        return                  ;
;------------------------------------------------------------------------------
;Anzeige "1" im Display
lcd1:
        movlw   '1'             ;
        goto    LCDChar         ;
;------------------------------------------------------------------------------
;Anzeige "0" im Display
lcd0:
        movlw   '0'             ;
        goto    LCDChar         ;
;..............................................................................
;Funktion       Sprung zur entsprechenden Setupfunktion
;..............................................................................
csetup:                         ;SCHLEIFE(1)
        call    Tastegedrueckt ;
        bc      csetup          ;ENDE(1) keine Taste gedrueckt
        call    LCDDisplayClear ;LCD loeschen
        movlw   .1              ;Funktionsnummer
        subwf   setupnr,W       ;
        bnz     csetup01        ;
        bra     csmeter         ;S-Meter ein/aus
csetup01:                       ;
        movlw   .2              ;
        subwf   setupnr,W       ;
        bnz     csetup02        ;
        bra     slightauto      ;Lichtautomatik ein/aus
csetup02:                       ;
        movlw   .3              ;
        subwf   setupnr,W       ;
        bnz     csetup03        ;
        bra     slight          ;Licht dauerhaft ein/aus
csetup03:                       ;
        movlw   .4              ;
        subwf   setupnr,W       ;
        bnz     csetup04        ;
        bra     s1hz            ;1Hz Anzeige ein/aus
csetup04:                       ;
        movlw   .5              ;
        subwf   setupnr,W       ;
        bnz     csetup05        ;
        bra     skey            ;Keyer ein/aus
csetup05:                       ;
        movlw   .6              ;
        subwf   setupnr,W       ;
        bnz     csetup051       ;
        bra     ctxzf           ;Beim Senden +/- ZF
csetup051:                      ;
        movlw   .7              ;
        subwf   setupnr,W       ;
        bnz     csetup052       ;
        bra     sminuszf        ;VFO = RXfrq +/- ZF
csetup052:                      ;
        movlw   .8              ;
        subwf   setupnr,W       ;
        bnz     csetup053       ;
        bra     sbandmode       ;Band Modeumschaltung aktiv
csetup053:                      ;
        movlw   .9              ;
        subwf   setupnr,W       ;
        bnz     csetup054       ;
        bra     svfox4          ;VFO x 4
csetup054:                      ;
        movlw   .10             ;
        subwf   setupnr,W       ;
        bnz     csetup06        ;
        bra     sddskonst       ;DDS-Konstande aendern
csetup06:                       ;
        movlw   .11             ;
        subwf   setupnr,W       ;
        bnz     csetup07        ;
        MOVLF   modecw,mode     ;
        bra     szfkonst        ;ZF CW aendern
csetup07:                       ;
        movlw   .12             ;
        subwf   setupnr,W       ;                                                                       
        bnz     csetup071       ;
        MOVLF   modecwr,mode    ;
        bra     szfkonst        ;ZF CWr aendern
csetup071:                      ;
        movlw   .13             ;
        subwf   setupnr,W       ;
        bnz     csetup08        ;
        MOVLF   modelsb,mode    ;
        bra     szfkonst        ;ZF LSB aendern
csetup08:                       ;
        movlw   .14             ;
        subwf   setupnr,W       ;
        bnz     csetup09        ;
        MOVLF   modeusb,mode    ;
        bra     szfkonst        ;ZF USB aendern
csetup09:                       ;
        movlw   .15             ;
        subwf   setupnr,W       ;
        bnz     csetup10        ;
        MOVLF   modedig,mode    ;
        bra     szfkonst        ;ZF digital aendern
csetup10:                       ;
        movlw   .16             ;
        subwf   setupnr,W       ;
        bnz     csetup101       ;
        MOVLF   modedigr,mode   ;
        bra     szfkonst        ;ZF digital revers aendern
csetup101:                      ;
        movlw   .17             ;
        subwf   setupnr,W       ;
        bnz     csetup11        ;
        goto    sddstype        ;DDS-Type auswaehlen
csetup11:                       ;
        movlw   .18             ;
        subwf   setupnr,W       ;
        bnz     csetup12        ;
        bra     stransverter    ;LCDoffset aendern
csetup12:                       ;
        movlw   .19             ;
        subwf   setupnr,W       ;
        bnz     csetup13        ;
        goto    seichsmeter     ;Kalibrieren S-Meter
csetup13:                       ;
        movlw   .20             ;
        subwf   setupnr,W       ;
        bnz     csetup13_1      ;
        goto    sspot           ;Tonhoehe Mithoerton einstellen
csetup13_1:                     ;
        movlw   .21             ;
        subwf   setupnr,W       ;
        bnz     csetup13_2      ;
        goto    stxhang         ;Tonhoehe Mithoerton einstellen
csetup13_2:                     ;
        movlw   .22             ;
        subwf   setupnr,W       ;
        bnz     csetup14        ;
        goto    sendeeprom      ;Eeprom Inhalt senden
csetup14:                       ;
        movlw   .23             ;
        subwf   setupnr,W       ;
        bnz     csetup15        ;
        goto    leseeprom       ;Eeprom Inhalt senden
csetup15:                       ;
        movlw   .24             ;
        subwf   setupnr,W       ;
        bnz     csetup16        ;
        goto    ee2flash        ;Eeprom in Flash speichern
csetup16:                       ;
        movlw   .25             ;
        subwf   setupnr,W       ;
        bnz     csetup17        ;
        goto    flash2ee        ;Flash in Eeprom laden
csetup17:                       ;
        return                  ;
;..............................................................................
;Funktion       Aendern der ZF im Ram
;               Anzeige dezimal mit Komma
szfkonst:
        movff   mode,modeA              ;Mode an die richtige Stelle kopieren
          bcf       bvfo                    ;FrequenzA einstellen
          clrf      impulse                 ;Drehimpulse auf 0 stellen
          call      ezfread                 ;entsprechende ZF lesen
          bsf       bohnezf                 ;ZF-Berechnung ausschalten
          movff     zwischenfrequenz,mddskonst      ;ZF in Zwischenspeicher
          movff     zwischenfrequenz+1,mddskonst+1 ;kopieren
          movff     zwischenfrequenz+2,mddskonst+2 ;
          movff     zwischenfrequenz+3,mddskonst+3 ;
          call      LCDDisplayClear         ;LCD loeschen
          LCDStrp   qtext6                  ;"<   >    X     ok"
          movlw     .0                      ;zuordnen
          movwf     stemp                   ;Stellenlaenge = 6
          rcall     z1clr                   ;Zeile1 loeschen
          LCDStr    tmhz                    ;MHz dahinter
szfk00:                                     ;SCHLEIFE(3)
         movff      mddskonst,frequenza     ; Zwischenspeicher in VFOa
         movff      mddskonst+1,frequenza+1 ; kopieren
         movff      mddskonst+2,frequenza+2 ; ZF wird direkt am DDS ausgegeben
         movff      mddskonst+3,frequenza+3 ;
         call       ddsbinausrechnen        ; BINs neu ausrechnen tx + rx
         call       bin2ddsrx               ; RX-Frequenz in DDS laden
         clrf       WREG                    ; auf LCD neue
         call       LCDPos                  ; Frequenz anzeigen
         movff      mddskonst,faktor1       ;
         movff      mddskonst+1,faktor1+1   ;
         movff      mddskonst+2,faktor1+2   ;
         movff      mddskonst+3,faktor1+3   ;
         call       hex2bcd5b               ;
         call       bcd2char6b              ;
         MOVLF      .10,laenge              ;
         MOVLF      .6,komma                ;
         movff      stemp,kuerzen           ;
         call       printf                  ;
szfk001:
         movf       impulse,F              ;   ? Impulse angefallen
         bnz        szfk002                ;
         bra        szfk022
szfk002:
         CLR4       stemp1                 ;
         btfsc      impulse,7              ;   1 ? positive Drehrichtung
         bra        szfk023                ;
         movf       stemp,F                ;     1 ? Anzahl der Stellen
         bnz        szfk024                ;       8 Stellen aendern
         INC4       mddskonst              ;
         bra        szfk0224               ;
szfk024:
         movlw      .2                     ;       6 Stellen aendern
         subwf      stemp,W                ;
         bnz        szfk025                ;
         MOVLF      .100,stemp1            ;
         ADD4       mddskonst,stemp1       ;
         bra        szfk0224               ;
szfk025:
         movlw      .4                     ;       4 Stellen aendern
         subwf      stemp,W                ;
         bnz        szfk026                ;
         MOVLF      0x10,stemp1            ;
         MOVLF      0x27,stemp1+1          ;
         ADD4       mddskonst,stemp1       ;
         bra        szfk0224               ;
szfk026:                                   ;
         movlw      .6                     ;       2 Stellen aendern
         subwf      stemp,W                ;
         bnz        szfk027                ;
         MOVLF      0x40,stemp1            ;
         MOVLF      0x42,stemp1+1          ;
         MOVLF      0x0f,stemp1+2          ;
         ADD4       mddskonst,stemp1       ;
         bra        szfk0224               ;
szfk027:                                   ;   0 negative Drehrichtung
szfk023:                                   ;   0 negative Drehrichtung
        movf        stemp,F                ;     ? Stellen
        bnz         szfk0221               ;
        DEC4        mddskonst              ;    8 Stellen aendern
        bra         szfk0224               ;
szfk0221:
        movlw       .2                     ;    6 Stellen aendern
        subwf       stemp,W                ;
        bnz         szfk0222               ;
        MOVLF       .100,stemp1            ;
         SUB4     mddskonst,stemp1       ;                                                                  
         bra      szfk0224               ;
szfk0222:
         movlw    .4                     ;     4 Stellen aendern
         subwf    stemp,W                ;
         bnz      szfk0223               ;
         MOVLF    0x10,stemp1            ;
         MOVLF    0x27,stemp1+1          ;
         SUB4     mddskonst,stemp1       ;
         bra      szfk0224               ;
szfk0223:
         movlw    .6                     ;     2 Stellen aendern
         subwf    stemp,W                ;
         bnz      szfk0224               ;
         MOVLF    0x40,stemp1            ;
         MOVLF    0x42,stemp1+1          ;
         MOVLF    0x0f,stemp1+2          ;
         SUB4     mddskonst,stemp1       ;
szfk0224:
         clrf     impulse                ;     0 Impulse loeschen
         bra      szfk00                 ;
szfk022:                                 ;
         call     Tastegedrueckt         ;   ? Taste gedrueckt
         bc       szfk0231               ;
         bra      szfk001                ;
szfk0231:
         call     quittungston           ; 1 Quittungston ausgeben
         call     Tastaturstatus         ;    ? Welche Taste
         movlw    .1                     ;
         subwf    tastennummer,W         ;
         bnz      szfk03                 ;
         incf     stemp,F                ;    1 Taste 1
         incf     stemp,F                ;
         movlw    .8                     ;
         subwf    stemp,W                ;      ? min erreicht
         bnz      szfk021                ;
         clrf     stemp                  ;      1 wieder mit 0 laden
szfk021:                                 ;
         bra      szfk98                 ;
szfk03:                                  ;
         movlw    .2                     ;
         subwf    tastennummer,W         ;
         bnz      szfk04                 ;
         decf     stemp,F                ;    2 Taste 2
         decf     stemp,F                ;
         btfss    stemp,7                ;      ? min erreicht Ueberlauf unter null
         bra      szfk031                ;
         movlw    .6                     ;      1 wieder max einstellen
         movwf    stemp                  ;
szfk031:                                 ;
         bra      szfk06                 ;
szfk04:                                  ;
         movlw    .3                     ;
         subwf    tastennummer,W         ;
         bnz      szfk05                 ;
         bra      szfk99X                ;    3 Taste 3 Cancel
szfk05:                                  ;
         movlw    .4                     ;
         subwf    tastennummer,W         ;
         bnz      szfk06                 ;
         bra      szfk99ok               ;    4 Taste 4 OK
szfk06:                                  ;
         rcall    z1clr                  ; Zeile1 loeschen
         LCDStr   tmhz                   ; MHz dahinter
szfk98:                                  ;
        call      Tastegedrueckt         ; SCHLEIFE(2)
        bc        szfk98                 ; ENDE(2) Taste nicht mehr gedrueckt
        bra       szfk00                 ;ENDE(3) Taste X oder OK
szfk99X:
        return                            ;X
szfk99ok:                                 ;OK
        movff     mddskonst,zwischenfrequenz     ;Zwischenspeicher wieder
        movff     mddskonst+1,zwischenfrequenz+1 ;in ZF laden
        movff     mddskonst+2,zwischenfrequenz+2 ;zum Abspeichern
        movff     mddskonst+3,zwischenfrequenz+3 ;
        goto      ezfwrite                ; zf speichern

tmhz    db      "MHz",0
;..............................................................................
z1clr:
         clrf    WREG                   ; Pos 0 auf LCD
         call    LCDPos                 ;
         movlw   .13                    ; 1. zeile loeschen
         goto    LCDSpacel              ;
;..............................................................................
sddskonst:
         bcf     bvfo                   ;FrequenzA einstellen
         bsf     bohnezf                ;ZF aus
         movff   ddskonst,mddskonst     ;DDS Konstande
         movff   ddskonst+1,mddskonst+1 ;kopieren
         movff   ddskonst+2,mddskonst+2 ;
         movff   ddskonst+3,mddskonst+3 ;
         movff   ddskonst+4,mddskonst+4 ;
         call    LCDDisplayClear        ;LCD loeschen
         LCDStrp qtext6                 ;"<   >    X    ok"
         movlw   .5                     ;zuordnen
         movwf   stemp                  ;Stellenlaenge = 5
sddsk00:                                ;SCHLEIFE(3)
         call    ddsbinausrechnen       ; BINs neu ausrechnen tx + rx
         call    bin2ddsrx              ; RX-Frequenz laden
         rcall   z1clr                  ; Zeile 1 loeschen
         clrf    WREG                   ; an den Anfang der zeile
         call    LCDPos                 ; und
         movff   stemp,schleife         ; HEX neu auf LCD
         lfsr    0,ddskonst+4           ; darstellen
sddsk01:                                ; SCHLEIFE(1)
        movf     POSTDEC0,W             ;    je nach laenge
        call     LCDHEX                 ;    HEX darstellen
        decfsz schleife,F               ;
        bra      sddsk01                ; ENDE(1) stemp==0
sddsk02:                                ;
        movf     impulse,F              ; ? Impulse angefallen
        bz       sddsk022               ;
        btfsc    impulse,7              ; 1 ? positive Drehrichtung
        bra      sddsk023               ;
        movlw    .5                     ;    1 ? Anzahl der Stellen
        subwf    stemp,W                ;      5 Stellen aendern
        bnz      sddsk024               ;
        INC5     ddskonst               ;
sddsk024:                               ;
        movlw    .4                     ;      4 Stellen aendern
        subwf    stemp,W                ;
        bnz      sddsk025               ;
        INC4     ddskonst+1             ;
sddsk025:                               ;
        movlw    .3                     ;      3 Stellen aendern
        subwf    stemp,W                ;
        bnz      sddsk026               ;
        INC3     ddskonst+2             ;
sddsk026:                               ;
        movlw    .2                     ;      2 Stellen aendern
        subwf    stemp,W                ;
        bnz      sddsk027               ;
        INC2     ddskonst+3             ;
sddsk027:                               ;
        movlw    .1                     ;      1 Stelle aendern
        subwf    stemp,W                ;
        bnz      sddsk028               ;
        incf     ddskonst+4,F           ;
sddsk028:                               ;
        bra      sddsk0225              ;
sddsk023:                               ; 0 negative Drehrichtung
        movlw    .5                     ;    ? Anzahl der Stellen
        subwf    stemp,W                ;    5 Stellen aendern
        bnz      sddsk0221              ;
        DEC5     ddskonst               ;
sddsk0221:                              ;
        movlw    .4                     ;    4 Stellen aendern
        subwf    stemp,W                ;
        bnz      sddsk0222              ;
        DEC4     ddskonst+1             ;
sddsk0222:                              ;
        movlw    .3                     ;    3 Stellen aendern
        subwf    stemp,W                ;
        bnz      sddsk0223              ;
        DEC3     ddskonst+2             ;
sddsk0223:                              ;
        movlw    .2                     ;    2 Stellen aendern
        subwf    stemp,W                ;
        bnz      sddsk0224              ;
         DEC2   ddskonst+3              ;                                                                  
sddsk0224:                              ;
         movlw  .1                      ;    1 Stelle aendern
         subwf  stemp,W                 ;
         bnz    sddsk0225               ;
         decf   ddskonst+4,F            ;
sddsk0225:                              ;
         clrf   impulse                 ;    01Impulse loeschen
         bra    sddsk00                 ;
sddsk022:                               ;
         call   Tastegedrueckt          ; ? Taste gedrueckt
         bnc    sddsk02                 ;
         call   quittungston            ; 1 Quittungston ausgeben
         call   Tastaturstatus          ;    ? Welche Taste
         movlw  .1                      ;
         subwf  tastennummer,W          ;
         bnz    sddsk03                 ;
         decf   stemp,F                 ;    1 Taste 1
         movf   stemp,F                 ;       ? min erreicht
         bnz    sddsk021                ;
         movlw  .5                      ;       1 wieder mit max beginnen
         movwf  stemp                   ;
sddsk021:                               ;
         bra    sddsk98                 ;
sddsk03:                                ;
         movlw  .2                      ;
         subwf  tastennummer,W          ;
         bnz    sddsk04                 ;
         incf   stemp,F                 ;    2 Taste 2
         movlw  .6                      ;       ? max erreicht
         subwf  stemp,W                 ;
         bnz    sddsk031                ;
         movlw  .1                      ;       1 wieder mit min beginnen
         movwf  stemp                   ;
sddsk031:                               ;
         bra    sddsk98                 ;
sddsk04:                                ;
         movlw  .3                      ;
         subwf  tastennummer,W          ;
         bnz    sddsk05                 ;
         bra    sddsk99X                ;    3 Taste 3 Cancel
sddsk05:                                ;
         movlw  .4                      ;
         subwf  tastennummer,W          ;
         bnz    sddsk06                 ;
         bra    sddsk99ok               ;    4 Taste 4 OK
sddsk06:                                ;
sddsk98:                                ;
         call   Tastegedrueckt          ; SCHLEIFE(2)
         bc     sddsk98                 ; ENDE(2) Taste nicht mehr gedrueckt
         bra    sddsk00                 ;ENDE(3) Taste X oder OK
sddsk99X:                               ;
         movff  mddskonst,ddskonst      ;Cancel
         movff  mddskonst+1,ddskonst+1 ;DDSKonstande alt zurueckkopieren
         movff  mddskonst+2,ddskonst+2 ;
         movff  mddskonst+3,ddskonst+3 ;
         movff  mddskonst+4,ddskonst+4 ;
         return                         ;
sddsk99ok:                              ;OK
         movlw  addrddskonst            ;Adresse im Eeprom
         movwf  data_ee_addr            ;Vorbereiten zum Eepromlesen adr festlegen
         lfsr   0,ddskonst              ;Adresse im RAM
         MOVLF  5,schleife              ;2 Byte schreiben
         goto   lewrite                 ;ModeA + ModeB schreiben
;..............................................................................
sminuszf:                               ;
        LCDStrp qtext1                  ;"X   X     on off"
        LCDStrp qtext11                 ;"VFO = RXfrq - ZF"
smzfa01:                                ;
        call    Tastegedrueckt          ;
        bnc     smzfa01                 ;
        call    Tastaturstatus          ;
        movlw   .1                      ;
        subwf   tastennummer,W          ;
        bz      smzf05                  ;
smzf01:                                 ;
        movlw   .2                      ;
        subwf   tastennummer,W          ;
        bz      smzf05                  ;
smzf02:                                 ;
        movlw   .3                      ;
        subwf   tastennummer,W          ;
        bnz     smzf03                  ;
        bsf     bminuszf                ;
        bra     smzf04                  ;
smzf03:                                 ;
         movlw   .4                     ;
         subwf   tastennummer,W         ;
         bnz     smzf05                 ;
         bcf     bminuszf               ;
smzf04:                                 ;
         call    ebandwrite             ;Banddaten speichern
smzf05:                                 ;
         return                         ;
;..............................................................................
svfox4:                         ;
         LCDStrp qtext20                ;"VFOx4 I/Q Mixer"
         rcall   readflag               ;
svfox4a01:                              ;
         call    Tastegedrueckt         ;
         bnc     svfox4a01              ;
         call    Tastaturstatus         ;
         movlw   .1                     ;
         subwf   tastennummer,W         ;
         bz      svfox405               ;
svfox401:                               ;
         movlw   .2                     ;
         subwf   tastennummer,W         ;
         bz      svfox405               ;
svfox402:                               ;
         movlw   .3                     ;
         subwf   tastennummer,W         ;
         bnz     svfox403               ;
         bsf     ergebnis,7             ;
         bra     svfox404               ;
svfox403:                               ;
         movlw   .4                     ;
         subwf   tastennummer,W         ;
         bnz     svfox405               ;
         bcf     ergebnis,7             ;
svfox404:                               ;
         bra     writeflag              ;
svfox405:                               ;
         return                         ;
;..............................................................................
sbandmode:                              ;
         LCDStrp qtext19                ;"Band/Mode aktiv "
         rcall   readflag               ;
sbma01:                         ;
         call    Tastegedrueckt         ;
         bnc     sbma01                 ;
         call    Tastaturstatus         ;
         movlw   .1                     ;
         subwf   tastennummer,W         ;
         bz      sbm05                  ;
sbm01:                                  ;
         movlw   .2                     ;
         subwf   tastennummer,W         ;
         bz      sbm05                  ;
sbm02:                                  ;
         movlw   .3                     ;
         subwf   tastennummer,W         ;
         bnz     sbm03                  ;
         bsf     ergebnis,6             ;
         bra     sbm04                  ;
sbm03:                                  ;
         movlw   .4                     ;
         subwf   tastennummer,W         ;
         bnz     sbm05                  ;
         bcf     ergebnis,6             ;
sbm04:                                  ;
         bra     writeflag              ;
sbm05:                                  ;
         return                         ;
;..............................................................................
skey:                                   ;
        LCDStrp qtext7                  ;"Keyer           "
        rcall    readflag               ;
skeya01:                                ;
        call     Tastegedrueckt         ;
        bnc     skeya01                 ;                                                               
        call    Tastaturstatus          ;
        movlw   .1                      ;
        subwf   tastennummer,W          ;
        bz      skey05                  ;
skey01:                                 ;
         movlw  .2                      ;
         subwf  tastennummer,W          ;
         bz     skey05                  ;
skey02:                                 ;
         movlw  .3                      ;
         subwf  tastennummer,W          ;
         bnz    skey03                  ;
         bsf    ergebnis,4              ;
         bra    skey04                  ;
skey03:                                 ;
         movlw  .4                      ;
         subwf  tastennummer,W          ;
         bnz    skey05                  ;
         bcf    ergebnis,4              ;
skey04:                                 ;
         bra    writeflag               ;
skey05:                                 ;
         return                         ;
;..............................................................................
s1hz:                                   ;
        LCDStrp qtext5                  ;"1 Hz Aufloesung "
        rcall   readflag                ;
s1hza01:                                ;
        call    Tastegedrueckt          ;
        bnc     s1hza01                 ;
        call    Tastaturstatus          ;
        movlw   .1                      ;
        subwf   tastennummer,W          ;
        bz      s1hz05                  ;
s1hz01:                                 ;
        movlw   .2                      ;
        subwf   tastennummer,W          ;
        bz      s1hz05                  ;
s1hz02:                                 ;
        movlw   .3                      ;
        subwf   tastennummer,W          ;
        bnz     s1hz03                  ;
        bsf     ergebnis,3              ;
        bra     s1hz04                  ;
s1hz03:                                 ;
        movlw   .4                      ;
        subwf   tastennummer,W          ;
        bnz     s1hz04                  ;
        bcf     ergebnis,3              ;
s1hz04:                                 ;
        bra     writeflag               ;
s1hz05:                                 ;
        return                          ;
;..............................................................................
slightauto:                             ;
        LCDStrp qtext2                  ;"Lichtautomatik "
        rcall   readflag                ;
slightauto01:                           ;
        call    Tastegedrueckt          ;
        bnc     slightauto01            ;
        call    quittungston            ;1 Quittungston ausgeben
        call    Tastaturstatus          ;
        movlw   .1                      ;
        subwf   tastennummer,W          ;
        bz      slauto05                ;
slauto01:                               ;
        movlw   .2                      ;
        subwf   tastennummer,W          ;
        bz      slauto05                ;
slauto02:                               ;
        movlw   .3                      ;
        subwf   tastennummer,W          ;
        bnz     slauto03                ;
        bsf     ergebnis,2              ;
        bcf     ergebnis,1              ;
        bra     slauto04                ;
slauto03:                               ;
        movlw   .4                      ;
        subwf   tastennummer,W          ;
         bnz     slauto04               ;
         bcf     ergebnis,2             ;
         bcf     ergebnis,1             ;
slauto04:                               ;
         bra     writeflag              ;
slauto05:                               ;
         return                         ;
;...............................................................................
slight:                                 ;
         LCDStrp qtext3                 ;"Licht           "
         rcall   readflag               ;
slight01:                               ;
         call    Tastegedrueckt         ;
         bnc     slight01               ;
         call    quittungston           ; 1 Quittungston ausgeben
         call    Tastaturstatus         ;
         movlw   .1                     ;
         subwf   tastennummer,W         ;
         bz      slight06               ;
slight02:                               ;
         movlw   .2                     ;
         subwf   tastennummer,W         ;
         bz      slight06               ;
slight03:                               ;
         movlw   .3                     ;
         subwf   tastennummer,W         ;
         bnz     slight04               ;
         bsf     ergebnis,1             ;
         bcf     ergebnis,2             ;
         bsf     a_licht                ;
         bra     slight05               ;
slight04:                               ;
         movlw   .4                     ;
         subwf   tastennummer,W         ;
         bnz     slight06               ;
         bcf     ergebnis,1             ;
         bcf     ergebnis,2             ;
slight05:                               ;
         bra     writeflag              ;
slight06:                               ;
        return                          ;
;...............................................................................
readflag:
        MOVLF    addrflag,data_ee_addr  ;
        call     eread                  ;
        movff    data_ee_data,ergebnis  ;
;        call    LCDDisplayClear
;        movf    data_ee_data,W
;        call    LCDHEX
;        call    t5Sek
        LCDStrp qtext1                  ;"X   X    on off"
        return
;...............................................................................
writeflag:
        movff    ergebnis,data_ee_data  ;
        MOVLF    addrflag,data_ee_addr  ;
;        call    LCDDisplayClear
;        movf    data_ee_data,W
;        call    LCDHEX
;        call    t5Sek
        goto     ewrite                 ;
;...............................................................................
csmeter:                                ;
        LCDStrp qtext4                  ;"S-Meter         "
        rcall    readflag               ;
csmeter01:                              ;
        call     Tastegedrueckt         ;
        bnc      csmeter01              ;
        call     quittungston           ;1 Quittungston ausgeben
        call     Tastaturstatus         ;
        movlw    .1                     ;
        subwf    tastennummer,W         ;
        bz       csmeter06              ;
csmeter02:                              ;
        movlw    .2                     ;
        subwf    tastennummer,W         ;
        bz       csmeter06              ;
csmeter03:                              ;
        movlw    .3                     ;
        subwf    tastennummer,W         ;
         bnz     csmeter04               ;                                                               
         bsf     ergebnis,0              ;
         bra     csmeter05               ;
csmeter04:                               ;
         movlw   .4                      ;
         subwf   tastennummer,W          ;
         bnz     csmeter06               ;
         bcf     ergebnis,0              ;
csmeter05:                               ;
         bra     writeflag               ;
csmeter06:                               ;
         return                          ;
;...............................................................................
ctxzf:                                   ;
         LCDStrp qtext8                  ;"TX ZF permanent "
         rcall   readflag                ;
ctxzf01:                                 ;
         call    Tastegedrueckt          ;
         bnc     ctxzf01                 ;
         call    quittungston            ;1 Quittungston ausgeben
         call    Tastaturstatus          ;
         movlw   .1                      ;
         subwf   tastennummer,W          ;
         bz      ctxzf06                 ;
ctxzf02:                                 ;
         movlw   .2                      ;
         subwf   tastennummer,W          ;
         bz      ctxzf06                 ;
ctxzf03:                                 ;
         movlw   .3                      ;
         subwf   tastennummer,W          ;
         bnz     ctxzf04                 ;
         bsf     ergebnis,5              ;
         bra     ctxzf05                 ;
ctxzf04:                                 ;
        movlw    .4                      ;
        subwf    tastennummer,W          ;
        bnz      ctxzf06                 ;
        bcf      ergebnis,5              ;
ctxzf05:
        bra      writeflag               ;
ctxzf06:                                 ;
        return                           ;
;...............................................................................
stransverter:
        LCDStrp qtext9                   ;"Displayshift=0 "
        LCDStrp qtext10                  ;"ok           next"
str01:                                   ;SCHLEIFE(1,3)
        call     Tastegedrueckt          ;
        bnc      str01                   ; ENDE(1) Taste gedrueckt
        call     Tastaturstatus          ; Abfrage der Taste
        movlw    4                       ; ? Taste 4
        subwf    tastennummer,W          ;
        bz       str02                   ; 1 ---> weiter
        movlw    1                       ; ? Taste 1
        subwf    tastennummer,W          ;
        bnz      str03                   ;
        CLR5     lcdoffset               ; 1 loeschen vom LCD offset
        bra      strans99null            ;    ---> break zum Speichern
str03:                                   ; SCHLEIFE(2)
        call     Tastegedrueckt          ;
        bc       str03                   ; ENDE(2) keine Taste gedrueckt
        bra      str01                   ;ENDE(3) ewige Schleife
str02:
        call     Tastegedrueckt          ;
        bc       str02                   ;
stransonst
        bcf      bvfo                    ;FrequenzA einstellen
        clrf     impulse                 ;
        movff    lcdoffset,mddskonst     ;LCDoffset Konstande
        movff    lcdoffset+1,mddskonst+1 ;kopieren
        movff    lcdoffset+2,mddskonst+2 ;
        movff    lcdoffset+3,mddskonst+3 ;
        movff    lcdoffset+4,mddskonst+4 ;
        ADD5     mddskonst,frequenza     ;
        call     LCDDisplayClear         ;LCD loeschen
        LCDStrp qtext6                   ;"<   >    X     ok"
        movlw    .0                      ;zuordnen
        movwf    stemp                   ;Stellenlaenge = 6
        rcall    z1clr                   ;Zeile1 loeschen
        LCDStr   tmhz                    ;MHz dahinter
strans00:                                ;SCHLEIFE(3)
        clrf     WREG                    ;
        call     LCDPos                  ; Pos 0 in LCD
        movff    mddskonst,faktor1       ; in Operator
        movff    mddskonst+1,faktor1+1   ; kopieren
        movff    mddskonst+2,faktor1+2   ;
        movff    mddskonst+3,faktor1+3   ;
        movff    mddskonst+4,faktor1+4   ;
        call     hex2bcd5b               ; in BCD wandeln
        call     bcd2char6b              ; in Char wandeln
        MOVLF    .11,laenge              ; printf Ausgabe
        MOVLF    .6,komma                ;
        movff    stemp,kuerzen           ;
        call     printf                  ;
strans001:                               ;
        movf     impulse,F               ; ? Impulse angefallen
        bnz      strans002               ;
        bra      strans022               ;
strans002:                               ; 1
        CLR5     stemp1                  ;    loeschen des Operators
        btfsc    impulse,7               ;    ? positive Drehrichtung
        bra      strans023               ;
        movf     stemp,F                 ;    1 ? Anzahl der Stellen
        bnz      strans024               ;      10 Stellen aendern
        INC5     mddskonst               ;
        bra      strans0225              ;
strans024:                               ;
        movlw    .2                      ;      8 Stellen aendern
        subwf    stemp,W                 ;
        bnz      strans025               ;
        MOVLF    .100,stemp1             ;      100 addieren
        ADD5     mddskonst,stemp1        ;
        bra      strans0225              ;
strans025:                               ;
        movlw    .4                      ;      6 Stellen aendern
        subwf    stemp,W                 ;
        bnz      strans026               ;      10 000 addieren
        MOVLF    0x10,stemp1             ;
        MOVLF    0x27,stemp1+1           ;
        ADD5     mddskonst,stemp1        ;
        bra      strans0225              ;
strans026:                               ;
        movlw    .6                      ;      4 Stellen aendern
        subwf    stemp,W                 ;
        bnz      strans027               ;
        MOVLF    0x40,stemp1             ;      1 000 000 addieren
        MOVLF    0x42,stemp1+1           ;
        MOVLF    0x0f,stemp1+2           ;
        ADD5     mddskonst,stemp1        ;
        bra      strans0225              ;
strans027:                               ;
        movlw    .8                      ;      2 Stellen aendern
        subwf    stemp,W                 ;
        bnz      strans028               ;
        MOVLF    0xe1,stemp1+1           ;      100 000 000 addieren
        MOVLF    0xf5,stemp1+2           ;
        MOVLF    0x05,stemp1+3           ;
        ADD5     mddskonst,stemp1        ;
        bra      strans0225              ;
strans028:                               ;
strans023:                               ; 0 negative Drehrichtung
        movf     stemp,F                 ;    ? Stellen
        bnz      strans0221              ;
        DEC5     mddskonst               ;    10 Stellen aendern
        bra      strans0225              ;
strans0221:                              ;
        movlw    .2                      ;    8 Stellen aendern
        subwf    stemp,W                 ;
        bnz      strans0222              ;
        MOVLF    .100,stemp1             ;    100 subtrahieren
        SUB5     mddskonst,stemp1        ;
        bra      strans0225              ;
strans0222:
        movlw    .4                      ;   6 Stellen aendern
        subwf    stemp,W                 ;
        bnz      strans0223              ;
        MOVLF    0x10,stemp1             ;   10 000 subtrahieren
        MOVLF    0x27,stemp1+1           ;
        SUB5     mddskonst,stemp1        ;
        bra     strans0225              ;                                                                  
strans0223:
        movlw   .6                      ;    4 Stellen aendern
        subwf   stemp,W                 ;
        bnz     strans0224              ;
        MOVLF   0x40,stemp1             ;    1 000 000 subtrahieren
        MOVLF   0x42,stemp1+1           ;
        MOVLF   0x0f,stemp1+2           ;
        SUB5    mddskonst,stemp1        ;
        bra     strans0225              ;
strans0224:                             ;
        movlw   .8                      ;    2 Stellen aendern
        subwf   stemp,W                 ;
        bnz     strans0225              ;
        MOVLF   0xe1,stemp1+1           ;    100 000 000 subtrahieren
        MOVLF   0xf5,stemp1+2           ;
        MOVLF   0x05,stemp1+3           ;
        SUB5    mddskonst,stemp1        ;
strans0225:
        clrf    impulse                 ;    01Impulse loeschen
        bra     strans00                        ;
strans022:                              ;
        call    Tastegedrueckt          ; ? Taste gedrueckt
        bc      strans0231              ;
        bra     strans001                       ;
strans0231:
        call    quittungston            ; 1 Quittungston ausgeben
        call    Tastaturstatus          ;    ? Welche Taste
        movlw   .1                      ;
        subwf   tastennummer,W          ;
        bnz     strans03                        ;
        incf    stemp,F                 ;    1 Taste 1
        incf    stemp,F                 ;
        movlw   .10                     ;
        subwf   stemp,W                 ;      ? min erreicht
        bnz     strans021                       ;
        clrf    stemp                   ;      1 wieder mit 0 laden
strans021:                              ;
        bra     strans98                ;
strans03:                               ;
        movlw   .2                      ;
        subwf   tastennummer,W          ;
        bnz     strans04                ;
        decf    stemp,F                 ;    2 Taste 2
        decf    stemp,F                 ;
        btfss   stemp,7                 ;      ? min erreicht Ueberlauf unter null
        bra     strans031               ;
        movlw   .8                      ;      1 wieder max einstellen
        movwf   stemp                   ;
strans031:                              ;
        bra     strans06                ;
strans04:                               ;
        movlw   .3                      ;
        subwf   tastennummer,W          ;
        bnz     strans05                ;
        bra     strans99X               ;    3 Taste 3 Cancel
strans05:                               ;
        movlw   .4                      ;
        subwf   tastennummer,W          ;
        bnz     strans06                ;
        bra     strans99ok              ;    4 Taste 4 OK
strans06:                               ;
        rcall   z1clr                   ; Zeile1 loeschen
        LCDStr tmhz                     ; MHz dahinter
strans98:                               ;
        call    Tastegedrueckt          ; SCHLEIFE(2)
        bc      strans98                ; ENDE(2) Taste nicht mehr gedrueckt
        bra     strans00                ;ENDE(3) Taste X oder OK
strans99X:
        return                          ;X
strans99ok:                             ;OK
        SUB5    mddskonst,frequenza     ;Frequenz wieder subtrahieren
        movff   mddskonst,lcdoffset     ;neuer Wert wird in
        movff   mddskonst+1,lcdoffset+1 ;LCDoffset
        movff   mddskonst+2,lcdoffset+2 ;gespeichert
        movff   mddskonst+3,lcdoffset+3 ;
        movff   mddskonst+4,lcdoffset+4 ;
strans99null:                           ;Einsprung fuer nur abspeichern
        goto    ebandwrite              ;Banddaten speichern
;...............................................................................
ddstypemax      equ     .6

sddstype:                                ;
         call    Tastegedrueckt          ;
         bc      sddstype                ;
         LCDStrp qtext6                  ;"<   >    X    ok"
         rcall   typlcd                  ;DDS-Type anzeigen
sddst00:
         call    Tastegedrueckt          ;
         bc      sddst00                 ;
sddst01:                                 ;
         call    Tastegedrueckt          ;
         bnc     sddst01                 ;
         call    quittungston            ;1 Quittungston ausgeben
         call    Tastaturstatus          ;
         movlw   .1                      ;
         subwf   tastennummer,W          ;
         bnz     sddst02                 ;
         incf    ddstype,F               ;
         movlw   ddstypemax+1            ;
         subwf   ddstype,W               ;
         bnz     sddst011                        ;
         clrf    ddstype                 ;
sddst011:                                ;
         bra     sddstype                ;
sddst02:                                 ;
         movlw   .2                      ;
         subwf   tastennummer,W          ;
         bnz     sddst03                 ;
         decf    ddstype,F               ;
         incf    ddstype,W               ;
         bnz     sddst021                        ;
         MOVLF   ddstypemax,ddstype      ;
sddst021:                                ;
         bra     sddstype                ;
sddst03:                                 ;
         movlw   .3                      ;
         subwf   tastennummer,W          ;
         bnz     sddst04                 ;
         bra     sddst06                 ;
sddst04:                                 ;
        movlw    .4                      ;
        subwf    tastennummer,W          ;
        bnz      sddst06                 ;
         MOVLF   addrddstype,data_ee_addr;
         movff   ddstype,data_ee_data    ;
         goto    ewrite                  ;
sddst06:                                 ;
        return
;...............................................................................
typlcd:
        movf     ddstype,F
        bnz      type01
        LCDStrp tlcd01
        bra      type99
type01:
        movlw    .1
        subwf    ddstype,W
        bnz      type02
        LCDStrp tlcd02
        bra      type99
type02:
        movlw    .2
        subwf    ddstype,W
        bnz      type03
        LCDStrp tlcd03
        bra      type99
type03:
        movlw    .3
        subwf    ddstype,W
        bnz      type04
        LCDStrp tlcd04
        bra      type99
type04:
        movlw    .4
        subwf    ddstype,W
        bnz      type05
        LCDStrp tlcd05
        bra      type99
type05:
          movlw     .5                                                                                  
          subwf     ddstype,W
          bnz       type06
          LCDStrp   tlcd06
          bra       type99
type06:
          movlw     .6
          subwf     ddstype,W
          bnz       type07
          LCDStrp   tlcd07
type07:
type99:
        CLR5    faktor1                 ;
        movff   ddstype,faktor1         ;
        call    hex2bcd5b               ; in BCD wandeln
        call    bcd2char6b              ; in Char wandeln
        MOVLF   .2,laenge               ; printf Ausgabe
        MOVLF   .0,komma                ;
        MOVLF   .0,kuerzen              ;
        goto    printf                  ;
;------------------------------------------------------------------------------
stxhang:
        LCDStrp qtext12                 ;"ok        cancel"
stxhang01:
        LCDStrp qtext29                 ;"TXhang= "
        call    math_ramclr             ;
        movff   stimer,faktor1          ;
        MOVLF   .5,faktor2              ;
        call    mul5b                   ;
        call    ergebnis2faktor1_5b     ;
        call    hex2bcd5b               ;
        call    bcd2char6b              ; in Char wandeln
        MOVLF   .4,laenge               ; printf Ausgabe
        MOVLF   .0,komma                ;
        MOVLF   .0,kuerzen              ;
        call    printf                  ;
        LCDStr qtext30                  ;
stxhang02:
        call    Tastegedrueckt          ;
        bnc     stxhang06               ;
        call    quittungston            ;1 Quittungston ausgeben
        call    Tastaturstatus          ;
        movlw   .4                      ;
        subwf   tastennummer,W          ;
        bnz     stxhang03               ;
        return                          ;
stxhang03:                              ;
        movlw   .1                      ;
        subwf   tastennummer,W          ;
        bz      stxhang05               ;
stxhang06:                              ;
        movf    impulse,F               ;
        bz      stxhang02               ;
        btfsc   impulse,7               ;
        bra     stxhang08               ;
        btfss   impulse,4               ;
        bra     stxhang02               ;
        incf    stimer,F                ;
        clrf    impulse                 ;
        bra     stxhang01               ;
stxhang08:                              ;
        negf    impulse                 ;
        btfss   impulse,4               ;
        bra     stxhang09               ;
        decf    stimer,F                ;
        clrf    impulse                 ;
        bra     stxhang01               ;
stxhang09:                              ;
        negf    impulse                 ;
        bra     stxhang02               ;
stxhang05:                              ;
        call    writesicherung          ;
stxhang07:                              ;
        return                          ;

;------------------------------------------------------------------------------
sspot:
        LCDStrp qtext12                 ;"ok        cancel"
sspot01:
        bsf     tonenable               ;
         call    mtausrechnen           ;
         LCDStrp qtext21                ;"SPOT= "
         CLR4    faktor1+1              ;
         movff   mithoerton,faktor1     ;
         call    hex2bcd5b              ;
         movf    ergebnis,W             ;
         call    LCDHEX                 ;
         movlw   '0'                    ;
         call    LCDChar                ;
         LCDStr qtext22                 ;
sspot02:                                ;
         call    Tastegedrueckt         ;
         bnc     sspot06                ;
         call    quittungston           ;1 Quittungston ausgeben
         call    Tastaturstatus         ;
         movlw   .4                     ;
         subwf   tastennummer,W         ;
         bnz     sspot03                ;
         return                         ;
sspot03:                                ;
         movlw   .1                     ;
         subwf   tastennummer,W         ;
         bz      sspot05                ;
sspot06:                                ;
         movf    impulse,F              ;
         bz      sspot02                ;
         btfsc   impulse,7              ;
         bra     sspot08                ;
         btfss   impulse,4              ;
         bra     sspot02                ;
         incf    mithoerton,F           ;
         clrf    impulse                ;
         bra     sspot01                ;
sspot08:                                ;
         negf    impulse                ;
         btfss   impulse,4              ;
         bra     sspot09                ;
         decf    mithoerton,F           ;
         clrf    impulse                ;
         bra     sspot01                ;
sspot09:                                ;
         negf    impulse                ;
         bra     sspot02                ;
sspot05:                                ;
        call     writesicherung         ;
sspot07:                                ;
        return                          ;
;------------------------------------------------------------------------------
testanzeige:
        LCDHEX2Z1        xsm     ;
        LCDHEX2Z2        ysm     ;
        goto     t5Sek

eichanz:
        movf    messh,W                 ;
        call    LCDHEX                  ;
        movf    messl,W                 ;
        goto    LCDHEX                  ;
;------------------------------------------------------------------------------
seichsmeter:
        LCDStrp qtext12                 ;"ok        cancel"
seichsm01:
        LCDStrp qtext13                 ;"S0 = "
        call    umesssmeter             ;Spannung messen
        rcall   eichanz                 ;
        call    t100mSek                ;
        call    Tastegedrueckt          ;
        bnc     seichsm01               ;
        call    quittungston            ;1 Quittungston ausgeben
        call    Tastaturstatus          ;
        movlw   .4                      ;
        subwf   tastennummer,W          ;
        bnz     seichsm02               ;
        return                          ;
seichsm02:
        movlw   .1                      ;
        subwf   tastennummer,W          ;
        bz      seichsm03               ;
        bra     seichsm01               ;
seichsm03:
        call    Tastegedrueckt          ;                                                                
        bc      seichsm03               ;
        movff   messl,xsm               ;
        movff   messh,xsm+1             ;
;       rcall   testanzeige             ;
seichsm04:
        LCDStrp qtext14                 ;"S9 = "
        call    umesssmeter             ;Spannung messen
        rcall   eichanz                 ;
        call    t100mSek                ;
        call    Tastegedrueckt          ;
        bnc     seichsm04               ;
        call    quittungston            ;1 Quittungston ausgeben
        call    Tastaturstatus          ;
        movlw   .4                      ;
        subwf   tastennummer,W          ;
        bnz     seichsm05               ;
        return                          ;
seichsm05:
        movlw   .1                      ;
        subwf   tastennummer,W          ;
        bz      seichsm06               ;
        bra     seichsm04               ;
seichsm06:
        call    Tastegedrueckt          ;
        bc      seichsm06               ;
        movff   messl,ysm               ;
        movff   messh,ysm+1             ;
;       rcall   testanzeige             ;
;------------------------------------------------------------------------------
berechnen_xysm:
        call    math_ramclr             ;operanten loeschen
        movff   ysm,minuend             ;messungS0 holen
        movff   ysm+1,minuend+1         ;
        movff   xsm,subtrahend          ;messungS9 holen
        movff   xsm+1,subtrahend+1      ;
        SUB5    subtrahend,minuend      ;messungS9 = messungS0 - messungS9
        nop
        CLR5    minuend                 ;Operant loeschen
        nop
        MOVLF   D'20',minuend+3         ;
        nop
        NEG5    minuend                 ;ergebnis = (-20 * 2 hoch 16)/(messS0 - messS9)
        call    div5b                   ;
        btfsc   ergebnis+4,7            ;? ist das ergebnis negativ
        bra     berechnen_xysm02        ;
        btfss   ergebnis,7              ;0 ? Aufrunden
        bra     berechnen_xysm01        ;
        INC2    ergebnis+1              ; 1 um eins aufrunden
        bra     berechnen_xysm01        ;
berechnen_xysm02:
        btfsc   ergebnis,7              ;1 ? Aufrunden
        bra     berechnen_xysm01        ;
        DEC2    ergebnis+1              ; 1 um eins aufrunden im negativ Bereich
berechnen_xysm01:
        movff   ergebnis+1,ysm          ;Ergebnis abspeichern ist gleich X-Wert
        movff   ergebnis+2,ysm+1        ;
        call    math_ramclr             ;operanten loeschen
        call    xsmladen                ;messungS9 holen in faktor2
        CLR5    faktor1                 ;op faktor1 loeschen
        movff   ysm,faktor1             ;X-Wert ist faktor1
        movff   ysm+1,faktor1+1         ;
        btfss   ysm+1,7                 ;? ist X-Wert negativ
        bra     berechnen_xysm03        ;
        comf    faktor1+2,F             ;1 die freien Stellen auch noch negieren
        comf    faktor1+3,F             ; so das der Operant 5 stellig richtig
        comf    faktor1+4,F             ; ist
berechnen_xysm03:                       ;
        call    mul5b                   ;ergebnis = X-Wert * messungS9
        btfss   ergebnis+1,7            ;? eventuell aufrunden
        bra     berechnen_xysm04        ;
        INC2    ergebnis+2              ;
berechnen_xysm04:
        movff   ysm,xsm                 ;X-Wert an die richtige Stelle kopieren
        movff   ysm+1,xsm+1             ;
        movff   ergebnis+2,ysm          ;Y-Wert an die richtige Stelle kopieren
        movff   ergebnis+3,ysm+1        ;
        NEG2    ysm                     ;Y-Wert muss negativ sein
        LCDStrp qtext15                 ;"Save?           "
berechnen_xysm05:                       ;
        call    Tastegedrueckt          ;
        bnc     berechnen_xysm05        ;
        call    Tastaturstatus          ;
        movlw   .4                      ;
        subwf   tastennummer,W          ;
        bnz     berechnen_xysm06        ;
        return                          ;
berechnen_xysm06:
        movlw   .1                      ;
        subwf   tastennummer,W          ;
        bz      berechnen_xysm07        ;
        bra     berechnen_xysm05        ;
berechnen_xysm07:                       ;
        goto    writesicherung          ;Sichern
;=========================================================================
sendeeprom:
        LCDStrp qtext16                 ;"PIC ---> PC TX "
        LCDStrp qtext12                 ;"OK        Abruch"
sendeeprom01:                           ;
        call    Tastegedrueckt          ;
        bnc     sendeeprom01            ;
        call    quittungston            ;Quittungston ausgeben
        call    Tastaturstatus          ;
        movlw   .4                      ;
        subwf   tastennummer,W          ;
        bnz     sendeeprom02            ;
        return                          ;
sendeeprom02:                           ;
        call    Tastaturstatus          ;
        movlw   .1                      ;
        subwf   tastennummer,W          ;
        bz      sendeeprom03            ;
        bra     sendeeprom              ;
sendeeprom03:                           ;
        call    LCDDisplayClear         ;
        LCDStrp qtext16                 ;"PIC ---> PC TX "
        bcf     INTCON,GIE              ;Interrups sperren
        bsf     bintsperren             ;Interrupt dauerhaft sperren
        clrf    schleife+2              ;
        clrf    data_ee_addr            ;
sendeeprom04:                           ;
        movlw   0x40                    ;
        call    LCDPos                  ;
        movf    data_ee_addr,W          ;
        call    LCDHEX                  ;
        call    eread                   ;
        call    rs232tx                 ;
        call    t10mSek         ;
        incf    data_ee_addr,F          ;
        decfsz schleife+2,F             ;
        bra     sendeeprom04            ;
        LCDStrp qtext17                 ;"finished!       "
        call    t5Sek                   ;
sendeeprom05:                           ;
        call    Tastegedrueckt          ;
        bc      sendeeprom05            ;
        return                          ;
;=========================================================================
ee2flash:
        LCDStrp qtext23                 ;"Eeprom --> Flash"
        LCDStrp qtext12                 ;"OK        Abruch"
ee2flash01:                             ;
        call    Tastegedrueckt          ;
        bnc     ee2flash01              ;
        call    quittungston            ;Quittungston ausgeben
        call    Tastaturstatus          ;
        movlw   .4                      ;
        subwf   tastennummer,W          ;
        bnz     ee2flash02              ;
        return                          ;
ee2flash02:                             ;
;........................................
        call    Tastaturstatus          ;
        movlw   .1                      ;
        subwf   tastennummer,W          ;
        bz      ee2flash03              ;
        bra     ee2flash                ;
;........................................
ee2flash03:                             ;
        call    Tastegedrueckt          ;
        bc      ee2flash03              ;                                                                
;........................................
        LCDStrp qtext24                 ;"Speicherplatz    "
        LCDStrp qtext25                 ;"1    2     3   4"
ee2flash04:                             ;
        call    Tastegedrueckt          ;
        bnc     ee2flash04              ;
;........................................
        call    quittungston            ;Quittungston ausgeben
        call    Tastaturstatus          ;
        movlw   .1                      ;
        subwf   tastennummer,W          ;
        bz      ee2flash05              ;
        call    Tastaturstatus          ;
        movlw   .2                      ;
        subwf   tastennummer,W          ;
        bz      ee2flash06              ;
        call    Tastaturstatus          ;
        movlw   .3                      ;
        subwf   tastennummer,W          ;
        bz      ee2flash07              ;
        call    Tastaturstatus          ;
        movlw   .4                      ;
        subwf   tastennummer,W          ;
;........................................
        POINT   0x7e00                  ;
        bra     ee2flash08              ;
;........................................
ee2flash05:
        POINT   0x7b00                  ;
        bra     ee2flash08              ;
;........................................
ee2flash06:
        POINT   0x7c00                  ;
        bra     ee2flash08              ;
;........................................
ee2flash07:
        POINT   0x7d00                  ;
;........................................
ee2flash08:
        bcf     INTCON,GIE              ;Interrups sperren
        bsf     bintsperren             ;Interrupt dauerhaft sperren
        clrf    schleife+2              ;256 Byte
        clrf    data_ee_addr            ;bei Adresse 0 im Eeprom beginnen
        lfsr    FSR0,flbuffer           ;zuerst in Ram speichern
ee2flash09:                             ;SCHLEIFE(1)
        call    eread                   ; eeprom lesen
        movwf   POSTINC0                ; in Ram ablegen
        incf    data_ee_addr,F          ; adr+1
        decfsz schleife+2,F             ;
        bra     ee2flash09              ;ENDE(1) nach 256 Byte
;........................................
        lfsr    FSR0,flbuffer           ;
ee2flash10:                             ;
        rcall   flashwrite              ;
;........................................
        LCDStrp qtext17                 ;"finished!       "
        call    t1Sek                   ;
ee2flash99:                             ;
        call    Tastegedrueckt          ;
        bc      ee2flash99              ;
        return                          ;
;-------------------------------------------------------------------------
flashwrite:                             ;
        MOVLF   .32,PRODL               ;
flwrite02:                              ;
        movlw   b'11111000'             ;
        andwf   TBLPTRL,F               ;
        movlw   .8                      ;
flwrite01:                              ;SCHLEIFE(1)
        movff   POSTINC0,TABLAT         ;    Ram -> Wreg ,Adr + 1
        TBLWT*                          ;    TABLAT -> Zwischenspeicher, Adr + 1
        TBLRD*+                         ;
        decfsz WREG,F                   ;
        bra     flwrite01               ;ENDE(1) PRODH == 0
        TBLRD*-                         ;zurueck in den Block
        movlw   b'10000100'             ;
        movwf   EECON1                  ;
        movlw   0x55                    ;
        movwf   EECON2                  ;
        movlw   0xaa                    ;
        movwf   EECON2                  ;
        bsf     EECON1,WR               ;Schreibstart
        nop                             ;
        TBLRD*+                         ;
        decfsz PRODL,F                  ;
        bra     flwrite02               ;
        return                          ;
;=========================================================================
flash2ee:                               ;
        bsf     a_licht                 ;
        LCDStrp qtext27                 ;"Flash --> Eeprom"
        LCDStrp qtext12                 ;"OK        Abruch"
flash2ee001:                            ;
        call    Tastegedrueckt          ;
        bc      flash2ee001             ;
flash2ee01:                             ;
        call    Tastegedrueckt          ;
        bnc     flash2ee01              ;
        call    quittungston            ;Quittungston ausgeben
        call    Tastaturstatus          ;
        movlw   .4                      ;
        subwf   tastennummer,W          ;
        bnz     flash2ee02              ;
        return                          ;
flash2ee02:                             ;
;........................................
        call    Tastaturstatus          ;
        movlw   .1                      ;
        subwf   tastennummer,W          ;
        bz      flash2ee03              ;
        bra     flash2ee                ;
;........................................
flash2ee03:                             ;
        call    Tastegedrueckt          ;
        bc      flash2ee03              ;
;........................................
        LCDStrp qtext24                 ;"Speicherplatz   "
        LCDStrp qtext25                 ;"1    2    3    4"
flash2ee04:                             ;
        call    Tastegedrueckt          ;
        bnc     flash2ee04              ;
;........................................
        call    quittungston            ;Quittungston ausgeben
        call    Tastaturstatus          ;
        movlw   .1                      ;
        subwf   tastennummer,W          ;
        bz      flash2ee05              ;
        call    Tastaturstatus          ;
        movlw   .2                      ;
        subwf   tastennummer,W          ;
        bz      flash2ee06              ;
        call    Tastaturstatus          ;
        movlw   .3                      ;
        subwf   tastennummer,W          ;
        bz      flash2ee07              ;
        call    Tastaturstatus          ;
        movlw   .4                      ;
        subwf   tastennummer,W          ;
;........................................
        POINT   0x7e00                  ;
        bra     flash2ee08              ;
;........................................
flash2ee05:                             ;
        POINT   0x7b00                  ;
        bra     flash2ee08              ;
;........................................
flash2ee06:                             ;
        POINT   0x7c00                  ;
        bra     flash2ee08              ;
;........................................
flash2ee07:                             ;
        POINT   0x7d00                  ;
;........................................
flash2ee08:                             ;
        call    urladungerweitert       ;
;........................................
        LCDStrp qtext17                 ;"finished!       "
        call    t1Sek                   ;
flash2ee99:                             ;
        call    Tastegedrueckt          ;                                                                 
        bc      flash2ee99              ;
        return                          ;
;=========================================================================
leseeprom:
        LCDStrp qtext18                 ;"PC ---> PIC RX "
        LCDStrp qtext12                 ;"OK        Abruch"
leseeprom01:                            ;
        call    Tastegedrueckt          ;
        bnc     leseeprom01             ;
        call    quittungston            ;Quittungston ausgeben
        call    Tastaturstatus          ;
        movlw   .4                      ;
        subwf   tastennummer,W          ;
        bnz     leseeprom02             ;
        return                          ;
leseeprom02:                            ;
        call    Tastaturstatus          ;
        movlw   .1                      ;
        subwf   tastennummer,W          ;
        bz      leseeprom03             ;
        bra     leseeprom               ;
leseeprom03:                            ;
        call    LCDDisplayClear         ;
        LCDStrp qtext18                 ;"PC ---> PIC RX "
        bcf     INTCON,GIE              ;Interrups sperren
        bsf     bintsperren             ;Interrupt dauerhaft sperren
        clrf    schleife+3              ;
        clrf    data_ee_addr            ;
leseeprom04:                            ;
        movlw   0x40                    ;
        call    LCDPos                  ;
        movf    data_ee_addr,W          ;
        call    LCDHEX                  ;
        call    LCDSpace
        call    rs232rx                 ;
        movwf   data_ee_data            ;
        call    LCDHEX                  ;
        call    ewrite                  ;
        incf    data_ee_addr,F          ;
        decfsz schleife+3,F             ;
        bra     leseeprom04             ;
        LCDStrp qtext17                 ;"finished!       "
        call    t5Sek                   ;
leseeprom05:                            ;
        call    Tastegedrueckt          ;
        bc      leseeprom05             ;
        return
;=========================================================================
;Initialisieren der UART Port RC6 bis RC7
;=========================================================================
baud9600:       equ     (((pictakt*d'1000')/d'9600')/d'64')-1

rs232init:
        movlw   baud9600        ;Setze Baudrate
        movwf   SPBRG
        bcf     TXSTA,BRGH      ;keine baud rate high speed option
        bsf     TXSTA,TXEN      ;enable transmission
        bsf     RCSTA,CREN      ;enable reception
        bsf     RCSTA,SPEN      ;enable serial port
        return
;==============================================================================
;Funktion       Auslesen des RS232 Empfangsbuffers und auswerten der Zeichen
;Eingang.       Auslesen des Empfangsbuffer Pointer in FSR1 bis Empfangsbuffer
;               keine Zeichen mehr enthaelt
;Ausgang
;------------------------------------------------------------------------------
rxbuffer:                               ;
        movff   FSRL_rs232_read, FSR1L ;Pointer laden
        movff   FSRH_rs232_read, FSR1H ;
rxbuffer02:                             ;
        movf    FSRL_rs232_write,W      ;? Pointer Schreiben von RS232 in Buffer
        subwf   FSR1L,W                 ; == Pointer Lesen aus Buffer
        bnz     rxbuffer01              ;
        clrc                            ; kein Zeichen C loeschen
        return                          ; Funktion wieder verlassen keine Zeichen
                                        ;
rxbuffer01:                             ;
        bcf     bnofunktion             ;0 Zeitkritische Funktion startet
        movf    POSTINC1,W              ; Lesen des Zeichens aus dem Buffer
        movwf   CAT_in_byte             ; Zeichen zwischenspeichern
        movlw   LOW end_CAT_buffer+1    ; ? Test ob Pufferende erreicht
        subwf   FSR1L,W                 ;
        bnz     read_ptr_in_range       ;
        lfsr    1,start_CAT_buffer      ; 1 Wieder an den Anfang des Ringbuffers
read_ptr_in_range:
        movff   FSR1L, FSRL_rs232_read ; Pointer merken
        movff   FSR1H, FSRH_rs232_read ;
        movf    CAT_in_byte,W           ; Byte holen in WREG
        bsf     STATUS,C                ; Byte vorhanden C setzen
        return                          ;
;==============================================================================
swrmessen:                              ;SWR messen und darstellen
        call    Tastegedrueckt          ;SCHLEIFE(1)
        bc      swrmessen               ;ENDE(1) keine Taste
        call    bin2ddstx               ;Sendefrequenz einstellen
        bsf     a_senderein             ;Sender einschalten
        bsf     a_tastungein            ;Sender tasten
;       bsf     tonenable               ;Mithoerton ein
        call    LCDDisplayClear         ;Display loeschen
swrm02:                                 ;SCHLEIFE(2)
        call    math_ramclr             ;
        MOVLF   .8,schleife+1           ;
        CLR2    uvor                    ; Loeschen der Vor und Rueck
        CLR2    urueck                  ; Spannungen
swrm01:                                 ; SCHLEIFE(1)
        call    umessvor                ;    Uvorwaertz messen
        ADD2    uvor,mess               ;    Uvor aufaddieren
        call    umessrueck              ;    Urueckwaertz messen
        ADD2    urueck,mess             ;    Urueck aufaddieren
        decfsz schleife+1,F             ; ENDE(1) 8 Durchlaeufe
        bra     swrm01                  ;
        CMP2    urueck,uvor             ; ? Urueck > Uvor
        bnc     swrm05                  ;
        LDK2    faktor1,HIGH .9999,LOW .9999;
        bra     swrm06                  ;
swrm05:
        LD2     faktor1,uvor            ; Uges = Uvor + Urueck
        CLR3    faktor1+2               ;
        ADD2    faktor1,urueck          ;
        LDK2    faktor2,0,D'100'        ;
        CLR5    faktor2+2               ;
        call    mul5b                   ; Uges = Uges * 100
        LD5     faktor1,ergebnis        ;
        LD2     swr,ergebnis            ;
        LD2     faktor2,uvor            ;
        CLR3    faktor2+2               ;
        SUB2    faktor2,urueck          ; Uges1 = Uvor - Urueck
        call    div5b                   ; SWR = Uges / Uges1
        LD5     faktor1,ergebnis        ;
        LD2     swrmerke,ergebnis       ; SWR merken
swrm06:
        call    hex2bcd5b               ; HEX in BCD wandeln
        call    bcd2char6b              ; BCD in Char wandeln
        MOVLF   .4,laenge               ; 3 stellige Anzeige
        MOVLF   .2,komma                ; 2 Stelle hintern Komma
        clrf    kuerzen                 ; von rechts nichts kuerzen
        LCDStrp stext18                 ; "SWR="
        call    printf                  ; Formatiert ausgeben
        call    t10mSek                 ; einen Moment warten
        LD2     faktor1,swrmerke        ; SWR = SWR / 10
        CLR3    faktor1+2               ;
        MOVLF   .10,faktor2             ;
        CLR4    faktor2+1               ;
        call    div5b                   ;
        LDK2    faktor2,0,.10           ; SWR = SWR - 10
        SUB2    ergebnis,faktor2        ;
        LD2     swrmerke,ergebnis       ;
        LD2     faktor1,ergebnis        ;
        CLR3    faktor1+2               ;
        CLR4    faktor2+1               ;
        MOVLF   .3,faktor2              ; b1 = SWR / 3
        call    div5b                   ;
        movlw   .16                     ; ? b1 > 16
        subwf   ergebnis,W              ;
        bnc     swrm07                  ;
        MOVLF   .16,bargraph            ; 1 b1 = 16
        CLR2    bargraph+1              ;    b2 = 0
        bra     swrm08                  ;    b3 = 0
swrm07:                                 ;
        movff    ergebnis,bargraph      ; 0 b3 = 16 - b1                                                 
        movff    faktor1,bargraph+1     ;
        MOVLF    .16,faktor1            ;
        movf     bargraph,W             ;
        subwf    faktor1,W              ;
        movwf    bargraph+2             ;
        tstfsz   bargraph+1             ;
        decf     bargraph+2,F           ;
swrm08:                                 ;
        movlw   0x40                    ; Anfang 2. Zeile
        call    LCDPos                  ;
        tstfsz bargraph                 ; ? b1 != 0
        call    s3                      ; 1 Alle ||| ausgeben
        tstfsz bargraph+1               ; ? b2 != 0
        call    s12                     ; 1 | oder || ausgeben
        tstfsz bargraph+2               ; ? b3 != 0
        call    sleer                   ; 1 mit Leerzeichen auffuellen
        call    Tastegedrueckt          ; ? Taste gedrueckt
        bc      swrm03                  ;
        btfsc   e_strichpin             ; ? Keyer Strich gedrueckt
        bra     swrm02                  ;ENDE(2) keine Taste gedrueckt + Keyerstrich
swrm03:                                 ;
        call    quittungston            ;Quittungston ausgeben
        bcf     tonenable               ;Mithoerton ein
        bcf     a_tastungein            ;Sender tasten
        bcf     a_senderein             ;Sender einschalten
        call    bin2ddsrx               ;Empfangsfrequenz einstellen
swrm04:                                 ;SCHLEIFE(1)
        btfss   e_strichpin             ; ? Strichpin gedrueckt
        bra     swrm04                  ;
        call    Tastegedrueckt          ; ? Taste gedrueckt
        bc      swrm04                  ;ENDE(1) keine Taste + keine Strichpin
        call    t100mSek                ;Wartezeit Entprellung
        return                          ;
;..............................................................................
s3:                                     ;
        movff   bargraph,schleife       ;
s301:                                   ;SCHLEIFE(1)
        movlw   6                       ; Zeichen |||
        call    LCDChar                 ; ausgeben
        decfsz schleife,F               ;ENDE(1) Schleife zuende
        bra     s301                    ;
        return                          ;
;..............................................................................
s12:                                    ;
        movlw   3                       ;3 addieren
        addwf   bargraph+1,F            ;
        movf    bargraph+1,W            ;
        goto    LCDChar                 ;und | oder || ausgeben
;..............................................................................
sleer:                                  ;
        movf    bargraph+2,W            ;mit Leerzeichen
        goto    LCDSpacel               ;auffuellen
;..............................................................................
bandrs232:
        btfss   bbandmode               ;? Band/Mode-umschaltung aktiv deaktiviert
        return                          ;1 ---> break beenden
        movlw   0x80                    ;Kennung fuer Bandumschaltung
        call    rs232tx                 ;auf RS232 ausgeben
        movf    band,W                  ;Bandnummer
        goto    rs232tx                 ;auf RS232 ausgeben
;..............................................................................
moders232:
        btfss   bbandmode               ;? Band/Mode-umschaltung aktiv deaktiviert
        return                          ;1 ---> break beenden
        movlw   0x81                    ;Kennung fuer Modeumschaltung
        call    rs232tx                 ;auf RS232 ausgeben
        movf    mode,W                  ;Mode vfoA
        goto    rs232tx                 ;auf RS232 ausgeben
;##############################################################################

;;;;;;; Interruptbehandlung Hohe Prioritaet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

interrupt1:
        movwf    W_TEMP                ;W_TEMP is in virtual bank
        movff    STATUS, STATUS_TEMP   ;STATUS_TEMP located anywhere
        movff    BSR, BSR_TEMP         ;BSR located anywhere ;
        movff    FSR0H, FSR0H_TEMP     ;FSR0 sichern
        movff    FSR0L, FSR0L_TEMP     ;

        movff    FSRH_rs232_write, FSR0H ;Pointer fuer Buffer in
        movff    FSRL_rs232_write, FSR0L ;FSR0 laden
get_RS232:
        btfss    PIR1,RCIF              ;?   liegen Bytes an RS232 an
        bra      end_ISR_rs232          ;
        movf     RCREG, W               ;1   Byte holen
                                        ;
        btfss   RCSTA, OERR             ;    ? error
        bra     checkinrange            ;      -> alles ok
        movf    RCREG, W                ;    1 Byte holen
        bcf     RCSTA, CREN             ;      und OERR loeschen
        bsf     RCSTA, CREN             ;
        bra     get_RS232               ;
checkinrange:                           ;
        movwf   POSTINC0                ;    und in den Buffer speichern
        movlw   LOW end_CAT_buffer+1    ;
        subwf   FSR0L,W                 ;    test above top limit
        bnz     write_ptr_in_range      ;
        lfsr    0,start_CAT_buffer      ;
write_ptr_in_range:                     ;
        bra     get_RS232               ;
end_ISR_rs232:                          ;
        movff   FSR0H, FSRH_rs232_write ;    Pointer merken
        movff   FSR0L, FSRL_rs232_write ;

         btfss   INTCON,T0IF            ;Timer 0 (Tonausgabe)
         bra     ir1                    ;
         bcf     INTCON,T0IF            ;Timer0 Bit loeschen
         movff   tmr0const,TMR0L        ;
         btfsc   tonenable              ;? Ton eingeschaltet
         btg     a_mton                 ;1 Ausgangsbit wechseln
         btfss   tonenable              ;? Ton eingeschaltet
         bcf     a_mton                 ;0 Tonbit bei aus auf Low
ir1:
         btfss   INTCON,INT0IF          ;?   RB0 Interrupt durch Drehgeber
         bra     ir2                    ;
         btfss   INTCON2,INTEDG0        ;1   ? Impulsflanke
         bra     ir2_3                  ;
         btfsc   e_DrehgeberD           ;    1 ? Drehrichtung links herum
         bra     ir2_1                  ;
         decf    impulse,F              ;      1 Impulse =-1
         bra     ir2_2                  ;
ir2_1:                                  ;
         incf    impulse,F              ;      0 Impulse =+1
         bra     ir2_2                  ;
ir2_3:                                  ;
         btfss   e_DrehgeberD           ;    0 ? Drehrichtung links herum
         bra     ir2_4                  ;
         decf    impulse,F              ;      1 Impulse =-1
         bra     ir2_2                  ;
ir2_4:                                  ;
         incf    impulse,F              ;      0 Impulse =+1
ir2_2:
         btg     INTCON2,INTEDG0        ;    Impulsflanke wechseln
         bcf     INTCON,INT0IF          ;    Bit wieder bereit
ir2:                                    ;
         btfss   PIR1,TMR1IF            ;?   timer1 (verschiedene Timer)
         bra     ir3                    ;
         bcf     PIR1,TMR1IF            ;
         movf    zs1,F                  ;1   ? zs1 > 0
         bz      ir3_1                  ;
         decf    zs1,F                  ;    1 zs1 =-1
ir3_1:
         movf    zs2,F                  ;1 ? > 0
         bz      ir3_2                  ;
         decf    zs2,F                  ; 1 zs2 =-1
ir3_2:                                  ;
         MOVLF   tmr1lconst,TMR1L       ; timer1 neu laden
         MOVLF   tmr1hconst,TMR1H       ;
ir3:                                    ;
         btfsc   PIR2,EEIF              ;Eeprom write interrupt
         bcf     PIR2,EEIF              ;

         btfss   PIR1,TMR2IF            ;?   Timer 2 Interrupt (Punktlaenge)
         bra     ir4                    ;
         bcf     PIR1,TMR2IF            ;1   Bit loeschen
         movf    zs3,F                  ;    ? zs3 > 0
         bz      ir4                    ;
         decf    zs3,F                  ;    1 zs3 =-1
ir4:
          movff    FSR0H_TEMP, FSR0H         ;Restore     FSR0                                          
          movff    FSR0L_TEMP, FSR0L         ;
          movff    BSR_TEMP,BSR              ;Restore     BSR
          movf     W_TEMP,W                  ;Restore     WREG
          movff    STATUS_TEMP,STATUS        ;Restore     STATUS
          retfie

;;;;;;; Interruptbehandlung niedrige Prioritaet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

interrupt2:
        movwf      W_TEMP                   ; W_TEMP is in virtual bank
        movff      STATUS, STATUS_TEMP      ; STATUS_TEMP located anywhere
        movff      BSR, BSR_TEMP            ; BSR located anywhere ;

        movff   BSR_TEMP, BSR           ; Restore BSR
        movf    W_TEMP, W               ; Restore WREG
        movff   STATUS_TEMP,STATUS      ; Restore STATUS
        retfie
;##############################################################################
;##############################################################################
;##############################################################################

text1:             DB      0,    "V1.08 09.11.2009",0

          if       bytevariante == .1;
text2:             DB      40H,"(c)DL4JAL Va:1      ",0
          endif
          if       bytevariante == .2;
text2:             DB      40H,"(c)DL4JAL Va:2      ",0
          endif
          if       bytevariante == .3;
text2:             DB      40H,"(c)DL4JAL Va:3      ",0
          endif
          if       bytevariante == .21;
text2:             DB      40H,"(c)DL4JAL Va.21 ",0
          endif
          if       bytevariante == .22;
text2:             DB      40H,"(c)DL4JAL Va.22 ",0
          endif
          if       bytevariante == .23;
text2:             DB      40H,"(c)DL4JAL Va.23 ",0
        endif
;...............................................
        if      (bytevariante == .1)||(bytevariante == .21);englisch

tlcd01:            DB      0x0   ,"AD9833/AD9834   ",0
tlcd02:            DB      0x0   ,"AD9850/51       ",0
tlcd03:            DB      0x0   ,"AD9851 PLL*6    ",0
tlcd04:            DB      0x0   ,"AD9951 PLL*4    ",0
tlcd05:            DB      0x0   ,"AD9951 PLL*5    ",0
tlcd06:            DB      0x0   ,"AD9951 PLL*20   ",0
tlcd07:            DB      0x0   ,"AD9951 no PLL   ",0

stext:             DB      0x40,"SET           X ",0
stext1:            DB           "01 S-Meter     ",0
stext2:            DB           "02 Light auto ",0
stext3:            DB           "03 Light perm. ",0
stext4:            DB           "04 Frq 1Hz     ",0
stext5:            DB           "05 Keyer       ",0
stext6:            DB           "06 TX ZF perm. ",0
stext7:            DB           "07 VFO=RXf-ZF ",0
stext7_1:          DB           "08 Band/Mode   ",0
stext7_2:          DB           "09 VFOx4 I/Qmix",0
stext8:            DB           "10 DDS-Const    ",0
stext9:            DB           "11 ZF CW        ",0
stext9_1           DB           "12 ZF CW Revers ",0
stext10:           DB           "13 ZF LSB       ",0
stext11:           DB           "14 ZF USB       ",0
stext12:           DB           "15 ZF DIG       ",0
stext12_1:         DB           "16 ZF DIG Revers",0
stext13:           DB           "17 DDS Type   ",0
stext14:           DB           "18 LCDshift + RX",0
stext15:           DB           "19 Cal. S-Meter",0
stext15_1:         DB           "20 Spot adjust ",0
stext15_2:         DB           "21 TX hang time ",0
stext16:           DB           "22 Eepr. PIC->PC",0
stext17:           DB           "23 Eepr. PC->PIC",0
stext17_1:         DB           "24 Eepr -> Flash",0
stext17_2:         DB           "25 Flash -> Eepr",0
stext18:           DB      0x00,"SWR=",0

qtext1:           DB      0x40,"X   X     on off",0
qtext2:           DB      0x0 ,"Lightautomatic ",0
qtext3:           DB      0x0 ,"Light             ",0
qtext4:           DB      0x0 ,"S-Meter           ",0
qtext5:           DB      0x0 ,"1 Hz Resolution ",0
qtext6:           DB      0x40,"<   >     X     ok",0
qtext7:           DB      0x0 ,"Keyer             ",0
qtext8:           DB      0x0 ,"TX ZF permanent ",0
qtext9:           DB      0x0 ,"Displayshift=0 ",0
qtext10:          DB      0x40,"ok            next",0
qtext11:          DB      0x0 ,"VFO = RXfrq - ZF",0
qtext12:          DB      0x40,"ok          cancel",0
qtext13:          DB      0x0 ,"S0 = ",0
qtext14:          DB      0x0 ,"S9 = ",0
qtext15:          DB      0x0 ,"Save?             ",0
qtext16:          DB      0x0 ,"PIC ---> PC TX ",0
qtext17:          DB      0x40,"finished!         ",0
qtext18:          DB      0x0 ,"PC ---> PIC RX ",0
qtext19:          DB      0x0 ,"Band/Mode aktiv ",0
qtext20           DB      0x0 ,"VFOx4 I/Q Mixer ",0
qtext21           DB      0x0 ,"SPOT= ",0
qtext22           DB           " Hz",0
qtext23:          DB      0x0 ,"Eeprom --> Flash",0
qtext24:          DB      0x0 ,"Memory space      ",0
qtext25:          DB      0x40,"1     2     3    4",0
qtext26:          DB      0x40,"                  ",0
qtext27:          DB      0x0 ,"Flash --> Eeprom",0
qtext28:          DB      0x0 ,"Eeprom refresh ",0
qtext29           DB      0x0 ,"TXhang= ",0
qtext30           DB           " mS",0

tmode0:           DB      .13,"MHz",0
tmode1:           DB      .13,"CW ",0
tmode2:           DB      .13,"CWr",0
tmode3:           DB      .13,"LSB",0
tmode4:           DB      .13,"USB",0
tmode5:           DB      .13,"DIG",0
tmode6:           DB      .13,"DIr",0

text3:            DB      40H,"change frequenz",0
text4:            DB      .8 ," Volt",0
text5:            DB      40H,"WpM: ",0
text6:            DB      0 ,"Daten to Puffer ",0

text1po           DB      0,    "basic setting     ",0
text2po           DB      0,    "finish            ",0

;                         Taste   2     3   4
menu1:            DB      40H,"1 A/B STEP RIT      ",0
menu2:            DB      40H,"2 A=B Spot 100k     ",0
menu3:            DB      40H,"3 B=A Mem Lock      ",0
menu4:            DB      40H,"4 SWR SET Ubat      ",0
menu5:            DB      40H,"5 CW LSB USB        ",0
menu6:            DB      40H,"6 CWr DIG DIr       ",0
menu7:            DB      40H,"7 Mess 160m 80m     ",0
menu8:            DB      40H,"8    60m 40m 30m    ",0
menu9:            DB      40H,"9    20m 17m 15m    ",0
menu10:           DB      40H,"10 12m 10m 6m       ",0
menu11:           DB      40H,"11 2m tr1 tr2       ",0
menu12:           DB      40H,"12 tr3 tr4 tr5      ",0

          endif

        if        (bytevariante == .2)||(bytevariante ==.3)||(bytevariante ==.22)||(bytevariante ==.23);deutsch

tlcd01:           DB      0x0   ,"AD9833/AD9834   ",0
tlcd02:           DB      0x0   ,"AD9850/51       ",0
tlcd03:           DB      0x0   ,"AD9851 PLL*6    ",0
tlcd04:           DB      0x0   ,"AD9951 PLL*4    ",0
tlcd05:           DB      0x0   ,"AD9951 PLL*5    ",0
tlcd06:           DB      0x0   ,"AD9951 PLL*20   ",0
tlcd07:           DB      0x0   ,"AD9951 o. PLL   ",0

stext:            DB      0x40,"SET           X ",0
stext1:           DB           "01 S-Meter     ",0
stext2:           DB           "02 Licht auto ",0
stext3:           DB           "03 Licht perm. ",0
stext4:           DB           "04 Frq 1Hz     ",0
stext5:           DB           "05   Keyer       ",0                                   
stext6:           DB           "06   TX ZF perm. ",0
stext7:           DB           "07   VFO=RX-ZF   ",0
stext7_1:         DB           "08   Band/Mode   ",0
stext7_2:         DB           "09   VFOx4 I/Qmix",0
stext8:           DB           "10   DDS-Const    ",0
stext9:           DB           "11   ZF CW        ",0
stext9_1          DB           "12   ZF CW Revers ",0
stext10:          DB           "13   ZF LSB       ",0
stext11:          DB           "14   ZF USB       ",0
stext12:          DB           "15   ZF DIG       ",0
        if        (bytevariante ==   .2)||(bytevariante ==.22)
stext12_1:        DB           "16   ZF DIG Revers",0
        endif
        if        (bytevariante == .3)||(bytevariante ==.23)
stext12_1:        DB           "16 ZF FM        ",0
        endif
stext13:          DB           "17 DDS Type   ",0
stext14:          DB           "18 LCDshift + RX",0
stext15:          DB           "19 Kal. S-Meter",0
stext15_1:        DB           "20 Mithoerton   ",0
stext15_2:        DB           "21 TX hang time ",0
stext16:          DB           "22 Eepr. PIC->PC",0
stext17:          DB           "23 Eepr. PC->PIC",0
stext17_1:        DB           "24 Eepr -> Flash",0
stext17_2:        DB           "25 Flash -> Eepr",0
stext18:          DB      0x00,"SWV=",0

qtext1:           DB      0x40,"X   X    on off",0
qtext2:           DB      0x0 ,"Lichtautomatik ",0
qtext3:           DB      0x0 ,"Licht            ",0
qtext4:           DB      0x0 ,"S-Meter          ",0
qtext5:           DB      0x0 ,"1 Hz Aufloesung ",0
qtext6:           DB      0x40,"<   >    X     ok",0
qtext7:           DB      0x0 ,"Keyer            ",0
qtext8:           DB      0x0 ,"TX ZF permanent ",0
qtext9:           DB      0x0 ,"Displayshift=0 ",0
qtext10:          DB      0x40,"ja         weiter",0
qtext11:          DB      0x0 ,"VFO = RXfrq - ZF",0
qtext12:          DB      0x40,"OK         Abruch",0
qtext13:          DB      0x0 ,"S0 = ",0
qtext14:          DB      0x0 ,"S9 = ",0
qtext15:          DB      0x0 ,"Speichern?       ",0
qtext16:          DB      0x0 ,"PIC ---> PC TX ",0
qtext17:          DB      0x40,"Fertig!          ",0
qtext18:          DB      0x0 ,"PC ---> PIC RX ",0
qtext19:          DB      0x0 ,"Band/Mode aktiv ",0
qtext20           DB      0x0 ,"VFOx4 I/Q Mixer ",0
qtext21           DB      0x0 ,"MT= ",0
qtext22           DB           " Hz",0
qtext23:          DB      0x0 ,"Eeprom --> Flash",0
qtext24:          DB      0x0 ,"Speicherplatz    ",0
qtext25:          DB      0x40,"1     2    3    4",0
qtext26:          DB      0x40,"                 ",0
qtext27:          DB      0x0 ,"Flash --> Eeprom",0
qtext28:          DB      0x0 ,"Eepr.auffrischen",0
qtext29           DB      0x0 ,"TXhang= ",0
qtext30           DB           " mS",0

tmode0:           DB      .13,"MHz",0
tmode1:           DB      .13,"CW ",0
tmode2:           DB      .13,"CWr",0
tmode3:           DB      .13,"LSB",0
tmode4:           DB      .13,"USB",0
tmode5:           DB      .13,"DIG",0
          if      (bytevariante == .2)||(bytevariante ==.22)
tmode6:           DB      .13,"DIr",0
          endif
          if      (bytevariante == .3)||(bytevariante ==.23)
tmode6:           DB      .13,"FM ",0
          endif

text3:            DB      40H,"Bandwechsel    ",0
text4:            DB      .8 ," Volt",0
text5:            DB      40H,"WpM: ",0
text6:            DB      0 ,"Daten im Buffer ",0

text1po           DB      0,   "Urladung        ",0
text2po           DB      0,   "Urladung beendet",0

;                         Taste   2    3   4
menu1:            DB      40H,"1 A/B STEP RIT ",0
menu2:            DB      40H,"2 A=B Mton 100k ",0
menu3:            DB      40H,"3 B=A Mem Lock ",0
menu4:            DB      40H,"4 SWR SET Ubat ",0
menu5:            DB      40H,"5 CW LSB USB ",0
          if      (bytevariante == .2)||(bytevariante ==.22)
menu6:            DB      40H,"6 CWr DIG DIr ",0
          endif
          if      (bytevariante == .3)||(bytevariante ==.23)
menu6:            DB      40H,"6 CWr DIG FM ",0
          endif
menu7:            DB      40H,"7 Mess 160m   80m   ",0
menu8:            DB      40H,"8   60m 40m   30m   ",0
menu9:            DB      40H,"9   20m 17m   15m   ",0
menu10:           DB      40H,"10 12m 10m     6m   ",0
menu11:           DB      40H,"11 2m tr1     tr2   ",0
menu12:           DB      40H,"12 tr3 tr4    tr5   ",0

          endif

;##############################################################################

                ORG       0x7F00
mgrunddatenanf:
;mddskonst_mstep:db      0c4h,01dh,0f3h,02ah,0x0,1       ;DDSTakt=100MHz Step=10Hz
;mddskonst_mstep:db      0a2h,07dh,08eh,0dch,0x17,1      ;DDSTakt=180MHz Step=10Hz
;mddskonst_mstep:db      079h,0c0h,04fh,02fh,0xb,1       ;DDSTakt=24MHz Step=10Hz
mddskonst_mstep:db       017h,071h,0c7h,0bch,0x0a,1      ;DDSTakt=400MHz Step=10Hz
mband_mflag:    db       .2,b'01010011'                  ;Startband, Flag
mdsstype_mst    db       .6,.1                           ;DDS-Type, SenderEinTimer 10mSek
mzfcw:          db       0x20,0xa1,0x07,0                ;ZF CW
mzfcwr:         db       0x20,0xa1,0x07,0                ;ZF CWr
mzflsb:         db       0x40,0x42,0x0f,0                ;ZF LSB
mzfusb:         db       0x60,0xe3,0x16,0                ;ZF USB
mzfdig:         db       0x80,0x84,0x1e,0                ;ZF DIG
mzfdigr         db       0x80,0x84,0x1e,0                ;ZF DIGr
mxsm:           db       0x93,0x07                       ;1939
mysm            db       0,0                             ;0
mt              db       .68,0                           ;Mithoerton, 1 Fuellbyte
mmess:          db       0x80,0x96,0x98,0                ;10 MHz
                db       0x80,0x96,0x98,0                ;10 MHz
                db       0x00,0x00                       ;modeA mess modeB mess
m160m:          db       0x50,0x9e,0x1b,0                ;1,810 MHz
                db       0x90,0x3a,0x1c,0                ;1,850 MHz
                db       0x01,0x03                       ;modeA CW modeB LSB
m80m:           db       0x40,0x52,0x36,0                ;3,560 MHz
                db       0x20,0x75,0x38,0                ;3,700 MHz
                db       0x01,0x03                       ;modeA CW modeB LSB
m60m:           db       0x20,0x5c,0x51,0                ;5,332 MHz
                db       0x20,0x5c,0x51,0                ;5,332 MHz
                db       0x01,0x03                       ;modeA CW modeB LSB
m40m:           db       0xf0,0x44,0x6b,0                ;7,03 MHz
                db       0x60,0x56,0x6c,0                ;7,1 MHz
                db       0x01,0x03                       ;modeA CW modeB LSB
m30m:           db       0x20,0x1d,0x9a,0                ;10,1 MHz
                db       0x20,0x1d,0x9a,0                ;10,1 MHz
                db       0x01,0x04                       ;modeA CW modeB USB
m20m:           db       0xe0,0x89,0xd6,0                ;14,06 MHz
                db       0xc0,0xac,0xd8,0                ;14,2 MHz
                db       0x01,0x04                       ;modeA CW modeB USB
m17m:           db       0x80,0x1f,0x14,1                ;18,096 MHz
                db       0x80,0x1f,0x14,1                ;18,096 MHz
                db       0x01,0x04                       ;modeA CW modeB USB
m15m:           db       0xa0,0x59,0x41,1                ;21,06 MHz
                db       0x30,0xb9,0x42,1                ;21,15 MHz
                db       0x01,0x04                       ;modeA CW modeB USB
m12m:           db       0x90,0xca,0x7b,1                ;24,89 MHz
                db       0x90,0xca,0x7b,1                ;24,89 MHz
                db       0x01,0x04                       ;modeA CW modeB USB
m10m:           db       0x60,0x29,0xac,1                ;28,060 MHz
                db       0xe0,0xd2,0xaf,1                ;28,300 MHz
                db       0x01,0x04                       ;modeA CW modeB USB
m6m:            db       0x00,0x29,0xfc,2                ;50,08 MHz
                db       0x00,0x29,0xfc,2                ;50,08 MHz
                db       0x01,0x04                       ;modeA CW modeB USB
m2m:            db       0xe0,0xd7,0x99,8                ;144,300 MHz
                db       0xe0,0xd7,0x99,8                ;144,300 MHz
                db       0x01,0x04                       ;modeA CW modeB USB
mtv1:           db       0x00,0x3f,0xab,1                ;28 MHz
                db      0x00,0x3f,0xab,1                ;28 MH
                db      0x01,0x04                       ;ModeA Mode
                db      0,0,0,0,0,0                     ;LCD-Offset Fla
mtv2:           db      0x00,0x3f,0xab,1                ;28 MH
                db      0x00,0x3f,0xab,1                ;28 MH
                db      0x01,0x04                       ;ModeA Mode
                db      0,0,0,0,0,0                     ;LCD-Offset Fla
mtv3:           db      0x00,0x3f,0xab,1                ;28 MH
                db      0x00,0x3f,0xab,1                ;28 MH
                db      0x01,0x04                       ;ModeA Mode
                db      0,0,0,0,0,0                     ;LCD-Offset Fla
mtv4:           db      0x00,0x3f,0xab,1                ;28 MH
                db      0x00,0x3f,0xab,1                ;28 MH
                db      0x01,0x04                       ;ModeA Mode
                db      0,0,0,0,0,0                     ;LCD-Offset Fla
mtv5:           db      0x00,0x3f,0xab,1                ;28 MH
                db      0x00,0x3f,0xab,1                ;28 MH
                db      0x01,0x04                       ;ModeA Mode
                db      0,0,0,0,0,0                     ;LCD-Offset Fla
                db      0,0,0,0,0,version               ;letzte Address ist die Versionsnumme
;#############################################################################

       en
