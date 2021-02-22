EESchema Schematic File Version 4
EELAYER 30 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 1
Title "Simple preamp for HW9 DDS-VFO"
Date "2021-02-21"
Rev "1.0-1"
Comp "Dirk Baechle, DL9OBN"
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L hw9_preamp:BFR93A Q1
U 1 1 60318E1A
P 4900 3400
F 0 "Q1" H 5090 3446 50  0000 L CNN
F 1 "BFR93A" H 5090 3355 50  0000 L CNN
F 2 "Package_TO_SOT_SMD:SOT-23" H 5100 3325 50  0001 L CIN
F 3 "https://assets.nexperia.com/documents/data-sheet/BFR92A_N.pdf" H 4900 3400 50  0001 L CNN
	1    4900 3400
	1    0    0    -1  
$EndComp
$Comp
L Device:R R2
U 1 1 60319464
P 5000 2750
F 0 "R2" H 5070 2796 50  0000 L CNN
F 1 "4.7k" H 5070 2705 50  0000 L CNN
F 2 "Resistor_SMD:R_0805_2012Metric" V 4930 2750 50  0001 C CNN
F 3 "~" H 5000 2750 50  0001 C CNN
	1    5000 2750
	1    0    0    -1  
$EndComp
$Comp
L Device:R R4
U 1 1 603198C6
P 5000 4100
F 0 "R4" H 5070 4146 50  0000 L CNN
F 1 "1k" H 5070 4055 50  0000 L CNN
F 2 "Resistor_SMD:R_0805_2012Metric" V 4930 4100 50  0001 C CNN
F 3 "~" H 5000 4100 50  0001 C CNN
	1    5000 4100
	1    0    0    -1  
$EndComp
$Comp
L Device:R R1
U 1 1 60319C57
P 4250 2750
F 0 "R1" H 4320 2796 50  0000 L CNN
F 1 "100k" H 4320 2705 50  0000 L CNN
F 2 "Resistor_SMD:R_0805_2012Metric" V 4180 2750 50  0001 C CNN
F 3 "~" H 4250 2750 50  0001 C CNN
	1    4250 2750
	1    0    0    -1  
$EndComp
$Comp
L Device:R R3
U 1 1 6031A362
P 4250 4100
F 0 "R3" H 4377 4146 50  0000 L CNN
F 1 "33k" H 4377 4055 50  0000 L CNN
F 2 "Resistor_SMD:R_0805_2012Metric" V 4180 4100 50  0001 C CNN
F 3 "~" H 4250 4100 50  0001 C CNN
	1    4250 4100
	1    0    0    -1  
$EndComp
$Comp
L Device:C C3
U 1 1 6031A75E
P 5500 4100
F 0 "C3" H 5615 4146 50  0000 L CNN
F 1 "10nF" H 5615 4055 50  0000 L CNN
F 2 "Capacitor_SMD:C_0805_2012Metric" H 5538 3950 50  0001 C CNN
F 3 "~" H 5500 4100 50  0001 C CNN
	1    5500 4100
	1    0    0    -1  
$EndComp
$Comp
L Device:C C2
U 1 1 6031AA9C
P 5950 3000
F 0 "C2" V 5698 3000 50  0000 C CNN
F 1 "10nF" V 5789 3000 50  0000 C CNN
F 2 "Capacitor_SMD:C_0805_2012Metric" H 5988 2850 50  0001 C CNN
F 3 "~" H 5950 3000 50  0001 C CNN
	1    5950 3000
	0    1    1    0   
$EndComp
$Comp
L Device:C C1
U 1 1 6031AF89
P 3600 3400
F 0 "C1" V 3348 3400 50  0000 C CNN
F 1 "10nF" V 3439 3400 50  0000 C CNN
F 2 "Capacitor_SMD:C_0805_2012Metric" H 3638 3250 50  0001 C CNN
F 3 "~" H 3600 3400 50  0001 C CNN
	1    3600 3400
	0    1    1    0   
$EndComp
$Comp
L Device:R R5
U 1 1 6031B71D
P 6500 3650
F 0 "R5" H 6570 3696 50  0000 L CNN
F 1 "200k" H 6570 3605 50  0000 L CNN
F 2 "Resistor_SMD:R_0805_2012Metric" V 6430 3650 50  0001 C CNN
F 3 "~" H 6500 3650 50  0001 C CNN
	1    6500 3650
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0101
U 1 1 6031BE9A
P 5000 4750
F 0 "#PWR0101" H 5000 4500 50  0001 C CNN
F 1 "GND" H 5005 4577 50  0000 C CNN
F 2 "" H 5000 4750 50  0001 C CNN
F 3 "" H 5000 4750 50  0001 C CNN
	1    5000 4750
	1    0    0    -1  
$EndComp
$Comp
L power:+9V #PWR0102
U 1 1 6031C285
P 5000 2100
F 0 "#PWR0102" H 5000 1950 50  0001 C CNN
F 1 "+9V" H 5015 2273 50  0000 C CNN
F 2 "" H 5000 2100 50  0001 C CNN
F 3 "" H 5000 2100 50  0001 C CNN
	1    5000 2100
	1    0    0    -1  
$EndComp
Wire Wire Line
	6100 3000 6500 3000
Wire Wire Line
	6500 3500 6500 3000
Wire Wire Line
	5000 3600 5000 3750
Wire Wire Line
	5500 3950 5500 3750
Wire Wire Line
	5500 3750 5000 3750
Connection ~ 5000 3750
Wire Wire Line
	5000 3750 5000 3950
Wire Wire Line
	5000 4250 5000 4550
Wire Wire Line
	5500 4250 5500 4550
Wire Wire Line
	5500 4550 5000 4550
Connection ~ 5000 4550
Wire Wire Line
	5000 4550 5000 4750
Wire Wire Line
	4250 4250 4250 4550
Wire Wire Line
	4250 4550 5000 4550
Wire Wire Line
	6500 3800 6500 4550
Wire Wire Line
	6500 4550 5500 4550
Connection ~ 5500 4550
Wire Wire Line
	3750 3400 3900 3400
Wire Wire Line
	4250 2900 4250 3400
Connection ~ 4250 3400
Wire Wire Line
	4250 3400 4700 3400
Wire Wire Line
	4250 3950 4250 3400
Wire Wire Line
	5000 3200 5000 3000
Wire Wire Line
	5800 3000 5000 3000
Connection ~ 5000 3000
Wire Wire Line
	5000 3000 5000 2900
Wire Wire Line
	5000 2100 5000 2350
Wire Wire Line
	4250 2600 4250 2350
Wire Wire Line
	4250 2350 5000 2350
Connection ~ 5000 2350
Wire Wire Line
	5000 2350 5000 2600
$Comp
L Connector_Generic:Conn_01x02 J3
U 1 1 60328669
P 7550 3100
F 0 "J3" H 7630 3092 50  0000 L CNN
F 1 "RF OUT" H 7630 3001 50  0000 L CNN
F 2 "hw9_preamp:Wire_Connector_SMD_01x02" H 7550 3100 50  0001 C CNN
F 3 "~" H 7550 3100 50  0001 C CNN
	1    7550 3100
	1    0    0    1   
$EndComp
$Comp
L Connector_Generic:Conn_01x02 J1
U 1 1 60328E7B
P 2600 3500
F 0 "J1" H 2518 3175 50  0000 C CNN
F 1 "RF IN" H 2518 3266 50  0000 C CNN
F 2 "hw9_preamp:Wire_Connector_SMD_01x02" H 2600 3500 50  0001 C CNN
F 3 "~" H 2600 3500 50  0001 C CNN
	1    2600 3500
	-1   0    0    1   
$EndComp
$Comp
L Connector_Generic:Conn_01x02 J4
U 1 1 603293B2
P 9000 1600
F 0 "J4" H 8918 1275 50  0000 C CNN
F 1 "POWER" H 8918 1366 50  0000 C CNN
F 2 "hw9_preamp:Wire_Connector_SMD_01x02" H 9000 1600 50  0001 C CNN
F 3 "~" H 9000 1600 50  0001 C CNN
	1    9000 1600
	-1   0    0    1   
$EndComp
Wire Wire Line
	3450 3400 2800 3400
Wire Wire Line
	3150 3500 3150 4550
Wire Wire Line
	3150 4550 3900 4550
Wire Wire Line
	2800 3500 3150 3500
Connection ~ 4250 4550
Wire Wire Line
	7350 3000 6500 3000
Connection ~ 6500 3000
Wire Wire Line
	7350 3100 7050 3100
Wire Wire Line
	7050 3100 7050 4550
Wire Wire Line
	7050 4550 6500 4550
Connection ~ 6500 4550
$Comp
L power:+9V #PWR0103
U 1 1 6032B990
P 9350 1050
F 0 "#PWR0103" H 9350 900 50  0001 C CNN
F 1 "+9V" H 9365 1223 50  0000 C CNN
F 2 "" H 9350 1050 50  0001 C CNN
F 3 "" H 9350 1050 50  0001 C CNN
	1    9350 1050
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0104
U 1 1 6032BF86
P 9350 1800
F 0 "#PWR0104" H 9350 1550 50  0001 C CNN
F 1 "GND" H 9355 1627 50  0000 C CNN
F 2 "" H 9350 1800 50  0001 C CNN
F 3 "" H 9350 1800 50  0001 C CNN
	1    9350 1800
	1    0    0    -1  
$EndComp
Wire Wire Line
	9200 1600 9350 1600
Wire Wire Line
	9350 1600 9350 1800
Wire Wire Line
	9200 1500 9350 1500
Wire Wire Line
	9350 1500 9350 1050
$Comp
L Connector_Generic:Conn_01x02 J2
U 1 1 6032D944
P 3700 4150
F 0 "J2" H 3618 3825 50  0000 C CNN
F 1 "OPT TRIM" H 3618 3916 50  0000 C CNN
F 2 "Resistor_SMD:R_0805_2012Metric" H 3700 4150 50  0001 C CNN
F 3 "~" H 3700 4150 50  0001 C CNN
	1    3700 4150
	-1   0    0    1   
$EndComp
Wire Wire Line
	3900 4050 3900 3400
Connection ~ 3900 3400
Wire Wire Line
	3900 3400 4250 3400
Wire Wire Line
	3900 4150 3900 4550
Connection ~ 3900 4550
Wire Wire Line
	3900 4550 4250 4550
$Comp
L power:PWR_FLAG #FLG0101
U 1 1 603484AB
P 7600 1150
F 0 "#FLG0101" H 7600 1225 50  0001 C CNN
F 1 "PWR_FLAG" H 7600 1323 50  0000 C CNN
F 2 "" H 7600 1150 50  0001 C CNN
F 3 "~" H 7600 1150 50  0001 C CNN
	1    7600 1150
	-1   0    0    1   
$EndComp
$Comp
L power:PWR_FLAG #FLG0102
U 1 1 603490CD
P 7600 1700
F 0 "#FLG0102" H 7600 1775 50  0001 C CNN
F 1 "PWR_FLAG" H 7600 1873 50  0000 C CNN
F 2 "" H 7600 1700 50  0001 C CNN
F 3 "~" H 7600 1700 50  0001 C CNN
	1    7600 1700
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0105
U 1 1 6034945D
P 7600 1800
F 0 "#PWR0105" H 7600 1550 50  0001 C CNN
F 1 "GND" H 7605 1627 50  0000 C CNN
F 2 "" H 7600 1800 50  0001 C CNN
F 3 "" H 7600 1800 50  0001 C CNN
	1    7600 1800
	1    0    0    -1  
$EndComp
$Comp
L power:+9V #PWR0106
U 1 1 603498EB
P 7600 1050
F 0 "#PWR0106" H 7600 900 50  0001 C CNN
F 1 "+9V" H 7615 1223 50  0000 C CNN
F 2 "" H 7600 1050 50  0001 C CNN
F 3 "" H 7600 1050 50  0001 C CNN
	1    7600 1050
	1    0    0    -1  
$EndComp
Wire Wire Line
	7600 1150 7600 1050
Wire Wire Line
	7600 1700 7600 1800
$EndSCHEMATC
