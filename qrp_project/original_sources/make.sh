#!/bin/bash
# (c) Andreas Lindenau DL4JAL
# Version 0.1

# Variable Strings zum Kompilieren
##################################
QUELLTEXT=mc
#LINKERSCRIPT=/usr/local/share/gputils/lkr/16f873.lkr
LINKERSCRIPT=/usr/share/gputils/lkr/16f876.lkr

# Variablen fuer das Programmiergeraet
######################################
FASSUNG=SICSP
#FASSUNG=S28
#FASSUNG=S40
#PIC=F18
PIC=F16

# loeschen der "Errorausgabe"
if [ -e error.pic ]
then
  rm error.pic
fi
# Kopilieren und eventuelle Errors in die Errorausgabe umleiten
gpasm -c -w1 $QUELLTEXT.pic | tee error.pic
# Linken und eventuelle Errors in die Errorausgabe umleiten
gplink -o $QUELLTEXT.hex -c -a inhx32 -s $LINKERSCRIPT -m $QUELLTEXT.o | tee -a error.pic
# Test ob "error.pic" einen Inhalt hat
if test -s error.pic
then
  echo "Kompilieren: Es sind Fehler aufgetreten!"
  exit 1
else
  echo "Kompilieren: Fehlerfrei!"
fi
# wurde ein Argument angehangen
if [ $# -ne 1 ]
then
  exit 0
fi
# ist das Argument "-p", dann brennen der Software in den PIC
OPTION=$1
if [ $OPTION = '-p' ]
then
  usburn -$FASSUNG -$PIC -w -H $QUELLTEXT.hex
fi
exit 0

