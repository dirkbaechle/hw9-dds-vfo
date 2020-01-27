
all:
	@echo "Available targets are: layout, hex, clean"

layout: avfo.pdf
	pdftotext -layout avfo.pdf > avfo.txt

hex: avfo.txt
	python get_hex.py avfo.txt > avfo.asm

clean:
	rm -f *.asm *.txt

