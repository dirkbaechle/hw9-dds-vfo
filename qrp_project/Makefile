
all:
	@echo "Available targets are: disasm, resyn, clean"	

disasm: unidds_v1_08.dis unidds_v1_10.dis
	

resyn: unidds_v1_08_rsh.hex unidds_v1_10_rsh.hex
	

clean:
	rm -f *.dis *_rsh.hex *.cod *.lst


unidds_v1_08.dis: unidds_v1_08.hex
	gpdasm -nos -p16f876 unidds_v1_08.hex > unidds_v1_08.dis

unidds_v1_10.dis: unidds_v1_10.hex
	gpdasm -nos -p16f876 unidds_v1_10.hex > unidds_v1_10.dis

unidds_v1_08_rsh.hex: unidds_v1_08.dis
	gpasm -a inhx32 -p p16f876 -o unidds_v1_08_rsh unidds_v1_08.dis

unidds_v1_10_rsh.hex: unidds_v1_10.dis
	gpasm -a inhx32 -p p16f876 -o unidds_v1_10_rsh unidds_v1_10.dis

unidds_v1_10.dis_w_labels: unidds_v1_10.hex
	gpdasm -nos -k unidds_1_10.ulist -p16f876 unidds_v1_10.hex > unidds_v1_10.dis_w_labels

