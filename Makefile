OUTPUT_DIR = pdf

${OUTPUT_DIR}/graphcount.pdf: src/*.lhs
	mkdir -p ${OUTPUT_DIR}
	lhs2TeX src/Main.lhs > ${OUTPUT_DIR}/graphcount.tex
	cd ${OUTPUT_DIR} && pdflatex graphcount.tex
