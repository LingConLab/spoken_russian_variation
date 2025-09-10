R_OPTS = --vanilla

.DEFAULT_GOAL: all
.PHONY: clean all

all: compile clean

compile:
	Rscript scripts/generate_qmd_and_compile.R
clean:
	rm -rf docs/data docs/LICENSE docs/Makefile docs/scripts *.html
