OUTPUTDIR=public
SSH_TARGET=cloud:/home/andrew/sites/evalsp20.classes/public_html

.PHONY : all

all: build

clean:
	rm -rf public/

build:
	Rscript -e "blogdown::build_site()"

serve: build
	Rscript -e "blogdown::serve_site(port=4321)"

deploy: build
	rsync -Prvzc --exclude='.DS_Store' --delete $(OUTPUTDIR)/ $(SSH_TARGET)
