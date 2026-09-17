
MAKEFILE=Makefile
VERSION=0.1.0
PANDOC=/path/to/bin/pandoc

.PHONY: help

#------------------------------------------------------------------
# Targets
#------------------------------------------------------------------


help:
	@echo ""
	@echo "- Available targets:"
	@echo "- Info: make check VERSION=0.0.1 "
	@echo "- Example: make release VERSION=0.0.1"
	@echo "- Example: make hotfix VERSION=0.0.1"
	@perl -ne 'if(	/^([A-Za-z]+[A-Za-z_]*):/){print "\t",$$1,"\n"}' $(MAKEFILE)
	@echo ""
	@echo ""

clean:
	@rm -rf ./dplab.Rcheck
	@rm -f tests/testthat/Rplot*; rm -rf tests/testthat/_snaps; rm -rf tests/testthat/results; rm -rf results
	@rm -f *~

check: clean
	rm -rf /tmp/dplab; mkdir -p /tmp/dplab; cp -r ./* /tmp/dplab; cd /tmp/dplab; \
	cd ..; R_PROFILE_USER=/dev/null R_ENVIRON_USER=/dev/null R CMD build dplab; R_PROFILE_USER=/dev/null R_ENVIRON_USER=/dev/null R CMD check --no-stop-on-test-error dplab_$(VERSION).tar.gz

run_example:
	@echo "devtools::run_examples(pkg = '.')" | R --slave

checkfast: clean
	@rm -f dplab_$(VERSION).tar.gz check; \
	R_PROFILE_USER=/dev/null R_ENVIRON_USER=/dev/null R CMD build --no-build-vignettes . && _R_CHECK_FORCE_SUGGESTS_=false R_PROFILE_USER=/dev/null R_ENVIRON_USER=/dev/null R CMD check dplab_$(VERSION).tar.gz

build_and_check: doc
	rm -f check; echo ">>>Building"; R CMD build . ;  echo ">>>Checking"; R CMD check `ls -t . | head -n1`

all: build_and_check

doc:
	@echo ">>> Creating a package documentation"
	@echo ">>> Deleting NAMESPACE and man"
	rm -Rf NAMESPACE man;
	echo "library(roxygen2); roxygen2::roxygenise()" | R --slave

install:
	@echo ">>> Installing..."
	@rm -f src/*.o src/*.so
	@R CMD INSTALL .

test:
	@echo ">>> Testing package"
	@rm -rf `ls tests/testthat/| grep -v \R$$`
	@echo "devtools::test()" | R --slave

test_by_file:
	@echo 'library(dplab); for(i in dir("./tests/testthat/", pattern = ".R$$")){devtools::test_active_file(file.path("./tests/testthat/", i))}' | R --slave

coverage:
	@echo "Checking coverage"
	@echo "usethis::use_github_action('test-coverage'); cov <- covr::package_coverage(); print(as.data.frame(cov))" | R --slave

codecov:
	@echo "Uploading coverage (https://app.codecov.io/github/dputhier/dplab)"
	@echo "library(covr); codecov(token ='8f08768a-0629-4ed0-91b9-bdd9f7019916')" | R --slave


#------------------------------------------------------------------
# Creating a release
#------------------------------------------------------------------

__check_defined_VER:
	@[ "$(VERSION)" ] || ( echo ">> VER is not set"; exit 1 )

release: __check_defined_VER
	@ echo "#-----------------------------------------------#"
	@ echo "# Bumping the program version                   #"
	@ echo "#-----------------------------------------------#"
	@ git flow release start 'v$(VERSION)'
	@ git checkout ./DESCRIPTION
	@ git checkout ./Makefile
	@ R CMD INSTALL .
	@ cat ./DESCRIPTION | perl -npe "s/Version: .*/Version: $(VERSION)/" > /tmp/dplab.bump
	@ mv /tmp/dplab.bump ./DESCRIPTION
	@ cat ./Makefile | perl -npe 's/^VERSION=.*/VERSION=$(VERSION)/' > /tmp/dplab.bump
	@ mv /tmp/dplab.bump ./Makefile
	@ echo "Version was bump to $(VERSION)"
	@ make install
	@ git commit -m 'Bumped version $(VERSION)'

release_finish: __check_defined_VER
	@ echo "#-----------------------------------------------#"
	@ echo "# Finishing the release                         #"
	@ echo "#-----------------------------------------------#"
	@ git flow release finish 'v$(VERSION)'
	@ git push origin develop
	@ git push origin main --tags

hotfix: __check_defined_VER
	@ echo "#-----------------------------------------------#"
	@ echo "# Bumping the program version                   #"
	@ echo "#-----------------------------------------------#"
	@ git hf hotfix start 'v$(VERSION)'
	@ git checkout ./DESCRIPTION
	@ git checkout ./Makefile
	@ R CMD INSTALL .
	@ cat ./DESCRIPTION | perl -npe "s/Version: .*/Version: $(VERSION)/" > /tmp/dplab.bump
	@ mv /tmp/dplab.bump ./DESCRIPTION
	@ cat ./Makefile | perl -npe 's/^VERSION=.*/VERSION=$(VERSION)/' > /tmp/dplab.bump
	@ mv /tmp/dplab.bump ./Makefile
	@ echo "Version was bump to $(VERSION)"
	@ make install
	@ git commit -m 'Bumped version $(VERSION)'

readme: clean
	@ echo "- Rebuilting README.md from README.Rmd"
	@ echo "devtools::build_readme()" | R --slave

build_vignette_as_pdf: clean
	@ echo "Sys.setenv(RSTUDIO_PANDOC='$(PANDOC)'); rmarkdown::render('vignettes/dplab-usage.Rmd', 'pdf_document')" | R --slave
	@ mkdir -p inst/doc; mv vignettes/dplab-usage.pdf inst/doc

doc_html:
	@ echo "#-----------------------------------------------#"
	@ echo "# Building doc                                  #"
	@ echo "#-----------------------------------------------#"
	@ echo "Sys.setenv(RSTUDIO_PANDOC='$(PANDOC)'); library(knitr); pkgdown::build_site(install = TRUE, examples = FALSE)" | R --slave
	@ git add -u
	@ git commit -m "Updated html doc to $(VERSION)."


all: doc install check test
