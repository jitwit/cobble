lib-path = .:$(CHEZSCHEMELIBDIRS)
scheme = scheme -q --optimize-level 3 --libdirs $(lib-path)
boards = 1000
local-boards = local-boards
objs = gobbler.so gobble.so
out = 

.PHONY : clean boards

build : $(objs)

install : $(objs)
	mkdir -p $(out)/lib/csv-site
	mkdir -p $(out)/bin
	cp gobbler.so $(out)/lib/csv-site
	cp gobble.so $(out)/bin
	cp gobbler $(out)/bin

gobble.so : gobble.sls code/*.scm
	echo "(compile-library \"$<\")" | $(scheme)

gobbler.so : gobbler.ss gobble.so
	echo "(compile-script \"$<\")" | $(scheme)

boards : gobbler.so
	rm -rf $(local-boards)
	mkdir -p $(local-boards)
	$(scheme) --program $< -n $(boards) -dawg share/collins.fasl -d $(local-boards)

clean :
	find . -name "*~" -exec rm {} \;
	find . -name "*.so" -exec rm {} \;
