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
	cp gobble.so $(out)/lib/csv-site
	cp gobbler.so $(out)/bin
	cp gobbler $(out)/bin

gobble.so : gobble.sls code/*.scm
	echo "(compile-library \"$<\")" | $(scheme)

gobbler.so : gobbler.ss gobble.so
	echo "(compile-program \"$<\")" | $(scheme)

share/collins.fasl : make-dictionary.ss gobble.so
	scheme --script make-dictionary.ss

boards : gobbler.so
	rm -rf $(local-boards)
	mkdir -p $(local-boards)
	$(scheme) --program $< -n $(boards) -dawg share/collins.fasl -d $(local-boards)

clean :
	find . -name "*~" -exec rm {} \;
	find . -name "*.so" -exec rm {} \;
