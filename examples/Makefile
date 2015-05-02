war: pythag.bas webcloud.bas

%.bas: %.idr
	idris $< --codegen vba -o $@

clean:
	rm *.ibc
	rm *.bas
