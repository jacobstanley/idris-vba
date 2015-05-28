warp: pythag.bas webcloud.bas

%.bas: %.idr
	idris $< --codegen vba -o $@

clean:
	rm -f *.ibc
	rm -f *.bas
