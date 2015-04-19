manual/manual.% : manual/manual.md
	pandoc -s -o $@ $^
