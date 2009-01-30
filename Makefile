all:
	make -C site-packages/lisp/sam
	make -C site-packages/lisp/introspector
	make -C site-packages/lisp/anti-gnus

clean:
	make -C site-packages/lisp/sam clean
	make -C site-packages/lisp/introspector clean
	make -C site-packages/lisp/anti-gnus clean
