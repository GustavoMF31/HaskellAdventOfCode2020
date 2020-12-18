profile: compile12 profile3
	./day15-1 +RTS -p; ./day15-2 +RTS -p;

profile3: compile3
	./day15-3 +RTS -p

compile12:
	ghc -prof -fprof-auto day15-1.hs; ghc -prof -fprof-auto day15-2.hs;

compile3:
	ghc -prof -fprof-auto day15-3.hs
