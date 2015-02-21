SRC=../grade.hs gradefinal.hs solution.lhs ${GRADEDIR}/final.lhs

all:
	ghc -XFlexibleContexts -XFlexibleInstances \
		-odir ${GRADEDIR} -o ${GRADEDIR}/gradefinal --make ${SRC}
