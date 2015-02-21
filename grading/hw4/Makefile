SRC=../grade.hs gradehw4.hs solution.lhs ${GRADEDIR}/hw4*.lhs

all:
	ghc -XFlexibleInstances -XFlexibleContexts -XTypeSynonymInstances \
          -i../../SOE/src/ -odir ${GRADEDIR} -o ${GRADEDIR}/gradehw4 --make ${SRC}
