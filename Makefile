CC		= ghc
FLAGS	= -O2 -Wall -XBangPatterns -odir obj -hidir obj
PFLAGS	= -rtsopts -prof -auto-all -caf-all
SRCDIR	= src
FILES	= Main.hs Octree.hs Vec3D.hs
SOURCES	= $(FILES:%.hs=${SRCDIR}/%.hs)
EXEC	= Octree

all : ${EXEC}

profile : ${SOURCES}
	${CC} ${FLAGS} ${PFLAGS} -o ${EXEC} $^

${EXEC} : ${SOURCES}
	${CC} ${FLAGS} -o $@ $^

clean :
	- rm ${EXEC}
