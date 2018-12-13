# Makefile pour le projet
# Pour compiler, il suffit d'ecrire 'make' dans la shell
# make va compiler tous les fichiers necessaires et les linker dans l'executable
# seuls les fichiers modifies sont recompiles

# le compilateur a utiliser
CC=gfortran
# les options du compilateur e.g. -Wall
CFLAGS= -Wall -fdefault-integer-8
# les librairies a linker
LIBS=

# les objets
OBJS=constants.o age.o list.o random.o pop_sim.o  data_utils.o ipf.o main.o

# Regle implicite pour compiler les fichiers .f90 en fichier .o (objets)
%.o: %.f90
	$(CC) $(CFLAGS) -c $<
#.f90.mod:
#	$(CC) $(CFLAGS) -c $<

main: $(OBJS)
	$(CC) -o main $(OBJS) $(LIBS)

clean:
	rm -f *.o *.mod

# Do not delete this line
