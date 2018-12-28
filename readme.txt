# readme file for population simulation in fortran

# Authors ###############################################
Leo Meurice (meuricel)
Cedric Simal (csimal)

# Compilation ###########################################
run "make" in the terminal to compile everything
"make clean" to delete every compiled file.

# Execution #############################################
The main program can be called for a single seed with 
"./main <year> <seed>"
where <year> is the year the simulation should end at, and <seed> is the seed for the
random number generator (it can be an integer between 1 and 999).
The resulting files are written in a directory called <seed>.

To run a batch of simulations on the cluster, put all the compiled code, along
with the required data files ("Taux_fecondite.txt", "Taux_deces.txt", "Structure_Age.txt", 
"ContrainteStatut.txt", "ContrainteGenre.txt", "ContrainteDipl.txt", "ContrainteAge.txt", "BelgiqueConting.txt")
in a directory called "tpp" and run
"sbatch job.sh" in the directory containing tpp.
The resulting files are written in "tpp/result" which must exist before exection.
This will run 200 simulations.

Alternatively, you can run the 200 simulations locally by running
"bash run.sh" in the directory containing the compiled code.
Due to the large amount of disk operations done by the program, this is actually
much faster than running on the cluster, because only one program can read/write
on disk at a given time. So running on the cluster takes more time because
all these programs are trying to use the disk at the same time
and have to wait for each other to finish.

The total size of the result of 200 simulations should be about 3.6GiB.
Make sure you have enough disk space.

# Structure of the program ##############################
The main program starts by reading the data necessary for the
simulation from local files (see files listed earlier), and then
computes the simulation for each ins code in succession, creating
output files along the way.
The code is distributed across several files, each corresponding to
a module:

# main.f90
the main program

# constants.f90
constants used throughout the project

# data_utils.f90
a collection of functions and subroutines for reading/writing various
files for the simulation

# ipf.f90
synthetic population generation using the ipf method
and integerization of it's result using trs

# age.f90
subroutines for reading data about age, birth and death distributions

# random.f90
pseudorandom numbers generators

# list.f90
an implementation of a doubly linked list for use in the simulation

# pop_sim.f90
simulation of births and deaths on the population

# Output ################################################
Each simulation writes 6 files for each ins code (of which there are 38)

# stopping_criteria_<ins>.txt
contains the sequence of values of the ipf method for <ins>.
Each line contains the iteration number and the stopping criterion at that iteration

# final_pop_<ins>.txt
contains the list of individuals of the population after the ipf and trs process
The first line is a header and each subsequent line is of the format

<age group> <sex> <diploma> <work status>

These values are kept as integers to reduce file size.

# margin_<ins>.txt
contains the marginal distributions of the variables of the ipf.
The data is written as group of 5 lines with the following format:

<iteration number>
<age group distribution>
<gender distribution>
<diploma distribution>
<work status distribution>

# sextable_<ins>.txt
contains the number of men and women per year and age for the entire simulation
The first line is a header, and each subsequent line is of the format
<year> <age> <number of men> <number of women>

# population_<ins>_<year>.txt
contains the list of individuals of the population for commune <ins>
at the last year of the simulation.
The first line is a header, and each subsequent line is of the format

<age> <sex> <diploma> <work status>

These values are kept as integers in order to reduce file size.
See "data_utils.f90" for what they correspond to.

# cont_table_final_<year>_<ins>.txt
contains the contingency table of the population of commune <ins>
at the final year of the simulation.
The first line is a header and each subsequent line is of the format

<age group> <sex> <diploma> <work status> <number of people>

These values are written as strings in order to keep the same format
used in "BelgiqueConting.txt". The only difference is that only categories
with more than 0 people are written, in order to reduce file size.
