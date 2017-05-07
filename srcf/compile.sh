#!/bin/bash
# f90wrap -m fnc fnc.f90 types.f90

# f2py -c -m main fnc.f90 f90wrap_*.f90 *.o

# gfortran -c f90wrap_types.f90
# f90wrap -m fnc fnc.f90
# gfortran  -c fnc.f90
# gfortran  -c main.f90
# f2py -c f90wrap_types.f90 f90wrap_fnc.f90 main.f90 -m main

# f2py -c f90wrap_types.o fnc.o main.o -m main


# gfortran -c f90wrap_types.o fnc.o -o main


rm *.so *.o *.mod



f90wrap -m types types.f90
gfortran -c types.f90
gfortran -c -fPIC fnc.f90
gfortran -c -fPIC process.f90
gfortran -c -fPIC runoff.f90
f2py -c f90wrap_types.f90 types.o fnc.o process.o runoff.o main.f90 -m main



# f90wrap -m types types.f90 fnc.f90
# gfortran -c types.f90
# gfortran -c types.f90
# f90wrap -m fnc .f90
# f2py -c f90wrap_types.f90  fnc.f90 main.f90 -m main







# tanto je to ok s types jen
# f90wrap -m types types.f90
# gfortran -c types.f90
# f2py -c f90wrap_types.f90  main.f90 -m main





# 2 soubory f2py bez der type
# f2py -c fnc.f90  main.f90 -m main


# types pouzito jen v mailu
# f90wrap -m types types.f90
# gfortran -c types.f90
# f2py -c f90wrap_types.f90 fnc.f90 main.f90 -m main