#!/bin/bash
f90wrap -m types types.f90
gfortran -c types.f90
f2py -c f90wrap_types.f90 main.f90 -m main