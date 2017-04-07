#!/usr/bin/python

import numpy as np
import srcf.main as ffnc

from   srcpy.resolve_partial_computing import *


boundaryRows, boundaryCols, \
mat_boundary, rrows, rcols, outletCells, \
x_coordinate, y_coordinate,\
NoDataValue, array_points, \
cols, rows, combinatIndex, delta_t,  \
mat_pi, mat_ppl, \
surface_retention, mat_inf_index, mat_hcrit, mat_aa, mat_b,\
mat_fd, mat_dmt, mat_efect_vrst, mat_slope, mat_nan, \
mat_a,   \
mat_n,   \
output, pixel_area, points, poradi,  end_time, spix, state_cell, \
temp, type_of_computing, vpix, mfda, sr, itera, \
toky, cell_stream, mat_tok_usek, STREAM_RATIO


ffnc.main(mat_boundary,x_coordinate, y_coordinate,
          NoDataValue, array_points,
          cols, rows, 
          mat_pi, mat_ppl,
          surface_retention, mat_inf_index, mat_hcrit, mat_aa, mat_b,
          mat_fd, mat_dmt, mat_efect_vrst, mat_slope, mat_nan,
          mat_a,
          mat_n,
          pixel_area, end_time, type_of_computing,sr,itera,output)