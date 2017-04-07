import os 


import srcpy.constants                         as constants
from   srcpy.tools                   import get_argv
from   srcpy.tools                   import set_argv
from   srcpy.tools                   import prt_sys_argv
from   srcpy.tools                   import int_comp_type



import  srcpy.save_load_data as sld
import srcpy.rainfall    as rainfall
#import  main_srcpy.save_load_data_nopickle    as sld   # preparated


indata = get_argv(constants.PARAMETER_INDATA)

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
toky, cell_stream, mat_tok_usek, STREAM_RATIO, tokyLoc = sld.load_data(indata)


"""
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
toky, cell_stream, mat_tok_usek, STREAM_RATIO, tokyLoc = sld.load(indata)   #preparated
"""


if get_argv(constants.PARAMETER_PATH_TO_OUTPUT_DIRECTORY) == '-':
  set_argv(constants.PARAMETER_PATH_TO_OUTPUT_DIRECTORY, output)
  
if get_argv(constants.PARAMETER_END_TIME) == '-':
  set_argv(constants.PARAMETER_END_TIME, end_time)  

if get_argv(constants.PARAMETER_MFDA) == '-':
  set_argv(constants.PARAMETER_MFDA,mfda)
  
if get_argv(constants.PARAMETER_SURFACE_RETENTION) == '-':
  set_argv(constants.PARAMETER_SURFACE_RETENTION,surface_retention)

if get_argv(constants.PARAMETER_TYPE_COMPUTING) == '-':
  set_argv(constants.PARAMETER_TYPE_COMPUTING,int_comp_type(type_of_computing))


#jj end time se musi takto delta vzdy, neni v save
output = get_argv(constants.PARAMETER_PATH_TO_OUTPUT_DIRECTORY)
end_time = float(get_argv(constants.PARAMETER_END_TIME))*60.0
mfda                    = get_argv(constants.PARAMETER_MFDA)
surface_retention       = float(get_argv(constants.PARAMETER_SURFACE_RETENTION))/1000 #prevod z [mm] na [m]



arcgis                  = get_argv(constants.PARAMETER_ARCGIS)


# ponechani deste z save
rainfall_file_path      = get_argv(constants.PARAMETER_PATH_TO_RAINFALL_FILE)
if rainfall_file_path == '-' :
  pass
# zmena deste posave, pokud se pocita jen roff
else:
  sr,itera  = rainfall.load_precipitation(rainfall_file_path)



if os.path.exists(output):
  import shutil
  shutil.rmtree(output)
if not os.path.exists(output):
  os.makedirs(output)
  #os.makedirs(output+os.sep+'prubeh')
  
print '--------- po nacteni z save ---------'    
prt_sys_argv()
print '--------- ---------- - ---- ---------'    


for item in sld.load_data(indata):
  print type(item)
  print item

