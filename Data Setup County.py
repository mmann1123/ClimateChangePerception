# -*- coding: utf-8 -*-
# Import system modules
import arcpy
from arcpy import env
 
# Set environment settings
env.workspace = "C://Users//mmann//Google Drive//Climate Change Perception//Mike County Data Files"
arcpy.env.overwriteOutput = True

#%%

# list all shapefiles
#arcpy.ListFeatureClasses("S*")


#%% Loop through various record lengths and missing data #s 
for record_length in [30,40,50]:   #30,40,50
    for missing in [5,10,15]:

#R_Start: starting year of the record for the station.
#R_End: ending year of the record for the station.
#R_Length: among all 365 days, what is the max length of record.
#R_Gap: number of years that data is missing.
#R_Min: among all 365 days, what is the min length of record. 

record_length = str(record_length)   # convert to string for this to work
missing = str(missing)

# Select stations meeting minimum criteria 
arcpy.MakeFeatureLayer_management("//Station Data//Stations_equidistant.shp", "Stations_select")  # copy data into memory
names = arcpy.ListFields("Stations_select")

for field in names:
    print("{0} is a type of {1} with a length of {2}"
          .format(field.name, field.type, field.length))
          
arcpy.SelectLayerByAttribute_management("Stations_select", "NEW_SELECTION",'"R_End" >= 2014')          
arcpy.SelectLayerByAttribute_management("Stations_select", "SUBSET_SELECTION",'("R_End" - "R_Start" + 1) > ' + record_length )
arcpy.SelectLayerByAttribute_management("Stations_select", "SUBSET_SELECTION",'("R_End" - "R_Start" + 1) - ("R_Min") < ' + missing )
arcpy.CopyFeatures_management("Stations_select", "//Intermediates//Stations_select")            # make physical copy


#%% CREATE POLYGONS LINKING STATION TO NEAREST CENSUS BOUNDARY
# Set local variables
inFeatures = "//Intermediates//Stations_select.shp"
outFeatureClass = "//Intermediates//Stations_eq_ThiesP.shp"
outFields = "ALL"
 
# Execute CreateThiessenPolygons
arcpy.CreateThiessenPolygons_analysis(inFeatures, outFeatureClass, outFields)


#%%
# UNION OF STATES AND THEISSEN POLYGONS 
# LINK STATION POLYGONS TO CENSUS
inFeatures = ["//Boundary Files//CensusTractVotingAge_equidistant.shp", "//Intermediates//Stations_eq_ThiesP.shp"]
outFeatures = "//Intermediates//Census_Station_P2.shp"
arcpy.Union_analysis (inFeatures, outFeatures, "ALL")

#%% Create feature selection in order to remove all water bodies
arcpy.MakeFeatureLayer_management("//Intermediates//Census_Station_P2.shp", "Census_Station_P3")   # copy into memory
names = arcpy.ListFields("Census_Station_P3")
print names
for field in names:
    print("{0} is a type of {1} with a length of {2}"
          .format(field.name, field.type, field.length))

#%%

# Make a layer from the feature class  Remove all water 
arcpy.SelectLayerByAttribute_management("Census_Station_P3", "NEW_SELECTION", ' "ALAND10" > 0 ')
#arcpy.FeatureClassToFeatureClass_conversion("Census_Station_P3","G:/Faculty/Mann/Projects/Global Warming/", "Census_Station_P3_Nowater", NULL)
arcpy.CopyFeatures_management("Census_Station_P3", "//Intermediates//Census_Station_P3_Nowater")


#%%
# CLIP TO STATES 
in_features = "//Intermediates//Census_Station_P3_Nowater.shp"
clip_features = "//Boundary Files//States_equidistant.shp"
out_feature_class = "//Intermediates//Census_Station_Clip3.shp"
xy_tolerance = ""

# Execute Clip
arcpy.Clip_analysis(in_features, clip_features, out_feature_class, xy_tolerance)

#%%

# FIGURE OUT WEIGHTING SCHEME BASED ON AREA
arcpy.CalculateAreas_stats("//Intermediates//Census_Station_Clip3.shp", "//Output//Census_Station_"+record_length+"_"+missing+".shp")
#arcpy.Delete_management("in_memory","")




#%%
#switching over to R now to read the .dbf and find weighted mean





#%%
# UNION OF STATES AND THEISSEN POLYGONS 
# LINK STATION POLYGONS TO CENSUS
#inFeatures = ["States_equidistant.shp", "Stations_eq_ThiesP.shp"]
#outFeatures = "State_Station_P2.shp"
#arcpy.Union_analysis (inFeatures, outFeatures, "ALL")

#%%
# CLIP TO STATES 

#in_features = "State_Station_P2.shp"
#clip_features = "States_equidistant.shp"
#out_feature_class = "G://Faculty//Mann//Projects//Global Warming//State_Station_Clip2.shp"
#xy_tolerance = ""

# Execute Clip
#arcpy.Clip_analysis(in_features, clip_features, out_feature_class, xy_tolerance)

#%%
# FIGURE OUT WEIGHTING SCHEME BASED ON AREA
#arcpy.CalculateAreas_stats("State_Station_Clip2.shp", "State_Station_Clip2_Area.shp")

#%%
#switching over to R now to read the .dbf and find weighted mean






