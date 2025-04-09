================================================================================
Documentation: lancet greenspace indicator dataset
================================================================================
Urban greenspace indicators for over 1,000 global urban areas. File includes 
absolute peak and annual NDVI as well as population weighted peak and annual
NDVI. Indicator levels:

	Exceptionally high		NDVI ≥ 0.7
	Very high				0.6 ≤ NDVI < 0.7
	High					0.5 ≤ NDVI < 0.6
	Moderate				0.4 ≤ NDVI < 0.5
	Low						0.3 ≤ NDVI < 0.4
	Very Low				0.2 ≤ NDVI < 0.3
	Exceptionally Low		NDVI < 0.2


---------- FILE METADATA -------------------------------------------------------
File name:              greenspace_data_share.csv
File created by:        Jennifer Stowell (stowellj@bu.edu)
Date added:             
Date last modified:     


---------- DATA EXTENT/RESOLUTION ----------------------------------------------

Time extent:            2010-2021
Time resolution:        annual

Spatial extent:         global
Spatial resolution:     urban area


---------- VARIABLES -----------------------------------------------------------
City					Urban area name
Country					Country location of urban area
Major_Geo_Region		Major global region
HDI_level				level of human development index
Climate_region			major climate region
WHO_region				WHO region
annual_avg_****			Annual NDVI as mean of seasonal NDVI values
peak_NDVI_****			Annual maximum NDVI
annual_weight_avg_****	Population weighted annual NDVI as mean of seasonal NDVI values
peak_weight_****		Population weighted annual maximum NDVI
indicator_****			Category of NDVI (levels outlined above)


---------- DESCRIPTION OF DATA SET DERIVATION ----------------------------------
Code directory: /projectnb/climlab/Data_Hub/<dataset>/Code

General steps used to process and compile data:
	1.  Collected data from: 
		a. Gridded Population of the World (GPWv4)
		b. Global Human Settlement Urban Centre Database R2019A
		c. Human Development Index
		d. Köppen-Geiger climate classifications
		e. NDVI from Landsat 7 and 8
	2.  ArcGIS used to subset cities by population and imported shapefiles to 
		Google Earth Engine (GEE)
	3.  Built-in GEE methods used to compute NDVI values by pixel and season
	4.  Values for annual and peak NDVI calculated by urban area.
	5.  R statistical software used to calculate population weighted values of
		NDVI measures