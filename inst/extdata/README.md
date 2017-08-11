
# AWUDS Export Files

The files contained in the inst/extdata/ folder are AWUDS export files for use in testing wateRuse functions. Exported files are either Excel (.xlsx) or "dump" (.txt) files. The Excel files are typically exported from AWUDS for review by a Water Science Center Water-Use Specialist (WUS). Data generally have been loaded into AWUDS by WUS by populating a blank template Excel file exported from AWUDS. (Excel files imported to and exported from AWUDS are formatted identically.) The "dump" text files that may contain multiple states and years of water-use data are generally exported from AWUDS by a Regional Water-Use Specialist. 

## Excel Files

These files are currently exports of annual data for one or more categories of water- use data aggregated by county, HUC8, and aquifer (type of aggregation is termed 'Areas' in wateRuse), which are identified in the export file name. Two years (compilations) of each type are provided for batch loading and comparisons in wateRuse. Only one type of aggregation is loaded at a time in a batch of multiple files. With AWUDS release 3.2.4 in February 2017, both area codes and area names are included, and files exported from prior versions of AWUDS will no longer load into wateRuse.

###    County

/excel/Export-Indiana-County-2005-2b.xlsx, 

/excel/Export-Indiana-County-2010-2b.xlsx

###    HUC8

/excel/Export-Indiana-HUC8-2005-2b.xlsx, 

/excel/Export-Indiana-HUC8-2010-2b.xlsx

##    Aquifer

/excel/Export-Indiana-Aquifer-2005-2c.xlsx, 

/excel/Export-Indiana-Aquifer-2010-2c.xlsx

These aquifer files do not load currently (Issue#187) as AWUDS 3.2.4 dropped TP, total population worksheet for aquifers (TP data.elements is NA). 

## Dump File

/dump/exampleAWUDSdump.txt

This is an example of a file that contains multiple annual datasets for multiple states for multiple categories of water use.

This file is modified from a dataset retrieved from the U.S. Geological Survey Aggregated Water Use Database (AWUDS) on April 27, 2016, and data in AWUDS may have been revised since this date. The data were approved and contain county-level data for Hawaii and Delaware. The file contains data for multiple years, and all USGS categories of water-use data.
