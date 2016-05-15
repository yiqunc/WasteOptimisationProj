# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ========================================================================
# ========================================================================
# Copyright 2015-2016 University of Melbourne
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# ========================================================================
# ========================================================================
#
# Purpose: Build vehicle travel lines from VRP optimisation outputs
# Version: 1.0
# Last Updated: 15-May-2016
# Written by: Dr. Yiqun Chen    yiqun.c@unimelb.edu.au
#
# Change Log:
# v1.0: (1) add two methods to process vehicle travel lines into shp files, 
# one for each service round, one for each service travel step with load and time attributes

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# required packages
library(maptools)   # for geospatial services; also loads foreign and sp
library(rgdal)      # for map projection work; also loads sp
library(rgeos)

# set up the working directory
setwd("C:\\Users\\chen1\\DateSets\\Chengcheng")

# in v1, all nodes in each row (one service round of a vehicle) will be formed as one line
# the output shp file attribute structure:
# (1) vehid: vehicle id
# (2) vnseqX: visit node sequence, e.g., vnseq1, vnseq2, vnseq3, ....vnseqN
# (3) round: service round number for a vehicle (each row in Solutions.csv stands for one round)
f_CreatePolylineFromSolution_v1 <- function(){
  
  # load data
  dfSolution = read.csv(file = "Solutions.csv", header=FALSE, sep=",")
  dfVehLoadRecord = read.csv(file = "VehLoadRecord.csv", header=FALSE, sep=",")
  dfNodetimes = read.csv(file = "Nodetimes.csv", header=FALSE, sep=",")
  
  # load points data
  spdfBuildingPoints = readOGR(dsn=".",layer="kinglake_residential_buildings_point_28355",encoding="utf8", verbose=FALSE)
  proj4string = spdfBuildingPoints@proj4string
  
  
  curVehId = -1
  curRoundNum = 1
  
  roundNum = c()
  linesList = list()
  # go through solution list, create one line for each row
  for(i in 1:nrow(dfSolution)){
    
    if(dfSolution[i,1]!=curVehId){
      curVehId = dfSolution[i,1]
      curRoundNum = 1
    }
    
    lineList = list()
    print(sprintf("=== process row [%i]",i))
    lineCoords = c()
    for(j in 2:ncol(dfSolution)) # this first column is vehicle id
    {
      nodeORIGFID = dfSolution[i,j]
      
      if(is.na(nodeORIGFID)) next
      
      nodeCoord = spdfBuildingPoints@coords[spdfBuildingPoints@data[,"ORIG_FID"]==nodeORIGFID]
      lineCoords = rbind(lineCoords,nodeCoord)
    }

    lineList[1] = Line(lineCoords)
    linesList[i] = Lines(lineList, i)
    
    roundNum = c(roundNum,curRoundNum)
    
    curRoundNum = curRoundNum + 1
    
    
  }
  
  spLines = SpatialLines(linesList, proj4string = proj4string)
  dfSolution[,"round"] = as.integer(roundNum)
  colnames(dfSolution) = c(c("vehid"),paste("vnseq",c(1:(ncol(dfSolution)-2)),sep=""), c("round")) # have to -2 since a new "round" column is just added
  spdfResult = SpatialLinesDataFrame(spLines, dfSolution)
  writeOGR(obj=spdfResult, dsn=".", layer="SolutionRoutes", driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
  
  print("=== all done")
}

# in v2, each continuous node pair (which represents each travel step from nodeA to nodeB) of each row (one service round of a vehicle) will be formed as one line, 
# so its load record and visit time can all be appended as attributes in the output.
# the output shp file attribute structure:
# (1) vehid: vehicle id
# (2) ttlstep: total travel step for a vehicle
# (3) round: service round number for a vehicle (each row in Solutions.csv stands for one round)
# (4) roundstep: travel step of each service round for a vehicle
# (5) sload: start load value
# (6) eload: end load value
# (7) stime: start travel time
# (8) etime: end travel time

f_CreatePolylineFromSolution_v2 <- function(){
  
  # load data
  dfSolution = read.csv(file = "Solutions.csv", header=FALSE, sep=",")
  dfVehLoadRecord = read.csv(file = "VehLoadRecord.csv", header=FALSE, sep=",")
  dfNodetimes = read.csv(file = "Nodetimes.csv", header=FALSE, sep=",")
  
  # load points data
  spdfBuildingPoints = readOGR(dsn=".",layer="kinglake_residential_buildings_point_28355",encoding="utf8", verbose=FALSE)
  proj4string = spdfBuildingPoints@proj4string
  
  # define a new dataframe to contain the output
  dfVisitDetails = data.frame(vehid=NULL,ttlstep=NULL,round=NULL, roundstep=NULL,sload=NULL, eload=NULL, stime=NULL, etime=NULL)
  curVehId = -1
  curRoundNum = 1
  curRoundStepNum = 1
  curVehTtlStepNum = 1
  linesList = list()
  counter = 1
  for(i in 1:nrow(dfSolution)){
    
    if(dfSolution[i,1]!=curVehId){
      curVehId = dfSolution[i,1]
      curRoundNum = 1
      curVehTtlStepNum = 1
      print(sprintf("=== new vehicle found [%i]",curVehId))
    }
    
    print(sprintf("=== process vehid %i, round %i",curVehId,curRoundNum))
    
    curRoundStepNum = 1
    for(j in 2:(ncol(dfSolution)-1)){

      nodeORIGFID_Start = dfSolution[i,j]
      nodeORIGFID_End = dfSolution[i,j+1]
      
      if(is.na(nodeORIGFID_End)) next
      
      # grab coords, load and time attributes for each segment
      nodeCoord_Start = spdfBuildingPoints@coords[spdfBuildingPoints@data[,"ORIG_FID"]==nodeORIGFID_Start]
      nodeCoord_End = spdfBuildingPoints@coords[spdfBuildingPoints@data[,"ORIG_FID"]==nodeORIGFID_End]
      nodeTime_Start =  dfNodetimes[i,j]
      nodeTime_End =  dfNodetimes[i,j+1]
      nodeLoad_Start =  dfVehLoadRecord[i,j]
      nodeLoad_End =  dfVehLoadRecord[i,j+1]
        
      lineList = list()
      lineList[1] = Line(rbind(nodeCoord_Start,nodeCoord_End))
      linesList[counter] = Lines(lineList, counter)
      dfVisitDetails = rbind(dfVisitDetails,data.frame(vehid=curVehId,ttlstep=curVehTtlStepNum,round=curRoundNum,roundstep=curRoundStepNum, sload=nodeLoad_Start,eload=nodeLoad_End,stime=nodeTime_Start,etime=nodeTime_End))
      
      # increase counter
      counter = counter + 1
      
      # increase round step number
      curRoundStepNum = curRoundStepNum + 1
      
      # increase vehicle total step number
      curVehTtlStepNum = curVehTtlStepNum + 1
      
    }
    
    # increase vehicle round number
    curRoundNum = curRoundNum + 1
  }
  
  spLines = SpatialLines(linesList, proj4string = proj4string)
  
  # enforce column type as integer 
  dfVisitDetails[,"ttlstep"] = as.integer(dfVisitDetails[,"ttlstep"])
  dfVisitDetails[,"round"] = as.integer(dfVisitDetails[,"round"])
  dfVisitDetails[,"roundstep"] = as.integer(dfVisitDetails[,"roundstep"])
  
  spdfResult = SpatialLinesDataFrame(spLines, dfVisitDetails)
  writeOGR(obj=spdfResult, dsn=".", layer="SolutionRoutes_Detail", driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
  
  print("=== all done")
}
