### DataAnalysis_ActivityMonitoring_Devices.R --- 
## 
## Filename: DataAnalysis_ActivityMonitoring_Devices.R
## Description: 
## Author: Sergio-Feliciano Mendoza-Barrera
## Maintainer: 
## Created: Wed Oct 15 07:22:12 2014 (-0500)
## Version: 
## Package-Requires: ()
## Last-Updated: Thu Oct 16 08:43:13 2014 (-0500)
##           By: Sergio-Feliciano Mendoza-Barrera
##     Update #: 136
## URL: 
## Doc URL: 
## Keywords: 
## Compatibility: 
## 
######################################################################
## 
### Commentary: 
## 
## It is now possible to collect a large amount of data about personal
## movement using activity monitoring devices such as a Fitbit, Nike
## Fuelband, or Jawbone Up. These type of devices are part of the
## “quantified self” movement – a group of enthusiasts who take
## measurements about themselves regularly to improve their health, to
## find patterns in their behavior, or because they are tech
## geeks. But these data remain under-utilized both because the raw
## data are hard to obtain and there is a lack of statistical methods
## and software for processing and interpreting the data.

## This assignment makes use of data from a personal activity
## monitoring device. This device collects data at 5 minute intervals
## through out the day. The data consists of two months of data from
## an anonymous individual collected during the months of October and
## November, 2012 and include the number of steps taken in 5 minute
## intervals each day.
## 
######################################################################
## 
### Change Log:
## 
## 
######################################################################
## 
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or (at
## your option) any later version.
## 
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
## 
######################################################################
## 
### Code:
rm(list = ls())                         # Remove all workspace data
library(data.table)
library(plyr)
library(ggplot2)
library(parallel)
library(scales)

## Directory structure creation
if(!file.exists("../data")) {
        dir.create(../"data")
}

if(!file.exists("../graphs")) {
        dir.create("../graphs")
}

######################################################################
## Loading and processing the data

## Reading camera data in csv format, in this case read.csv() method
## was used for the appropiate data source.
deviceData <- read.table("../data/activity.csv", sep = ",", header = TRUE)

## Setting date field as date class, the initial class for this field
## was factor, then the conversion will allow us to manipulate dates
## in an easy way
deviceData <- transform(deviceData, date = as.Date(date, format = "%Y-%m-%d"))

## Using data table class in order to improve the speed
dd.dt <- data.table(deviceData)

## Delete data frames not needed
rm(deviceData)

#######################
## Summary help code ##
head(dd.dt)
summary(dd.dt)
print(head(dd.dt))
anyNA(dd.dt$date)
anyNA(dd.dt$steps)
anyNA(dd.dt$interval)
#######################

######################################################################
## What is mean total number of steps taken per day?

## Calculating the number of steps per date:
stepsPerDay <- aggregate(dd.dt$steps, list(dd.dt$date), FUN = "sum")

colnames(stepsPerDay) <- c("date", "steps")

## The histogram si showed below,
g <- ggplot(stepsPerDay, aes(x = date, y = steps))

p <- g + geom_bar(stat="identity", position="identity") +
        labs(x = "Date [date]") +
        labs(y = expression("Steps taken per day")) +
        labs(title = "Steps taken per date")

print(p)

graphics.off()                          # help command
#######################
## Summary help code ##
summary(stepsPerDay)
dim(stepsPerDay)
anyNA(stepsPerDay)

#######################

## Calculate the mean and median of the total number of the steps taken
## each day.

## The mean of the total number of the steps taken each day is
meanTotalStepsPerDay <- mean(stepsPerDay$steps, na.rm = TRUE)
meanLabel <- paste("Mean =", meanTotalStepsPerDay, sep = " ")

## The median of the total number of the steps taken each day is
medianTotalStepsPerDay <- median(stepsPerDay$steps, na.rm = TRUE)
medianLabel <- paste("Median =", medianTotalStepsPerDay, sep = " ")

## The histogram si showed below, now with the mean and median.
g <- ggplot(stepsPerDay, aes(x = date, y = steps))

xrng <- stepsPerDay$date

p <- g + geom_bar(stat="identity", position="identity") +
    geom_hline(aes(yintercept = meanTotalStepsPerDay),
               colour = alpha("#990000", 0.5), linetype="dashed") +
    geom_hline(aes(yintercept = medianTotalStepsPerDay),
               colour = alpha("blue", 0.5), linetype="dashed") +
    labs(x = "Date [date]") +
    labs(y = expression("Steps taken per day")) +
    labs(title = "Steps taken per date") +
    geom_text(data=NULL, aes(x = xrng[26],  y = 17500,
              label = meanLabel), colour = alpha("#990000", 0.25),
              size=4, hjust = 0, vjust = 0) +
    geom_text(data=NULL, aes(x = xrng[26],  y = 16500,
              label = medianLabel), colour = alpha("blue", 0.25),
              size=4, hjust = 0, vjust = 0)

print(p)



graphics.off()                          # help command

######################################################################
### DataAnalysis_ActivityMonitoring_Devices.R ends here
