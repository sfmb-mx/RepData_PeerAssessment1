### DataAnalysis_ActivityMonitoring_Devices.R --- 
## 
## Filename: DataAnalysis_ActivityMonitoring_Devices.R
## Description: 
## Author: Sergio-Feliciano Mendoza-Barrera
## Maintainer: 
## Created: Wed Oct 15 07:22:12 2014 (-0500)
## Version: 
## Package-Requires: ()
## Last-Updated: Thu Oct 16 21:21:37 2014 (-0500)
##           By: Sergio-Feliciano Mendoza-Barrera
##     Update #: 263
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
## library(data.table)
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

######################################################################
## What is mean total number of steps taken per day?

## Calculating the number of steps per date:
stepsPerDay <- aggregate(deviceData$steps, list(deviceData$date), FUN = "sum")
colnames(stepsPerDay) <- c("date", "steps")

## The histogram si showed below,
g <- ggplot(stepsPerDay, aes(x = date, y = steps))

p <- g + geom_bar(stat="identity", position="identity") +
        labs(x = "Date [date]") +
        labs(y = expression("Steps taken per day")) +
        labs(title = "Steps taken per date")

print(p)
invisible(readline(prompt="Press [enter] to continue"))
graphics.off()                          # help command

######################################################################
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
               colour = alpha("dark red", 0.5), linetype="dashed") +
    geom_hline(aes(yintercept = medianTotalStepsPerDay),
               colour = alpha("red", 0.5), linetype="dashed") +
    labs(x = "Date [date]") +
    labs(y = expression("Steps taken per day")) +
    labs(title = "Steps taken per date") +
    geom_text(data=NULL, aes(x = xrng[26],  y = 17500,
              label = meanLabel), colour = alpha("dark red", 0.25),
              size=4, hjust = 0, vjust = 0) +
    geom_text(data=NULL, aes(x = xrng[26],  y = 16500,
              label = medianLabel), colour = alpha("red", 0.25),
              size=4, hjust = 0, vjust = 0)

print(p)
invisible(readline(prompt="Press [enter] to continue"))
graphics.off()                          # help command

######################################################################
## What is the average daily activity pattern?

## Calculating the average number of steps per date per time interval:

bad <- is.na(deviceData[, 1])
completeDeviceData <- deviceData[!bad, ]

intervalStepsPerDay <- aggregate(completeDeviceData$steps,
                                 list(completeDeviceData$interval),
                                 FUN = "mean")
colnames(intervalStepsPerDay) <- c("interval", "steps")

## The maximum number of steps for all intervals.
highest <- subset(intervalStepsPerDay, steps == max(steps))
maximumStepsLabel <- paste("Maximum: Steps =", highest$steps, ":",
                           "Interval =", highest$interval)
## Ploting the results
g <- ggplot(intervalStepsPerDay, aes(x = interval, y = steps))
p <- g + geom_point(color = "dark red") +
  geom_line(color = "dark red") +
    labs(x = "5 minutes interval [Minutes]") + 
    labs(y =
             expression("Average number of steps per interval across all days [Steps]")) +
    labs(title =
             " Average number of steps per interval across all days [Steps]") +
    geom_point(data = highest, size = 3, colour = alpha("green", 0.5)) +
    geom_text(data=NULL, aes(x = 835,  y = 210,
    label = maximumStepsLabel), colour = alpha("dark red", 0.25),
    size=4, hjust = 0, vjust = 0)

print(p)
invisible(readline(prompt="Press [enter] to continue"))
graphics.off()                          # help command

## 1. Calculate and report the total number of missing values in the
## dataset (i.e. the total number of rows with `NA`s)

numberRowsWithNA <- nrow(deviceData[!complete.cases(deviceData), ])

######################################################################
## 2. Devise a strategy for filling in all of the missing values in
## the dataset. The strategy does not need to be sophisticated. For
## example, you could use the mean/median for that day, or the mean
## for that 5-minute interval, etc.

## Explain the strategy.

## 3. Create a new dataset that is equal to the original dataset but
## with the missing data filled in.

newDeviceData <- deviceData

for (i in 1:nrow(newDeviceData)) {
    if (is.na(newDeviceData[i, 1])) {
        interval <- newDeviceData[i, 3]
        newDeviceData[i, 1] <- intervalStepsPerDay[intervalStepsPerDay$interval == interval, 2]
    }
}

## anyNA(newDeviceData$steps)

######################################################################
## 4. Make a histogram of the total number of steps taken each day and
## Calculate and report the **mean** and **median** total number of
## steps taken per day. Do these values differ from the estimates from
## the first part of the assignment? What is the impact of imputing
## missing data on the estimates of the total daily number of steps?
## The histogram si showed below, now with the mean and median.

## Calculating the number of steps per date:
newStepsPerDay <- aggregate(newDeviceData$steps, list(newDeviceData$date), FUN = "sum")
colnames(newStepsPerDay) <- c("date", "steps")

## The histogram si showed below,

g <- ggplot(newStepsPerDay, aes(x = date, y = steps))
xrng <- newStepsPerDay$date

p <- g + geom_bar(stat="identity", position="identity") +
    geom_hline(aes(yintercept = meanTotalStepsPerDay),
               colour = alpha("dark red", 0.5), linetype="dashed") +
    geom_hline(aes(yintercept = medianTotalStepsPerDay),
               colour = alpha("red", 0.5), linetype="dashed") +
    labs(x = "Date [date]") +
    labs(y = expression("Steps taken per day")) +
    labs(title = "Steps taken per date") +
    geom_text(data=NULL, aes(x = xrng[26],  y = 17500,
              label = meanLabel), colour = alpha("dark red", 0.25),
              size=4, hjust = 0, vjust = 0) +
    geom_text(data=NULL, aes(x = xrng[26],  y = 16500,
              label = medianLabel), colour = alpha("red", 0.25),
              size=4, hjust = 0, vjust = 0)


print(p)
invisible(readline(prompt="Press [enter] to continue"))
graphics.off()                          # help command

######################################################################
### DataAnalysis_ActivityMonitoring_Devices.R ends here
