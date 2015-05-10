## Generating histogram for plot 1 for the Course Project 1 in Coursera course Exploratory Data Analysys

## For a best performance is verified if data.table packet is installed in order 
## to use data.table functions to get the data in a fast way
useDataTable <- FALSE
if("data.table" %in% rownames(installed.packages()) == TRUE) useDataTable <- TRUE;

## if data.table is installed, load library
if (useDataTable)
	library(data.table)

########################################
## gen_plot1 <- This is the function to use to generate histogram for plot 1
## partial_read parameter is the data read to use
gen_plot1 <- function(partial_read) {
	## Generate the histogram as required: read Global_active_power column,  
	## col = red -> color red for bars, xlab ="Global Active Power (kilowats)" -> Label for x axis in the graph
	## main ="Global Active Power" -> label for histogram title, ylim = c(0,1200)-> limits for y axis, 
	## las = 1 -> change orientation of the y label in the plot
	hist(as.numeric(partial_read$Global_active_power), col = "red",xlab ="Global Active Power (kilowatts)",  
	     main ="Global Active Power", ylim = c(0,1200), las = 1)
}

########################################
## Doing the process to generate plot: getting data, doing plot and saving it to a file

## Setting column names to use for the data
col_names <- c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity",
               "Sub_metering_1","Sub_metering_2","Sub_metering_3")


## Read the data for the indicated dates "2007/02/01" & "2007/02/02" using a subset in the given .txt file
## Use the fast function fread if data.table is installed
if (useDataTable){
	partial_read <- suppressWarnings(subset(fread("household_power_consumption.txt",colClasses ="character",
				               sep =";", na.strings = "?"),
                                       	as.Date(Date,"%d/%m/%Y") == as.Date("2007/02/01","%Y/%m/%d") 
                                       	| as.Date(Date,"%d/%m/%Y") == as.Date("2007/02/02","%Y/%m/%d"),  
                                       	select = col_names))
} else {
	partial_read <- subset(read.table("household_power_consumption.txt",colClasses ="character", comment.char ="",
                                   	   sep =";", na.strings = "?", col.names = col_names),
                       		as.Date(Date,"%d/%m/%Y") == as.Date("2007/02/01","%Y/%m/%d") 
                       		| as.Date(Date,"%d/%m/%Y") == as.Date("2007/02/02","%Y/%m/%d"),  
                       		select = col_names)
}

## Creating file  with the characteristics indicated in assigment t osave the histogram created
png(filename = "plot1.png",  width = 480, height = 480, units = "px", bg = "white")

## Using gen_plot1 function above created to generate the plot
gen_plot1(partial_read)

## Closing the file device with the plot generated
dev.off()

