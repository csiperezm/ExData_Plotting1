## Generating graph for plot 4 for the Course Project 1 in Coursera course Exploratory Data Analysys

## For a best performance is verified if data.table packet is installed in order 
## to use data.table functions to get the data in a fast way
useDataTable <- FALSE
if("data.table" %in% rownames(installed.packages()) == TRUE) useDataTable <- TRUE;

## if data.table is installed, load library
if (useDataTable)
	library(data.table)

########################################
## gen_plot2 <- This is the function to use to generate the plot 2 graph
## partial_read parameter is the data read to use, pos_days are the locations for the
## x-axis labels according to days associated with data
gen_plot2 <- function(partial_read,pos_days) {
	## Doing the correspondant plot to obtain the graph and setting the label indicated for y-axis (ylab), 
	## x-axis blank (xlab), type = "l" -> type as lines, axes = F -> set false to not set axis when plot is created
	## ann = T -> set true to handle axis titles
	plot(partial_read$Global_active_power, type="l", axes=F, ann=T, ylab="Global Active Power", xlab = "")
	
	## Set x axis with the first location of the indicated days as obtained
	axis(1, labels = c("Thu","Fri","Sat"), at= pos_days)

	## Set y axis with the specified values 
	axis(2, labels = c(0,2,4,6), at=c(0,2,4,6))

	## Create a box around the plot
	box()
}

########################################
## gen_plot3 <- This is the function to use to generate the plot 3 graph
## partial_read parameter is the data read to use, pos_days are the locations for the
## x-axis labels according to days associated with data
gen_plot3 <- function(partial_read,pos_days) {

	## setting the plot color to use for the graph
	plot_colors <- c("black", "red", "blue")

	## Doing the correspondant plots to obtain the graph and setting the label indicated for y-axis (ylab), 
	## x-axis blank (xlab), type = "l" -> type as lines, axes = F -> set false to not set axis when plot is created
	## ann = T -> set true to handle axis titles, col -> correspondant color to use for the specific plot (black default)

	# Graph Sub_metering_1 with default color black
	plot(partial_read$Sub_metering_1, type="l", axes=F, ann=T, ylab="Energy sub metering", , xlab = "")

	# Graph Sub_metering_2 with red 
	lines(partial_read$Sub_metering_2, type="l",ann=F, col=plot_colors[2])

	# Graph Sub_metering_3 with blue line
	lines(partial_read$Sub_metering_3, type="l",ann=F, col=plot_colors[3])

	## Set x axis with the first location of the indicated days as obtained
	axis(1, labels = c("Thu","Fri","Sat"), at= pos_days)

	## Set y axis with the specified values 
	axis(2, labels = c(0,10,20,30), at=c(0,10,20,30))

	## Create a box around the plot
	box()

	## Setting the legend for the graph using colnames and plot_colors
	legend("topright", colnames(partial_read)[7:9], cex=0.8, col=plot_colors, lty=1)
}

########################################
## gen_plot5 <- This is the function to use to generate the plot 5 graph corresponding
## to second plot in the first row in the plot 4 generation of 4 plots
## partial_read parameter is the data read to use, pos_days are the locations for the
## x-axis labels according to days associated with data
gen_plot5 <- function(partial_read,pos_days) {
	## Doing the correspondant plot to obtain the graph and setting the label indicated for y-axis (ylab), 
	## x-axis blank (xlab), type = "l" -> type as lines, axes = F -> set false to not set axis when plot is created
	## ann = T -> set true to handle axis titles
	plot(partial_read$Voltage, type="l", axes=F, ann=T, ylab="Voltage", xlab = "datetime")
	
	## Set x axis with the first location of the indicated days as obtained
	axis(1, labels = c("Thu","Fri","Sat"), at= pos_days)

	## Set y axis with the specified values 
	axis(2, labels = c(234,238,242,246), at=c(234,238,242,246))

	## Create a box around the plot
	box()
}

########################################
## gen_plot6 <- This is the function to use to generate the plot 5 graph corresponding
## to second plot in thesecond row in the plot 4 generation of 4 plots
## partial_read parameter is the data read to use, pos_days are the locations for the
## x-axis labels according to days associated with data
gen_plot6 <- function(partial_read,pos_days) {
	## Doing the correspondant plot to obtain the graph and setting the label indicated for y-axis (ylab), 
	## x-axis blank (xlab), type = "l" -> type as lines, axes = F -> set false to not set axis when plot is created
	## ann = T -> set true to handle axis titles
	plot(partial_read$Global_reactive_power, type="l", axes=F, ann=T, ylab="Global_reactive_power", xlab = "datetime")
	
	## Set x axis with the first location of the indicated days as obtained
	axis(1, labels = c("Thu","Fri","Sat"), at= pos_days)

	## Set y axis with the specified values 
	axis(2, labels = c("0.0","0.1","0.2","0.3","0.4","0.5"), at=c(0.0,0.1,0.2,0.3,0.4,0.5))

	## Create a box around the plot
	box()
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

## Settig the values to locate the x-axis labels indicated ("Thu","Fri","Sat")
## First location in the results of the day Thursday (value 4 in format day in %w)
## as 01/02/2007 was a thursday
pos_day1 <- match(4,format(as.Date(partial_read$Date,"%d/%m/%Y"),"%w"))

## First location in the results of the day Friday (value 5 in format day in %w)
## as 02/02/2007 was a friday
pos_day2 <- match(5,format(as.Date(partial_read$Date,"%d/%m/%Y"),"%w"))
	
## As just two days are read, then saturday is not in the results, so put it after last position
## in results (nrow + 1)
pos_day3 <- nrow(partial_read) + 1


## Creating file  with the characteristics indicated in assigment
png(filename = "plot4.png",  width = 480, height = 480, units = "px", bg = "white")

## Setting the quantity of plots to show
par(mfrow =c(2,2))

## Using gen_plot3 function above created to generate the plot
with (partial_read,{
    gen_plot2(partial_read,c(pos_day1,pos_day2,pos_day3))
    gen_plot5(partial_read,c(pos_day1,pos_day2,pos_day3))
    gen_plot3(partial_read,c(pos_day1,pos_day2,pos_day3))
    gen_plot6(partial_read,c(pos_day1,pos_day2,pos_day3))
})

## Closing the file device with the plot generated
dev.off()
