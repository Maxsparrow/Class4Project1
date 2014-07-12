plot4 <- function() {
    ##Downloads the data if it doesn't already exist
    if(!file.exists("household_power_consumption.txt")) {
        url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        download.file(url,"data.zip")
        unzip("data.zip")
    }
    
    ##Loads the data into R, but we only use 2 dates, so the data for those dates is pulled out, 
    ##and other data is removed to save RAM    
    data <- read.table("household_power_consumption.txt",header=TRUE,sep=";")
    date1<-data[data[,1]=="1/2/2007",]
    date2<-data[data[,1]=="2/2/2007",]
    finaldata<-rbind(date1,date2)
    rm(data,date1,date2)
    
    ##Adds the time column to the date column to improve table layout
    finaldata[,1]<-paste(finaldata[,1],finaldata[,2])
    finaldata<-finaldata[,c(1,3:9)]
    
    ##Reformats the columns to correct class type
    for (i in c(1:8)) {finaldata[,i]<-as.character(finaldata[,i])}    
    for (i in c(2:8)) {finaldata[,i]<-as.numeric(finaldata[,i])}
    
    ##Reformats first column to POSIXct for date and time
    datetime<-strptime(finaldata[,1],format = "%d/%m/%Y %H:%M:%S")
    finaldata<-cbind(datetime,finaldata)
    finaldata<-finaldata[,c(1,3:9)]
    
    ##Creates plot4 - 4 plots shown together, if it hasn't been created already
    if(!file.exists("plot4.png")) {
        png("plot4.png")
        
        ##sets devce to show 4 plots at once
        par(mfcol=c(2,2))
        
        with(finaldata,{
            ##first plot for global active power line graph
            plot(datetime,Global_active_power,ylab="Global Active Power",xlab="",type="n")
            lines(datetime,Global_active_power)
            
            ##second plot for energy sub metering
            plot(datetime,Sub_metering_1,ylab="Energy sub metering",xlab="",type="n")
            lines(datetime,Sub_metering_1)
            lines(datetime,Sub_metering_2,col="red")
            lines(datetime,Sub_metering_3,col="blue")
            legend("topright",legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col = c("black","red","blue"),lty="solid",bty="n")
            
            ##Creates voltage plot
            plot(datetime,Voltage,type="n")
            lines(datetime,Voltage)
            
            ##Creates global reactive power line plot
            plot(datetime,Global_reactive_power,type="n")
            lines(datetime,Global_reactive_power)
        })
        
        dev.off()
    } else {print("plot4.png exists")}
    
}
