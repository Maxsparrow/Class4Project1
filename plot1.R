plot1 <- function() {
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
    
    ##Creates plot1 - red histogram if it hasn't been created already
    if(!file.exists("plot1.png")) {
        png("plot1.png")
        
        hist(finaldata$Global_active_power,col="red",main="Global Active Power", xlab="Global Active Power (kilowatts)")
        
        dev.off()
    } else {print("plot1.png exists")}    
}

