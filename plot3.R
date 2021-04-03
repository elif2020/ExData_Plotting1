library(data.table)

#open connection to the data file
con<-file("household_power_consumption.txt","r")

#open a data frame with the same column names
varnames<-scan(con,what="",nlines=1,sep=";",quote="\"",)
powerdata<-data.frame(matrix(ncol=length(varnames),nrow=0))
colnames(powerdata)<-varnames

#set a dummy variable to exit after 2/2/2007
dummy<-FALSE

#read necessary lines
while(TRUE){
  line<-readLines(con,1)
  if(length(line)==0){
    break
  }
  else if(grepl("^1/2/2007|^2/2/2007",line)){
    line<-as.data.frame(strsplit(line,";"))
    line<-transpose(line)
    colnames(line)<-varnames
    powerdata<-rbind(powerdata,line) 
    dummy<-TRUE
  }
  else if(dummy==TRUE){
    break
  }
}

#close connection
close(con)

#paste Date and Time together
powerdata$Time<-do.call(paste,powerdata[,1:2])

#change class of Date/Time into POSIXlt
powerdata$Time<-strptime(powerdata$Time,format="%d/%m/%Y %H:%M:%OS")

#change the classes of other variables to numeric
powerdata[,3:length(varnames)]<-lapply(powerdata[,3:length(varnames)],
                                       as.numeric) 

#plot Submetering of three types over time 
png("plot3.png")
plot(powerdata$Time,powerdata$Sub_metering_1,type="l",
                    xlab="",ylab="Energy sub metering")
lines(powerdata$Time,powerdata$Sub_metering_2,type="l",col="red")
lines(powerdata$Time,powerdata$Sub_metering_3,type="l",col="blue")
legend("topright",lty=c(1,1,1),col=c("black","red","blue"),
       legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))

#close the graphics device 
dev.off()