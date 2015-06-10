ErrorCount <- function (directory, searchString) {
#Set Dir to current directory so that it can be reset after data is captured
Dir<-getwd()

#set working directory to the input directory
setwd(directory)

#Pass the file names to list files
files <- list.files(directory)

#Identify end value used in for loop
length <- length(files)

#Initialize and set constants
totalReprime <-c()
totalRaw <-c()
filtersReprimed<-c()
ERR<-"ERR"
IND<-"IND"

#Perform logic clauses to identify if searching for an Error code of an Indeterminate. 
x<-charmatch(ERR, searchString, nomatch = 0)
y<-charmatch(IND, searchString, nomatch = 0)

#Processing for Error code search string
if (x == 1){
	ErrorString<-searchString
	for(i in 1:length){
	
	#Pass .csv files to data.frames from 1 to the end of list.files
	data<- data.frame(read.csv(files[i]))
	dataA<-data
	dataB<-data
	
	#order the data in ascending Time, this puts lanes in order and places reprimed filters in order. 
	data1<-dataA[with(dataA, order(Time)),]
	data2<-dataB[with(dataB, order(Time)),]
	
	rows<-nrow(data) #Counts rows to use as indexes for logic 
	#counts values matching ErrorString for both the raw and reprimed filter sets
	countRaw<- sum(sapply(data2[1:24,], match, ErrorString, nomatch=0))
	totalRaw<-c(totalRaw, countRaw) 
	
		if((rows-24)>0){
			countReprime<- sum(sapply(data1[25:rows,], match, ErrorString, nomatch=0))
			totalReprime <- c(totalReprime, countReprime)
			reprimes<-rows-24 #math to identify how many filters were reprimed.  Subtracting the total rows from standard 24 filters + heading
			filtersReprimed<-c(filtersReprimed,reprimes)
			}
	}
	overallRaw<-sum(totalRaw)
	overallReprime<-sum(totalReprime)
	setwd(Dir)

	#divide totals by 3: Errors are present in triplicate for a single filter.  Dividing by 3 represents how many filters threw the error. 
	finalRaw<-overallRaw / 3
	finalReprime<-overallReprime/3
	total_filters_reprimed<-sum(filtersReprimed)
	sprintf("Error Code: %s   Raw count: %03d   Re-Prime count: %03d   Filters Re-primed: %04d",ErrorString, finalRaw, finalReprime,total_filters_reprimed)
	
} else if(y == 1){
	INDString<-searchString
	for(i in 1:length){
	
	data<- data.frame(read.csv(files[i]))
	dataA<-data
	dataB<-data
	
	data1<-dataA[with(dataA, order(Time)),]
	data2<-dataB[with(dataB, order(Time)),]
	
	rows<-nrow(data) #Counts rows to use as indexes for logic 
	#counts values matching ErrorString for both the raw and reprimed filter sets
	countRaw<- sum(sapply(data2[1:24,], match, INDString, nomatch=0))
	totalRaw<-c(totalRaw, countRaw) 
	
		if((rows-24)>0){
			countReprime<- sum(sapply(data1[24:rows,], match, INDString, nomatch=0))
			totalReprime <- c(totalReprime, countReprime)
			reprimes<-rows-24 #math to identify how many filters were reprimed.  Subtracting the total rows from standard 24 filters + heading
			filtersReprimed<-c(filtersReprimed,reprimes)
			}
	}
	#No division is performed.  Indeterminate calls are made on a per filter and per target basis. 
	overallRaw<-sum(totalRaw)
	overallReprime<-sum(totalReprime)
	total_filters_reprimed<-sum(filtersReprimed)
	setwd(Dir) 
	sprintf("IND Code: %s   Raw count: %03d   Re-Prime count: %03d   Filters Re-primed: %04d",INDString, overallRaw, overallReprime,total_filters_reprimed)
} else {
	setwd(Dir)
	print("Function failure")
}
}
