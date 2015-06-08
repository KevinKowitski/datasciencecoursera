pollutantmean <- function(directory, pollutant, id){
currentDir<-getwd()
specdata<-"specdata"
newDir<-paste(currentDir, specdata, sep="/")
setwd(newDir)
files<-list.files(newDir)
total<- 0
sulfate<-"sulfate"
nitrate<-"nitrate"

if (!is.na(pmatch(sulfate, pollutant))==TRUE) {
	for(i in id[1]:tail(id, 1)){
		file<-read.csv(files[i])
		file_short <- subset(file, is.na(sulfate)<1, is.na(nitrate)<1)
		select<-file_short[,2]
		total <- c(total, select)	
	}
	
 }else if (!is.na(pmatch(nitrate, pollutant))==TRUE) {
	for(i in id[1]:tail(id, 1)){
		file<-read.csv(files[i])
		file_short <- subset(file, is.na(sulfate)<1, is.na(nitrate)<1)
		select<-file_short[,3]
		total <- c(total, select)	
		
	}
	
} else {
 print("Function failed to execute")
}
mean(total)
setwd(currentDir)
sprintf("Done!")
}
