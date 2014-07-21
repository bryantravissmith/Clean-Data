
features <- read.fwf('features.txt',widths=c(100),as.is=TRUE)
fnum = numeric()
fname = character()
j = 1
for (i in 1:length(features[,1])){
        if(  length(grep("mean|std",features[i,1])) >= 1){
                temp <- strsplit(sub("^\\s","",features[i,1])," ")
                fnum[j] = as.numeric(temp[[1]][1])
                fname[j] = sub("\\(\\)","",temp[[1]][2])
                j = j + 1
        }

}
featuresDf = data.frame(col=fnum,name=fname)



x_train = read.table("./train/X_train.txt",header=FALSE,as.is=T)
x_test = read.table("./test/X_test.txt",header=FALSE,as.is=T)
xData <- rbind(x_train,x_test)

xData <- xData[,featuresDf$col]
colnames(xData) <- featuresDf$name

y_train = read.csv("./train/y_train.txt",header=FALSE,as.is=T)
y_test = read.csv("./test/y_test.txt",header=FALSE,as.is=T)
yData <- rbind(y_train,y_test)

subject_train = read.csv("./train/subject_train.txt",header=FALSE, as.is=T)
subject_test = read.csv("./test/subject_test.txt",header=FALSE,as.is=T)
subjectData <- rbind(subject_train,subject_test)

rm(features);rm(subject_test);rm(subject_train);rm(x_test);rm(x_train);rm(y_train);rm(y_test);

activity <- read.table("./activity_labels.txt",header=FALSE,as.is=T)

xData$activity = rep("",length(xData[,1]))
xData$subject = rep(0,length(xData[,1]))
for( i in 1:length(yData[,1])){
        xData$activity[i] = activity[activity[,1]==yData[i,1],2]
        xData$subject[i] = subjectData[i,1]
}
xData$activity <- factor(xData$activity)
rm(activity)

mat <- matrix(,ncol=length(featuresDf$name)+2)
for (subject in 1:30){
        for (level in levels(xData$activity)){
                data <- xData[xData$subject==subject & xData$activity==level,]
                row = c(apply(data[,featuresDf$name],2,mean),level,subject)
                mat <- rbind(mat,row)
        }
}


newNames = c(paste("mean(",featuresDf$name,")"),"activity","subject")
df = data.frame(mat)
colnames(df) <- newNames
write.csv(df,file="tidy.txt")
