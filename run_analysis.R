##Take "features.txt" which is the column names for the 561 features for each record
##Space delimited is an appropriate format
##Remove the first column which is row numbers the next column is the row number


FeatureColNames<-function(){
  
  library(data.table)
  features<-read.table(paste0(getwd(),"/UCI HAR Dataset/features.txt"),
                      col.names = c("Row Number","Feature Names")
                       )
  ##features into data table
  DT<-data.table(features)
  
  #returning vector which is the feature names
  DT$Feature.Names
}



##Next take the "X_test.txt" in either train folder or test folder
##Create a data table from the data and add column names

GetXDataWithColHeaders<-function(trainOrTest="train"){
  
  featurenames<-FeatureColNames()
  
  xtest<-read.table(paste0(getwd(),paste0("/UCI HAR Dataset/",trainOrTest, "/X_",trainOrTest,".txt")),
                    col.names=featurenames
                    )
  returnedDT<-data.table(xtest)
  
  ##returning the data table
  returnedDT
  
}


##Next take the "Inertial Signals" in either train folder or test folder
##Create a data table from the data and add column names

GetInertialSignals<-function(trainOrTest="train", file="body_acc_x"){
  
  #featurenames<-FeatureColNames()
  
  xtest<-read.table(paste0(getwd(),paste0("/UCI HAR Dataset/",trainOrTest,"/Inertial Signals/",file,"_" ,trainOrTest,".txt"))#,
                    #col.names=file
  )
  returnedDT<-data.table(xtest)
  
  ##returning the data table
  returnedDT
  
}








##Take "subject_test.txt" train or test and create a datatable from it
##Manually create column name which is "Test_Subject_ID"

GetSubjectIDData<-function(trainOrTest="train")
{
  
  subjectIDs<-read.table(paste0(getwd(),paste0("/UCI HAR Dataset/",trainOrTest, "/subject_",trainOrTest,".txt")),
                    col.names="ID Test Subject"
  )
  
  DT<-data.table(subjectIDs)
  DT
  
  
  
}




##Take Y data and create a datatable from it
##Manually create a column name which is "Activity_Index" 


GetYDataWithColHeaders<-function(trainOrTest="train"){
  
  featurenames<-FeatureColNames()
  
  ytest<-read.table(paste0(getwd(),paste0("/UCI HAR Dataset/",trainOrTest, "/Y_",trainOrTest,".txt")),
                    col.names="Activity Label"
  )
  returnedDT<-data.table(ytest)
  
  ##returning the data table
  returnedDT
  
}


##Replaces Activity.Label with Activity string
ActivityString<-function(x)
{

labelsvector<-NULL;
#x<-as.integer(x)
for(ActivityLabel in x){
  
  #print(ActivityLabel)
  
  if(ActivityLabel==1){
    label<-"WALKING"
  }
  else if(ActivityLabel==2){
    label<-"WALKING_UPSTAIRS"
  }
  else if(ActivityLabel==3){
    label<-"WALKING_DOWNSTAIRS"
  }
  else if(ActivityLabel==4){
    label<-"SITTING"
  }
  else if(ActivityLabel==5){
    label<-"STANDING"
  }
  else if(ActivityLabel==6){
    label<-"LAYING"
  }
  else{
    label<-NA
  }
  
  labelsvector<-append(labelsvector,label)
  
}

labelsvector
  
  
}






##Main data logic 

##Get training set

cleandata<-function(){
  library(data.table)
trainDT<-GetXDataWithColHeaders("train")
testDT<-GetXDataWithColHeaders("test")
mergedtables<-rbind(trainDT, testDT)
#mergedtables
trainSubject<-GetSubjectIDData("train")
testSubject<-GetSubjectIDData("test")
mergedSubject<-rbind(trainSubject,testSubject)
mergedtables<-cbind(mergedtables,mergedSubject)
mergedtables

trainDT<-GetYDataWithColHeaders("train")
testDT<-GetYDataWithColHeaders("test")
mergedActivities<-rbind(trainDT, testDT)


ActivityAsStrings<-ActivityString(mergedActivities$Activity.Label)
mergedActivities[,Activity.Label:=ActivityAsStrings]

#testreturn<-GetInertialSignals("train", "body_acc_x")

mergedtables<-cbind(mergedtables,mergedActivities)

mergedNames<-colnames(mergedtables)

measuredmean<-NULL
for(name in mergedNames)
{
  (isMean<-grep("mean",name, fixed=FALSE))
  (isStd<-grep("std",name, fixed=FALSE))
  
  if(     (!(is.na(isMean[1]))) || (!(is.na(isStd[1])))    ){
    
    measuredmean<-append(measuredmean,name)
    
  }
  

 
}

measuredmeanNon<-append(measuredmean, "Activity.Label")
#measuredmeanNon<-append(measuredmeanNon, "ID.Test.Subject")
measuredMeansNoActivities<-append(measuredmean, "ID.Test.Subject")
#measuredmean






meanstdtables<-mergedtables[,measuredmeanNon, with=FALSE]
meanstdtablesNoIDs<-mergedtables[,measuredMeansNoActivities, with=FALSE]

mean.summarized<-meanstdtables[,lapply(.SD, mean), by ="Activity.Label"]
#mean.summarized2<-meanstdtables[,lapply(.SD, mean), by ="ID.Test.Subject"]
write.table(mean.summarized, "byActivity.txt", row.names=FALSE)

mean.summarized2<-meanstdtablesNoIDs[,lapply(.SD, mean), by ="ID.Test.Subject"]
write.table(mean.summarized2, "bySubjectID.txt", row.names=FALSE)


}