##Main script or "main portion of code"
run_analysis<-function(){
  
  ##loading library
  library(data.table)
  
  ##Geting X Data and adding descriptive columns
  trainDT<-GetXDataWithColHeaders("train")
  testDT<-GetXDataWithColHeaders("test")
  
  ##merging X Data as rows
  mergedtables<-rbind(trainDT, testDT)
  
  ##Getting Subject ID data
  trainSubject<-GetSubjectIDData("train")
  testSubject<-GetSubjectIDData("test")
  
  ##combining Subject ID Rows 
  mergedSubject<-rbind(trainSubject,testSubject)
  
  
  #mergining with X data
  mergedtables<-cbind(mergedtables,mergedSubject)
  
  ##Getting Y data and adding column headers
  trainDT<-GetYDataWithColHeaders("train")
  testDT<-GetYDataWithColHeaders("test")
  
  ##combining Y data data
  mergedActivities<-rbind(trainDT, testDT)
  
  ##Pulling this data back out to change data to be more descriptive
  ActivityAsStrings<-ActivityString(mergedActivities$Activity.Label)
  
  #putting the more descriptive activity data back into the datatable
  mergedActivities[,Activity.Label:=ActivityAsStrings]
  
  #Adding activity data to the merged table (which has X and Y data)
  mergedtables<-cbind(mergedtables,mergedActivities)
  
  ##returning the tidy set
  mergedtables
  
}





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

