#Create a dataset from a directory of text files

createDataSet <- function(directory) {
  orig_wd <- getwd()
  setwd(directory)
  
  #create a blank data set with a dummy row
  txt <- NA
  file <- NA
  dataset <- data.frame(txt, file)
  
  #Get a list of files in the directory
  file_list <- list.files()
  
  #Merge the content from all files into a single data frame
  for (file in file_list){
    
    #read paragraphs and file name from the current file into a dataframe
    txt <- readLines(file)
    txt.df <- data.frame(txt, file)
    
    #remove the blankspaces
    txt.df[txt.df==""]<-NA
    txt.df <- txt.df[complete.cases(txt.df),]
    
    #append contents from the current file into the return results
    dataset<-rbind(dataset, txt.df)
    
  }
  
  #Remove the first dummy row from the data set
  txt <- dataset[-1,]
  dataset <- as.data.frame(txt)
  
  #Format the text in the dataset
  dataset$txt <- as.character(dataset$txt)
  
  setwd(orig_wd)
  
  return(dataset)
  
}