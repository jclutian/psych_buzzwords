# Josua Lutian 
# Jan 10, 2019 
# Clean up dataset to get first Names

#PRE CLEAN UP 
# made all empties into "{'EMPTY':'EMPTY'}
# All last names with '; deleted the '
# Women's Men's King's Luke's Mary's O'R O'B O'K O'S O'C

#TODO: change it so that only the unique LOCATIONS are in to lessen the API requests. 
library(readxl)
library(tidyr)
library(tidyverse)

namesIns <- read.csv("Paper+Dataset.csv", stringsAsFactors= FALSE)

dat <- namesIns$`Author.Institution_All`
merge <- NULL
paper <- NULL
authors <- NULL

for (i in 1:nrow(namesIns)) {
  #w0 <- substr(dat[i], 2, nchar(dat[i])-1)
  w0 <- as.character(dat[i])
  w1 <- as.data.frame (strsplit (w0,";")[[1]])
  temp <- vector(mode = "character")
  colnames(w1) <- "info"
  for (j in 1:nrow(w1)){
    temp <- append(temp,as.character(w1$info[j]))
  }
  ins <- vector(mode = "character")
  auts <- vector (mode = "character")
  paperNum <- vector (mode = "integer")
  year<- vector (mode = "integer")
  for (k in 1:length (temp)){
    if (length(temp) == 1){
      auts <- append (auts,temp[k])
      ins <- append (ins,"empty")
    }
    else{
      if (k %% 2 == 0){ 
        ins <- append (ins,temp[k])
      }
      else{
        auts <- append (auts, temp[k])
      }
    }
  }
  paperNum <- append (paperNum,i)
  year <- append (year,namesIns$Year[i] )
  paper <- cbind (year,paperNum, auts,ins)
  merge <- rbind (merge,paper)
  authors <- rbind(authors, cbind (auts, paperNum, year))
}

write.csv(merge, file = "JPSP2019+NEWEST+IPROMISE+Authors+Institutions.csv")
################################################################################################
#split the names into first names and last names.
#take out the non-unique names 
authors <- as.data.frame(authors)

authors <- authors %>%
  group_by(paperNum) %>%
  mutate (row = row_number())
tempNames <- NULL

#write.csv (authors, file = "JPSP+Paper+Names.csv")

#Take out the isolate the first names 
for (i in 1: length(authors$auts)){
  #print (typeof(authors$auts[i])) }
  name <- as.character( authors$auts[i])
  splitName <- unlist (strsplit(name,"\\s"))
  #splitName <- unlist (strsplit(splitName," "))[1]
  #if the split name is 2 
  
  #CASE 1: IF the first name is just an initial.
  #print (splitName)
  if (length(splitName) == 0){
    noName <- "NoName"
    tempNames <- rbind (tempNames,noName)
  }
  else if ( grepl("\\.",splitName[1]) & nchar(splitName[1]) <=2 ) {
    #print (splitName[2])
    tempNames <- rbind (tempNames,splitName[2])
  }
  #if there is a space in front of the name 
  else if (nchar(splitName[1]) == 0){
    #print (splitName)
    tempNames <- rbind (tempNames,splitName[2])
  }
  #if its formatted as lastname, first name 
  else if (grepl ("\\,",splitName[1])){
    tempNames <- rbind (tempNames,splitName[2])
    #this is because the first part is the LAST NAME!!!
  }
  else{
    tempNames <- rbind(tempNames,splitName[1] )
  }
}
View(tempNames)

colnames (tempNames)<- "firstName"
authors1 <- data.frame (authors, tempNames)

write.csv (authors1, file = "+Paper+Names.csv")














