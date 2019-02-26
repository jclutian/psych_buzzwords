# Josua Lutian  
# February 25, 2019 
# Clean up coordinates 

# Categorize each paper as American, Collobration, or International 
# Calculate the statistics for each demographic 
# Reform dataset to be departure and arrival points. 


library (dplyr)
newMapData <- read.csv("./+COORDS.csv",fill =TRUE)

# CHECK WHICH ONES ARE FROM AMERICA
addresses <- newMapData$address 
collaboration <- matrix(0,ncol = 1, nrow = length(addresses))
colnames(collaboration) <- "col"

for (i in 1: length(newMapData$paperNum) ){
  if (grepl("USA",addresses[i]) | grepl ("United States", addresses[i])){
    #print ("AMERICA!")
    collaboration[i] <- 1 
  }
  else{
    #print ("Not America!")
    collaboration[i] <- 0 
  }
}
newMapData<- cbind (newMapData,collaboration)

#this counts the number of 1's and 0's within each paper 
newMapDataCounts <- newMapData %>% 
  count(paperNum,col)

#isolate the paperNum of the ones with 
# THERE NO ONES 1's = Pure International
# ALL 1's = Pure American 
# 0 AND 1  COLLABORATION
zeroList = vector()
oneList = vector()
for (j in 1:length(newMapDataCounts$paperNum)){
  if (newMapDataCounts$col[j] == 0)
    zeroList <- append(zeroList, newMapDataCounts$paperNum[j])
  else if (newMapDataCounts$col[j] == 1){
    oneList <- append(oneList, newMapDataCounts$paperNum[j])
  }
}

#Find the indices/papernums of all the categories 
#Collaboration 
collab <- intersect (zeroList, oneList)
#View (collab)
#Pure International 
pureInt <- setdiff(zeroList, collab)
#View (pureInt)
#Pure USA 
pureUS <- setdiff(oneList, collab)
#View (pureUS)

paperType <- matrix(0,ncol = 1, nrow = length(addresses))
for (idx in which(newMapData$paperNum %in% collab)){
  paperType[idx] <- "collaboration"
}
for (idx in which(newMapData$paperNum %in% pureInt)){
  print(idx)
  paperType[idx] <- "international"
}
for (idx in which(newMapData$paperNum %in% pureUS)){
  print (idx)
 paperType[idx] <- "american"
}


newMapData <- cbind(newMapData,paperType)
write.csv(newMapData, file = ".csv")

############################################################################################
#Counts for demographics 
demoData <- read.csv("./.csv",fill =TRUE)

#this is for full version
demoCounts <- demoData %>%
  group_by (year) %>%
  mutate(totalPaperPerYear = n())%>%
  ungroup()%>%
  group_by(year,paperType) %>%
  mutate (totalTypePerYear = n(), collabRate = n()/totalPaperPerYear)

write.csv(demoCounts, file ="+FULL+DEMO.csv")


#this is for summary
demoCounts <- demoData %>%
  group_by (year,paperType) %>%
  count(year) %>%
  ungroup() 

totalPaperPerYear <- aggregate(demoCounts$n, by=list(year=demoCounts$year), FUN=sum)


total <- matrix(0,ncol = 1, nrow = length(demoCounts$year))

for (j in 1:length(demoCounts$year) ){
  for ( i in 1:length(totalPaperPerYear$year)){
    if (demoCounts$year[j] ==  totalPaperPerYear$year[i]) {
      print (totalPaperPerYear$x[i])
      demoCounts$total[j] <- totalPaperPerYear$x[i]
    }
  }
}

write.csv(demoCounts, file ="+SUMMARY+DEMO.csv")

###########################################################################################
#NEXT STEP IS TO MAKE IT SO THAT THERE IS A DEPARTURE and an ARRIVAL 
#check if there is more than one place per paper 

#this splits the dataset into institutions
newMapDataSplitPlaces <- newMapData %>% 
  count(paperNum,ins, lat, lng,year) %>%
  filter (lat != 0 & ins != " " & ins != ",")


#take out the ones that have 1 institution
newMapDataSplitPlacesCounts <- newMapDataSplitPlaces %>% 
  count(paperNum)

#isolate the solo papers and the multipapers
ones <- newMapDataSplitPlacesCounts %>%
  filter (nn == 1)
multiples <- newMapDataSplitPlacesCounts %>%
  filter (nn > 1)

colnames(multiples) <- c("paperNum", "total") 
colnames(ones) <- c("paperNum", "total") 

#This is for papers written in one institution 
onesSplit <- newMapDataSplitPlaces %>%
  filter (paperNum %in% ones$paperNum, ins != " ") %>%
  group_by(paperNum) %>% 
  add_count() %>%
  mutate (rowNum = row_number()) 

colnames(onesSplit)[6] <- "totalAuts"
colnames(onesSplit)[7] <- "totalIns"


together3 <- cbind (onesSplit,onesSplit)

#This is for papers written with more than 1 institution  
depart <- newMapDataSplitPlaces %>%
  filter (paperNum %in% multiples$paperNum, ins != " ") %>%
  group_by(paperNum) %>% 
  add_count() %>%
  mutate (rowNum = row_number()) 

colnames(depart)[6] <- "totalAuts"
colnames(depart)[7] <- "totalIns"

#THIS IS FOR ONES THAT HAVE MORE THAT 2 ins
departures0 <- depart %>%
  filter(totalIns > 2) %>%
  group_by(paperNum) %>%
  filter(rowNum < totalIns)

arrivals0 <- depart %>%
  filter(totalIns > 2) %>%
  group_by(paperNum) %>%
  filter(rowNum > 1)


#This is for institution with just 2 
departures1 <- depart %>%
  filter(totalIns == 2) %>%
  group_by(paperNum) %>%
  filter(rowNum == 1)

arrivals1 <- depart %>%
  filter(totalIns == 2) %>%
  group_by(paperNum) %>%
  filter(rowNum == 2)

# Merging all the datasets together
together0 <- cbind (departures0, arrivals0)
together1 <- cbind (departures1, arrivals1)
together2 <- rbind (together0, together1,together3)
View (together2)

#Taking away the redundant columns 
together2[5:9] <- list(NULL) 
together2[9:11] <- list(NULL) 

write.csv(together2, file = "+MAPREADY.csv")

#new dataset look 
#paperNum   authors   ins   lat0    lng0    lat1    lng0    date 

# Next steps. To change departure points to the following: 

# The First Author institution 
#Dep    Arr
#ins1   ins2 
#ins1   ins3 
#ins1   ins4 

# The Senior Author Institution 
#Dep    Arr
#ins4   ins2 
#ins4   ins3 
#ins4   ins4 



