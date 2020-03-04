

#Bring in Libraries and Functions

library("tm")
library("pdftools")
library("stringr")

##reading text
a<-pdf_text("/Users/sulli/Documents/Victoria County/2019_Victoria.pdf")
##Splitting by line
a2<-strsplit(a, "\\r\n") 

#function to get rid of spaces
trim <- function( x ) {
  
  gsub("[[:space:]]", "", x)
  
}

trim2 <- function(x){
  
  gsub("[[:space:]][[:space:]]","",x)
}

###Creating data frame to combine each school to

SchoolInfoALL<-data.frame(school=character(),rating=character(),students=character(),type=character(),TotalOpEx=character(),InstExp=character(),InstLeadExp=character(),LeadExp=character())

PagesALL<-data.frame(school=character(),firstpage=numeric(),lastpage=numeric())

schoolnumber<-c(1:26)
FirstSchoolFirstPage<-1
FirstSchoolLength<-length(a2[[1]])
FirstSchoolLastPage<-as.numeric(str_sub(trim(a2[[1]][FirstSchoolLength]),-1,-1))  
Pages<-data.frame(school=1,firstpage=FirstSchoolFirstPage,lastpage=FirstSchoolLastPage)
PagesALL<-rbind(PagesALL,Pages)

for(i in 2:26){
  prior<-i-1
  FirstPage<-as.numeric(paste0(PagesALL$lastpage[prior]))+1
  LengthT<-length(a2[[FirstPage]])
  lastpageT<-as.numeric(str_sub(trim(a2[[FirstPage]][LengthT]),-1,-1))+as.numeric(paste0(FirstPage))-1  
  Pages<-data.frame(school=i,firstpage=FirstPage,lastpage=lastpageT)
  PagesALL<-rbind(PagesALL,Pages)
  print(paste0("Finished School: ",i)) #See where we are in the process 
}



###Testing Loop
for (i in 2:2){
  FirstPageL<-as.numeric(paste0(PagesALL$firstpage[i]))
  LengthL<-as.numeric(length(a2[[FirstPageL]]))
  ##School name
  schoolname<-trim2(paste0(a2[[FirstPageL]][3]))
  
  ##Rating
  overallrating<-str_sub(str_extract(trim(subset(a2[[FirstPageL]],grepl("earned a",a2[[FirstPageL]]))),".\\([[:digit:]][[:digit:]]|F\\(|.TotalStudents"),1,1)

  
  
  ##Campus Type and Total Students
  campus<-paste0(a2[[FirstPageL]][6:9])
  
  ##Expenditures per Student
  ExpLoop<-LengthL-10
  Expenditures<-paste0(a2[[FirstPageL]][ExpLoop:LengthL])
  
  
  ##Getting just the type of school
  type<-str_extract(subset(campus,grepl("High School|Elementary/Secondary|Middle School|Elementary",campus)),"High School|Elementary/Secondary|Middle School|Elementary")
  
  ##Getting Just the total number of students
  regexp <- "[[:digit:]]+[:punct:][[:digit:]]+|[[:digit:]]+"
  students<-str_extract(str_sub(subset(campus,grepl("Total Students:",campus)),-25,-1), regexp)
  
  
  ##Getting Total Operating Expenditures for campus
  TotalOpExp<-str_extract(subset(trim(Expenditures),grepl("^TotalOperatingExpenditures",trim(Expenditures))), "\\$[[:digit:]]+[:punct:][[:digit:]]+|\\$[[:digit:]]+")
  
  
  ##Getting Instuction Expenditures for campus
  InstExp<-str_extract(subset(trim(Expenditures),grepl("^Instruction\\$",trim(Expenditures))), "\\$[[:digit:]]+[:punct:][[:digit:]]+|\\$[[:digit:]]+")
  
  ##Getting Instructional Leadership Expenditures for campus
  InstLeadExp<-str_extract(subset(trim(Expenditures),grepl("^InstructionalLeadership",trim(Expenditures))), "\\$[[:digit:]]+[:punct:][[:digit:]]+|\\$[[:digit:]]+")
  
  ##Getting School Leadership Expenditures for campus
  LeadExp<-str_extract(subset(trim(Expenditures),grepl("^SchoolLeadership",trim(Expenditures))), "\\$[[:digit:]]+[:punct:][[:digit:]]+|\\$[[:digit:]]+")
  
  ##Create 1 data frame to have all these information
  SchoolInfo<-data.frame(school=schoolname,rating=overallrating,students=students,type=type,TotalOpEx=TotalOpExp,InstExp=InstExp,InstLeadExp=InstLeadExp,LeadExp=LeadExp)
  
  SchoolInfoALL<-rbind(SchoolInfoALL,SchoolInfo)
  print(paste0("Finished School: ", schoolname)) #See where we are in the process 
  
  
}




###Testing Loop
for (i in 4:9){
  FirstPageL<-as.numeric(paste0(PagesALL$firstpage[i]))
  LengthL<-as.numeric(length(a2[[FirstPageL]]))
  ##School name
  schoolname<-trim2(paste0(a2[[FirstPageL]][3]))
  
  ##Rating
  overallrating<-str_sub(str_extract(trim(subset(a2[[FirstPageL]],grepl("earned a",a2[[FirstPageL]]))),".\\([[:digit:]][[:digit:]]|F\\(|.TotalStudents"),1,1)
  
  
  
  ##Campus Type and Total Students
  campus<-paste0(a2[[FirstPageL]][6:9])
  
  ##Expenditures per Student
  ExpLoop<-LengthL-10
  Expenditures<-paste0(a2[[FirstPageL]][ExpLoop:LengthL])
  
  
  ##Getting just the type of school
  type<-str_extract(subset(campus,grepl("High School|Elementary/Secondary|Middle School|Elementary",campus)),"High School|Elementary/Secondary|Middle School|Elementary")
  
  ##Getting Just the total number of students
  regexp <- "[[:digit:]]+[:punct:][[:digit:]]+|[[:digit:]]+"
  students<-str_extract(str_sub(subset(campus,grepl("Total Students:",campus)),-25,-1), regexp)
  
  
  ##Getting Total Operating Expenditures for campus
  TotalOpExp<-str_extract(subset(trim(Expenditures),grepl("^TotalOperatingExpenditures",trim(Expenditures))), "\\$[[:digit:]]+[:punct:][[:digit:]]+|\\$[[:digit:]]+")
  
  
  ##Getting Instuction Expenditures for campus
  InstExp<-str_extract(subset(trim(Expenditures),grepl("^Instruction\\$",trim(Expenditures))), "\\$[[:digit:]]+[:punct:][[:digit:]]+|\\$[[:digit:]]+")
  
  ##Getting Instructional Leadership Expenditures for campus
  InstLeadExp<-str_extract(subset(trim(Expenditures),grepl("^InstructionalLeadership",trim(Expenditures))), "\\$[[:digit:]]+[:punct:][[:digit:]]+|\\$[[:digit:]]+")
  
  ##Getting School Leadership Expenditures for campus
  LeadExp<-str_extract(subset(trim(Expenditures),grepl("^SchoolLeadership",trim(Expenditures))), "\\$[[:digit:]]+[:punct:][[:digit:]]+|\\$[[:digit:]]+")
  
  ##Create 1 data frame to have all these information
  SchoolInfo<-data.frame(school=schoolname,rating=overallrating,students=students,type=type,TotalOpEx=TotalOpExp,InstExp=InstExp,InstLeadExp=InstLeadExp,LeadExp=LeadExp)
  
  SchoolInfoALL<-rbind(SchoolInfoALL,SchoolInfo)
  print(paste0("Finished School: ", schoolname)) #See where we are in the process 
  
  
}



###Testing Loop
for (i in 11:26){
  FirstPageL<-as.numeric(paste0(PagesALL$firstpage[i]))
  LengthL<-as.numeric(length(a2[[FirstPageL]]))
  ##School name
  schoolname<-trim2(paste0(a2[[FirstPageL]][3]))
  
  ##Rating
  overallrating<-str_sub(str_extract(trim(subset(a2[[FirstPageL]],grepl("earned a",a2[[FirstPageL]]))),".\\([[:digit:]][[:digit:]]|F\\(|.TotalStudents"),1,1)
  
  
  
  ##Campus Type and Total Students
  campus<-paste0(a2[[FirstPageL]][6:9])
  
  ##Expenditures per Student
  ExpLoop<-LengthL-10
  Expenditures<-paste0(a2[[FirstPageL]][ExpLoop:LengthL])
  
  
  ##Getting just the type of school
  type<-str_extract(subset(campus,grepl("High School|Elementary/Secondary|Middle School|Elementary",campus)),"High School|Elementary/Secondary|Middle School|Elementary")
  
  ##Getting Just the total number of students
  regexp <- "[[:digit:]]+[:punct:][[:digit:]]+|[[:digit:]]+"
  students<-str_extract(str_sub(subset(campus,grepl("Total Students:",campus)),-25,-1), regexp)
  
  
  ##Getting Total Operating Expenditures for campus
  TotalOpExp<-str_extract(subset(trim(Expenditures),grepl("^TotalOperatingExpenditures",trim(Expenditures))), "\\$[[:digit:]]+[:punct:][[:digit:]]+|\\$[[:digit:]]+")
  
  
  ##Getting Instuction Expenditures for campus
  InstExp<-str_extract(subset(trim(Expenditures),grepl("^Instruction\\$",trim(Expenditures))), "\\$[[:digit:]]+[:punct:][[:digit:]]+|\\$[[:digit:]]+")
  
  ##Getting Instructional Leadership Expenditures for campus
  InstLeadExp<-str_extract(subset(trim(Expenditures),grepl("^InstructionalLeadership",trim(Expenditures))), "\\$[[:digit:]]+[:punct:][[:digit:]]+|\\$[[:digit:]]+")
  
  ##Getting School Leadership Expenditures for campus
  LeadExp<-str_extract(subset(trim(Expenditures),grepl("^SchoolLeadership",trim(Expenditures))), "\\$[[:digit:]]+[:punct:][[:digit:]]+|\\$[[:digit:]]+")
  
  ##Create 1 data frame to have all these information
  SchoolInfo<-data.frame(school=schoolname,rating=overallrating,students=students,type=type,TotalOpEx=TotalOpExp,InstExp=InstExp,InstLeadExp=InstLeadExp,LeadExp=LeadExp)
  
  SchoolInfoALL<-rbind(SchoolInfoALL,SchoolInfo)
  print(paste0("Finished School: ", schoolname)) #See where we are in the process 
  
  
}



#############################################################################################################################
