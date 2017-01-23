require("ggplot2")
require("dplyr")
require("lubridate")

setwd("~/Documents/UCD/BA Prac/6sense/takehome")
train<-read.csv("training.tsv",header = F,sep="\t")
test<-read.csv("test.tsv",header = F,sep="\t")
d<-train
d$type<-as.factor("train")
test$type<-as.factor("test")
d<-rbind(d,test)

colnames(d)<-c("userid","date","activity","type")
nrow(d[d$type=="test",])

str(d)
d$date<-as.Date(d$date,"%Y-%m-%d")
summary(d$date[d$type=="train"]) # first 22 months train
summary(d$date[d$type=="test"]) # last 2 months test

ggplot(d,aes(d$activity,fill=factor(d$type)))+geom_bar()
table(d$activity[d$type=="test"])
table(d$activity[d$type=="train"])

custid<-unique(d$userid[d$activity=="Purchase"]) #cust id who have purchased
cust<-d[d$userid %in% custid,] 
ggplot(cust,aes(cust$activity,fill=factor(cust$type)))+geom_bar()

length(unique(custid[custid %in% d$userid[d$type=="test"]])) #number of userids in test that have already purchased atleast once before - 27,474

nocustid<-unique(d$userid[!d$userid %in% custid]) #cust ids who havent purchased
length(unique(d$userid))-length(nocustid) #check

nocust<-d[d$userid %in% nocustid,] 
ggplot(nocust,aes(nocust$activity,fill=factor(nocust$type)))+geom_bar()

nocust %>% group_by(userid) %>% transmute(sum=sum(nocust$activity=="EmailOpen"))

#Feature engineering
d<-d %>% dplyr::group_by(userid) %>% dplyr::mutate(rank=row_number()) #action count
summary(d$rank)

d<-d %>% group_by(userid,date) %>% dplyr::mutate(dayactivityct=length(date)) #dayactivity count


# Convert from long to wide - spread the activity column
d$b<-"1"
new<-d %>% spread(activity,b) 
new[is.na(new)]<-"0"
###########




set1<- filter(d,d$date<"2013-09-01")
set2<- filter(d,d$date>="2013-09-01" & d$date<"2013-11-01")
set3<- filter(d,d$date>="2013-11-01" & d$date<"2014-01-01")
set4<- filter(d,d$date>="2014-01-01" & d$date<"2014-03-01")
set5<- filter(d,d$date>="2014-03-01" & d$date<"2014-05-01")
set6<- filter(d,d$date>="2014-05-01" & d$date<"2014-07-01")
set7<- filter(d,d$date>="2014-07-01" & d$date<"2014-09-01")
set8<- filter(d,d$date>="2014-09-01" & d$date<"2014-11-01")
set9<- filter(d,d$date>="2014-11-01" & d$date<"2015-01-01")
set10<- filter(d,d$date>="2015-01-01" & d$date<"2015-03-01")
set11<- filter(d,d$date>="2015-03-01" & d$date<"2015-05-01")
set12<- filter(d,d$date>="2015-05-01")
set11<-rbind(set11,set12[set12$type=="train",])
set12<-set12[set12$type=="test",]


nrow(set1)+nrow(set2)+nrow(set3)+nrow(set4)+nrow(set5)+nrow(set6)+nrow(set7)+nrow(set8)+nrow(set9)+nrow(set10)+nrow(set11)+nrow(set12) #check

#aggregate

set1<- set1 %>% group_by(userid) %>% dplyr::summarise_each(funs(sum),CustomerSupport:WebVisit)
set2<- set2 %>% group_by(userid) %>% dplyr::summarise_each(funs(sum),CustomerSupport:WebVisit)
set3<- set3 %>% group_by(userid) %>% dplyr::summarise_each(funs(sum),CustomerSupport:WebVisit)
set4<- set4 %>% group_by(userid) %>% dplyr::summarise_each(funs(sum),CustomerSupport:WebVisit)
set5<- set5 %>% group_by(userid) %>% dplyr::summarise_each(funs(sum),CustomerSupport:WebVisit)
set6<- set6 %>% group_by(userid) %>% dplyr::summarise_each(funs(sum),CustomerSupport:WebVisit)
set7<- set7 %>% group_by(userid) %>% dplyr::summarise_each(funs(sum),CustomerSupport:WebVisit)
set8<- set8 %>% group_by(userid) %>% dplyr::summarise_each(funs(sum),CustomerSupport:WebVisit)
set9<- set9 %>% group_by(userid) %>% dplyr::summarise_each(funs(sum),CustomerSupport:WebVisit)
set10<- set10 %>% group_by(userid) %>% dplyr::summarise_each(funs(sum),CustomerSupport:WebVisit)
set11<- set11 %>% group_by(userid) %>% dplyr::summarise_each(funs(sum),CustomerSupport:WebVisit)
set12<- set12 %>% group_by(userid) %>% dplyr::summarise_each(funs(sum),CustomerSupport:WebVisit)

=========
  
  i<-2
temp<-set1
for (i in 2:12)){
  a<-noquote(paste("set",i,sep=""))
  temp<- rbind(temp,a) %>% group_by(userid) %>% dplyr::summarise_each(funs(sum),CustomerSupport:WebVisit)
  newset<-left_join(cat("set",i,sep=""),temp,by="userid")
}


temp2<-left_join(temp2,temp,by="userid")

a<-cat("set",i,sep="")


temp2<-left_join(temp2,temp,by="userid")
i<-2
temp<-set1()

temp<- rbind(temp,noquote(as.character(paste0("set",i))))  %>% group_by(userid) %>% dplyr::summarise_each(funs(sum),CustomerSupport:WebVisit)
temp3<-left_join(temp3,temp,by="userid")


sprintf(paste("set",i,sep=""))
cat("set",i,sep="")

for (i in 2:12)){
  make.names("set%d",i)
}

noquote(paste("newname", 1, sep=""))

a<-list(set1,set2,set3,set4,set5,set6,set7,set8,set9,set10,set11,set12)


df<-NULL
for (i in 1:12){
  df[i]<- a[3] %>% as.data.frame() %>% group_by(userid) %>% dplyr::summarise_each(funs(sum),CustomerSupport:WebVisit)
}





do.call(paste, "set",i, sep="")

rbind(set1,a)
rbind(set1,set2)

a[2]
a<-paste("temp",i,sep="")

p<-d
p$set[d$date<"2013-09-01"] <-1


