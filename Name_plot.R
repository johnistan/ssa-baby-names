setwd("/tmp/")

names<-read.csv("~/top_for_all1.csv", header = TRUE)
names[,4]<-as.character(names$Female)
names[,3]<-as.character(names$Male)
names<-names[order(names$Rank),]


nm<-"Sharron"
p<-ggplot(names,aes(x=Year,y=Rank)) + ylim(max(names$Rank),min(names$Rank)) + geom_line(data = names[which(names$Female == nm),], aes(group=Female, colour = Female), alpha = 1, size = 2)+geom_line(aes(group=Female),colour="#431600",alpha=0.1)+ opts(title = 'Popularity of the name Trinity in the Last 50 Years(AKA The Matrix was a Really Popular Movie)')+ opts(axis.text.x=theme_text(angle=-70),hjust=0)
p

nm<-c("Malcolm","Jane")
p<-ggplot(names,aes(x=Year,y=Rank)) + ylim(max(names$Rank),min(names$Rank)) + geom_line(data = names[which(names$Male == nm),], aes(group=Male),colour = "Blue", alpha = 1)+geom_line(aes(group=Male),colour="#431600",alpha=0.06)+ opts(title = nm)+ opts(axis.text.x=theme_text(angle=-70),hjust=0)
p


###MULTIPLE PEOPLE####


nm<-c("Malcolm","Ethan","John","Allen","Eric")
p<-ggplot(names,aes(x=Year,y=Rank)) + ylim(max(names$Rank),min(names$Rank)) + geom_line(data = names[which(names$Male %in% nm),], aes(group=Male, colour = Male), alpha = 1, size = 1)+geom_line(aes(group=Male),colour="#431600",alpha=0.1)+ opts(title = "Male Baby Name Popularity Since 1950")+ opts(axis.text.x=theme_text(angle=-70),hjust=0)
p

nm<-"Malcolm"

sqldf("select a.* from names where Male = 'Malcolm'")

min.max<-function(nm){
data.frame(
  name = nm,
  min = min(names[which(names$Female == nm),2]),
  max = max(names[which(names$Female == nm),2]),
  dif = max(names[which(names$Female == nm),2]) - min(names[which(names$Female == nm),2])
  )
}

out[order(out$dif),]

out<-lapply(X = as.list(name.list), FUN = min.max)

out <- do.call("rbind", out)
out<-rbind(df,out)

p<-ggplot(names,aes(x=Year,y=Rank)) + ylim(max(names$Rank),min(names$Rank)) + geom_line(data = names[which(names$Male == nm),], aes(group=Male),colour = "Blue", alpha = 1)+geom_line(aes(group=Male),colour="#431600",alpha=0.2)+ opts(title = nm)+ opts(axis.text.x=theme_text(angle=-70),hjust=0)

name.list<-unique(names$Male)
for (i in 1:length(name.list)) {
  print(name.list[i])
p<-ggplot(names,aes(x=Year,y=Rank)) + ylim(max(names$Rank),min(names$Rank)) + geom_line(data = names[which(names$Male == name.list[i]),], aes(group=Male),colour = "Blue", alpha = 1)+geom_line(aes(group=Male),colour="#431600",alpha=0.2)+ opts(title = name.list[i])+ opts(axis.text.x=theme_text(angle=-70),hjust=0)
print(p)
Sys.sleep(.2)
}

p<-ggplot(names,aes(x=as.factor(Year),y=Rank)) 
p<-p+ geom_line(data = names[which(names$Male == name.list[i]),], aes(group=Male),colour = "Blue", alpha = 1)
p<-p + geom_text(data = names[which(names$Male == name.list[i]) && which(names$Year == "1986"),], aes(x=Year, y = Rank, label = Rank),colour = "Blue", alpha = 1)







