library(ggplot2)

#load
names<-read.csv("~/ssa-baby-names/top_names_since_1950.csv", header = TRUE)
names[,4]<-as.character(names$Female)
names[,3]<-as.character(names$Male)

###Single Female Name#####
nm<-"Hillary" #Enter Female Name

+geom_line(aes(group=Female),colour="#431600",alpha=0.1)

p<-ggplot(names,aes(x=Year,y=Rank)) 
p <- p + ylim(max(names$Rank),min(names$Rank)) # Flip the Y-Axis
nm <- "Desiree" # enter a female name
p <- p  + geom_line(data = names[which(names$Female == nm),], aes(group=Female, colour = Female), alpha = 1, size = 2) + opts(title = nm)
p
#+ opts(title = 'Popularity of the Female Name')+ opts(axis.text.x=theme_text(angle=-45),hjust=0)
p
#            Set up ggplot object                  reverse the Y-axis                         Plot the Specific Name you highligted                                                                 Plot All names with low alpha -- a little too much Noise              Title                                           Angle X-Axis Text
p<-ggplot(names,aes(x=Year,y=Rank)) + ylim(max(names$Rank),min(names$Rank)) + geom_line(data = names[which(names$Female == nm),], aes(group=Female, colour = Female), alpha = 1, size = 2)+geom_line(aes(group=Female),colour="#431600",alpha=0.1)+ opts(title = 'Popularity of the Female Name')+ opts(axis.text.x=theme_text(angle=-45),hjust=0)
p



####Single Male Name####
nm<-c("Sue")
p<-ggplot(names,aes(x=Year,y=Rank)) + ylim(max(names$Rank),min(names$Rank)) + geom_line(data = names[which(names$Male == nm),], aes(group=Male),colour = "Blue", alpha = 1)+geom_line(aes(group=Male),colour="#431600",alpha=0.06)+ opts(title = nm)+ opts(axis.text.x=theme_text(angle=-70),hjust=0)
p

###MULTIPLE PEOPLE####

###MALE####
nm<-c("Malcolm","Ethan","John","Eric")
p<-ggplot(names,aes(x=Year,y=Rank)) + ylim(max(names$Rank),min(names$Rank))
p <- p + geom_line(data = names[which(names$Male %in% nm),], aes(group=Male, colour = Male), alpha = 1, size = 1)
p <- p + opts(title = "Male Baby Name Popularity Since 1950")
p <- p + facet_wrap(~Male)
p
+ opts(axis.text.x=theme_text(angle=-70),hjust=0)
p


####FEMALE#####
p<-ggplot(names,aes(x=Year,y=Rank)) + ylim(max(names$Rank),min(names$Rank)) + geom_line(data = names[which(names$Female %in% nm),], aes(group=Female, colour = Female), alpha = 1, size = 1)+geom_line(aes(group=Female),colour="#431600",alpha=0.1)+ opts(title = "Female Baby Name Popularity Since 1950")+ opts(axis.text.x=theme_text(angle=-70),hjust=0)
p

nm<-"Trinity"
p<-ggplot(names,aes(x=Year,y=Rank)) 
p<- p + ylim(max(names$Rank),min(names$Rank)) 
p<- p + geom_line(data = names[which(names$Female %in% nm),], aes(group=Female, colour = Female), alpha = 1, size = 1)
p<- p + opts(title = "People Liked the Matrix Way Too Much")
matrix.label<-data.frame(Year = 1985 , Rank = 220, Text = "Matrix Released - 1999") # create the custom on graphic text label
p <- p +  geom_rect(aes(xmin = 1998 , xmax = 2000 , ymin = 1000 , ymax = 1 ),fill = "Green", alpha = .002)
p <- p + geom_text(data = matrix.label, aes(label = Text))
p


rm(p)

###Calculate Greatest Change over Time
#This would be more elequently done with Hadley Wickam's ddply. The lapply/do.call("rbind") combo is brillinatly useful and for simple things I use
name.min.max<-function(nm){
data.frame(
  name = nm,
  min = min(names[which(names$Female == nm),2]),
  max = max(names[which(names$Female == nm),2]),
  dif = max(names[which(names$Female == nm),2]) - min(names[which(names$Female == nm),2])
  )
}
name.list<-unique(names$Female)
out<-lapply(X = as.list(name.list), FUN = name.min.max)
out <- do.call("rbind", out)
female.dif<-out[order(-out$dif),]
#Top Female Names with greatest change overtime # Need to seperate the winners and losser just plots largest difference
nm<-as.character(female.dif[1:10,1])

#Plot
p<-ggplot(names,aes(x=Year,y=Rank)) 
p <- p + ylim(max(names$Rank),min(names$Rank)) 
p <- p + geom_line(data = names[which(names$Female %in% nm),], aes(group=Female, colour = Female), alpha = 1, size = 1)
p <- p + opts(title = "Top Movers")
p <- p + facet_wrap(~Female)
p
`Delt.Absolute` <-
function(x1,x2=NULL,k=0,type=c('arithmetic','log'))
{
    x1 <- try.xts(x1, error=FALSE)
    type <- match.arg(type[1],c('log','arithmetic', 'absolute'))
    if(length(x2)!=length(x1) && !is.null(x2)) stop('x1 and x2 must be of same length');
    if(is.null(x2)){
        x2 <- x1 #copy for same symbol deltas
        if(length(k) < 2) {
            k <- max(1,k)
        }
    }
    dim(x2) <- NULL  # allow for multiple k matrix math to happen
    if(type=='log') {
        xx <- lapply(k, function(K.) {
                log(unclass(x2)/Lag(x1,K.))
              })
    } else if (type=='absolute') {
        xx <- lapply(k, function(K.) {
                unclass(x2) - Lag(x1,K.)
              })
    } else {
       xx <- lapply(k, function(K.) {
                unclass(x2)/Lag(x1,K.)-1
              })

    }
    xx <- do.call("cbind", xx)
    colnames(xx) <- paste("Delt",k,type,sep=".")
    reclass(xx,x1)
}

female.names<-names[,c(1,4,2)]
female.names<-female.names[order(female.names$Female, female.names$Year),]
female.names$delta <- Delt.Absolute(female.names$Rank,k=1, type = "absolute")
#female.names$delta3 <- Delt.Absolute(female.names$Rank)

female.names[20:30,]


female.names[c(TRUE, female.names$Female[-1] != female.names$Female[-length(female.names$Female)]), 4] <- NA
female.names.winners<-female.names[order(female.names[4]),]
female.names.losers<-female.names[order(-female.names[4]),]

female.names.winners<-female.names[order(female.names[4]),]    
top.female.names<-unique(female.names.winners[1:12,2])
p<-ggplot(names,aes(x=Year,y=Rank)) + ylim(max(names$Rank),min(names$Rank)) + geom_line(data = names[which(names$Female %in% top.female.names),], aes(group=Female, colour = Female), alpha = 1, size = 1)+ opts(title = "Female Baby Name Popularity Since 1950")+ opts(axis.text.x=theme_text(angle=-70),hjust=0) + facet_wrap(~ Female)
p



#Came across this by using the same fast accedency script as above, but using fastest decline. This Is what I found
#Poor, Poor Hillary

nm<-"Hillary"
p<-ggplot(names[which(names$Female %in% nm),],aes(x=Year,y=Rank)) 
p<- p + ylim(max(names$Rank),min(names$Rank)) 
p<- p + geom_line(data = names[which(names$Female %in% nm),], aes(group=Female, colour = Female), alpha = 1, size = 1)
p<- p + opts(title = "Poor, Poor Hillary: Popularity of the Female Name 'Hillary' \n But we already that the Clinton Prseidency was better to certain women", size = 30)
matrix.label<-data.frame(Year = 1996 , Rank = 100, Text = "Clinton \n Presidency") # create the custom on graphic text label, kind of a hack
p <- p +  geom_rect(aes(xmin = 1992 , xmax = 2001 , ymin = 1000 , ymax = 1 ),fill = "Blue", alpha = .002) 
p <- p +  geom_rect(aes(xmin = 2007 , xmax = 2008 , ymin = 1000 , ymax = 1 ),fill = "Green", alpha = .002) 
matrix.label2<-data.frame(Year = 2007 , Rank = 100, Text = "Hillary's \n Presidential \n Run")

# 1993 ->2001 the years of the Clinton Presdidency but 1992 was when he won the election so that is our starting point
p <- p + geom_text(data = matrix.label, aes(label = Text, size =20))
p <- p + geom_text(data = matrix.label2, aes(label = Text, size = 20))
print(p)







###Loop through all Male names ### Might give you a Seizure
name.list<-unique(names$Male)
for (i in 1:length(name.list)) {
  print(name.list[i])
p<-ggplot(names,aes(x=Year,y=Rank)) + ylim(max(names$Rank),min(names$Rank)) + geom_line(data = names[which(names$Male == name.list[i]),], aes(group=Male),colour = "Blue", alpha = 1)+geom_line(aes(group=Male),colour="#431600",alpha=0.2)+ opts(title = name.list[i])+ opts(axis.text.x=theme_text(angle=-70),hjust=0)
print(p)
Sys.sleep(.2)
}







