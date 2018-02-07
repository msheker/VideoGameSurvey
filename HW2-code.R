#directory
setwd("c:/Users/doris/Desktop/math189_HW2")

#read data
data=read.table("videodata.txt", header=TRUE)
head(data)

#cleaning
data$math[data$math==99]=NA
data$educ[data$educ==99]=NA
data$like[data$like==99]=NA
data$freq[data$freq==99]=NA
#math divid into two groups
math1.ind= which(data$math==1)
math1= data[math1.ind,]
math0.ind= which(data$math==0)
math0= data[math0.ind,]
nrow(math1)
nrow(math0)

#label
data$math= ordered(data$math, levels=c(1,0), labels=c("Hate Math", "Not Hate Math"))
data$educ= ordered(data$educ, levels=c(1, 0), labels= c("Yes", "No"))
data$grade= ordered (data$grade, levels=c(4,3,2,1,0),labels=c("A","B","C", "D", "F"))
data$like= ordered(data$like, levels=c(1, 2, 3, 4, 5), labels=c("never played", "very much", "somewhat", "not really","not at all"))
data$freq= ordered(data$freq, levels=c(1,2,3,4), labels=c("Daily", "Weekly", "Monthly","Semesterly"))

#compare math with educ
prop.table(table(data$math, data$educ),1)
prop.table(table(data$math, data$grade),1)
prop.table(table(data$math, data$like), 1)
prop.table(table(data$math, data$freq),1)
prop.table(table(data$like, data$grade),1)
prop.table(table(data$grade, data$like),1)
#like and dislike group 
#data$like2 = ifelse(data$like %in% c("very much", "somewhat"),1, ifelse(data$like %in% c("not really", "not at all", "never played"), 2, NA ))
#data$like2 = ordered(data$like2, levels=c(1,2), labels=c("Like", "Don't like"))


# Grouped Bar Plot
counts =table(data$math, data$educ)
barplot(prop.table(counts,1)*100,main="Are video games educational?", col=c("darkblue","red"),beside=TRUE, ylim=c(0, 100), ylab="Precent", legend.text=rownames(counts), args.legend=list(x='topright', bty='n'))

counts2 =table(data$math, data$grade)
barplot(prop.table(counts2,1)*100,main="Expected Grade", col=c("darkblue","red"),beside=TRUE, ylim=c(0, 100), ylab="Precent", legend.text=rownames(counts2), args.legend=list(x='topright', bty="n"))

counts3 =table(data$math, data$like)
barplot(prop.table(counts3,1)*100,main="Like Video Game ", col=c("darkblue","red"),beside=TRUE, ylim=c(0, 100),ylab="Precent", legend.text=rownames(counts3), args.legend=list(x='topright', bty='n'))

counts4 =table(data$math, data$freq)
barplot(prop.table(counts4,1)*100,main="Frequency of Playing Video Game ", col=c("darkblue","red"),beside=TRUE, ylim=c(0, 100), ylab="Precent", legend.text=rownames(counts4), args.legend=list(x='topright', bty='n'))
