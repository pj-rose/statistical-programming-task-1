###
###    Henry Blackwell, Peijie Zeng, Jing Pan
###

setwd("D:/������/�����ҵα���/��������/programming/week2/��һ��С�����/statistical-programming-task-1")
a <- scan("bible.txt",what="character",skip=104) ## skip contents ֱ�ӵ�1:1����
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers

split_punct=function()
{position=grep("\\W",a) #find the index that contains punctuations marks; return index
 x=rep("",length(a)+length(position))  #create a empty x, put strings in it
 
 iis<-position+1:length(position) #indice to put the marks
 x[iis]<-substr(a[position],-1,-2) 
 gsub(pattern="\\W"," ",a) #delete all the marks
  
 
}
  
