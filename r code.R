###
###    Henry Blackwell, Peijie Zeng, Jing Pan
###

setwd("D:/爱丁堡/来啦我滴宝儿/正课内容/programming/week2/第一次小组合作/statistical-programming-task-1")
a <- scan("bible.txt",what="character",skip=104) ## skip contents 直接到1:1后面
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
  

