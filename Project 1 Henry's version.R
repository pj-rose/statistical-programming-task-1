setwd("/Users/henryblackwell/Library/Mobile Documents/com~apple~CloudDocs/University /Edinburgh /Statistical Programming/Group 35/statistical-programming-task-1")
a <- scan("bible.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers

split_punct=function(punct_mark,l)
{
  position=grep(punct_mark,l) #find the index that contains punctuations marks; return index
  x=rep("",length(l)+length(position))  #create a empty x, put strings in it
  
  iis<-position+1:length(position) #indice to put the marks
  x[iis]<-substr(l[position],nchar(l[position]),nchar(l[position])) 
  y=gsub(punct_mark,"",l) #delete all the marks
  x[-iis]<-y
  
  
  return(x)
  
}
pm=',|\\.|;|:|!|\\?'

a_m=split_punct(pm, a)
a_c=gsub(pm,'',a)
#6
#(a)
lowered_a=tolower(a_c)
unique_word=unique(lowered_a)
#(b)
u_index=match(lowered_a,unique_word)
#(c)
u_n=tabulate(u_index)
#(d)
threshold=u_n[rank(u_n,ties.method = 'random')==length(u_n)-500]
#(e)
b=unique_word[u_n>160]

#7a
q7a <- match(lowered_a, b)

#7b
c1 <-match(unique_word, b)
c2 <- head(c1 + 1, -1)
c3 <- head(c2 + 1, -1)

matrix <- cbind(c1,c2,c3)
matrix
#7c
sum_of_rows<-rowSums (matrix, na.rm = FALSE, dims = 1)
removed_som <- sum_of_rows[!is.na(sum_of_rows)]
#7d 
T array







