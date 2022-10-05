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
a_m=split_punct(pm,a)

u <-unique(tolower(a_m), incomparables = FALSE)
g <-match(tolower(a_m), u, nomatch = NA_integer_, incomparables = NULL)
t <-tabulate(g)

vec <- unlist(t)
n <- 3
partial <- length(v) - N + 1
Nth <- sort(vec, partial = partial)[partial]
indexes <- which(vec >= Nth)
vec[indexes]


