## Group members: Henry Blackwell(), Jing Pan(s2312688), Peijie Zeng(s2332799)


## Contribution statement: The work is contributed by Henry Blackwell, Peijie Zeng and 
## Jing pan. Peijie and Henry did most of the code work. Where Peijie provided main
## structure of the code and Henry optimised them and contributed to comments. Jing
## helped to explain the math basis and gave suggestions to underlying logic of the question, 
## and also did the proofreading work.



setwd("/Users/henryblackwell/Library/Mobile Documents/com~apple~CloudDocs/University /Edinburgh /Statistical Programming/Group 35/statistical-programming-task-1")
a <- scan("bible.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers
a=iconv(a,'WINDOWS-1252','UTF-8')


#### 4 




## We define function which takes arguments two agruments:
##  1 - 'punct_mark' -- which is a list of punctuation marks 
##  2 - 'l' -- which is the text which we want to strip of punctuation marks
##  How the function works:
##  Use grep to find the index that contains punctuation marks; return index.
##  Use rep to create an empty vector x, which we will put strings in to.
##  Use gsub to remove all the punction marks
split_punct=function(punct_mark,l)
{
  position=grep(punct_mark,l) 
  x=rep("",length(l)+length(position))  
  iis<-position+1:length(position) ## Indices to put the marks.
  x[iis]<-substr(l[position],nchar(l[position]),nchar(l[position])) 
  y=gsub(punct_mark,"",l)  
  x[-iis]<-y
  return(x)
}


#### 5

pm=',|\\.|;|:|!|\\?' ## A 'list' of punctuation marks that we are looking to separate.
a_m=split_punct(pm,a) ## a_m is the new vector with words and punctuation marks in separate entries.



####6

##a_c=gsub(pm,'',a) ## remove punctuation from the text (a).
##(a)
lowered_a=tolower(a_m) ## Sets all of the text to lowercase.
unique_word=unique(lowered_a) ## Finds the subset of unique words in the lowercase text.
##(b)
u_index=match(lowered_a,unique_word) ##matches each word in the lowercase text to the index of the unique word.
##(c) 
u_tab=tabulate(u_index) ## Tabulates the frequency of each unique word.
##(d)
threshold=u_tab[rank(u_tab,ties.method = 'random')==length(u_tab)-500] ## Finds the minimum number of occurrences (e.g. the threshold) that makes the size of b approximately 500.
##(e)
b=unique_word[u_tab>threshold] ## Defines a vector,b, of approximately the 500 most common words, by only taking words which occur more than 160 times. 




####7 

##(a)
c1= match(lowered_a,b)## Creates column one by matching the lowercase text to the vector of common words.

##(b)
c2=c1[-1] ## Creates column two by indexing from the second position in the lowercase case text.
c3=c2[-1] ## Creates column three by indexing from the third position in the lowercase case text.

Triplet=cbind(c1,c2,c3) ## Creates a matrix, Triplet, with 3 columns.
###(c)
sum_of_rows<-rowSums (Triplet,na.rm = FALSE, dims = 1) ## Finds the sums of the rows of the matrix 'Triplet' or inserts 'False' if there are NA values. 
matrix=Triplet[!is.na(sum_of_rows),1:3] ## Creates a matrix without the rows which their exists at least one NA value.
####(d)
## We first create an empty m * m * m array named T.
## Then we loop over the matrix 'matrix' checking for the existence of sequential rows e.g. '1,2,3' 
## If we find any then we add 1 to the corresponding position in T.
T=array(0,c(length(b),length(b),length(b))) 
for (i in 1:length(matrix[,1])){
  T[matrix[i,1],matrix[i,2],matrix[i,3]]=T[matrix[i,1],matrix[i,2],matrix[i,3]]+1}
####(e)
## We left this blank as we used the sampling function in Q8

####(f) we follow a similar process as part (e)
## We create an empty m * m matrix named A.
## Then we loop over the first two and second two column's of 'matrix' checking for sequential rows e.g. 1,2.
## Upon finding these we add 1 to the corresponding position in A.
A=matrix(0,length(b),length(b))
for(j in 1:length(matrix[,1])){
  A[matrix[j,1],matrix[j,2]]=A[matrix[j,1],matrix[j,2]]+1
  A[matrix[j,2],matrix[j,3]]=A[matrix[j,2],matrix[j,3]]+1
}

## for S we need to find the number of times that b is never followed by another word. This is simply the occurrence of each of the
## common words as we have already calculated this in 6b we just restrict the code to only included words over the 'threshold'.  

S=u_tab[u_tab>threshold]








####8

## We define a function Bible_simulation where n is the length of the simulated text we want to generate.
## simu_index creates a vector of zeros with length n.
## We take a random sample of size 2 from the vector S 
## 
## What the Loop does:
## By looping over t we check whether this initial pair has a corresponding triplet if so then we randomly sample from T to get the 
## next word.
## If not then we check if either word from the initial pair has another corresponding word pair if so 
## then we randomly sample from the matrix A to get the next word.
## If both of the above aren't true then we simply sample from the the set S to obtain the next word
## we keep on doing this until we have generated all 50 words. 
## Then we use simu_txt to return the result so we can print it later on.
Bible_simulation=function(T,A,S,b,n)
{
  simu_index=rep(0,n)
  size=length(b)
  simu_index[1:2]=sample(size,2,prob=S)
  for (i in 3:n)
  {
    if (sum(T[simu_index[i-2],simu_index[i-1],]!=0))
    {
      simu_index[i]=sample(size,1,prob=T[simu_index[i-2],simu_index[i-1],])
    }
    else if(sum(A[simu_index[i-1],1])!=0)
    {
      simu_index[i]=sample(size,1,prob=A[i-1,])
    }
    else
    {
      simu_index[i]=sample(size,1,prob=S)
    }
  } 
  simu_txt=b[simu_index]
  return(simu_txt)
}
Markov_bible=Bible_simulation(T,A,S,b,50)
bible_in_order= paste(Markov_bible,collapse=' ')


####9

## This is a simplified text generator than only samples from the vector b given the probabilities stated by S.
random_bible=paste(sample_S=sample(b,50,prob=S),collapse=' ')


####10

lower_count=tabulate(match(a_m,b)) ## Checks the number of times a upper case version appears per word.
count=tabulate(match(lowered_a,b)) ## Checks the number of times a lower case version appears per word.
cases=c() 
for (i in 1:length(b)) #Check words in the whole text which only start with a capital letter
{
  if (any(a_m==b[i])==FALSE)
    cases=append(cases,i)
}
cat(cases) ## Indices from b of the words which ONLY start with a capital.
any(a_m==b[cases]) ## Checks  whether they have no lower cases.
lower_count[cases]=0 ## Adds to two zero to end of lower count to ensure the dimenisions are the same.
capital_count=count-lower_count ## Finds the difference between the number of capital and lowercase appearances.
b_m=b ## The modified version of b which has capitals included.
substr(b_m[which(capital_count/count>=0.5)],1,1)=toupper(substr(b[which(capital_count/count>=0.5)],1,1)) ## Capitalizes the words which are more frequently seen as starting with a capital.
paste(Bible_simulation(T,A,S,b_m,50),collapse=' ') ## Pastes the same generated text from Q8 but using the modified version of b.
