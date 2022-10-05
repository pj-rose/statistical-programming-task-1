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
 return(x)
 
}
  
#5

```{r }
pm=',|\\.|;|:|!|\\?'
a_m=split_punct(pm,a) #a_m is the new list with character and marks individually
```

##6
```{r }
#remove punctuations from txt
a_c=gsub(pm,'',a) #a_c : a cancel all the pronounciation marks

#(a)
lowered_a=tolower(a_c)
unique_word=unique(lowered_a)

#(b)
u_index=match(lowered_a,unique_word) # unique words of the lower letter a

#(c)
u_n=tabulate(u_index) #

#(d)
threshold=u_n[rank(u_n,ties.method = 'random')==length(u_n)-500]

#(e)
b=unique_word[u_n>160]

```

##7  b is the most commonly words 500.
```{r}
#7a
q7a <- match(lowered_a, b)

#7b
c1 <-match(unique_word, b) #unique有13072个，b仅499个。找在b中的下标。 unique不含标点
#c2<-head(c1+1,-1)
c2 <- c1[-1] #c1减掉第一位

c3 <- c2[-1]

matrix <- cbind(c1,c2,c3)
matrix
#7c
sum_of_rows<-rowSums (matrix, na.rm = FALSE, dims = 1)
removed_som <- sum_of_rows[!is.na(sum_of_rows)]
#7d 
T array
```

#6



