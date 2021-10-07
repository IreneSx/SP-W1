setwd("C:\\Users\\jing_\\OneDrive\\桌面\\SP-W1")
a <- scan("1581-0.txt",what="character",skip=156,encoding = 'UTF-8')

## strip license
n <- length(a)
a <- a[-((n-2909):n)] 

############################4.create the function called split_punct

split_punct = function(a){
  
##create vectors to store the word and punctuation
  ii<-grep("[[:punct:]]",a)         
  a_new<-rep(0,length(ii)+length(a))  
  
##remove all letters, numbers and punctuation mark
  punct<-gsub("[[:alnum:]]","",a)  
  punct=punct[nchar(punct)>0]      
  words<-gsub("[[:punct:]]","",a)  
  
##insert the punctuation mark and words
  iis<-ii+1:length(ii)            
  a_new[iis] <-punct              
  a_new[-iis] <- words             
  
  return(a_new[nchar(a_new)>0])    
}         

##############################5.separate the punctuation marks from words

a_splited=split_punct(a)          
a_splited=gsub(':.','.',a_splited)

##############################6.find the most commonly occurring words

##find the vector of unique words
a_lower=tolower(a_splited)  
a_unique=unique(a_lower)            
unique_index=match(a_lower,a_unique) 
a_uni_sorted=sort(a_unique)   

##how many time each unique word occurs in the text.
a_frequency=tabulate(factor(a_lower))

##adjust the number which makes the length(b)≈1000
m=92                                 
n=which((a_frequency>=m))             
b=a_uni_sorted[n]                  
length(b)                    

#########################################7.make the A matrix

##set the common words' index from match a and b 
common_index<-match(a_lower,b,nomatch = NA_integer_) 

##put the common words' index and following word's index into 2 column matrix
common_pairs<-cbind(common_index[1:length(common_index)-1],common_index[2:length(common_index)])

##drop the word pairs with NA 
common_pairs=common_pairs[which(is.na(rowSums(common_pairs))==F),]

##return the original text of pair of common words
common_words=cbind(b[common_pairs[,1]],b[common_pairs[,2]])

##set A matrix 
A=matrix(data=0,nrow=length(b),ncol=length(b))

##loop through the common word pairs
nk=nrow(common_pairs)
for(k in 1:nk){
  i=common_pairs[k,1] ##common words index of matrix
  j=common_pairs[k,2] ##follow words index of matrix
  A[i,j]=A[i,j]+1   
}

##standardize the rows of A
for(i in 1:nrow(A)){
  A[i,]=A[i,]/rowSums(A)[i] ##make sure every sum of row=1
}

##########################################8.simulate 50-word sections

##create vector to store the index and text
B=matrix(1:length(b)^2,nrow=length(b),ncol=length(b))
A_sample=vector(mode = 'integer',length = 50)
A_index=vector(mode = 'integer',length = 100)
A_text=vector(mode = 'character',length = 50)

##select a word randomly from B and pick the next word according to the probability in A
A_sample[1]=sample(B,1,prob = A/sum(A))
A_index=which(B==A_sample[1], arr.ind = TRUE)
A_text[1]=b[A_index[1]]
A_text[2]=b[A_index[2]]

##loop to get the following words
for (i in 3:50){
  A_sample[i-1]=sample(B[A_index[2*(i-2)],],1,prob = A[A_index[2*(i-2)],])  
  A_index=c(A_index,which(B==A_sample[i-1], arr.ind = TRUE))
  A_text[i]=b[A_index[2*i-2]]
}

##print the text
cat(A_text)

########################################9.modify our model

##create the function to start with a caplital letter 
upper_initial = function(x) {
  substr(x, 1, 1) = toupper(substr(x, 1, 1))
  return(x)
}

##create vector to store the index and text
A_sample2=vector(mode = 'integer',length = 50)
A_index2=vector(mode = 'integer',length = 100)
A_text2=vector(mode = 'character',length = 50)

##pick a random word after a period 
period_index=which(b=='.') # the index of period
A_sample2[1]=sample(B[period_index,],1,prob = A[period_index,])
A_index2=which(B==A_sample2[1], arr.ind = TRUE)
A_text2[1]=upper_initial(b[A_index2[2]])

##loop to get the following words according to the probability
for (i in 2:50){
  A_sample2[i]=sample(B[A_index2[2*(i-1)],],1,prob = A[A_index2[2*(i-1)],])  
  A_index2=c(A_index2,which(B==A_sample2[i], arr.ind = TRUE))
  A_text2[i]=b[A_index2[2*i]]
}

##capitalize the first letter after a period
for (i in 1:49){
  if(A_text2[i]=='.'){
    A_text2[i+1]=upper_initial(A_text2[i+1])
  }
}

##print the text
cat(A_text2)

