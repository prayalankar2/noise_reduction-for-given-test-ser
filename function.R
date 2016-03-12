library(data.table)


scon <- fread("D:/New folder (7)/matr/SCONES_test.tsv")
scon<-data.frame(scon)

#converting log values into numeric data form
  
      scon$normalts1<-exp(scon$testSample1)
      scon$normalts2<-exp(scon$testSample2)


#removing extra columns
      scon1<-scon[,-c(4,5)]

#separating two samples
      
      scon11<-scon1[scon1$chr==1,]
      scon12<-scon1[scon1$chr!=1,]


scon11$id<-1:nrow(scon11)
scon12$id<-1:nrow(scon12)

scon11<-scon11[,-1]
scon12<-scon12[,-1]

df1<-scon11[,c(5,1:4)]
df2<-scon12[,c(5,1:4)]



#removing outlier data

##   plotting of data shows that result should be around 1 but few results 
##   are less then 0.7 and 
##  a little are more then 1.3 
##  they have no contribution in result other then adding noise 
##  to remove this noise 
##  we will make a final column from two columns which are near to one

# in dataset 1

nrow(df1)        #7540


skewness(scon11$normalts1)      ### 8.866
##(temp does not contain row number 4229 which has inf value of normalts2)
temp<-scon11[-4229,]
skewness(temp$normalts2)     ### 2.152    


table(df1$normalts1< (0.7))  ###  153
table(df1$normalts1>(1.3))   #### 84


table(df1$normalts2<(0.7))  ### 304
table(df1$normalts2>(1.3))  ### 46



df11<-df1[df1$normalts1>=(0.7) & df1$normalts1 <=(1.3),]
df12<-df11[df11$normalts2>=(0.7) & df11$normalts2 <=(1.3),]


skewness(df12$normalts1)      ### -0.390
skewness(df12$normalts2)      ### 0.051


### it can been seen that highly assymetrical data is now almost symmetrical 
   #from 8.866 -> -0.390
   #from 2.152 ->  0.051


# in dataset 2

nrow(df2)

skewness(df2$normalts1)      ### -0.652
skewness(df2$normalts2)      ### -0.146


table(df2$normalts1< (0.7))  ###  243
table(df2$normalts1>(1.3))   #### 22


table(df2$normalts2<(0.7))  ### 111
table(df2$normalts2>(1.3))  ### 40


df21<-df2[df2$normalts1>=(0.7) & df2$normalts1 <=(1.3),];nrow(df21)
df22<-df21[df21$normalts2>=(0.7) & df21$normalts2 <=(1.3),];nrow(df22)


skewness(df22$normalts1)      ### -0.235
skewness(df22$normalts2)      ### -0.178


#### asymmetricty is highly reduced  in test sample 1 
        # from -0.652 -> -0.235


#### if each of the testsample1 & testsample2 are the result of same experiment 
#### we can take one which is nearer two 1 from two samples

###sample 1


df12$finalres<-0          #creating final value column from two values

df12$res1<-abs(df12$normalts1-1)
df12$res2<-abs(df12$normalts2-1)


for(i in 1:nrow(df12)) {
  
   if(df12[i,]$res1>df12[i,]$res2)
        df12[i,]$finalres<-df12[i,]$normalts2
   else
        df12[i,]$finalres<-df12[i,]$normalts1
     
}

df12<-df12[,-c(7,8)]

skewness(df12$finalres)   ### 0.1819

df12$final<-log(df12$finalres)
##sample 2

df22$finalres<-0          #creating final value column from two values

df22$res1<-abs(df22$normalts1-1)
df22$res2<-abs(df22$normalts2-1)


for(i in 1:nrow(df22)) {
  
  if(df22[i,]$res1>df22[i,]$res2)
    df22[i,]$finalres<-df22[i,]$normalts2
  else
    df22[i,]$finalres<-df22[i,]$normalts1
  
}


df22<-df22[,-c(7,8)]

skewness(df22$finalres)    #####  0.15


df22$final<-log(df22$finalres)






