setwd('D:\\Data Science\\Loan Prediction')
train=read.csv("loan.csv")
dim(train)
library(plyr)
count(train,'loan_status')
sum(is.na(train$loan_status))
train<-train[train['loan_status']!='Issued',]
default <- c("Charged Off",
             "Default",
             "Does not meet the credit policy. Status:Charged Off", 
             "Late (16-30 days)",
             "Late (31-120 days)")
train['Default'] <- ifelse(train$loan_status  %in% default, 1,0)
NullValues<-colSums(is.na(train))
NullValues
RemoveColl<-NullValues[NullValues>400000]
RemoveColl
RemoveColl<-as.vector(names(RemoveColl))
train<-train[,!(names(train) %in% RemoveColl)]
count(train,'Default')
library(ggplot2)
p <- ggplot(train, aes(grade,int_rate))
p + geom_boxplot()
q<-ggplot(train, aes(factor(Default),int_rate))
q + geom_boxplot()
p <- ggplot(train, aes(factor(inq_last_6mths),int_rate))
p + geom_boxplot()
chisq.test(table(train[!is.na(train['inq_last_6mths']),c("Default","inq_last_6mths")]))
chisq.test(table(train[!is.na(train['open_acc']),c("Default","open_acc")]))
chisq.test(table(train[!is.na(train['pub_rec']),c("Default","pub_rec")]))
chisq.test(table(train[!is.na(train['total_acc']),c("Default","total_acc")]))
chisq.test(table(train[!is.na(train['acc_now_delinq']),c("Default","acc_now_delinq")]))
p <- ggplot(train, aes(factor(Default),revol_util))
p + geom_boxplot()
p <- ggplot(train, aes(factor(Default),total_rev_hi_lim ))
p + geom_boxplot()
p <- ggplot(train, aes(int_rate,total_rev_hi_lim ))
p +  geom_point()+scale_y_continuous(limits=c(0,2000000))
p <- ggplot(train, aes(factor(Default),tot_coll_amt ))
p + geom_boxplot()
p <- ggplot(train, aes(int_rate,tot_coll_amt ))
p +  geom_point()+scale_y_continuous(limits=c(0,500000))
p <- ggplot(train, aes(factor(Default),tot_coll_amt ))
p + geom_boxplot()
p <- ggplot(train, aes(int_rate,tot_coll_amt ))
p +  geom_point()+scale_y_continuous(limits=c(0,500000))
drop<-c("revol_util", "total_rev_hi_lim", "tot_coll_amt", "tot_cur_bal","acc_now_delinq")
train<-train[,!(names(train) %in% drop)]
train<-train[!(is.na(train['open_acc']) | is.na(train['collections_12_mths_ex_med'])),]
colSums(is.na(train))
drop<-c("id","member_id","loan_status","issue_d","url","desc","addr_state")
train<-train[,!(names(train) %in% drop)]
str(train)
cat<-c("term","grade","sub_grade","emp_title","emp_length","home_ownership","verification_status"
       ,"pymnt_plan","purpose","title","zip_code","earliest_cr_line","initial_list_status","last_pymnt_d"
       ,"next_pymnt_d","last_credit_pull_d","application_type","verification_status_joint")
for (i in 1:length(cat)){
  chisq.test(table(train[,c('Default',cat[i])]))
}
df<-aggregate(train$Default, by=list(train$zip_code), FUN=mean)
df<-df[order(-df['x']),]
ggplot(df, aes(x = Group.1, y = x)) + geom_bar(stat = "identity")
df[1:10,]
count(train[train["zip_code"]=='516xx',],'Default')
count(train[train["zip_code"]=='524xx',],'Default')
count(train[train["zip_code"]=='643xx',],'Default')
count(train[train["zip_code"]=='682xx',],'Default')
count(train[train["zip_code"]=='938xx',],'Default')
count(train,'verification_status_joint')
count(train,'verification_status')
drop<-c("emp_title","title","zip_code","earliest_cr_line","last_credit_pull_d","verification_status_joint",
        "last_pymnt_d","next_pymnt_d","last_credit_pull_d")
train<-train[,!(names(train) %in% drop)]
ggplot(train, aes(factor(term),int_rate))+ geom_boxplot()
aggregate(train$Default, by=list(train$term), FUN=mean)

ggplot(train, aes(factor(emp_length),int_rate))+ geom_boxplot()
aggregate(train$Default, by=list(train$emp_length), FUN=mean)

ggplot(train, aes(factor(home_ownership),int_rate))+ geom_boxplot()
aggregate(train$Default, by=list(train$home_ownership), FUN=mean)

ggplot(train, aes(factor(verification_status),int_rate))+ geom_boxplot()
aggregate(train$Default, by=list(train$verification_status), FUN=mean)

ggplot(train, aes(factor(pymnt_plan),int_rate))+ geom_boxplot()
aggregate(train$Default, by=list(train$pymnt_plan), FUN=mean)

ggplot(train, aes(factor(purpose),int_rate))+ geom_boxplot()
aggregate(train$Default, by=list(train$purpose), FUN=mean)

train$emp_length<-NULL
indx <- sapply(train, is.factor)
train <- lapply(train[indx], function(x) as.numeric(x))
train[is.na(train)]<-0
train<-as.data.frame(train)
library(corrplot)
corrplot(cor(train[,1:28]), method="circle")
drop<-c("funded_amnt","funded_amnt_inv","installment","out_prncp_inv","total_rec_prncp","total_pymnt_inv")
train<-train[,!(names(train) %in% drop)]
S