install.packages('dplyr')
install.packages('car')
install.packages('ggplot2')
install.packages('scales')
install.packages('ggpubr')
installed.packages('gridExtra')
library(dplyr)
library(car)
library(ggplot2)
library(scales)
library(ggpubr)
library(gridExtra)
getwd()
setwd('E://Programming DS with R and python -MSIS 5223//Project//MSIS_Project_data')

data<-read.csv('loan_data_analysis_2015to2017.csv',header=T,sep=',')
attach(data)
str(data)

boxplot(funded_amnt)
sort(unique(boxplot.stats(funded_amnt)$out))
length(boxplot.stats(funded_amnt)$out)

boxplot(int_rate)
sort(unique(boxplot.stats(int_rate)$out))
length(boxplot.stats(int_rate)$out)

boxplot(installment)
sort(unique(boxplot.stats(installment)$out))
length(boxplot.stats(installment)$out)


boxplot(annual_inc)
sort(unique(boxplot.stats(annual_inc)$out))
length(boxplot.stats(annual_inc)$out)

boxplot(dti)
sort(unique(boxplot.stats(dti)$out))
length(boxplot.stats(dti)$out)


boxplot(total_rec_prncp)
sort(unique(boxplot.stats(total_rec_prncp)$out))
length(boxplot.stats(total_rec_prncp)$out)

boxplot(total_rec_int)
sort(unique(boxplot.stats(total_rec_int)$out))
length(boxplot.stats(total_rec_int)$out)

boxplot(last_fico_range_high, main="'Box plot for 'Last_fico_range_high'")
sort(unique(boxplot.stats(last_fico_range_high)$out))
length(boxplot.stats(last_fico_range_high)$out)

boxplot(last_fico_range_low, main="'Box plot for 'Last_fico_range_low'")
sort(unique(boxplot.stats(last_fico_range_low)$out))
length(boxplot.stats(last_fico_range_low)$out)


boxplot(avg_cur_bal)
sort(unique(boxplot.stats(avg_cur_bal)$out))
length(boxplot.stats(avg_cur_bal)$out)

data_without_outliers<-subset(data, (funded_amnt<38025 & int_rate<24.85 & annual_inc<166314 & installment<1090.26 & dti<43.86
                                     & total_rec_prncp<21965.06 & total_rec_int<5629 & last_fico_range_high>564 & last_fico_range_high<829
                                     & last_fico_range_low>560 & last_fico_range_high<845 & avg_cur_bal<41536))
sum(is.na(data_witnout_outliers))


#Aggregatingregions
region_bins = tribble(~addr_state, ~region,'CT','NorthEast','ME','NorthEast','MA','NorthEast','NH','NorthEast',
                      'RI','NorthEast','VT','NorthEast','NY','NorthEast','NJ','NorthEast','PA','NorthEast',
                      'IL','MidWest','IN','MidWest','MI','MidWest','OH','MidWest','WI','MidWest','IA','MidWest',
                      'KS','MidWest','MN','MidWest','MO','MidWest','NE','MidWest','ND','MidWest','SD','MidWest',
                      'DE','South','FL','South','GA','South','MD','South','NC','South','SC','South','VA','South',
                      'WV','South','DC','South','AL','South','KY','South','MS','South','TN','South','AR','South',
                      'LA','South','OK','South','TX','South','AZ','West','CO','West','ID','West','MT','West','NV',
                      'West','NM','West','UT','West','WY','West','AK','West','CA','West','HI','West','OR','West','WA','West')

data_with_regions = left_join(data_without_outliers, region_bins, by='addr_state')
str(data_with_regions)

#binning dti
bin_interval = c(-1,12,18,24,1000)
table(cut(data_with_regions$dti, bin_interval, right = F))
dti_vector = car::recode(data_with_regions$dti, "-1:12='0-12'; 12:18='13-18'; 
                    18:24='19-24'; 24:1000='24-1000'")
data_with_regions$dti_categ = dti_vector
summary(data_without_outliers$dti)


#binning loan status
Loan_Status_ =tribble(~loan_status, ~Loan_Status,'Fully Paid','Fully_Paid','Late (31-120 days)','Fully_Paid','Late (16-30 days)',
                      'Fully_Paid','Default','Defaulted','Charged Off','Defaulted',
                     'Current','current','In Grace Period','current') 
data_with_regions_loanstatus = left_join(data_with_regions, Loan_Status_, by='loan_status')



#Aggrregating Issue date
Month = tribble(~issue_d,~Quarter,'Jan-2015','2015-Q1','Feb-2015','2015-Q1','Mar-2015','2015-Q1','Apr-2015',
                '2015-Q2','May-2015','2015-Q2','Jun-2015','2015-Q2','Jul-2015','2015-Q3','Aug-2015','2015-Q3',
                'Sep-2015','2015-Q3','Oct-2015','2015-Q4','Nov-2015','2015-Q4','Dec-2015','2015-Q4',
                'Jan-2016','2016-Q1','Feb-2016','2016-Q1','Mar-2016','2016-Q1','Apr-2016','2016-Q2',
                'May-2016','2016-Q2','Jun-2016','2016-Q2','Jul-2016','2016-Q3','Aug-2016','2016-Q3',
                'Sep-2016','2016-Q3','Oct-2016','2016-Q4','Nov-2016','2016-Q4','Dec-2016','2016-Q4',  
                'Jan-2017','2017-Q1','Feb-2017','2017-Q1','Mar-2017','2017-Q1','Apr-2017','2017-Q2',
                'May-2017','2017-Q2','Jun-2017','2017-Q2','Jul-2017','2017-Q3','Aug-2017','2017-Q3',
                'Sep-2017','2017-Q3','Oct-2017','2017-Q4','Nov-2017','2017-Q4','Dec-2017','2016-Q4') 

final_data_set_before_removing = left_join(data_with_regions_loanstatus, Month, by='issue_d')
write.csv(final_data_set_before_removing,'E://Programming DS with R and python -MSIS 5223//Project//MSIS_Project_data//final_data_set_before_removing.csv',sep=',',row.names = F)
final_data_set_before_removing=read.csv('E://Programming DS with R and python -MSIS 5223//Project//MSIS_Project_data//final_data_set_before_removing.csv',sep=',',header=T)
detach(data)

final_dataset = data_with_regions_loanstatus_issuedate[,-c(11,12,15)]

#Finaldataset for Descriptive Statistics
write.csv(final_dataset,'E://Programming DS with R and python -MSIS 5223//Project//MSIS_Project_data//final_data_set.csv',sep=',',row.names = F)
final_data_set=read.csv('E://Programming DS with R and python -MSIS 5223//Project//MSIS_Project_data//final_data_set.csv',sep=',',header=T)
detach(data)

#Descriptive Analytics
#BarGraph for LoanStatus

count = table(final_data_set$Loan_Status)
aa= as.data.frame(count)
aa
colnames(aa)=c("Loan_Status","No_of_Customers")
check=ggplot(data=aa, aes(x= reorder(Loan_Status,No_of_Customers), 
y = No_of_Customers, fill=Loan_Status)) + geom_bar(stat="identity")
+geom_text(aes(label = No_of_Customers), vjust = -0.3)+ggtitle("Loan Status distribution") 
+xlab("Loan Status")+ylab("No. of Customers")+theme(plot.title = element_text(hjust = 0.5))
check + scale_y_continuous(labels = scales::comma)


# Pie Chart with Percentages for Grade
slices <- c(155613,283818,279644,105107,30976) 
lbls <- c("A", "B", "C", "D", "E")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct,sep=' ') # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Grade Distribution")

#Line graph for Issue_date

count = table(final_data_set$Quarter,final_data_set$Loan_Status)
count
cc= as.data.frame(count)
cc
ggplot(data=cc, aes(x=Var1, y=Freq, group=Var2)) + 
  geom_line(aes(colour=Var2), size=.5) + 
  geom_point(colour="blue", size=1, shape=10, fill="white")
+ggtitle("Loans Issued in each quarter from 2015 to 2017") +xlab("Quarter of a year")
+ylab("No. of Customers")+theme(plot.title = element_text(hjust = 0.5))+scale_y_continuous(labels = scales::comma)

#Tileplot between verification_status and loan_status

count = table(final_data_set$verification_status,final_data_set$Loan_Status)
dd= as.data.frame(count)
dd
ggplot(data = dd, aes(x=Var1, y=Var2, fill=Freq)) + 
  geom_tile()+ggtitle("Verification Status Vs Loan Status") +xlab("Verification Status")
+ylab("Loan Status")+theme(plot.title = element_text(hjust = 0.5))
colnames(dd)=c("verification_status","Loan_Status","Frequency")
grid.table(dd,rows=NULL)


#Histogram for dti

ggplot(data=final_data_set, aes(x=final_data_set$dti,fill=final_data_set$Loan_Status)) + 
  geom_histogram( col="red",fill="blue",alpha = .2) +labs(title="Histogram for dti ratio") +
  labs(x="dti ratio", y="No. of Customers")+theme(plot.title = element_text(hjust = 0.5))

#Barplot between dti ratio and loan_status


count = table(final_data_set$dti_categ,final_data_set$Loan_Status)
ee= as.data.frame(count)
ee
ggplot(ee, aes(Var1, Freq, fill = Var2)) + 
  geom_bar(stat = 'identity', position = 'dodge') +labs(title="dti ratio Vs Loan Status") +
  labs(x="dti ratio", y="No. of Customers", fill='Loan Status')+theme(plot.title = element_text(hjust = 0.5))

#Gridtable for Purpose

count = table(final_data_set$purpose)
ff= as.data.frame(count)
ff
colnames(ff)=c("Purpose","No of Customers")
grid.table(ff,rows=NULL)
