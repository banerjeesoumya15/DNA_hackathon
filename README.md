# DNA_hackathon
setwd("C:/Users/soumya.banerjee01.ITLINFOSYS/Documents/R/DNA_Hackathon")

############### load packages ###############
library(xlsx)

########### input data ###############
transaction <- read.xlsx("Fraud_Analytics_Dataset.xlsx", 1)
customer <- read.xlsx("Fraud_Analytics_Dataset.xlsx", 2)
sales_data <- read.xlsx("Fraud_Analytics_Dataset.xlsx", 3)

############ explore dataset ##########
str(transaction)
str(customer)  # large no. of columns
str(sales_data)  # large no. of columns

summary(transaction)  # 614 NAs in each columns
summary(customer)
summary(sales_data)

################ remove NAs ##############
transaction <- transaction[-which(is.na(transaction$Transaction_Id)),]
customer <- customer[-which(customer$LOYALTY_ID == ""),]

# customer$PLACE_OF_BITRH <- droplevels(customer$PLACE_OF_BITRH)
# customer$CUSTOMER_CREATED_DT <- factor(customer$CUSTOMER_CREATED_DT)

########## remove unnecessary columns ##########
customer <- subset(customer, select = -c(MEMBERSHIP_TYPE, MEMBERSHIP_TYPE__ID,
                                         CUSTOMER_FLG, customer_type_id, Customer_Type, 
                                         SALUTATION, GENDER, BIRTH_DATE,
                                         CUSTOMER_STATUS_ID, CUSTOMER_STATUS,
                                         DEVICE_ID, MARITAL_STATUS_ID, MARITAL_STATUS,
                                         ANNIVERSARY_DT, INCOME, PROFESSION, PROFESSION_TYPE_ID,
                                         PREF_COMM_LANG_ID, PREF_COMM_LANG, PREF_COMM_LANG_SC,
                                         PREF_ADDR, Residence_Address_Line_1, Residence_Address_Line_2,
                                         Residence_Address_Line_3, Residence_Country, residence_fax_number,
                                         Office_Address_Line_1, Office_Address_Line_2, Office_Address_Line_3,
                                         OFFICE_COUNTRY, office_fax_number,
                                         Preferred_Comm_Sub_Channel,
                                         Acquistion_Channel_ID, Acquistion_Channel_SC, Acquistion_Channel_Description,
                                         acquisition_subchannel_id, acquisition_subchannel,
                                         credit_risk_score, COUNTRY_OF_BIRTH_SC, EVER_BANKRUPT_FLG,
                                         BLACKLISTED_FLG, Record_created_By))

sales_data <- subset(sales_data, select = -c(Gender, Age, Membership_Id, Membership_Type,
                                             Campaign_Id, Store_Id, Store_Description,
                                             City, City_Id, Country_Id, Country_Sc,
                                             Country, Order_Item_Paidprice_Gross_Amt_,
                                             Order_Item_Listprice_Gross_Amt_, Currency,
                                             Discount_Amt, Cancel_Description, Return_Amt,
                                             Sales_Rep_Name))

######### join data frames ###########
master_table1 <- merge(transaction, sales_data, by = "Transaction_Id",
                       suffixes = c(".tra", ".sal"))

master_table1 <- setNames(master_table1, toupper(names(master_table1)))
master_table <- merge(customer, master_table1, by = "CUSTOMER_ID",
                      suffixes = c(".cu", ".ma"))
master_table <- setNames(master_table, toupper(names(master_table)))

############### feature engineering ############
str(master_table)
master_table <- master_table[, -grep("*_ID\\.*", colnames(master_table))]
master_table[] <- lapply(master_table, function(x) if(is.factor(x)) factor(x) else x)


########### split into train and test ##############
smp_size <- floor(0.7 * nrow(transaction))
set.seed(100)
train_ind <- sample(seq_len(nrow(transaction)), size = smp_size)

train <- transaction[train_ind, ]
test <- transaction[-train_ind, ]

############ explore datasets #########
str(train)
str(test)

sum(is.na(train))
sum(is.na(test))
summary(train)
summary(test)

num_cols <- train[, c("DNETVALUEPERUNIT.x", "DPABASEPRICE.x", "DTAX.x")]
cor(num_cols)
plot(num_cols)

# remove unneccesary columns
train <- train[, -c(1,2)]
test <- test[, -c(1,2)]
