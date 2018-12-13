# DNA_hackathon
setwd("C:/Users/soumya.banerjee01.ITLINFOSYS/Documents/R/DNA_Hackathon")

############### load packages ###############
library(xlsx)
library(mlr)
library(FSelector)
library(kernlab)

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
colnames(master_table)
#master_table <- master_table[, -grep("*_ID\\.*", colnames(master_table))]
master_table[] <- lapply(master_table, function(x) if(is.factor(x)) factor(x) else x)

master_table$EXP <- master_table$AGE - master_table$AGE_AT_JOINING  # remove AGE &
                                                                    # AGE_AT_JOINING
master_table <- subset(master_table, select = -c(AGE_AT_JOINING, AGE))

# remove primary keys
master_table <- subset(master_table, select = -c(CUSTOMER_ID, LOYALTY_ID.CU, TRANSACTION_ID,
                                                 INVOICE_ID, ORDER_ID, MDM_CUSTOMER_ID,
                                                 LOYALTY_ID.MA))
# two times PLACE_OF_BITRH column
master_table <- master_table[, -46]

master_table <- master_table[, -4]  # GENDER_ID not needed

# residence_city and residence_province are same
# residence_zip_cd is i
master_table <- master_table[, c(-17,-19)]

# extract std code from phone
master_table$RES_PH_CD <- as.character(lapply(master_table$RESIDENCE_PHONE,
                                 function(x) substr(x, 2, 4)))
master_table$RES_PH_CD <- as.factor(master_table$RES_PH_CD)
master_table <- master_table[, -18]  # RESIDENCE_PHONE

master_table <- master_table[, -20]  # OFFICE__ZIP_CD
master_table$OFF_PH_CD <- as.character(lapply(master_table$OFFICE_PHONE_NO,
                                              function(x) substr(x, 2, 4)))
master_table$OFF_PH_CD <- as.factor(master_table$OFF_PH_CD)
master_table <- master_table[, -20]  # OFFICE_PHONE_NO

# extract domain name from e-mail
master_table$DOM_EMAIL <- as.character(lapply(master_table$PREFERRED_EMAIL_ID,
                                              function(x) gsub(".*@|\\..*", "", x)))
master_table$DOM_EMAIL <- as.factor(master_table$DOM_EMAIL)
master_table <- master_table[, -20]  # PREFERRED_EMAIL_ID

# interests seem irrelevant
master_table <- master_table[, c(-31, -32)]

# DRIVING_LICENSE_NO BIRTH_MONTH BIRTH_YEAR BIRTH_QUARTER not needed
master_table <- master_table[, c(-33:-36)]
# PAYMENT.INFORMATION.TYPED.WITH.CAPITAL.LETTERS..
master_table <- master_table[, -43]

# convert string to date format
'''

for(i in 1:nrow(master_table)){

master_table$CANCEL_DATE[i] <- as.Date(master_table$CANCEL_DATE[i], "%m/%d/%Y")

master_table$EXPIRY_DATE[i] <- as.Date(master_table$EXPIRY_DATE[i], "%m/%d/%Y")
master_table$RETURN_DT[i] <- as.Date(master_table$RETURN_DT[i], "%m/%d/%Y")
master_table$SALE_DT[i] <- as.Date(master_table$SALE_DT[i], "%m/%d/%Y")
master_table$LASTMODIFIED_DT[i] <- as.Date(master_table$LASTMODIFIED_DT[i],"%m/%d/%Y")
}


for(i in 1:nrow(master_table)){
  master_table$DIFF_CAN_EXP[i] <- as.numeric(master_table$EXPIRY_DATE[i] - master_table$CANCEL_DATE[i])
  master_table$DIFF_RET_EXP[i] <- as.numeric(master_table$EXPIRY_DATE[i] - master_table$RETURN_DT[i])
  master_table$DIFF_SAL_EXP[i] <- as.numeric(master_table$EXPIRY_DATE[i] - master_table$SALE_DT[i])
  master_table$DIFF_MOD_EXP[i] <- as.numeric(master_table$EXPIRY_DATE[i] - master_table$LASTMODIFIED_DT[i])
}
'''
# remove dates
master_table <- subset(master_table, select = -c(SALE_DT, CANCEL_DATE, EXPIRY_DATE,
                                                 RETURN_DT, LASTMODIFIED_DT))

head(master_table)  # \r at the end of SALES_REP_ID
master_table$SALES_REP_ID <- as.character(master_table$SALES_REP_ID)
for(i in 1:nrow(master_table)){
  master_table$SALES_REP_ID[i] <- gsub("\\r$", "", master_table$SALES_REP_ID[i])
}
master_table$SALES_REP_ID <- as.factor(master_table$SALES_REP_ID)
# too many friends count is 0
# instead use FRIEND_FLG
master_table$FRIEND_FLG <- "N"
master_table$FRIEND_FLG <- factor(master_table$FRIEND_FLG, levels = c("N", "Y"))
for(i in 1:nrow(master_table)){
  if(master_table$FRIENDS_COUNT[i] > 0){
    master_table$FRIEND_FLG[i] <- "Y"
  }
}
master_table <- master_table[, -31]

############# convert some numeric columns to factors ##################
str(master_table)
'''
NO_TIMES_DELINQUENT_IN_365_DAYS
NO_OF_DEPENDANTS
PRODUCT_ID
CHANNEL_ID
SUB_CHANNEL_ID
GENDER_AGE_ID
SIZE_ID
COLOR_ID
CANCEL_REASON_CODE
RES_PH_CD
'''
num_col <- c("NO_TIMES_DELINQUENT_IN_365_DAYS", "NO_OF_DEPENDANTS",
             "PRODUCT_ID", "CHANNEL_ID", "SUB_CHANNEL_ID", "GENDER_AGE_ID",
             "SIZE_ID", "COLOR_ID", "CANCEL_REASON_CODE", "RES_PH_CD")
master_table[, num_col] <- lapply(master_table[, num_col], factor)

########## remove all numerics ##########
'''
master_table <- subset(master_table, select = -c(FBCK_FOLLOWING_CNT, FBCK_FOLLOWER_CNT,
                                                 TWT_FOLLOWER_CNT, TWT_FOLLOWING_CNT,
                                                 NO_OF_ITEMS, DISCOUNT_PERCENTAGE,
                                                 QUANTITY_RETURNED, NO_OF_ITEMS_AFTER_RETURN,
                                                 ORDER_RETURNPAID_GROSS_AMT, EXP))
'''
summarizeColumns(master_table)  # 165 NAs in MEMBERSHIP_SC
table(master_table$MEMBERSHIP_SC)
sum(is.na(master_table$MEMBERSHIP_SC))

na_ind <- sample(which(is.na(master_table$MEMBERSHIP_SC)), size = length(which(is.na(master_table$MEMBERSHIP_SC))))

# BRZ
brz_size <- floor((length(which(master_table$MEMBERSHIP_SC=="BRZ"))*sum(is.na(master_table$MEMBERSHIP_SC)))/nrow(master_table))
#brz_ind <- sample(which(is.na(master_table$MEMBERSHIP_SC)), size = smp_size)

# GLD
gld_size <- floor((length(which(master_table$MEMBERSHIP_SC=="GLD"))*sum(is.na(master_table$MEMBERSHIP_SC)))/nrow(master_table))
#gld_ind <- sample(which(is.na(master_table$MEMBERSHIP_SC)), size = smp_size)

# PLT
plt_size <- floor((length(which(master_table$MEMBERSHIP_SC=="PLT"))*sum(is.na(master_table$MEMBERSHIP_SC)))/nrow(master_table))
#plt_ind <- sample(which(is.na(master_table$MEMBERSHIP_SC)), size = smp_size)

# SLV
#slv_size <- floor((length(which(master_table$MEMBERSHIP_SC=="SLV"))*sum(is.na(master_table$MEMBERSHIP_SC)))/nrow(master_table))
#slv_ind <- sample(which(is.na(master_table$MEMBERSHIP_SC)), size = smp_size)

nas <- which(is.na(master_table$MEMBERSHIP_SC))
j <- 1
for(i in 1:brz_size){
  master_table$MEMBERSHIP_SC[nas[j]] <- "BRZ"
  j <- j+1
}

for(i in 1:gld_size){
  master_table$MEMBERSHIP_SC[nas[j]] <- "GLD"
  j <- j+1
}

for(i in 1:plt_size){
  master_table$MEMBERSHIP_SC[nas[j]] <- "PLT"
  j <- j+1
}

for(i in 1:slv_size){
  master_table$MEMBERSHIP_SC[nas[j]] <- "SLV"
  j <- j+1
}
nas <- which(is.na(master_table$MEMBERSHIP_SC))
for(i in nas){
  if(i %% 2 == 0){
    master_table$MEMBERSHIP_SC[i] <- "BRZ"
  }
  else{
    master_table$MEMBERSHIP_SC[i] <- "GLD"
  }
}
########### split into train and test ##############
smp_size <- floor(0.7 * nrow(master_table))
set.seed(100)
train_ind <- sample(seq_len(nrow(master_table)), size = smp_size)

train <- master_table[train_ind, ]
test <- master_table[-train_ind, ]

########## create model ##############
#create a task
traintask <- makeClassifTask(data = train, target = "FRAUD") 
testtask <- makeClassifTask(data = test, target = "FRAUD")

# normalise variables
traintask <- normalizeFeatures(traintask,method = "standardize")
testtask <- normalizeFeatures(testtask,method = "standardize")

#Feature importance
im_feat <- generateFilterValuesData(traintask, method = c("information.gain","chi.squared"))
plotFilterValues(im_feat,n.show = 20)

#create learner
bag <- makeLearner("classif.rpart", predict.type = "response")
bag.lrn <- makeBaggingWrapper(learner = bag, bw.iters = 400, bw.replace = TRUE)

#set 3 fold cross validation
rdesc <- makeResampleDesc("CV", iters=3L)

r <- resample(learner = bag.lrn , task = traintask, resampling = rdesc, 
              measures = list(tpr,fpr,tnr,fnr,acc) ,show.info = T)
##################
'''
#impute missing values by mean and mode
imp <- impute(train, classes = list(factor = imputeMode(), integer = imputeMean()), dummy.classes = c("integer","factor"), dummy.type = "numeric")
imp1 <- impute(test, classes = list(factor = imputeMode(), integer = imputeMean()), dummy.classes = c("integer","factor"), dummy.type = "numeric")

imp_train <- imp$data
imp_test <- imp1$data
'''

########################
######### random forest #############
#create a learner
rf <- makeLearner("classif.randomForest", predict.type = "response",
                  par.vals = list(ntree = 200, mtry = 3))
rf$par.vals <- list(importance = TRUE)

#set tunable parameters
rf_param <- makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)

#random search for 50 iterations
rancontrol <- makeTuneControlRandom(maxit = 50L)

#set 3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

#hypertuning
rf_tune <- tuneParams(learner = rf, resampling = set_cv, task = traintask, par.set = rf_param, control = rancontrol, measures = acc)
