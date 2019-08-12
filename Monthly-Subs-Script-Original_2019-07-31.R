
#   JUNE 2019 (Q2-'19) Monthly Subs Report Filtering Methodology by Brand/File   #

library(lubridate)
library(plyr)
library(readr)


filename_date<-'2019-07-31'
raw_msr_subs_directory<-'//corp.endurance.com/acl/data/SubscriberReports/'
act_date<-'2019-07-31'
exp_date<-'2019-08-01'
   

#####   1) Bluehost  ####

bh_path <- raw_msr_subs_directory
files <- list.files(path=bh_path, pattern="([Bb]lue[Hh]ost).*(*csv)")

for(bh_file in files)
{
   perpos <- which(strsplit(bh_file, "")[[1]]==".")
   assign(
      gsub(" ","",substr(bh_file, 1, perpos-1)),
      read_csv(paste(bh_path,bh_file,sep="")
               , quote = "\'"
               , col_types = cols(`Product Term Duration` = col_double(),
                                  `Cancellation Date` = col_date(format = '%Y-%m-%d'))))
}

h<-get(ls(pattern = 'Hosting'))
a<-get(ls(pattern = 'Addon'))

h.active<-h[h$`Subscription Status` == 'Active',]
a.active<-a[a$`Subscription Status` == 'Active',]

paste("Range of `Activation Date` in active hosting population is:", range(h.active$`Activation Date`))
paste("Range of `Term Expiration Date` in active hosting population is:", range(h.active$`Term Expiration Date`))
paste("Total # of hosting records where `Last Payment is less than or equal to $0.00:", length(which(h.active$`Last Payment Amount` <= 0)))
paste("Total # of NULL values for hosting `Product Term Duration`:", sum(is.na(h.active$`Product Term Duration`)))

paste("Range of `Activation Date` in active addon population is:", range(a.active$`Activation Date`))
paste("Range of `Term Expiration Date` in active addon population is:", range(a.active$`Term Expiration Date`))
paste("Total # of addon records where `Last Payment is less than or equal to $0.00:", length(which(a.active$`Last Payment Amount` <= 0)))
paste("Total # of NULL values for addon `Product Term Duration`:", sum(is.na(a.active$`Product Term Duration`)))
paste("Total # of NULL values for addon `Term Expiration Date`:", sum(is.na(a.active$`Product Term Duration`)))

a.active<-a.active[!is.na(a.active$`Term Expiration Date`),]

bh.addon.list<-split(a.active, a.active$Property)

# Might need to re-think this as next step to select latest 'Activation Date' of 'Account ID' before taking distinct record...

addon.bluehost<-as.data.frame(bh.addon.list[1])
addon.bluehost.unique<-subset(addon.bluehost, !duplicated(`bluehost.Account.ID`))

addon.fastdomain<-as.data.frame(bh.addon.list[2])
addon.fastdomain.unique<-subset(addon.fastdomain, !duplicated(`fastdomain.Account.ID`))

addon.hostmonster<-as.data.frame(bh.addon.list[3])
addon.hostmonster.unique<-subset(addon.hostmonster, !duplicated(`hostmonster.Account.ID`))

addon.justhost<-as.data.frame(bh.addon.list[4])
addon.justhost.unique<-subset(addon.justhost, !duplicated(`justhost.Account.ID`))

###  Now do the same thing for Hosting

bh.hosting.list<-split(h.active, h.active$Property)

hosting.bluehost<-as.data.frame(bh.hosting.list[1])
hosting.bluehost.unique<-subset(hosting.bluehost, !duplicated(`bluehost.Account.ID`))

hosting.fastdomain<-as.data.frame(bh.hosting.list[2])
hosting.fastdomain.unique<-subset(hosting.fastdomain, !duplicated(`fastdomain.Account.ID`))

hosting.hostmonster<-as.data.frame(bh.hosting.list[3])
hosting.hostmonster.unique<-subset(hosting.hostmonster, !duplicated(`hostmonster.Account.ID`))

hosting.justhost<-as.data.frame(bh.hosting.list[4])
hosting.justhost.unique<-subset(hosting.justhost, !duplicated(`justhost.Account.ID`))

# Now look up unique, active, Add-on Account IDs NOT in Hosting -- PER PROPERTY.  If don't exist count as LWP.

BH.lwp<-addon.bluehost.unique[!addon.bluehost.unique$bluehost.Account.ID %in% hosting.bluehost.unique$bluehost.Account.ID,]
FD.lwp<-addon.fastdomain.unique[!addon.fastdomain.unique$fastdomain.Account.ID %in% hosting.fastdomain.unique$fastdomain.Account.ID,]
HM.lwp<-addon.hostmonster.unique[!addon.hostmonster.unique$hostmonster.Account.ID %in% hosting.hostmonster.unique$hostmonster.Account.ID,]
JH.lwp<-addon.justhost.unique[!addon.justhost.unique$justhost.Account.ID %in% hosting.justhost.unique$justhost.Account.ID,]

dim(BH.lwp)
dim(FD.lwp)
dim(HM.lwp)
dim(JH.lwp)

# rename columns for each of the 4 respective brands' lwp dataframes to combine into one final bluehost lwp dataframe

colnames(BH.lwp)<-c('AccountID', 'SubscriptionID', 'Property', 'ProductType', 'ActivationDate', 'LastAuthorizationDate',
                    'LastPaymentAmount', 'ProductTermDuration', 'CancellationDate', 'SubscriptionStatus', 'ProductDescription', 'TermExpirationDate', 'NGS', 
                    'InitialSubscriptionID', 'CurrentSubscriptionID', 'Username',
                    'OfferID', 'DomainHostingBundle')

colnames(FD.lwp)<-c('AccountID', 'SubscriptionID', 'Property', 'ProductType', 'ActivationDate', 'LastAuthorizationDate',
                    'LastPaymentAmount', 'ProductTermDuration', 'CancellationDate', 'SubscriptionStatus', 'ProductDescription', 'TermExpirationDate', 'NGS', 
                    'InitialSubscriptionID', 'CurrentSubscriptionID', 'Username',
                    'OfferID', 'DomainHostingBundle')

colnames(HM.lwp)<-c('AccountID', 'SubscriptionID', 'Property', 'ProductType', 'ActivationDate', 'LastAuthorizationDate',
                    'LastPaymentAmount', 'ProductTermDuration', 'CancellationDate', 'SubscriptionStatus', 'ProductDescription', 'TermExpirationDate', 'NGS', 
                    'InitialSubscriptionID', 'CurrentSubscriptionID', 'Username',
                    'OfferID', 'DomainHostingBundle')

colnames(JH.lwp)<-c('AccountID', 'SubscriptionID', 'Property', 'ProductType', 'ActivationDate', 'LastAuthorizationDate',
                    'LastPaymentAmount', 'ProductTermDuration', 'CancellationDate', 'SubscriptionStatus', 'ProductDescription', 'TermExpirationDate', 'NGS', 
                    'InitialSubscriptionID', 'CurrentSubscriptionID', 'Username',
                    'OfferID', 'DomainHostingBundle')

total_lwp<-rbind(BH.lwp, FD.lwp, HM.lwp, JH.lwp)
dim(total_lwp)
table(total_lwp$Property)
table(h.active$Property)

# Exclude test accounts that we started excluding in June, 2019.

bh_test_accounts <- read_csv("D:/Subs files/2019Q2 - Subs Data - Jun/validation and activity scripts/bh_test_cards_201907241413.csv")

# Create composite key of 'Account ID' and 'Property' across all 3 files - hosting, lwp, and test accounts file

bh_test_accounts$pk<-paste0(bh_test_accounts$`Customer ID`, 'bluehost')

h.active$pk<-paste0(h.active$`Account ID`, h.active$Property)
total_lwp$pk<-paste0(total_lwp$AccountID, total_lwp$Property)

h.final<-h.active[!h.active$pk %in% bh_test_accounts$pk, ]

total_lwp_final<-total_lwp[!total_lwp$pk %in% bh_test_accounts$pk, ]

# Export final filtered files - Hosting & LWP

setwd('D:/Subs files/2019Q3 - Subs Data - Jul/filtered/')

h2 = '2019-07-31'

write_csv(total_lwp_final, paste0('bh_lwp_msr_', h2, '_filtered.csv'))
write_csv(h.final, paste0('bh_hosting_msr_', h2 ,'_filtered.csv'))

#  Legacy eHost Subs - Need to include in BH Hosting #

ehost<-read_csv('D:/Subs files/2019Q3 - Subs Data - Jul/data/ehost-justhost_msr_2019-06-30_filtered.csv',
                col_names = T, cols(`Term Expiration Date` = col_date(format = '%m/%d/%Y'),
                                    `Activation Date` = col_date(format = '%m/%d/%Y'),
                                    `Cancellation Date` = col_date(format = '%m/%d/%Y')))

ehost$`Cancellation Date`[which(is.na(ehost$`Cancellation Date`))] <- as.Date('1970-01-01')

ehost.active<-ehost[(ehost$`Term Expiration Date` >= as.Date('2019-08-01')),]

ehost.final<-ehost.active[complete.cases(ehost.active),]

dim(ehost.final)

write_csv(ehost.final, paste0('ehost-justhost_msr_', h2, '_filtered.csv'))


# Using rbind to combine bluehost hosting and ehost-justhost filtered subscriber lists

bh_hosting_msr_filtered <- read_csv("D:/Subs files/2019Q3 - Subs Data - Jul/filtered/bh_hosting_msr_2019-07-31_filtered_v1.csv")

#ehost_justhost_msr_2019_06_30_filtered <- read_csv("D:/Subs files/2019Q2 - Subs Data - Jul/filtered/ehost-justhost_msr_2019-07-31_filtered.csv")

ehost<-ehost.final

ehost$`NGS`<-NA
ehost$`Initial Subscription ID`<-NA
ehost$`Current Subscription ID`<-NA
ehost$`User Name`<-NA
ehost$`Offer ID`<-NA
ehost$`Domain Hosting Bundle`<-NA
ehost$`Activation Date`<-as.Date(ehost$`Activation Date`, format = '%m/%d/%Y')
ehost$`Last Authorization Date`<-as.Date(ehost$`Last Authorization Date`, format = '%m/%d/%Y')
ehost$`Term Expiration Date`<-as.Date(ehost$`Term Expiration Date`, format = '%m/%d/%Y')
ehost$pk<-paste0(ehost$`Account ID`, ehost$Property)

bh_hosting_total<-rbind(bh_hosting_msr_filtered, ehost)

# with ehost included:
write_csv(bh_hosting_total, paste0('bh_hosting_msr_', h2, '_filtered.csv'))


## BH Q2-'19 Adjustment to remove Test Accounts from Bluehost Property

bh_test_cards_201907241413 <- read_csv("D:/Subs files/2019Q2 - Subs Data - Jun/validation and activity scripts/bh_test_cards_201907241413.csv")
bh_hosting_msr_2019_06_30_filtered <- read_csv("D:/Subs files/2019Q2 - Subs Data - Jun/filtered/auditors/bh_hosting_msr_2019-06-30_filtered.csv")
bh_lwp_msr_2019_06_30_filtered <- read_csv("D:/Subs files/2019Q2 - Subs Data - Jun/filtered/auditors/bh_lwp_msr_2019-06-30_filtered.csv")

bh_msr<-bh_hosting_msr_2019_06_30_filtered
bh_lwp<-bh_lwp_msr_2019_06_30_filtered
test<-bh_test_cards_201907241413

test_in_bh_hosting<-bh_msr[bh_msr$`Account ID` %in% test$`Customer ID`,]

test_in_bh_lwp<-bh_lwp[bh_lwp$AccountID %in% test$`Customer ID`,]

bh_hosting_test<-test_in_bh_hosting[test_in_bh_hosting$Property == 'bluehost',]
bh_lwp_test<-test_in_bh_lwp[test_in_bh_lwp$Property == 'bluehost',]


dim(bh_hosting_test)
dim(bh_lwp_test)

#write_csv(bh_hosting_test, 'bh_hosting_test_accounts_2019-06-30.csv')
#write_csv(bh_lwp_test, 'bh_lwp_test_accounts_2019-06-30.csv')

### Removing the 1,677 from June-ending Hosting & LWP Subs Reports:

# 1) Create a primary key in all 3 files consisting of 'Account ID' + 'Property'

bh_msr$pk<-paste0(bh_msr$`Account ID`, bh_msr$Property)

bh_lwp$pk<-paste0(bh_lwp$AccountID, bh_lwp$Property)

bh_lwp_test$pk<-paste0(bh_lwp_test$AccountID, bh_lwp_test$Property)

bh_hosting_test$pk<-paste0(bh_hosting_test$`Account ID`, bh_hosting_test$Property)

# Run outer joins between 'test accounts' & 'active subscriber lists' w/ test accounts included.

bh_hosting_new<-bh_msr[!bh_msr$pk %in% bh_hosting_test$pk,]

bh_lwp_new<-bh_lwp[!bh_lwp$pk %in% bh_lwp_test$pk,]

# Export revised BH Group June-ending active subscriber lists

write_csv(bh_hosting_new, 'bh_hosting_msr_2019-06-30_filtered.csv')
write_csv(bh_lwp_new, 'bh_lwp_msr_2019-06-30_filtered.csv')

dim(bh_hosting_new)
dim(bh_lwp_new)


# how to escape single quotes: x<-test_import_singlequotes[test_import_singlequotes$product == '\'kind\'',]

#       To calculate 'Office 365' LWP in Monthly Subs Reports:       ###

#office_addons<-a.active[a.active$`Product Description` %in% c()]

a.active$grep_office<-grepl('.*[Oo]ffice.*', a.active$`Product Description`)

active_addon_office_grep_df<-a.active[a.active$grep_office == T,]

addon_dupes<-active_addon_office_grep_df[c('Account ID', 'Property')]

distinct_office_addons<-active_addon_office_grep_df[!duplicated(addon_dupes),]
distinct_office_addons$pk<-paste0(distinct_office_addons$`Account ID`, distinct_office_addons$Property)

h.active$pk<-paste0(h.active$`Account ID`, h.active$Property)
# Look up those with active Hosting subscriptions:

office_lwp<-distinct_office_addons[!distinct_office_addons$pk %in% h.active$pk,]

#####   2) vDeck     ####
# Updated: 7/10/18
# Need to take a distinct count of "Account ID" in vDeck Hosting Monthly Subs Report to avoid counting subs who have both a paid hosting offer and a free hosting paid domain offer.
h_final<-subset(h, !duplicated(`AccountID`))
#  MAY 2018  #

vdeck_path <- raw_msr_subs_directory

files <- list.files(path=raw_msr_subs_directory, pattern="([Vv][Dd]eck.*)(*csv)")

for(vdeck_file in files)
{
   perpos <- which(strsplit(vdeck_file, "")[[1]]==".")
   assign(
      gsub(" ","",substr(vdeck_file, 1, perpos-1)),
      read_csv(paste(vdeck_path,vdeck_file,sep=""), col_names = T,
               cols(`Cancellation Date` = col_date(format = '%Y-%m-%d'),
                    `Activation Date` = col_date(format = '%Y-%m-%d'),
                    `Last Authorization Date` = col_date(format = '%Y-%m-%d'),
                    `Term Expiration Date` = col_date(format = '%Y-%m-%d')),
               quote = "\""))
}

# Assigns the right starting variables to be used in the existing MSR filtered script

h<-get(ls(pattern = 'vDeck_Hosting'))
a<-get(ls(pattern = 'vDeck_Addon'))

report_date <- '2019-07-31'

#vdeck_free_hosting_offers<-read_csv('C:/Users/aljackson/Documents/Brands analysis/vDeck/free_hosting_marked_as_count_membership.csv', col_names = T)
vdeck_brand_mapping<-read_csv('C:/Users/aljackson/Documents/Brands analysis/vDeck/vdeck-hosting mapping_20170831CSV.csv', col_names = T)

#h<-join(h, vdeck_free_hosting_offers, by = 'OfferID')
h<-join(h, vdeck_brand_mapping, by = 'Property')
a<-join(a, vdeck_brand_mapping, by = 'Property')

h<-h[h$`Subscription Status` == 'Active',]
a<-a[a$`Subscription Status` == 'Active',]

h$days_until_ngs<-h$`Term Expiration Date` - as.Date('2019-07-31')
a$ted_in_range<-a$`Term Expiration Date` - as.Date('2019-07-31')
length(which(h$ngs <= -45))
length(which(a$ngs <= -45))
length(which(h$`Activation Date` >= '2019-08-01'))
length(which(a$`Activation Date` >= '2019-08-01'))
# LWP

h_final<-subset(h, !duplicated(`Account ID`))

all_lwp<-a[!a$`Account ID` %in% h_final$`Account ID`,]

unique_all_lwp<-subset(all_lwp, !duplicated(`Account ID`))
# Free Hosting + Paid domain bundles
free_hosting<-h_final[h_final$`Domain Hosting Bundle` == 1,]
#free_hosting<-h[h$OfferID %in% vdeck_free_hosting_offers$OfferID,]
true_hosting<-h_final[h_final$`Domain Hosting Bundle` == 0,]

vdeck_hosting<-rbind(true_hosting, free_hosting)

# drop calculated fields used to audit report

filtered_lwp <- unique_all_lwp[,1:19]
filtered_hosting <- vdeck_hosting[,1:19]

write_csv(filtered_lwp, paste0('vdeck_lwp_msr_', report_date, '_filtered.csv'))
write_csv(filtered_hosting, paste0('vdeck_hosting_msr_', report_date, '_filtered.csv'))
#write_csv(free_hosting, 'vdeck_domain_hosting_msr_2019-03-31_filtered.csv')

# create loop or lapply for this step -- combine values into one .csv to export instead of 3 separate ones (fix)

v_lwp<-as.data.frame(table(unique_all_lwp$BrandGroup))
v_lwp$SubsCategory<-'LWP'

v_host<-as.data.frame(table(true_hosting$BrandGroup))
v_host$SubsCategory<-'Hosting'

v_free<-as.data.frame(table(free_hosting$BrandGroup))
v_free$SubsCategory<-'Domain-Hosting'

v_all<-rbind(v_lwp, v_host, v_free)

write_csv(v_all, paste0('vdeck_subs-summary_', report_date, '_filtered.csv'))
#####   3) Hostgator ####

#hg<-read_csv('//corp.endurance.com/acl/data/SubscriberReports/HostGator-Monthly-Subs-Report_2018-09-30_2018- 10-01_14-22- 10.csv', col_names = T)
library(readr)

report_date<-'2019-07-31'
hg<-read_csv('D:/Subs files/2019Q3 - Subs Data - Jul/data/HostGator_Monthly-Subs-Report_2019-07-31.csv', col_names = T,
             cols(`Term Expiration Date` = col_character()))


hg$`Term Expiration Date`<-as.Date(hg$`Term Expiration Date`, format = '%Y-%m-%d')
h<-hg
h$exp<-h$`Term Expiration Date` + 45

range(h$`Activation Date`, na.rm=T)
range(h$`Term Expiration Date`, na.rm=T)
range(h$`Last Payment Amount`, na.rm=T)

# Check dates and 'NA' values for 'Term Expiration Date' !!!
h<-h[(h$`Activation Date` <= as.Date('2019-07-31'))
     & (h$exp >= as.Date('2019-08-01'))
     & ((h$`Cancellation Date` >= as.Date('2019-08-01')) | is.na(h$`Cancellation Date`)) 
     & (h$`Activation Date` >= as.Date('1975-01-01'))
     & (h$`Subscription Status` %in% c('Active')),]


h<-h[!(h$`Account ID` %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 17, 125, 1659, 5642, 40031, 94554, 131809, 530253, 751107, 754766, 758780, 770349, 836728, 909190, 1015855, 1146548, 1565360, 1626100, 1626101, 1629637, 1647898, 1655940, 1691847, 1783311, 1798383, 2068588, 2094015, 2126693, 2136307, 2188810, 2285687, 2289349, 2300497, 2343934, 2346532, 2347251, 2352297, 2445300, 2467523, 2479157, 2488507, 2500109, 2542142, 2687864, 2715364, 2789127, 2862557, 2934070, 3000558, 3187005, 3323109, 3333936, 1576948, 1497949, 993717, 1576948, 1497949, 3134856)),]

filtered<-h[complete.cases(h[,1:2]),]

dupes<-filtered[c('Account ID', 'Subscription ID')]

final<-filtered[!duplicated(dupes),]

table(final$`Subscription Status`)

setwd('D:/Subs files/2019Q2 - Subs Data - Jun/filtered/')

write_csv(final, paste0('hg_hosting_msr_', report_date, '_filtered.csv'))


# HG LWP #

lwp_file<-'D:/Subs files/2019Q3 - Subs Data - Jul/data/HG_lwp_extended_basis-all-2019-07.csv'
hosting<-hg_hosting_msr_2019_07_31_filtered


lwp<-read_csv(lwp_file, col_names = T, cols(`privacy_start_date` = col_date(format = '%Y-%m-%d'),
                                            `lwp_cancel_date` = col_date(format = '%Y-%m-%d'),
                                            `last_payment_date` = col_date(format = '%Y-%m-%d'),
                                            `privacy_cancel_date` = col_date(format = '%Y-%m-%d'),
                                            `term_expiration_date` = col_date(format = '%Y-%m-%d'),
                                            `final_privacy_cancel` = col_date(format = '%Y-%m-%d'),
                                            `first_hosting_start` = col_date(format = '%Y-%m-%d'),
                                            `last_hosting_cancel` = col_date(format = '%Y-%m-%d')))

lwp$exp<-lwp$term_expiration_date + 45

# Filters -->
# Exclude 'Cancelled' records from `lwp_status` column:
# Check 'privacy_start_date' range

# change
lwp<-lwp[(lwp$lwp_status == 'Active')
         & (lwp$privacy_start_date <= as.Date('2019-07-31'))
         & (lwp$exp >= as.Date('2019-08-01'))
         & (is.na(lwp$final_privacy_cancel)),]

lwp<-lwp[complete.cases(lwp[,1:2]),]

lwp_filtered<-subset(lwp, !duplicated(client_id))

# Last step: Exclude any client_ID in filtered HG Hosting Report NOT in filtered LWP report

#hosting<-read_csv(filtered_hosting_file, col_names = T, cols(`Cancellation Date` = col_character()))

true_lwp<-lwp_filtered[!lwp_filtered$client_id %in% hosting$`Account ID`,]

final_lwp<-true_lwp[complete.cases(true_lwp[,1:2]),]

dim(final_lwp)

setwd('D:/Subs files/2019Q3 - Subs Data - Jul/filtered')

report_date = '2019-07-31'

write_csv(final_lwp, paste0('hg_lwp_msr_', report_date, '_filtered.csv'))



#####   4) CTCT      ####

#  CTCT NGS Period is 75 days as of 9/30/2018 -- Last updated: 1/2/2019
#corp<-'//corp.endurance.com/acl/data/SubscriberReports/ConstantContact_Monthly-Subs-Report_2019-04-30.csv'
#path<-'D:/Subs files/2019Q1 - Subs Data - Jan/ConstantContact_Monthly-Subs-Report_2019-01-31.csv'
#ctct<-read_csv(corp, col_names = T, cols(`Account ID` = col_character()))
## CTCT Import & Filters Syntax   ###
# Remember to include in import syntax double quote text qualifiers because CTCT MSR has double quotes around almost every value even empty ones.

library(readr)
ctct_raw <- read_csv(paste0(raw_msr_subs_directory, 'ConstantContact_Monthly-Subs-Report_', filename_date, '.csv'),
                     quote = "\"", col_types = cols(`Account ID` = col_character(),                                                                            `Activation Date` = col_date(format = "%Y-%m-%d"), 
                                                    `Cancellation Date` = col_date(format = "%Y-%m-%d"), 
                                                    `Current Subscription ID` = col_character(), 
                                                    `Initial Subscription ID` = col_character(), 
                                                    `Last Authorization Date` = col_date(format = "%Y-%m-%d"), 
                                                    `Offer ID` = col_character(), `Subscription ID` = col_character(), 
                                                    `Term Expiration Date` = col_date(format = "%Y-%m-%d"), 
                                                    `User Name` = col_character()))


ctct_raw$Exp<-ctct_raw$`Term Expiration Date` + 75

ctct_active<-ctct_raw[(ctct_raw$`Subscription Status` == 'Active') & (ctct_raw$Exp >= as.Date(exp_date)),]

# Removing the above 16 SOIDs manually as of Q2'19 close. 

ctct_filtered<-ctct_active[!(ctct_active$`Account ID` %in% c('-1', '1129449717503',
                                                             '1128868941064', '1126500734464', '1129999256902', '1126431725025',
                                                             '1125716911198', '1131967340335', '1118749797967', '1105463465900', 
                                                             '1131671591037', '1130425605901', '1113679002383', '1131418661397',
                                                             '1132381993199', '1114582898831')),]

ctct_final<-ctct_filtered[complete.cases(ctct_filtered[,c('Account ID')]),]

range(ctct_final$`Exp`, na.rm=T)
length(which(is.na(ctct_final$`Last Payment Amount`)))

ctct_final$`Account ID` <- as.numeric(ctct_final$`Account ID`)

#setwd('D:/Subs files/2019Q2 - Subs Data - Jun/filtered/')

write_csv(ctct_final, 'ctct_msr_2019-07-31_filtered.csv')


#####   5) WHMCS     ####

# Part 1 - Regular filtering process

library(plyr)
library(readr)

seowh_productkey_as_of_2017_11_10<-read_csv('D:/Subs files/Analysis/whmcs/seowh_productkey_as_of_2017_11_10.csv')
seoh_productkey_as_of_2017_11_10<-read_csv('D:/Subs files/Analysis/whmcs/seoh_productkey_as_of_2017_11_10.csv')
ctx_productkey_as_of_2017_11_10<-read_csv('D:/Subs files/Analysis/whmcs/ctx_productkey_as_of_2017_11_10.csv')
h9_productkey_as_of_2017_11_10<-read_csv('D:/Subs files/Analysis/whmcs/H9_productkey_as_of_2017_11_10.csv')
s5_productkey_as_of_2017_11_10<-read_csv('D:/Subs files/Analysis/whmcs/s5_productkey_as_of_2017_11_10.csv')
aso_productkey_as_of_2017_11_10<-read_csv('D:/Subs files/Analysis/whmcs/aso_productkey_as_of_2017_11_10.csv')
arvx_productkey_as_of_2017_11_10<-read_csv('D:/Subs files/Analysis/whmcs/arvx_productkey_as_of_2017_11_10.csv')

seowh<-read_csv('//corp.endurance.com/acl/data/SubscriberReports/SEOWH_Monthly-Subs-Report_2019-07-31.csv', col_names = T)
aso<-read_csv('//corp.endurance.com/acl/data/SubscriberReports/ASO_Monthly-Subs-Report_2019-07-31.csv', col_names = T)
arvx<-read_csv('//corp.endurance.com/acl/data/SubscriberReports/ARVX_Monthly-Subs-Report_2019-07-31.csv', col_names = T, cols(`Term Expiration Date` = col_date(format = '%Y-%m-%d')))
site5<-read_csv('//corp.endurance.com/acl/data/SubscriberReports/S5_Monthly-Subs-Report_2019-07-31.csv', col_names = T)
ctx<-read_csv('//corp.endurance.com/acl/data/SubscriberReports/CTX_Monthly-Subs-Report_2019-07-31.csv', col_names = T)
h9<-read_csv('//corp.endurance.com/acl/data/SubscriberReports/H9_Monthly-Subs-Report_2019-07-31.csv', col_names = T)
seoh<-read_csv('//corp.endurance.com/acl/data/SubscriberReports/SEOH_Monthly-Subs-Report_2019-07-31.csv', col_names = T)


#s5<-read_csv("D:/Subs files/2019Q2 - Subs Data - Jun/data/S5_Monthly-Subs-Report_2019-07-31.csv", 
#             col_types = cols(`Account ID` = col_character(),                                                                           `Activation Date` = col_date(format = "%Y-%m-%d"), 
#                             `Cancellation Date` = col_date(format = "%Y-%m-%d"), 
#                            `Last Authorization Date` = col_date(format = "%Y-%m-%d"), 
#                           `Term Expiration Date` = col_date(format = "%Y-%m-%d")))


a<-h9

aa<-h9_productkey_as_of_2017_11_10

a2<-join(a, aa, by = 'Product Description')

a2$pk2<-paste(a2$ProductDescriptor, a2$`Product Description`)

a2$Match<-ifelse(a2$pk2 == a2$productkey, "Yes", "No")

dupes<-a2[c('Account ID', 'Subscription ID')]

a3<-a2[!duplicated(dupes),]
table(a3$Match)

a3$`ExpDate`<-a3$`Term Expiration Date` + 21

# UPDATE DATE !!
a3$`Last Auth Date in Range`<-ifelse((a3$`Last Authorization Date` + a3$`Product Term Duration` * 30.42 + 21) >= as.Date('2019-07-01'), "Yes","No")

#a3$`LastAuthDateChk`<-a3$`Last Authorization Date` + a3$`Product Term Duration` * 30.42 + 21

# Change measurement dates
a4<-a3[(a3$`Activation Date` <= as.Date('2019-07-31')) 
       & (a3$`Activation Date` > as.Date('1988-12-31'))
       & (a3$`ExpDate` >= as.Date('2019-08-01')) 
       & (a3$`Last Payment Amount` > 0) 
       & (a3$`Subscription Status` %in% c('active', 'suspended')),]

a4.hosting<-a4[(a4$finance_category == 'hosting'),]

a4.addon<-a4[(a4$finance_category == 'addon'),]

a4.unique<-subset(a4.addon, !duplicated(`Account ID`))

a4.lwp<-a4.unique[!a4.unique$`Account ID` %in% a4.hosting$`Account ID`,]

a.total<-rbind(a4.hosting, a4.lwp)

dim(a.total)
sum(is.na(a.total$`Account ID`))

final<-a.total[complete.cases(a.total[,1:2]),]
dim(final)
table(final$Match)

sum(is.na(final$`Account ID`))
sum(!is.na(final$`Cancellation Date`))
sum(is.na(final$`Term Expiration Date`))

write_csv(final, 'h9_msr_2019-07-31_Round1_filtered.csv')

setwd('D:/Subs files/2019Q3 - Subs Data - Jul/whmcs/')

#    Create a subset of the data that is "No" for `Last Auth Date in Range` and send to Jeremy, 
#    but the file needs to have an additional field called `Brand` which Jeremy needs in his script to look these up.

library(readr)
library(plyr)

path <- 'D:/Subs files/2019Q3 - Subs Data - Jul/whmcs/'
files <- list.files(path=path, pattern="*csv")
for(file in files)
{
   perpos <- which(strsplit(file, "")[[1]]==".")
   assign(
      gsub(" ","",substr(file, 1, perpos-1)),
      read_csv(paste(path,file,sep="")))
}

whmcs<-list(`arvx_msr_2019-07-31_Round1_filtered`, `seoh_msr_2019-07-31_Round1_filtered`,
            `seowh_msr_2019-07-31_Round1_filtered`, `s5_msr_2019-07-31_Round1_filtered`,
            `ctx_msr_2019-07-31_Round1_filtered`, `h9_msr_2019-07-31_Round1_filtered`,
            `aso_msr_2019-07-31_Round1_filtered`)

grab_column<-function(df){
   df[df$`Last Auth Date in Range` == 'No',]
}

whmcs_no<-lapply(whmcs, grab_column)

whmcs_no_df<-ldply(whmcs_no, data.frame)

whmcs_no_df$Brand<-ifelse(whmcs_no_df$`Property` == 'Arvixe', "ARVX",
                          ifelse(whmcs_no_df$`Property` == "Host9", "H9",
                                 ifelse(whmcs_no_df$`Property` == "A Small Orange", "ASO",
                                        ifelse(whmcs_no_df$`Property` == "Site5", "S5",
                                               ifelse(whmcs_no_df$`Property` == "SEO Hosting", "SEOH",
                                                      ifelse(whmcs_no_df$`Property` == "SEO Web Hosting", "SEOWH",
                                                             ifelse(whmcs_no_df$`Property` == "Cirtex", "CTX",
                                                                    "Error")))))))

write_csv(whmcs_no_df, 'whmcs_subscriber_lookup_2019-07-31.csv')

#     Note to Jeremy      #

#Hi Jeremy,

#Please see attached for the April, 2019, refresh of the data from the Monthly Subs Reports which isolates a subset of subscribers that have a 'Last Authorization Date' way in the past relative to their 'Product Term Duration' and NGS period.  I have added the 'Brand' field for your reference that has the short code for each brand in it.  Please add the following additional fields to the attached file so we can continue filtering the WHMCS monthly subscribers reports:

#  Last Invoice Due Date (LastInvDueDate)
# Subscription Recurring Amount (SubRecurringAmount)
# Last Invoice Payment Amount (LastInvPayAmt)
# Thank you,




# Part 2: Load Jeremy's subs lookup file and filter the results: 

library(readr)

jeremy_path<-"D:/Subs files/2019Q3 - Subs Data - Jul/whmcs/whmcs_subscriber_data_lookup_2019-08-01 14_14_32.csv"

subs_lookup <- read_csv(jeremy_path, col_types = cols(Activation.Date = col_date(format = "%Y-%m-%d"), 
                                                      Cancellation.Date = col_date(format = "%Y-%m-%d"), 
                                                      ExpDate = col_date(format = "%Y-%m-%d"), 
                                                      Last.Authorization.Date = col_date(format = "%Y-%m-%d"), 
                                                      LastInvDueDate = col_date(format = "%Y-%m-%d"), 
                                                      LastInvPayDate = col_datetime(format = "%Y-%m-%d %H:%M:%S")))


#subs_lookup <- read_csv("D:/Subs files/2019Q2 - Subs Data - Apr/data/SubscribersReports/whmcs/subscriber_data_lookup_2019-05-01 21_22_03.csv", 
#                        col_types = cols(Activation.Date = col_date(format = "%m/%d/%Y"), 
#                                        Cancellation.Date = col_date(format = "%m/%d/%Y"), 
#                                       ExpDate = col_date(format = "%m/%d/%Y"), 
#                                      Last.Authorization.Date = col_date(format = "%m/%d/%Y"), 
#                                     LastInvDueDate = col_date(format = "%m/%d/%Y"), 
#                                    LastInvPayDate = col_datetime(format = "%m/%d/%Y %H:%M"), 
#                                   Term.Expiration.Date = col_date(format = "%m/%d/%Y")))

#subs_lookup<-whmcs_subscriber_data_lookup_2019_06_03_21_09_17
# Add "inv_exp_date" calculated field

subs_lookup$inv_exp_date <- subs_lookup$`LastInvDueDate` + (subs_lookup$`Product.Term.Duration` * 30.42 + 21) 

# Include LastInvPayAmt >= 0 or less
# Include SubRecurringAmount > 0 or less
# Include "inv_exp_date" calculated field after or equal to measurement period

subs_lookup_filtered <- subs_lookup[subs_lookup$`LastInvPayAmt` >= 0 
                                    & subs_lookup$`SubRecurringAmount` > 0
                                    & subs_lookup$`inv_exp_date` >= as.Date("2019-08-01"),]


# Import Round 1 filtered WHMCS files
# Exclude everyone who has a "No" for 'Last Auth Date in Range.'
# Add back in those from 'subs_lookup_filtered' dataframe.  

library(plyr)
library(readr)

path <- 'D:/Subs files/2019Q3 - Subs Data - Jul/whmcs/'
files <- list.files(path=path, pattern="*csv")
for(file in files)
{
   perpos <- which(strsplit(file, "")[[1]]==".")
   assign(
      gsub(" ","",substr(file, 1, perpos-1)),
      read_csv(paste(path,file,sep="")))
}

whmcs<-list(`arvx_msr_2019-07-31_Round1_filtered`, `seoh_msr_2019-07-31_Round1_filtered`,
            `seowh_msr_2019-07-31_Round1_filtered`,
            `ctx_msr_2019-07-31_Round1_filtered`, `h9_msr_2019-07-31_Round1_filtered`,
            `aso_msr_2019-07-31_Round1_filtered`)

grab_column<-function(df){
   df[df$`Last Auth Date in Range` == 'Yes',]
}

whmcs_yes<-lapply(whmcs, grab_column)
whmcs_yes_df<-ldply(whmcs_yes, data.frame)

# Include only relevant columns to export and change column names so 'whmcs_yes_df' and 'subs_lookup_filtered' match each other so their rows can be combined.

### whmcs_yes_df


colnames(whmcs_yes_df)<-c('Account ID', 'Subscription ID', 'Property', 'Product Type', 'Activation Date', 'Last Authorization Date', 'Last Payment Amount', 'Product Term Duration',
                          'Cancellation Date', 'Subscription Status', 'Product Description', 'Term Expiration Date', 'Product Descriptor', 'productkey', 'finance_category', 'pk2', 
                          'Match', 'ExpDate', 'Last Auth Date in Range')

whmcs_final<-whmcs_yes_df[,c('Account ID', 'Subscription ID', 'Property', 'finance_category', 'Activation Date', 'Last Authorization Date', 'Last Payment Amount', 'Product Term Duration',
                             'Cancellation Date', 'Subscription Status', 'Product Description', 'Term Expiration Date', 'ExpDate')]


colnames(whmcs_final)<-c('Account ID', 'Subscription ID', 'Property', 'Product Type', 'Activation Date', 'Last Authorization Date', 'Last Payment Amount', 'Product Term Duration',
                         'Cancellation Date', 'Subscription Status', 'Product Description', 'Term Expiration Date', 'ExpDate')

### subs_lookup_filtered

subs_lookup_names<-c('Account.ID', 'Subscription.ID', 'Property', 'finance_category', 'Activation.Date', 'Last.Authorization.Date', 'Last.Payment.Amount',
                     'Product.Term.Duration', 'Cancellation.Date', 'Subscription.Status', 'Product.Description', 'Term.Expiration.Date', 'ExpDate')

subs_lookup_short<-subs_lookup_filtered[subs_lookup_names]

colnames(subs_lookup_short)<-c('Account ID', 'Subscription ID', 'Property', 'Product Type' ,'Activation Date', 'Last Authorization Date', 'Last Payment Amount', 'Product Term Duration',
                               'Cancellation Date', 'Subscription Status', 'Product Description', 'Term Expiration Date', 'ExpDate')

whmcs_all_final<-rbind(whmcs_final, subs_lookup_short)

whmcs_all_final$`Product Type`<-ifelse(whmcs_all_final$`Product Type` == 'hosting', 'Hosting',
                                       ifelse(whmcs_all_final$`Product Type` == 'addon', 'Addon', whmcs_all_final$`Product Type`))

aso<-whmcs_all_final[whmcs_all_final$Property == 'A Small Orange',]
arvx<-whmcs_all_final[whmcs_all_final$Property == 'Arvixe',]
cirtex<-whmcs_all_final[whmcs_all_final$Property == 'Cirtex',]
host9<-whmcs_all_final[whmcs_all_final$Property == 'Host9',]
seoh<-whmcs_all_final[whmcs_all_final$Property == 'SEO Hosting',]
seowh<-whmcs_all_final[whmcs_all_final$Property == 'SEO Web Hosting',]
#site5<-whmcs_all_final[whmcs_all_final$Property == 'Site5',]
# exclude Site5 Account ID #186
#site5<-site5[!site5$`Account ID` %in% c('H186'),]

setwd('D:/Subs files/2019Q3 - Subs Data - Jul/filtered/')

# Check for NA in 'Account ID' & 'Subscription ID' fields before outputting final filtered files.

write_csv(aso, 'aso_msr_2019-07-31_filtered.csv')
write_csv(arvx, 'arvx_msr_2019-07-31_filtered.csv')
write_csv(cirtex, 'ctx_msr_2019-07-31_filtered.csv')
write_csv(host9, 'h9_msr_2019-07-31_filtered.csv')
write_csv(seoh, 'seoh_msr_2019-07-31_filtered.csv')
write_csv(seowh, 'seowh_msr_2019-07-31_filtered.csv')
#write_csv(site5, 's5_msr_2019-07-31_filtered_p1.csv')

# Continue with Site5-IX migration filter process to get final count of subs.

check_na<-function(df){
   sum(is.na(df$`Account ID`))
}

whmcs_list<-list(`aso_msr_2019-06-30_filtered`, `arvx_msr_2019-06-30_filtered`,
                 `ctx_msr_2019-06-30_filtered`, `h9_msr_2019-06-30_filtered`,
                 `seoh_msr_2019-06-30_filtered`, `seowh_msr_2019-06-30_filtered`)

lapply(whmcs_list, dim)

#####   6) Site5 - Part 2   #####

# Site5 - WHMCS Subscriber Close

# High level description of monthly subscriber close for Site5
# 1) Filter Jeremy's Monthly Subs Report
# 2) Filter Jeremy's IX-Migration Monthly Report
# 3) Create subset from filtered 1) that look expired. - send to Jeremy
# 4) Filter the file that comes back from Jeremy and re-include in final filtered file.

# Combine lists 1, 2, and 4 and remove duplicates.

library(readr)
library(plyr)
library(stringr)

s5_productkey<-read_csv('D:/Subs files/Analysis/whmcs/s5_productkey_as_of_2017_11_10.csv')


##  1) Filter Jeremy's Monthly Subs Report         

s5_file_path<-"D:/Subs files/2019Q3 - Subs Data - Jul/data/S5_Monthly-Subs-Report_2019-07-31.csv"

s5_msr_raw <- read_csv(s5_file_path, col_types = cols(`Activation Date` = col_date(format = "%Y-%m-%d"), 
                                                      `Cancellation Date` = col_date(format = "%Y-%m-%d"), 
                                                      `Last Authorization Date` = col_date(format = "%Y-%m-%d"), 
                                                      `Term Expiration Date` = col_date(format = "%Y-%m-%d")))

# output is final msr filtered part 1 subs that have a 'last auth date out of range' excluded
s5_filtered_p1_LAD_InRange_func<-function(s5_msr_raw, ac_date_max, exp_date){
   a<-s5_msr_raw
   aa<-s5_productkey
   a2<-join(a, aa, by = 'Product Description')
   a2$pk2<-paste(a2$ProductDescriptor, a2$`Product Description`)
   a2$Match<-ifelse(a2$pk2 == a2$productkey, "Yes", "No")
   dupes<-a2[c('Account ID', 'Subscription ID')]
   a3<-a2[!duplicated(dupes),]
   table(a3$Match)
   a3$`ExpDate`<-a3$`Term Expiration Date` + 21
   a3$`Last Auth Date in Range`<-ifelse((a3$`Last Authorization Date` + a3$`Product Term Duration` * 30.42 + 21) >= as.Date(exp_date), "Yes","No")
   a4<-a3[(a3$`Activation Date` <= as.Date(ac_date_max)) 
          & (a3$`Activation Date` > as.Date('1988-12-31'))
          & (a3$`ExpDate` >= as.Date(exp_date)) 
          & (a3$`Last Payment Amount` > 0) 
          & (a3$`Subscription Status` %in% c('active', 'suspended'))
          & (a3$`Last Auth Date in Range` == 'Yes'),]
   a4.hosting<-a4[(a4$finance_category == 'hosting'),]
   a4.addon<-a4[(a4$finance_category == 'addon'),]
   a4.unique<-subset(a4.addon, !duplicated(`Account ID`))
   a4.lwp<-a4.unique[!a4.unique$`Account ID` %in% a4.hosting$`Account ID`,]
   a.total<-rbind(a4.hosting, a4.lwp)
   dim(a.total)
   sum(is.na(a.total$`Account ID`))
   final<-a.total[complete.cases(a.total[,1:2]),]
   dim(final)
   table(final$Match)
   return(final)
   # output of final msr filtered part 1 subs that have a 'last auth date out of range' excluded
}

# Remember to enter function arguments !!!
s5_filtered_p1_LAD_excluded<-s5_filtered_p1_LAD_InRange_func(s5_msr_raw = s5_msr_raw, ac_date_max = '2019-07-31',
                                                             exp_date = '2019-08-01')


#         2) Filter Jeremy's IX-Migration Monthly Subs Report         

library(stringr)

s5_productkey<-read_csv('D:/Subs files/Analysis/whmcs/s5_productkey_as_of_2017_11_10.csv')

ix_file_path <- 'D:/Subs files/2019Q3 - Subs Data - Jul/data/ix_migrated_08-01-19.csv'

#ix_mig_raw<-ix_migrated_06_01_19_v2

ix_mig_raw<-read_csv(ix_file_path, 
                     col_types = cols(`Activation Date` = col_date(format = "%Y-%m-%d"), 
                                      `Migration Date` = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                      `Term Exp Date` = col_date(format = "%Y-%m-%d")))


# check that 'Product' col name needs to be renamed to 'Product Description'
colnames(ix_mig_raw)[colnames(ix_mig_raw) == 'Product'] <- 'Product Description'

# run function
ix_filtered_func<-function(ix_mig_raw, ac_date, exp_date){
   df<-ix_mig_raw[complete.cases(ix_mig_raw[,1:2]),]
   df<-join(df, s5_productkey, by = 'Product Description')
   df$finance_category<-toupper(df$finance_category)
   df$subid<-paste0(str_sub(df$finance_category, 1, 1), df$`Subscription ID`)
   df$exp<-df$`Term Exp Date` + 21
   df_filtered<-df[((df$`Activation Date` <= as.Date(ac_date))
                    & (df$exp >= as.Date(exp_date))),]
   ix_hosting<-df_filtered[df_filtered$finance_category == 'HOSTING',]
   ix_addon<-df_filtered[df_filtered$finance_category == 'ADDON',]
   ix_unique<-subset(ix_addon, !duplicated(`WHMCS ID`))
   ix_lwp<-ix_unique[!ix_unique$`WHMCS ID` %in% ix_hosting$`WHMCS ID`,]
   ix_total<-rbind(ix_hosting, ix_lwp)
   ix_total$`Last Payment Amount` <- NA
   ix_total$Property<-'Site5'
   ix_total$`Cancellation Date`<- NA
   ix_total$`Last Authorization Date` <- NA
   ix_final<-ix_total[complete.cases(ix_total[,1:2]),]
   return(ix_final)
}

# 2) function output
ix_final<-ix_filtered_func(ix_mig_raw, ac_date = '2019-07-31', exp_date= '2019-08-01')

write_csv(ix_final, 'ix-migration_2019-07-31_filtered.csv')
##  3) Filtered MSR of Subs having Last Auth Date + Term + NGS before Measurement Period (Send to Jeremy)                  

s5_filtered_p1_LAD_OutofRange_func<-function(s5_msr_raw, ac_date_max, exp_date){
   a<-s5_msr_raw
   aa<-s5_productkey
   a2<-join(a, aa, by = 'Product Description')
   a2$pk2<-paste(a2$ProductDescriptor, a2$`Product Description`)
   a2$Match<-ifelse(a2$pk2 == a2$productkey, "Yes", "No")
   dupes<-a2[c('Account ID', 'Subscription ID')]
   a3<-a2[!duplicated(dupes),]
   table(a3$Match)
   a3$`ExpDate`<-a3$`Term Expiration Date` + 21
   a3$`Last Auth Date in Range`<-ifelse((a3$`Last Authorization Date` + a3$`Product Term Duration` * 30.42 + 21) >= as.Date(exp_date), "Yes","No")
   a4<-a3[(a3$`Activation Date` <= as.Date(ac_date_max)) 
          & (a3$`Activation Date` > as.Date('1988-12-31'))
          & (a3$`ExpDate` >= as.Date(exp_date)) 
          & (a3$`Last Payment Amount` > 0) 
          & (a3$`Subscription Status` %in% c('active', 'suspended'))
          & (a3$`Last Auth Date in Range` == 'No'),]
   a4.hosting<-a4[(a4$finance_category == 'hosting'),]
   a4.addon<-a4[(a4$finance_category == 'addon'),]
   a4.unique<-subset(a4.addon, !duplicated(`Account ID`))
   a4.lwp<-a4.unique[!a4.unique$`Account ID` %in% a4.hosting$`Account ID`,]
   a.total<-rbind(a4.hosting, a4.lwp)
   dim(a.total)
   sum(is.na(a.total$`Account ID`))
   final<-a.total[complete.cases(a.total[,1:2]),]
   dim(final)
   table(final$Match)
   return(final)
   # output of final msr filtered part 1 subs that have a 'last auth date out of range'
}

s5_filtered_p1_LAD_outofrange<-s5_filtered_p1_LAD_OutofRange_func(s5_msr_raw = s5,
                                                                  ac_date_max = '2019-05-31',
                                                                  exp_date = '2019-06-01')



####  4) Filter the file that comes back from Jeremy and re-include in final filtered file.                        

library(readr)

subs_lookup_path<-"D:/Subs files/2019Q2 - Subs Data - May/data/whmcs_subscriber_data_lookup_2019-06-03 21_09_17.csv"

subs_lookup <- read_csv(subs_lookup_path, col_types = cols(Activation.Date = col_date(format = "%m/%d/%Y"),
                                                           Cancellation.Date = col_date(format = "%m/%d/%Y"), 
                                                           ExpDate = col_date(format = "%m/%d/%Y"), 
                                                           Last.Authorization.Date = col_date(format = "%m/%d/%Y"), 
                                                           LastInvDueDate = col_date(format = "%Y-%m-%d"), 
                                                           LastInvPayDate = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                                           Term.Expiration.Date = col_date(format = "%m/%d/%Y")))


s5_lookup_filtered_func<-function(subs_lookup, exp_date){
   subs_lookup$inv_exp_date <- subs_lookup$`LastInvDueDate` + (subs_lookup$`Product.Term.Duration` * 30.42 + 21) 
   subs_lookup_filtered <- subs_lookup[subs_lookup$`LastInvPayAmt` >= 0 
                                       & subs_lookup$`SubRecurringAmount` > 0
                                       & subs_lookup$`inv_exp_date` >= as.Date("2019-06-01")
                                       & subs_lookup$Brand == 'S5',]
   final<-subs_lookup_filtered[complete.cases(subs_lookup_filtered[,c('Account.ID', 'Subscription.ID')]),]
   return(final)
}

s5_lookup_filtered<-s5_lookup_filtered_func(subs_lookup, '2019-06-01')


###   Combine filtered lists into one -- grab columns from each filtered output file that match each other.

dim(s5_filtered_p1_LAD_excluded)
dim(ix_final)
dim(s5_lookup_filtered)


s5_exc_col<-c('Account ID', 'Subscription Status', 'Term Expiration Date', 'Product Term Duration',
              'Product Description', 'Activation Date', 'Product Type', 'Subscription ID', 'ExpDate',
              'Last Payment Amount', 'Property', 'Cancellation Date', 'Last Authorization Date')

s5_p1<-s5_filtered_p1_LAD_excluded_2019_05_31[s5_exc_col]


ix_col<-c('WHMCS ID','Subscription Status', 'Term Exp Date',
          'Term', 'Product Description', 'Activation Date', 'finance_category',
          'subid', 'exp', 'Last Payment Amount', 'Property', 'Cancellation Date', 'Last Authorization Date')

ix<-ix_final[ix_col]
names(ix)<-c('Account ID', 'Subscription Status', 'Term Expiration Date', 'Product Term Duration',
             'Product Description', 'Activation Date', 'Product Type', 'Subscription ID', 
             'ExpDate', 'Last Payment Amount', 'Property', 'Cancellation Date', 'Last Authorization Date')


s5_lookup_col<-c('Account.ID', 'Subscription.Status', 'Term.Expiration.Date', 'Product.Term.Duration',
                 'Product.Description', 'Activation.Date', 'Product.Type', 'Subscription.ID', 'ExpDate',
                 'Last.Payment.Amount', 'Property', 'Cancellation.Date', 'Last.Authorization.Date')

s5_p2<-s5_lookup_filtered_2019_05_31_mysql[s5_lookup_col]
names(s5_p2)<-c('Account ID', 'Subscription Status', 'Term Expiration Date', 'Product Term Duration',
                'Product Description', 'Activation Date', 'Product Type', 'Subscription ID', 'ExpDate',
                'Last Payment Amount', 'Property', 'Cancellation Date', 'Last Authorization Date')


total_filtered<-rbind(s5_p1, ix, s5_p2)

# re-order column names

total_filtered<-total_filtered[c('Account ID', 'Subscription ID', 'Property', 'Product Type', 'Activation Date',
                                 'Last Authorization Date', 'Last Payment Amount', 'Product Term Duration', 'Cancellation Date',
                                 'Subscription Status', 'Product Description', 'Term Expiration Date', 'ExpDate')]

total_filtered$`Product Type`<-tolower(total_filtered$`Product Type`)
total_filtered$`Subscription Status`<-tolower(total_filtered$`Subscription Status`)

total_filtered$`Product Term Duration`<-ifelse(total_filtered$`Product Term Duration` == 'Monthly', 1,
                                               ifelse(total_filtered$`Product Term Duration` == 'Quarterly', 3,
                                                      ifelse(total_filtered$`Product Term Duration` == 'Semi-Annually', 6,
                                                             ifelse(total_filtered$`Product Term Duration` == 'Annually', 12,
                                                                    ifelse(total_filtered$`Product Term Duration` == 'Biennially', 24,
                                                                           ifelse(total_filtered$`Product Term Duration` == 'Triennially', 36, total_filtered$`Product Term Duration`))))))
dupes<-total_filtered[c('Account ID', 'Subscription ID')]

total_filtered_no_dupes<-total_filtered[!duplicated(dupes),]

final<-total_filtered_no_dupes[complete.cases(total_filtered_no_dupes[,c('Account ID', 'Subscription ID')]),]




#a3<-a2[!duplicated(dupes),]
#final<-h[complete.cases(h[,1:2]),]

not_in_msr<-final[!final$`Subscription ID` %in% site5_msr_2019_04_30_filtered$`Subscription ID`,]


#####   7) Netfirms/Datamines  ####
# Filters:
# Exclude ExpireDate == 'DFL'; before 7/1/2019
# Exclude LastTransactionDate == (blank) & < 7/1/2019
# Activation Date before 7/1/2019
# Distinct Count of 'Username' 

nf <- read_csv("D:/Subs files/2019Q3 - Subs Data - Jul/data/Netfirms-Monthly-Subs-Report_2019-07-31.csv", 
               col_types = cols(`ExpireDate(NextBill Date)` = col_date(format = "%Y-%m-%d"),
                                `Last Transaction Date` = col_date(format = "%Y-%m-%d"), 
                                SignupDate = col_date(format = "%Y-%m-%d")))

#nf$SignupDate<-as.Date(nf$SignupDate, format = '%m/%d/%Y')
#nf$`ExpireDate(NextBill Date)`<-as.Date(nf$`ExpireDate(NextBill Date)`, format = '%m/%d/%Y')

nf_filtered<-nf[(nf$`SignupDate` <= as.Date('2019-07-31'))
                & (nf$`ExpireDate(NextBill Date)` >= as.Date('2019-08-01'))
                & (!is.na(nf$`Last Transaction Date`))
                & (nf$`Last Transaction Date` <= as.Date('2019-07-31')),]

nf_distinct<-subset(nf_filtered, !duplicated(`UserName`))

nf_final<-nf_distinct[complete.cases(nf_distinct[,c('UserName')]),]

write_csv(nf_final, 'nf_msr_2019-07-31_filtered.csv')

#####   8) Directi   #####

library(lubridate)
library(plyr)
library(readr)

# Manually update latest MSR Raw data file path for reporting month:

## Directi Import & Filters Syntax   ##
# Import syntax for Directi Monthly Subs Report (as of 5/28/2019 that now has double quotes around text fields - including "Subscription ID" as well as the blank additional fields scheduled to be added on 6/1)

directi_path <- raw_msr_subs_directory
files <- list.files(path=directi_path, pattern="([Dd]irecti).*(*csv)")

for(directi_file in files)
{
   perpos <- which(strsplit(directi_file, "")[[1]]==".")
   assign(
      gsub(" ","",substr(directi_file, 1, perpos-1)),
      read_csv(paste(directi_path,directi_file,sep=""), quote = "\""))
}

# Assigns the right starting variables to be used in the existing MSR filtered script

rh<-get(ls()[grep(pattern = '(Directi_Retail_Hosting.*)', ls())])
rl<-get(ls()[grep(pattern = '(Directi_Retail_LightWeb.*)', ls())])
wh<-get(ls()[grep(pattern = '(Directi_Wholesale_Hosting.*)', ls())])
wl<-get(ls()[grep(pattern = '(Directi_Wholesale_LightWeb.*)', ls())])


directi_msr_filtered<-function(rh = rh, rl = rl, wh = wh, wl = wl, ac_date, exp_date, rep_date){
   
   #                    --  Function Argument Definitions  --
   #                    
   # rh = rh, rl = rl, wh = ...
   # ac_date = Maximum acceptable "Activation Date" value (string: 'YYYY-MM-DD'; ex: 4/30/2019 MSR would be '2019-04-30')
   # exp_date = Measurement Date (string: 'YYYY-MM-DD'; ex: 4/30/2019 MSR would be '2019-05-01')
   # rep_date = last day of the month for which the data is a snapshot
   
   # Begin Retail Hosting MSR Filters
   
   rh$`Activation Date`<-as.Date(rh$`Activation Date`)
   rh$`Term Expiration Date`<-as.Date(rh$`Term Expiration Date`)
   rh$`Last Authorization Date`<-as.Date(rh$`Last Authorization Date`)
   #rh$`Activation Date`<-mdy_hms(rh$`Activation Date`)
   rh$`Cancellation Date`<-as.Date(rh$`Cancellation Date`)
   rh$`exp`<-rh$`Term Expiration Date` + (rh$`NGS`)
   rh.filtered<-rh[(rh$`Activation Date` <= as.Date(ac_date)) 
                   & (rh$exp >= as.Date(exp_date))
                   #  & (rh$`Last Authorization Date` <= as.Date('2019-05-31'))
                   & (rh$`Subscription Status` == 'Active'),]
   rh.filtered<-rh.filtered[complete.cases(rh.filtered[,1:2]),]
   write_csv(rh.filtered, paste0('directi_retail_hosting_msr_', rep_date, '_filtered.csv'))
   rh.table<-table(rh.filtered$Property)
   rh_tab<-as.data.frame(rh.table)
   rh_tab$class<-"Retail Hosting"
   rh_tab$type<-"Hosting End"
   #write_csv(rh_tab, paste0('Directi_Retail_Hosting_',rep_date,'_filtered_table.csv'))
   
   # Begin Retail LWP MSR Filters
   
   rl$`Activation Date`<-as.Date(rl$`Activation Date`)
   rl$`Last Authorization Date`<-as.Date(rl$`Last Authorization Date`)
   rl$`Term Expiration Date`<-as.Date(rl$`Term Expiration Date`)
   rl$`Cancellation Date`<-as.Date(rl$`Cancellation Date`)
   rl$`exp`<-rl$`Term Expiration Date` + (rl$`NGS`)
   rl.filtered<-rl[(rl$`Activation Date` <= as.Date(ac_date)) 
                   & (rl$`Activation Date` >= as.Date('2015-01-01')) 
                   #& (rl$`Last Authorization Date` <= as.Date('2019-05-31'))
                   & (rl$exp >= as.Date(exp_date)) 
                   & (rl$`Subscription Status` == 'Active'),]
   rl.unique<-subset(rl.filtered, !duplicated(`Account ID`))
   rl.lwp<-rl.unique[!rl.unique$`Account ID` %in% rh.filtered$`Account ID`,]
   rl.lwp<-rl.lwp[complete.cases(rl.lwp[,1:2]),]
   write_csv(rl.lwp, paste0('directi_retail_lwp_msr_', rep_date, '_filtered.csv'))
   rl.table<-table(rl.lwp$Property)
   rl_tab<-as.data.frame(rl.table)
   rl_tab$class<-"Retail LWP"
   rl_tab$type<-"LWP End"
   #write_csv(rl_tab, paste0('Directi_Retail_LWP_', rep_date, '_filtered_table.csv'))
   
   # Begin Wholesale Hosting MSR Filters
   
   wh$`Activation Date`<-as.Date(wh$`Activation Date`)
   wh$`Last Authorization Date`<-as.Date(wh$`Last Authorization Date`)
   wh$`Term Expiration Date`<-as.Date(wh$`Term Expiration Date`)
   wh$`exp`<-wh$`Term Expiration Date` + (wh$`NGS`)
   wh.filtered<-wh[(wh$`Activation Date` <= as.Date(ac_date)) 
                   & (wh$exp >= as.Date(exp_date))
                   #& (wh$`Last Authorization Date` <= as.Date('2019-05-31'))
                   & (wh$`Subscription Status` == 'Active'),]
   wh.unique<-subset(wh.filtered, !duplicated(`Account ID`))
   wh.unique<-wh.unique[complete.cases(wh.unique[,1:2]),]
   write_csv(wh.unique, paste0('directi_wholesale_hosting_msr_', rep_date, '_filtered.csv'))
   wh.table<-table(wh.unique$Property)
   wh_tab<-as.data.frame(wh.table)
   wh_tab$class<-"Wholesale Hosting"
   wh_tab$type<-"Hosting End"
   #write_csv(wh_tab, paste0('Directi_Wholesale_Hosting_', rep_date, '_filtered_table.csv'))
   
   # Begin Wholesale LWP MSR Filters
   
   wl$`Activation Date`<-as.Date(wl$`Activation Date`)
   wl$`Term Expiration Date`<-as.Date(wl$`Term Expiration Date`)
   wl$`Last Authorization Date`<-as.Date(wl$`Last Authorization Date`)
   wl$`exp`<-wl$`Term Expiration Date` + (wl$`NGS`)
   wl.filtered<-wl[(wl$`Activation Date` <= as.Date(ac_date)) 
                   & (wl$`Activation Date` >= as.Date('2015-01-01')) 
                   #& (wl$`Last Authorization Date` <= as.Date('2019-05-31'))
                   & (wl$exp >= as.Date(exp_date)) 
                   & (wl$`Subscription Status` == 'Active'),]
   wl.unique<-subset(wl.filtered, !duplicated(`Account ID`))
   wl.lwp<-wl.unique[!wl.unique$`Account ID` %in% wh.unique$`Account ID`,]
   wl.lwp<-wl.lwp[complete.cases(wl.lwp[,1:2]),]
   write_csv(wl.lwp, paste0('directi_wholesale_lwp_msr_', rep_date, '_filtered.csv'))
   wl.table<-table(wl.lwp$Property)
   wl_tab<-as.data.frame(wl.table)
   wl_tab$class<-"Wholesale LWP"
   wl_tab$type<-"LWP End"
   #write_csv(wl_tab, paste0('Directi_Wholesale_LWP_', rep_  #date, '_filtered_table.csv'))
   
   # Create 4-filtered file summary view
   directi_all<-rbind(rl_tab, rh_tab, wh_tab, wl_tab)
   directi_all$superkey<-paste(directi_all$type, directi_all$class, directi_all$Var1)
   write_csv(directi_all, paste0('Directi_', rep_date, '_MSR_filtered-table.csv'))
}

# The following calls the function created above and will output Directi filtered MSR reports to current working directory

# Make sure to check function arguments before running function!
directi_msr_filtered(rh = rh, rl = rl, wh = wh, wl = wl,
                     ac_date = '2019-07-31',
                     exp_date = '2019-08-01',
                     rep_date = '2019-07-31')


#####   9) Brazil & LATAM    ####

h<-read_csv('D:/Subs files/2019Q3 - Subs Data - Jul/data/HGBrazil_Monthly-Subs-Report_2019-07-31.csv')
h$exp<-h$`Term Expiration Date` + 30
h<-h[(h$`Activation Date` <= as.Date("2019-07-31")) & (h$exp >= as.Date("2019-08-01")) & (h$`Subscription Status` %in% c("Active", "Suspended")),]

range(h$`Last Payment Amount`, na.rm=T)
zero<-h[which(is.na(h$`Last Payment Amount`)),]

h.hosting<-h[h$`Product Type` == 'Hosting',]
h.addon<-h[h$`Product Type` == 'Addon',]
h.unique<-subset(h.addon, !duplicated(`Account ID`))
h.lwp<-h.unique[!h.unique$`Account ID` %in% h.hosting$`Account ID`,]
h.total<-rbind(h.hosting, h.lwp)

#h.final<-h.total[which(is.na(h.total[,11])),]
# Check that "Last Authorization Date" does not exceed measurement period

range(h.total$`Last Authorization Date`)

# Check that no dupes exist for `Account ID` and `Subscription ID`
dupes<-h.total[c('Account ID', 'Subscription ID')]

h_check<-h.total[!duplicated(dupes),]

h_final<-h_check[complete.cases(h_check[,c('Account ID', 'Subscription ID')]),]

table(h_final$`Cancellation Date`)
#check and remove any columns with "NA"
#h.final<-h.total[,-14]
dim(h_final)

write_csv(h_final, 'hg_brazil_msr_2019-07-31_filtered.csv')





#####   10) Gator    #####

library(plyr)
library(readr)

h<-read_csv("D:/Subs files/2019Q3 - Subs Data - Jul/data/Gator_Monthly-Subs-Report_2019-07-31_v2.csv", 
            col_types = cols(`Activation Date` = col_date(format = "%Y-%m-%d"), 
                             `Cancellation Date` = col_date(format = "%Y-%m-%d"), 
                             `Last Authorization Date` = col_date(format = "%Y-%m-%d"), 
                             `Term Expiration Date` = col_date(format = "%Y-%m-%d")))


h<-h[h$`Subscription Status` %in% c('active', 'suspended'),]

length(which(is.na(h$`Cancellation Date`)))

#gator_NA_cancels<-h[grep("\\N", h$`Cancel Date`),]

range(h$`Activation Date`)
range(h$`Term Expiration Date`)

h$Exp<-h$`Term Expiration Date` + h$NGS

h<-h[!(h$`Product Type` %in% c("ADTIME1", "ADTIME1.5", "ADTIME30", "AVEBOOK", "BCEBO", "BPEBOOK", 
                               "CVTEBO", "DESEBO", "DFSEBO", "IMAGE", "LOGO", "MANSSL", "OYUS", "PCLEBO",
                               "SEOEBO", "SEOPRO", "SSOEBO", "STPER", "STPRO", "STSMB", "UCEBO", "WTIEBO",
                               "DOMAIN", "FREEDOM", "FREEMAIL")),]

h<-subset(h, !(`Product Term Duration` %in% c(0)))
h<-subset(h, (`Subscription Status` %in% c('active', 'suspended')))

# CHECK DATE RANGES !!!
h<-h[(h$`Activation Date` <= as.Date('2019-07-31')) & (h$`Activation Date` > as.Date('1999-01-01')) & (h$`Exp` >= ('2019-08-01')),]
# CHECK DATE RANGES !!!

range(h$`Activation Date`, na.rm=T)
range(h$`Term Expiration Date`)


h.hosting<-subset(h, (`Product Type` %in% c("PLAN1", "PLAN2", "PLAN3", "PLAN4", "PLAN5", "PLAN6")))
h.addons<-subset(h, !(`Product Type` %in% c("PLAN1", "PLAN2", "PLAN3", "PLAN4", "PLAN5", "PLAN6")))
h.unique.addons<-subset(h.addons, !duplicated(`Account ID`))
h.lwp<-h.unique.addons[!h.unique.addons$`Account ID` %in% h.hosting$`Account ID`,]
h.final.filtered<-rbind(h.hosting, h.lwp)

dupes<-h.final.filtered[c('Account ID', 'Subscription ID')]

final<-h.final.filtered[!duplicated(dupes),]

range(final$`Last Payment Amount`, na.rm = T)

setwd('D:/Subs files/2019Q3 - Subs Data - Jul/filtered')

write_csv(final, 'gator_msr_2019-07-31_filtered.csv')



#####   11) JDI Backup  ####

library(readr)
b<-read_csv('D:/Subs files/2019Q3 - Subs Data - Jul/data/Backup_Monthly-Subs-Report_2019-07-31.csv', col_names = T)

#b$ngs<-ifelse(b$`Property` == 'MyPC Backup', 49, 30)
#b$exp<-b$`Term Expiration Date` + b$ngs
b$exp<-b$`Term Expiration Date` + b$NGS
# CHECK DATES !!
b<-b[(b$`Activation Date` <= as.Date('2019-07-31')) 
     & (b$`Activation Date` >= as.Date('1990-01-01')) 
     & (b$`Subscription Status` %in% c('ACTIVE', 'SUSPENDED')) 
     & (b$`Last Payment Amount` > 0) 
     & (b$exp >= as.Date('2019-08-01')),]

#Need to manually review Cancellation Dates that have an 'Active' status but are in the reporting month.
# & ((b$`Cancellation Date` >= as.Date('2019-04-01')) | is.na(b$`Cancellation Date`)),]

b<-b[which(!is.na(b[,2])),]
b.filtered<-subset(b, !duplicated(`Account ID`))
b.filtered<-b.filtered[!b.filtered$`Account ID` %in% c(1),]

b.final<-b.filtered[complete.cases(b.filtered[,c('Account ID', 'Subscription ID')]),]

write_csv(b.final, 'backup_msr_2019-07-31_filtered.csv')

#####   12) Homestead   #####

# Exclude Cancelled Statuses; check that Activation Date & Term Expiration Date are within range.  

hs<-read_csv('D:/Subs files/2019Q3 - Subs Data - Jul/data/Homestead_Monthly-Subs-Report_2019-07-31.csv',
             col_types = cols(`Activation Date` = col_date(format = "%m/%d/%Y"),
                              `Cancellation Date` = col_date(format = "%m/%d/%Y"), 
                              `Last Authorization Date` = col_date(format = "%m/%d/%Y"), 
                              `Term Expiration Date` = col_date(format = "%m/%d/%Y"),
                              `Cancellation Date` = col_date(format = "%m/%d/%Y")))

hs_filtered<-hs[hs$`Subscription Status` %in% c('Active', 'Suspended'),]

range(hs_filtered$`Activation Date`)
range(hs_filtered$`Term Expiration Date`)

hs_final<-hs_filtered[complete.cases(hs_filtered[,1:2]),]

write_csv(hs_final, 'homestead_msr_2019-07-31_filtered.csv')

#####   13) SP    ####
# Exclude Cancelled Statuses; check that Activation Date & Term Expiration Date are within range. 
# "D:\Subs files\2019Q2 - Subs Data - Jun\SinglePlatform_Monthly-Subs-Report_2019-06-30_exclusions.xlsx"
# Excluded 5 Account IDs highlighted in red in above file that had a "Last Authorization Date" + 105-day enddate of before 7/1/2019.
# 105 days = 30-day use of service + 75-day NGS period.
# Will need to confirm with Sarah about this on a quarterly basis how many that violate the filter above need to come out of the active subscriber population.

sp<-read_csv('D:/Subs files/2019Q3 - Subs Data - Jul/data/SinglePlatform_Monthly-Subs-Report_2019-07-31.csv',
             col_types = cols(`Activation Date` = col_date(format = "%Y-%m-%d"),
                              `Cancellation Date` = col_date(format = "%Y-%m-%d"), 
                              `Last Authorization Date` = col_date(format = "%Y-%m-%d"), 
                              `Term Expiration Date` = col_date(format = "%Y-%m-%d"),
                              `Cancellation Date` = col_date(format = "%Y-%m-%d")))

sp_filtered<-sp[sp$`Subscription Status` %in% c('Active', 'Suspended'),]

range(sp_filtered$`Activation Date`)
range(sp_filtered$`Term Expiration Date`)

sp_final<-sp_filtered[complete.cases(sp_filtered[,1]),]

write_csv(sp_final, 'sp_msr_2019-07-31_filtered.csv')

#####   14) CTCT-Sites    ####   

library(readr)
library(plyr)

h<-read_csv("D:/Subs files/2019Q3 - Subs Data - Jul/data/CTCTSites_Monthly-Subs-Report_2019-07-31_v2.csv", 
            col_types = cols(`Activation Date` = col_date(format = "%Y-%m-%d"),
                             `Cancellation Date` = col_date(format = "%Y-%m-%d"), 
                             `Last Authorization Date` = col_date(format = "%Y-%m-%d"), 
                             `Term Expiration Date` = col_date(format = "%Y-%m-%d")))

h$Exp<-h$`Term Expiration Date` + h$NGS

h<-h[!(h$`Product Type` %in% c("ADTIME1", "ADTIME1.5", "ADTIME30", "AVEBOOK", "BCEBO", "BPEBOOK", 
                               "CVTEBO", "DESEBO", "DFSEBO", "IMAGE", "LOGO", "MANSSL", "OYUS", "PCLEBO",
                               "SEOEBO", "SEOPRO", "SSOEBO", "STPER", "STPRO", "STSMB", "UCEBO", "WTIEBO",
                               "DOMAIN", "FREEDOM", "FREEMAIL")),]

h<-subset(h, !(`Product Term Duration` %in% c(0)))
h<-subset(h, (`Subscription Status` %in% c('active', 'suspended')))
range(h$`Activation Date`)

# CHANGE DATES !!!
h<-h[(h$`Activation Date` <= as.Date('2019-07-31'))
     & (h$`Activation Date` > as.Date('1990-01-01')) 
     & (h$`Last Authorization Date` <= as.Date('2019-07-31'))
     & (h$`Exp` >= ('2019-08-01')),]
# CHECK DATE RANGES !!!

range(h$`Activation Date`, na.rm=T)
range(h$`Term Expiration Date`)
range(h$Exp)

h.hosting<-subset(h, (`Product Type` %in% c('PLAN1', 'PLAN2', 'PLAN3', 'PLAN4', 'PLAN5', 'PLAN6')))
h.addons<-subset(h, !(`Product Type` %in% c('PLAN1', 'PLAN2', 'PLAN3', 'PLAN4', 'PLAN5', 'PLAN6')))
h.unique.addons<-subset(h.addons, !duplicated(`Account ID`))
h.lwp<-h.unique.addons[!h.unique.addons$`Account ID` %in% h.hosting$`Account ID`,]
h.final.filtered<-rbind(h.hosting, h.lwp)
#final<-h.final.filtered[complete.cases(h.final.filtered),]
range(h.final.filtered$`Last Payment Amount`, na.rm=T)

dupes<-h.final.filtered[c('Account ID', 'Subscription ID')]

final2<-h.final.filtered[!duplicated(dupes),]
final3<-final2[complete.cases(final2[,1:2]),]

final3$`Account ID`<-as.character(final3$`Account ID`)

#write_csv(final3, 'ctct-sites_msr_2019-05-31_filtered.csv')


# Now import CTCT MSR filtered and CTCT-Builder Mapping file from Albert Beuker.  #

library(plyr)

# join ctct-builder mapping file with filtered ctct-sites MSR

map <- read_csv("D:/Subs files/2019Q3 - Subs Data - Jul/data/Builder_ConstantContact_map_2019-07-31.csv", 
                col_types = cols(`ConstantContact AccountID` = col_character(), 
                                 `Ubersmith AccountID` = col_character()))

colnames(map)[1]<-'Account ID'

x<-join(final3, map, by = 'Account ID')

overlap<-x[x$`ConstantContact AccountID` %in% map$`ConstantContact AccountID`,]

# Next import filtered month-ending CTCT Monthly Subs Report:
# Convert 'Account ID' column to character field using col_character()

ctct_msr_filtered<-read_csv("D:/Subs files/2019Q3 - Subs Data - Jul/filtered/ctct_msr_2019-07-31_filtered.csv", col_types = cols(`Account ID` = col_character()))

ctct_sites_deduped<-x[!x$`ConstantContact AccountID` %in% ctct_msr_filtered$`Account ID`,]

ctct_sites_final<-ctct_sites_deduped[complete.cases(ctct_sites_deduped[,1:2]),]

write_csv(ctct_sites_final, 'ctct-sites_msr_2019-07-31_filtered.csv')

#####   15) MOJO    ####

m <- read_csv("D:/Subs files/2019Q3 - Subs Data - Jul/data/Mojo_Monthly-Subs-Report_2019-07-31.csv", 
              col_types = cols(`Activation Date` = col_date(format = "%Y-%m-%d"), 
                               `Cancellation Date` = col_date(format = "%Y-%m-%d"), 
                               `Last Authorization Date` = col_date(format = "%Y-%m-%d"), 
                               `Term Expiration Date` = col_date(format = "%Y-%m-%d")))

m<-m[(m$`Activation Date` <= as.Date('2019-07-31'))
     & (m$`Term Expiration Date` >= as.Date('2019-08-01'))
     & (m$`Subscription Status` %in% c('active', 'suspended'))
     & (m$Property == 'Mojo')
     & (m$`Last Authorization Date` <= as.Date('2019-07-31'))
     & (m$`Last Payment Amount` > 0),]

m_distinct<-subset(m, !duplicated(`Account ID`))

m_final<-m_distinct[complete.cases(m_distinct[,c('Account ID', 'Subscription ID')]),]

write_csv(m_final, 'mojo_msr_2019-07-31_filtered.csv')

#####   16) AppMachine   ####

am <- read_csv("D:/Subs files/2019Q3 - Subs Data - Jul/data/AppMachine_Monthly-Subs-Report_2019-07-31.csv", 
               col_types = cols(`Activation Date` = col_date(format = "%Y-%m-%d"), 
                                `Cancellation Date` = col_date(format = "%Y-%m-%d"), 
                                `Last Authorization Date` = col_date(format = "%Y-%m-%d"), 
                                `Term Expiration Date` = col_date(format = "%Y-%m-%d")))

am<-am[(am$`Activation Date` <= as.Date('2019-07-31'))
       & (am$`Term Expiration Date` >= as.Date('2019-08-01'))
       & (am$`Subscription Status` %in% c('active', 'suspended'))
       & (am$`Last Authorization Date` <= as.Date('2019-07-31'))
       & (am$`Last Payment Amount` > 0),]

am_distinct<-subset(am, !duplicated(`Account ID`))

am_final<-am_distinct[complete.cases(am_distinct[,c('Account ID', 'Subscription ID')]),]

write_csv(am_final, 'appmachine_msr_2019-07-31_filtered.csv')

#####   17) MSS/Stripe Report  ####
# Filters:
# "Canceled At (UTC)" - blank values only
# Distinct Count of "Customer ID"

#####   18) WZ (w/ logic to exclude "Suspicious Subscribers")   ####

library(readr)
library(plyr)

# Import the file

h<-read_csv("D:/Subs files/2019Q3 - Subs Data - Jul/data/Builder_eHost_Cloud_Monthly-Subs-Report_2019-07-31_v2.csv", 
            col_types = cols(`Activation Date` = col_date(format = "%Y-%m-%d"), 
                             `Cancellation Date` = col_date(format = "%Y-%m-%d"), 
                             `Last Authorization Date` = col_date(format = "%Y-%m-%d"), 
                             `Term Expiration Date` = col_date(format = "%Y-%m-%d")))

# Import the brand_mapping file to be able to group subscribers into WZ / WZ Others / Cloud brand groups

wz_brand_mapping<-read_csv('D:/Subs files/Analysis/UK/wz_brand_lookup_201804.csv', col_names = T)

h<-join(h, wz_brand_mapping, by = 'Property')

# Calculating 'Expiration Date' by adding 'Term Expiration Date' + 'NGS'

h$Exp<-h$`Term Expiration Date` + h$NGS

h<-h[!(h$`Product Type` %in% c("ADTIME1", "ADTIME1.5", "ADTIME30", "AVEBOOK", "BCEBO", "BPEBOOK", 
                               "CVTEBO", "DESEBO", "DFSEBO", "IMAGE", "LOGO", "MANSSL", "OYUS", "PCLEBO",
                               "SEOEBO", "SEOPRO", "SSOEBO", "STPER", "STPRO", "STSMB", "UCEBO", "WTIEBO",
                               "DOMAIN", "FREEDOM", "FREEMAIL")),]

h<-subset(h, !(`Product Term Duration` %in% c(0)))
h<-subset(h, (`Subscription Status` %in% c('active', 'suspended')))
range(h$`Activation Date`)

# CHANGE DATES !!!

h<-h[(h$`Activation Date` <= as.Date('2019-07-31'))
     & (h$`Activation Date` > as.Date('1990-01-01')) 
     & (h$`Last Authorization Date` <= as.Date('2019-07-31'))
     & (h$`Exp` >= ('2019-08-01')),]


range(h$`Activation Date`, na.rm=T)
range(h$`Term Expiration Date`)
range(h$Exp)

h.hosting<-subset(h, (`Product Type` %in% c('PLAN1', 'PLAN2', 'PLAN3', 'PLAN4', 'PLAN5', 'PLAN6')))
h.addons<-subset(h, !(`Product Type` %in% c('PLAN1', 'PLAN2', 'PLAN3', 'PLAN4', 'PLAN5', 'PLAN6')))
h.unique.addons<-subset(h.addons, !duplicated(`Account ID`))
h.lwp<-h.unique.addons[!h.unique.addons$`Account ID` %in% h.hosting$`Account ID`,]
h.final.filtered<-rbind(h.hosting, h.lwp)
#final<-h.final.filtered[complete.cases(h.final.filtered),]

range(h.final.filtered$`Last Payment Amount`, na.rm=T)

# exclude eHost

final<-h.final.filtered[h.final.filtered$Brand != 'eHost', ]

dupes<-final[c('Account ID', 'Subscription ID')]

# Gator specific
dupes<-final[c('Account ID', 'Subscription ID')]

final2<-final[!duplicated(dupes),]

finalcomplete<-final2[complete.cases(final2[,c('Account ID', 'Subscription ID')]),]


setwd('D:/Subs files/2019Q2 - Subs Data - Jun/filtered')
#write_csv(final2, 'wz_msr_2019-06-30_filtered.csv')
table(final$Brand)


# Next: Import filtered "Suspended Services on Active Subscriber" (a.k.a. `list1_exclude`) to exclude all services in this list (N = 914) from filtered Builder report.

list1_exclude <- read_csv("D:/Subs files/2019Q2 - Subs Data - Jun/wz/wz round 2 exclusions 2019-07-09/Builder_List1of2_Exclusions_914.csv")

table(list1_exclude$`Cancel Date? (Q2/PreQ2)`)   

list1_exclude<-join(list1_exclude, wz_brand_mapping, by = 'Property')

#  (Q2): 38    |     Pre(Q2): 876
#  Count 'Pre(Q2)' 876 towards adjustment

# 115,922 in Builder MSR filtered - before exclusions from List 1 & 2

wz_list1_excluded<-finalcomplete[!finalcomplete$`Subscription ID` %in% list1_exclude$`Subscription ID`,]

# 115,121 (115,922 - 115,121 = 801 excluded from List1)

# Next: Exclude List2 (N = 1,188) from `wz_list1_excluded` and calculate pre-Q2 cancellations from both lists (List1 & List2)

wz_list2_excluded <- read_csv("D:/Subs files/2019Q2 - Subs Data - Jun/wz/wz round 2 exclusions 2019-07-09/WZ_List2_final_exclusions_20190709.csv")

wz_list2_excluded<-join(wz_list2_excluded, wz_brand_mapping, by = 'Property')

#  (Q2): 80   |   Pre(Q2):  556
#  Adjustment from list 1: 556/1192
#  
# list 1 & list 2 - Account ID/Subscription ID

l1<-list1_exclude[,c('Account ID', 'Subscription ID', 'Property', 'Brand', 'Cancel Date? (Q2/PreQ2)')]
l2<-wz_list2_excluded[,c('Account ID', 'Subscription ID', 'Property', 'Brand', 'Cancel Date')]

colnames(l1)<-c('Account ID', 'Subscription ID', 'Property', 'Brand', 'Cancel Date')

total_exclude<-rbind(l1, l2)

dupes<-total_exclude[c('Account ID', 'Subscription ID')]
total_exclude<-total_exclude[!duplicated(dupes),]

# Not final, need to remove one more subscriber from 'Cloud' having Pre-Q2 Cancel Date

final_with_total_exclusions<-finalcomplete[!finalcomplete$`Subscription ID` %in% total_exclude$`Subscription ID`,]

table(final_with_total_exclusions$Brand)

# Final

wz_final<-final_with_total_exclusions[!(final_with_total_exclusions$Brand) %in% c('Cloud'),]

wz_final<-wz_final[complete.cases(wz_final[,1:2]),]

# Adjustment calculation

total_exclusions_and_msr_filtered<-total_exclude[total_exclude$`Subscription ID` %in% finalcomplete$`Subscription ID`,]

# Round 3 of filtering after finding out that 3 of my samples were still in final filtered June-ending Subs Report.  Now adding a calc field of: "Last Authorization Date" + "Product Term Duration" * 30.42 + NGS <= 6/30/19 for Builder & Builder Other properties only (excluding Sitelio).

wz<-wz_final

wz$calc_date<-wz$`Last Authorization Date` + wz$`Product Term Duration` * 30.42 + wz$NGS

wz_not_sitelio<-wz[wz$Brand != 'Sitelio',]

wz_not_sitelio<-wz_not_sitelio[wz_not_sitelio$calc_date >= as.Date('2019-08-01'),]

wz_sitelio<-wz[wz$Brand == 'Sitelio',]

wz_all<-rbind(wz_sitelio, wz_not_sitelio)

final<-wz_all[complete.cases(wz_all[,1:2]),]

write_csv(final, 'wz_msr_2019-07-31_filtered.csv')



# -------------   End  ------------------- #



#####   19) Final Count Validation  ####

#    Make list of dataframes for quarter-ending KPMG final counts checking:
#    Final Check counts rollup  
#    Convert df objects in local environment to dataframe using plyr
#    combine into list and get counts

library(readr)

path <- 'D:/Subs files/2019Q3 - Subs Data - Jul/filtered/'
files <- list.files(path=path, pattern="*csv")
for(file in files)
{
   perpos <- which(strsplit(file, "")[[1]]==".")
   assign(
      gsub(" ","",substr(file, 1, perpos-1)),
      read_csv(paste(path,file,sep="")))
}

dfs<-Filter(function(x) is(x, "data.frame"), mget(ls()))
counts<-lapply(dfs, nrow)
counts<-as.data.frame(counts)

lapply(dfs, dim)

do.call(sum, counts)

write.csv(counts, 'Jun-2019-final_counts_prelim.csv', row.names=F)



# complete cases function to test complete records for first column

complete_cases<-function(df){
   x<-df[complete.cases(df[,1]),]
   dim(x)
}

test<-lapply(dfs, complete_cases)

test_df<-as.data.frame(test)

do.call(sum, test_df)
write_csv(test_df, 'test_df.csv')

