# Set the working directory
setwd('C:/Users/jason/OneDrive - Office 365/year_4/EL4013 Data Programming')

# install packages
install.packages('explore')
install.packages('correlationfunnel')
install.packages('tidyverse')

# call libraries
library(explore)
library(correlationfunnel)
library(tidyverse)

# read in bank data - store in bank_data - seperator set to ;
bank_data <- read.csv(file = 'bank-additional-full.csv', sep = ';')

# view head of data
head(bank_data)

# using explore - describe the dataset
describe_tbl(bank_data)
describe(bank_data, n = 21) # limits to 10 lines
report(bank_data, output_file = 'bdreport' , 
       output_dir = 'C:/Users/jason/OneDrive - Office 365/year_4/EL4013 Data Programming')
describe(bank_data, y)

# count outcomes of target variables
count_pct(bank_data, y)
count_pct(bank_data, housing)
count_pct(bank_data, loan)

# rename variable
bank_data <- rename(bank_data, term_dep = y)
describe(bank_data, term_dep)

# binarize data
b_data1 <- binarize(bank_data)
glimpse(b_data1)

# correlate data
c_data1 <- correlate(b_data1, target = 'term_dep__yes')
head(c_data1, 15)

# remove duration
bank_data <- subset(bank_data, select = -c(duration))

# binarize data
b_data1 <- binarize(bank_data)

# correlate data
c_data1 <- correlate(b_data1, target = 'term_dep__yes')
head(c_data1, 15)

# plot correlation funnel
plot_correlation_funnel(c_data1, limits = c(-0.35, 0.35))


# rename further variables
bank_data <- rename(bank_data, comm_type = contact, day = day_of_week,
                    loan_house = housing, loan_pers = loan,
                    num_cont = campaign, days_passed = pdays,
                    num_cont_prev = previous, prev_outcome = poutcome,
                    evr = emp.var.rate, cpi = cons.price.idx,
                    cci = cons.conf.idx, euribor = euribor3m,
                    empl_num = nr.employed)

# create subsets of data set
demog_hist <- select(bank_data, term_dep, age, job, marital, education, default, loan_house, loan_pers)
contact_hist <- select(bank_data, term_dep, comm_type, month, day, num_cont, days_passed, num_cont_prev, prev_outcome)
soceco <- select(bank_data, term_dep, evr, cpi, cci, euribor, empl_num)

# subset demographic data - analysis
b_demog_hist <- binarize(demog_hist)
glimpse(b_demog_hist)
c_demog_hist <- correlate(b_demog_hist, target = 'term_dep__yes')
head(c_demog_hist, 15)
plot_correlation_funnel(c_demog_hist, limits = c(-0.1, 0.1))

count_pct(demog_hist, default)

# use explore all - demog_hist
explore_all(select(demog_hist, term_dep, job, marital, education), target = term_dep, ncol = 3)

# highlighting age
explore(demog_hist, age, target = term_dep)

# counts in each age range
count_pct(demog_hist[which(demog_hist$age < 34),], term_dep)
count_pct(demog_hist[which(demog_hist$age >= 34 & demog_hist$age <= 57),], term_dep)
count_pct(demog_hist[which(demog_hist$age > 57),], term_dep)


# plotting for less than 34s
demog_lt34 <- demog_hist[which(demog_hist$age < 34),]
explore_all(select(demog_lt34, term_dep, job, marital, education), target = term_dep, ncol = 3)

# plotting for over 57s
demog_mt57 <- demog_hist[which(demog_hist$age > 57),]
explore_all(select(demog_mt57, term_dep, job , marital, education), target = term_dep, ncol = 3)

# plotting for between 34 - 57
demog_34_57 <- demog_hist[which(demog_hist$age >= 34 & demog_hist$age <= 57),]
explore_all(select(demog_34_57, term_dep, job , marital, education), target = term_dep, ncol = 3)

# analysis of subset contact data - see 3.5.2
# subset contact data
b_contact_hist <- binarize(contact_hist)
c_contact_hist <- correlate(b_contact_hist, target = 'term_dep__yes')
head(c_contact_hist, 15)
plot_correlation_funnel(c_contact_hist, limits = c(-0.35, 0.35))

# bars of month and day correlations
explore_bar(contact_hist, month, target = term_dep, flip = y)
explore_bar(contact_hist, day, target = term_dep, flip = n)

count_pct(bank_data, prev_outcome)
success_test <- bank_data[which(bank_data$prev_outcome == 'success'),]
count_pct(success_test, term_dep)

explore_cor(success_test, days_passed, term_dep)

explore(bank_data[which(bank_data$prev_outcome == 'success'),], days_passed, target = term_dep)

# analysis of subset socioeconmoic data - see 3.5.3
# subset socio economic data
b_soceco <- binarize(soceco)
c_soceco <- correlate(b_soceco, target = 'term_dep__yes')
head(c_soceco, 15)
plot_correlation_funnel(c_soceco, limits = c(-0.30, 0.30))


# use explore all on employment level, EVR, CPI and Euribor
explore_all(select(soceco, term_dep, evr, cpi, euribor, empl_num), target = term_dep)

# use explore on CCI
explore(soceco, cci, target = term_dep)


demog_lt34 <- demog_hist[ which(demog_hist$age < 34),]
explore_all(select(demog_lt34, term_dep, job, marital, education), target = term_dep, ncol = 3)

demog_mt57 <- demog_hist[ which(demog_hist$age > 57),]
explore_all(select(demog_mt57, term_dep, job , marital, education), target = term_dep, ncol = 3)





explore_all(contact_hist, target = term_dep)

explore_all(soceco, target = term_dep)


explore(demog_hist, target = term_dep, var = age, var2 = education)




b_data1 <- binarize(bank_data, n_bins = 4)

b_data2 <- correlate(b_data1, target = 'contact__cellular')
head(b_data2, 20)

plot_correlation_funnel(b_data2)

slice <- select(bank_data, y, duration, poutcome, pdays, previous, housing, contact, month, job)
slice <- select(bank_data, y, marital, job, education)

explore_all(slice, target = y)
explain_tree(bank_data, target = y)
report(bank_data)
explore(bank_data)

newdata <- bank_data[which(bank_data$y=='yes'),]
newdata <- bank_data[which(bank_data$job=='management' & bank_data$marital == 'single'
                           & bank_data$education == 'tertiary'
                           & bank_data$contact =='cellular'),]
describe_tbl(newdata)
nd1 <- binarize(newdata)
nd2 <- correlate(nd1, y__yes)
plot_correlation_funnel(nd2)
