library(Matching)
library(lubridate)
library(doParallel)
library(readr)
library(xgboost)
`%notin%` <- Negate(`%in%`)
library(caret)
library(haven)
library(data.table)
# library(MASS)
# library(nbpMatching)
library(crossmatch)
library(survey)

match_func <- function(acu_sub,non_acu_sub) {
	X <- rbind(acu_sub,non_acu_sub)
	Y <- X$outcome
	WTS <- X$X_CNTYWT
	X[,c('outcome','X_CNTYWT'):=NULL]
	X <- data.matrix(X)
	X <- apply(X, 2, function(y) (y - mean(y)) / sd(y) ^ as.logical(sd(y)))
	X <- X %*% diag(imp_val^10)
	Tr <- c(rep(1,nrow(acu_sub)),rep(0,nrow(non_acu_sub)))
	m <- Match(Y=Y, X=X, Tr=Tr, weights = WTS, ties=FALSE)
	return(m)	
}

matches_into_valid_counties <- function() {
	va_ests <- unlist(lapply(unlist(va_matches,recursive = F),function(x){abs(x$est)}))
	va_ests <- matrix(va_ests,ncol = length(non_acu_counties), byrow=T)
	
	va_sds <- unlist(lapply(unlist(va_matches,recursive = F),function(x){x$se.standard}))
	va_sds <- matrix(va_sds,ncol = length(non_acu_counties), byrow=T)
	
	# from all matches, select those which are not significantly different
	# which we'll call "indistinguishable"
	# currently defined as within 1 std dev
	# which_sig_by_row <- va_ests<2*va_sds
	which_sig_by_row <- apply(abs(va_ests/va_sds),1,which.min)
	est_by_row <- va_ests[which_sig_by_row]
	sd_by_row <- va_sds[which_sig_by_row]
	# print out the number of indistinguishable counties
	print(table(sig_va <<- est_by_row > sd_by_row))
	# cbind(est_by_row,sd_by_row)

	# match the counties which did not have acupuncture clinics
	# to the counties which did have actupuncture clinics
	# where the pairing is indistinguishable
	valid_counties <- non_acu_counties[which_sig_by_row] #apply(which_sig_by_row,1,function(x){non_acu_counties[which(x)]})
	names(valid_counties) <- acu_counties
	
	cat("Printing out how often a non-acu county is matched in duplicate
	ideally we will come back and account for this duplicate nature in our
	variance and error calculations")
	print("Count of Matches for Non-Acu Counties")
	print(table(table(unlist(valid_counties))))
	print("Pct of Dupe Matched Non-Acu Counties")
	print(prop.table(table(duplicated(unlist(valid_counties)))))
	return(valid_counties)
}

load_data <- function(){
	# acupuncture data
	crosswalk <<- read_dta('~/Downloads/ZIP5_County_Crosswalk.dta')
	state_fips_name_map <- unique(crosswalk[,c('state_fips','state')])
	
	by_cnty <- lapply(Sys.glob('~/Documents/brfss_smart_data/*'),read_xpt)
	
	name_overlap <- Reduce(intersect, lapply(by_cnty,names))
	by_cnty <- lapply(by_cnty,function(x){x[,name_overlap]})
	by_cnty <- do.call(rbind,by_cnty)
	names(by_cnty) <- make.names(names(by_cnty))
	by_cnty <- subset(by_cnty,IYEAR %notin% c(2006,2010,2011)) # high missingness
	by_cnty <- data.table(by_cnty)
	state_fips_name_map <<- state_fips_name_map[order(state_fips_name_map$state_fips),]

	by_cnty$X_STATE <- state_fips_name_map$state[match(by_cnty$X_STATE,state_fips_name_map$state_fips)]
	
	by_cnty$IDATE <- as.numeric(mdy(by_cnty$IDATE))
	by_cnty$state_cnty <- with(by_cnty,paste(X_STATE,X_CNTYNAM))
	state_cnty_tbl <- with(by_cnty,table(state_cnty,IYEAR))
	state_cnty_tbl_rowsums <- rowSums(state_cnty_tbl<100)
	table(state_cnty_tbl_rowsums)
	viable_st_cntys <- names(state_cnty_tbl_rowsums[state_cnty_tbl_rowsums<=1])
	by_cnty <<- subset(by_cnty,state_cnty %in% viable_st_cntys)
}

# Load BRFSS data (https://www.cdc.gov/brfss/index.html)
# and label outcome as QLACTLM2
# "Are you limited in any way in any activities because of
# physical, mental, or emotional problems?"
# Removing any records which did not answer this question
# Further, remove any variable with >1k missing observations.
# Then remove any record which still has missing data.
make_brfss <- function(by_cnty){
	brfss <- by_cnty
	brfss$IYEAR <- parse_integer(brfss$IYEAR)
	brfss$outcome <- brfss$QLACTLM2 == 1
	brfss$outcome <- as.numeric(brfss$outcome)
	brfss <- subset(brfss,!is.na(outcome))
	
	missing_vars <- names(brfss)[colSums(is.na(brfss))>nrow(brfss)/1000]
	brfss <- brfss[,!..missing_vars]
	brfss <- na.omit(brfss)
}

# We will run an XGBoost model
# to predict the outcome (missing work due to health)
# in our validation period.
# We then take the important variables and use them
# to generate matches between acu and non-acu counties.
get_imp_vars <- function(){
	va_yr <<- 2008
	print(va_yr)
	oos_yr <<- va_yr+1
	
	dat_trva <<- subset(brfss,IYEAR == va_yr)
	tr_rows <- createDataPartition(dat_trva$outcome, p = .8, 
																 list = FALSE, 
																 times = 1)
	va_rows <- setdiff(seq(nrow(dat_trva)),tr_rows)
	dat_oos <<- subset(brfss,IYEAR == oos_yr)

	cut_vars <- c(
								'outcome','QLACTLM2',
								'X_CNTYWT','X_WT2','X_CNTY','X_CNTYNAM','ADJCNTY','INTVID','SEQNO','X_STATE','X_PSU','X_STSTR','NATTMPTS',
								'IDATE','IDAY','AGE_C_F','RACE_C_F','IMONTH'
								)
	
	tr_packaged <- xgb.DMatrix(data.matrix(dat_trva[tr_rows,!..cut_vars]),
														 label=dat_trva[tr_rows,outcome]
														 ,weight=dat_trva[tr_rows,X_CNTYWT]
	)
	va_packaged <- xgb.DMatrix(data.matrix(dat_trva[va_rows,!..cut_vars]),
														 label=dat_trva[va_rows,outcome]
														 ,weight=dat_trva[va_rows,X_CNTYWT]
	)
	oos_packaged <- xgb.DMatrix(data.matrix(dat_oos[,!..cut_vars]),
															label=dat_oos[,outcome]
															,weight=dat_oos[,X_CNTYWT]
	)
	
	tr_va_xgb_m <- xgb.train(
		objective = "binary:logistic", 
		eta = .1,
		tree_method = 'hist',
		grow_policy = 'lossguide',
		early_stopping_rounds = 20,
		maximize=T,
		eval_metric = 'auc',
		nrounds =  400,
		data = tr_packaged,
		max_depth = 5,
		print_every_n = 10,
		watchlist=list(train=tr_packaged,validate=va_packaged)
	)

	(model_imp <- head(xgb.importance(model=tr_va_xgb_m),20))
	imp_val <<- model_imp$Gain
	imp_vars <- model_imp$Feature
	imp_vars <<- unique(c(imp_vars,'state_cnty','outcome','X_CNTYWT'))
}

load_data()
brfss <- make_brfss(by_cnty)
get_imp_vars()

state_cnty_vec <- unique(dat_oos$state_cnty)
acu_counties <- sort(sample(state_cnty_vec,10))
non_acu_counties <- setdiff(state_cnty_vec,acu_counties)
	
brfss_match_va <- subset(brfss,IYEAR==va_yr)[,..imp_vars]
brfss_match_oos <- subset(brfss,IYEAR==oos_yr)[,..imp_vars]

# A/A test setup
state_cnty_vec <- unique(brfss_match_va$state_cnty)
acu_counties <- sort(sample(state_cnty_vec,round(length(state_cnty_vec)/10)))
non_acu_counties <- setdiff(state_cnty_vec,acu_counties)
brfss_match_va_acu_subs <- lapply(acu_counties,function(x){subset(brfss_match_va,state_cnty==x)[,!'state_cnty']})
brfss_match_va_non_acu_subs <- lapply(non_acu_counties,function(x){subset(brfss_match_va,state_cnty==x)[,!'state_cnty']})

cl <- makeCluster(8,outfile='')
registerDoParallel(cl)

va_matches <- foreach(acu_sub_va=brfss_match_va_acu_subs,acu_label_va=acu_counties,.packages=c('Matching','data.table')) %:%
	foreach(non_acu_sub_va=brfss_match_va_non_acu_subs,non_acu_label_va=non_acu_counties) %dopar% {
	print(paste(acu_label_va,non_acu_label_va))
	m_va <- match_func(acu_sub_va,non_acu_sub_va) 
	return(m_va)
}
# name these objects
names(va_matches) <- acu_counties
va_matches <- lapply(va_matches,function(x){
	names(x) <- non_acu_counties
	return(x)
})

# looking at all potential matches
# between acu and non-acu counties
# in the validation period
# and selecting those which are 
# statistically indistinguishable.
# returned object is set of
# indistinguishable non-acu counties
# grouped by acu county.
valid_counties <- matches_into_valid_counties()

# now we move from validation period
# into out-of-sample period.
# we take the matches which were indistinguishable
# in the validation period and 
# see if they are now distinguishable in
# the out-of-sample period.
		
oos_ests_aa_by_acu_county <- foreach(acu_county=names(valid_counties),.packages=c('data.table')) %do% {
	acu_sub_oos <- subset(brfss_match_oos,state_cnty == acu_county)[,!'state_cnty']
	
	non_acu_counties_oos <- valid_counties[[acu_county]]
	brfss_match_oos_non_acu_subs <- lapply(non_acu_counties_oos,function(x){subset(brfss_match_oos,state_cnty == x)[,!'state_cnty']})
	names(brfss_match_oos_non_acu_subs) <- non_acu_counties_oos
	oos_matches <- foreach(non_acu_county_name_oos=names(brfss_match_oos_non_acu_subs)) %do% {
		print(paste(acu_county,non_acu_county_name_oos))
		non_acu_sub_oos <- brfss_match_oos_non_acu_subs[[non_acu_county_name_oos]]
		m_oos <- match_func(acu_sub_oos,non_acu_sub_oos) 
		return(m_oos)
	}
	
}

oos_ests_aa <- lapply(unlist(oos_ests_aa_by_acu_county,recursive = F),function(x){
	data.frame('est'=x$est,'se_std'=x$se.standard)
})
oos_ests_aa <- do.call(rbind,oos_ests_aa)
table(sig_oos <- with(oos_ests_aa,abs(est) > 2*se_std))
par(mfrow=c(2,1))
with(oos_ests_aa,hist(est,col='lightblue'))
with(oos_ests_aa,boxplot(est,col='lightblue',horizontal = T))

with(oos_ests_aa,sd(est))
with(oos_ests_aa,mean(est))

# Okay, A/A test done, let's get real data in there

acu <- read_csv('~/Downloads/POCA Clinic Establishment Dates - Sheet1.csv')
acu$zip <- parse_number(acu$location)

k <- merge(crosswalk,acu,by.x = 'zip5',by.y = 'zip')
k <- with(k,data.frame('established'=parse_integer(established),'state_cnty'=paste(state,county_name)))
k <- na.omit(k)

k <- subset(k,toupper(state_cnty) %in% toupper(brfss$state_cnty))
k <- subset(k,established <= va_yr)

acu_counties <- as.character(k$state_cnty)
non_acu_counties <- setdiff(state_cnty_vec,acu_counties)
brfss_match_va_acu_subs <- lapply(acu_counties,function(x){subset(brfss_match_va,state_cnty==x)[,!'state_cnty']})
brfss_match_va_non_acu_subs <- lapply(non_acu_counties,function(x){subset(brfss_match_va,state_cnty==x)[,!'state_cnty']})

cl <- makeCluster(8,outfile='')
registerDoParallel(cl)

va_matches <- foreach(acu_sub_va=brfss_match_va_acu_subs,acu_label_va=acu_counties,.packages=c('Matching','data.table')) %:%
	foreach(non_acu_sub_va=brfss_match_va_non_acu_subs,non_acu_label_va=non_acu_counties) %dopar% {
		start <- Sys.time()
		print(paste(acu_label_va,non_acu_label_va))
		m_va <- match_func(acu_sub_va,non_acu_sub_va) 
		print(Sys.time() - start)
		return(m_va)
	}
# name these objects
names(va_matches) <- acu_counties
va_matches <- lapply(va_matches,function(x){
	names(x) <- non_acu_counties
	return(x)
})

# looking at all potential matches
# between acu and non-acu counties
# in the validation period
# and selecting those which are 
# statistically indistinguishable.
# returned object is set of
# indistinguishable non-acu counties
# grouped by acu county.
valid_counties <- matches_into_valid_counties()

# now we move from validation period
# into out-of-sample period.
# we take the matches which were indistinguishable
# in the validation period and 
# see if they are now distinguishable in
# the out-of-sample period.

oos_ests_poca_by_acu_county <- foreach(acu_county=names(valid_counties),.packages=c('data.table')) %do% {
	acu_sub_oos <- subset(brfss_match_oos,state_cnty == acu_county)[,!'state_cnty']
	non_acu_counties_oos <- valid_counties[[acu_county]]
	brfss_match_oos_non_acu_subs <- lapply(non_acu_counties_oos,function(x){subset(brfss_match_oos,state_cnty == x)[,!'state_cnty']})
	names(brfss_match_oos_non_acu_subs) <- non_acu_counties_oos
	oos_matches <- foreach(non_acu_county_name_oos=names(brfss_match_oos_non_acu_subs)) %do% {
		print(paste(acu_county,non_acu_county_name_oos))
		non_acu_sub_oos <- brfss_match_oos_non_acu_subs[[non_acu_county_name_oos]]
		m_oos <- match_func(acu_sub_oos,non_acu_sub_oos) 
		return(m_oos)
	}
}

oos_ests_poca <- lapply(unlist(oos_ests_poca_by_acu_county,recursive = F),function(x){
	data.frame('est'=x$est,'se_std'=x$se.standard)
})
oos_ests_poca <- do.call(rbind,oos_ests_poca)
table(sig_oos <- with(oos_ests_poca,abs(est) > 2*se_std))
par(mfrow=c(2,1))
with(oos_ests_poca,hist(est,col='lightblue'))
with(oos_ests_poca,boxplot(est,col='lightblue',horizontal = T))

with(oos_ests_poca,sd(est))
with(oos_ests_poca,mean(est))
with(oos_ests_poca,median(est))

par(mfrow=c(1,1))
aa_test_conf_int <- with(oos_ests_aa,quantile(est/se_std,c(.025,.975)))
with(oos_ests_poca,
		 median(est/se_std))
hist_xlim <- range(round(c(with(oos_ests_aa,est/se_std),with(oos_ests_poca,est/se_std))/5)*5)
hist_breaks <- seq(hist_xlim[1],hist_xlim[2],10)
poca_est <- with(oos_ests_poca,mean(est/se_std))
poca_test_conf_int <- with(oos_ests_poca,quantile(est/se_std,c(.025,.975)))
with(oos_ests_poca,plot(density(est/se_std),col='cyan4'))
abline(v=poca_test_conf_int,col='cyan4',lty=2)
with(oos_ests_aa,lines(density(est/se_std),col='indianred1'))
abline(v=aa_test_conf_int,col='indianred1',lty=2)

	