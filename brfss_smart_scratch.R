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

# acupuncture data
crosswalk <- read_dta('~/Downloads/ZIP5_County_Crosswalk.dta')
state_fips_name_map <- unique(crosswalk[,c('state_fips','state')])

by_cnty <- lapply(Sys.glob('~/Documents/brfss_smart_data/*'),read_xpt)

name_overlap <- Reduce(intersect, lapply(by_cnty,names))
# sort(name_overlap)

by_cnty <- lapply(by_cnty,function(x){x[,name_overlap]})
by_cnty <- do.call(rbind,by_cnty)
names(by_cnty) <- make.names(names(by_cnty))
by_cnty <- subset(by_cnty,IYEAR %notin% c(2006,2011))
by_cnty <- data.table(by_cnty)
# by_cnty$X_STATE <- factor(by_cnty$X_STATE)
# by_cnty$X_STATE <- as.numeric(by_cnty$X_STATE)
state_fips_name_map <- state_fips_name_map[order(state_fips_name_map$state_fips),]
by_cnty$X_STATE <- state_fips_name_map$state[by_cnty$X_STATE]

table(unique(by_cnty$X_CNTYNAM) %in% crosswalk$county_name)

table(toupper(unique(with(by_cnty,paste(X_STATE,X_CNTYNAM)))) %in% toupper(unique(with(crosswalk,paste(state,county_name)))))


toupper(unique(with(by_cnty,paste(X_STATE,X_CNTYNAM))))[toupper(unique(with(by_cnty,paste(X_STATE,X_CNTYNAM)))) %notin% toupper(unique(with(crosswalk,paste(state,county_name))))]

grep('LOS ANGELES',toupper(unique(with(crosswalk,paste(state,county_name)))),value = T)

# levels(by_cnty$X_STATE) <- c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", 
# 	"COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", 
# 	"FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA",
# 	"IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND",
# 	"MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", 
# 	"MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE",
# 	"NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", 
# 	"NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA",
# 	"RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE",
# 	"TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON",
# 	"WEST VIRGINIA", "WISCONSIN", "WYOMING", "GUAM", "PUERTO RICO",
# 	"U.S. VIRGIN ISLANDS")

# with(subset(by_cnty,X_STATE=='OREGON'),table(X_CNTYNAM,IYEAR))

# lapply(unique(by_cnty$X_STATE),function(x){with(subset(by_cnty,X_STATE==x),table(X_CNTYNAM,IYEAR))})

by_cnty$IDATE <- as.numeric(mdy(by_cnty$IDATE))
by_cnty$state_cnty <- with(by_cnty,paste(X_STATE,X_CNTYNAM))
state_cnty_tbl <- with(by_cnty,table(state_cnty,IYEAR))
state_cnty_tbl_rowsums <- rowSums(state_cnty_tbl<100)
table(state_cnty_tbl_rowsums)
viable_st_cntys <- names(state_cnty_tbl_rowsums[state_cnty_tbl_rowsums<=1])
by_cnty <- subset(by_cnty,state_cnty %in% viable_st_cntys)
# Where to make cutoff of missing years
# table(state_cnty_tbl_rowsums)
# plot(ecdf(state_cnty_tbl_rowsums), do.points=F,col.01line = NULL,verticals=TRUE)

# lapply(seq(8),function(num_missing_yrs){
# 	table(unlist((apply(state_cnty_tbl[state_cnty_tbl_rowsums<=num_missing_yrs,],1,function(yr_obs){which(yr_obs==0)}))))
# })

# dropping 2011, which has so many missing
# by_cnty <- subset(by_cnty,IYEAR<2011)


# dropping this survey biz in favor of XGB to get variable importances
# library(survey)
# 
# options(survey.lonely.psu = "adjust")
# 
# system.time(brfss_design <-
# 	svydesign(
# 		id = ~ 1 ,
# 		strata = ~ X_STSTR,
# 		data = by_cnty ,
# 		weight = ~ X_CNTYWT
# 	))
# 
# svymean( ~ AGE , brfss_design )
# 
# by_cnty$AGE

# XGB to get importances

brfss <- by_cnty
brfss$IYEAR <- parse_integer(brfss$IYEAR)
brfss$outcome <- brfss$QLACTLM2 == 1
brfss$outcome <- as.numeric(brfss$outcome)
brfss <- subset(brfss,!is.na(outcome))

missing_vars <- names(brfss)[colSums(is.na(brfss))>nrow(brfss)/1000]
brfss <- brfss[,!..missing_vars]
brfss <- na.omit(brfss)


# model_iters <- function(brfss) {
	iter_yrs <- sort(unique(brfss$IYEAR),decreasing = F)
	d <- iter_yrs[-c(1,2)][1]
	# eval_by_yrs <- lapply(iter_yrs[-c(1,2)],function(d) {
		print(d)
		va_yr <- d-1
		oos_yr <- d
		
		dat_trva <- subset(brfss,IYEAR == va_yr)
		tr_rows <- createDataPartition(dat_trva$outcome, p = .8, 
																								 list = FALSE, 
																								 times = 1)
		va_rows <- setdiff(seq(nrow(dat_trva)),tr_rows)
		dat_oos <- subset(brfss,IYEAR == oos_yr)

		# 7/21/20 We'll cut the below, all too closely related
		# 1:  USEEQUIP 2.728468e-01 9.605390e-02 0.039149653
		# 2:  POORHLTH 1.324804e-01 4.591618e-02 0.041885919
		# 3:   GENHLTH 1.262460e-01 8.154534e-02 0.035571459
		# 4:  PHYSHLTH 1.012915e-01 4.656002e-02 0.023784466
		# 6:  X_RFHLTH 5.433353e-02 1.446244e-02 0.005683014
		# 9:  MENTHLTH 1.228678e-02 1.997885e-02 0.023363502
		# INTVID is consistently in the top 10...very curious, but would confound the planned analysis\
		# Same with SEQNO
		cut_vars <- c('USEEQUIP','POORHLTH','GENHLTH','PHYSHLTH','X_RFHLTH','MENTHLTH','IDATE',
									'outcome','QLACTLM2','X_CNTYWT','X_WT2','X_CNTY','X_CNTYNAM','ADJCNTY','INTVID','SEQNO','X_STATE','X_PSU')
		
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
		
		# run xgb iters
		# eval_by_md <- lapply(5,function(md) {
			tr_va_xgb_m <- xgb.train(
				objective = "binary:logistic", 
				# booster = 'gblinear',
				eta = .1,
				tree_method = 'hist',
				grow_policy = 'lossguide',
				early_stopping_rounds = 20,
				# verbose=F,
				maximize=T,
				# feval=evalerror,
				eval_metric = 'auc',
				nrounds =  400,
				data = tr_packaged,
				max_depth = 5,
				print_every_n = 10,
				watchlist=list(train=tr_packaged,validate=va_packaged))
		# 	return(tr_va_xgb_m)
		# })
		
		(model_imp <- head(xgb.importance(model=tr_va_xgb_m),10))
			
			
		# trying classical stats
		
		options(survey.lonely.psu = "adjust")
		# des<-svydesign(ids=~1, strata=~X_STSTR, weights=~X_CNTYWT, data = brfss )
		fit1 <- glm(formula(paste('outcome~',paste(c(model_imp$Feature,'IYEAR'),collapse =  '+'))), data=brfss[, c(model_imp$Feature,'X_CNTYWT','outcome','IYEAR'), with=F], weights = X_CNTYWT, family = "binomial")
		summary(fit1)
		
		default_glm_mod = train(
			form = default ~ .,
			data = default_trn,
			trControl = trainControl(method = "cv", number = 5),
			method = "glm",
			family = "binomial"
		)
			
		imp_val <- model_imp$Gain
		imp_vars <- model_imp$Feature
		imp_vars <- unique(c(imp_vars,'state_cnty','outcome','X_CNTYWT'))
		brfss_match_va <- subset(brfss,IYEAR==va_yr)[,..imp_vars]
		brfss_match_oos <- subset(brfss,IYEAR==oos_yr)[,..imp_vars]
		
		# run_mds <- function(hl_a,hl_subs,hl_zips){
		state_cnty_vec <- unique(brfss_match_va$state_cnty)
		acu_counties <- sample(state_cnty_vec,10)
		non_acu_counties <- setdiff(state_cnty_vec,acu_counties)
		brfss_match_va_acu_subs <- lapply(acu_counties,function(x){subset(brfss_match_va,state_cnty==x)[,!'state_cnty']})
		brfss_match_va_non_acu_subs <- lapply(non_acu_counties,function(x){subset(brfss_match_va,state_cnty==x)[,!'state_cnty']})
		
		cl <- makeCluster(8,outfile='')
		registerDoParallel(cl)
		
		matches <- foreach(acu_sub=brfss_match_va_acu_subs,acu_label=acu_counties,.packages=c('Matching','data.table')) %:%
			foreach(non_acu_sub=brfss_match_va_non_acu_subs,non_acu_label=non_acu_counties) %dopar% {
			print(paste(acu_label,non_acu_label))
			# acu_sub=brfss_match_va_acu_subs[[1]]
			# non_acu_sub=brfss_match_va_non_acu_subs[[1]]
			# acu_label=acu_counties[[1]]
			# non_acu_label=non_acu_counties[[1]]
			
			X <- rbind(acu_sub,non_acu_sub)
			Y <- X$outcome
			WTS <- X$X_CNTYWT
			X[,c('outcome','X_CNTYWT'):=NULL]
			X <- data.matrix(X)
			X <- apply(X, 2, function(y) (y - mean(y)) / sd(y) ^ as.logical(sd(y)))
			X <- X %*% diag(imp_val)
			Tr <- c(rep(1,nrow(acu_sub)),rep(0,nrow(non_acu_sub)))
			m <- Match(Y=Y, X=X, Tr=Tr, weights = WTS, ties=F)
			
			return(m)
		}
		# name these objects
		names(matches) <- acu_counties
		matches <- lapply(matches,function(x){
			names(x) <- non_acu_counties
			return(x)
		})
		
		va_ests <- lapply(unlist(matches,recursive = F),function(x){x$est})
		va_ests <- as.numeric(va_ests)
		va_ests <- matrix(va_ests,nrow = length(acu_counties))
		
		va_sds <- lapply(unlist(matches,recursive = F),function(x){x$se.standard})
		va_sds <- as.numeric(va_sds)
		va_sds <- matrix(va_sds,nrow = length(acu_counties))
		
		which_min_by_row <- apply(abs(va_ests),1,which.min)
		est_by_row <- abs(va_ests)[cbind(seq(nrow(va_ests)),which_min_by_row)]
		sd95_by_row <- 2*abs(va_sds)[cbind(seq(nrow(va_sds)),which_min_by_row)]
		table(sig_va <- est_by_row > sd95_by_row)
		
		# for each acu county, get comparison counties
		valid_counties <- non_acu_counties[which_min_by_row]
		names(valid_counties) <- acu_counties
		# diagnostics of selected counties
		table(table(unlist(valid_counties)))
		prop.table(table(duplicated(unlist(valid_counties))))
		
		# acu_county <- names(valid_counties)[1]
		oos_ests_by_acu_county <- lapply(names(valid_counties),function(acu_county){
			
			print(acu_county)
			acu_sub <- subset(brfss_match_oos,state_cnty == acu_county)[,!'state_cnty']
			
			non_acu_counties <- valid_counties[[acu_county]]
			brfss_match_oos_non_acu_subs <- lapply(non_acu_counties,function(x){subset(brfss_match_oos,state_cnty == x)[,!'state_cnty']})
			# non_acu_sub <- brfss_match_oos_non_acu_subs[[1]]
			matches <- lapply(brfss_match_oos_non_acu_subs,function(non_acu_sub){
				X <- rbind(acu_sub,non_acu_sub)
				Y <- X$outcome
				WTS <- X$X_CNTYWT
				X[,c('outcome','X_CNTYWT'):=NULL]
				X <- data.matrix(X)
				X <- apply(X, 2, function(y) (y - mean(y)) / sd(y) ^ as.logical(sd(y)))
				X <- X %*% diag(imp_val)
				Tr <- c(rep(1,nrow(acu_sub)),rep(0,nrow(non_acu_sub)))
				m <- Match(Y=Y, X=X, Tr=Tr, weights = WTS, ties=F)
				return(m)
			})
			oos_ests <- lapply(matches,function(x){
				data.frame('est'=x$est,'se_std'=x$se.standard)
			})
			return(do.call(rbind,oos_ests))
		})
		oos_ests_by_acu_county <- do.call(rbind,oos_ests_by_acu_county)
		table(sig_oos <- with(oos_ests_by_acu_county,abs(est) > 2*se_std))
	})
	eval_by_yrs <- do.call(rbind.data.frame,eval_by_yrs)
	return(eval_by_yrs)
# }













