library('haven')
by_cnty <- lapply(Sys.glob('~/Documents/brfss_smart_data/*'),read_xpt)

name_overlap <- Reduce(intersect, lapply(by_cnty[-1],names))
# sort(name_overlap)

by_cnty <- lapply(by_cnty[-1],function(x){x[,name_overlap]})
by_cnty <- do.call(rbind,by_cnty)
names(by_cnty) <- make.names(names(by_cnty))
by_cnty$X_STATE <- factor(by_cnty$X_STATE)
levels(by_cnty$X_STATE) <- c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", 
	"COLORADO", "CONNECTICUT", "DELAWARE", "DISTRICT OF COLUMBIA", 
	"FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA",
	"IOWA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND",
	"MASSACHUSETTS", "MICHIGAN", "MINNESOTA", "MISSISSIPPI", 
	"MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE",
	"NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", 
	"NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON", "PENNSYLVANIA",
	"RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA", "TENNESSEE",
	"TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON",
	"WEST VIRGINIA", "WISCONSIN", "WYOMING", "GUAM", "PUERTO RICO",
	"U.S. VIRGIN ISLANDS")

with(subset(by_cnty,X_STATE=='OREGON'),table(X_CNTYNAM,IYEAR))

lapply(unique(by_cnty$X_STATE),function(x){with(subset(by_cnty,X_STATE==x),table(X_CNTYNAM,IYEAR))})

by_cnty$state_cnty <- with(by_cnty,paste(X_STATE,X_CNTYNAM))
state_cnty_tbl <- with(by_cnty,table(state_cnty,IYEAR))
state_cnty_tbl_rowsums <- rowSums(state_cnty_tbl==0)
# Where to make cutoff of missing years
table(state_cnty_tbl_rowsums)
plot(ecdf(state_cnty_tbl_rowsums), do.points=F,col.01line = NULL,verticals=TRUE)

lapply(seq(8),function(num_missing_yrs){
	table(unlist((apply(state_cnty_tbl[state_cnty_tbl_rowsums<=num_missing_yrs,],1,function(yr_obs){which(yr_obs==0)}))))
})

# dropping 2011, which has so many missing
by_cnty <- subset(by_cnty,IYEAR<2011)


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

library(xgboost)
library(fst)
library(readr)
`%notin%` <- Negate(`%in%`)
library(FNN)



evalerror <- function(preds, dtrain) {
	labels <- getinfo(dtrain, "label")
	mae <- mean(log(abs(preds/labels-1)))
	return(list(metric = "error", value = mae))
}
brfss <- by_cnty
brfss$IYEAR <- parse_integer(brfss$IYEAR)
model_iters <- function(brfss) {
	iter_yrs <- sort(unique(brfss$IYEAR),decreasing = F)
	
	eval_by_yrs <- lapply(iter_yrs[-1],function(d) {
		print(d)
		va_yr <- d-1
		oos_yr <- d
		
		dat_trva <- subset(brfss,IYEAR == va_yr)
		tr_rows <- #dat_trva$sale_yr_2 < va_yr
		va_rows <- #dat_trva$sale_yr_2 >= va_yr & dat_trva$sale_yr_2 < oos_yr
		dat_oos <- subset(brfss,IYEAR == oos_yrss)
		
		tr_packaged <- xgb.DMatrix(data.matrix(dat_trva[tr_rows,!c('sale_price','active_mls_number')]),
															 label=dat_trva[tr_rows,sale_price],
															 weight=dat_trva[tr_rows,sale_price]*dat_trva[tr_rows,active_areabuilding]
		)
		va_packaged <- xgb.DMatrix(data.matrix(dat_trva[va_rows,!c('sale_price','active_mls_number')]),
															 label=dat_trva[va_rows,sale_price],
															 weight=dat_trva[va_rows,sale_price]*dat_trva[va_rows,active_areabuilding]
		)
		oos_packaged <- xgb.DMatrix(data.matrix(dat_oos[,!c('sale_price','active_mls_number')]),
																label=dat_oos[,sale_price],
																weight=dat_oos[,sale_price]*dat_oos[,active_areabuilding]
		)
		
		# run xgb iters
		eval_by_md <- lapply(5,function(md) {
			tr_va_xgb_m <- xgb.train(
				objective = "reg:linear", 
				# booster = 'gblinear',
				eta = .1,
				tree_method = 'hist',
				grow_policy = 'lossguide',
				early_stopping_rounds = 20,
				verbose=F,
				maximize=F,
				feval=evalerror,
				nrounds =  400,
				data = tr_packaged,
				max_depth = md,
				# print_every_n = 10,
				watchlist=list(train=tr_packaged,validate=va_packaged))
			return(tr_va_xgb_m)
		})
		saveRDS(eval_by_md[[1]],paste0('~/Documents/GitHub/data-science/attom_avm_data/attom_xgb_',county_fips,'.RDS'))
		xgb_m_preds <- predict(eval_by_md[[1]],
													 data.matrix(dat_oos[,!c('sale_price','active_mls_number')]))
		xgb_m_raw_err <- xgb_m_preds/dat_oos$sale_price-1
		xgb_m_imp <- xgb.importance(feature_names = eval_by_md[[1]]$feature_names,model = eval_by_md[[1]])
		
		# knn
		dat_trva_scaled <- scale(rbind(data.matrix(dat_trva),data.matrix(dat_oos)))
		dat_trva_scaled[is.na(dat_trva_scaled)] <- 0
		dat_oos_scaled <- dat_trva_scaled[seq(nrow(dat_trva)+1,nrow(dat_trva_scaled)),]
		dat_trva_scaled <- dat_trva_scaled[seq(nrow(dat_trva)),]
		
		tr_x <- dat_trva_scaled[tr_rows,-which(attr(dat_trva_scaled,'dimnames')[[2]]=='sale_price')]
		va_x <- matrix(dat_trva_scaled[va_rows,-which(attr(dat_trva_scaled,'dimnames')[[2]]=='sale_price')],nrow=sum(va_rows))
		tr_va_x <- dat_trva_scaled[,-which(attr(dat_trva_scaled,'dimnames')[[2]]=='sale_price')]
		oos_x <- matrix(matrix(dat_oos_scaled,nrow=ifelse(is.null(nrow(dat_oos_scaled)),1,nrow(dat_oos_scaled)))[,-which(attr(dat_trva_scaled,'dimnames')[[2]]=='sale_price')],nrow=ifelse(is.null(nrow(dat_oos_scaled)),1,nrow(dat_oos_scaled)))
		
		# cbind(colnames(tr_x)[match(xgb_m_imp$Feature,colnames(tr_x))],xgb_m_imp$Feature) # match function check
		munged_importance <- apply(xgb_m_imp[,c(3,4)],1,max)
		# barplot(munged_importance)
		to_diag <- diag(munged_importance)
		col_matchup <- match(xgb_m_imp$Feature,colnames(tr_x))
		tr_x <- tr_x[,col_matchup] %*% to_diag
		va_x <- va_x[,col_matchup] %*% to_diag
		oos_x <- oos_x[,col_matchup] %*% to_diag
		tr_va_x <- tr_va_x[,col_matchup] %*% to_diag
		
		best_k <- 10# which.min(eval_by_k_agg$resid)
		
		oos_kr <- knn.reg(train = tr_va_x,
											test = oos_x,
											y = dat_trva$sale_price,
											k = best_k)
		
		oos_kr_min_nn_dists <- apply(attr(oos_kr,'nn.dist'),1,min)
		oos_kr_resid_centered <- oos_kr$pred/dat_oos$sale_price-1
		
		model_comp_lm <- lm(xgb_m_raw_err~oos_kr_resid_centered)
		
		indices <- attr(oos_kr, "nn.index")
		dists <- attr(oos_kr, "nn.dist")
		
		ensemble_pred <- rowMeans(cbind(oos_kr$pred, xgb_m_preds))
		ensemble_resid <- ensemble_pred/dat_oos$sale_price-1
		
		return(data.frame(xgb_pred=xgb_m_preds
											,xgb_resid=xgb_m_raw_err
											,knn_pred=oos_kr$pred
											,knn_resid=oos_kr_resid_centered
											,ensemble_pred=ensemble_pred
											,ensemble_resid=ensemble_resid
											,d=d
											,actual=dat_oos$sale_price
											,county=county_fips
											,min_nn_dist=oos_kr_min_nn_dists
											,model_disagreement=abs(log(xgb_m_preds/oos_kr$pred))
											,oos_mls_numbers=oos_mls_numbers))
		
	})
	eval_by_yrs <- do.call(rbind.data.frame,eval_by_yrs)
	
	
	par(mfcol=c(2,2))
	(knn_distance_cutoff_val <- with(eval_by_yrs,
																	 quantile(min_nn_dist,1-cor(min_nn_dist,
																	 													 log(abs(knn_resid)))^2)))
	with(eval_by_yrs,plot(min_nn_dist,abs(knn_resid)))
	abline(v=knn_distance_cutoff_val,col='orange')
	axis(2,at=seq(0,1,.1))
	with(eval_by_yrs,plot(min_nn_dist,abs(xgb_resid)))
	abline(v=knn_distance_cutoff_val,col='orange')
	axis(2,at=seq(0,1,.1))
	# saveRDS(knn_distance_cutoff_val,paste0('~/model_files_backup/knn_distance_cutoff_val_',county_fips,'.RDS'))
	
	(model_disagreement_cutoff_val <- with(eval_by_yrs,
																				 quantile(model_disagreement,1-cor(model_disagreement,
																				 																	log(abs(knn_resid)))^2)))
	with(eval_by_yrs,plot(model_disagreement,abs(knn_resid)))
	abline(v=model_disagreement_cutoff_val,col='orange')
	axis(2,at=seq(0,1,.1))
	with(eval_by_yrs,plot(model_disagreement,abs(xgb_resid)))
	abline(v=model_disagreement_cutoff_val,col='orange')
	axis(2,at=seq(0,1,.1))
	# saveRDS(model_disagreement_cutoff_val,paste0('~/model_files_backup/model_disagreement_cutoff_val_',county_fips,'.RDS'))
	return(eval_by_yrs)
}













