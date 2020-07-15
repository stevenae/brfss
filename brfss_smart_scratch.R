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











