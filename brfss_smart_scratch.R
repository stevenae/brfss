library('haven')
by_cnty <- lapply(Sys.glob('~/Documents/brfss_smart_data/*'),read_xpt)

name_overlap <- Reduce(intersect, lapply(by_cnty[-1],names))
sort(name_overlap)

by_cnty <- do.call(rbind,by_cnty)

cnty02$A_STATE <- factor(cnty02$A_STATE)
levels(cnty02$A_STATE) <- c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", 
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

with(subset(cnty02,A_STATE=='OREGON'),table(A_CNTYNA))








