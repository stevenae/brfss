# install.packages( "devtools" , repos = "http://cran.rstudio.com/" )
# library(devtools)
# install_github( "ajdamico/lodown" , dependencies = TRUE )

lodown( "brfss" , output_dir = file.path( path.expand( "~" ) , "BRFSS" ) )

options( survey.lonely.psu = "adjust" )

library(survey)
library(lubridate)

variables_to_keep <-
	c( 'one' , 'xpsu' , 'xststr' , 'xllcpwt' , 'genhlth' , 'medcost' , 
		 'xstate' , 'xage80' , 'nummen' , 'numadult' , 'hlthpln1' )

yrs <- year(seq(ymd("2011-1-1"), ymd("2015-1-1"), by = "years"))
multi_df <- lapply(yrs, function(yr){
	brfss_df <- readRDS( file.path( path.expand( "~" ) , "BRFSS" , paste0(yr," main.rds" ) ));
	print(rbind(variables_to_keep,variables_to_keep %in% names(brfss_df)))
	brfss_df[ variables_to_keep ]
})

brfss_df <-
	readRDS( file.path( path.expand( "~" ) , "BRFSS" , "2010 main.rds" ) )

lapply(multi_df,function(x)grep('age',names(x),value = T))

brfss_df <- brfss_df[ variables_to_keep ] ; gc()

brfss_design <-
	svydesign(
		id = ~ xpsu ,
		strata = ~ xststr ,
		data = brfss_df ,
		weight = ~ xllcpwt ,
		nest = TRUE
	)

str(brfss_df)

brfss_design <- 
	update( 
		brfss_design ,
		
		fair_or_poor_health = ifelse( genhlth %in% 1:5 , as.numeric( genhlth > 3 ) , NA ) ,
		
		couldnt_see_doc_due_to_cost = 
			factor( 
				medcost , 
				levels = c( 1 , 2 , 7 , 9 ) , 
				labels = c( "yes" , "no" , "dk" , "rf" ) 
			) ,
		
		state_name =
			
			factor(
				
				xstate ,
				
				levels = 
					c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 
						21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 
						37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 
						55, 56, 66, 72, 78) ,
				
				labels = 
					c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", 
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
				
			)
	)

sum( weights( brfss_design , "sampling" ) != 0 )

svyby( ~ one , ~ state_name , brfss_design , unwtd.count )

svytotal( ~ one , brfss_design )

svyby( ~ one , ~ state_name , brfss_design , svytotal )

glm_result <- 
	svyglm( 
		xage80 ~ fair_or_poor_health + couldnt_see_doc_due_to_cost , 
		brfss_design
	)

summary( glm_result )














