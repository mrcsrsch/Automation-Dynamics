##################################################################################
##### Automation Workforce Dynamics in (Non-)MNEs, master script  ###### 
# This script loads global settings and
# sources all other scripts in the folder to 
# i) build the analysis dataset
# ii) run the analyses
# Setting "run.scripts" to TRUE will execute the full project.
# Please set the project path in "main_path" and
# make sure everything is set up (see the main README of the replication package)
##################################################################################
##################################################################################
# clean start 
rm(list=ls(all.names = TRUE))
gc() # return memory to system

# track memory usage
# gcinfo(FALSE)
# increase the memory limit to server maximum
memory.limit(size=55500)

# global packages
if (!require("data.table")) install.packages("data.table"); library("data.table")
if (packageVersion("data.table")!="1.14.2") warning("Analysis ran in data.table version 1.14.2 - consider up- or downgrading")

# create folder structure and set paths
## SET YOUR MAIN FOLDER AND DATA SOURCE FOLDER HERE
main_path <- NA 
if (is.na(main_path)) warning("You need to set the main folder path.")

## set paths
map_scripts <- paste0(main_path, "scripts/")
map_data_analysis <- paste0(main_path, "data/")
map_data_source <- paste0(map_data_analysis, "source_data/")
map_output <-  paste0(main_path, "outputs/")

## create data and output paths if necessary
if (!dir.exists(map_data_analysis)) dir.create(map_data_analysis)
if (!dir.exists(map_output)) dir.create(map_output)


##################################################################################
### Call scripts to run analysis ####

run.scripts <- FALSE # set to TRUE to execute full project.
countert <- 0
ttimers <- list()

# small functions to wrap around script calls
start.part <- function(){
  # save and show timer
  countert <<- countert + 1 # update in global environment
  ttimers[[countert]] <<- Sys.time()
  cat(paste(ttimers[[countert]]), "\n")
}


end.part <- function(){
  # save and show timer
  ttimers[[countert]] <<- Sys.time()-ttimers[[countert]]
  print(ttimers[[countert]])
  # get list of objects to remove
  objs <<- ls(all.names = TRUE)[-which(ls(all.names = TRUE) %in% c("map_data_analysis", "map_data_source", "map_scripts", "map_output", "countert", "ttimers", "end.part", "start.part", "run.scripts"))]
}

if (run.scripts){
  ##################################################################################
	
  TO DO: ADJUST ALL CALLS HERE.
  ### 01 Load and clean data ####
  
  #### Firm level data ####
  # retrieve firm level data from SQL server (Note: you may need to adjust this depending on your source for firm level data within CBS)
  # clean firm level data
  start.part()
  source(paste0(map_scripts, "01A_companydata.R"), echo=TRUE, verbose = FALSE, max.deparse.length = 100000, spaced = TRUE)
  end.part()
  rm(list=objs); gc()
  
  ##################################################################################
  # save timers
  saveRDS(ttimers, file=paste0(map_data_paper2, "project_timers.rds"))
}
