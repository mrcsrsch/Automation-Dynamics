##################################################################################
### Build worker data for workers in firm data #####
##################################################################################
### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table")
if (packageVersion("data.table")!="1.14.8") warning("Analysis ran in data.table version 1.14.8 - consider up- or downgrading")

### Output dir #####
if (!dir.exists(paste0(map_data_analysis, "step3/"))) dir.create(paste0(map_data_analysis, "step3/"))
map_output_here <- paste0(map_data_analysis, "step3/")

### load data #####
selected_firms <- readRDS(paste0(map_data_analysis, "step3/selected_firms_2010_2021.rds")) # selected firms panel

##################################################################################
#### load POLIS (no internships; age 18-65) #######
POLIS <- list()
k <- 0
for (current_year in 2010:2021){ 
  cat(paste(Sys.time(), ":", current_year), "\n")
  k <- k +1
  POLIS[[k]] <- readRDS(paste0(map_data_analysis, "step1/SPOLIS_yearly/POLIS_", current_year, ".rds"))
}
rm(current_year, k)

POLIS <- rbindlist(POLIS)
gc()

#### create numeric workerID #######
RINPs <- data.table(RINP=POLIS[, unique(RINP)])
RINPs[, workerID := 1:.N]
saveRDS(RINPs, paste0(map_output_here, "RINP_workerID_trans.rds"), compress=T)
POLIS <- merge(POLIS, RINPs, by="RINP") # add workerID to POLIS
POLIS[, RINP := NULL]
setcolorder(POLIS,c("workerID", "year", "SBEID"))

##################################################################################
#### extract workers that ever employed at a firm in firm data #####
firmIDs <- selected_firms[, unique(SBEID)]
workerIDs <- POLIS[SBEID %in% firmIDs, unique(workerID)]
POLIS_worker <- POLIS[workerID %in% workerIDs, ] # subset to workerIDs in POLIS_firm (i.e. also SBEID-years not captured in POLIS_firm)
rm(POLIS)
rm(workerIDs)
gc()

##################################################################################
#### calculate overall measures on POLIS #################################
POLIS_overall <- POLIS_worker[, lapply(.SD, sum), by=.(workerID, year), .SDcols = c("SBASISLOON", "SBIJZONDEREBELONING", "SLNOWRK", "SOVERWERKUREN", "SREGULIEREUREN")]
saveRDS(POLIS_overall, paste0(map_output_here, "POLIS_overall.rds"), compress=T)
rm(POLIS_overall)
##################################################################################
#### pick main job per year ##### 
POLIS_worker[, .N]
POLIS_worker[, obs := .N, by=.(workerID, year)] # find workerID-years with multiple obs
POLIS_worker[obs==1, keep := T]
#POLIS_worker[, uniqueN(workerID[obs>1])/uniqueN(workerID), by=year][order(year),] # around 25% workers per year
##### employment at firm in selected_firms ####
POLIS_worker[obs > 1, in_sample := SBEID %in% firmIDs]
POLIS_worker[obs > 1, keep := ifelse(all(!in_sample), T, in_sample), by=.(workerID, year)]
POLIS_worker <- POLIS_worker[keep==T, !c("in_sample")]

POLIS_worker[obs>1, obs := .N, by=.(workerID, year)] # update obs for remaining workers

##### other criteria #####
POLIS_worker[, inc_firm := SBASISLOON]
POLIS_worker[, hours_firm := SREGULIEREUREN]

POLIS_worker[obs>1, keep := inc_firm==max(inc_firm), by=.(workerID, year)] # maximum income
POLIS_worker <- POLIS_worker[keep==T, ]
POLIS_worker[obs>1, obs := .N, by=.(workerID, year)] # update obs for remaining workers

POLIS_worker[obs>1, keep := hours_firm==max(hours_firm), by=.(workerID, year)] # maximum hours
POLIS_worker <- POLIS_worker[keep==T, ]
POLIS_worker[obs>1, obs := .N, by=.(workerID, year)] # update obs for remaining workers

POLIS_worker[obs>1, keep := SCONTRACTSOORT==min(SCONTRACTSOORT), by=.(workerID, year)] # best contract type
POLIS_worker <- POLIS_worker[keep==T, ]
POLIS_worker[obs>1, obs := .N, by=.(workerID, year)] # update obs for remaining workers

POLIS_worker[obs>1, keep := job.days==max(job.days), by=.(workerID, year)] # maximum job days in year
POLIS_worker <- POLIS_worker[keep==T, ]
POLIS_worker[obs>1, obs := .N, by=.(workerID, year)] # update obs for remaining workers

POLIS_worker[obs>1, keep := c(T, rep(F, .N-1)), by=.(workerID, year)] # for rest just keep one observation
POLIS_worker <- POLIS_worker[keep==T, !c("keep", "obs")]
POLIS_worker[, .N]

POLIS_worker[, obs := .N, by=.(workerID, year)] # update obs for remaining workers
POLIS_worker[, .N, by=obs]

##################################################################################
#### calculate real hourly wages ##### 
POLIS_worker <- merge(POLIS_worker, cpi, by="year")
POLIS_worker[, hwage := (SBASISLOON+SBIJZONDEREBELONING+SLNOWRK)/(SOVERWERKUREN+SREGULIEREUREN)]
POLIS_worker[, lrhwage := log(hwage * factor)]

## lag hourly wages
setorderv(POLIS_worker, c("workerID", "year"))
POLIS_worker[, paste0("lrhwage_", c(1,2)) := shift(lrhwage, n=c(1,2), type="lag"), by=workerID]


##################################################################################
#### Subset workers ##### 
# log real hourly wage changes outside -1 and 1
POLIS_worker[, wage_change := abs(lrhwage - lrhwage_1)]
POLIS_worker[, delete := any(wage_change>1, na.rm=T), by=workerID]
POLIS_worker[, .N, by=delete]
POLIS_worker <- POLIS_worker[delete==F, !c("delete", "wage_change")]

##### from SECM, remove observations where worker is a student #####
RINP_workerID_selected <- merge(POLIS_worker[, .(workerID=unique(workerID))], RINPs, by="workerID")

##################################################################################
##################################################################################
##### read in data and prepare #####
SECM <- fread(paste0(map_data_source,"SECMBUS/SECMBUSBUSV1.csv"), sep=";")

# create RINP
SECM[, RINP := paste0(RINPERSOONS, RINPERSOON)]

# extract only necessary vars
SECM <- SECM[, c("RINP", "AANVSECM", "EINDSECM", "SECM")]
gc()

##### subset SECM to workers in dataset #####
SECM[, .N]
SECM <- merge(SECM, RINP_workerID_selected, by=c("RINP"))
SECM[, .N]
rm(RINP_workerID_selected)
SECM[, RINP := NULL]
gc()

##### adjust date format #####
SECM[, c("begin", "end") := .(as.IDate(paste0(AANVSECM), format="%Y%m%d"),
                              as.IDate(paste0(EINDSECM), format="%Y%m%d"))]
SECM[, c("AANVSECM", "EINDSECM") := NULL]

##### subset SECM to years in main dataset #####
SECM[, year := year(end)]
SECM[, .N]
SECM <- SECM[year>=2010, ] # only look at events that end after 2010
SECM[, .N]
SECM[, year := NULL]
gc()

## adjust start dates to beginning of data set (don't need earlier SECM spells)
SECM[begin < as.IDate("2010-01-01"), begin := as.IDate("2010-01-01")]

##### subset SECM status #####
# In the end I want to identify
# 1. days spend in employment (SECM %in% c(11, 12))
# 2. days spend in self-employment (SECM %in% c(13, 14))
# 3. days spend in pension (SECM == 25)
# 4. days spend non-employed (all other categories, except student)
# 5. workers that are students in a given year (SECM %in% c(26,31)) --> I will remove these
SECM[SECM %in% c(11, 12) , SECM_new := 1]
SECM[SECM %in% c(13, 14) , SECM_new := 2]
SECM[SECM %in% c(25) , SECM_new := 3]
SECM[SECM %in% c(26,31) , SECM_new := 5]
SECM[is.na(SECM_new), SECM_new := 4]
# delete original SECM var
#SECM[, SECM := NULL]

##################################################################################
### Expand SECM by year and calculate days per SECM cat. #####
SECM[, c("year_begin", "year_end") := .(year(begin), year(end))]
gc()
# for full years, we assign 365 days, ignoring leap years
timer <- Sys.time()
timer
# for full years, we assign 365 days, ignoring leap years
SECM_expanded <- SECM[, .(year=year_begin:year_end, year_begin, year_end, end, days = 365), .(workerID, SECM_new, begin)]
# in other years need to calculate days
## begin and end within, same year
SECM_expanded[year_begin==year & year_end==year, days := end-begin+1]
## begin and end in different year
### begin year
SECM_expanded[year_begin==year & year_end!=year, days := as.IDate(paste0(year, "-12-31")) - begin + 1]
### end year
SECM_expanded[year_begin!=year & year_end==year, days := end - as.IDate(paste0(year, "-01-01")) + 1]

### because of the SECM_new aggregation, there are now duplicate workerID-year-SECM_new entries  --> aggregate
SECM_expanded <- SECM_expanded[, .(days=sum(days)), by=c("workerID", "year", "SECM_new")]
### adjust leap years
SECM_expanded[days > 365, days := 365]

### create column per SECM cat. ####
# Pivot the data so that each SECM becomes a column
SECM_wide <- dcast(SECM_expanded, workerID + year ~ SECM_new, value.var = "days", fill=0)
setnames(SECM_wide, c("workerID", "year", "days_employed", "days_self_employed", "days_pension", "days_non_employed", "days_student"))

timer <- Sys.time()-timer
timer
rm(timer)
##################################################################################
### save result #### 
saveRDS(SECM_wide, paste0(map_output_here, "SECM_wide.rds"), compress=T)

##################################################################################
##################################################################################
# actually subset
POLIS_worker[, .N]
POLIS_worker <- merge(POLIS_worker, SECM_wide[days_student > days_employed, .(workerID, year, student=T)], by=c("workerID", "year"), all.x=T)
POLIS_worker[is.na(student), student := F]
POLIS_worker[, .N, by=student]

POLIS_worker <- POLIS_worker[student==F, !("student")]
POLIS_worker[, .N]

rm(SECM_wide, SECM_expanded, SECM)
##################################################################################

##### Subset to single spell #####
# subset to SBEID-workerID matched with only one spell, i.e. no breaks. 
# This is important if you don't focus on stayers because otherwise "same_firm" can create problems
POLIS_worker[order(workerID, year), include := all(diff(year)==1), by=c("workerID", "SBEID")]
#POLIS_worker[, sum(include)/.N] # 5% of observation
## remove observations
POLIS_worker <- POLIS_worker[include==T, !c("include")]

# again subset to workers that are in selected_firms
firmIDs <- selected_firms[, unique(SBEID)]
workerIDs <- POLIS_worker[SBEID %in% firmIDs, unique(workerID)]
POLIS_worker[, .N]
POLIS_worker <- POLIS_worker[workerID %in% workerIDs, ] # subset to workerIDs in POLIS_firm (i.e. also SBEID-years not captured in POLIS_firm)
POLIS_worker[, .N ]
workerIDs <- POLIS_worker[, unique(workerID)]


##################################################################################
#### add identifiers for managers (manager data and EBB) ####
###### load bestuurders data to indentify managers ##### 
best <- list()
i <- 0
for (year in 2010:2021){
  # message 
  cat(paste(Sys.time(), "reading", year), "\n")
  i <- i + 1
  best[[i]] <- fread(file=paste0(map_data_source, "BESTUURDERS/FCV", year, "_Kenmerken.csv"))
  best[[i]][, "year" := year]
}
rm(i)
best <- rbindlist(best)
setnames(best, c("CBKSoortNR", "Rinpersoon", "OgIdentificatie"), c("RINPERSOONS", "RINPERSOON", "OG_BEID"))
# set identifier
best[, manager_best := TRUE]
best <- best[, c("RINPERSOONS", "RINPERSOON", "year", "OG_BEID", "manager_best")]

# translate RINP to workerID and only keep workerIDs in POLIS_worker
best[, RINP := paste0(RINPERSOONS, RINPERSOON)]
best[, c("RINPERSOONS", "RINPERSOON") := NULL]
best <- merge(best, RINPs[workerID %in% workerIDs, ], by="RINP")
best[, RINP := NULL]

# add to POLIS_worker
## temporarily add OG_BEID to POLIS_worker
beog <- readRDS(paste0(map_data_analysis, "step1/company/beog.rds")) 
POLIS_worker <- merge(POLIS_worker, beog, by=c("SBEID", "year"), all.x=T)
rm(beog)
## add best to POLIS_worker
POLIS_worker <- merge(POLIS_worker, best, by=c("workerID", "year", "OG_BEID"), all.x=T)
## clean up 
POLIS_worker[, OG_BEID := NULL]
rm(best)

gc()
# note: managers will be assigned within SBEID-workerID matches below

###### load EBB data to identify managers ##### 
# load EBBs 
t <- 0
colClass <- c("character", "character", "NULL", "character", "integer")
EBB <- list()
for (current_year in 2010:2019){ # only available up to 2019; but that's not problem given the classificaiton below
  # message
  cat(paste(Sys.time(), "Starting with year:", current_year), "\n")
  t <- t+1
  
  # load EBB
  path <- paste0(map_data_source,"EBB/EBB", current_year, "1299ANAV1.csv")
  EBB[[t]] <- fread(path, sep=";",colClasses = colClass)
}
rm(t, path, colClass, current_year)
EBB <- rbindlist(EBB)
setnames(EBB, c("EBBTW1ISCO2008V", "jaar"), c("ISCO08", "year"))

# remove "Armed Forces Occupations" (not applicable here)
EBB <- EBB[substr(ISCO08,1,1)!="0", ]

# create numeric ISCO08
EBB[, ISCO08 := as.integer(ISCO08)]

# translate RINP to workerID and only keep workerIDs in POLIS_worker
EBB[, RINP := paste0(RINPERSOONS, RINPERSOON)]
EBB[, c("RINPERSOONS", "RINPERSOON") := NULL]
EBB <- merge(EBB, RINPs[workerID %in% workerIDs, ], by="RINP")
EBB[, RINP := NULL]

# remove duplicates (#1)
EBB <- unique(EBB)

# I update the manager ID based on the full EBB
EBB[, manager_EBB := substr(ISCO08,1,1)==1]
POLIS_worker <- merge(POLIS_worker, EBB[manager_EBB==T, c("workerID", "year", "manager_EBB")], by=c("workerID", "year"), all.x=T)
rm(EBB)

# adjust manager at workerID-SBEID level
POLIS_worker[is.na(manager_best), manager_best := F]
POLIS_worker[is.na(manager_EBB), manager_EBB := F]
POLIS_worker[, manager := manager_best | manager_EBB]
POLIS_worker[, c("manager") := .(any(manager)), by=c("workerID", "SBEID")] # if ever manager, assign permanently

##################################################################################
#### add education data and manipulate #####
# load yearly EDUC data 
EDUC <- readRDS(paste0(map_data_analysis, "step1/EDUC.rds"))
EDUC <- EDUC[year %in% 2010:2021, ]
# add workerID
EDUC[, .N]
EDUC <- merge(EDUC, RINPs, by="RINP")
EDUC[, RINP := NULL]
EDUC[, .N]
# subset EDUC to workerIDs in POLIS_worker
EDUC <- EDUC[workerID %in% workerIDs, ]
EDUC[, .N]
gc()
# define three levels of education level
EDUC[, educ_level := as.integer(substr(OPLNIVSOI2021AGG1HB,1,1))]
# merge relevant vars to POLIS_firm and POLIS_worker
POLIS_worker <- merge(POLIS_worker, EDUC[, c("workerID", "year", "educ_level", "OPLRICHTING")], by=c("workerID", "year"), all.x=T)

rm(EDUC)
rm(workerIDs)
gc()

##################################################################################
#### calculate weeks spend in job in current year ##### 
POLIS_worker[, days_job := as.integer((job.exit-job.entry)+1)]

##################################################################################
#### subset to necessary vars #####
POLIS_worker <- unique(POLIS_worker[, c("workerID", "year", "SBEID",
                                 "SBASISLOON", "SBIJZONDEREBELONING", "SLNOWRK",
                                 "SOVERWERKUREN", "SREGULIEREUREN", "days_job",
                                 "SCONTRACTSOORT", "deeltijdfactor",
                                 "female", "birthcountry", "age",
                                 "factor", "hwage", "lrhwage", "lrhwage_1", "lrhwage_2", 
                                 "manager", "manager_best", "manager_EBB",
                                 "educ_level", "OPLRICHTING")]) 
##################################################################################
#### save POLIS_worker (all workerIDs in selected_firms) ###### 
saveRDS(POLIS_worker, paste0(map_output_here, "POLIS_worker.rds"), compress=T)
rm(cpi, RINPs, selected_firms, firmIDS, year)
gc()