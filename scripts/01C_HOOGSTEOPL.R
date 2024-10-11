##################################################################################
### Read in Hoogsteopl and classify levels / directions #####
##################################################################################
#### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table")

#### output dir #####
if (!dir.exists(paste0(map_data_analysis, "step1/"))) dir.create(paste0(map_data_analysis,  "step1/"))
map_output_here <- paste0(map_data_analysis, "step1/")
##################################################################################
##################################################################################
### read hoogste behaalde opleiding from source #####
EDUC <- list()
i <- 0
for (y in 2006:2021){
  # message
  cat(paste(Sys.time(), y, "\n"))
  i <- i+1
  EDUC[[i]] <- fread(file=paste0(map_data_source, "HOOGSTEOPLTAB/HOOGSTEOPLTAB", y, "1299ANAV1.csv"), colClasses="character")
  EDUC[[i]][, year := y]
}
rm(y, i)
EDUC <- rbindlist(EDUC, fill=T, use.names=T)

### combine and subset #####
# create RINP and subset
EDUC[, RINP := paste0(RINPERSOONS, RINPERSOON)]
EDUC <- EDUC[, c("RINP", "year", "OPLNRHB")]
gc()

##################################################################################
### use refboeks to determine level and direction #####
#### load refboeks and combine #####
# NOTE: The variable definitions follow from the relevant .INC/.bla files in the input folder. oplrichting_definities is from SSB webtool.
# read OPLEIDINGSNRREF
OPLEIDINGSNRREF <- fread(file = paste0(map_data_analysis, "input/EDUC/OPLEIDINGSNRREFV30.txt"), header=F, fill=T, colClasses="character")
# extract vars
OPLEIDINGSNRREF[, OPLNR := substr(V1, 1, 6)]
OPLEIDINGSNRREF[, CTO := substr(V1, 105, 109)]
OPLEIDINGSNRREF[, CTO2021V := substr(V1, 274, 278)] # to merge education level (2016 version of CTO)
OPLEIDINGSNRREF[, ISCEDF2013RICHTINGNLSOI2021V := substr(V1, 286, 289)] # to merge education direction
OPLEIDINGSNRREF <- OPLEIDINGSNRREF[-1, c("OPLNR", "CTO", "CTO2021V", "ISCEDF2013RICHTINGNLSOI2021V")]
OPLEIDINGSNRREF[CTO == "     ", CTO := NA_character_]
OPLEIDINGSNRREF[CTO2021V == "     ", CTO2021V := NA_character_]
OPLEIDINGSNRREF[ISCEDF2013RICHTINGNLSOI2021V %in% c("", "    ") , ISCEDF2013RICHTINGNLSOI2021V := NA_character_]

# determine opleidingsniveau 
## from CTOREFV12
CTOREFV12 <- fread(file = paste0(map_data_analysis, "input/EDUC/CTOREFV12.txt"), header=F, fill=T, colClasses="character")
CTOREFV12 <- CTOREFV12[, .(CTO=V1, OPLNIVSOI2021AGG1HB=V33)]
opl <- merge(OPLEIDINGSNRREF[, .(OPLNR, CTO=CTO2021V, ISCEDF2013RICHTINGNLSOI2021V)], CTOREFV12[, .(CTO, OPLNIVSOI2021AGG1HB)], by="CTO", all.x=T) # "Opleidingsniveau hoogst behaald 3-deling (publicatie) SOI 2021" 

# determine and add opleidingsrichting
# from ISCEDF2013RICHTINGREFV2
ISCEDF2013RICHTINGREFV2 <- fread(file = paste0(map_data_analysis, "input/EDUC/ISCEDF2013RICHTINGREFV2.txt"), header=F, fill=T, colClasses="character")
ISCEDF2013RICHTINGREFV2 <- ISCEDF2013RICHTINGREFV2[, .(ISCEDF2013RICHTINGNLSOI2021V=V1, ISCEDF2013RICHTINGPUBLICATIEINDNL=V4)]
opl <- merge(opl, ISCEDF2013RICHTINGREFV2, by="ISCEDF2013RICHTINGNLSOI2021V", all.x=T) # "ISCEDF2013 RICHTING PUBLICATIEINDELING 11 GROEPEN" 

# add descriptions of ISCEDF2013RICHTINGPUBLICATIEINDNL
oplrichting_definities <- fread(file = paste0(map_data_analysis, "input/EDUC/oplrichting_definities.csv"), header=T, fill=T, colClasses="character")
opl <- merge(opl, oplrichting_definities, by="ISCEDF2013RICHTINGPUBLICATIEINDNL", all.x=T)
rm(OPLEIDINGSNRREF, CTOREFV12, ISCEDF2013RICHTINGREFV2, oplrichting_definities)

#### add to EDUC #####
EDUC <- merge(EDUC, opl[, .(OPLNRHB=OPLNR, OPLNIVSOI2021AGG1HB, OPLRICHTING)], by="OPLNRHB", all.x=T)
#EDUC[, unique(OPLNIVSOI2021AGG1HB)]
EDUC[, OPLNIVSOI2021AGG1HB := as.integer(OPLNIVSOI2021AGG1HB)]
EDUC[, uniqueN(OPLNRHB), by=is.na(OPLNIVSOI2021AGG1HB)] # 0 / 34324 OPLNRHB without level
# let's remove workers without level
EDUC <- EDUC[!is.na(OPLNIVSOI2021AGG1HB),]
EDUC[, uniqueN(OPLNRHB), by=is.na(OPLRICHTING)] # 2440 / 34324 OPLNRHB without direction
### factorize to save memory #### 
EDUC[, OPLRICHTING := factor(OPLRICHTING)]           
# remove workers without OPLRICHTING 
EDUC <- EDUC[!is.na(OPLRICHTING),]
gc()

##################################################################################
#### Save result #####
saveRDS(EDUC[, !c("OPLNRHB")], paste0(map_output_here, "EDUC.rds"), compress=T)
