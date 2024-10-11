##################################################################################
### Firm-level data #####
# Start point are firm-years in POLIS. Script mergers other firm data sources; identifies automation; identifies acquisitions
# Output dataset starts in 2010
##################################################################################
### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table")
if (packageVersion("data.table")!="1.14.8") warning("Analysis ran in data.table version 1.14.8 - consider up- or downgrading")
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if (packageVersion("ggplot2")!="3.4.2") warning("Analysis ran in ggplot2 version 3.4.2 - consider up- or downgrading")
### Output dir #####
if (!dir.exists(paste0(map_data_analysis, "step3/"))) dir.create(paste0(map_data_analysis, "step3/"))
map_output_here <- paste0(map_data_analysis, "step3/")

### load data #####
cpi <- readRDS(paste0(map_data_analysis, "step1/cpi_year.rds")) # base year is 2021
POLIS_firms <- readRDS(paste0(map_data_analysis, "step2/POLIS_firms.rds")) # firms in POLIS, i.e. the complete firm-year panel
##################################################################################

##################################################################################
#### Subset to 2010 - 2021 ##### 
# take firm-years in POLIS and subset to time-frame. 
selected_firms <- POLIS_firms[year %in% 2010:2021, ]

##################################################################################
### add NACE, size class, firm age, sales from bedrijfsecondata ####
beidkenmerken <- readRDS(paste0(map_data_analysis, "step1/company/beidkenmerken.rds")) 
setnames(beidkenmerken, c("size"), c("firm_size_class"))
firmbirth <- readRDS(paste0(map_data_analysis, "step1/company/firmbirth.rds")) 
bedrijfsecondata <- readRDS(paste0(map_data_analysis, "step1/company/bedrijfsecondata.rds"))
bedrijfsecondata <- bedrijfsecondata[!is.na(sales_bedrijfsecondata),]

#### merge with selected_firms #####
selected_firms[, .(.N, firms=uniqueN(SBEID))]
selected_firms <- merge(selected_firms, beidkenmerken, by=c("SBEID", "year"), all.x=T) 
selected_firms <- merge(selected_firms, firmbirth, by=c("SBEID"), all.x=T)
selected_firms <- merge(selected_firms, bedrijfsecondata, by=c("SBEID", "year"), all.x=T)
selected_firms[, .(.N, firms=uniqueN(SBEID))]
rm(beidkenmerken)
rm(firmbirth)
rm(bedrijfsecondata)

#### manipulate #### 
# set unique NACE per firm --> most commonly observed; if draw keep latest classification
# also identify NACE_letter
tt <- selected_firms[, .(.N, year = max(year)), by=c("SBEID", "nace_sbi08")][, keep := N==max(N), by=SBEID][keep==T, keep := fifelse(.N==1 | (.N>1 & year==max(year)), T, F), by=SBEID][keep==T, !c("keep", "year", "N")]
# identify NACE letter (SBI1)
tt[,SBI2 := substr(nace_sbi08,1,2)]
tt[SBI2 %in% c("01", "02", "03"), NACE_letter := "A"]
tt[SBI2 %in% c("06", "08", "09"), NACE_letter := "B"]
tt[SBI2 >= 10 & SBI2 <= 33, NACE_letter := "C"]
tt[SBI2 == 35, NACE_letter := "D"]
tt[SBI2 >= 36 & SBI2 <= 39, NACE_letter := "E"]
tt[SBI2 >= 41 & SBI2 <= 43, NACE_letter := "F"]
tt[SBI2 >= 45 & SBI2 <= 47, NACE_letter := "G"]
tt[SBI2 >= 49 & SBI2 <= 53, NACE_letter := "H"]
tt[SBI2 >= 55 & SBI2 <= 56, NACE_letter := "I"]
tt[SBI2 >= 58 & SBI2 <= 63, NACE_letter := "J"]
tt[SBI2 >= 64 & SBI2 <= 66, NACE_letter := "K"]
tt[SBI2 == 68, NACE_letter := "L"]
tt[SBI2 >= 69 & SBI2 <= 75, NACE_letter := "M"]
tt[SBI2 >= 77 & SBI2 <= 82, NACE_letter := "N"]
tt[SBI2 == 84, NACE_letter := "O"]
tt[SBI2 == 85, NACE_letter := "P"]
tt[SBI2 >= 86 & SBI2 <= 88, NACE_letter := "Q"]
tt[SBI2 >= 90 & SBI2 <= 93, NACE_letter := "R"]
tt[SBI2 >= 94 & SBI2 <= 96, NACE_letter := "S"]
tt[SBI2 >= 97 & SBI2 <= 98, NACE_letter := "T"]
tt[SBI2 == 99, NACE_letter := "U"]
tt[, SBI2 := NULL]

## add back to selected_firms
selected_firms[, .(.N, firms=uniqueN(SBEID))]
selected_firms <- merge(selected_firms[, !c("nace_sbi08")], tt, by=c("SBEID"))
selected_firms[, .(.N, firms=uniqueN(SBEID))]
rm(tt)

##################################################################################
### add exports / imports overall ####
ihg <- readRDS(paste0(map_data_analysis, "step1/company/ihg.rds")) # 2007 - 2021
selected_firms[, .(.N, firms=uniqueN(SBEID))]
selected_firms <- merge(selected_firms, ihg, by=c("SBEID", "year"), all.x=T)
selected_firms[, .(.N, firms=uniqueN(SBEID))]
#### manipulate #####
vars <- names(ihg[, !c("SBEID", "year")])
selected_firms[, c(paste(vars)) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols=vars]
rm(ihg)

##################################################################################
#### Add MNE status ##### 
MNEs <- readRDS(paste0(map_data_analysis, "step1/company/MNEs_2010_2021.rds"))
## add to full firm panel (as PS has gaps)
MNEs_full <- merge(POLIS_firms[year %in% 2010:2021, c("SBEID", "year")], MNEs, by=c("SBEID", "year"), all.x=T)
## add OG_BEID
beog <- readRDS(paste0(map_data_analysis, "step1/company/beog.rds")) 
MNEs_full <- merge(MNEs_full, beog, by=c("SBEID", "year"), all.x=T)
rm(beog)

#### manipulate ##### 
MNEs_full[is.na(NED_MUL), NED_MUL := F]
MNEs_full[is.na(BUI_MUL), BUI_MUL := F]
MNEs_full[, MNE := fifelse(NED_MUL | BUI_MUL, T, F)]

# fix MNE vectors for gaps, if one year gap between two TRUE, impute TRUE
impute_MNE <- function(bui_mul){
  # check for changes in bui_mul vector
  gap <- diff(bui_mul, lag = 1)
  
  # can stop here if no changes
  replaced <- rep(FALSE, length(bui_mul))
  if (all(gap==0))   return(list(bui_mul, replaced))
  
  # set MNE == TRUE if year in between two MNE == TRUE observations is FALSE
  gap <- which(diff(gap, lag = 1)==2)+1
  
  # adjust MNE vector and add identifier
  bui_mul[gap] <- TRUE
  replaced[gap] <- TRUE
  
  # now check domestic firms
  ## check for changes in bui_mul vector
  gap <- diff(bui_mul, lag = 1)
  
  # set MNE == FALSE if year in between two MNE == FALSE observations is TRUE
  gap_DOM <- which(diff(gap, lag = 1)==-2)+1
  
  bui_mul[gap_DOM] <- FALSE
  replaced[gap_DOM] <- TRUE
  
  return(list(bui_mul, replaced))
}
setorderv(MNEs_full, cols=c("SBEID", "year"))
MNEs_full[SBEID %in% MNEs_full[MNE==T, unique(SBEID)], c("MNE", "replaced_MNE") := impute_MNE(MNE), by=SBEID]

# save full MNE assignment
saveRDS(MNEs_full, paste0(map_output_here, "MNEs.rds"), compress=T)

#### identify MNE status changes ##### 
setorderv(MNEs_full, c("SBEID", "year"))
MNEs_full[, MNE_data_type := fifelse(all(MNE), "always_MNE", fifelse(all(!MNE), "always_domestic", "MNE_DOM_change")),by=SBEID]

#### Add MNEs to selected_firms #####
selected_firms[, .(.N, firms=uniqueN(SBEID))]
selected_firms <- merge(selected_firms, MNEs_full[, !c("replaced_MNE")], by=c("SBEID", "year"))
selected_firms[, .(.N, firms=uniqueN(SBEID))]
rm(MNEs_full)

##################################################################################
#### Subsetting (#1) #####
##### remove firms with missing nace sector #### 
##### Drop small sectors that aren't in PS as it is sector-based (important for PS auto) ##### 
selected_firms <- selected_firms[NACE_letter %in% c("C", "F", "G", "H", "I", "J", "M", "N"), ]
selected_firms[, .(.N, firms=uniqueN(SBEID))]

#### focus on MNEs and domestic firms that do not change status #####
selected_firms <- selected_firms[MNE_data_type %in% c("always_MNE", "always_domestic"), ]
selected_firms[, .(.N, firms=uniqueN(SBEID))]

#### focus on firms with logical sales data ####
selected_firms[sales_bedrijfsecondata<= 0, sales_bedrijfsecondata := NA_real_]
selected_firms <- selected_firms[!is.na(sales_bedrijfsecondata),]

#### well observed firms according to BDK ####
# load BDK
bdk <- readRDS(paste0(map_data_analysis, "step1/company/bdk.rds"))
# from BDK, just keep BEID / longBEID translation, add identifier whether firm in PS
longbeids <- unique(bdk[jaar %in% 2010:2021, .(SBEID=beid, begin_beid = begin_beid, year=jaar)])
longbeids <- merge(longbeids, selected_firms[, .(SBEID, year, in_ps=T)], by=c("SBEID", "year"), all.x=T)
longbeids[is.na(in_ps), in_ps := F]
# find begin_beid that are associated with > 1 SBEIDs in a given year. Only need to do this for firm-years in selected_firms
tt <- longbeids[, keep:= any(in_ps), by=c("begin_beid", "year")][keep==T, .(SBEIDs=.N), by=c("begin_beid", "year")]
# select all SBEIDs that are always associated with a begin_beid with only 1 SBEID (i.e. well-defined SBEIDs)
tt <- longbeids[begin_beid %in% tt[, all(SBEIDs==1), by=begin_beid][V1==T, begin_beid], unique(SBEID)]

# subset selected_firms to well-defined SBEIDs
selected_firms[, single_ID_firm := SBEID %in% tt]
selected_firms[, .(.N, firms=uniqueN(SBEID))]
selected_firms <- selected_firms[single_ID_firm==T, !c("single_ID_firm")]
selected_firms[, .(.N, firms=uniqueN(SBEID))]
rm(tt, longbeids, bdk)

#### keep only cont. observed firms ####
# keep only continuously observed firms 
selected_firms[order(SBEID,year), keep := all(diff(year)==1), by=SBEID]
selected_firms[, .(.N, firms=uniqueN(SBEID)), by=keep]
selected_firms <- selected_firms[keep==T, !c("keep")]

##################################################################################
### Identify automation events #####

#### Productiestatistiek (Total and automation cost) for 2000 - 2021 ####
ps_costs <- readRDS(paste0(map_data_analysis, "input/PS_costs.rds"))
setnames(ps_costs, c("beid", "BEDRLST310000", "BEDRLST348400", "PERSONS110000"), c("SBEID", "total_cost", "automation_cost", "workers_fte"))
ps_costs[, year := as.integer(year)]
ps_costs <- ps_costs[, c("SBEID", "year", "workers_fte", "total_cost", "automation_cost")]

# need to observe all costs in same row and remove potential duplicates
ps_costs <- ps_costs[!is.na(automation_cost) & !is.na(total_cost), ] # need to observe both costs and workers
ps_costs[, .N, by=.(SBEID, year)][N>1, ] # some duplicates
ps_costs <- unique(ps_costs)
ps_costs[, .N, by=.(SBEID, year)][N>1, ] # duplicates not removed --> They occur with is.na(workers_fte)
ps_costs[, obs := .N, by=.(SBEID, year)]
ps_costs[obs == 1, keep := TRUE]
ps_costs[obs > 1, keep := !is.na(workers_fte)]
ps_costs <- ps_costs[keep==T, !c("keep", "obs")]
ps_costs[, .N, by=.(SBEID, year)][N>1, ] # duplicates not removed --> They occur with is.na(workers_fte)
ps_costs[, .(.N, firms=uniqueN(SBEID))]

# remove rows with automation_cost > total_cost, or where total_cost or automation_cost is <= 0 (i.e. costs not well observed)
ps_costs[, .(.N, firms=uniqueN(SBEID))]
ps_costs <- ps_costs[total_cost >= 0 & automation_cost >=0 & automation_cost <= total_cost,]
ps_costs[, .(.N, firms=uniqueN(SBEID))]

## For years > 2010 keep only fully observed firms
# match with firms
firms <- merge(selected_firms[, c("SBEID", "year", "MNE", "workers_fte")], ps_costs[, !c("workers_fte")], by=c("SBEID", "year"), all.x=T)
firms[, remove := all(is.na(automation_cost)), by=SBEID]
firms <- firms[remove==F, !c("remove")]

## keep firms with at least 8 years of observed data --> time frame needed for event study
firms[, keep := .N>=8, by=SBEID]
firms[, .(.N, firms=uniqueN(SBEID)), by=keep]
firms <- firms[keep==T, !c("keep")]

## keep firms with at least 3 years of automation cost 
firms[, keep := sum(!is.na(automation_cost))>=3, by=SBEID]
firms[, .(.N, firms=uniqueN(SBEID)), by=keep]
firms <- firms[keep==T, !c("keep")]

# What is the distribution of gaps in automation costs?
tt <- firms[order(SBEID, year), .(ceiling((sum(!is.na(automation_cost))/.N)^(-1))), by=.(SBEID, MNE)][, .(SBEID, MNE, V1=fifelse(V1==Inf, 0, V1))][, .N, by=.(V1, MNE)][order(V1), .(V1, cumsum(N/sum(N)*100)), by=MNE]
ggplot(data = tt, aes(x=V1, y=V2, color=MNE)) + geom_point() + 
  xlab("Observed every x year") + ylab("Cumulative distribution")

# keep firms that are observed at least every third year, on average
firms[order(SBEID, year), keep := {t <- (sum(!is.na(automation_cost))/.N)^(-1)
                                   t <- fifelse(t==Inf, 1, t)
                                   t <= 3}
      , by=.(SBEID)]
firms[, .(.N, firms=uniqueN(SBEID), MNEs=uniqueN(SBEID[MNE]), Domestic = uniqueN(SBEID[!MNE])), by=keep]
firms <- firms[keep==T, !c("keep")]
firms <- firms[!is.na(automation_cost), c("SBEID", "year", "workers_fte", "total_cost", "automation_cost")]
 
# add automation cost for < 2010
ps_costs <- ps_costs[SBEID %in% firms[, unique(SBEID)] & year < 2010, ]
ps_costs <- rbindlist(list(ps_costs, firms))
rm(firms)

# calculate real value of costs
ps_costs <- merge(ps_costs, cpi, by="year")
ps_costs[, c("real_total_cost", "real_automation_cost") := .(total_cost*factor, automation_cost*factor)]

#### Identify automation events etc. #####
# see Bessen et al 2023: Automation cost spike occurs when real automation costs relative to real total operating costs (excluding automation costs) averaged across all years t, are at least thrice the average firm-level cost share
ps_costs[, auto_cost_share := real_automation_cost / mean((real_total_cost - real_automation_cost)), by="SBEID"] # automation cost / average cost
#ps_costs[, auto_cost_share := real_automation_cost / (real_total_cost - real_automation_cost), by="SBEID"]
ps_costs[, compare := (sum(auto_cost_share)-auto_cost_share)/(.N-1), by="SBEID"] # average automation cost share excluding current row
ps_costs[, ratio := fifelse(auto_cost_share==0, 0, fifelse(compare == 0, 999, auto_cost_share/compare))] # take care of 0's --> where auto_cost_share==0 & compare==0, ratio == Inf --> set to 0. For compare == 0 but auto_cost_share > 0, set to 999
# spike if thrice the average share of other years (in a year with automation cost). 
ps_costs[, auto_spike_ps := ratio >= 3] 

ps_costs[, sum(auto_spike_ps)]

# see Bessen scripts: drop spikes if spending per (average) worker is less than 25th percentile of non-zero spendings
#ps_costs[, auto_spike_ps := fifelse(auto_cost_share < ps_costs[auto_cost_share>0, quantile(auto_cost_share, p=0.25)], F, auto_spike_ps)]

ps_costs[, real_automation_cost_pw := real_automation_cost/mean(workers_fte,na.rm=T), by=SBEID]

#ps_costs[, auto_spike_ps := fifelse(real_automation_cost_pw < quantile(real_automation_cost_pw[real_automation_cost_pw>0], p=0.25), F, auto_spike_ps)]

# see Bessen scripts: drop spikes if less than 25th percentile of non-zero shares
ps_costs[, auto_spike_ps := fifelse(auto_cost_share < quantile(auto_cost_share[auto_cost_share>0], p=0.25), F, auto_spike_ps)]

ps_costs[, sum(auto_spike_ps)]

## drop spike if absolute level of spending is minimum observed
ps_costs[, auto_spike_ps := fifelse(round(real_automation_cost,5)<=round(min(real_automation_cost),5), F, auto_spike_ps), by=SBEID]

ps_costs[, sum(auto_spike_ps)]

## automation event is first spike of SBEID
ps_costs[, auto_adopter_ps := SBEID %in% ps_costs[, any(auto_spike_ps), by=SBEID][V1==T, SBEID]]
ps_costs[auto_adopter_ps==T, auto_change_year_ps := min(year[auto_spike_ps]), by=SBEID]
# or largest spike
#ps_costs[auto_adopter_ps==T, auto_change_year_ps := min(year[ratio==max(ratio)]), by=SBEID]

##################################################################################
#### Subsetting (#2 -- automation defined) #####
# subset selected_firms to firms captured in PS_auto
selected_firms[, keep := SBEID %in% ps_costs[, unique(SBEID)]]
selected_firms[, .(.N, firms=uniqueN(SBEID)), by=keep]
selected_firms <- selected_firms[keep==T, !c("keep")]


##################################################################################
### Identify PS_auto categories and add automation data #####
# add fixed variables to selected_firms
selected_firms[, .(.N, firms=uniqueN(SBEID))]
selected_firms <- merge(selected_firms, unique(ps_costs[, c("SBEID", "auto_adopter_ps", "auto_change_year_ps")]),
                        by="SBEID")
selected_firms[, .(.N, firms=uniqueN(SBEID))]

# add time-varying variables, including missings
selected_firms <- merge(selected_firms, ps_costs[, c("SBEID", "year", "auto_spike_ps", "auto_cost_share", "automation_cost", "total_cost", "real_automation_cost_pw")],
                        by=c("SBEID", "year"), all.x=T)
rm(ps_costs)

#### define types #####

# firm is never observed as automised --> never automised
selected_firms[, auto_ps_data_type := fifelse(all(is.na(auto_change_year_ps)), "never_auto", NA_character_), by=SBEID]
# and if all available years lie before auto event --> no information about post-automisation outcomes
selected_firms[is.na(auto_ps_data_type) & !is.na(auto_change_year_ps), auto_ps_data_type := fifelse(all(year<auto_change_year_ps), "never_auto", NA_character_), by=SBEID]
# firm is always automised if all observations lie after auto event 
selected_firms[is.na(auto_ps_data_type) & !is.na(auto_change_year_ps), auto_ps_data_type := fifelse(all(year>auto_change_year_ps[!is.na(auto_change_year_ps)][1]), "always_auto", NA_character_), by=SBEID]
#selected_firms[!is.na(auto_changed_ps) & auto_ps_data_type == "always_auto", c("auto_changed_ps", "auto_adopter_ps", "auto_event_ps", "auto_spike_ps") := .(T, T, F, F)] # with subsetting this changed
selected_firms[is.na(auto_ps_data_type), auto_ps_data_type := "auto_adaptor"]
# expand across all datapoints
#selected_firms[, c("auto_change_year_ps", "auto_ps_data_type") := lapply(.SD, function(x) x[!is.na(x)][1]), by=SBEID, .SDcols=c("auto_change_year_ps", "auto_ps_data_type")]

selected_firms[, uniqueN(SBEID), by=auto_ps_data_type]
# How many firms are in each category?
selected_firms[, .(MNE_data_type=MNE_data_type[1], auto_ps_data_type=auto_ps_data_type[1]), by=SBEID][, table(auto_ps_data_type, MNE_data_type)]


##################################################################################
### subset firms ### 
# remove firms that automate before 2010 
selected_firms[, .(.N, firms=uniqueN(SBEID))]
selected_firms <- selected_firms[auto_ps_data_type != "always_auto"]
selected_firms[, .(.N, firms=uniqueN(SBEID))]

##################################################################################
### stats of selected firms #####
# number firms and their employment
selected_firms[, uniqueN(SBEID)]
# employment per year
selected_firms[, sum(workers_fte), by=year]
selected_firms[, sum(workers_fte), by=year][, mean(V1)]
# share of total employment 
tt <- merge(selected_firms[, .(selected = sum(workers_fte)), by=year], POLIS_firms[, .(all = sum(workers_fte)), by=year], by="year")
tt[, share := selected/all*100]
tt[, mean(share)]

##################################################################################
### save result #### 
saveRDS(selected_firms, paste0(map_output_here, "selected_firms_2010_2021.rds"), compress=T)
