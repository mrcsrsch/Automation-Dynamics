##################################################################################
### This script runs the analysis, including all additional checks #####
# It sources other scripts below. They are named as 04_SOURCE_
##################################################################################
### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table")
if (packageVersion("data.table")!="1.14.8") warning("Analysis ran in data.table version 1.14.8 - consider up- or downgrading")

# need to install newer fixest version to use ggfixest
if (isNamespaceLoaded("fixest")){
  if (packageVersion("fixest")!="0.11.2"){
    detach("package:fixest", unload=T)
  }
}

if (!require("dreamerr")) install.packages("dreamerr"); library("dreamerr")

# For the analysis we want a specific fixest version, which is not available on CBS CRAN mirror right now (March 2024). So we install it from source.
install.packages(paste0(map_data_analysis, "packages/fixest_0.11.2.tar.gz"), lib = paste0(map_data_analysis,"packages/library/"))
library(fixest, lib.loc=paste0(map_data_analysis,"packages/library/"))
# ggfixest dependency
if (!require("ggh4x")){
  install.packages(paste0(map_data_analysis, "packages/ggh4x_0.2.7.tar.gz"), lib = paste0(map_data_analysis,"packages/library/"))
}

if (!require("marginaleffects")) install.packages("marginaleffects"); library("marginaleffects")

# install ggfixest for easy coefficient plots
install.packages(paste0(map_data_analysis, "packages/ggfixest_0.1.0.tar.gz"), lib = paste0(map_data_analysis,"packages/library/"))
library(ggfixest, lib.loc=paste0(map_data_analysis,"packages/library/"))

### source functions and styles #### 
source(paste0(map_scripts, "00_functions_analysis.R"), echo=F) # functions creating analyses 
source(paste0(map_scripts, "00_fixest_styles.R"), echo=F) # styles of fixest output tables

### Output dir #####
if (!dir.exists(paste0(map_output, "main_estimates/"))) dir.create(paste0(map_output, "main_estimates/"))
if (!dir.exists(paste0(map_output, "presentation/"))) dir.create(paste0(map_output, "presentation/"))

map_output_here <- paste0(map_output, "main_estimates/")
if (!dir.exists(paste0(map_output_here, "heterogeneity/"))) dir.create(paste0(map_output_here, "heterogeneity/"))
map_output_here_main <- map_output_here
map_output_here_hetero <-  paste0(map_output_here, "heterogeneity/")
map_output_presentation <- paste0(map_output, "presentation/")


### load data #####
# firm/worker data
selected_firms <- readRDS(paste0(map_data_analysis, "step3/selected_firms_2010_2021.rds")) # selected firms panel

POLIS_worker <- readRDS(paste0(map_data_analysis, "step3/POLIS_worker.rds")) # full worker panel (at all SBEIDs)

# additional vars
cpi <- readRDS(paste0(map_data_analysis, "step1/cpi_year.rds")) # base year is 2021

##################################################################################

### prepare worker-level ####

### clean data ###
#POLIS_worker[SBEID==11517662 , mean(lrhwage), by=year][order(year),] # firm has sudden drop in hourly wage
#POLIS_worker[SBEID==11517662 , mean(SOVERWERKUREN + SREGULIEREUREN), by=year][order(year),] # seems to be because of misreported hours
# --> remove firm
selected_firms <- selected_firms[SBEID!=11517662,]

## set educ level for missing
POLIS_worker[is.na(educ_level), educ_level := -1]

# identify fixed term contracts
POLIS_worker[, fixed_term := SCONTRACTSOORT!=1]

# create dummy for part-time
POLIS_worker[, part_time := deeltijdfactor < 0.8]

### add educ  subjects ###
POLIS_worker[, st_dir := NULL]
POLIS_worker[, st_dir := as.character(OPLRICHTING)]

POLIS_worker[is.na(st_dir), st_dir := "onbekend"]
POLIS_worker[st_dir=="Algemeen", st_dir := "onbekend"]


POLIS_worker[st_dir %in% c("Informatica",
                           "Wiskunde, natuurwetenschappen", "Techniek, industrie en bouwkunde"), st_dir := "Analytical and Technical"]
POLIS_worker[st_dir %in% c("Recht, administratie, handel en zakelijke dienstverlening"), st_dir := "Administrative"]
POLIS_worker[st_dir %in% c("Dienstverlening", "Onderwijs",
                           "Gezondheidszorg en welzijn", 
                           "Landbouw, diergeneeskunde en -verzorging"), st_dir := "Service"]
POLIS_worker[st_dir %in% c("Journalistiek, gedrag en maatschappij", "Vormgeving, kunst, talen en geschiedenis"), st_dir := "Creative"]


inc <- c("Analytical and Technical", "Administrative", "Service", "Creative")
# set rest to missing
POLIS_worker[!(st_dir %in% inc), st_dir := "onbekend"]

##################################################################################
### prepare firm-level ####

# subset selected_firms to firm-years in POLIS_worker
selected_firms[, .(.N, uniqueN(SBEID), uniqueN(SBEID[auto_adopter_ps]))]
selected_firms <- merge(selected_firms, unique(POLIS_worker[, c("SBEID", "year")]), by=c("SBEID", "year"))
selected_firms[, .(.N, uniqueN(SBEID), uniqueN(SBEID[auto_adopter_ps]))]

# add cpi factor to selected_firms
selected_firms <- merge(selected_firms, cpi, by="year")
rm(cpi)

# create event time variable 
selected_firms[, auto_event_time := year-auto_change_year_ps]

# 2-digit industry ID
selected_firms[, nace_2digit := substr(nace_sbi08,1,2)]

# calculate real values of imports and exports 
selected_firms[, rimport := import*factor]
selected_firms[, rexport := export*factor]
# sales real
selected_firms[, rsales := sales_bedrijfsecondata*factor]

# create importer / exporter dummy
selected_firms[, importer := mean(rimport) > 10, by=SBEID]
selected_firms[, exporter := mean(rexport) > 10, by=SBEID]


# log sales and lag 
selected_firms[, lrsales := log(rsales)]
selected_firms[order(SBEID,year), paste0("lrsales_", c(1,2)) := shift(lrsales, n=c(1,2), type="lag"), by=SBEID]

# logs
selected_firms[, lrexport := log(rexport+1)]
selected_firms[, lrimport := log(rimport+1)]


# create log empl
selected_firms[,log_empl := log(workers_fte)]


#### Define industries ###
# knowledge intensive services (KIS)
KIS <- c(50, 51, 58:63, 64:66, 69:75, 78, 80, 84:93)
# less knowledge intensive services (LKIS)
LKIS <- c(45:47, 49, 52:53, 55, 56, 68, 77, 79, 81, 82, 94:96, 97:99)
# high technology manufacturing
HT <- c(21, 26)
# medium-high-technology
MHT <- c(20, 27:30)
# medium-low-technology
MLT <- c(19, 22:25, 33)
# low technology 
LT <- c(10:18, 31:32)
# add to treated_firms
selected_firms[nace_2digit %in% KIS, industry_class := "knowledge-intensive services"]
selected_firms[nace_2digit %in% LKIS, industry_class := "less knowledge-intensive services"]
selected_firms[nace_2digit %in% HT, industry_class := "high-tech manufacturing"]
selected_firms[nace_2digit %in% MHT, industry_class := "high-tech manufacturing"]
selected_firms[nace_2digit %in% MLT, industry_class := "low-tech manufacturing"]
selected_firms[nace_2digit %in% LT, industry_class := "low-tech manufacturing"]
selected_firms[is.na(industry_class), industry_class := "other industries"]

# broader definition
selected_firms[nace_2digit %in% c(KIS, LKIS), industry_class_2 := "services"]
selected_firms[nace_2digit %in% c(HT, MHT, MLT, LT), industry_class_2 := "manufacturing"]
selected_firms[industry_class == "other industries", industry_class_2 := "other industries"]

rm(KIS, LKIS, HT, MHT, MLT, LT)

##################################################################################
### Extract treated firms at firm-level #### 
# extract treated firms
treated_firms <- selected_firms[auto_ps_data_type=="auto_adaptor" & SBEID !=10055487, ] 

##################################################################################
### Create analysis panels ####
##################################################################################
# Note: workers are unique at .(workerID, cohort) level!
ref_event_time <- -1

event_time_var <- "auto_event_time"
event_time <- -3:4
matching_event_time <- -1 
stayer_event_time <- min(event_time):matching_event_time 
firm_exact_matching_vars <- c("nace_2digit")  
firm_cem_vars <- "log_empl" 
probs <- unique(c(seq(0,1,0.1), 0.99, 1))
probs <- probs[order(probs)]
quantile_probs_firm_cem <- probs
worker_exact_matching_vars <- NULL 
worker_cem_vars_1 <- c("lrhwage_2", "lrhwage_1", "lrhwage") # Note that when including non_stayers, there is a group called "NA"
probs <- unique(c(seq(0,1,0.1), 0.99, 1))
probs <- probs[order(probs)]
quantile_probs_worker_cem_1 <- probs
worker_cem_vars_2 <- NULL 
quantile_probs_worker_cem_2 <- NULL  

#### create worker panel ######
result <- create.worker.event.data(firm_panel = treated_firms, 
                                   POLIS_panel = POLIS_worker,  =
                                   event_time_var =  event_time_var, event_time = event_time, 
                                   matching_event_time = matching_event_time, stayer_event_time = stayer_event_time, 
                                   firm_observed_after = T, # regulates whether firm should be observed in year after event panel
                                   firm_type_var = "MNE",
                                   firm_exact_matching_vars = firm_exact_matching_vars, 
                                   firm_cem_vars=firm_cem_vars, 
                                   quantile_probs_firm_cem=quantile_probs_firm_cem,
                                   worker_exact_matching_vars = worker_exact_matching_vars,  
                                   worker_cem_vars_1 = worker_cem_vars_1,
                                   demean_cem_vars = TRUE,
                                   quantile_probs_worker_cem_1 = quantile_probs_worker_cem_1, 
                                   worker_cem_vars_2 = worker_cem_vars_2, 
                                   quantile_probs_worker_cem_2 = quantile_probs_worker_cem_2, 
                                   const_vars_POLIS_panel =  c("st_dir", "educ_level", "SCONTRACTSOORT", "part_time", "manager"), #,
                                   const_vars_firm_panel = c("MNE", "NACE_letter", "nace_2digit", "exporter", "importer", "industry_class", "industry_class_2")) #  )

worker_panel <- copy(result$worker_panel) # main estimation panel 
potential_treated_control <- copy(result$potential_treated_control) # all firm IDs that fall in event_time
rm(result)

# unique workers
worker_panel[, .(obs= .N, workers= uniqueN(workerID), firms= uniqueN(SBEID))]
worker_panel[treated==T & get(event_time_var)==matching_event_time, .(obs= .N, workers= uniqueN(workerID), firms= uniqueN(SBEID)), by=MNE_match]

# unique worker-cohort
worker_panel[, .(obs= .N, workers= uniqueN(workerID), firms= uniqueN(SBEID)), by=cohort][, .(obs=sum(obs), workers=sum(workers), firms=sum(firms))]

### prepare panel data for analysis ####
worker_panel[treated==F, paste0(event_time_var, "_treated") := ref_event_time]
worker_panel[, MNE_group := fifelse(MNE_match, "MNE", "Domestic")]

#### Sample selection dummies #####
##### Identify stayer dummy vs their matched controls #### 
# new stayer dummy that adjusts control group (vs. matched control workers)
worker_panel[treated==T, stayer_DiD := same_firm]
worker_panel[treated==T & get(event_time_var)<matching_event_time, stayer_DiD := T] # set stayer_DiD=T for earlier periods
worker_panel[treated==F, stayer_DiD := T] # with TRUE control workers are allowed to move freely
# adjust for being a stayer from matching moment until at least s=s 
# adjust control group for being observed in employment during same period
s <- worker_panel[, max(get(event_time_var))]
worker_panel[, stayer_DiD := sum(stayer_DiD[get(event_time_var) %in% matching_event_time:s])==length(matching_event_time:s), by=.(workerID, cohort)]
# remove cluster-years without matched controls
worker_panel[, keep := any(stayer_DiD & treated) & any(stayer_DiD & !treated),.(clusterID_worker, year)]
worker_panel[, c("stayer_DiD", "keep") := .(fifelse(keep==T, stayer_DiD, F), NULL)]

# let's redefine stayers by also being in the firm in matching_event_time+max(get(event_time_var))+1 (hence year after panel)
## get all stayers in their last year
tt <- worker_panel[treated==T & get(event_time_var)==max(event_time) & stayer_DiD==T, c("workerID", "cohort", "year", "SBEID")]
tt[, year2 := year]
tt[, year := year+1]
## find matches in POLIS_worker
POLIS_worker[, in_polis :=T]
tt <- merge(POLIS_worker[, c("workerID", "SBEID", "year", "in_polis")], tt, by=c("workerID", "SBEID", "year"), all.y=T)
POLIS_worker[, in_polis := NULL]
tt[is.na(in_polis), in_polis:=F]
tt[, year := year2]
tt[, year2 := NULL]
worker_panel <- merge(worker_panel, tt, by=c("workerID", "cohort", "SBEID", "year"), all.x=T)
worker_panel[stayer_DiD==T & treated==T & get(event_time_var)==max(event_time), unique(in_polis)]

# worker_panel[, stayer_DiD_2 := stayer_DiD]
worker_panel[!is.na(in_polis), stayer_DiD := stayer_DiD & in_polis]

# remove cluster-years without matched controls
worker_panel[, keep := any(stayer_DiD & treated) & any(stayer_DiD & !treated),.(clusterID_worker, year)]
worker_panel[, c("stayer_DiD", "keep") := .(fifelse(keep==T, stayer_DiD, F), NULL)]

##################################################################################
#### create balanced worker panel based on firm-worker panel ####
worker_panel_balanced <- create.balanced.panel(matched_worker_panel = worker_panel, 
                                               event_time_var = event_time_var, event_time=event_time, 
                                               expand_vars_const = c("female", "clusterID_worker"), matching_event_time = matching_event_time,
                                               firm_type_var = NULL)

#### adjust worker_panel_balanced #####
# need to set control workers to reference year
worker_panel_balanced[treated==F, paste0(event_time_var, "_treated") := ref_event_time]

# create grouping names for plots
worker_panel_balanced[, MNE_group := fifelse(MNE_match, "MNE", "Domestic")]

##################################################################################
### create (matched) firm panel based on firm-worker panel ####
# extract treated and control firms. These are stable at auto_event_time == matching_event_time
tt <- worker_panel[get(event_time_var)==matching_event_time, .(treated = treated[1], SBEID_cluster = SBEID_cluster[1], clusterID_firm = clusterID_firm[1], MNE_group = MNE_group[1]), by=.(SBEID, cohort)]
## add variables from treated_firms and create duplicates for control firms
## Make sure to remove event_time_var since that is redefined for control firms
firm_panel <- merge(treated_firms[, !c(event_time_var), with=F], tt, by=c("SBEID"), allow.cartesian = T)
rm(tt)
## create vars and subset panel to event time
firm_panel[, paste0(event_time_var) := year-cohort]
firm_panel[, paste0(event_time_var, "_treated") := fifelse(treated, get(event_time_var), ref_event_time)]
firm_panel <- firm_panel[get(event_time_var) %in% event_time, ]

##################################################################################
### add count of other spikes ###
worker_panel_balanced[, auto_spike_ps := NULL]
worker_panel_balanced <- merge(worker_panel_balanced, firm_panel[treated==T, c("SBEID", "year", "auto_spike_ps")], by=c("SBEID", "year"), all.x=T)
worker_panel_balanced[is.na(auto_spike_ps), auto_spike_ps :=F]
worker_panel_balanced[, auto_spike_ps := fifelse(!treated, FALSE, auto_spike_ps)]
worker_panel_balanced[, auto_spike_ps := fifelse(treated & same_firm, auto_spike_ps, FALSE)]
worker_panel_balanced[order(workerID, cohort, year), spikes := cumsum(auto_spike_ps), by=c("workerID", "cohort")]
worker_panel_balanced[, spikes := fifelse(spikes==0, 0, spikes-1)]
# calculate maximum spikes experienced by worker
worker_panel_balanced[, max_spikes := max(spikes), by=.(workerID,cohort)]


worker_panel[, auto_spike_ps := NULL]
worker_panel <- merge(worker_panel, firm_panel[treated==T, c("SBEID", "year", "auto_spike_ps")], by=c("SBEID", "year"), all.x=T)
worker_panel[is.na(auto_spike_ps), auto_spike_ps :=F]
worker_panel[, auto_spike_ps := fifelse(!treated, FALSE, auto_spike_ps)]
worker_panel[, auto_spike_ps := fifelse(treated & same_firm, auto_spike_ps, FALSE)]
worker_panel[order(workerID, cohort, year), spikes := cumsum(auto_spike_ps), by=c("workerID", "cohort")]
worker_panel[, spikes := fifelse(spikes==0, 0, spikes-1)]
worker_panel[, max_spikes := max(spikes), by=.(workerID,cohort)]

##################################################################################
### Matched sample descriptive stats ####
##################################################################################

source(paste0(map_scripts, "04_SOURCE_matching_summary_stats.R"), echo=F) # source script that calculates matched sample summary stats

##################################################################################
### Main Regressions ####
##################################################################################
### Stayer probability ####
#### formulas ####
fixefs_stayer <- paste0(event_time_var, "^MNE_group^max_spikes +", event_time_var, "^clusterID_worker + workerID^clusterID_worker")
controls_stayer <-"+ i(SCONTRACTSOORT_match, age, ref=9) + i(SCONTRACTSOORT_match, age^2, ref=9)"

#### regressions ######
# redefine dependent variable
worker_panel_balanced[, leave := !same_firm]

stayer_ana <- plots.estimates(dt=worker_panel_balanced[], y = "leave", event_time_var = event_time_var, MNE_grouping_var = "MNE_group",
                              ref_event_time = ref_event_time, 
                              controls = controls_stayer, fes = fixefs_stayer,
                              plot_title = "", ci_level = 0.95,
                              create.OLS.weights = T, weight_group = "clusterID_worker", 
                              cluster_vars = c("SBEID_cluster"),
                              #dummy_scaling_var = "log_cost_event",
                              diff_regs = T, diff_ref = "Domestic")
etable(stayer_ana$regs)


stayer_ana$plots

# difference regressions
etable(stayer_ana$diff_regs)


# Probabilities in the control group
worker_panel_balanced[treated==F, .N, by=get(event_time_var)] # panel is perfectly balanced
worker_panel_balanced[treated==F, mean(leave), by=get(event_time_var)]

# using OLS weights
worker_panel_balanced[treated==F, weighted.mean(leave, weight_OLS), by=get(event_time_var)]

# for MNE controls
worker_panel_balanced[, MNE_control := any(treated & MNE_group == "MNE"), by=clusterID_worker]
worker_panel_balanced[MNE_control==T & treated==F, weighted.mean(leave, weight_OLS), by=get(event_time_var)]
# for domestic controls
worker_panel_balanced[, domestic_control := any(treated & MNE_group == "Domestic"), by=clusterID_worker]
worker_panel_balanced[domestic_control==T & treated==F, weighted.mean(leave, weight_OLS), by=get(event_time_var)]


################################################################################
#### Wage of stayers ####
# formula
fixefs_wage <- paste0(event_time_var, "^MNE_group^max_spikes +", event_time_var, "^clusterID_worker + workerID^clusterID_worker")
controls_wage <- "+ i(SCONTRACTSOORT_match, age, ref=9) + i(SCONTRACTSOORT_match, age^2, ref=9)"

#### Regressions #####
##### wage: stayers vs. their matched controls ##### 
wage_regs <- plots.estimates(dt=worker_panel[stayer_DiD==T, ], y = "lrhwage", event_time_var = event_time_var, MNE_grouping_var = "MNE_group",
                             ref_event_time = ref_event_time, 
                             controls = controls_wage, fes = fixefs_wage,
                             plot_title = "", ci_level = 0.95,
                             create.OLS.weights = T, weight_group = "clusterID_worker",
                             cluster_vars = "SBEID_cluster",
                             #dummy_scaling_var = "log_cost_event",
                             diff_regs = T, diff_ref = "Domestic")
etable(wage_regs$regs)
wage_regs$plots


etable(wage_regs$diff_regs)

# Wages in the control group
worker_panel[stayer_DiD==T & treated==F, mean(lrhwage), by=get(event_time_var)][, .(get, V1-V1[get==ref_event_time])]

# using OLS weights
worker_panel[stayer_DiD==T & treated==F, weighted.mean(lrhwage), by=get(event_time_var)][, .(get, V1-V1[get==ref_event_time])]

# for MNE controls
worker_panel[, MNE_control := any(treated & MNE_group == "MNE"), by=clusterID_worker]
worker_panel[MNE_control ==T & stayer_DiD==T & treated==F, weighted.mean(lrhwage), by=get(event_time_var)][, .(get, V1-V1[get==ref_event_time])]
# for domestic controls
worker_panel[, domestic_control := any(treated & MNE_group == "Domestic"), by=clusterID_worker]
worker_panel[domestic_control ==T & stayer_DiD==T & treated==F, weighted.mean(lrhwage), by=get(event_time_var)][, .(get, V1-V1[get==ref_event_time])]

##### Save main outputs ######
## Plots 
### Stayer ### 
ggsave(filename = paste0(map_output_here_main, "plot_stayer_main.pdf"), plot=stayer_ana$plots, 
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))

# For presentation
ggsave(filename = paste0(map_output_presentation, "plot_stayer_main.pdf"), plot=stayer_ana$plots, 
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(6*9/16, "inches"))

### Wage ###
ggsave(filename = paste0(map_output_here_main, "plot_wage_stayer_main.pdf"), plot=wage_regs$plots, 
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))

# For presentation
ggsave(filename = paste0(map_output_presentation, "plot_wage_stayer_main.pdf"), plot=wage_regs$plots, 
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(6*9/16, "inches"))

### difference regressions ###
etable(list(stayer_ana$diff_regs, wage_regs$diff_regs),
       title = "The effect of automation on workers in MNEs vs. domestic firms.",
       label = "tab:main_estimates",
       placement = "H",
       tex = T,
       fontsize = "footnotesize",
       file =  paste0(map_output_here_main, "table_main_diff.tex"),
       replace = T)

################################################################################
#### Heterogeneity ####
################################################################################


###### NACE Industry #####
# Overview NACE industries (NACE_letter_match)
#"C", "Manufacturing", 
#"G", "Wholesale and Retail Trade",
#"H", "Transportation and Storage",
#"F", "Construction",
#"J", "Information and Communication",
#"M", "Prof., Scientific and Technical Activities",
#"N", "Administrative and Support Activities"
#"I", "Accomodation and Food Service", 
# --> MNEs are high in Manufacturing and Wholesale and Retail Trade (ca. 70%)


# Define broad industry classes
worker_panel_balanced[, industry_match := fifelse(NACE_letter_match %in% c("C", "F", "H"), "Manufacturing", "Services")]
worker_panel[, industry_match := fifelse(NACE_letter_match %in% c("C", "F", "H"), "Manufacturing", "Services")]

# Save 3 plots: separations in all sectors and MNE/DOM + wages split up by sector
worker_panel_balanced[, group := "both"]
worker_panel_balanced[, industry_match_split := fifelse(MNE_match, paste0("MNE_", industry_match), paste0("DOM_", industry_match))]

# Note: you created this plot by manually adjusting the plot call within the function (see # commands)
subset_stayer <- plots.estimates.hetero(dt=worker_panel_balanced, y = "leave", event_time_var = event_time_var, 
                                        grouping_var = "group",
                                        hetero_var = "industry_match_split",
                                        exclude_levels = NULL,
                                        levels_dict = c("MNE (Service sec.)" = "MNE_Services",
                                                        "MNE (Manufacturing sec.)" = "MNE_Manufacturing",
                                                        "Domestic (Service sec.)" = "DOM_Services",
                                                        "Domestic (Manufacturing sec.)" = "DOM_Manufacturing"),
                                        ref_event_time = ref_event_time, 
                                        controls = controls_stayer, fes = fixefs_stayer,
                                        add_fe_split = F,
                                        plot_title = "", ci_level = 0.95,
                                        create.OLS.weights = T, weight_group = "clusterID_worker", 
                                        cluster_vars = "SBEID_cluster",
                                        #dummy_scaling_var = "log_cost_event",
                                        diff_regs = T, diff_ref = "Domestic")

subset_stayer$plts$both


# wage
subset_wage <- plots.estimates.hetero(dt=worker_panel[stayer_DiD==T,], y = "lrhwage", event_time_var = event_time_var, 
                                      grouping_var = "industry_match",
                                      hetero_var = "MNE_group",
                                      exclude_levels = NULL,
                                      levels_dict =  NULL,
                                      ref_event_time = ref_event_time, 
                                      controls = controls_wage, fes = fixefs_wage,
                                      add_fe_split = F,
                                      plot_title = "", ci_level = 0.95,
                                      create.OLS.weights = T, weight_group = "clusterID_worker", 
                                      cluster_vars = "SBEID_cluster",
                                      #dummy_scaling_var = "log_cost_event",
                                      diff_regs = T, diff_ref = "Domestic")
subset_wage$plts$Services
subset_wage$plts$Manufacturing

# save outputs
## Stayer
### Plot
ggsave(filename = paste0(map_output_here_hetero, "plot_NACE_Letter_stayer.pdf"), plot=subset_stayer$plts$both,
       scale = 1.2, dpi = 300,
       width=unit(0.45/0.7*6, "inches"), height=unit(0.45/0.7*4.5, "inches"))


  ## Wage
  ### Plot
  ggsave(filename = paste0(map_output_here_hetero, "plot_NACE_Letter_", "Services", "_wage_stayer.pdf"), plot=subset_wage$plts$Services,
         scale = 1.2, dpi = 300,
         width=unit(0.45/0.7*6, "inches"), height=unit(0.45/0.7*4.5, "inches"))

  ggsave(filename = paste0(map_output_here_hetero, "plot_NACE_Letter_", "Manufacturing", "_wage_stayer.pdf"), plot=subset_wage$plts$Manufacturing,
         scale = 1.2, dpi = 300,
         width=unit(0.45/0.7*6, "inches"), height=unit(0.45/0.7*4.5, "inches"))

  
rm(subset_stayer, subset_wage)  

################################################################################
##### Within group of MNEs and group of domestic ####
# Need to adjust fixed effects since now all treated firms belong to a single group
# Note that the function calls below add a fixed effect for event_time_var^group^subset_var
# Estimates are based on the full matched samples.
fixefs_within <- fixefs_stayer

#### Education level (where observed) ####

# Simplify
worker_panel_balanced[, educ_level_match_new := fifelse(educ_level_match==-1, -1, fifelse(educ_level_match==3, 3, 1))]
worker_panel[, educ_level_match_new := fifelse(educ_level_match==-1, -1, fifelse(educ_level_match==3, 3, 1))]

# create categorical variables for hetero plot function
worker_panel_balanced[treated==T, educ_level_cat := fifelse(educ_level_match_new==3, "High", 
                                                            fifelse(educ_level_match_new==1, "Low", "unknown"))]
worker_panel[treated==T, educ_level_cat := fifelse(educ_level_match_new==3, "High", 
                                                   fifelse(educ_level_match_new==1, "Low", "unknown"))]
# for control workers just set any category to trick function below
worker_panel_balanced[treated==F, educ_level_cat := "High"]
worker_panel[treated==F, educ_level_cat := "High"]

# NOTE: shares here are relative to the population with observed educ levels.
# Hence a) they don't add up to the total treatment effect because the total treatment effect in the subpopulation differs
# and b) estimes for group "other" have no interpretation here.
# Alternatively, you could decompose fully, s.t. estimates would add up but that adds noise in the MNE vs. DOM comparison 
# scale by share of type in respective population (observed educ level) --> Now estimates below add up to the overall stayer estimate 
worker_panel_balanced[treated==T & educ_level_match_new %in% c(1,3), share_type := fifelse(educ_level_match_new==1,
                                                                                           sum(educ_level_match_new[auto_event_time==-1]==1)/length(educ_level_match_new[auto_event_time==-1]),
                                                                                           fifelse(educ_level_match==3,
                                                                                                   sum(educ_level_match_new[auto_event_time==-1]==3)/length(educ_level_match_new[auto_event_time==-1]),
                                                                                                   sum(educ_level_match_new[auto_event_time==-1]==-1)/length(educ_level_match_new[auto_event_time==-1]))),
                      by=MNE_group]

worker_panel_balanced[treated==T & educ_level_match_new %in% c(-1), share_type := sum(educ_level_match_new[auto_event_time==-1]==-1)/length(educ_level_match_new[auto_event_time==-1])]

worker_panel_balanced[treated==F, share_type := -1] # just set to something other than missing

worker_panel_balanced[educ_level_cat=="High" & treated==T & auto_event_time==-1, mean(share_type), by=MNE_group]
worker_panel_balanced[educ_level_cat=="Low" & treated==T & auto_event_time==-1, mean(share_type), by=MNE_group]

# need inverse for correct scaling
worker_panel_balanced[, share_type := 1/share_type]

# Estimate, weighing by shares (interpretation is share of workerforce that separates with given educ level)
subset_stayer <- plots.estimates.hetero(dt=worker_panel_balanced, y = "leave", event_time_var = event_time_var, 
                                        grouping_var = "educ_level_cat",
                                        hetero_var = "MNE_group",
                                        exclude_levels = NULL, #-1,
                                        #levels_dict = c("low" = 1, "mid" = 2, "high" = 3), 
                                        levels_dict = NULL, # c("No University degree" = 1, "University degree" = 3), 
                                        ref_event_time = ref_event_time, 
                                        controls = controls_stayer, fes = fixefs_stayer,
                                        add_fe_split = F,
                                        plot_title = "", ci_level = 0.95,
                                        create.OLS.weights = T, weight_group = "clusterID_worker", 
                                        cluster_vars = "SBEID_cluster",
                                        dummy_scaling_var = "share_type",
                                        diff_regs = T, diff_ref = "Domestic", i_select = 3)

subset_stayer$plts$High
subset_stayer$plts$Low
subset_stayer$plts$unknown # cannot be interpreted easily given settings above


# wage
subset_wage <- plots.estimates.hetero(dt=worker_panel[stayer_DiD==T,], y = "lrhwage", event_time_var = event_time_var, 
                                      grouping_var = "educ_level_cat",
                                      hetero_var = "MNE_group",
                                      exclude_levels = NULL, #-1,
                                      #levels_dict = c("low" = 1, "mid" = 2, "high" = 3), 
                                      levels_dict = NULL, #c("No University degree" = 1, "University degree" = 3), 
                                      ref_event_time = ref_event_time, 
                                      controls = controls_wage, fes = fixefs_wage,
                                      add_fe_split = F,
                                      plot_title = "", ci_level = 0.95,
                                      create.OLS.weights = T, weight_group = "clusterID_worker", 
                                      cluster_vars = "SBEID_cluster",
                                      #dummy_scaling_var = "log_cost_event",
                                      diff_regs = T, diff_ref = "Domestic")
subset_wage$plts$High
subset_wage$plts$Low 
subset_wage$plts$unknown # Note: follows population average.


# save outputs
## Stayer
### Plot
ggsave(filename = paste0(map_output_here_hetero, "plot_educ_level_stayer_High.pdf"), plot=subset_stayer$plts$High,
       scale = 1.2, dpi = 300,
       width=unit(0.45/0.7*6, "inches"), height=unit(0.45/0.7*4.5, "inches"))
ggsave(filename = paste0(map_output_here_hetero, "plot_educ_level_stayer_Low.pdf"), plot=subset_stayer$plts$Low,
       scale = 1.2, dpi = 300,
       width=unit(0.45/0.7*6, "inches"), height=unit(0.45/0.7*4.5, "inches"))

## Wage
### Plot
ggsave(filename = paste0(map_output_here_hetero, "plot_educ_level_stayer_wage_High.pdf"), plot=subset_wage$plts$High,
       scale = 1.2, dpi = 300,
       width=unit(0.45/0.7*6, "inches"), height=unit(0.45/0.7*4.5, "inches"))
ggsave(filename = paste0(map_output_here_hetero, "plot_educ_level_stayer_wage_Low.pdf"), plot=subset_wage$plts$Low,
       scale = 1.2, dpi = 300,
       width=unit(0.45/0.7*6, "inches"), height=unit(0.45/0.7*4.5, "inches"))

rm(subset_stayer, subset_wage)


#### Education subject (where observed) ####
# set new grouping
worker_panel[, st_dir_match := OPLRICHTING[auto_event_time==-1], by=.(workerID, cohort)]
worker_panel[is.na(st_dir_match), st_dir_match := "Unknown"]

worker_panel[st_dir_match %in% c("Wiskunde, natuurwetenschappen", "Informatica"), st_dir_match := "IT and Sciences"]
worker_panel[st_dir_match %in% c("Techniek, industrie en bouwkunde"), st_dir_match := "Engineering"]
worker_panel[st_dir_match %in% c("Recht, administratie, handel en zakelijke dienstverlening", "Dienstverlening",
                                 "Journalistiek, gedrag en maatschappij"), st_dir_match := "Other"]
worker_panel[st_dir_match %in% c("Algemeen",                                                  
                                 "Onderwijs",
                                 "Landbouw, diergeneeskunde en -verzorging",
                                 "Gezondheidszorg en welzijn","Vormgeving, kunst, talen en geschiedenis"), st_dir_match := "Other"]
worker_panel[st_dir_match %in% c("onbekend"), st_dir_match := "Unknown"] #"unknown"
worker_panel[, unique(st_dir_match)]


# for untreated set to Other
worker_panel[treated==F, st_dir_match := "Other"]


#worker_panel[, MNE_man := paste0(MNE_group, "_", manager_cat)]
# Wages
subset_wage_general <- plots.estimates.hetero(dt=worker_panel[stayer_DiD==T,], y = "lrhwage", event_time_var = event_time_var, 
                                              grouping_var = "st_dir_match",
                                              hetero_var = "MNE_group",
                                              exclude_levels = "Unknown", #"onbekend",
                                              levels_dict =  NULL,
                                              ref_event_time = ref_event_time, 
                                              controls = controls_wage, fes = fixefs_wage,
                                              add_fe_split = F,
                                              plot_title = "", ci_level = 0.95,
                                              create.OLS.weights = T, weight_group = "clusterID_worker", 
                                              cluster_vars = "SBEID_cluster",
                                              diff_regs = T, diff_ref = "Domestic")
subset_wage_general$plts$`IT and Sciences`
subset_wage_general$plts$Engineering
subset_wage_general$plts$Other
subset_wage_general$plts$Unknown


# save
ggsave(filename = paste0(map_output_here_hetero, "plot_educ_IT_stayer_wage.pdf"), plot=subset_wage_general$plts$`IT and Sciences`,
       scale = 1.2, dpi = 300,
       width=unit(0.45/0.7*6, "inches"), height=unit(0.45/0.7*4.5, "inches"))
ggsave(filename = paste0(map_output_here_hetero, "plot_educ_Engineering_stayer_wage.pdf"), plot=subset_wage_general$plts$Engineering,
       scale = 1.2, dpi = 300,
       width=unit(0.45/0.7*6, "inches"), height=unit(0.45/0.7*4.5, "inches"))
ggsave(filename = paste0(map_output_here_hetero, "plot_educ_Other_stayer_wage.pdf"), plot=subset_wage_general$plts$Other,
       scale = 1.2, dpi = 300,
       width=unit(0.45/0.7*6, "inches"), height=unit(0.45/0.7*4.5, "inches"))



# stayer 
# set new grouping
worker_panel_balanced[, st_dir_match := OPLRICHTING[auto_event_time==-1], by=.(workerID, cohort)]
worker_panel_balanced[is.na(st_dir_match), st_dir_match := "Unknown"]

worker_panel_balanced[st_dir_match %in% c("Wiskunde, natuurwetenschappen", "Informatica"), st_dir_match := "IT and Sciences"]
worker_panel_balanced[st_dir_match %in% c("Techniek, industrie en bouwkunde"), st_dir_match := "Engineering"]
worker_panel_balanced[st_dir_match %in% c("Recht, administratie, handel en zakelijke dienstverlening", "Dienstverlening",
                                 "Journalistiek, gedrag en maatschappij"), st_dir_match := "Other"]
worker_panel_balanced[st_dir_match %in% c("Algemeen",                                                  
                                 "Onderwijs",
                                 "Landbouw, diergeneeskunde en -verzorging",
                                 "Gezondheidszorg en welzijn","Vormgeving, kunst, talen en geschiedenis"), st_dir_match := "Other"]
worker_panel_balanced[st_dir_match %in% c("onbekend"), st_dir_match := "Unknown"] #"unknown"
worker_panel_balanced[, unique(st_dir_match)]


# for untreated set to Other
worker_panel_balanced[treated==F, st_dir_match := "Other"]


# scale by share of contracts in respective population --> Now estimates below add up to the overall stayer estimate 
worker_panel_balanced[treated==T & !(st_dir_match %in% c("Unknown")), share_type := fifelse(st_dir_match=="IT and Sciences",
                                                                     sum(st_dir_match[auto_event_time==-1]=="IT and Sciences")/length(st_dir_match[auto_event_time==-1]),
                                                                     fifelse(st_dir_match=="Engineering",
                                                                             sum(st_dir_match[auto_event_time==-1]=="Engineering")/length(st_dir_match[auto_event_time==-1]),
                                                                             sum(st_dir_match[auto_event_time==-1]=="Other")/length(st_dir_match[auto_event_time==-1]))),
                      by=.(MNE_group)]
worker_panel_balanced[treated==T & (st_dir_match %in% c("Unknown")), share_type := sum(st_dir_match[auto_event_time==-1]=="Unknown")/length(st_dir_match[auto_event_time==-1])]

worker_panel_balanced[treated==F, share_type := -1]

worker_panel_balanced[st_dir_match=="IT and Sciences" & treated==T & auto_event_time==4, mean(share_type), by=MNE_group]
worker_panel_balanced[st_dir_match=="Engineering" & treated==T & auto_event_time==4, mean(share_type), by=MNE_group]
worker_panel_balanced[st_dir_match=="Other" & treated==T & auto_event_time==4, mean(share_type), by=MNE_group]



# take inverse for correct scaling
worker_panel_balanced[, share_type := 1/share_type]

# set subsample 
worker_panel_balanced[, include := ((treated &  !(st_dir_match %in% c("Unknown"))) | !treated)]
worker_panel_balanced[include==T, include := any(treated) & any(!treated), by=c("clusterID_worker", "year")]

# stayer
subset_stayer <- plots.estimates.hetero(dt=worker_panel_balanced, 
                                        y = "leave", event_time_var = event_time_var, 
                                        grouping_var = "st_dir_match",
                                        hetero_var = "MNE_group",
                                        exclude_levels = NULL, # c(9),
                                        levels_dict =  NULL, #c("Permanent" = 1, "Temporary" = 2),
                                        ref_event_time = ref_event_time, 
                                        controls = controls_stayer, fes = fixefs_stayer,
                                        add_fe_split = F,
                                        plot_title = "", ci_level = 0.95,
                                        create.OLS.weights = T, weight_group = "clusterID_worker", 
                                        cluster_vars = "SBEID_cluster",
                                        dummy_scaling_var = "share_type",
                                        diff_regs = T, diff_ref = "Domestic", i_select = 3)
subset_stayer$plts$`IT and Sciences`
subset_stayer$plts$Engineering
subset_stayer$plts$Other
subset_stayer$plts$Unknown



# save
ggsave(filename = paste0(map_output_here_hetero, "plot_educ_IT_stayer.pdf"), plot=subset_stayer$plts$`IT and Sciences`,
       scale = 1.2, dpi = 300,
       width=unit(0.45/0.7*6, "inches"), height=unit(0.45/0.7*4.5, "inches"))
ggsave(filename = paste0(map_output_here_hetero, "plot_educ_Engineering_stayer.pdf"), plot=subset_stayer$plts$Engineering,
       scale = 1.2, dpi = 300,
       width=unit(0.45/0.7*6, "inches"), height=unit(0.45/0.7*4.5, "inches"))
ggsave(filename = paste0(map_output_here_hetero, "plot_educ_Other_stayer.pdf"), plot=subset_stayer$plts$Other,
       scale = 1.2, dpi = 300,
       width=unit(0.45/0.7*6, "inches"), height=unit(0.45/0.7*4.5, "inches"))




###### Manager ####
# stayer worker_panel_balanced
# create categorical variables for hetero plot function
worker_panel_balanced[, manager_cat := fifelse(manager_match, "Manager", "Other")]
worker_panel[, manager_cat := fifelse(manager_match, "Manager", "Other")]

# scale by share of type in respective population --> Now estimates below add up to the overall stayer estimate 
worker_panel_balanced[treated==T, share_type := fifelse(manager_match==T,
                                                        sum(manager_match[auto_event_time==-1]==TRUE)/length(manager_match[auto_event_time==-1]),
                                                        sum(manager_match[auto_event_time==-1]==FALSE)/length(manager_match[auto_event_time==-1])),
                      by=MNE_group]
worker_panel_balanced[treated==F, share_type := -1] # just set to something other than missing

# need inverse for correct scaling
worker_panel_balanced[, share_type := 1/share_type]

subset_stayer <- plots.estimates.hetero(dt=worker_panel_balanced, y = "leave", event_time_var = event_time_var, 
                                        grouping_var = "manager_cat",
                                        hetero_var = "MNE_group",
                                        exclude_levels = NULL,
                                        levels_dict =  NULL, #c("Manager" = TRUE, "Non-Manager" = FALSE),
                                        ref_event_time = ref_event_time, 
                                        controls = controls_stayer, fes = fixefs_stayer,
                                        add_fe_split = F,
                                        plot_title = "", ci_level = 0.95,
                                        create.OLS.weights = T, weight_group = "clusterID_worker", 
                                        cluster_vars = "SBEID_cluster",
                                        dummy_scaling_var = "share_type",
                                        diff_regs = T, diff_ref = "Domestic", i_select = 3)
subset_stayer$plts$Manager
subset_stayer$plts$Other
#etable(subset_stayer$regs$MNE)
#etable(subset_stayer$regs$Domestic)

# wage
subset_wage <- plots.estimates.hetero(dt=worker_panel[stayer_DiD==T,], y = "lrhwage", event_time_var = event_time_var, 
                                      grouping_var = "manager_cat",
                                      hetero_var = "MNE_group",
                                      exclude_levels = NULL,
                                      levels_dict =  NULL, # c("Manager" = TRUE, "Non-Manager" = FALSE),
                                      ref_event_time = ref_event_time, 
                                      controls = controls_wage, fes = fixefs_wage,
                                      add_fe_split = F,
                                      plot_title = "", ci_level = 0.95,
                                      create.OLS.weights = T, weight_group = "clusterID_worker", 
                                      cluster_vars = "SBEID_cluster",
                                      #dummy_scaling_var = "log_cost_event",
                                      diff_regs = T, diff_ref = "Domestic")
subset_wage$plts$Manager
subset_wage$plts$Other

# Stayer
ggsave(filename = paste0(map_output_here_hetero, "plot_Manager_stayer.pdf"), plot=subset_stayer$plts$Manager,
       scale = 1.2, dpi = 300,
       width=unit(0.45/0.7*6, "inches"), height=unit(0.45/0.7*4.5, "inches"))
ggsave(filename = paste0(map_output_here_hetero, "plot_Non_Manager_stayer.pdf"), plot=subset_stayer$plts$Other,
       scale = 1.2, dpi = 300,
       width=unit(0.45/0.7*6, "inches"), height=unit(0.45/0.7*4.5, "inches"))

# wage
ggsave(filename = paste0(map_output_here_hetero, "plot_Manager_stayer_wage.pdf"), plot=subset_wage$plts$Manager,
       scale = 1.2, dpi = 300,
       width=unit(0.45/0.7*6, "inches"), height=unit(0.45/0.7*4.5, "inches"))
ggsave(filename = paste0(map_output_here_hetero, "plot_Non_Manager_stayer_wage.pdf"), plot=subset_wage$plts$Other,
       scale = 1.2, dpi = 300,
       width=unit(0.45/0.7*6, "inches"), height=unit(0.45/0.7*4.5, "inches"))

rm(subset_wage, subset_stayer)


###### Contract type ####
controls_min <- "+ age^2"

# simplify and set categorical variable
# Note: only very few workers have a contract of type 9 ("not applicable") -- These are for example Directors
worker_panel_balanced[, SCONTRACTSOORT_match_new := fifelse(SCONTRACTSOORT_match==1, 1, 2)]
worker_panel_balanced[, SCONTRACTSOORT_match_cat := fifelse(SCONTRACTSOORT_match_new==1, "permanent", "temporary")]

# scale by share of contracts in respective population --> Now estimates below add up to the overall stayer estimate 
worker_panel_balanced[treated==T, share_type := fifelse(SCONTRACTSOORT_match_new==1,
                                                        sum(SCONTRACTSOORT_match_new[auto_event_time==-1]==1)/length(SCONTRACTSOORT_match_new[auto_event_time==-1]),
                                                        sum(SCONTRACTSOORT_match_new[auto_event_time==-1]==2)/length(SCONTRACTSOORT_match_new[auto_event_time==-1])),
                      by=MNE_group]
worker_panel_balanced[treated==F, share_type := -1]

worker_panel_balanced[SCONTRACTSOORT_match_cat=="temporary" & treated==T & auto_event_time==4, mean(share_type), by=MNE_group]
worker_panel_balanced[SCONTRACTSOORT_match_cat=="permanent" & treated==T & auto_event_time==4, mean(share_type), by=MNE_group]

# take inverse for correct scaling
worker_panel_balanced[, share_type := 1/share_type]

# stayer
subset_stayer <- plots.estimates.hetero(dt=worker_panel_balanced[,], 
                                        y = "leave", event_time_var = event_time_var, 
                                        grouping_var = "SCONTRACTSOORT_match_cat",
                                        hetero_var = "MNE_group",
                                        exclude_levels = NULL, # c(9),
                                        levels_dict =  NULL, #c("Permanent" = 1, "Temporary" = 2),
                                        ref_event_time = ref_event_time, 
                                        controls = controls_min, fes = fixefs_stayer,
                                        add_fe_split = F,
                                        plot_title = "", ci_level = 0.95,
                                        create.OLS.weights = T, weight_group = "clusterID_worker", 
                                        cluster_vars = "SBEID_cluster",
                                        dummy_scaling_var = "share_type",
                                        diff_regs = T, diff_ref = "Domestic")
#subset_stayer$plts$unknown
subset_stayer$plts$temporary
subset_stayer$plts$permanent

# Coefficients
# MNE temporary
subset_stayer$reg$coefficients[grep("event_time_temporary_MNE::4", names(subset_stayer$reg$coefficients))]
# MNE permanent
subset_stayer$reg$coefficients[grep("event_time_permanent_MNE::4", names(subset_stayer$reg$coefficients))]
# DOM  
subset_stayer$reg$coefficients[grep("event_time_temporary_Domestic::4", names(subset_stayer$reg$coefficients))] +
subset_stayer$reg$coefficients[grep("event_time_permanent_Domestic::4", names(subset_stayer$reg$coefficients))]

# compare
stayer_ana$regs$coefficients[grep("event_time_Domestic::4", names(stayer_ana$regs$coefficients))]


# save outputs 
## temporary 
ggsave(filename = paste0(map_output_here_hetero, "plot_temporary_contract_stayer.pdf"), plot=subset_stayer$plts$temporary, 
       scale = 1.2, dpi = 300,
       width=unit(0.45/0.7*6, "inches"), height=unit(0.45/0.7*4.5, "inches"))
## permanent
#### Plot  
ggsave(filename = paste0(map_output_here_hetero, "plot_permanent_contract_stayer.pdf"), plot=subset_stayer$plts$permanent, 
       scale = 1.2, dpi = 300,
       width=unit(0.45/0.7*6, "inches"), height=unit(0.45/0.7*4.5, "inches"))

rm(subset_stayer)


###### Full-time vs part-time ####
# set categories
worker_panel[, part_time_cat := fifelse(part_time_match, "parttime", "fulltime")]
worker_panel_balanced[, part_time_cat := fifelse(part_time_match, "parttime", "fulltime")]


# stayer
# scale by share of type in respective population (observed educ level) --> Now estimates below add up to the overall stayer estimate 
worker_panel_balanced[treated==T, share_type := fifelse(part_time_match,
                                                        sum(part_time_match[auto_event_time==-1]==TRUE)/length(part_time_match[auto_event_time==-1]),
                                                        sum(part_time_match[auto_event_time==-1]==FALSE)/length(part_time_match[auto_event_time==-1])),
                      by=MNE_group]
worker_panel_balanced[treated==F, share_type := -1] # just set to something other than missing
worker_panel_balanced[, share_type := 1/share_type]


subset_stayer <- plots.estimates.hetero(dt=worker_panel_balanced, y = "leave", event_time_var = event_time_var, 
                                        grouping_var = "part_time_cat",
                                        hetero_var =  "MNE_group",
                                        exclude_levels = NULL,
                                        levels_dict =  NULL, #c("Part-time (< 0.8 fte)" = TRUE, "Full-time" = FALSE),
                                        ref_event_time = ref_event_time, 
                                        controls = controls_stayer, fes = fixefs_stayer,
                                        add_fe_split = F,
                                        plot_title = "", ci_level = 0.95,
                                        create.OLS.weights = T, weight_group = "clusterID_worker", 
                                        cluster_vars = "SBEID_cluster",
                                        dummy_scaling_var = "share_type",
                                        diff_regs = T, diff_ref = "Domestic", i_select = 3)
subset_stayer$plts$parttime
subset_stayer$plts$fulltime
#etable(subset_stayer$regs$MNE)
#etable(subset_stayer$regs$Domestic)

# wage
subset_wage <- plots.estimates.hetero(dt=worker_panel[stayer_DiD==T,], y = "lrhwage", event_time_var = event_time_var, 
                                      grouping_var = "part_time_cat",
                                      hetero_var =  "MNE_group",
                                      exclude_levels = NULL,
                                      levels_dict =  c("Part-time (< 0.8 fte)" = TRUE, "Full-time" = FALSE),
                                      ref_event_time = ref_event_time, 
                                      controls = controls_wage, fes = fixefs_wage,
                                      add_fe_split = F,
                                      plot_title = "", ci_level = 0.95,
                                      create.OLS.weights = T, weight_group = "clusterID_worker", 
                                      cluster_vars = "SBEID_cluster",
                                      #dummy_scaling_var = "share_fixed_term",
                                      diff_regs = T, diff_ref = "Domestic")
subset_wage$plts$fulltime
subset_wage$plts$parttime
#etable(subset_wage$regs$MNE)
#etable(subset_wage$regs$Domestic)


# save outputs 
### Stayer
ggsave(filename = paste0(map_output_here_hetero, "plot_parttime_stayer.pdf"), plot=subset_stayer$plts$parttime, 
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))
ggsave(filename = paste0(map_output_here_hetero, "plot_fulltime_stayer.pdf"), plot=subset_stayer$plts$fulltime, 
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))
#### Difference regression (to do)
#etable(1)
### Wage
ggsave(filename = paste0(map_output_here_hetero, "plot_parttime_stayer_wage.pdf"), plot=subset_wage$plts$parttime, 
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))
ggsave(filename = paste0(map_output_here_hetero, "plot_fulltime_stayer_wage.pdf"), plot=subset_wage$plts$fulltime, 
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))

rm(subset_wage, subset_stayer)

############# Other additions ############

#### Educ level by sector #####
# stayer
worker_panel_balanced[, MNE_group_industry := paste0(MNE_group, "_", industry_match)] #"Manufacturing", "Services"
worker_panel_balanced[, include := (treated & educ_level_match_new!=-1) | !treated ] #(treated & educ_level_match) | !treated]
worker_panel_balanced[include==T, include := any(treated) & any(!treated), by=.(clusterID_worker, year)]

subset_stayer_level <- plots.estimates.hetero(dt=worker_panel_balanced[include==T,], y = "leave", event_time_var = event_time_var, 
                                              grouping_var = "MNE_group_industry",
                                              hetero_var = "educ_level_match_new",
                                              exclude_levels = -1,
                                              levels_dict = c("low" = 1, "medium" = 2, "high" = 3),
                                              ref_event_time = ref_event_time, 
                                              controls = controls_stayer, fes = fixefs_stayer,
                                              add_fe_split = F,
                                              plot_title = "", ci_level = 0.95,
                                              create.OLS.weights = T, weight_group = "clusterID_worker", 
                                              cluster_vars = "SBEID_cluster",
                                              #dummy_scaling_var = "log_cost_event",
                                              diff_regs = T, diff_ref = "Domestic")

subset_stayer_level$plts$MNE_Manufacturing
subset_stayer_level$plts$MNE_Services
subset_stayer_level$plts$Domestic_Manufacturing
subset_stayer_level$plts$Domestic_Services


# save outputs
## Stayer
### Plot
ggsave(filename = paste0(map_output_here_hetero, "plot_Manufacturing_educ_level_stayer_MNE.pdf"), plot=subset_stayer_level$plts$MNE_Manufacturing,
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))
ggsave(filename = paste0(map_output_here_hetero, "plot_Services_educ_level_stayer_MNE.pdf"), plot=subset_stayer_level$plts$MNE_Services,
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))
ggsave(filename = paste0(map_output_here_hetero, "plot_Manufacturing_educ_level_stayer_DOM.pdf"), plot=subset_stayer_level$plts$Domestic_Manufacturing,
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))
ggsave(filename = paste0(map_output_here_hetero, "plot_Services_educ_level_stayer_DOM.pdf"), plot=subset_stayer_level$plts$Domestic_Services,
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))


# wage
worker_panel[, MNE_group_industry := paste0(MNE_group, "_", industry_match)]
worker_panel[, include := stayer_DiD]
worker_panel[include==T, include := (treated & educ_level_match_new!=-1) | !treated ] #(treated & educ_level_match) | !treated]
worker_panel[include==T, include := any(treated) & any(!treated), by=.(clusterID_worker, year)]


subset_wage_level <- plots.estimates.hetero(dt=worker_panel[include==T,], y = "lrhwage", event_time_var = event_time_var, 
                                            grouping_var = "MNE_group_industry",
                                            hetero_var = "educ_level_match_new",
                                            exclude_levels = -1,
                                            levels_dict = c("low" = 1, "medium" = 2, "high" = 3),
                                            ref_event_time = ref_event_time, 
                                            controls = controls_wage, fes = fixefs_wage,
                                            add_fe_split = F,
                                            plot_title = "", ci_level = 0.95,
                                            create.OLS.weights = T, weight_group = "clusterID_worker", 
                                            cluster_vars = "SBEID_cluster",
                                            #dummy_scaling_var = "log_cost_event",
                                            diff_regs = T, diff_ref = "Domestic")
subset_wage_level$plts$MNE_Manufacturing
subset_wage_level$plts$MNE_Services
subset_wage_level$plts$Domestic_Manufacturing
subset_wage_level$plts$Domestic_Services

# save outputs
## Stayer
### Plot
ggsave(filename = paste0(map_output_here_hetero, "plot_Manufacturing_educ_level_stayer_wage_MNE.pdf"), plot=subset_wage_level$plts$MNE_Manufacturing,
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))
ggsave(filename = paste0(map_output_here_hetero, "plot_Services_educ_level_stayer_wage_MNE.pdf"), plot=subset_wage_level$plts$MNE_Services,
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))
ggsave(filename = paste0(map_output_here_hetero, "plot_Manufacturing_educ_level_stayer_wage_DOM.pdf"), plot=subset_wage_level$plts$Domestic_Manufacturing,
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))
ggsave(filename = paste0(map_output_here_hetero, "plot_Services_educ_level_stayer_wage_DOM.pdf"), plot=subset_wage_level$plts$Domestic_Services,
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))

rm(subset_wage_level, subset_stayer_level)


##################################################################################
### Conditional treatment effect ####
##################################################################################

source(paste0(map_scripts, "05_SOURCE_conditional_treatment_effect.R"), echo=F) # source script that calculates treatment effect of MNEs (rel. to domestic) conditional on other treatments

##################################################################################
### Analysis of hires ####
##################################################################################

source(paste0(map_scripts, "04_SOURCE_firm_hires.R"), echo=F) # source script that runs hires analysis

##################################################################################
### Analysis of IT and Machinery spikes ####
##################################################################################

source(paste0(map_scripts, "04_SOURCE_IT_Machinery_analyses.R"), echo=F) # source script that runs analyses on spikes in IT or Machinery investments

