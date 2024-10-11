##################################################################################
### This script creates summary stats of the matched sample ####
### It is sourced in the script 04_main_worker.R ####
##################################################################################

### packages ######
if (!require("xtable")) install.packages("xtable"); library("xtable")
if (packageVersion("xtable")!="1.8.4") warning("Analysis ran in xtable version 1.8.4 - consider up- or downgrading")

if (!require("Hmisc")) install.packages("Hmisc"); library("Hmisc")
if (packageVersion("Hmisc")!="5.1.0") warning("Analysis ran in Hmisc version 5.1.0 - consider up- or downgrading")


### Output dir (dependent on map_output_here setting) #####
if (!dir.exists(paste0(map_output_here, "matching_stats/"))) dir.create(paste0(map_output_here, "matching_stats/"))
map_output_script <- paste0(map_output, "main_estimates/matching_stats/")

##################################################################################
##################################################################################
### OLS Weights (for quick check) ###
# calculate weights (1 for treated, N(treated)/N(untreated) in stratum for controls)
worker_panel[get(event_time_var) == matching_event_time, weight_OLS := fifelse(treated, 1, sum(treated)/sum(!treated)), by=c("clusterID_worker", event_time_var)]
# assign over panel
worker_panel[, weight_OLS := weight_OLS[get(event_time_var)==matching_event_time], by=.(workerID, cohort)]

# calculate weights (1 for treated, N(treated)/N(untreated) in stratum for controls)
firm_panel[get(event_time_var) == matching_event_time, weight_OLS := fifelse(treated, 1, sum(treated)/sum(!treated)), by=c("clusterID_firm", event_time_var)]
# assign over panel
firm_panel[, weight_OLS := weight_OLS[get(event_time_var)==matching_event_time], by=.(SBEID, cohort)]


# worker_panel
OLS_weights <- worker_panel[auto_event_time == matching_event_time, .(treated, weight_OLS)]
OLS_weights <- as.matrix(OLS_weights[treated==F, quantile(weight_OLS, p = c(seq(0,0.9,0.1),0.95,0.99,0.9999,1))])
colnames(OLS_weights) <- c("Weight")

print(xtable(OLS_weights, caption = "Distribution of OLS weights among control workers.", digits = 3),
      caption.placement = "top", size = "\\footnotesize", table.placement = "H", 
      label = "tab:OLS_weights",
      booktabs = T,
      file = paste0(map_output_script, "table_weights_worker.tex"))


##################################################################################
# function to create summary stat tables

# function that calculates summary stats using OLS weights
summary_stat_worker_panel <- function(dt, vars, 
                                      col_var = "treated", 
                                      col_var_order = c("unmatched", "matched_untreated", "matched_treated"), 
                                      col_var_dict = c("Full sample", "Control workers", "Treated workers"),
                                      categorical_vars = NULL, vars_dict = NULL, 
                                      strata_var = "clusterID_worker", use_weights = F, weight_var = NULL){
  # NOTE: this function is extremely specific to my use case.
  # dt is a data.table
  # vars are the variales for the summary statistics
  # col_var is a categorical variable that determines the division of the data (i.e. seperate columns in output)
  # col_var_order is the desired order
  # col_var dict is a dictionary
  # categorical_vars identifies categorical vars within vars
  # vars_dict is a dictionary for the rownames
  # strata_var is the variable identifying strata in dt. NOTE: for categorical vars this needs to be set, also when no matching is used.
  # use_weights determines whether frequency weights should be used
  # weight_var identifies the variable containing the frequency weights
  
  
  # library to calculate weighted standard deviation
  require(Hmisc)
  
  # check if weighing should be applied and set var_weight
  if (use_weights==T){
    dt[, var_weight := get(weight_var)]
  } else{
    dt[, var_weight := 1]
  }
  
  # add n
  if (length(categorical_vars) != 0){
    # Note categorical vars will be added at end
    vars <- vars[-which(vars %in% categorical_vars)]
    rows <- length(vars) + 5
    # add one row per category per variable
    rows <- rows + sum(unlist(dt[, lapply(.SD, uniqueN), .SDcols = categorical_vars]))
    
    # set row names 
    rnames <- c("N (workers)", "Unique workers", "N (firms)", "Unique firms", "Matching strata", vars)
    rnames <- c(rnames, dt[, unlist(lapply(.SD, unique)), .SDcols = categorical_vars])
    # paste0(unlist(dt[, lapply(.SD, unique), .SDcols = categorical_vars]), " (%)")
  } else {
    rows <- length(vars)+5
    rnames <- c("N (workers)", "unique workers", "N (firms)", "unique firms", "strata", vars)
  }
  # create summary statistics matrix
  ## 
  
  summary_stats <- matrix(NA, nrow=rows, ncol=length(col_var_order)*2)
  # for each category create heading
  cnames <- c()
  for (n in col_var_dict){
    cnames <- c(cnames, paste0(n, " (Mean)"), paste0(n, " (SD)"))
  }
  colnames(summary_stats) <- cnames
  rownames(summary_stats) <- rnames
  
  # loop through categories
  c <- -1
  for (n in col_var_order){
    c <- c+2
    # add workers and unique workers
    summary_stats[1:2, c] <- t(dt[get(col_var)==n, .(.N, uniqueN(workerID))])
    # add firm and unique firms
    summary_stats[3:4, c] <- t(dt[get(col_var)==n, .(uniqueN(paste0(SBEID, "-", cohort)), uniqueN(SBEID))])
    # add matching strata, if there is a strata var
    # add Strata stats if there is a strata_var
    if (length(strata_var)!=0 & strata_var %in% names(dt)){
      # strata: add N and unique N of untreated
      summary_stats[5,c] <- t(dt[get(col_var)==n, .(uniqueN(get(strata_var)))])
    }
    
    # add means and SD of non-categorical variables
    #summary_stats[6:(length(vars)+5),c] <- t(dt[get(col_var)==n, lapply(.SD, function(x) mean(x, na.rm=T)), .SDcols = vars])
    #summary_stats[6:(length(vars)+5),c+1] <- t(dt[get(col_var)==n, lapply(.SD, function(x) sd(x, na.rm=T)), .SDcols = vars])
    ## using weights
    summary_stats[6:(length(vars)+5),c] <- t(dt[get(col_var)==n, lapply(.SD, function(x) wtd.mean(x, weights = var_weight, na.rm=T, normwt = F)), .SDcols = vars])
    summary_stats[6:(length(vars)+5),c+1] <- t(dt[get(col_var)==n, lapply(.SD, function(x) sqrt(wtd.var(x, weights = var_weight, na.rm=T, normwt = F))), .SDcols = vars])
    
    
    # Note: for categorical vars, I use strata_var weighing.
    i <- (length(vars)+5) # keep track of row position
    i_initial <- i+1
    for (variable in categorical_vars){ # loop through categorical vars
      
      # loop through categories 
      for (f in 1:dt[, uniqueN(get(variable))]){
        i <- i+1 # update matrix row
        # add share
        summary_stats[i,c] <- dt[get(col_var)==n, uniqueN(get(strata_var)[(get(variable)==rnames[i])])/uniqueN(get(strata_var))*100]
      }
    }
    
  }
  
  # set names catergorical vars
  rnames[i_initial:i] <- paste0(rnames[i_initial:i], " (%)")
  rownames(summary_stats) <- rnames
  
  # replace rownames by vars_dict if specified
  if (length(vars_dict)!=0){
    replace <- (rownames(summary_stats) %in% names(vars_dict))
    rownames(summary_stats)[replace] <- vars_dict[match(rownames(summary_stats)[replace], names(vars_dict))]
  }
  
  return(summary_stats)
}

##################################################################################

##################################################################################
### Overall summary stats #####
#### summary stats settings ####
matching_event_time <- matching_event_time 


# variables to evaluate
vars_worker <- c("female", "foreign", "age", "part_time", "fixed_term", "manager_match", "hwage_real", "hwage_real_growth")
vars_worker_only_treated <- NULL
vars_firm <- c("MNE", "rsales_mean", "exporter", "importer", "NACE_letter_label")
vars_firm_only_treated <- NULL # c("auto_cost_percent", "automation_cost_real", "automation_cost_real_pw")

# identify categorical vars (need special handling)
categorical_vars <- c("NACE_letter_label")

# combine for creating summary data sets below and call for MNE vs. Domestic treated difference
vars_w <- unique(c(vars_worker, vars_worker_only_treated))
vars_f <- unique(c(vars_firm, vars_firm_only_treated))

# create a dictionary
vars_dict_labels <- unique(c(vars_w, vars_f))
vars_dict_labels
vars_dict <- c("Female", "Foreign born or foreign-born parents", "Age", "Part-time", "Fixed term contract", "Manager", "Hourly wage real", "Hourly wage growth",
               "MNE",
               "Sales per worker real (1000 EURs)", "Exporter", "Importer",
               "NACE Industry")
               #, "Automation cost share", "Automation cost real (1000 EURs)", "Automation cost real per worker (1000 EURs)")
names(vars_dict) <- vars_dict_labels

##################################################################################
#### matched data set: create summary vars in worker_panel and firm_panel #####

##### in worker panel ######
worker_panel[, fixed_term := SCONTRACTSOORT == 2]
# create hwage real
worker_panel[, hwage_real := exp(lrhwage)]

worker_panel[, hwage_real_growth := lrhwage-lrhwage_1]

# foreign born or foreign born parents
worker_panel[, foreign := birthcountry!=6030]

##### in firm panel: different variables ######
# mean employment (worker_fte) in pre-treatment period
firm_panel[get(event_time_var) %in% min(event_time):matching_event_time, 
           workers_fte_mean := mean(workers_fte),
           by=.(SBEID, cohort)]

# mean workers_fte growth
firm_panel[order(SBEID, cohort, year)  &  get(event_time_var) %in% min(event_time):matching_event_time,
           workers_fte_growth := mean(log(workers_fte)-shift(log(workers_fte), n=1, type="lag"), na.rm=T),
           by=.(SBEID, cohort)]

firm_panel[get(event_time_var) %in% min(event_time):matching_event_time,
           rsales_mean := mean(rsales/workers_fte), by=.(SBEID, cohort)]

firm_panel[get(event_time_var) %in% min(event_time):matching_event_time,
           rexport_pw_new := fifelse(exporter, mean(rexport/workers_fte_mean), NA_real_),
           by=.(SBEID, cohort)]
firm_panel[get(event_time_var) %in% min(event_time):matching_event_time,
           rimport_pw_new := fifelse(importer, mean(rimport/workers_fte_mean), NA_real_),
           by=.(SBEID, cohort)]

# automation expenditures
firm_panel[treated==T, auto_cost_percent := auto_cost_share[auto_event_time==0]*100, by=.(SBEID, cohort)]
firm_panel[treated==T, automation_cost_real := automation_cost[auto_event_time==0]*factor, by=.(SBEID, cohort)]
firm_panel[treated==T, automation_cost_real_pw := real_automation_cost_pw]


# NACE industry labels
firm_panel[, NACE_letter_label := fifelse(NACE_letter=="C", "Manufacturing", 
                                          fifelse(NACE_letter=="F", "Construction",
                                                  fifelse(NACE_letter=="G", "Wholesale and Retail Trade",
                                                          fifelse(NACE_letter=="H", "Transportation and Storage",
                                                                  fifelse(NACE_letter=="I", "Accomodation and Food Service", 
                                                                          fifelse(NACE_letter=="J", "Information and Communication",
                                                                                  fifelse(NACE_letter=="M", "Prof., Scientific and Technical Activities",
                                                                                          fifelse(NACE_letter=="N", "Administrative and Support Activities", "MISSING"))))))))]



##################################################################################
#### matched data set: extract worker-level panel at auto_event_time == matching_event_time ####
suma_matched <- worker_panel[auto_event_time == matching_event_time, unique(c("treated", "workerID", "clusterID_worker", "clusterID_firm", "SBEID", "cohort", "SBEID_cluster", "weight_OLS", vars_w)), with=F]
## add all necessary firm-level vars
suma_matched <- merge(suma_matched, 
                      firm_panel[auto_event_time == matching_event_time, unique(c("SBEID", "cohort", "MNE", vars_f)), with=F],
                      by=c("SBEID", "cohort"))


##################################################################################
#### unmatched data set: create dataset and summary vars #####
# create a worker panel out of potential treated and control (return of matching function)
worker_panel_full <- merge(POLIS_worker, potential_treated_control, by=c("SBEID", "year"))
worker_panel_full[, treated := treated_firm]
worker_panel_full[, auto_event_time := year - cohort]

# keep only stayer event times
worker_panel_full <- worker_panel_full[auto_event_time %in% stayer_event_time,]

# find stayers
worker_panel_full[, keep := uniqueN(SBEID)==1 & .N==length(stayer_event_time), by=.(workerID, cohort)]
worker_panel_full <- worker_panel_full[keep ==T , !c("keep")]

# fixed term
worker_panel_full[, fixed_term := SCONTRACTSOORT == 2]
# create hwage real and growth
worker_panel_full[, hwage_real := exp(lrhwage)]
worker_panel_full[, hwage_real_growth := lrhwage-lrhwage_1]

# foreign born or foreign born parents
worker_panel_full[, foreign := birthcountry!=6030]

# create summary stat vars at worker level
worker_panel_full[, manager_match := manager]

# create firm panel  out of potential treated and control (return of matching function)
firm_panel_full <- merge(treated_firms, potential_treated_control, by=c("SBEID", "year"))
firm_panel_full[, treated := treated_firm]
firm_panel_full[, auto_event_time := year - cohort]
## keep only relevent years
firm_panel_full <- firm_panel_full[auto_event_time %in% stayer_event_time,]

# mean employment (worker_fte) in pre-treatment period
firm_panel_full[get(event_time_var) %in% min(event_time):matching_event_time, 
           workers_fte_mean := mean(workers_fte),
           by=.(SBEID, cohort)]

# mean workers_fte growth
firm_panel_full[order(SBEID, cohort, year)  &  get(event_time_var) %in% min(event_time):matching_event_time,
           workers_fte_growth := mean(log(workers_fte)-shift(log(workers_fte), n=1, type="lag"), na.rm=T),
           by=.(SBEID, cohort)]

# mean rsales_pw 
firm_panel_full[get(event_time_var) %in% min(event_time):matching_event_time,
           rsales_mean := mean(rsales/workers_fte), by=.(SBEID, cohort)]
# firm_panel_full[order(SBEID, cohort, year)  &  get(event_time_var) %in% min(event_time):matching_event_time, 
#            rsales_pw_growth := mean(log(rsales/workers_fte_mean)-shift(log(rsales/workers_fte_mean), n=1, type="lag"), na.rm=T),
#            by=.(SBEID, cohort)]

# reexport_pw_new (exports per mean worker for exporters)
firm_panel_full[get(event_time_var) %in% min(event_time):matching_event_time,
           rexport_pw_new := fifelse(exporter, mean(rexport/workers_fte_mean), NA_real_),
           by=.(SBEID, cohort)]
firm_panel_full[get(event_time_var) %in% min(event_time):matching_event_time,
           rimport_pw_new := fifelse(importer, mean(rimport/workers_fte_mean), NA_real_),
           by=.(SBEID, cohort)]

# automation expenditures
firm_panel_full[treated==T, auto_cost_percent := auto_cost_share[auto_event_time==0]*100, by=.(SBEID, cohort)]
firm_panel_full[treated==T, automation_cost_real := automation_cost[auto_event_time==0]*factor, by=.(SBEID, cohort)]
firm_panel_full[treated==T, automation_cost_real_pw := real_automation_cost_pw]


# NACE industry labels
firm_panel_full[, NACE_letter_label := fifelse(NACE_letter=="C", "Manufacturing", 
                                          fifelse(NACE_letter=="F", "Construction",
                                                  fifelse(NACE_letter=="G", "Wholesale and Retail Trade",
                                                          fifelse(NACE_letter=="H", "Transportation and Storage",
                                                                  fifelse(NACE_letter=="I", "Accomodation and Food Service", 
                                                                          fifelse(NACE_letter=="J", "Information and Communication",
                                                                                  fifelse(NACE_letter=="M", "Prof., Scientific and Technical Activities",
                                                                                          fifelse(NACE_letter=="N", "Administrative and Support Activities", "MISSING"))))))))]







##################################################################################
#### unmatched data set: extract worker-level panel at auto_event_time == matching_event_time ####
suma_unmatched <- worker_panel_full[auto_event_time == matching_event_time, unique(c("treated", "workerID", "SBEID", "cohort", vars_w)), with=F]
## add all necessary firm-level vars
suma_unmatched <- merge(suma_unmatched, 
                        firm_panel_full[auto_event_time == matching_event_time, unique(c("SBEID", "cohort", "MNE", vars_f)), with=F],
                        by=c("SBEID", "cohort"))


#### append both panels to create one ####
suma_matched[, group_v := fifelse(treated, "matched_treated", "matched_untreated")]
suma_unmatched[, group_v := "unmatched"]
suma_unmatched[, weight_OLS := 1]
suma_unmatched[, clusterID_worker := paste0(workerID, "-", cohort)] # set fake clusterID to trick function

suma <- rbindlist(list(suma_matched, suma_unmatched), fill =T)


##################################################################################
#### create summary statistics tables ####
# unmatched vs. matched control vs. matched treated
summary_overall <- summary_stat_worker_panel(dt=suma, vars=c(vars_worker, vars_firm), 
                                             col_var = "group_v", 
                                             col_var_order = c("unmatched", "matched_untreated", "matched_treated"), 
                                             col_var_dict = c("Sample before matching", "Control workers", "Treated workers"),
                                             categorical_vars = categorical_vars, vars_dict = vars_dict,
                                             strata_var = "clusterID_worker", 
                                             use_weights = T, weight_var = "weight_OLS")

# treated matched MNE vs Domestic

summary_MNE_DOM <- summary_stat_worker_panel(dt=suma_matched[treated==T,], vars=c(vars_worker, vars_firm, vars_firm_only_treated), 
                                             col_var = "MNE", 
                                             col_var_order = c("TRUE", "FALSE"), 
                                             col_var_dict = c("MNE", "Domestic"),
                                             categorical_vars = categorical_vars, vars_dict = vars_dict,
                                             strata_var = "clusterID_worker", 
                                             use_weights = F, weight_var = "weight_OLS")

##################################################################################
##################################################################################
#### save summary statistics tables ####
# main Tables 
print(xtable(summary_overall, caption = "Summary Statistics, before and after matching."),
      caption.placement = "top", size = "\\footnotesize", table.placement = "H", 
      booktabs = T,
      label = "tab:sum_matched",
      file = paste0(map_output_script, "table_summary_matched.tex"))

print(xtable(summary_MNE_DOM, caption = "Summary Statistics, matched sample, treated MNE vs. Domestic workers."),
      caption.placement = "top", size = "\\footnotesize", table.placement = "H", 
      booktabs = T,
      label = "tab:sum_matched_MNE_DOM",
      file = paste0(map_output_script, "table_summary_MNE_DOM_matched.tex"))
