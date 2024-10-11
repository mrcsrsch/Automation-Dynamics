##################################################################################
### Functions to create and analyse matched worker dataset #### 
##################################################################################
# function to cut var x into quantile groups (for coarsened exact matching)
qcut <- function(x, q, unique=F, numbered = F){
  #q <- seq(0,1, length.out=n+1)
  if (length(q)==1) warning("specify prob. vector for quantile cuts to avoid unexpected behaviour")
  cuts <- unname(quantile(x, q, na.rm=T))
  if (unique) cuts <- unique(cuts)
  if (numbered){
    res <- as.character(cut(x, cuts, include.lowest=T, labels = FALSE, dig.lab=10))
  } else{
    res <- as.character(cut(x, cuts, include.lowest=T, dig.lab=10))
  }
  return(res)
}

# function to extract treated and control firms
# input is a panel (dt) of all treated firms
extract.firm.event.data <- function(dt, event_time_var, event_time = -3:4, 
                                    firm_observed_after = T,
                                    firm_type_var = "MNE",
                                    firm_exact_matching_vars = NULL, 
                                    firm_cem_vars=NULL, quantile_probs_firm_cem=NULL,
                                    demean_cem_vars = TRUE,
                                    matching_event_time = -1){ # simple function that creates a stacked and balanced event dataset as described in Bessen et al 2023
  # dt should be the dataset with only treated firms
  # event_time_var is the event time variable, e.g. "time_since_automation"
  # event_time is the event time period that the selection should be balanced on, where 0 is the treatment year
  # firm_observed_after ensures a survival criterium for treated firms (i.e. shortening to number of firms that will be included)
  # firm_type_var is a variable that defines firm types (e.g. MNE vs. non-MNE). It is enforced that firm types are stable across the observation period.
  # If specified, only firms that have the same status of firm_type_var throughout the event_time are considered.
  # firm_exact_matching_vars specifies an additional matching vars, e.g. it can ensure that only cohort-industry combinations with treated and control firms are returned
  # matching_event_time determines the event time on which firm_exact_matching_vars are matched
  
  # create variable that identifies treatment year
  dt[, treatment_year := year[get(event_time_var)==0], by=SBEID]
  
  # Treated firms in event data set
  # find treated firms that fulfill the event time requirements 
  if (firm_observed_after){
    # enforce survival requirement
    # i.e. These are firms observed until 1 year AFTER event time (to ensure survival)
    # (this removes the last possible treatment year from the panel)
    treated_ids <- dt[, sum(get(event_time_var) %in% c(event_time, max(event_time)+1)) == length(event_time)+1, by=SBEID][V1==T, SBEID]
  } else{
    # without survival requirement: continously observed
    treated_ids <- dt[, sum(get(event_time_var) %in% event_time) == length(event_time), by=SBEID][V1==T, SBEID]
  }
  # create event time dataset of treated firms
  treated_event <- dt[SBEID %in% treated_ids & get(event_time_var) %in% event_time, ]; rm(treated_ids)
  # extract treatment years
  treated_years <- treated_event[get(event_time_var)==0, unique(treatment_year)]
  
  # loop through treated years and find later treated firms as controls and store in list
  control_event <- list()
  i <- 0
  for (y in treated_years){
    i <- i+1 # update i 
    # extract later treated firms (after event_time of y)
    control_event[[i]] <- dt[treatment_year>=y+max(event_time)+1,]
    # keep same years as treated years for treatment year y
    control_event[[i]] <- control_event[[i]][year %in% treated_event[treatment_year==y, unique(year)], ]
    # keep control firm only if observed in all years
    control_event[[i]][, keep := .N == length(event_time), by=SBEID]
    control_event[[i]] <- control_event[[i]][keep==T, !c("keep")]
    # adjust variables
    control_event[[i]][, c("treatment_year_control", "treated_firm", event_time_var, "treatment_year", "cohort", "SBEID_new") := .(treatment_year, F, year-y, NA_integer_, y, paste0(SBEID, "-", y))]
  }
  rm(treated_years, i , y)
  # combine list
  control_event <- rbindlist(control_event)
  # create vars in treated_event
  treated_event[, c("treatment_year_control", "treated_firm", "cohort", "SBEID_new") := .(NA_integer_, T, treatment_year, paste0(SBEID, "-", treatment_year))]
  # combine treated and controls
  event_data <- rbindlist(list(treated_event, control_event)); rm(treated_event, control_event)
  event_data[, treatment_year := NULL]
  
  # if specified, select firms with a unique firm type and set an identifier variable
  if (length(firm_type_var)!=0){
    if (length(firm_type_var)>1) stop("Only one firm_type_var allowed.")
    if (!(firm_type_var %in% names(event_data))) stop(paste(firm_type_var, "not in data."))
    
    # enforce unique type and set id var
    event_data <- event_data[SBEID_new %in% event_data[, uniqueN(get(firm_type_var))==1, by="SBEID_new"][V1==1, SBEID_new],]
    event_data[, paste0(firm_type_var, "_group") := get(firm_type_var)]
  }
  
  # store data.table of potentially treated and control firms
  potential_treated_control <- copy(event_data[, c("SBEID", "year", "cohort", "treated_firm")])
  
  ##### Coarsened exact matching at firm-level ######
  # extract treated and control at matching_event_time
  matching_firm <- event_data[get(event_time_var) == matching_event_time, ]
  
  # if desired, demean firm_cem_vars by firm_exact_matching_vars
  if (length(firm_cem_vars)!=0 & demean_cem_vars == T){
    for (o in 1:length(firm_cem_vars)){
      #form <- as.formula(paste0(firm_cem_vars[o], "~ 0 | year + ", paste0(firm_exact_matching_vars, collapse = "+")))
      form <- as.formula(paste0(firm_cem_vars[o], "~ 0 | ", paste0(c(firm_exact_matching_vars), collapse = "^"), "^year"))
      
      dem <- feols(form, data=matching_firm[!is.na(get(firm_cem_vars[o])),])
      matching_firm[!is.na(get(firm_cem_vars[o])), paste0(firm_cem_vars[o]) := dem$residuals]
      rm(dem)
    }
  }
  
  # if specified, create coarsened exact matching vars (by distribution)
  if (length(firm_cem_vars)!=0){
    
    matching_firm[, paste0(firm_cem_vars, "_group") := lapply(.SD, function(x) qcut(x, quantile_probs_firm_cem, unique=T)), .SDcols = firm_cem_vars]
    firm_exact_matching_vars <- c(firm_exact_matching_vars, paste0(firm_cem_vars, "_group"))
    
    # for (o in 1:length(firm_cem_vars)){
    #   matching_firm[, paste0(firm_cem_vars[o], "_group") := qcut(get(firm_cem_vars[o]), q=quantile_probs_firm_cem, unique=T)]
    #   ## add to exact matching vars 
    #   firm_exact_matching_vars <- c(firm_exact_matching_vars, paste0(firm_cem_vars[o], "_group"))
    # }
  }
  
  # perform matching
  # add cohort to matching vars
  firm_exact_matching_vars <- c("cohort", firm_exact_matching_vars)
  
  ## only keep firm_exact_matching_vars combinations with both treated and control firms
  matching_firm <- merge(matching_firm, matching_firm[, any(treated_firm) & any(!treated_firm), 
                                                      by=firm_exact_matching_vars][V1==T, !c("V1")], by=firm_exact_matching_vars)
  
  ## create a matched cluster ID at firm level
  #event_data[, .N, by=group_var][, clusterID_firm := 1:.N]
  ids <- unique(matching_firm[, firm_exact_matching_vars, with=F])
  ids[, clusterID_firm := 1:.N]
  matching_firm <- merge(matching_firm, ids, by=firm_exact_matching_vars)
  
  ## create a numeric firmID for SBEID-clusterID_firm interaction (as control firms can be observed multiple times)
  ids <- unique(matching_firm[, c("SBEID", "clusterID_firm")])
  ids[, SBEID_cluster := 1:.N]
  matching_firm <- merge(matching_firm, ids, by=c("SBEID", "clusterID_firm"))
  ########################################################################
  # add back to event_data and return full selected panel
  event_data <- merge(event_data, matching_firm[, c("SBEID_new", "clusterID_firm", "SBEID_cluster")], by=c("SBEID_new"))
  
  # return dataset
  vars <- c("clusterID_firm", "SBEID_cluster", "SBEID", event_time_var, "year", "treated_firm", "cohort")
  return(list(event_data = event_data[, vars, with=F], potential_treated_control = potential_treated_control))
}



# function that creates the worker-level event data. It calls the functions above.
create.worker.event.data <- function(firm_panel, POLIS_panel, 
                                     event_time_var, event_time = -3:3, 
                                     matching_event_time, stayer_event_time = -3:-1, 
                                     firm_observed_after = T, 
                                     firm_type_var = "MNE",
                                     firm_exact_matching_vars,
                                     firm_cem_vars=NULL, quantile_probs_firm_cem=NULL,
                                     worker_exact_matching_vars, 
                                     worker_cem_vars_1, quantile_probs_worker_cem_1, 
                                     worker_cem_vars_2 = NULL, quantile_probs_worker_cem_2, 
                                     demean_cem_vars = TRUE,
                                     const_vars_POLIS_panel = NULL,
                                     const_vars_firm_panel){
  # extract treated and control firms (separately per class)
  firm_level <- extract.firm.event.data(dt=firm_panel, event_time_var=event_time_var, event_time = event_time, 
                                   firm_observed_after = firm_observed_after,
                                   firm_type_var = firm_type_var,
                                   firm_exact_matching_vars = firm_exact_matching_vars, 
                                   firm_cem_vars=firm_cem_vars, quantile_probs_firm_cem=quantile_probs_firm_cem,
                                   matching_event_time = matching_event_time)
  
  # extract matched firms
  firms <- firm_level$event_data
  
  ##### create panel of treated and potential control workers #####
  ## subset POLIS-panel to workers observed in treated or control firm at pre-treatment year
  tt <- firms[get(event_time_var)==matching_event_time, .(SBEID, SBEID_cluster, year, cohort, treated = treated_firm, clusterID_firm)]
  
  ### extract workers and add duplicates of workers
  workers <- merge(POLIS_panel[, c("workerID", "SBEID", "year")], tt, by=c("SBEID", "year")) # get workers
  ### subset POLIS_panel to potential treated and control workers
  POLIS_panel <- POLIS_panel[workerID %in% workers[, unique(workerID)], ] # subset POLIS_panel
  # create worker level panel that includes all potential control and treated workers; and all their SBEID matches over time
  ## since control workers can serve as control to multiple treated workers, we duplicate all their observations here
  vars <- c("workerID", "cohort", "clusterID_firm", "SBEID_cluster", "treated")
  POLIS_panel <- merge(POLIS_panel, workers[, vars, with=F], by=c("workerID"), allow.cartesian = T)
  # only keep worker observations in event time frame
  ## create event_time_var
  POLIS_panel[, paste0(event_time_var) := year - cohort]
  POLIS_panel <- POLIS_panel[get(event_time_var) %in% event_time, ]
  
  ##### we focus on stayers  - observed at same SBEID during stayer_event_time ######
  POLIS_panel[, keep := sum(get(event_time_var) %in% stayer_event_time)==length(stayer_event_time) & 
                uniqueN(SBEID[get(event_time_var) %in% stayer_event_time])==1, by=c("workerID", "cohort")]
  POLIS_panel <- POLIS_panel[keep==T, !c("keep")]
  
  
  ##### Coarsened exact matching at worker-level ######
  ## extract cross-section at event_time_var==matching_event_time
  POLIS_pre <- POLIS_panel[get(event_time_var)==matching_event_time, ]
  
  ## if desired, demean worker_cem_vars_1 & worker_cem_vars_2 by worker_exact_matching_vars AND firm_exact_matching_vars
  worker_cem_vars <- c(worker_cem_vars_1, worker_cem_vars_2)
  if (length(worker_cem_vars)!=0 & demean_cem_vars == T){
    # add firm_exact_matching_vars to POLIS_pre
    if (any(names(POLIS_pre) %in% firm_exact_matching_vars)) POLIS_pre[, paste0(firm_exact_matching_vars) := NULL]
    POLIS_pre <- merge(POLIS_pre, firm_panel[, c("SBEID", "year", firm_exact_matching_vars), with=F], by=c("SBEID", "year"))
    
    for (o in 1:length(worker_cem_vars)){
      #form <- as.formula(paste0(worker_cem_vars[o], "~ 0 | year + ", paste0(worker_exact_matching_vars, collapse = "+"),
      #                          "+", paste0(firm_exact_matching_vars, collapse = "+")))
      # by interacted fixed effects
      #form <- as.formula(paste0(worker_cem_vars[o], "~ 0 | ", paste0(c(worker_exact_matching_vars, firm_exact_matching_vars), collapse = "^"), "^year"))
      
      # within firm clusters and worker exact matching var clusters
      form <- as.formula(paste0(worker_cem_vars[o], "~ 0 | ", paste0(c(worker_exact_matching_vars, "clusterID_firm"), collapse = "^"), "^year"))
      dem <- feols(form, data=POLIS_pre[!is.na(get(worker_cem_vars[o])),])
      POLIS_pre[!is.na(get(worker_cem_vars[o])), paste0(worker_cem_vars[o]) := dem$residuals]
      rm(dem)
    }
  }
  
  # paste0(paste0(c(worker_exact_matching_vars, firm_exact_matching_vars), collapse = "^"), "^year")
  
  ## if specified, create coarsened exact matching vars (by distribution)
  if (length(worker_cem_vars_1)!=0){
    POLIS_pre[, paste0(worker_cem_vars_1, "_group") := lapply(.SD, function(x) qcut(x, quantile_probs_worker_cem_1, unique=T)), .SDcols = worker_cem_vars_1]
    worker_exact_matching_vars <- c(worker_exact_matching_vars, paste0(worker_cem_vars_1, "_group"))
  }
  
  if (length(worker_cem_vars_2)!=0){
    POLIS_pre[, paste0(worker_cem_vars_2, "_group") := lapply(.SD, function(x) qcut(x, quantile_probs_worker_cem_2, unique=T)), .SDcols = worker_cem_vars_2]
    worker_exact_matching_vars <- c(worker_exact_matching_vars, paste0(worker_cem_vars_2, "_group"))
  }
  
  ## add clusterID_firm to matching vars (this ensures matching on firm_exact_matching_vars)
  worker_exact_matching_vars <- c(worker_exact_matching_vars, "clusterID_firm")
  
  ## add cohort to matching vars
  worker_exact_matching_vars <- c("cohort", worker_exact_matching_vars)
  
  ## perform matching: i.e. extract groups with both treated and untreated observations
  tt <- POLIS_pre[, any(treated) & any(!treated), 
                  by=worker_exact_matching_vars][V1==T, !c("V1")]
  tt[, clusterID_worker := 1:.N] # create a cluster ID
  POLIS_pre <- merge(POLIS_pre, tt, by=worker_exact_matching_vars)
  ### subset POLIS_panel to matches and add clusterID_worker
  POLIS_panel <- merge(POLIS_panel, POLIS_pre[, c("workerID", "cohort", "clusterID_worker")], by=c("workerID", "cohort"))
  
  ###### create panel vars  #######
  POLIS_panel[, paste0(event_time_var, "_treated") := fifelse(treated, get(event_time_var), 999)] # event_time interacted with treated
  
  # add se-cluster var (firm at auto_event_time==-1)
  POLIS_panel[, SBEID_cluster := SBEID_cluster[get(event_time_var)==matching_event_time], by=c("workerID", "cohort")] # firmID before event
  
  # if const_vars_firm_panel is specificied, add vars from firm_panel data
  if (length(const_vars_firm_panel)!=0){
    # delete vars in POLIS_panel if necesasry
    if (any(const_vars_firm_panel %in% names(POLIS_panel))) POLIS_panel[, paste(const_vars_firm_panel) := NULL]
    POLIS_panel <- merge(POLIS_panel, firm_panel[, c("SBEID", "year", const_vars_firm_panel), with=F], by=c("SBEID","year"), all.x=T)
    # add vars to const_vars_POLIS_panel
    const_vars_POLIS_panel <- c(const_vars_POLIS_panel, const_vars_firm_panel)
  }
  
  # if const_vars is specified, create consistent based on matching_event_time
  POLIS_panel[, paste0(const_vars_POLIS_panel, "_match") := lapply(.SD, function(x) x[get(event_time_var)==matching_event_time]), by=c("workerID", "cohort"), .SDcols=const_vars_POLIS_panel]
  
  #### identify stayer dummies
  setorderv(POLIS_panel, c("workerID", "cohort", "year"))
  #POLIS_panel[, stayer := SBEID==SBEID[get(event_time_var)==matching_event_time] & cumsum(c(0,diff(SBEID)))==0, by=c("workerID", "cohort")]
  POLIS_panel[, stayer := SBEID==SBEID[get(event_time_var)==matching_event_time] & cumsum(SBEID - SBEID[get(event_time_var)==matching_event_time])==0, by=c("workerID", "cohort")]
  
  
  # identify complete stayers
  POLIS_panel[, stayer_complete := sum(stayer)==length(event_time), by=c("workerID", "cohort")]
  
  
  #### identify dummy for being at same firm as at matching_event_time 
  setorderv(POLIS_panel, c("workerID", "cohort", "year"))
  POLIS_panel[, same_firm := SBEID==SBEID[get(event_time_var)==matching_event_time], by=c("workerID", "cohort")]
  
  
  
  # #### identify alternative stayer dummy --> true if workers is also in firm next year ##### 
  # if (firm_observed_after==T){
  #   # subset POLIS_panel_copy (full employment histories) to selected treated and control workers
  #   # also create duplicates along the way
  #   POLIS_panel_copy <- merge(POLIS_panel_copy[, c("workerID", "SBEID", "year")], unique(POLIS_panel[, c("workerID", "cohort")]), by="workerID", allow.cartesian = T)
  #   # recreate event_time_var
  #   POLIS_panel_copy[, paste0(event_time_var) := year - cohort]
  #   
  #   # identify stayer2 variable (FALSE in last year at treated and control firm and thereafter)
  #   setorderv(POLIS_panel_copy, c("workerID", "year"))
  #   POLIS_panel_copy[, stayer2 := SBEID == shift(SBEID, n=1, type="lead") & SBEID == SBEID[get(event_time_var)==matching_event_time], by=workerID]
  #   # where stayer2 is mising, set it to FALSE (since we know that the firm exists another year but the worker is not observed there)
  #   POLIS_panel_copy[is.na(stayer2), stayer2 := F]
  #   
  #   # identify leaving observation from treated or control firm
  #   POLIS_panel_copy[, exit := stayer2==F  & SBEID == SBEID[get(event_time_var)==matching_event_time], by=workerID]
  #   POLIS_panel_copy[, exit2 := fifelse(SBEID==SBEID[get(event_time_var)==matching_event_time], exit, NA), by=workerID]
  #   
  #   # add to POLIS_panel
  #   POLIS_panel <- merge(POLIS_panel, 
  #                        POLIS_panel_copy[, c("workerID", "cohort", "year", "stayer2", "exit", "exit2")], 
  #                        by=c("workerID", "year", "cohort"))
  # }
  
  # NOTE !stayer is the equivalent to Bessen leaver. (TRUE while worker in firm, FALSE thereafter)
  # !stayer2 is a better equivalent of Bessen leaver (FALSE in last year at firm and thereafter)
  # Bessen leaver2 (for hazard rate) is FALSE at all obs within firm. It is TRUE at first obs outside firm and MISSING thereafter
  # This is somewhat similar to exit (FALSE until last obs in firm, if leaving then TRUE), and exit2 which is MISSING thereafter

  # return result worker panel 
  return(list(worker_panel = POLIS_panel, potential_treated_control = firm_level$potential_treated_control))
}

### function that creates a balanced panel ####
create.balanced.panel <- function(matched_worker_panel, event_time_var, event_time, expand_vars_const = NULL, matching_event_time, firm_type_var = NULL){
  # create perfectly balanced panel based on matched worker data (i.e. return of create.worker.event.data())
  tt <- matched_worker_panel[, .(event_time_var=event_time), by=c("workerID", "cohort")]
  setnames(tt,"event_time_var", event_time_var)
  matched_worker_panel <- merge(tt, matched_worker_panel, by=c("workerID", "cohort", event_time_var), all.x=T)
  # expand stayer
  matched_worker_panel[is.na(stayer), stayer := F]
  # expand same_firm (worker at same firm as at matching_event_time)
  matched_worker_panel[is.na(same_firm), same_firm := F]
  
  # if it exists also expand stayer2 and exit (note that exit2 should remain missing when worker has left firm)
  if (any(names(matched_worker_panel)=="stayer2")) matched_worker_panel[is.na(stayer2), stayer2 := F]
  if (any(names(matched_worker_panel)=="exit")) matched_worker_panel[is.na(exit), exit := F]
  
  rm(tt)
  
  # expand variables 
  matched_worker_panel[, year := get(event_time_var)+cohort]
  matched_worker_panel[, treated := as.logical(max(treated, na.rm=T)), .(workerID, cohort)]
  matched_worker_panel[, age := age[get(event_time_var)==matching_event_time]+get(event_time_var)+1, by=c("workerID", "cohort")]
  
  if (length(expand_vars_const)!=0){
    for (v in 1:length(expand_vars_const)){
      matched_worker_panel[, paste0(expand_vars_const[v]) := get(expand_vars_const[v])[get(event_time_var)==matching_event_time], by=c("workerID", "cohort")]
    }
  }
  
  
  # extract const_vars from worker panel and expand them 
  if (any(grepl("_match$", names(matched_worker_panel)))){
    const_vars <- names(matched_worker_panel)[grep("_match$", names(matched_worker_panel))]
    for (v in 1:length(const_vars)){
      matched_worker_panel[, paste0(const_vars[v]) := get(const_vars[v])[get(event_time_var)==matching_event_time], by=c("workerID", "cohort")]
    }
  }
  
  # event_time_var * treated
  matched_worker_panel[, paste0(event_time_var, "_treated") := fifelse(treated, get(event_time_var), 999)] # event_time interacted with treated
  
  # add se-cluster var (firm at auto_event_time==-1)
  matched_worker_panel[, SBEID_cluster := SBEID_cluster[get(event_time_var)==matching_event_time], by=c("workerID", "cohort")] # firmID before event
  
  
  if (length(firm_type_var)!=0){
    matched_worker_panel[, paste0(firm_type_var, "_group_match") := get(paste0(firm_type_var, "_group"))[get(event_time_var)==matching_event_time], by=c("workerID", "cohort")] # matched firm type before event
    
  }
  
  return(matched_worker_panel)
}


### function that estimates DiD without creating plots. Split_var allows to interact treatment. ###
estimates.simple <- function(dt, y = "stayer", event_time_var = "auto_event_time",
                             split_var = "NACE_letter_match",
                             add_fe_split_var = F, 
                             ref_event_time, 
                             controls = controls_stayer, fes = fixefs_stayer,
                             cluster_vars = c("SBEID_cluster"),
                             create.OLS.weights = T, weight_group = "clusterID_worker", 
                             dummy_scaling_var = NULL){
  
  # if weighted OLS is wanted, then create weights here
  if (create.OLS.weights){
    dt[, weight_OLS := fifelse(treated, 1, sum(treated)/sum(!treated)), by=c(weight_group, event_time_var)]
    
    w <-  as.formula(paste0("~", "weight_OLS"))
    
  } else{
    w <- 1
  }
  
  
  # create split i() vars in dataset
  ## set event_time_var to ref_event_time for non-treated
  dt[, "event_time" := fifelse(treated, get(event_time_var), ref_event_time)]
  
  # if split var is specified, create splot with split. Otherwise simple
  if (length(split_var)!=0){
    slope_1 <- paste0("+ i(event_time, i.", split_var, ", ref=c(\"", ref_event_time, "\"))")
  } else{
    slope_1 <- paste0("+ i(event_time, ref=c(\"", ref_event_time, "\"))")
  }
  
  # if add_fe_split_var is wanted, add fe to set of fixed effects
  if (add_fe_split_var & length(split_var)!=0){
    fes <- paste0(fes, paste0("+ ", split_var, "^", event_time_var))
  }
  
  # add simple weights to account for differences in auto_cost_shares between MNEs and domestic firms
  if (length(dummy_scaling_var)!=0){
    dt[treated==T, weight := get(dummy_scaling_var)]
    dt[treated==F, weight := 0]
    
    slope_1 <- paste0(slope_1, ":weight")
  }
  
  
  ## create formulas 
  form <- as.formula(paste0(y,  "~", slope_1, controls, "|", fes))
  
  # estimate coefficients
  reg <- feols(form, cluster=cluster_vars, data=dt, weights=w, nthreads = 7)
  
  return(reg)
}

# function around estimates.simple
# allows to split a variable and compare within MNE and Domestic firm subsamples
subsample.analysis.by.group <- function(dt,
                                        splitting_var = "MNE_group",
                                        subset_var = "educ_level_match",
                                        add_fe_subset_var = F,
                                        exclude_levels = NULL, 
                                        legend_title = "Education level",
                                        levels_dict = NULL,
                                        matching_cluster_var = "clusterID_worker",
                                        y = "stayer", event_time_var = "auto_event_time", 
                                        ref_event_time, 
                                        controls = controls_stayer, fes = fixefs_stayer, cluster_vars = c("SBEID_cluster"),
                                        create.OLS.weights = T, weight_group = "clusterID_worker", 
                                        dummy_scaling_var = NULL,
                                        create.plots = T, plot_title = "", ci_level = 0.95){
  
  # if add_fe_split_var is wanted, add fe to set of fixed effects
  if (add_fe_subset_var & length(subset_var)!=0){
    fes <- paste0(fes, paste0("+ ", subset_var, "^", event_time_var))
  }
  
  
  # split dataset by grouping_var
  types <- dt[, unique(get(splitting_var))]
  
  # create list per type 
  # Note that there can only be two types
  regs <- vector("list", length(types))
  names(regs) <- types
  
  plots <- vector("list", length(types))
  names(plots) <- types
  
  for (type in types){ # loop over types
    # include only treated of current type
    dt[, include := (get(splitting_var)==type & treated==T) | (treated == F)]
    dt[include==T, include := any(treated) & any(!treated), by=c(matching_cluster_var, event_time_var)]
    # extract data
    dt_current <- dt[include==T, !c("include")]
    
    # loop over possible subsets
    subsets <- dt_current[, unique(get(subset_var))]
    # exclude unwanted subsets
    if (length(exclude_levels)!=0){
      subsets <- subsets[!(subsets %in% exclude_levels)]
    }
    
    k <- 0 # position in list
    for (subset in subsets){ # loop over subsets
      k <- k+1
      # identify subsample to use
      dt_current[, include := (get(subset_var)==subset & treated==T) | (treated == F)]
      # subset to years with observed treated and control workers
      dt_current[include==T, include := any(treated) & any(!treated), by=c(matching_cluster_var, event_time_var)]
      
      # run regression and store in list
      regs[[type]][[k]] <-   estimates.simple(dt_current[include==T,], y = y, event_time_var = event_time_var,
                                              split_var = NULL,
                                              add_fe_split_var = F, 
                                              ref_event_time = ref_event_time, 
                                              controls = controls, fes = fes,
                                              cluster_vars = cluster_vars,
                                              create.OLS.weights = create.OLS.weights, weight_group = weight_group, 
                                              dummy_scaling_var = dummy_scaling_var)
      
      # reset
      dt_current[, include := NULL]
    }
    
    # add subsets as names
    # replace by dict if specified
    if (length(levels_dict)!=0){
      replace <- (subsets %in% levels_dict)
      nn <- subsets
      nn[replace] <- names(levels_dict)[match(nn[replace], levels_dict)] # replace where specified
      names(regs[[type]]) <- nn
    }else{
      names(regs[[type]]) <- subsets
    }
    
    # create plot if desired
    if (create.plots){
      # create plots, list names are in legend
      plots[[type]] <- ggiplot(regs[[type]], ci_level = ci_level,
                               pt.join=T, ref.line = 0, main = plot_title, geom_style = 'errorbar', theme = theme_bw()) + #geom = 'errorbar'
        scale_x_continuous(breaks = dt[, min(get(event_time_var))]:dt[, max(get(event_time_var))]) + xlab("Years since automation event") + 
        theme(legend.position = "bottom", panel.grid = element_blank(), panel.border = element_rect(colour = "black", size = 1.1, fill = NA)) +
        scale_colour_grey(end = 0.3, start = 0.7) +
        scale_fill_grey(end = 0.3, start = 0.7) 
      
      # adjust legend title
      plots[[type]]$labels$colour <- plots[[type]]$labels$shape <- plots[[type]]$labels$fill <- ifelse(length(legend_title)==0, subset_var, legend_title)
      
    }
    
    # reset
    dt[, include := NULL]
  }
  
  # return result
  return(list(regs = regs, plots = plots))
}



# this function allows for subsample analysis that compares MNE vs. Domestic firms
# for a specific level of a specific var
subsample.analysis.by.level <- function(dt,
                                        subset_var = "educ_level_match",
                                        subset_level = 1,
                                        matching_cluster_var = "clusterID_worker",
                                        y = "stayer", event_time_var = "auto_event_time", MNE_grouping_var = "MNE_group",
                                        ref_event_time, 
                                        controls = controls_stayer, fes = fixefs_stayer, cluster_vars = c("SBEID_cluster"),
                                        diff_regs = T, diff_ref = "Domestic",
                                        create.plots = T, plot_title = "", ci_level = 0.95, 
                                        create.OLS.weights = T, weight_group = "clusterID_worker", 
                                        dummy_scaling_var = NULL){
  
  # identify subsample to use
  dt[, include := (get(subset_var)==subset_level & treated==T) | (treated == F)]
  # subset to years with observed treated and control workers
  dt[include==T, include := any(treated) & any(!treated), by=c(matching_cluster_var, event_time_var)]
  
  
  # calculate DiD and plots using other function
  result <- plots.estimates(dt=dt[include==T,], y = y, event_time_var = event_time_var, MNE_grouping_var = MNE_grouping_var,
                            ref_event_time = ref_event_time, 
                            controls = controls, fes = fes, cluster_vars = cluster_vars,
                            diff_regs = diff_regs, diff_ref = diff_ref,
                            create.plots = create.plots, plot_title = plot_title, ci_level = ci_level, 
                            create.OLS.weights = create.OLS.weights, weight_group = weight_group, 
                            dummy_scaling_var = dummy_scaling_var, dummy_scaling_post_only = FALSE)
  
  # return result
  return(result)
}


### function to great plots with domestic / MNE split ###
### function to great plots with domestic / MNE split ###
plots.estimates <- function(dt, y = "stayer", event_time_var = "auto_event_time", MNE_grouping_var = "MNE_group",
                            ref_event_time, 
                            controls = controls_stayer, fes = fixefs_stayer, cluster_vars = c("SBEID_cluster"),
                            diff_regs = F, diff_ref = "Domestic",
                            create.plots = T, plot_title = "", ci_level = 0.95, 
                            create.OLS.weights = T, weight_group = "clusterID_worker", 
                            dummy_scaling_var = NULL, dummy_scaling_post_only = FALSE){
  # This function plots the estimates of the analysis, split up by MNEs and domestic firms, as defined in XXX.
  # dt is the data.table
  # y is the dependent variable
  # ref_event_time is the reference moment
  # controls is a formula part for control variables, e.g. "+ age"
  # fes are the fixed effects, e.g. "workerID^cohort + SBEID^cohort"
  # plot_title is the title of the plot. If plots_title = "", no title is added
  # ci_level is the level of the confidence intervals in the plots
  # 
  # the ggiplot function does not easily allow to depict multiple i() interactions from the same fixest object in the same plot
  # here I exploit that ggiplot uses i.select=1 by default (it selects the first i() interaction and depicts it)
  # I create to fixest objects with different ordering and then use the multiplot functionality of ggiplot to depict them in the same plot
  
  # if weighted OLS is wanted, then create weights here
  if (create.OLS.weights){
    dt[, weight_OLS := fifelse(treated, 1, sum(treated)/sum(!treated)), by=c(weight_group, event_time_var)]
    
    w <-  as.formula(paste0("~", "weight_OLS"))
    
  }  else{
    w <- 1
  }
  
  
  
  # create split i() vars in dataset
  ## set event_time_var to ref_event_time for non-treated
  dt[, "event_time" := fifelse(treated, get(event_time_var), ref_event_time)]
  # loop through MNE grouping and create separate event time vars
  types <- dt[, unique(get(MNE_grouping_var))] 
  if (length(types) > 2 ) stop("MNE_grouping_var can only have two levels.")
  for (g in types){
    dt[, paste0("event_time_",g)  := fifelse(get(MNE_grouping_var)==g, event_time, ref_event_time)]
  }
  
  # create formulas with ordering variation
  slope_1 <- paste0("+ i(", paste0("event_time_",types[1]), ", ref=c(\"", ref_event_time, "\"))")
  slope_2 <- paste0("+ i(", paste0("event_time_",types[2]), ", ref=c(\"", ref_event_time, "\"))")
  
  
  # add weights to account for differences in expenditures between MNEs and domestic firms
  if (length(dummy_scaling_var)!=0){
    if (dummy_scaling_post_only){ # only for post coefficients
      dt[treated==T, weight := fifelse(get(event_time_var) >= 0, get(dummy_scaling_var), 1)]
    } else{
      dt[treated==T, weight := get(dummy_scaling_var)]
    }
    dt[treated==F, weight := 0]
    
    slope_1 <- paste0(slope_1, ":weight")
    slope_2 <- paste0(slope_2, ":weight")
  }
  
  ## create formulas 
  formulas <- list()
  formulas[[1]] <- as.formula(paste0(y,  "~", slope_1, slope_2, controls, "|", fes))
  formulas[[2]] <- as.formula(paste0(y,  "~", slope_2, slope_1, controls, "|", fes))
  
  ## if difference reg is desired, create formula
  if (diff_regs){
    slope_3 <- gsub(paste0("_", diff_ref), "", slope_1)
    slope_4 <- gsub(paste0("_", diff_ref), "", slope_2)
    formulas[[3]] <- as.formula(paste0(y,  "~", slope_3, slope_4, controls, "|", fes))
  }
  
  # estimate coefficients
  ## without subset 
  regs_1 <- feols(formulas[[1]], cluster=cluster_vars, data=dt, weights=w, nthreads = 7)
  
  if (create.plots){
    # if plot is wanted, run with variation in coefficient order
    regs_2 <- feols(formulas[[2]], cluster=cluster_vars, data=dt, weights=w, nthreads = 7) 
  }
  
  # run diff regs if they are required
  if (diff_regs){
    diff_regs_l <- feols(formulas[[3]], cluster=cluster_vars, data=dt, weights=w)
  } else{
    diff_regs_l <- NULL 
  }

  if (create.plots){
    # create plots
    # create plot list with correct names
    plot_list <- list(regs_1, regs_2)
    names(plot_list) <- types
    
    # create plots, plot_list names are in legend
    plots <- ggiplot(plot_list, ci_level = ci_level,
                          pt.join=T, ref.line = 0, main = plot_title, geom_style = 'errorbar', theme = theme_bw()) + #geom = 'ribbon'
      scale_x_continuous(breaks = dt[, min(get(event_time_var))]:dt[, max(get(event_time_var))]) + xlab("Years since automation event") + 
      theme(legend.position = "bottom", panel.grid = element_blank(), legend.title = element_blank(), 
            panel.border = element_rect(colour = "black", size = 1.1, fill = NA)) +
      scale_colour_grey(end = 0.3, start = 0.7) +
      scale_fill_grey(end = 0.3, start = 0.7) 
  } else{
    plots <- NULL
  }
  
  
  return(list(regs = regs_1, plots = plots, diff_regs = diff_regs_l))
}

### function to great seperate plots of heterogeneity analysis for domestic / MNE split ###
plots.estimates.hetero <- function(dt, y = "stayer",
                                   grouping_var = "MNE_group",
                                   hetero_var = "educ_level_match",
                                   exclude_levels = c(-1),
                                   levels_dict = NULL,
                                   event_time_var = "auto_event_time", 
                                   ref_event_time, 
                                   dummy_scaling_var = NULL,
                                   controls = controls_stayer, fes = fixefs_stayer, 
                                   add_fe_split = T,
                                   cluster_vars = c("SBEID_cluster"),
                                   create.OLS.weights = T, weight_group = "clusterID_worker",
                                   diff_regs = F, diff_ref = "Domestic",
                                   create.plots = T, plot_title = "", ci_level = 0.95, i_select = 1){
  require(data.table)
  require(fixest)
  require(ggplot2)
  require(ggfixest)
  
  # This function plots heterogeneity estimates (defined by hetero_var) for first-level groupings defined by grouping_var.
  # It is an inefficient function as it runs the same regression length(unique(grouping_var))*length(unique(hetero_var)) times.
  # This is done to exploit ggiplot's ordering and create plots of interactions between three factors.
  # Hence, ignoring multidimensional developments.
  
  # Note: diff_regs currently not implemented
  
  # dt is the data.table
  # y is the dependent variable
  # event_time_var is the event time variable
  # grouping_var is the first level grouping variable, e.g. "MNE" and "Domestic"
  # hetero_var is the second level grouping variale, e.g. Education level
  # exclude_levels is a vector that allows to exclude certain levels of hetero_var from being plotted. They still enter the estimation. 
  # levels_dict determines the names put in the legend for each level of hetero_var
  # ref_event_time is the reference moment for the DiD comparison
  # controls is a formula part for control variables, e.g. "+ age"
  # fes are the fixed effects, e.g. "workerID^cohort + SBEID^cohort"
  # option add_fe_split determines wheter a fixed event of the interaction is added, i.e. event_time_var^grouping_var^hetero_var
  # cluster_vars determines the clustering that is used for standard errors. It accepts two-way clustering.
  
  # create.OLS.weights determines whether weighted OLS should be used.
  # weight_group determines the variable used to create weights for treated and control observation (defined by a variable in dt called "treated")
  
  # diff_regs is currently not implemented.
  
  # plot_title is the title of the plot. If plots_title = "", no title is added
  # ci_level is the level of the confidence intervals in the plots
  # i_select is the i.select argument in ggiplot
  
  # Rational of the function:
  # the ggiplot function does not easily allow to depict multiple i() interactions from the same fixest object in the same plot
  # here I exploit that ggiplot uses i.select=1 by default (it selects the first i() interaction and depicts it)
  # I create to fixest objects with different ordering and then use the multiplot functionality of ggiplot to depict them in the same plot
  
  # if add_fe_split_var is wanted, add fe to set of fixed effects
  if (add_fe_split){
    fes <- paste0(fes, paste0("+ ", grouping_var, "^", hetero_var))
  }
  
  
  # if weighted OLS is wanted, then create weights here
  if (create.OLS.weights){
    dt[, weight_OLS := fifelse(treated, 1, sum(treated)/sum(!treated)), by=c(weight_group, event_time_var)]
    
    w <-  as.formula(paste0("~", "weight_OLS"))
    
  }  else{
    w <- 1
  }
  
  
  
  # create split i() vars in dataset
  ## set event_time_var to ref_event_time for non-treated
  dt[, "event_time" := fifelse(treated, get(event_time_var), ref_event_time)]
  
  # loop through grouping_var grouping and hetero_var to create separate event time vars
  grouping_var_types <- dt[, unique(get(grouping_var))] 
  
  # find subgroups
  hetero_var_types <- dt[, unique(get(hetero_var))] 
  
  # store names of variables 
  var_names <- vector(mode = "list", length(grouping_var_types))
  names(var_names) <- grouping_var_types
  for (g in grouping_var_types){
    
    
    # create empty vector for var names
    var_names[[g]]  <- c()
    i <- 0
    
    # loop through subgroups
    for (h in hetero_var_types){
      i <- i+1
      dt[, paste0("event_time_",g,"_",h)  := fifelse(get(grouping_var)==g & get(hetero_var)==h, event_time, ref_event_time)]
      var_names[[g]][i] <- paste0("event_time_",g,"_",h)
    }
  }
  
  # add dummy_scaling_var weight to data if requested
  if (length(dummy_scaling_var)!=0){
    dt[treated==T, weight := get(dummy_scaling_var)]
    dt[treated==F, weight := 0]
  }
  
  ## run regressions and create plots
  regs <- vector(mode = "list", length(grouping_var_types))
  names(regs) <- grouping_var_types
  
  plts <- vector(mode = "list", length(grouping_var_types))
  names(plts) <- grouping_var_types
  
  # store all var names
  others <- unlist(var_names)
  
  for (g in grouping_var_types){
    regs[[g]] <- vector(mode = "list", length(var_names[[g]]))
    
    for (i in 1:length(var_names[[g]])){
      # exclude unwanted subsets, i.e. don't run separate regression and leave list empty
      # below I take care of empty list items
      if (length(exclude_levels)!=0){
        if (var_names[[g]][i] %in% paste0("event_time_",g,"_",exclude_levels)){
          next
        }
      }
      
      
      # other treatments to consider in current run
      others_here <- others[-which(others==var_names[[g]][i])]
      
      # if scaling is wanted, add it to dummies
      if (length(dummy_scaling_var)!=0){
        slope_1 <- paste0("+ i(`", var_names[[g]][i], "`, ref=c(\"", ref_event_time, "\")):weight")
        slope_2 <- paste0("i(`", others_here, "`, ref=c(\"", ref_event_time, "\")):weight", collapse = "+")
      } else{ # other wise, create dummies without scaling
        slope_1 <- paste0("+ i(`", var_names[[g]][i], "`, ref=c(\"", ref_event_time, "\"))")
        slope_2 <- paste0("i(`", others_here, "`, ref=c(\"", ref_event_time, "\"))", collapse = "+")
      }
      
      # combine slopes
      slope <- paste0(slope_1, "+", slope_2)
      
      # create formula
      formu <-  as.formula(paste0(y,  "~", slope, controls, "|", fes))
      
      # run and store regressions
      regs[[g]][[i]] <- feols(formu, cluster=cluster_vars, data=dt, weights=w, nthreads = 7)
    }
    
    
    # add level names to list
    nn <- hetero_var_types
    ## use dict if specified
    if (length(levels_dict)!=0){
      replace <- (hetero_var_types %in% levels_dict)
      nn <- hetero_var_types
      nn[replace] <- names(levels_dict)[match(nn[replace], levels_dict)] # replace where specified
    }
    names(regs[[g]]) <- nn
    
    # remove empty list items (if levels excluded)
    if (length(exclude_levels)!=0){
      regs[[g]] <- regs[[g]][sapply(regs[[g]], length) > 0]
    }
    
    
    # create and store plot for grouping_var level g
    plts[[g]] <- ggiplot(regs[[g]], ci_level = ci_level,
                         pt.join=T, i.select = i_select, ref.line = 0, main = plot_title, #pt.pch = c(15, 16, 24, 25),
                         geom_style = 'errorbar', theme = theme_bw()) + #geom = 'ribbon'
      scale_x_continuous(breaks = dt[, min(get(event_time_var))]:dt[, max(get(event_time_var))]) + xlab("Years since automation event") + 
      theme(legend.position = "bottom", panel.grid = element_blank(), legend.title = element_blank(), 
            panel.border = element_rect(colour = "black", size = 1.1, fill = NA)) + # guides(color = guide_legend(ncol=2), shape = guide_legend(ncol=2)) +
      scale_colour_grey(end = 0.3, start = 0.7) +
      scale_fill_grey(end = 0.3, start = 0.7) 
  }
  
  return(list(reg = regs[[g]][[1]], plts = plts))
}


