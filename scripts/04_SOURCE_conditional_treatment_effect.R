##################################################################################
#### This script calculates the main treatment effect conditional on other treatment effects
# It is sourced from the script 04_main_worker.R 
##################################################################################

### set output dir ###
if (!dir.exists(paste0(map_output_here, "other_treatment_control/"))) dir.create(paste0(map_output_here, "other_treatment_control/"))
map_output_script <- paste0(map_output_here, "other_treatment_control/")

##################################################################################
### function ####
# function to estimate difference result, controlling for other treatments
estimates.other.treatments <- function(dt, y = "stayer", main_treatment = "MNE_group", main_treatment_ref = "Domestic",
                                       other_treatment = NULL, 
                                       #additional_treatments = NULL, additional_treatments_ref = NULL,
                                       add_fe_other_treatments = F,
                                       fe_correction = NULL,
                                       dummy_scaling_var = NULL,
                                       controls, fes, 
                                       event_time_var, w, cluster){
  
  # add weights to account for differences in expenditures between MNEs and domestic firms
  if (length(dummy_scaling_var)!=0){
    dt[treated==T, weight := get(dummy_scaling_var)]
    dt[treated==F, weight := 0]
    # Note slopes are adjusted below
  }
  
  # create main treatment slope
  slope_1 <- paste0("+ i(event_time, ", main_treatment, ", ref=c(\"", ref_event_time, "\"), ref2=c(\"", main_treatment_ref,"\"))")
  
  # add regular slope if no other treatment
  if (length(other_treatment)==0){
    slope_2 <- paste0("+ i(event_time, ref=c(\"", ref_event_time, "\"))")
  }else{
    # create other treatment slope 
    slope_2 <- paste0("+ i(event_time, i.", other_treatment, ", ref=c(\"", ref_event_time, "\"))")
  }
  
  # add weights
  if (length(dummy_scaling_var)!=0){
    slope_1 <- paste0(slope_1, ":weight")
    slope_2 <- paste0(slope_2, ":weight")
  }

  
  # add main fe interaction in fixed effect 
  fes <- paste0(fes, "+", event_time_var, "^", main_treatment, ifelse(length(fe_correction)==0, NULL, paste0("^", fe_correction)))
  
  # if needed add fixed effect interaction
  if (add_fe_other_treatments){
    # create new variable to split reference group by other treatment
    # dt[get(main_treatment)==main_treatment_ref, fe_group := as.character(get(other_treatment))]
    # dt[get(main_treatment)!=main_treatment_ref, fe_group := as.character(get(main_treatment))]
    dt[, fe_group := as.character(get(other_treatment))]
    
    fes <- paste0(fes, "+", event_time_var, "^", "fe_group", ifelse(length(fe_correction)==0, NULL, paste0("^", fe_correction)))
    
  } 
  # else{
  #   # create fe group --> easier for output handling
  #   #dt[, fe_group := get(main_treatment)]
  #   
  #   # add fixed effect interaction on main difference
  #   fes <- paste0(fes, "+", event_time_var, "^", "main_treatment", ifelse(length(fe_correction)==0, NULL, paste0("^", fe_correction)))
  # }
  # create formula
  form <- as.formula(paste0(y,  "~", slope_1, slope_2, controls, "|", fes))
  
  # estimate result
  reg <- feols(form, cluster=cluster, data=dt, weights=w, nthreads = 7)
  
  # return result
  return(reg)
}
##################################################################################
### Analysis settings ####
#### Create necessary variables ####
worker_panel_balanced[, .N]
##### exporter ######
tt <- firm_panel[get(event_time_var)==matching_event_time,
                 .(SBEID_cluster, treated, varx = log(rexport))]

tt[varx==-Inf, exporter_group := "none"]
tt[varx!=-Inf, save := as.integer(qcut(varx, q=seq(0,1,0.1), numbered = T))-1]
tt[varx!=-Inf, exporter_group := fifelse(save < 5, paste0("below_",save), paste0("above_",save))]


# add to worker panels
worker_panel_balanced[,exporter_group := NULL ]
worker_panel[,exporter_group := NULL ]

worker_panel_balanced <- merge(worker_panel_balanced, tt[, .(SBEID_cluster, exporter_group)], by="SBEID_cluster")
worker_panel <- merge(worker_panel, tt[, .(SBEID_cluster, exporter_group)], by="SBEID_cluster")
rm(tt)

worker_panel_balanced[, .N]
##### importer ######
tt <- firm_panel[get(event_time_var)==matching_event_time,
                 .(SBEID_cluster, treated, varx = log(rimport))]

tt[varx==-Inf, importer_group := "none"]
tt[varx!=-Inf, save := as.integer(qcut(varx, q=seq(0,1,0.1), numbered = T))-1]
tt[varx!=-Inf, importer_group := fifelse(save < 5, paste0("below_",save), paste0("above_",save))]

# add to worker panels
worker_panel_balanced[,importer_group := NULL ]
worker_panel[, importer_group := NULL ]

worker_panel_balanced <- merge(worker_panel_balanced, tt[, .(SBEID_cluster, importer_group)], by="SBEID_cluster")
worker_panel <- merge(worker_panel, tt[, .(SBEID_cluster, importer_group)], by="SBEID_cluster")
rm(tt)

worker_panel_balanced[, .N]

##### auto cost / worker #####
# get treated
tt <- firm_panel[get(event_time_var)==0,
                 .(SBEID_cluster, SBEID, treated, varx = fifelse(treated, log(real_automation_cost_pw), NA_real_))]
# get untreated from treated_firms
un <- merge(firm_panel[get(event_time_var)==0, c("SBEID", "SBEID_cluster")],
            treated_firms[get(event_time_var)==0 & SBEID %in% tt[treated==F, SBEID], .(SBEID, treated =F, varx = log(real_automation_cost_pw)) ],
            by = "SBEID")
# add to tt
tt <- rbindlist(list(tt[treated==T, c("SBEID_cluster", "treated", "varx")], un[, c("SBEID_cluster", "treated", "varx")]))
# create groups
tt[, save := as.integer(qcut(varx, q=seq(0,1,0.1), numbered = T))-1]
tt[, cost_group_worker := fifelse(save < 5, paste0("below_",save), paste0("above_",save))]

# add to worker panels
worker_panel_balanced[,cost_group_worker := NULL ]
worker_panel[,cost_group_worker := NULL ]

worker_panel_balanced <- merge(worker_panel_balanced, tt[, .(SBEID_cluster, cost_group_worker)], by="SBEID_cluster")
worker_panel <- merge(worker_panel, tt[, .(SBEID_cluster, cost_group_worker)], by="SBEID_cluster")
rm(tt)

worker_panel_balanced[, .N]


##### auto cost share #####
# get treated
tt <- firm_panel[get(event_time_var)==0,
                 .(SBEID_cluster, SBEID, treated, varx = fifelse(treated, log(auto_cost_share), NA_real_))]
# get untreated from treated_firms
un <- merge(firm_panel[get(event_time_var)==0, c("SBEID", "SBEID_cluster")],
            treated_firms[get(event_time_var)==0 & SBEID %in% tt[treated==F, SBEID], .(SBEID, treated =F, varx = log(auto_cost_share)) ],
            by = "SBEID")
# add to tt
tt <- rbindlist(list(tt[treated==T, c("SBEID_cluster", "treated", "varx")], un[, c("SBEID_cluster", "treated", "varx")]))
# create groups
tt[, save := as.integer(qcut(varx, q=seq(0,1,0.1), numbered = T))-1]
tt[, cost_group_share := fifelse(save < 5, paste0("below_",save), paste0("above_",save))]

# add to worker panels
worker_panel_balanced[,cost_group_share := NULL ]
worker_panel[,cost_group_share := NULL ]

worker_panel_balanced <- merge(worker_panel_balanced, tt[, .(SBEID_cluster, cost_group_share)], by="SBEID_cluster")
worker_panel <- merge(worker_panel, tt[, .(SBEID_cluster, cost_group_share)], by="SBEID_cluster")
rm(tt)

worker_panel_balanced[, .N]

#### Other settings ####

# create weights
worker_panel_balanced[, weight_OLS := fifelse(treated, 1, sum(treated)/sum(!treated)), by=c("clusterID_worker", "auto_event_time")]
worker_panel[stayer_DiD==T, weight_OLS := fifelse(treated, 1, sum(treated)/sum(!treated)), by=c("clusterID_worker", "auto_event_time")]
w <-  as.formula(paste0("~", "weight_OLS"))

# create event time variable
worker_panel_balanced[, "event_time" := fifelse(treated, get(event_time_var), ref_event_time)]
worker_panel[stayer_DiD==T, "event_time" := fifelse(treated, get(event_time_var), ref_event_time)]

# set controls and basic fes (NOTE: interaction with max_spikes and MNE_group is added by function)
fes <- paste0(event_time_var, "^clusterID_worker + workerID^clusterID_worker")
controls <-"+ i(SCONTRACTSOORT_match, age, ref=9) + i(SCONTRACTSOORT_match, age^2, ref=9)"

##################################################################################
# other differences based on observed difference in: 
other_treatments <- c("exporter_group", "importer_group",
                      "cost_group_worker", "cost_group_share", 
                      "nace_2digit_match")


# store results in lists
regs_otreat_stayer <- list()
regs_otreat_wage <- list()

stayer_y <- "leave"
wage_y <- "lrhwage"

i <- 0 # keep track of list positions

gc()
### Estimate main difference ####
i <- i + 1 

## stayer
regs_otreat_stayer[[i]] <- estimates.other.treatments(y = stayer_y,
                                                      dt = worker_panel_balanced,
                                                      main_treatment = "MNE_group",
                                                      main_treatment_ref = "Domestic",
                                                      other_treatment = NULL,
                                                      add_fe_other_treatments = F,
                                                      fe_correction = "max_spikes",
                                                      #dummy_scaling_var = "log_cost_event",
                                                      controls = controls,
                                                      fes = fes,
                                                      event_time_var = event_time_var,
                                                      w = w,
                                                      cluster = "SBEID_cluster")

## wage
regs_otreat_wage[[i]] <- estimates.other.treatments(y = wage_y,
                                                    dt = worker_panel[stayer_DiD==T,],
                                                    main_treatment = "MNE_group",
                                                    main_treatment_ref = "Domestic",
                                                    other_treatment = NULL,
                                                    add_fe_other_treatments = F,
                                                    fe_correction = "max_spikes",
                                                    #dummy_scaling_var = "log_cost_event",
                                                    controls = controls,
                                                    fes = fes,
                                                    event_time_var = event_time_var,
                                                    w = w,
                                                    cluster = "SBEID_cluster")


##### Loop over other treatments #####

other_treatments
for (other_treat in other_treatments){
  # message
  cat(paste0(Sys.time(), ": ", other_treat), "\n")
  
  i <- i+1
  
  
  ## stayer
  regs_otreat_stayer[[i]] <- estimates.other.treatments(y = stayer_y,
                                                        dt = worker_panel_balanced,
                                                        main_treatment = "MNE_group",
                                                        main_treatment_ref = "Domestic",
                                                        other_treatment = other_treat,
                                                        add_fe_other_treatments = T,
                                                        fe_correction = "max_spikes",
                                                        #dummy_scaling_var = "log_cost_event",
                                                        controls = controls,
                                                        fes = fes,
                                                        event_time_var = event_time_var,
                                                        w = w,
                                                        cluster = "SBEID_cluster")
  
  print(etable(list(regs_otreat_stayer[[1]], regs_otreat_stayer[[i]]), keep="MNE", fitstat = c("n", "r2", "ar2")))
  
  
  ## wage
  regs_otreat_wage[[i]] <- estimates.other.treatments(y = wage_y,
                                                      dt = worker_panel[stayer_DiD==T,],
                                                      main_treatment = "MNE_group",
                                                      main_treatment_ref = "Domestic",
                                                      other_treatment = other_treat,
                                                      add_fe_other_treatments = T,
                                                      fe_correction = "max_spikes",
                                                      #dummy_scaling_var = "log_cost_event",
                                                      controls = controls,
                                                      fes = fes,
                                                      event_time_var = event_time_var,
                                                      w = w,
                                                      cluster = "SBEID_cluster")
  
  # print
  print(etable(list(regs_otreat_wage[[1]], regs_otreat_wage[[i]]), keep="MNE", fitstat = c("n", "r2", "ar2")))
  
  # clear memory 
  gc()
}


##################################################################################
### print result ###


# add names to list
names(regs_otreat_stayer) <- c("Main", other_treatments)
names(regs_otreat_wage) <- c("Main", other_treatments)

# display
etable(regs_otreat_stayer, keep="MNE", fitstat = c("n", "r2", "ar2"))
etable(regs_otreat_wage, keep="MNE", fitstat = c("n", "r2", "ar2"))

# save output tables
etable(regs_otreat_stayer, keep="MNE", fitstat = c("n", "r2", "ar2"), 
       title = "Controlling for other treatments (Probability to stayer)",
       label = "tab:other_treat_stayer",
       headers = names(regs_otreat_stayer),
       placement = "H",
       tex = T,
       fontsize = "footnotesize",
       file =  paste0(map_output_script, "table_stayer_cont_other_treatment.tex"),
       replace = T)
etable(regs_otreat_wage, keep="MNE", fitstat = c("n", "r2", "ar2"),
       title = "Controlling for other treatments (Wage of stayers)",
       label = "tab:other_treat_wage",
       headers = names(regs_otreat_wage),
       placement = "H",
       tex = T,
       fontsize = "footnotesize",
       file =  paste0(map_output_script, "table_wage_cont_other_treatment.tex"),
       replace = T)

### save 2 additional tables (Trade and Intensity: seps + wage et) ####

# trade
etable(list(regs_otreat_stayer[[2]], regs_otreat_wage[[2]], regs_otreat_stayer[[3]], regs_otreat_wage[[3]]),
       agg = "(event_time::-?[[:digit:]]+):.*::(above|below|none)",
       fitstat = c("n", "r2"),
       title = "Controlling for other treatments (exporter and emporter groups)",
       label = "tab:other_treat_trade",
       headers = c(rep(names(regs_otreat_stayer)[2],2), rep(names(regs_otreat_stayer)[3],2)), 
       placement = "H",
       tex = T,
       fontsize = "footnotesize",
       file =  paste0(map_output_script, "table_other_treatment_trade.tex"),
       replace = T)

# automation intensity
etable(list(regs_otreat_stayer[[4]], regs_otreat_wage[[4]], regs_otreat_stayer[[5]], regs_otreat_wage[[5]]),
       agg = "(event_time::-?[[:digit:]]+):.*::(above|below|none)",
       fitstat = c("n", "r2"),
       title = "Controlling for other treatments (automation intensity Groups)",
       label = "tab:other_treat_intensity",
       headers = c(rep(names(regs_otreat_stayer)[4],2), rep(names(regs_otreat_stayer)[5],2)), 
       placement = "H",
       tex = T,
       fontsize = "footnotesize",
       file =  paste0(map_output_script, "table_other_treatment_intensity.tex"),
       replace = T)


