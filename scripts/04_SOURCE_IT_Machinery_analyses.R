##################################################################################
### This script calculates the effect of IT/Machinery investment spikes on separations and wages
# It is sourced from the script 04_main_worker.R 
##################################################################################

### Set output dir #### 
if (!dir.exists(paste0(map_output, "other_treatments/"))) dir.create(paste0(map_output, "other_treatments/"))
map_output_loops <- paste0(map_output, "other_treatments/")

##################################################################################
### add information on other spikes to selected_firms #####
#### Investment data ##### 
investment_spikes <- readRDS(paste0(map_data_paper3, "step4/investment_spikes.rds")) # investment spikes

# add information on investment spikes (where available)
vars_invest <- c("IT_inv", "Machinery_inv") 

selected_firms[, c("invest_data", paste0("adopter_", vars_invest), 
                   paste0("change_year_", vars_invest), 
                   vars_invest, paste0("real_", vars_invest, "_pw"), 
                   paste0("spike_", vars_invest), paste0(vars_invest, "_event_time")) := NULL]
selected_firms <- merge(selected_firms, 
                        unique(investment_spikes[, c("SBEID", "invest_data", paste0("adopter_", vars_invest), paste0("change_year_", vars_invest)), with=F]),
                        by = c("SBEID"), all.x=T)
vars_invest <- c("IT_inv", "Machinery_inv")

# add information on investment level of spikes and other spikes
selected_firms <- merge(selected_firms, unique(investment_spikes[, c("SBEID", "year", vars_invest, paste0("real_", vars_invest, "_pw"), paste0("spike_", vars_invest)), with=F]),
                        by= c("SBEID", "year"), all.x=T)

rm(investment_spikes)


##################################################################################
### Loop settings ####


event_time_vars <- paste0(c(vars_invest), "_event_time")

spike_vars <- paste0("spike_", c(vars_invest))

if (length(event_time_vars) != length(spike_vars)) stop("Length of vectors differs.")

##################################################################################

#### Create event time vars for other treatments #### 
##### Invest data ####
selected_firms[, invest_data := any(invest_data, na.rm=T), by=SBEID]

for (var in vars_invest){
  selected_firms[invest_data==T & get(paste0("adopter_", var))==T, paste0(var, "_event_time") := year - get(paste0("change_year_", var))]
  
}

##################################################################################
### Analysis settings ####

# This dictates whether a cost_var analysis should be run and scaling applied (Note: not implemented in estimations)
run_cost_var_analysis <- F

ref_event_time <- -1

event_time_var <- "auto_event_time"
event_time <- -3:4
matching_event_time <- -1 
stayer_event_time <- min(event_time):matching_event_time 
firm_exact_matching_vars <- c("nace_2digit")  
firm_cem_vars <- c("log_empl")  
probs <- unique(c(seq(0,1,0.1), 0.99, 1)) 
probs <- probs[order(probs)]
quantile_probs_firm_cem <- probs 
worker_exact_matching_vars <- NULL  
worker_cem_vars_1 <- c("lrhwage_2", "lrhwage_1", "lrhwage") 
probs <- unique(c(seq(0,1,0.1), 0.99, 1)) 
probs <- probs[order(probs)]
quantile_probs_worker_cem_1 <- probs
worker_cem_vars_2 <- NULL 
quantile_probs_worker_cem_2 <- NULL


##################################################################################
### Loop over other treatments from here ####
for (v in 1:length(event_time_vars)){
  # define event_time_var
  event_time_var <- event_time_vars[v]
  # define event_cost_var
  #event_cost_var <- event_cost_vars[v]
  # define spike var
  spike_var <- spike_vars[v]
  
  # Message
  cat(paste(Sys.time(), "Starting", event_time_var), "\n")
  
  # create treated_firms
  treated_firms <- selected_firms[!is.na(get(event_time_var)), ]
  
  ##################################################################################
  ### Create analysis panels ####
  ##################################################################################
  # Message
  cat(paste(Sys.time(), "Creating worker panel"), "\n")
  
  # create worker panel
  result <- create.worker.event.data(firm_panel = treated_firms, 
                                     POLIS_panel = POLIS_worker, 
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
                                     const_vars_POLIS_panel =  "SCONTRACTSOORT",
                                     const_vars_firm_panel =  c("MNE"))
  
  
  worker_panel <- copy(result$worker_panel) # main estimation panel 
  rm(result)
  
  ### prepare panel data for analysis ####
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
  
  #worker_panel[, stayer_DiD_2 := stayer_DiD]
  worker_panel[!is.na(in_polis), stayer_DiD := stayer_DiD & in_polis]
  
  # remove cluster-years without matched controls
  worker_panel[, keep := any(stayer_DiD & treated) & any(stayer_DiD & !treated),.(clusterID_worker, year)]
  worker_panel[, c("stayer_DiD", "keep") := .(fifelse(keep==T, stayer_DiD, F), NULL)]

  ##################################################################################
  cat(paste(Sys.time(), "Creating balanced panel"), "\n")
  
  #### create balanced worker panel based on firm-worker panel ####
  worker_panel_balanced <- create.balanced.panel(matched_worker_panel = worker_panel, 
                                                 event_time_var = event_time_var, event_time=event_time, 
                                                 expand_vars_const = c("female", "clusterID_worker"), matching_event_time = matching_event_time,
                                                 firm_type_var = NULL)
  
  # create grouping names for plots
  worker_panel_balanced[, MNE_group := fifelse(MNE_match, "MNE", "Domestic")]
   
  
  ##################################################################################
  # Identify spike 
  worker_panel_balanced[, paste0(spike_var) := NULL]
  worker_panel_balanced <- merge(worker_panel_balanced, firm_panel[treated==T, c("SBEID", "year", spike_var), with=F], by=c("SBEID", "year"), all.x=T)
  worker_panel_balanced[is.na(get(spike_var)), paste0(spike_var) :=F]
  worker_panel_balanced[, paste0(spike_var) := fifelse(!treated, FALSE, get(spike_var))]
  worker_panel_balanced[, paste0(spike_var) := fifelse(treated & same_firm, get(spike_var), FALSE)]
  worker_panel_balanced[order(workerID, cohort, year), spikes := cumsum(get(spike_var)), by=c("workerID", "cohort")]
  worker_panel_balanced[, spikes := fifelse(spikes==0, 0, spikes-1)]
  worker_panel_balanced[, max_spikes := max(spikes), by=.(workerID,cohort)]
  
  
  worker_panel[, paste0(spike_var) := NULL]
  worker_panel <- merge(worker_panel, firm_panel[treated==T, c("SBEID", "year", spike_var), with=F], by=c("SBEID", "year"), all.x=T)
  worker_panel[is.na(get(spike_var)), paste0(spike_var) :=F]
  worker_panel[, paste0(spike_var) := fifelse(!treated, FALSE, get(spike_var))]
  worker_panel[, paste0(spike_var) := fifelse(treated & same_firm, get(spike_var), FALSE)]
  worker_panel[order(workerID, cohort, year), spikes := cumsum(get(spike_var)), by=c("workerID", "cohort")]
  worker_panel[, spikes := fifelse(spikes==0, 0, spikes-1)]
  worker_panel[, max_spikes := max(spikes), by=.(workerID,cohort)]
   
  ##################################################################################
  ### Analysis ####
  ##################################################################################
  
  ##################################################################################
  ### Create formulas ####
  ##################################################################################
  #### formulas ####
  # stayer regs
  
  fixefs_stayer <- paste0(event_time_var, "^MNE_group^max_spikes +", event_time_var, "^clusterID_worker + workerID^clusterID_worker")
  if (worker_panel_balanced[, any(SCONTRACTSOORT_match==9)]){
    controls_stayer <-"+ i(SCONTRACTSOORT_match, age, ref=9) + i(SCONTRACTSOORT_match, age^2, ref=9)"
  } else{
    controls_stayer <-"+ age^2"
    
  }
  
  # wage regs
  fixefs_wage <-fixefs_stayer
  controls_wage <-controls_stayer
  
  ### Main: Stayer probability ###
  cat(paste(Sys.time(), "Running Stayer analysis"), "\n")
  
  #### regressions ######
  # redefine dependent variable
  worker_panel_balanced[, leave := !same_firm]
  
  stayer_ana <- plots.estimates(dt=worker_panel_balanced, y = "leave", event_time_var = event_time_var, MNE_grouping_var = "MNE_group",
                                ref_event_time = ref_event_time, 
                                controls = controls_stayer, fes = fixefs_stayer,
                                plot_title = "", ci_level = 0.95,
                                create.OLS.weights = T, weight_group = "clusterID_worker", 
                                cluster_vars = "SBEID_cluster",
                                #dummy_scaling_var = "log_cost_event",
                                diff_regs = T, diff_ref = "Domestic")
  
  #### Save outputs ######
  # Plot on main regression result
  ggsave(filename = paste0(map_output_loops, event_time_var, "_stayer.pdf"), plot = stayer_ana$plots,
         scale = 1.2, dpi = 300,
         width=unit(6, "inches"), height=unit(4.5, "inches"))
  

  ################################################################################
  ### Main: Wages of stayers ####
  cat(paste(Sys.time(), "Running Wage analysis"), "\n")

  #### Wage Regression #####
  wage_regs <- plots.estimates(dt=worker_panel[stayer_DiD==T, ], y = "lrhwage", event_time_var = event_time_var, MNE_grouping_var = "MNE_group",
                               ref_event_time = ref_event_time, 
                               controls = controls_wage, fes = fixefs_wage,
                               plot_title = "", ci_level = 0.95,
                               create.OLS.weights = T, weight_group = "clusterID_worker",
                               cluster_vars = "SBEID_cluster",
                               #dummy_scaling_var = "log_cost_event",
                               diff_regs = T, diff_ref = "Domestic")
  
  
  #### Save outputs ######
  # Plot on main regression result
  ggsave(filename = paste0(map_output_loops, event_time_var, "_wage.pdf"), plot = wage_regs$plots,
         scale = 1.2, dpi = 300,
         width=unit(6, "inches"), height=unit(4.5, "inches"))
  
  #### Save difference estimates ####
  etable(list(stayer_ana$diff_regs, wage_regs$diff_regs),
         title = paste0("The effect of ", spike_var, "on workers in MNEs vs. domestic firms."),
         label = paste0("tab:", spike_var),
         placement = "H",
         tex = T,
         fontsize = "footnotesize",
         file =  paste0(map_output_loops, "table_", event_time_var, "_diff.tex"),
         replace = T)
}


