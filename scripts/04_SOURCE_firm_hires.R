##################################################################################
### Firm level hires and wage #### 
# This script is sourced in the script 04_main_worker.R
##################################################################################

# Set output path 
if (!dir.exists(paste0(map_output_here, "hires/"))) dir.create(paste0(map_output_here, "hires/"))
map_output_script <- paste0(map_output_here, "hires/")
##################################################################################

### function to estimate poisson model and create plots with domestic / MNE split ###
plots.estimates.poisson <- function(dt, y = "stayer", event_time_var = "auto_event_time", MNE_grouping_var = "MNE_group",
                                    ref_event_time, 
                                    controls = controls_stayer, fes = fixefs_stayer, cluster_vars = c("SBEID_cluster"),
                                    diff_regs = F, diff_ref = "Domestic",
                                    create.plots = T, plot_title = "", ci_level = 0.95, 
                                    create.OLS.weights = T, weight_group = "clusterID_worker", 
                                    dummy_scaling_var = NULL, dummy_scaling_post_only = FALSE){
  # This function plots the estimates of the analysis, split up by MNEs and domestic firms.
  # dt is the data.table
  # y is the dependent variable
  # ref_event_time is the reference moment
  # controls is a formula part for control variables, e.g. "+ age"
  # fes are the fixed effects, e.g. "workerID^cohort + SBEID^cohort"
  # plot_title is the title of the plot. If plots_title = "", no title is added
  # ci_level is the level of the confidence intervals in the plots
  # 
  
  # to do: once you have a RI covariance matrix, you need to adjust this function.
  # 1. skip estimating standard errors in fixest estimation
  # 2. can you exploit ordering in covariance matrix for the two different plot regressions?
  
  
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
  regs_1 <- fepois(formulas[[1]], cluster=cluster_vars, data=dt, weights=w, nthreads = 7)
  
  if (create.plots){
    # if plot is wanted, run with variation in coefficient order
    regs_2 <- fepois(formulas[[2]], cluster=cluster_vars, data=dt, weights=w, nthreads = 7) 
  }
  
  # run diff regs if they are required
  if (diff_regs){
    diff_regs_l <- fepois(formulas[[3]], cluster=cluster_vars, data=dt, weights=w)
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


##################################################################################
##################################################################################

# Estimates on hires and their wage

# from POLIS get i) workers, ii) new hires, iii) (lagged) separations
## get all workers at an SBEID in matched data
wIDs <- POLIS_worker[SBEID %in% firm_panel[, unique(SBEID)], unique(workerID)]
## identify hires
# Note: gaps don't matter since I'm not interested in the mode of entry/exit
POLIS_worker[, hire := year==min(year), by=.(workerID, SBEID)]
## take care of missings and edge cases
### in 2010, everyone is new to firm by construction --> exclude these
POLIS_worker[year==2010, hire := NA]

## aggregate to firm level 
tt <- POLIS_worker[SBEID %in% firm_panel[, unique(SBEID)], .(workers=.N, 
                                                             wage = mean(hwage),
                                                             hires = sum(hire), 
                                                             wage_hires = mean(hwage[hire])),
                   by=.(SBEID, year)]
#tt[, sum(is.na(hires)), by=year]

#### add to firm_panel ####
firm_panel[, c("workers", "wage", "hires", "wage_hires") := NULL]
firm_panel <- merge(firm_panel, tt[, .(SBEID, year, workers, wage, hires, wage_hires)], 
                    by=c("SBEID", "year"), all.x=T)


#### Run firm level regression ###
# set formula
fixefs_firm <- paste0(event_time_var, "^MNE_group +", event_time_var, "^clusterID_firm + SBEID^clusterID_firm")
controls_firm <- "" 


# Run regressions: hires 
y <- "hires" #workers, wage, hires, wage_hires
# to do: to make this more comparable at the worker level, you have to weigh by employment at t=-1 
# in addition to weighing the control firms

hire_regs <- plots.estimates.poisson(dt=firm_panel[cohort!=2013,], y = y, event_time_var = event_time_var, MNE_grouping_var = "MNE_group",
                             ref_event_time = -3, 
                             controls = controls_firm, fes = fixefs_firm,
                             plot_title = "", ci_level = 0.95,
                             create.OLS.weights = T, weight_group = "clusterID_firm",
                             cluster_vars = "SBEID_cluster",
                             #dummy_scaling_var = "log_cost_event",
                             diff_regs = T, diff_ref = "Domestic")
etable(hire_regs$regs)
hire_regs$plots

ggsave(filename = paste0(map_output_script, "plot_hires.pdf"), plot=hire_regs$plots, 
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))

# Run regressions: wage hires 
firm_panel[is.na(wage_hires), wage_hires := 0]

y <- "wage_hires" #workers, wage, hires, wage_hires
# to do: to make this more comparable at the worker level, you have to weigh by employment at t=-1 
# in addition to weighing the control firms

hire_wage_regs <- plots.estimates.poisson(dt=firm_panel[cohort!=2013,], y = y, event_time_var = event_time_var, MNE_grouping_var = "MNE_group",
                                     ref_event_time = -3, 
                                     controls = controls_firm, fes = fixefs_firm,
                                     plot_title = "", ci_level = 0.95,
                                     create.OLS.weights = T, weight_group = "clusterID_firm", 
                                     cluster_vars = "SBEID_cluster",
                                     #dummy_scaling_var = "log_cost_event",
                                     diff_regs = T, diff_ref = "Domestic")
etable(hire_wage_regs$regs)
hire_wage_regs$plots



ggsave(filename = paste0(map_output_script, "plot_wage_hires.pdf"), plot=hire_wage_regs$plots, 
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))


### difference regressions ###
etable(list(hire_regs$diff_regs, hire_wage_regs$diff_regs),
       title = "Firm-level hires.",
       label = "tab:hires_firm_level",
       placement = "H",
       tex = T,
       fontsize = "footnotesize",
       file =  paste0(map_output_script, "table_firm_level_hires_diff.tex"),
       replace = T)

