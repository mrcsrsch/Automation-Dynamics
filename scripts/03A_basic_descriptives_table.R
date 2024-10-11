##################################################################################
### Basic descriptives statistics #####
##################################################################################
### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table")
if (packageVersion("data.table")!="1.14.8") warning("Analysis ran in data.table version 1.14.8 - consider up- or downgrading")
if (!require("xtable")) install.packages("xtable"); library("xtable")
if (packageVersion("xtable")!="1.8.4") warning("Analysis ran in xtable version 1.8.4 - consider up- or downgrading")
# This one is actually not necessary but the adjusted function calls it, so install here.
if (!require("Hmisc")) install.packages("Hmisc"); library("Hmisc")
if (packageVersion("Hmisc")!="5.1.0") warning("Analysis ran in Hmisc version 5.1.0 - consider up- or downgrading")

### Output dir #####
if (!dir.exists(paste0(map_output, "basic_descriptives/"))) dir.create(paste0(map_output, "basic_descriptives/"))
map_output_here <- paste0(map_output, "basic_descriptives/")

### load data #####
POLIS_firms <- readRDS(paste0(map_data_analysis, "step2/POLIS_firms.rds")) # firms in POLIS, i.e. the complete firm-year panel
POLIS_worker <- readRDS(paste0(paste0(map_data_analysis, "step3/"), "POLIS_worker.rds"))
selected_firms <- readRDS(paste0(paste0(map_data_analysis, "step3/"), "selected_firms_2010_2021.rds"))
cpi <- readRDS(paste0(map_data_analysis, "step1/cpi_year.rds")) # base year is 2021
selected_firms <- merge(selected_firms, cpi, by="year")

##################################################################################
##################################################################################
# extract worker-level information and create a new firm-level dataset
firmIDs <- selected_firms[, unique(SBEID)]
worker_data <- POLIS_worker[SBEID %in% firmIDs, .(workers_fte_POLIS = sum(deeltijdfactor),
                                   workers_POLIS = .N, 
                                   age = sum(age)/.N,
                                   fixed_term = sum(SCONTRACTSOORT==2, na.rm=T)/.N*100,
                                   educ_level_high = sum(educ_level==3, na.rm=T)/sum(!is.na(educ_level))*100,
                                   educ_level_medium = sum(educ_level==2, na.rm=T)/sum(!is.na(educ_level))*100,
                                   educ_level_low = sum(educ_level==1, na.rm=T)/sum(!is.na(educ_level))*100,
                                   #educ_level_unobserved = sum(is.na(educ_level)*deeltijdfactor)/.N*100,
                                   hwage_polis = exp(lrhwage)/.N), by=c("SBEID", "year")]
worker_data <- worker_data[, lapply(.SD, function(x) mean(x, na.rm=T)), by=SBEID, .SDcols = names(worker_data)[-c(1,2)]]

## create firm data 
firm_data <- selected_firms[, .(auto_adopter_ps = auto_adopter_ps[1],
                                workers_fte = mean(workers_fte),
                                MNE = MNE[1],
                                NACE_letter   = NACE_letter[1],
                                firm_age = mean(year-firm_birthyear),
                                auto_cost_share = mean(automation_cost/total_cost, na.rm=T)*100,
                                real_sales_pw = mean(sales_bedrijfsecondata*factor/workers_fte, na.rm=T),
                                real_automation_cost_pw = mean(automation_cost*factor/workers_fte, na.rm=T)), by=SBEID]

# add worker data
firm_data <- merge(firm_data, worker_data, by="SBEID")

# NACE industry labels
firm_data[, NACE_letter_label := fifelse(NACE_letter=="C", "Manufacturing", 
                                          fifelse(NACE_letter=="F", "Construction",
                                                  fifelse(NACE_letter=="G", "Wholesale and Retail Trade",
                                                          fifelse(NACE_letter=="H", "Transportation and Storage",
                                                                  fifelse(NACE_letter=="I", "Accomodation and Food Service", 
                                                                          fifelse(NACE_letter=="J", "Information and Communication",
                                                                                  fifelse(NACE_letter=="M", "Prof., Scientific and Technical Activities",
                                                                                          fifelse(NACE_letter=="N", "Administrative and Support Activities", "MISSING"))))))))]



##################################################################################
# how many workers per year do the firms employ? 

# number firms and their employment
selected_firms[, uniqueN(SBEID)]
selected_firms[, uniqueN(SBEID), by=MNE]


# employment per year
selected_firms[, sum(workers_fte), by=year]
selected_firms[, sum(workers_fte), by=year][, mean(V1)]

selected_firms[, sum(workers_fte), by=.(MNE,year)][, V1[MNE==T]/sum(V1)*100, by=year][, mean(V1)]


# share of total employment 
tt <- merge(selected_firms[, .(selected = sum(workers_fte)), by=year], POLIS_firms[, .(all = sum(workers_fte)), by=year], by="year")
tt[, share := selected/all*100]
tt[, mean(share)]

# function that calculates summary stats using OLS weights
summary_stat_basic <- function(dt, vars, 
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
    rows <- length(vars) + 1
    # add one row per category per variable
    rows <- rows + sum(unlist(dt[, lapply(.SD, uniqueN), .SDcols = categorical_vars]))
    
    # set row names 
    rnames <- c("Firms", vars)
    rnames <- c(rnames, dt[, unlist(lapply(.SD, unique)), .SDcols = categorical_vars])
    # paste0(unlist(dt[, lapply(.SD, unique), .SDcols = categorical_vars]), " (%)")
  } else {
    rows <- length(vars)+1
    rnames <- c("Firms", vars)
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
    # add firms
    summary_stats[1, c] <- t(dt[get(col_var)==n, .(uniqueN(SBEID))])
    # add means and SD of non-categorical variables
    #summary_stats[6:(length(vars)+5),c] <- t(dt[get(col_var)==n, lapply(.SD, function(x) mean(x, na.rm=T)), .SDcols = vars])
    #summary_stats[6:(length(vars)+5),c+1] <- t(dt[get(col_var)==n, lapply(.SD, function(x) sd(x, na.rm=T)), .SDcols = vars])
    ## using weights
    summary_stats[2:(length(vars)+1),c] <- t(dt[get(col_var)==n, lapply(.SD, function(x) wtd.mean(x, weights = var_weight, na.rm=T, normwt = F)), .SDcols = vars])
    summary_stats[2:(length(vars)+1),c+1] <- t(dt[get(col_var)==n, lapply(.SD, function(x) sqrt(wtd.var(x, weights = var_weight, na.rm=T, normwt = F))), .SDcols = vars])
    
    
    # Note: for categorical vars, I use strata_var weighing.
    i <- (length(vars)+1) # keep track of row position
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


# create summary stats table 
vars <- c("auto_adopter_ps", "auto_cost_share", "real_automation_cost_pw", "real_sales_pw", "workers_fte", "hwage_polis", "age",
          "educ_level_high" ,"educ_level_medium", "educ_level_low", "NACE_letter_label")


vars_dict <- c(auto_adopter_ps = "Automation adopter", 
               auto_cost_share= "Automation cost share", 
               real_automation_cost_pw = "Automation cost per worker real (1000 EURs)",
               real_sales_pw= "Sales per worker real (1000 EURs)", 
               workers_fte = "Employment (full time equivalent)", 
               hwage_polis = "Mean hourly wage real", 
               age = "Mean worker age",
               educ_level_high = "High Education level (%)",
               educ_level_medium = "Mid Education level (%)", 
               educ_level_low = "Low Education level (%)",
               educ_level_unobserved = "Unobserved Education level (%)")


stats <- summary_stat_basic(firm_data,
                          vars, 
                          col_var = "MNE", 
                          col_var_order = c("TRUE", "FALSE"), 
                          col_var_dict = c("MNE", "Domestic firm"),
                          categorical_vars = c("NACE_letter_label"), vars_dict = vars_dict, 
                          strata_var = "SBEID", use_weights = F, weight_var = NULL)



# save summary stats table
print.xtable( xtable(stats, caption = "Summary Statistics."), 
      caption.placement = "top", size = "\\footnotesize", table.placement = "H", 
      label = "tab:basic_summary",
      booktabs = T,
      file = paste0(map_output_here, "table_basic_summary_stats.tex"))
