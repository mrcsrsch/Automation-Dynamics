##################################################################################
### This script creates the analysis of firms growth paths for the Appendix #####
##################################################################################
### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table")
if (packageVersion("data.table")!="1.14.8") warning("Analysis ran in data.table version 1.14.8 - consider up- or downgrading")
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if (packageVersion("ggplot2")!="3.4.2") warning("Analysis ran in ggplot2 version 3.4.2 - consider up- or downgrading")
if (!require("fixest")) install.packages("fixest"); library("fixest")
if (packageVersion("fixest")!="0.11.2") warning("Analysis ran in xtable version 0.11.2 - consider up- or downgrading")

### source functions and styles #### 
source(paste0(map_scripts, "00_fixest_styles.R"), echo=F) # styles of fixest output tables

### Output dir #####
if (!dir.exists(paste0(map_output, "growth_paths/"))) dir.create(paste0(map_output, "growth_paths/"))
map_output_here <- paste0(map_output, "growth_paths/")

### load data #####
selected_firms <- readRDS(paste0(map_data_paper3, "step3/selected_firms_2010_2021.rds")) # selected firms panel
POLIS_worker <- readRDS(paste0(map_data_paper3, "step3/POLIS_worker.rds")) # full worker panel (at all SBEIDs)
cpi <- readRDS(paste0(map_data_paper3, "step1/cpi_year.rds")) # base year is 2021
selected_firms <- merge(selected_firms, cpi, by="year")
rm(cpi)
##################################################################################
##################################################################################
### prepare selected firms ####
setorderv(selected_firms, c("SBEID", "year"))

# subset to automating and non-automating firms over 2010:2021
selected_firms <- selected_firms[auto_ps_data_type %in% c("never_auto", "auto_adaptor"),]

# create estimation variables in POLIS_worker and add to selected_firms
POLIS_worker[, fixed_term := SCONTRACTSOORT == 2]
tt <- POLIS_worker[SBEID %in% selected_firms[, unique(SBEID)], .(mean_hwage = mean(hwage),
                                                           wage_bill = sum(SBASISLOON+SBIJZONDEREBELONING+SLNOWRK),
                                                           share_female = sum(female)/.N,
                                                           share_fixed_term = sum(fixed_term)/.N,
                                                           mean_worker_age = mean(age),
                                                           mean_hours_worked = sum(SREGULIEREUREN+SOVERWERKUREN),
                                                           workers = .N),
             by=.(SBEID, year)]
selected_firms[, .N]
selected_firms <- merge(selected_firms, tt, by=c("SBEID", "year"))
selected_firms[, .N]


# create estimation variables in selected_firms
selected_firms[, nace_2digit := as.numeric(substr(nace_sbi08, 1,2))]
selected_firms[, c("log_sales", "log_workers_fte", "log_mean_hwage", "log_wage_bill", "log_hours_worked") := .(log(sales_bedrijfsecondata), 
                                                                                        log(workers_fte), 
                                                                                        log(mean_hwage),
                                                                                        log(wage_bill),
                                                                                        log(mean_hours_worked))]

#### create initial year values for controls #### 
cols <- c("log_workers_fte", "log_sales", "log_mean_hwage", "log_wage_bill", "log_hours_worked") #"log_wage_bill"
selected_firms[order(SBEID, year), paste0(cols, "_initial") := lapply(.SD, function(x) x[1]), by=SBEID, .SDcols = cols]

#### create growth variables and add to selected_firms #### 
selected_firms[order(SBEID, year), paste0(cols, "_growth") := lapply(.SD, function(x) x-c(NA, x[-.N])), by=SBEID, .SDcols = cols]

#### create MNE and dom auto adaptor vars ####
selected_firms[, auto_adopter_MNE := auto_adopter_ps & MNE]
selected_firms[, auto_adopter_DOM := auto_adopter_ps & !MNE]

#### identify a perfectly balanced panel ####
selected_firms[, balanced := .N==length(2010:2021), by=SBEID]
 
##################################################################################
### Regressions: Compare automating firms to non-automating firms ####
# set formula parts
controls <- paste0(cols, "_initial")

regs_full <- list()
regs_balanced <- list()

k <- 0
i <- 1
for (y in paste0(cols, "_growth")){
  k <- k+1
  
  # levels
  form_basic <- as.formula(paste0(y, "~", "auto_adopter_MNE + auto_adopter_DOM + MNE", "| year + nace_2digit"))
  form_controls <- as.formula(paste0(y, "~", "auto_adopter_MNE + auto_adopter_DOM  + MNE + i(MNE,", controls[k], ") | year + nace_2digit"))

  # regs on full panel
  regs_full[[i]] <- feols(form_basic, data=selected_firms, cluster="SBEID")
  regs_full[[i+1]] <- feols(form_controls, data=selected_firms, cluster="SBEID")
  
  # regs on balanced panel
  regs_balanced[[i]] <- feols(form_basic, data=selected_firms[balanced==T,], cluster="SBEID")
  regs_balanced[[i+1]] <- feols(form_controls, data=selected_firms[balanced==T,], cluster="SBEID")
  
  i <- i + 2
}

etable(regs_full)

# save result
etable(regs_full,title = "Automating vs. non-automating firms.",
       label = "tab:DiD_motiv",
       placement = "H",
       tex = T,
       fontsize = "footnotesize",
       file =  paste0(map_output_here, "table_DiD_motivation.tex"),
       replace = T)
etable(regs_balanced,title = "Automating vs. non-automating firms (balanced sample).",
       label = "tab:DiD_motiv_balancled",
       placement = "H",
       tex = T,
       fontsize = "footnotesize",
       file =  paste0(map_output_here, "table_DiD_motivation_balanced.tex"),
       replace = T)


