# This script sets some global output styles

dict_vars <- c(same_firm = "Prob. to stay",
               `leave` = "Separation prob.",
               lrhwage = "Log(wage) stayers",
               event_time_MNE = "MNE $\\times$ event time  $\\times$ treated", `MNE_group::MNE` = "MNE  $\\times$ treated",
               event_time_Domestic = "Dom. firm $\\times$ event time $\\times$ treated", event_time = "event time", 
               `I(age^2)` = "age$^2$",
               SBEID_cluster = "Firm-cohort",
               `auto_event_time^MNE_group` = "event time-MNE", `MNE_group^auto_event_time` = "event time-MNE",
               `auto_event_time^MNE_group^NACE_letter_match` = "Firm type $\\times$ industry $\\times$ event time",
               `auto_event_time^MNE_group^exporter_match` = "Firm type $\\times$ exporter $\\times$ event time",
               `auto_event_time^MNE_group^importer_match` = "Firm type $\\times$ importer $\\times$ event time",
               `auto_event_time^MNE_group^rsales_group`  = "Firm type $\\times$ sales-group $\\times$ event time",
               `auto_event_time^fe_group^max_spikes`  = "event time-additional-spikes-group",
               `auto_event_time^MNE_group^max_spikes`  = "event time-additional-spikes-MNE",
               
               `nace_2digit` = "Industry (2 digit)",
               `year` = "Year",
               `workerID^clusterID_worker` = "Worker-cohort",
               `auto_adopter_MNETRUE` = "Automating & MNE",
               `auto_adopter_DOMTRUE` = "Automating & Domestic",
               `auto_event_time^clusterID_worker` = "Matched-group $\\times$ event time", `clusterID_worker^auto_event_time` = "Matched-group $\\times$ event time")
			   
			   
style_lines = style.tex(main = "base", line.top = "\\toprule", line.bottom = "\\bottomrule", fixef.title = "\\midrule Fixed-effects", fixef.where = "var",
                        stats.title ="\\midrule",
                        tabular = "normal",
                        depvar.title = "",
                        model.title = "",
                        var.title = "\\midrule",
                        yesNo = "$\\checkmark$")

setFixest_etable(digits = "r4", 
                 digits.stats = 4, 
                 fitstat = c("n", "r2", "ar2"), 
                 se.below = T, 
                 placement = "H",
                 dict = dict_vars,
                 style.tex = style_lines)
