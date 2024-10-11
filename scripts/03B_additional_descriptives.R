### Detailed summary statistics #####

##################################################################################
### packages ######
if (!require("data.table")) install.packages("data.table"); library("data.table")
if (packageVersion("data.table")!="1.14.8") warning("Analysis ran in data.table version 1.14.8 - consider up- or downgrading")
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if (packageVersion("ggplot2")!="3.4.2") warning("Analysis ran in ggplot2 version 3.4.2 - consider up- or downgrading")
if (!require("xtable")) install.packages("xtable"); library("xtable")
if (packageVersion("xtable")!="1.8.4") warning("Analysis ran in xtable version 1.8.4 - consider up- or downgrading")
if (!require("rlist")) install.packages("rlist"); library("rlist") # append list with names
if (packageVersion("rlist")!="0.4.6.2") warning("Analysis ran in rlist version 0.4.6.2 - consider up- or downgrading")
if (!require("fixest")) install.packages("fixest"); library("fixest") # append list with names
if (packageVersion("fixest")!="0.11.1") warning("Analysis ran in rlist version 0.11.1 - consider up- or downgrading")


### Output dir #####
if (!dir.exists(paste0(map_output, "summary_stats/"))) dir.create(paste0(map_output, "summary_stats/"))
map_output_here <- paste0(map_output, "summary_stats/")

if (!dir.exists(paste0(map_output, "presentation/"))) dir.create(paste0(map_output, "presentation/"))
map_output_presentation <- paste0(map_output, "presentation/")

### load data #####
selected_firms <- readRDS(paste0(map_data_analysis, "step3/selected_firms_2010_2021.rds")) # selected firms panel
cpi <- readRDS(paste0(map_data_analysis, "step1/cpi_year.rds")) # base year is 2021
selected_firms <- merge(selected_firms, cpi, by="year")

# load other treatments and add to selected firms
### add information on other spikes to selected_firms #####
#### Investment data ##### 
investment_spikes <- readRDS(paste0(map_data_analysis, "step4/investment_spikes.rds")) # investment spikes

# add information on investment spikes (where available)
vars <- c("IT_inv", "Machinery_inv")
selected_firms <- merge(selected_firms, 
                        unique(investment_spikes[, c("SBEID", "invest_data", paste0("adopter_", vars), paste0("change_year_", vars)), with=F]),
                        by = c("SBEID"), all.x=T)

# add information on investment level of spikes 
selected_firms <- merge(selected_firms, unique(investment_spikes[, c("SBEID", "year", vars, paste0("real_", vars, "_pw")), with=F]),
                        by= c("SBEID", "year"), all.x=T)

rm(investment_spikes)

##################################################################################
### Prepare data ####

# add sector labels
selected_firms[, NACE_letter_label := fifelse(NACE_letter=="C", "Manufacturing", 
                                                fifelse(NACE_letter=="F", "Construction",
                                                        fifelse(NACE_letter=="G", "Wholesale and Retail Trade",
                                                                fifelse(NACE_letter=="H", "Transportation and Storage",
                                                                        fifelse(NACE_letter=="I", "Accommodation and Food Service", 
                                                                                fifelse(NACE_letter=="J", "Information and Communication",
                                                                                        fifelse(NACE_letter=="M", "Prof., Scientific and Technical Activities",
                                                                                                fifelse(NACE_letter=="N", "Administrative and Support Activities", "MISSING"))))))))]




# work on copy
selected_firms_2 <- copy(selected_firms)

##################################################################################
### By sector and MNE/domestic ####
tt <- selected_firms_2[MNE_data_type %in% c("always_domestic", "always_MNE") & auto_ps_data_type == "auto_adaptor" & year == auto_change_year_ps,
                       .(auto_events = .N), by=.(NACE_letter_label, MNE_data_type)]
tt[, MNE_type := fifelse(MNE_data_type=="always_MNE", "MNE", "Domestic")]

tt_summary <- tt[, sum(auto_events), by=NACE_letter_label][order(-V1)]
tt[, NACE_letter_label := factor(NACE_letter_label, levels = tt_summary$NACE_letter_label)]

# For paper
ggplot(data=tt, aes(x=NACE_letter_label, y=auto_events, fill=MNE_type)) + geom_bar(stat="identity") + 
  ylab("Automation events") +
  theme_bw() + theme(legend.position = "bottom", legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust=1)) +
  scale_colour_grey(end = 0.3, start = 0.7) +
  theme(panel.grid = element_blank(), axis.title.x = element_blank(), panel.border = element_rect(colour = "black", size = 1.1, fill = NA)) +
  scale_fill_grey(end = 0.3, start = 0.7)  
ggsave(filename = paste0(map_output_here, "plot_auto_events_sector_MNE_DOM.pdf"), plot=last_plot(), 
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))

# For presentation
ggsave(filename = paste0(map_output_presentation, "plot_auto_events_sector_MNE_DOM.pdf"), plot=last_plot(), 
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(6*9/16, "inches"))

##################################################################################
#### plot automation events per year (bar plot)  ####
tt <- selected_firms_2[auto_ps_data_type %in% c("auto_adaptor", "always_auto"), uniqueN(SBEID), by=auto_change_year_ps][order(auto_change_year_ps),]
ggplot(data=tt, aes(x=auto_change_year_ps, y=V1)) + geom_bar(stat="identity") + 
  scale_x_continuous(breaks = seq(min(tt$auto_change_year_ps, na.rm=T), max(tt$auto_change_year_ps, na.rm=T), 2)) + 
  xlab("Year") + ylab("Automation events") + theme_bw() +
  theme(panel.grid = element_blank(), panel.border = element_rect(colour = "black", size = 1.1, fill = NA)) +
  scale_colour_grey(end = 0.3, start = 0.7) +
  scale_fill_grey(end = 0.3, start = 0.7) 
#ggsave(filename = paste0(map_output_here, "auto_events_year.pdf"), plot=last_plot(), width=16*0.8, height=9*0.8, scale=0.8)

#### plot automation events per year and always MNE | always domestic at automation event (bar plot)  ####
selected_firms_2[MNE_data_type %in% c("always_domestic", "always_MNE") & auto_ps_data_type %in% c("auto_adaptor", "always_auto") & auto_change_year_ps >= 2010,
                 MNE_auto := MNE_data_type == "always_MNE", by=SBEID]
tt <- selected_firms_2[MNE_data_type %in% c("always_domestic", "always_MNE") & auto_ps_data_type %in% c("auto_adaptor", "always_auto") & auto_change_year_ps >= 2010, 
                       uniqueN(SBEID), by=.(auto_change_year_ps, MNE_auto)][order(auto_change_year_ps, MNE_auto),]
tt[, MNE_auto := fifelse(MNE_auto, "MNE", "Domestic")]
ggplot(data=tt, aes(x=auto_change_year_ps, y=V1, fill=MNE_auto)) + geom_bar(stat="identity") + 
  scale_x_continuous(breaks = seq(min(tt$auto_change_year_ps, na.rm=T), max(tt$auto_change_year_ps, na.rm=T), 2)) + 
  xlab("Year") + ylab("Automation events") + theme_bw() + theme(legend.position = "bottom", legend.title = element_blank()) +
  theme(panel.grid = element_blank(), panel.border = element_rect(colour = "black", size = 1.1, fill = NA)) +
  scale_colour_grey(end = 0.3, start = 0.7) +
  scale_fill_grey(end = 0.3, start = 0.7) 
ggsave(filename = paste0(map_output_here, "plot_auto_events_year_always_MNE_dom.pdf"), plot=last_plot(), 
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))

##################################################################################
### automation spike frequency (Bessen Appendix C) in 2010-2021 ####
selected_firms_2[, nr_spikes_numeric := sum(auto_spike_ps, na.rm=T), by=SBEID]
selected_firms_2[, nr_spikes := as.character(nr_spikes_numeric)]
selected_firms_2[nr_spikes_numeric >=4, nr_spikes := ">= 4"]
selected_firms_2[auto_ps_data_type=="always_auto", nr_spikes := "automation event <= 2010"]

tt <- selected_firms_2[, .(firms = uniqueN(SBEID), share_firms = uniqueN(SBEID)/selected_firms_2[, uniqueN(SBEID)]*100), by=nr_spikes][order(nr_spikes),]

# colnames(tt) <- c("Automation spikes", "Firms", "Share of firms")
# print(xtable(tt), include.rownames = F,
#       file = paste0(map_output_here, "automation_spike_frequency.tex"), table.placement="H")

#### automation spike frequency by always MNE in 2010-2021 ####
tt <- selected_firms_2[MNE_data_type == "always_MNE", .(firms = uniqueN(SBEID),
                                                        share_firms = uniqueN(SBEID)/selected_firms_2[MNE_data_type == "always_MNE", uniqueN(SBEID)]*100), by=nr_spikes][order(nr_spikes),]

colnames(tt) <- c("Automation spikes", "Firms", "Share of firms")
print(xtable(tt, caption = "Distribution of automation spikes in MNEs.",
             digits = 2),
      include.rownames = FALSE,
      file = paste0(map_output_here, "table_automation_spike_frequency_always_MNE.tex"), 
      table.placement="H",
      label = "tab:spikes_MNE",
      size = "\\footnotesize",
      booktabs = T,
      caption.placement = "top",
      sanitize.text.function = function(x) {
        x <- gsub(">=", "\\geq ", x, fixed=T)
        x <- gsub("<=", "\\leq ", x, fixed=T)
        x <- gsub(">", "\\textgreater ", x, fixed=T)
        x <- gsub("<", "\\textless ", x, fixed=T)
        return(x)
      })

#### automation spike frequency by always domestic in 2010-2021 ####
tt <- selected_firms_2[MNE_data_type == "always_domestic", .(firms = uniqueN(SBEID),
                                                             share_firms = uniqueN(SBEID)/selected_firms_2[MNE_data_type == "always_domestic", uniqueN(SBEID)]*100), by=nr_spikes][order(nr_spikes),]
tt[, nr_spikes := fifelse(nr_spikes<999, paste(nr_spikes), "automation event <= 2010")]

colnames(tt) <- c("Automation spikes", "Firms", "Share of firms")
print(xtable(tt, caption = "Distribution of automation spikes in Domestic firms.",
             digits = 2),
      include.rownames = FALSE,
      file = paste0(map_output_here, "table_automation_spike_frequency_always_DOM.tex"), 
      table.placement="H",
      label = "tab:spikes_DOM",
      size = "\\footnotesize",
      booktabs = T,
      caption.placement = "top",
      sanitize.text.function = function(x) {
        x <- gsub(">=", "\\geq ", x, fixed=T)
        x <- gsub("<=", "\\leq ", x, fixed=T)
        x <- gsub(">", "\\textgreater ", x, fixed=T)
        x <- gsub("<", "\\textless ", x, fixed=T)
        return(x)
      })

##################################################################################
### plot automation cost share / automation, total cost around first event (balanced panel of firms)  ####
selected_firms_2[, auto_event_time := year-auto_change_year_ps]
#firms <- selected_firms_2[auto_ps_data_type %in% c("auto_adaptor") & auto_event_time %in% -3:4, unique(SBEID)]

# NOTE: THIS IS FOR A BALANCED PANEL OF FIRMS (i.e. excluding firms with missings)
firms <- selected_firms_2[auto_ps_data_type %in% c("auto_adaptor") & auto_event_time %in% -3:3, sum(!is.na(auto_cost_share)) == 7, by=SBEID][V1==T, SBEID]

### plot automation cost share around first event (balanced panel) ####
tt <- selected_firms_2[SBEID %in% firms, .(auto_cost_share=mean(auto_cost_share*100, na.rm = TRUE),
                                           se_auto_cost_share = sd(auto_cost_share*100, na.rm = TRUE)/sqrt(.N)),
                       by=year-auto_change_year_ps][year %in% -3:3,][order(year),]
ggplot(data=tt, aes(x=year, y=auto_cost_share)) + 
  geom_point() + geom_line() + 
  geom_ribbon(aes(ymin=auto_cost_share-1.96*se_auto_cost_share, ymax=auto_cost_share+1.96*se_auto_cost_share), alpha=0.2) +
  scale_x_continuous(breaks=-3:4) + 
  xlab("Years to automation event") + ylab("Automation cost share (%) and 95% Conf. Int.") + theme_bw() +
  theme(panel.grid = element_blank(), panel.border = element_rect(colour = "black", size = 1.1, fill = NA)) +
  scale_colour_grey(end = 0.3, start = 0.7) +
  scale_fill_grey(end = 0.3, start = 0.7) 
ggsave(filename = paste0(map_output_here, "plot_auto_cost_share_event.pdf"), plot=last_plot(),
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))

# For presentation
ggsave(filename = paste0(map_output_presentation, "plot_auto_cost_share_event.pdf"), plot=last_plot(), 
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(6*9/16, "inches"))

### plot automation cost and total cost around first event ####
tt <- selected_firms_2[SBEID %in% firms & !is.na(automation_cost), 
                       .(automation_cost_real = mean(automation_cost*factor),
                         se_automation_cost_real = sd(automation_cost*factor)/sqrt(.N),
                         total_cost_real = mean((total_cost-automation_cost)*factor), 
                       se_total_cost_real = sd((total_cost-automation_cost)*factor)/sqrt(.N)),
                       by=year-auto_change_year_ps][year %in% -3:3,][order(year),]
scale <- 0.23*tt[, mean(total_cost_real/automation_cost_real)]
tt[, total_cost_real := total_cost_real/scale]
tt[, se_total_cost_real := se_total_cost_real/scale]

ggplot(data=tt, aes(x=year)) + 
  geom_point(aes(y=automation_cost_real), color="darkgrey") + geom_line(aes(y=automation_cost_real), color="darkgrey") +
  geom_ribbon(aes(fill = "darkgrey", ymin=automation_cost_real-1.96*se_automation_cost_real, ymax=automation_cost_real+1.96*se_automation_cost_real), alpha=0.2) +
  geom_point(aes(y=total_cost_real), color="black") + geom_line(aes(y=total_cost_real), color="black") +
  geom_ribbon(aes(fill = "black", ymin=total_cost_real-1.96*se_total_cost_real, ymax=total_cost_real+1.96*se_total_cost_real), alpha=0.2) +
  scale_y_continuous(name = "Real automation cost (1000 EURs) and 95% Conf. Int.", sec.axis = sec_axis(~ . * scale, name = "Real total cost (1000 EURs) and 95% Conf. Int.")) +
  scale_x_continuous(breaks = -3:3) +
  scale_fill_manual(values = c("black", "darkgrey"), labels = c("Total cost (excluding automation cost)", "Automation cost")) + 
  xlab("Years to automation event") + theme_bw() + 
  theme(legend.position = "bottom", legend.title = element_blank(), panel.grid = element_blank(), panel.border = element_rect(colour = "black", size = 1.1, fill = NA))
ggsave(filename = paste0(map_output_here, "plot_total_auto_cost_event.pdf"), plot=last_plot(),
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))

##################################################################################
### Automation cost / pw around event  ####
tt <- selected_firms_2[SBEID %in% firms & !is.na(automation_cost), 
                       .(real_automation_cost_pw = mean(real_automation_cost_pw),
                         se_real_automation_cost_pw = sd(real_automation_cost_pw)/sqrt(.N)),
                       by=year-auto_change_year_ps][year %in% -3:3,][order(year),]

ggplot(data=tt, aes(x=year)) + 
  geom_point(aes(y=real_automation_cost_pw)) + geom_line(aes(y=real_automation_cost_pw)) +
  geom_ribbon(aes(ymin=real_automation_cost_pw-1.96*se_real_automation_cost_pw, ymax=real_automation_cost_pw+1.96*se_real_automation_cost_pw), alpha=0.2) +
  scale_y_continuous(name = "Real automation cost per worker (1000 EURs) and 95% Conf. Int.") +
  scale_x_continuous(breaks = -3:3) +
  xlab("Years to automation event") + 
  theme_bw() + 
  theme(legend.position = "bottom", legend.title = element_blank(), panel.grid = element_blank(), panel.border = element_rect(colour = "black", size = 1.1, fill = NA))

ggsave(filename = paste0(map_output_here, "plot_auto_cost_per_worker_event.pdf"), plot=last_plot(),
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))

##################################################################################
### Machinery / pw around event  ####
selected_firms_2[, Machinery_event_time := year-change_year_Machinery_inv]
firms <- selected_firms_2[invest_data==T & Machinery_event_time %in% -3:3, sum(!is.na(real_Machinery_inv_pw)) == 7, by=SBEID][V1==T, SBEID]

tt <- selected_firms_2[SBEID %in% firms & !is.na(real_Machinery_inv_pw), 
                       .(real_automation_cost_pw = mean(real_Machinery_inv_pw),
                         se_real_automation_cost_pw = sd(real_Machinery_inv_pw)/sqrt(.N)),
                       by=Machinery_event_time][Machinery_event_time %in% -3:3,][order(Machinery_event_time),]

ggplot(data=tt, aes(x=Machinery_event_time)) + 
  geom_point(aes(y=real_automation_cost_pw)) + geom_line(aes(y=real_automation_cost_pw)) +
  geom_ribbon(aes(ymin=real_automation_cost_pw-1.96*se_real_automation_cost_pw, ymax=real_automation_cost_pw+1.96*se_real_automation_cost_pw), alpha=0.2) +
  scale_y_continuous(name = "Real machinery cost per worker (1000 EURs) and 95% Conf. Int.") +
  scale_x_continuous(breaks = -3:3) +
  xlab("Years to machinery investment event") + 
  theme_bw() + 
  theme(legend.position = "bottom", legend.title = element_blank(), panel.grid = element_blank(), panel.border = element_rect(colour = "black", size = 1.1, fill = NA))

ggsave(filename = paste0(map_output_here, "plot_machinery_per_worker_event.pdf"), plot=last_plot(),
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))

##################################################################################
### IT / pw around event  ####
selected_firms_2[, IT_event_time := year-change_year_IT_inv]
firms <- selected_firms_2[invest_data==T  & IT_event_time %in% -3:3, sum(!is.na(real_IT_inv_pw)) == 7, by=SBEID][V1==T, SBEID]

tt <- selected_firms_2[SBEID %in% firms & !is.na(real_IT_inv_pw), 
                       .(real_automation_cost_pw = mean(real_IT_inv_pw),
                         se_real_automation_cost_pw = sd(real_IT_inv_pw)/sqrt(.N)),
                       by=IT_event_time][IT_event_time %in% -3:3,][order(IT_event_time),]

ggplot(data=tt, aes(x=IT_event_time)) + 
  geom_point(aes(y=real_automation_cost_pw)) + geom_line(aes(y=real_automation_cost_pw)) +
  geom_ribbon(aes(ymin=real_automation_cost_pw-1.96*se_real_automation_cost_pw, ymax=real_automation_cost_pw+1.96*se_real_automation_cost_pw), alpha=0.2) +
  scale_y_continuous(name = "Real IT cost per worker (1000 EURs) and 95% Conf. Int.") +
  scale_x_continuous(breaks = -3:3) +
  xlab("Years to IT investment event") + 
  theme_bw() + 
  theme(legend.position = "bottom", legend.title = element_blank(), panel.grid = element_blank(), panel.border = element_rect(colour = "black", size = 1.1, fill = NA))

ggsave(filename = paste0(map_output_here, "plot_IT_per_worker_event.pdf"), plot=last_plot(),
       scale = 1.2, dpi = 300,
       width=unit(6, "inches"), height=unit(4.5, "inches"))
