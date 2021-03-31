library(epiCata)
library(epiCataPlot)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyverse)
library(ggplot2)
library(purrr)
library(gtable)
library(lemon)

# from https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2
shift_legend3 <- function(p) {
  pnls <- cowplot::plot_to_gtable(p) %>% gtable::gtable_filter("panel") %>%
    with(setNames(grobs, layout$name)) %>% purrr::keep(~identical(.x,zeroGrob()))
  
  if( length(pnls) == 0 ) stop( "No empty facets in the plot" )
  
  lemon::reposition_legend( p, "center", panel=names(pnls) )
}

rmse <- function(gt, pred){
  sqrt(mean((gt - pred)^2))
}

mae <- function(gt, pred){
  mean(abs(gt - pred))
}

maed <- function(gt, pred){
  err <- abs(gt - pred)
  mean(abs(err-mean(err)))
}

get_metrics <- function(gt, p_mean, p_lower, p_upper, idx=NULL){
  if(!is.null(idx)) {
    gt <- gt[idx]
    p_mean <- p_mean[idx]
    p_lower <- p_lower[idx]
    p_upper <- p_upper[idx]
  }
  list(
    "rmse" = rmse(gt, p_mean),
    "rmse_conf" = mean(rmse(gt, p_lower), rmse(gt, p_upper)),
    "mae" = mae(gt, p_mean),
    "mae_conf" = mean(mae(gt, p_lower), mae(gt, p_upper)),
    "maed" = maed(gt, p_mean),
    "maed_conf" = mean(maed(gt, p_lower), maed(gt, p_upper))
  )
}

models_filenames = list()
for(dirpath in list.dirs("./results")[-1]){
  for(fname in list.files(dirpath, full.names = TRUE, include.dirs = FALSE, no.. = TRUE)){
    models_filenames <- append(models_filenames, fname)
  }
}
models_filenames <- unlist(models_filenames)

ground_truth = read.csv("ground_truth.csv") %>%
  rename(time=data_ocorrencia) %>%
  select(-casos)
ground_truth$time = ymd(ground_truth$time)

stats_list = list(
  name=list(),
  location_name=list(),
  reference_date=list(),
  rmse7=list(),
  rmse_conf7=list(),
  rmse30=list(),
  rmse_conf30=list(),
  mae7=list(),
  mae_conf7=list(),
  mae30=list(),
  mae_conf30=list(),
  maed7=list(),
  maed_conf7=list(),
  maed30=list(),
  maed_conf30=list()
)

ardf <- data.frame(
  model_name = c(),
  residuals = c(),
  locations = c(),
  idxs <- c()
)

for(fname in models_filenames){
  cat("Loading file", fname)
  load(fname)
  near_idx <- 2:8
  long_idx <- 2:31
  forecast <- 30
  
  if(grepl("extended", model_output$model_name, fixed=TRUE)) {
    near_idx <- near_idx + 7
    long_idx <- long_idx + 7
    forecast <- forecast + 7
  }
  
  for(location in model_output$stan_list$available_locations){
    fdf <- epiCata::get_merged_forecast_dfs(location, model_output, forecast=forecast)$data_location_forecast %>%
      left_join(ground_truth)
    ardf <- bind_rows(ardf,
                      data.frame(
                        model_name = rep(model_output$model_name, length(long_idx)),
                        residuals = abs(fdf$estimated_deaths_forecast - fdf$obitos)[long_idx],
                        locations = rep(location, length(long_idx)),
                        idxs = long_idx - min(long_idx) + 1
                      ))
    
    err7 <- get_metrics(fdf$obitos, fdf$estimated_deaths_forecast, fdf$death_min_forecast, fdf$death_max_forecast, idx=near_idx)
    err30 <- get_metrics(fdf$obitos, fdf$estimated_deaths_forecast, fdf$death_min_forecast, fdf$death_max_forecast, idx=long_idx)
    stats_list$name <- append(stats_list$name, model_output$model_name)
    stats_list$location_name <- append(stats_list$location_name, location)
    stats_list$reference_date <- append(stats_list$reference_date, model_output$reference_date_str)
    stats_list$rmse7 <- append(stats_list$rmse7, err7[["rmse"]])
    stats_list$rmse_conf7 <- append(stats_list$rmse_conf7, err7[["rmse_conf"]])
    stats_list$rmse30 <- append(stats_list$rmse30, err30[["rmse"]])
    stats_list$rmse_conf30 <- append(stats_list$rmse_conf30, err30[["rmse_conf"]])
    stats_list$mae7 <- append(stats_list$mae7, err7[["mae"]])
    stats_list$mae_conf7 <- append(stats_list$mae_conf7, err7[["mae_conf"]])
    stats_list$mae30 <- append(stats_list$mae30, err30[["mae"]])
    stats_list$mae_conf30 <- append(stats_list$mae_conf30, err30[["mae_conf"]])
    stats_list$maed7 <- append(stats_list$maed7, err7[["maed"]])
    stats_list$maed_conf7 <- append(stats_list$maed_conf7, err7[["maed_conf"]])
    stats_list$maed30 <- append(stats_list$maed30, err30[["maed"]])
    stats_list$maed_conf30 <- append(stats_list$maed_conf30, err30[["maed_conf"]])
  }
  model_data <- NULL
  #break
}

stats_df <- data.frame(
  name = unlist(stats_list$name),
  location_name = unlist(stats_list$location_name),
  reference_date = unlist(stats_list$reference_date),
  rmse7 = unlist(stats_list$rmse7),
  rmse_conf7 = unlist(stats_list$rmse_conf7),
  rmse30 = unlist(stats_list$rmse30),
  rmse_conf30 = unlist(stats_list$rmse_conf30),
  mae7 = unlist(stats_list$mae7),
  mae_conf7 = unlist(stats_list$mae_conf7),
  mae30 = unlist(stats_list$mae30),
  mae_conf30 = unlist(stats_list$mae_conf30),
  maed7 = unlist(stats_list$maed7),
  maed_conf7 = unlist(stats_list$maed_conf7),
  maed30 = unlist(stats_list$maed30),
  maed_conf30 = unlist(stats_list$maed_conf30)
  )

write.csv(stats_df, "stats.csv")
write.csv(ardf, "ardf.csv")

stats_df <- read.csv("stats.csv")
ardf <- read.csv("ardf.csv")

ardf$model_name <- ardf$model_name %>%
  str_replace_all("base-reported-no-overestimate-retroactive-pre", "base-rnr") %>%
  str_replace_all("base-reported-no-overestimate", "base-rnn") %>%
  str_replace_all("base-reported-retroactive-pre", "base-ror") %>%
  str_replace_all("base-reported", "base-ron")

stats_df$name <- stats_df$name %>%
  str_replace_all("base-reported-no-overestimate-retroactive-pre", "base-rnr") %>%
  str_replace_all("base-reported-no-overestimate", "base-rnn") %>%
  str_replace_all("base-reported-retroactive-pre", "base-ror") %>%
  str_replace_all("base-reported", "base-ron")

# By (name, location_name)
region_stats_df <- stats_df %>%
  group_by(name, location_name) %>%
  summarise(rmse7=mean(rmse7),
            rmse_conf7=mean(rmse_conf7),
            rmse30=mean(rmse30),
            rmse_conf30=mean(rmse_conf30),
            mae7=mean(mae7),
            mae_conf7=mean(mae_conf7),
            mae30=mean(mae30),
            mae_conf30=mean(mae_conf30),
            maed7=mean(maed7),
            maed_conf7=mean(maed_conf7),
            maed30=mean(maed30),
            maed_conf30=mean(maed_conf30))
# By (name)
agg_stats_df <- stats_df %>%
  group_by(name) %>%
  summarise(rmse7=mean(rmse7),
            rmse_conf7=mean(rmse_conf7),
            rmse30=mean(rmse30),
            rmse_conf30=mean(rmse_conf30),
            mae7=mean(mae7),
            mae_conf7=mean(mae_conf7),
            mae30=mean(mae30),
            mae_conf30=mean(mae_conf30),
            maed7=mean(maed7),
            maed_conf7=mean(maed_conf7),
            maed30=mean(maed30),
            maed_conf30=mean(maed_conf30))
# By (name, reference_date)
date_stats_df <- stats_df %>%
  group_by(name, reference_date) %>%
  summarise(rmse7=mean(rmse7),
            rmse_conf7=mean(rmse_conf7),
            rmse30=mean(rmse30),
            rmse_conf30=mean(rmse_conf30),
            mae7=mean(mae7),
            mae_conf7=mean(mae_conf7),
            mae30=mean(mae30),
            mae_conf30=mean(mae_conf30),
            maed7=mean(maed7),
            maed_conf7=mean(maed_conf7),
            maed30=mean(maed30),
            maed_conf30=mean(maed_conf30))
# 

region_stats_df

date_stats_df

agg_stats_df


agg_stats_df %>%
  select(c(name,rmse7,rmse_conf7,rmse30,rmse_conf30)) %>%
  write.csv("table1-1.csv")

agg_stats_df %>%
  mutate(mae7_val = paste0(round(mae7,2), " (±", round(maed7,2), ")"),
         mae7_conf_val = paste0(round(mae_conf7,2), " (±", round(maed_conf7,2), ")"),
         mae30_val = paste0(round(mae30,2), " (±", round(maed30,2), ")"),
         mae30_conf_val = paste0(round(mae_conf30,2), " (±", round(maed_conf30,2), ")"),
         ) %>%
  select(c(name,mae7_val,mae7_conf_val,mae30_val,mae30_conf_val)) %>%
  write.csv("table1-2.csv")

region_stats_df %>%
  select(c(location_name,name,rmse7,rmse_conf7,rmse30,rmse_conf30)) %>%
  arrange(location_name, name) %>%
  write.csv("table2-1.csv")

region_stats_df %>%
  mutate(mae7_val = paste0(round(mae7,2), " (±", round(maed7,2), ")"),
         mae7_conf_val = paste0(round(mae_conf7,2), " (±", round(maed_conf7,2), ")"),
         mae30_val = paste0(round(mae30,2), " (±", round(maed30,2), ")"),
         mae30_conf_val = paste0(round(mae_conf30,2), " (±", round(maed_conf30,2), ")"),
  ) %>%
  select(c(location_name,name,mae7_val,mae7_conf_val,mae30_val,mae30_conf_val)) %>%
  arrange(location_name, name) %>%
  write.csv("table2-2.csv")

date_stats_df %>%
  select(c(reference_date,name,rmse7,rmse_conf7,rmse30,rmse_conf30)) %>%
  arrange(reference_date, name) %>%
  write.csv("table3-1.csv")

date_stats_df %>%
  mutate(mae7_val = paste0(round(mae7,2), " (±", round(maed7,2), ")"),
         mae7_conf_val = paste0(round(mae_conf7,2), " (±", round(maed_conf7,2), ")"),
         mae30_val = paste0(round(mae30,2), " (±", round(maed30,2), ")"),
         mae30_conf_val = paste0(round(mae_conf30,2), " (±", round(maed_conf30,2), ")"),
  ) %>%
  select(c(reference_date,name,mae7_val,mae7_conf_val,mae30_val,mae30_conf_val)) %>%
  arrange(reference_date, name) %>%
  write.csv("table3-2.csv")

plot_ribbon <- TRUE

dsb_colours <- c("#ED297C", "#008FD5", "#FF2700", "#FFAB40", "#810F7C", "#619656",
             "#11C8ED", "#ED5434", "#11ED98", "#C61EF7", "#78342A", "#FC5900",
             "#AB5F00", "#414D91", "#032738", "#193AF7", "#243820")
wong_colours <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#0072B2",
                  "#D55E00", "#F0E442")
tol_colours <- c("#332288", "#117733", "#44AA99", "#88CCEE", "#DDCC77", "#CC6677",
                  "#AA4499", "#882255")
colours <- wong_colours

unique_model_names <- unique(ardf$model_name)
model_colour_mapping = as.array(colours[1:length(unique_model_names)])
names(model_colour_mapping) = unique_model_names
model_linetype_mapping = 1:length(unique_model_names)
names(model_linetype_mapping) = unique_model_names

unique_location_names <- sort(unique(ardf$locations))
location_colour_mapping = as.array(colours[1:length(unique_location_names)])
names(location_colour_mapping) = unique_location_names
location_linetype_mapping = 1:length(unique_location_names)
names(location_linetype_mapping) = unique_location_names

unique_region_names <- unique_location_names %>%
  str_replace_all("SC_MAC_", "") %>%
  str_replace_all("SC_RSA_", "") %>%
  str_replace_all("_", " ")
region_colour_mapping = as.array(colours[1:length(unique_region_names)])
names(region_colour_mapping) = unique_region_names
region_linetype_mapping = 1:length(unique_region_names)
names(region_linetype_mapping) = unique_region_names

plot_df <- ardf %>%
  group_by(model_name, idxs) %>%
  summarise(residuals_upper = mean(residuals) + sd(residuals),
            residuals_lower = max(mean(residuals) - sd(residuals),0),
            residuals = mean(residuals))
plot_df[["model_name"]] <- as.character(plot_df[["model_name"]])

plot_df[["colour"]] <- unlist(map(plot_df[["model_name"]],
                         function(x){
                           colours[[which(x==unique_model_names)[[1]]]]
                         }))

write.csv(plot_df, "plot_df.csv")

plot_theme <- theme_minimal() + theme(legend.position = "top")

p <- ggplot(plot_df,
            aes(x=idxs, y=residuals, ymax=residuals_upper, ymin=residuals_lower,
                colour=model_name, fill=model_name, group=model_name, linetype=model_name, shape=model_name)) +
  scale_colour_manual(values=model_colour_mapping,
                      name="Model Name",
                      breaks=names(model_colour_mapping)) +
  scale_linetype_manual(values=model_linetype_mapping,
                        name="Model Name",
                        breaks=names(model_linetype_mapping)) +
  scale_shape_manual(values=model_linetype_mapping,
                     name="Model Name",
                     breaks=names(model_linetype_mapping)) +
  scale_fill_manual(values=model_colour_mapping,
                    name="Model Name",
                    breaks=names(model_colour_mapping)) +
  scale_y_continuous(name = "Residuals") +
  scale_x_continuous(name = "Forecasted Day") +
  guides(fill=FALSE, colour=guide_legend(nrow=2)) +
  plot_theme


this_plot_df <- plot_df
pp <- p +
  geom_line(data=this_plot_df)# + geom_point(data=this_plot_df)
if(plot_ribbon){
  pp = pp +
    geom_ribbon(data=this_plot_df, alpha=0.1)
}
pp
ggsave(paste0("all",".pdf"), pp, width=20, height=10, unit="cm")
ggsave(paste0("all",".png"), pp, width=20, height=10, unit="cm")

facet_pp <- shift_legend3(
  pp + 
    guides(fill=FALSE, colour=guide_legend(nrow=5)) +
    facet_wrap("model_name") 
  
)
facet_pp
ggsave(paste0("all-facet",".pdf"), facet_pp, width=20, height=10, unit="cm")
ggsave(paste0("all-facet",".png"), facet_pp, width=20, height=10, unit="cm")

for(this_model_name in unique_model_names) {
  print(this_model_name)
  this_plot_df <- plot_df %>%
    filter(model_name==this_model_name)
  
  pp <- p +
    geom_line(data=this_plot_df)
  if(plot_ribbon){
    pp = pp + geom_ribbon(data=this_plot_df, alpha=0.1)
  }
  
  print("Saving Graphs")
  ggsave(paste0(this_model_name,".pdf"), pp, width=20, height=10, unit="cm")
  ggsave(paste0(this_model_name,".png"), pp, width=20, height=10, unit="cm")
}

date_plot_df <- date_stats_df %>%
  mutate(reference_date = ymd(reference_date))
timeline_p <- ggplot(date_plot_df,
                     aes(x=reference_date,
                         colour=name, group=name, linetype=name, shape=name)) +
  scale_colour_manual(values=model_colour_mapping,
                      name="Model Name",
                      breaks=names(model_colour_mapping)) +
  scale_linetype_manual(values=model_linetype_mapping,
                        name="Model Name",
                        breaks=names(model_linetype_mapping)) +
  scale_shape_manual(values=model_linetype_mapping,
                     name="Model Name",
                     breaks=names(model_linetype_mapping)) +
  scale_fill_manual(values=model_colour_mapping,
                    name="Model Name",
                    breaks=names(model_colour_mapping)) +
  scale_x_date(name = "", date_breaks="2 weeks") + 
  guides(fill=FALSE, colour=guide_legend(nrow=1)) +
  plot_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

timeline_pp <- timeline_p +
  geom_line(aes(y=rmse7))
timeline_pp
ggsave(paste0("timeline-rmse7",".pdf"), timeline_pp, width=20, height=10, unit="cm")
ggsave(paste0("timeline-rmse7",".png"), timeline_pp, width=20, height=10, unit="cm")

timeline_pp <- timeline_p +
  geom_line(aes(y=rmse30))
ggsave(paste0("timeline-rmse30",".pdf"), timeline_pp, width=20, height=10, unit="cm")
ggsave(paste0("timeline-rmse30",".png"), timeline_pp, width=20, height=10, unit="cm")




retroactive_df <- read.csv("retroactive.csv")

p <- ggplot(retroactive_df %>% filter(day>0),
            aes(x=day, colour=region, group=region, linetype=region, shape=region)) +
  scale_colour_manual(values=region_colour_mapping,
                      name="Region Name",
                      breaks=names(region_colour_mapping)) +
  scale_linetype_manual(values=region_linetype_mapping,
                        name="Region Name",
                        breaks=names(region_linetype_mapping)) +
  scale_shape_manual(values=region_linetype_mapping,
                     name="Region Name",
                     breaks=names(region_linetype_mapping)) +
  scale_fill_manual(values=region_colour_mapping,
                    name="Region Name",
                    breaks=names(region_colour_mapping)) +
  scale_x_continuous(name = "Days after snapshot", breaks=0:7) +
  guides(fill=FALSE, colour=guide_legend(nrow=4)) +
  plot_theme
pp <- p +
  geom_line(aes(y=obitos_delta_pct_avg), size=1.2, alpha=0.7) +
  scale_y_continuous(name = "Avg. retroactive increase in deaths", labels=scales::percent)
pp
ggsave(paste0("retroactive-deaths-all",".pdf"), pp, width=20, height=10, unit="cm")
ggsave(paste0("retroactive-deaths-all",".png"), pp, width=20, height=10, unit="cm")

facet_pp <- shift_legend3(
  pp + 
    facet_wrap("region") +
    theme(
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 7),
      strip.text.x = element_text(size = 7)
    )
)

ggsave(paste0("retroactive-deaths-all-facet",".pdf"), facet_pp, width=20, height=10, unit="cm")
ggsave(paste0("retroactive-deaths-all-facet",".png"), facet_pp, width=20, height=10, unit="cm")


pp <- p +
  geom_line(aes(y=casos_delta_pct_avg), size=1.2, alpha=0.7) +
  scale_y_continuous(name = "Avg. retroactive increase in cases", labels=scales::percent)
ggsave(paste0("retroactive-cases-all",".pdf"), pp, width=20, height=10, unit="cm")
ggsave(paste0("retroactive-cases-all",".png"), pp, width=20, height=10, unit="cm")

facet_pp <- shift_legend3(
  pp + 
    facet_wrap("region") +
    theme(
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 7),
      strip.text.x = element_text(size = 7)
    )
)

ggsave(paste0("retroactive-cases-all-facet",".pdf"), facet_pp, width=20, height=10, unit="cm")
ggsave(paste0("retroactive-cases-all-facet",".png"), facet_pp, width=20, height=10, unit="cm")


p <- ggplot(retroactive_df,
            aes(x=day, colour=region, group=region, linetype=region, shape=region)) +
  scale_colour_manual(values=region_colour_mapping,
                      name="Region Name",
                      breaks=names(region_colour_mapping)) +
  scale_linetype_manual(values=region_linetype_mapping,
                        name="Region Name",
                        breaks=names(region_linetype_mapping)) +
  scale_shape_manual(values=region_linetype_mapping,
                     name="Region Name",
                     breaks=names(region_linetype_mapping)) +
  scale_fill_manual(values=region_colour_mapping,
                    name="Region Name",
                    breaks=names(region_colour_mapping)) +
  scale_x_continuous(name = "Days after snapshot", breaks=0:7) +
  guides(fill=FALSE, colour=guide_legend(nrow=4)) +
  plot_theme

pp <- p +
  geom_line(aes(y=obitos_pct_avg), size=1.2, alpha=0.7) +
  scale_y_continuous(name = "Avg. cum. retroactive increase in deaths", labels=scales::percent, limits=c(0,NA))
pp
ggsave(paste0("retroactive-cum-deaths-all",".pdf"), pp, width=20, height=10, unit="cm")
ggsave(paste0("retroactive-cum-deaths-all",".png"), pp, width=20, height=10, unit="cm")

facet_pp <- shift_legend3(
  pp + 
    facet_wrap("region") +
    theme(
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 7),
      strip.text.x = element_text(size = 7)
    )
)

ggsave(paste0("retroactive-cum-deaths-all-facet",".pdf"), facet_pp, width=20, height=10, unit="cm")
ggsave(paste0("retroactive-cum-deaths-all-facet",".png"), facet_pp, width=20, height=10, unit="cm")


pp <- p +
  geom_line(aes(y=casos_pct_avg), size=1.2, alpha=0.7) +
  scale_y_continuous(name = "Avg. cum. retroactive increase in cases", labels=scales::percent, limits=c(0,NA))
ggsave(paste0("retroactive-cum-cases-all",".pdf"), pp, width=20, height=10, unit="cm")
ggsave(paste0("retroactive-cum-cases-all",".png"), pp, width=20, height=10, unit="cm")

facet_pp <- shift_legend3(
  pp + 
    facet_wrap("region") +
    theme(
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 7),
      strip.text.x = element_text(size = 7)
    )
)

ggsave(paste0("retroactive-cum-cases-all-facet",".pdf"), facet_pp, width=20, height=10, unit="cm")
ggsave(paste0("retroactive-cum-cases-all-facet",".png"), facet_pp, width=20, height=10, unit="cm")




load("results/2021_02_01/2021_02_01_base-reported_MAC+AGG-DEVELOP-1200-600-4-0.950000-8_574327-stanfit.Rdata")

overestimate_df <- data.frame(
  locations = c(),
  chains = c(),
  overestimate = c()
)

for(location in model_output$stan_list$available_locations) {
  i <- which(model_output$stan_list$available_locations == location)
  for(chain in 1:4) {
    chain_start = (chain-1)*600 + 1
    chain_end = chain*600
    this_odf <- data.frame(
      locations = rep(
        location  %>%
          str_replace_all("SC_MAC_", "") %>%
          str_replace_all("SC_RSA_", "") %>%
          str_replace_all("_", " "),
        dim(model_output$out$infection_overestimate)[[1]]
      ),
      chains=chain,
      overestimate = model_output$out$infection_overestimate[chain_start:chain_end,i]
    )
    cat(chain, " -> ", chain_start, ":", chain_end, "\n")
    
    overestimate_df <- bind_rows(overestimate_df, this_odf)
  }
}

p <- ggplot(overestimate_df, aes(x=overestimate)) +
  plot_theme

pp <- p + 
  geom_density()
ggsave(paste0("overestimate-all",".pdf"), pp, width=20, height=10, unit="cm")
ggsave(paste0("overestimate-all",".png"), pp, width=20, height=10, unit="cm")

pp <- p + 
  geom_density() +
  facet_wrap("locations")
ggsave(paste0("overestimate-all-facet",".pdf"), pp, width=20, height=10, unit="cm")
ggsave(paste0("overestimate-all-facet",".png"), pp, width=20, height=10, unit="cm")

pp <- p + 
  geom_density() +
  facet_grid(vars(locations), vars(chains)) +
  theme(strip.text.y = element_text(angle = 0))
ggsave(paste0("overestimate-all-facetgrid",".pdf"), pp, width=20, height=15, unit="cm")
ggsave(paste0("overestimate-all-facetgrid",".png"), pp, width=20, height=15, unit="cm")

