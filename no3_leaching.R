#no3 concentrations
df <- read_csv("~/Dropbox/Lab data/S Willamette GWMA Dropbox/Tidy Data/gradient_plot_lysimter_data.csv") %>% 
  filter(compound =='no3') %>% 
  group_by(field,treatment)

df <- rename(gradient_plot_lysimter_data, plot = "treatment")
df$field <- as.factor(gradient_plot_lysimter_data$field)
df$plot <- as.factor(gradient_plot_lysimter_data$plot)
df <- df[,-1]

df %>% 
  group_by(date, field) %>% 
  summarise(tally(date))

write.csv(df, "C:/Users/twilli2/Dropbox/Lab data/S Willamette GWMA Dropbox/no3_raw_data.csv")

leach <- read_csv("~/Dropbox/Lab data/S Willamette GWMA Dropbox/no3_raw_data.csv",
col_types = cols(X1 = col_skip(), compound = col_skip(),
date = col_date(format = "%m/%d/%Y"),
field = col_factor(levels = c("1",
"2", "3", "4")), group = col_skip(),
lys_num = col_skip(), plot = col_skip(),
season = col_skip()))

drainage <- rename(drainage, date = sample_date)
no3_leach<- left_join(leach,drainage, by = c("date","field"))
no3_leach <- no3_leach %>% 
  mutate(Dm = Dmm/1000) %>% 
  mutate(conc = concentration/10^3) %>% 
  mutate(kgN = Dm * conc) %>% 
  mutate(dgN_ha = kgN*10^4)

no3_leach_final <- no3_leach %>%
  filter(date <= '2019-07-16' & date >= '2018-07-05') %>% 
  group_by(field,treatment) %>% 
  summarize(total_no3_n_kg = sum(dgN_ha,na.rm=T)) 

no3_leach_final$treatment <- as.factor(no3_leach_final$treatment)
total_n_added_s2<- total_n %>% 
  filter(season == 2) %>% 
  rename(treatment = plot) %>% 
  rename(total_n_added_kg=total_n)
season_2_n_budget <- left_join(no3_leach_final, total_flux_by_season) %>% 
  select(1,2,3,6)
season_2_n_budget <- left_join(season_2_n_budget,total_n_added_s2)
season_2_n_budget %>% 
  select(1,2,6,3,4) %>% 
  mutate(percent_loss = (total_no3_n_kg+total_n2o_n_season_kg)/total_n_added_kg)
season_2_n_budget <- left_join(season_2_n_budget, plant_n_estimate_kg)
season_2_n_budget <- season_2_n_budget %>% 
  select(1,2,3,4,6,8)

#should flux just be sampling days or full year?
#correct for background (zero) plot?
t<- flextable(season_2_n_budget)
t <- theme_vanilla(t)
t <- merge_v(t, j = "Field")
t
doc <- read_docx()
doc <- body_add_flextable(doc, value = t)
print(doc, target = "n_budget_table.docx")
