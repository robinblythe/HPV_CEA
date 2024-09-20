data<- read.csv("nrdo data/1968-2021 cancer staple dataset.csv",header = T)

lapply(data, class)

# filter year
data_yr_filtered <- data[data$year_of_diagnosis>="2003",]

# unique(data_yr_filtered$age_at_diagnosis) |>  tail(100)
# data_yr_filtered[data_yr_filtered$age_at_diagnosis=="-",] |>  View()

# Clean age at diagnosis
data_yr_filtered$age_at_diagnosis <- as.numeric(data_yr_filtered$age_at_diagnosis)|> round()
data_yr_filtered$age_at_diagnosis |> unique()




data_yr_filtered$year_of_diagnosis <- as.numeric(data_yr_filtered$year_of_diagnosis)

age_range <- c(0,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,Inf)
age_cat <- c("<10","10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
             "60-64", "65-69", "70-74", "75-79", "80-84", ">85")

data_yr_filtered$age_agg <- cut(data_yr_filtered$age_at_diagnosis, breaks =age_range, labels =age_cat,right = T   )

data_yr_filtered$year_of_diagnosis |> unique()
data_yr_filtered$year_agg <- " "

data_yr_filtered$year_agg[data_yr_filtered$year_of_diagnosis<=2007] <- "2003-2007"
data_yr_filtered$year_agg[data_yr_filtered$year_of_diagnosis>2007 & data_yr_filtered$year_of_diagnosis<=2012 ] <- "2008-2012"
data_yr_filtered$year_agg[data_yr_filtered$year_of_diagnosis>2012 & data_yr_filtered$year_of_diagnosis<=2017 ] <- "2013-2017"
data_yr_filtered$year_agg[data_yr_filtered$year_of_diagnosis>2017 ] <- "2017-2021"



# filter primary sites
data_yr_filtered$pri_site |> unique()

cervix_cancer <- data_yr_filtered[
  data_yr_filtered$pri_site=="C53.0" | 
    data_yr_filtered$pri_site=="C53.1" | 
    data_yr_filtered$pri_site=="C53.8" | 
    data_yr_filtered$pri_site=="C53.9" , ]

oropharyngeal_cancer <- data_yr_filtered[
  data_yr_filtered$pri_site==  "C10.1" | 
    data_yr_filtered$pri_site=="C10.2" | 
    data_yr_filtered$pri_site=="C10.3" | 
    data_yr_filtered$pri_site=="C10.4" | 
    data_yr_filtered$pri_site=="C10.8" | 
    data_yr_filtered$pri_site=="C10.9" |
    
    data_yr_filtered$pri_site=="C14.0" |
    data_yr_filtered$pri_site=="C14.2" |
    data_yr_filtered$pri_site=="C14.8" |
    
    data_yr_filtered$pri_site=="C09.0" |
    data_yr_filtered$pri_site=="C09.1" |
    data_yr_filtered$pri_site=="C09.8" |
    data_yr_filtered$pri_site=="C09.9" |
    
    data_yr_filtered$pri_site=="C05.1" |
    data_yr_filtered$pri_site=="C05.2" |
    data_yr_filtered$pri_site=="C02.4" |
    data_yr_filtered$pri_site=="C02.8" |
    data_yr_filtered$pri_site=="C01.9" 
  
  , ]

oropharyngeal_cancer2 <- data_yr_filtered[
  data_yr_filtered$pri_site==  "C10.1" | 
    data_yr_filtered$pri_site=="C10.2" | 
    data_yr_filtered$pri_site=="C10.3" | 
    data_yr_filtered$pri_site=="C10.4" | 
    data_yr_filtered$pri_site=="C10.8" | 
    data_yr_filtered$pri_site=="C10.9" |
    
    data_yr_filtered$pri_site=="C14.0" |
    data_yr_filtered$pri_site=="C14.2" |
    data_yr_filtered$pri_site=="C14.8" |
    
    data_yr_filtered$pri_site=="C09.0" |
    data_yr_filtered$pri_site=="C09.1" |
    data_yr_filtered$pri_site=="C09.8" |
    data_yr_filtered$pri_site=="C09.9" 
  
  , ]


oropharyngeal_cancer3 <- data_yr_filtered[
  data_yr_filtered$pri_site==  "C10.1" | 
    data_yr_filtered$pri_site=="C10.2" | 
    data_yr_filtered$pri_site=="C10.3" | 
    data_yr_filtered$pri_site=="C10.4" | 
    data_yr_filtered$pri_site=="C10.8" | 
    data_yr_filtered$pri_site=="C10.9" |
    
    data_yr_filtered$pri_site=="C14.0" |
    data_yr_filtered$pri_site=="C14.2" |
    data_yr_filtered$pri_site=="C14.8" 
  
  , ]


vaginal_cancer <- data_yr_filtered[
  data_yr_filtered$pri_site==  "C51.0" | 
    data_yr_filtered$pri_site=="C51.1" | 
    data_yr_filtered$pri_site=="C51.2" | 
    data_yr_filtered$pri_site=="C51.8" | 
    data_yr_filtered$pri_site=="C51.9" | 
    data_yr_filtered$pri_site=="C52" 
  
  , ]


anal_cancer <- data_yr_filtered[
  data_yr_filtered$pri_site==  "C21.0" | 
    data_yr_filtered$pri_site=="C21.1" | 
    data_yr_filtered$pri_site=="C21.2" | 
    data_yr_filtered$pri_site=="C21.8" 
  
  , ]


penile_cancer <- data_yr_filtered[
  data_yr_filtered$pri_site==  "C60.0" | 
    data_yr_filtered$pri_site=="C60.1" | 
    data_yr_filtered$pri_site=="C60.2" | 
    data_yr_filtered$pri_site=="C60.8" |
    data_yr_filtered$pri_site=="C60.9" 
  
  , ]







all_cancer <- data_yr_filtered[
  data_yr_filtered$pri_site=="C53.0" | 
    data_yr_filtered$pri_site=="C53.1" | 
    data_yr_filtered$pri_site=="C53.8" | 
    data_yr_filtered$pri_site=="C53.9" |
    data_yr_filtered$pri_site=="C10.1" | 
    data_yr_filtered$pri_site=="C10.2" | 
    data_yr_filtered$pri_site=="C10.3" | 
    data_yr_filtered$pri_site=="C10.4" | 
    data_yr_filtered$pri_site=="C10.8" | 
    data_yr_filtered$pri_site=="C10.9" |
    
    data_yr_filtered$pri_site=="C14.0" |
    data_yr_filtered$pri_site=="C14.2" |
    data_yr_filtered$pri_site=="C14.8" |
    
    data_yr_filtered$pri_site=="C09.0" |
    data_yr_filtered$pri_site=="C09.1" |
    data_yr_filtered$pri_site=="C09.8" |
    data_yr_filtered$pri_site=="C09.9" |
    
    data_yr_filtered$pri_site=="C05.1" |
    data_yr_filtered$pri_site=="C05.2" |
    data_yr_filtered$pri_site=="C02.4" |
    data_yr_filtered$pri_site=="C02.8" |
    data_yr_filtered$pri_site=="C01.9" |
    
    data_yr_filtered$pri_site=="C51.0" | 
    data_yr_filtered$pri_site=="C51.1" | 
    data_yr_filtered$pri_site=="C51.2" | 
    data_yr_filtered$pri_site=="C51.8" | 
    data_yr_filtered$pri_site=="C51.9" | 
    data_yr_filtered$pri_site=="C52"   |
    
    data_yr_filtered$pri_site=="C21.0" | 
    data_yr_filtered$pri_site=="C21.1" | 
    data_yr_filtered$pri_site=="C21.2" | 
    data_yr_filtered$pri_site=="C21.8" |
    
    data_yr_filtered$pri_site=="C60.0" | 
    data_yr_filtered$pri_site=="C60.1" | 
    data_yr_filtered$pri_site=="C60.2" | 
    data_yr_filtered$pri_site=="C60.8" |
    data_yr_filtered$pri_site=="C60.9"
  
  , ]




all_hpv_related<- table(year=all_cancer$year_of_diagnosis, age=all_cancer$age_at_diagnosis, gender=all_cancer$gender) |> 
  as.data.frame()
write.csv(all_hpv_related,"all_hpv_related.csv")

# hpv_cervical<- table(year=cervix_cancer$year_of_diagnosis, age=cervix_cancer$age_at_diagnosis, gender=cervix_cancer$gender) |> 
#   as.data.frame()
# write.csv(hpv_cervical,"hpv_cervical.csv")

hpv_cervical<- table(year=cervix_cancer$year_agg, age=cervix_cancer$age_agg, gender=cervix_cancer$gender) |> 
  as.data.frame()
hpv_cervical$Freq <- ifelse(hpv_cervical$Freq >0 & hpv_cervical$Freq< 5,-99,hpv_cervical$Freq)
write.csv(hpv_cervical,"hpv_cervical.csv")


hpv_oropharyngeal<- table(year=oropharyngeal_cancer$year_agg, age=oropharyngeal_cancer$age_agg, 
                          gender=oropharyngeal_cancer$gender) |>  as.data.frame()
hpv_oropharyngeal$Freq <- ifelse(hpv_oropharyngeal$Freq>0 & hpv_oropharyngeal$Freq <5,-99,hpv_oropharyngeal$Freq)
write.csv(hpv_oropharyngeal,"hpv_oropharyngeal.csv")


hpv_oropharyngeal2<- table(year=oropharyngeal_cancer2$year_agg, age=oropharyngeal_cancer2$age_agg, 
                          gender=oropharyngeal_cancer2$gender) |> as.data.frame()
hpv_oropharyngeal2$Freq <- ifelse(hpv_oropharyngeal2$Freq>0 & hpv_oropharyngeal2$Freq <5,-99,hpv_oropharyngeal2$Freq)
write.csv(hpv_oropharyngeal2,"hpv_oropharyngeal2.csv")



hpv_anal<- table(year=anal_cancer$year_agg, age=anal_cancer$age_agg, 
                          gender=anal_cancer$gender) |> as.data.frame()
hpv_anal$Freq <- ifelse(hpv_anal$Freq>0 & hpv_anal$Freq <5,-99,hpv_anal$Freq)
write.csv(hpv_anal,"hpv_anal.csv")


hpv_vaginal<- table(year=vaginal_cancer$year_agg, age=vaginal_cancer$age_agg, 
                          gender=vaginal_cancer$gender) |>  as.data.frame()
hpv_vaginal$Freq <- ifelse(hpv_vaginal$Freq>0 & hpv_vaginal$Freq <5,-99,hpv_vaginal$Freq)
write.csv(hpv_vaginal,"hpv_vaginal.csv")


hpv_penile<- table(year=penile_cancer$year_agg, age=penile_cancer$age_agg, 
                          gender=penile_cancer$gender) |> as.data.frame()
hpv_penile$Freq <- ifelse(hpv_penile$Freq>0 & hpv_penile$Freq <5,-99,hpv_penile$Freq)
write.csv(hpv_penile,"hpv_penile.csv")

# Mortality

all_cancer_mort <- all_cancer[all_cancer$death==1,]

all_hpv_related_mort<- table(year=all_cancer_mort$year_agg, age=all_cancer_mort$age_agg, 
                             gender=all_cancer_mort$gender) |> as.data.frame()
all_hpv_related_mort$Freq <- ifelse(all_hpv_related_mort$Freq>0 & all_hpv_related_mort$Freq <5,-99,all_hpv_related_mort$Freq)
write.csv(all_hpv_related_mort,"all_hpv_related_mort.csv")


cervix_cancer_mort <- cervix_cancer[cervix_cancer$death==1,]

hpv_cervical_mort<- table(year=cervix_cancer_mort$year_agg, age=cervix_cancer_mort$age_agg, 
                     gender=cervix_cancer_mort$gender) |> as.data.frame()
hpv_cervical_mort$Freq <- ifelse(hpv_cervical_mort$Freq>0 & hpv_cervical_mort$Freq <5,-99,hpv_cervical_mort$Freq)
write.csv(hpv_cervical_mort,"hpv_cervical_mort.csv")


oropharyngeal_cancer_mort <- oropharyngeal_cancer[oropharyngeal_cancer$death==1,]

hpv_oropharyngeal_mort<- table(year=oropharyngeal_cancer_mort$year_agg, age=oropharyngeal_cancer_mort$age_agg, 
                          gender=oropharyngeal_cancer_mort$gender) |>  as.data.frame()
hpv_oropharyngeal_mort$Freq <- ifelse(hpv_oropharyngeal_mort$Freq>0 & hpv_oropharyngeal_mort$Freq <5,-99,hpv_oropharyngeal_mort$Freq)
write.csv(hpv_oropharyngeal_mort,"hpv_oropharyngeal_mort.csv")



anal_cancer_mort <- anal_cancer[anal_cancer$death==1,]

hpv_anal_mort<- table(year=anal_cancer_mort$year_agg, age=anal_cancer_mort$age_agg, 
                 gender=anal_cancer_mort$gender) |> as.data.frame()
hpv_anal_mort$Freq <- ifelse(hpv_anal_mort$Freq>0 & hpv_anal_mort$Freq <5,-99,hpv_anal_mort$Freq)

write.csv(hpv_anal_mort,"hpv_anal_mort.csv")


vaginal_cancer_mort <- vaginal_cancer[vaginal_cancer$death==1,]

hpv_vaginal_mort<- table(year=vaginal_cancer_mort$year_agg, age=vaginal_cancer_mort$age_agg, 
                    gender=vaginal_cancer_mort$gender) |> as.data.frame()
hpv_vaginal_mort$Freq <- ifelse(hpv_vaginal_mort$Freq>0 & hpv_vaginal_mort$Freq <5,-99,hpv_vaginal_mort$Freq)
write.csv(hpv_vaginal_mort,"hpv_vaginal_mort.csv")


penile_cancer_mort <- penile_cancer[penile_cancer$death==1,]

hpv_penile_mort<- table(year=penile_cancer_mort$year_agg, age=penile_cancer_mort$age_agg, 
                   gender=penile_cancer_mort$gender) |> as.data.frame()
hpv_penile_mort$Freq <- ifelse(hpv_penile_mort$Freq>0 & hpv_penile_mort$Freq <5,-99,hpv_penile_mort$Freq)
write.csv(hpv_penile_mort,"hpv_penile_mort.csv")








