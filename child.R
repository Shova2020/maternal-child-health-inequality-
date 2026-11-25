library(tidyverse)
setwd(file.path(Sys.getenv("USERPROFILE"),"Downloads"))
u5<-read_csv("WB_HCP_U5MORT.CSV")
mm<-read_csv("WB_WDI_SH_STA_MMRT.CSV")
glimpse(u5)
u5<-u5 %>% 
  select(REF_AREA,REF_AREA_LABEL,TIME_PERIOD,OBS_VALUE)
mm<-mm %>%
  select(REF_AREA,REF_AREA_LABEL,TIME_PERIOD,OBS_VALUE)
u5<-u5 %>% 
  rename(code=REF_AREA,country=REF_AREA_LABEL,
         year=TIME_PERIOD,U5_mort=OBS_VALUE)
mm<-mm %>% 
  rename(code=REF_AREA,country=REF_AREA_LABEL,
         year=TIME_PERIOD,mat_mort=OBS_VALUE)
glimpse(u5)
glimpse(mm)
health<-inner_join(u5,mm,by=c("code","country","year"))
glimpse(health)
regions <- c("AFE","AFW","ARB","CSS","CEB","EAR","EAS","EAP","TEA",
             "EMU","ECS","ECA","TEC","EUU","HPC","IBD","IBT","IDB",
             "IDX","IDA","LTE","LCN","LAC","TLA","LDC","LMY","MEA",
             "MNA","TMN","MIC","NAC","OED","OSS","PSS","PST","PRE",
             "SST","SAS","TSA","SSF","SSA","TSS","WLD")

health <- health %>%
  filter(!code %in% regions)
nrow(health)
n_distinct(health$country)
range(health$year)
summary(health)      
h2023<-health %>%filter(year==2023)
nrow(h2023)
ggplot(h2023,aes(x=U5_mort,y=mat_mort))+
  geom_point()
cor(h2023$U5_mort,h2023$mat_mort)
ggplot(h2023,aes(x=U5_mort,y=mat_mort))+
  geom_point(alpha = 0.6)+
  geom_smooth(method="lm")+
  labs(title="Under-5 mortality vs maternal mortality,2023",
       x="Under-5 mortality(per 1,000 live births)",
       y="maternal mortality(per 100,000 live births)")
h2023 %>%
  arrange(desc(mat_mort)) %>%
  select(country,U5_mort,mat_mort) %>%
  head(10)
bd<-health %>% filter(country=="Bangladesh")
ggplot(bd,aes(x=year,y=mat_mort))+
  geom_line()+
  labs(title="Bangladesh: maternal mortality over time ",
       x="year",y="maternal mortality(per 100,000)")
compare<-health %>%
  filter(country %in% c("Bangladesh","India","Pakistan","Nepal"))
ggplot(compare,aes(x=year,y=mat_mort,colour=country))+
  geom_line()+
  labs(title="Maternal mortality:South Asia",
       x="Year",y="Maternal mortality(per 100,000)")
compare %>%
  filter(year %in% c(1985,2023 )) %>%
  select(country,year,mat_mort)%>%
  pivot_wider(names_from=year,values_from=mat_mort)%>%
  mutate(percent_drop=(`1985`-`2023`)/`1985`*100)
drop_tbl<-compare %>% 
  filter(year %in% c(1985,2023))%>%
  select(country,year,mat_mort)%>%
  pivot_wider(names_from = year,values_from=mat_mort)%>%
  mutate(percent_drop=(`1985`-`2023`)/`1985`*100)
ggplot(drop_tbl,aes(x=country,y=percent_drop))+
  geom_col()+
  labs(title="maternal mortality drop,1985_2023",
       x="country",y="percent drop")
ggplot(drop_tbl,aes(x=reorder(country,-percent_drop),y=percent_drop))+
  geom_col(fill="steelblue")+
  geom_text(aes(label=round(percent_drop,1)),vjust=-0.5)+
  labs(title="Maternal mortality drop,1985-2023",
       x="country",y="percent drop")
list.files(pattern="BRTC")
list.files("API_SH.STA.BRTC.ZS_DS2_en_csv_v2_76124")

brth<-read_csv("API_SH.STA.BRTC.ZS_DS2_en_csv_v2_76124/API_SH.STA.BRTC.ZS_DS2_en_csv_v2_76124.csv",skip=4)
dim(brth)
names(brth)[1:6]
brth_long <- brth %>%
  select(code = `Country Code`, `1985`:`2023`) %>%
  pivot_longer(cols = -code, names_to = "year", values_to = "birth_att") %>%
  mutate(year = as.numeric(year)) %>%
  drop_na(birth_att)

glimpse(brth_long)
health2<-health %>%
inner_join(brth_long,by=c("code","year"))
glimpse(health2)
ggplot(health2, aes(x = birth_att, y = mat_mort)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  labs(title = "Skilled birth attendance vs maternal mortality",
       x = "Births attended by skilled staff (%)",
       y = "Maternal mortality (per 100,000)")

cor(health2$birth_att, health2$mat_mort)
model<-lm(mat_mort~birth_att,data=health2)            
summary(model)
ggsave("chart3.png",width=7,height=5)
list.files(pattern = "chart")
