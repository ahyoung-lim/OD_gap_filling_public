library(data.table)
source("script/00_setup.R"); source("functions/fn_Year_checker.R"); source("functions/fn_select_best_rec.R")
source("functions/fn_consecutive_gap_counter.R"); source("functions/fn_make_week_complete.R"); source("functions/fn_OD_region.R")
add_cy <- function(df) df %>% mutate(country_year = paste0(adm_0_name, "_", Year))

# === Load data ===
hm <- fread("data/processed_data/dt_heatmap_calibrated.csv")
hm[, country_year := paste0(adm_0_name, "_", Year)]

bt_new <- as.data.frame(fread("data/processed_data/Best_T_data_V1_3.csv")) %>%
  filter(between(Year,1990,2024)) %>% region_class() %>% Year_checker() %>% add_cy()
bt_old <- as.data.frame(fread("data/processed_data/Best_T_data_V1_3_excl_ad_hoc.csv")) %>%
  filter(between(Year,1990,2024)) %>% region_class() %>% Year_checker() %>% add_cy()

# === Build coverage tables ===
bc <- function(TD) {
  d <- TD %>% group_by(adm_0_name,Year,T_res) %>% summarise(n_record=n(),.groups="drop") %>%
    mutate(n_time_den=case_when(T_res=="Month"~12L,T_res=="Year"~1L,TRUE~52L)) %>% add_cy()
  ew <- build_epiweek_lookup(min(TD$Year),max(TD$Year))
  d <- d %>% left_join(ew,by="Year") %>% mutate(n_time_den=if_else(T_res=="Week",epiweeks,n_time_den),prop=n_record/n_time_den) %>% select(-epiweeks)
  ct <- TD %>% group_by(adm_0_name,Year) %>% summarise(annual_total=sum(dengue_total,na.rm=TRUE),.groups="drop") %>% add_cy()
  ds <- TD %>% distinct(adm_0_name,Year,cat) %>% tidyr::complete(adm_0_name,Year) %>%
    group_by(adm_0_name,Year) %>% mutate(p=case_when(cat=="ad_hoc_data"~1,cat=="OD"~2,TRUE~3)) %>%
    arrange(p) %>% slice(1) %>% select(-p) %>% ungroup() %>% add_cy() %>%
    mutate(data_source=case_when(is.na(cat)~"No_data",TRUE~cat)) %>% select(-cat)
  ct <- merge(ct, ds %>% select(country_year,adm_0_name,Year,data_source),
    by=c("country_year","adm_0_name","Year"), all.y=TRUE)
  ct %>% left_join(d %>% select(country_year,T_res,n_record,prop), by="country_year") %>% region_class()
}
cn <- bc(bt_new); co <- bc(bt_old)

# === First year violations (before correction) ===
fy <- read.csv("data/processed_data/first_year_summary.csv") %>% mutate(adm_0_name=toupper(adm_0_name))
cn_pre <- cn %>% left_join(fy %>% select(adm_0_name,first_year), by="adm_0_name")
violations_cy <- cn_pre %>% filter(Year < first_year, !is.na(first_year), annual_total != 0) %>% pull(country_year)

# === Apply first_year correction + remove zero_cases_only ===
cn <- cn %>% left_join(fy %>% select(adm_0_name,first_year), by="adm_0_name") %>%
  mutate(b=Year<first_year & !is.na(first_year),
    annual_total=if_else(b,0,annual_total), data_source=if_else(b,"first_year",data_source),
    T_res=if_else(b,"Year",T_res)) %>% select(-first_year,-b)
zco <- c("BURUNDI","CONGO","EQUATORIAL GUINEA","GAMBIA","GUINEA-BISSAU","MALAWI","NAMIBIA","RWANDA","UGANDA","ZAMBIA","ZIMBABWE")
cn <- cn %>% filter(!adm_0_name %in% zco)

# === 03a comparison → OD / Ad-hoc / No data ===
comp <- merge(cn %>% select(country_year,adm_0_name,Year,annual_total,data_source,T_res),
  co %>% select(country_year,adm_0_name,Year,annual_total,data_source,T_res),
  by=c("country_year","adm_0_name","Year"), all=TRUE, suffixes=c(".x",".y"))

fyn <- comp[comp$data_source.x=="first_year" & !is.na(comp$annual_total.x) & is.na(comp$annual_total.y), "country_year"]
fyb <- comp[comp$data_source.x=="first_year" & !is.na(comp$annual_total.x) & !is.na(comp$annual_total.y) & !comp$country_year %in% fyn, "country_year"]
cf <- comp[!comp$country_year %in% fyn, ]
same_cy <- cf$country_year[cf$data_source.x!="first_year" & !is.na(cf$annual_total.x) & !is.na(cf$annual_total.y) & cf$annual_total.x==cf$annual_total.y & cf$T_res.x==cf$T_res.y]
od_cys <- c(same_cy, fyb)
new_cy <- cf$country_year[!is.na(cf$annual_total.x) & is.na(cf$annual_total.y)]
hi <- cf$country_year[cf$data_source.x!="first_year" & !is.na(cf$annual_total.x) & !is.na(cf$annual_total.y) & cf$annual_total.x > cf$annual_total.y]
tr <- cf$country_year[cf$data_source.x!="first_year" & !is.na(cf$annual_total.x) & !is.na(cf$annual_total.y) & cf$annual_total.x==cf$annual_total.y & cf$T_res.x!=cf$T_res.y]
sm <- cf$country_year[cf$data_source.x!="first_year" & !is.na(cf$annual_total.x) & !is.na(cf$annual_total.y) & cf$annual_total.x < cf$annual_total.y]
adhoc_cys <- c(hi, tr, sm, new_cy)

# === Assign groups to heatmap ===
hm[, sg := fcase(
  country_year %in% od_cys, "OD",
  country_year %in% adhoc_cys, "Ad-hoc",
  default = "No data"
)]
hm[, tres := fifelse(T_res %in% c("Week","Month"), "Sub-annual", "Annual")]

# ========================================
# SECTION 1: Col 1 (source) -> Col 2
# ========================================
sa <- hm[sg %in% c("OD","Ad-hoc") & tres=="Sub-annual"]
an <- hm[sg %in% c("OD","Ad-hoc") & tres=="Annual"]

sankey <- rbind(
  hm[sg=="OD" & !country_year %in% violations_cy & tres=="Sub-annual",
     .(source="OD", target="Sub-annual", value=.N)],
  hm[sg=="OD" & !country_year %in% violations_cy & tres=="Annual",
     .(source="OD", target="Annual", value=.N)],
  hm[sg=="Ad-hoc" & tres=="Annual",
     .(source="Ad-hoc data", target="Annual", value=.N)],
  hm[sg=="Ad-hoc" & tres=="Sub-annual",
     .(source="Ad-hoc data", target="Sub-annual", value=.N)],
  data.table(source="Sub-annual", target="Complete monthly series",
             value=sa[prop==1 & annual_total > 0, .N]),
  data.table(source="Sub-annual", target="Sub-annual imputation model",
             value=sa[prop < 1 & annual_total > 0, .N]),
  data.table(source="Annual", target="Annual disaggregation model",
             value=an[annual_total > 0, .N]),
  data.table(source="Sub-annual", target="Confirmed or assumed absence of cases",
             value=sa[annual_total == 0, .N]),
  # Country-years recoded to zero under the first-outbreak-year rule (violations_cy)
  # enter this column through their own "OD -> absence" link below, so they are
  # excluded here; otherwise they would be counted twice.
  data.table(source="Annual", target="Confirmed or assumed absence of cases",
             value=an[annual_total == 0 & !country_year %in% violations_cy, .N]),
  data.table(source="OD", target="Confirmed or assumed absence of cases",
             value=length(violations_cy)),
  hm[sg=="No data" & data_source=="IHME_calibrated",
     .(source="Assumed absence", target="IHME", value=.N)],
  hm[sg=="No data" & data_source=="Median_from_neighbors",
     .(source="Assumed absence", target="Median from neighbors", value=.N)],
  hm[sg=="No data" & data_source=="first_year",
     .(source="Assumed absence", target="first year", value=.N)],
  hm[sg=="No data" & data_source=="Assumed_zero_cases",
     .(source="Assumed absence", target="EES", value=.N)],
  data.table(source="Median from neighbors", target="Annual disaggregation model",
             value=hm[sg=="No data" & data_source=="Median_from_neighbors", .N]),
  data.table(source="IHME", target="Annual disaggregation model",
             value=hm[sg=="No data" & data_source=="IHME_calibrated", .N]),
  data.table(source="first year", target="Confirmed or assumed absence of cases",
             value=hm[sg=="No data" & data_source=="first_year", .N]),
  data.table(source="EES", target="Confirmed or assumed absence of cases",
             value=hm[sg=="No data" & data_source=="Assumed_zero_cases", .N])
)
cat("=== Sankey table ===\n")
print(sankey)
cat("\nTotal sum:", sum(sankey$value), "\n")

fwrite(sankey, "data/processed_data/sankey_table.csv")
cat("\nSaved to data/processed_data/sankey_table.csv\n")
