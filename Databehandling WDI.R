# Velger norsk språk
Sys.setlocale(locale="no_NO")

# Setter riktig 'working directory' 
setwd("C:/Users/adria/OneDrive/Dokumenter/Master/WDI_CSV")

# Laster inn nødvendige pakker
library(tidyverse)
library(ggplot2)
library(grid)
library(tidyr)
library(janitor)
library(zoo)
library(fastDummies)
library(sjPlot)

# Laster inn WDI datasettet
WDI <- read.csv("WDICSV.csv")

str(WDI)
head(WDI)

colnames(WDI) <- gsub("^X", "", colnames(WDI))

WDI_long <- WDI %>% 
  pivot_longer(cols = starts_with("19") | starts_with("20"),
               names_to = "Year",
               values_to = "Value") %>% 
  mutate(Year = as.numeric(Year))

wdi_filter <- WDI_long %>% 
  filter(Country.Name %in% c("Angola", "Australia", "Austria", "Azerbaijan", "Bahrain", "Belgium", 
                             "Bhutan", "Botswana", "Brazil", "Brunei Darussalam", "Chile", "China", "Colombia", 
                             "Cyprus", "Djibouti", "Egypt, Arab Rep.", "Ethiopia", "Finland", "France", "Gabon", 
                             "Germany", "Ghana", "Greece", "Guinea", "Guyana", "India", "Indonesia", 
                             "Iran, Islamic Rep.", "Ireland", "Israel", "Italy", "Kazakhstan", "Kuwait", "Libya", 
                             "Malaysia", "Malta", "Mauritius", "Mexico", "Mongolia", "Morocco", 
                             "Namibia", "Nauru", "New Zealand", "Nigeria", "Norway", "Oman", 
                             "Palestine", "Panama", "Peru", "Philippines", "Poland", "Qatar", "Russian Federation", 
                             "Rwanda", "Saudi Arabia", "Senegal", "Singapore", "Slovenia", 
                             "Korea, Rep.", "Spain", "Timor-Leste", "Trinidad and Tobago", "Turkiye", 
                             "United Arab Emirates", "United Kingdom", "Uzbekistan", "Viet Nam", #Uten SWF: 
                             "Venezuela, RB"),  
         #^HUSK Å ENDRE DENNE ANDRE STEDER OGSÅ HVIS DU ENDRER HER^
         Indicator.Name %in% c("GDP per capita (constant 2015 US$)",
                               "GDP (constant 2015 US$)",
                               "GDP per capita growth (annual %)",
                               "Total natural resources rents (% of GDP)",
                               "General government final consumption expenditure (% of GDP)",
                               "Government expenditure on education, total (% of GDP)",
                               "Gross domestic savings (% of GDP)"))

wdi_filter2 <- WDI_long %>% 
  filter(Indicator.Name %in% c("GDP per capita (constant 2015 US$)",
                               "GDP (constant 2015 US$)",
                               "GDP per capita growth (annual %)",
                               "Total natural resources rents (% of GDP)",
                               "General government final consumption expenditure (% of GDP)",
                               "Government expenditure on education, total (% of GDP)",
                               "Gross domestic savings (% of GDP)"),
         !Country.Name %in% c(
           "Africa Eastern and Southern",
           "Africa Western and Central",
           "Arab World",
           "Caribbean small states",
           "Central Europe and the Baltics",
           "Early-demographic dividend",
           "East Asia & Pacific",
           "East Asia & Pacific (excluding high income)",
           "East Asia & Pacific (IDA & IBRD countries)",
           "Euro area",
           "Europe & Central Asia",
           "Europe & Central Asia (excluding high income)",
           "Europe & Central Asia (IDA & IBRD countries)",
           "European Union",
           "Fragile and conflict affected situations",
           "Heavily indebted poor countries (HIPC)",
           "High income",
           "IBRD only",
           "IDA & IBRD total",
           "IDA blend",
           "IDA only",
           "IDA total",
           "Late-demographic dividend",
           "Latin America & Caribbean",
           "Latin America & Caribbean (excluding high income)",
           "Latin America & the Caribbean (IDA & IBRD countries)",
           "Least developed countries: UN classification",
           "Low & middle income",
           "Low income",
           "Lower middle income",
           "Middle East & North Africa",
           "Middle East & North Africa (excluding high income)",
           "Middle East & North Africa (IDA & IBRD countries)",
           "Middle income",
           "North America",
           "Not classified",
           "OECD members",
           "Other small states",
           "Pacific island small states",
           "Post-demographic dividend",
           "Pre-demographic dividend",
           "Small states",
           "South Asia",
           "South Asia (IDA & IBRD)",
           "Sub-Saharan Africa",
           "Sub-Saharan Africa (excluding high income)",
           "Sub-Saharan Africa (IDA & IBRD countries)",
           "Upper middle income",
           "World"))

WDI_W <- wdi_filter2 %>%
  dplyr::select(Country.Name, Year, Indicator.Name, Value) %>% 
  pivot_wider(names_from = Indicator.Name, values_from = Value)

# Teller antall år i datasett
WDI_W %>% 
  count(Year)

# Laster inn WGI datasett
setwd("C:/Users/adria/OneDrive/Dokumenter/Master")

WGI <- read.csv("WGI.csv")

WGI <- WGI %>% 
  slice_head(n = nrow(WGI) - 8)

colnames(WGI)

WGI <- WGI %>% 
  rename(
    Year = 'Time',
    Control.of.Corruption = 'Control.of.Corruption..Estimate..CC.EST.',
    Rule.of.Law = 'Rule.of.Law..Estimate..RL.EST.'
  ) %>% 
  dplyr::select(Year, Country.Name, Control.of.Corruption, 
                Rule.of.Law)

# Teller antall land i datasett
WGI %>% 
  count(Country.Name)

# 217 land i WDI_W og 214 i WGI. Fjerner de tre landene fra WDI_W
# Finner hvilke land som skiller
wdi_countries <- unique(WDI_W$Country.Name)
wgi_countries <- unique(WGI$Country.Name)

countries_not_in_wgi <- setdiff(wdi_countries, wgi_countries)
print(countries_not_in_wgi)

WDI_W <- WDI_W %>% 
  filter(!Country.Name %in% countries_not_in_wgi)

# Teller
WDI_W %>% 
  count(Year)

WGI %>% 
  count(Year)

#205 land vs 214 - fjerner nå de som er til overs i WGI fra WDI_W

countries_not_in_wdi <- setdiff(wgi_countries, wdi_countries)

print(countries_not_in_wdi)

WGI <- WGI %>% 
  filter(!Country.Name %in% countries_not_in_wdi)

# Teller
WDI_W %>% 
  count(Year)

WGI %>% 
  count(Year)

# Landene matcher og går videre til neste datasett

# Laster inn SWF datasett
library(readxl)

SWF <- read_excel("ranking_export.xlsx")

colnames(SWF)

SWF <- SWF %>% 
  filter(Type == "SWF") %>% 
  rename(Country.Name = "Country",
         Assets.under.Management = "AuM ($b)*") %>%
  filter(!grepl("-", Country.Name)) %>% 
  dplyr::select(Country.Name, Est, Assets.under.Management)

SWF <- SWF %>% 
  group_by(Country.Name) %>% 
  summarise(Total.Assets.under.Management = sum(Assets.under.Management, 
                                                na.rm = TRUE),
            SWF.Est = min(Est, na.rm = TRUE)) %>% 
  mutate(Country.Name = ifelse(Country.Name == "Trinidad & Tobago", 
                               "Trinidad and Tobago", Country.Name))


# Slå sammen datasett

## Første 2 datasett
WDIWGI <- merge(WDI_W, WGI, by = c("Country.Name", "Year"), all.x = TRUE)

## Endrer navn så de matcher med SWF
WDIWGI <- WDIWGI %>% 
  mutate(Country.Name = case_when(
    Country.Name == "Brunei Darussalam" ~ "Brunei",
    Country.Name == "Egypt, Arab Rep." ~ "Egypt",
    Country.Name == "Iran, Islamic Rep." ~ "Iran",
    Country.Name == "Russian Federation" ~ "Russia",
    Country.Name == "Korea, Rep." ~ "South Korea",
    Country.Name == "Timor-Leste" ~ "Timor Leste",
    Country.Name == "Turkiye" ~ "Turkey",
    Country.Name == "United Arab Emirates" ~ "UAE",
    Country.Name == "United Kingdom" ~ "UK",
    Country.Name == "Viet Nam" ~ "Vietnam",
    TRUE ~ Country.Name))

# Teller antall land for å sjekke at jeg ikke har mistet noen
WDIWGI %>% 
  count(Year)

## + tredje
komplett <- merge(WDIWGI, SWF, by = "Country.Name", all.x = TRUE)

# Teller land igjen
komplett %>% 
  count(Year)

# Forenkler kolonnenavn
komplett <- komplett %>% 
  rename(
    Country = Country.Name,
    RealGDP = `GDP (constant 2015 US$)`,
    GDPpc = `GDP per capita (constant 2015 US$)`,
    GDPpc.growth = `GDP per capita growth (annual %)`,
    Government.Spending = `General government final consumption expenditure (% of GDP)`,
    Education.Expenditure = `Government expenditure on education, total (% of GDP)`,
    Gross.Domestic.Savings = `Gross domestic savings (% of GDP)`,
    Resource.Rents = `Total natural resources rents (% of GDP)`
  )

colnames(komplett)

# Kontrollerer at alle landene av interesse har kommet med i endelige datasett
komplett %>%
  distinct(Country) %>% 
  count()

SWF %>%
  distinct(Country.Name) %>% 
  count()

land_i_data <- unique(komplett$Country)

# Endrer til numeriske kolonner
komplett <- komplett %>% 
  mutate(Control.of.Corruption = as.numeric(Control.of.Corruption),
         Rule.of.Law = as.numeric(Rule.of.Law))

# teller antall land og år
komplett %>% count(Country)

komplett %>% count(Year)

# lager dummmy for om landet har SWF eller ikke
# INKLUDERT MINIMUM GRENSE PÅ SWF
komplett <- komplett %>% 
  mutate(SWF = ifelse(!is.na(Total.Assets.under.Management) 
                      & Total.Assets.under.Management > 20, 1, 0))


# lager dummy for om landet har SWF ENDA eller ikke! 0 før SWF blir implementert
komplett <- komplett %>% 
  mutate(Active.SWF = ifelse(SWF == 0, 0,
                             ifelse(!is.na(SWF) & Year >= SWF.Est, 1, 0)))

# lager dummy for om landet har et aktivt SWF og er et ressursrikt land
## Bruker rullerende gjennomsnitt, for at NA ikke skal skape problemer
library(zoo)

komplett <- komplett %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate(
    Resource.Rents.avg5 = rollapply(Resource.Rents, 
                                    width = 5, 
                                    FUN = mean, 
                                    fill = NA, 
                                    align = "right", 
                                    na.rm = TRUE),
    Resource.Rents.avg5 = ifelse(is.nan(Resource.Rents.avg5), NA, Resource.Rents.avg5)
  ) %>%
  ungroup()

komplett <- komplett %>% 
  mutate(Active.Resource.SWF = ifelse(Active.SWF == 1 & 
                                        Resource.Rents.avg5 >= 5, 1, 0))

colnames(komplett)

# Lager variabel for SWF alder
komplett <- komplett %>%
  mutate(SWF.Age = ifelse(!is.na(SWF.Est),
                                 pmax(0, Year - SWF.Est),
                                 0))

# Teller antall ressursrike land med stort nok SWF 
komplett %>% 
  filter(Active.Resource.SWF == 1) %>% 
  distinct(Country) %>% 
  nrow()

# Teller totalt antall land
komplett %>% 
  distinct(Country) %>% 
  nrow()



# Regresjon___________________________________________________

## MODELL A: FE med SWF dummy og lagged variabler
library(plm)

fe_model_A <- plm(GDPpc.growth ~ lag(Active.Resource.SWF,1) + 
                 lag(Rule.of.Law, 1) + 
                 lag(Government.Spending, 1) +
                 lag(Education.Expenditure, 1) + 
                 lag(Resource.Rents, 1) + 
                 lag(Gross.Domestic.Savings, 1),
               data = komplett,
               model = "within",        # FE (within estimator)
               effect = "twoways",      # land- og år-faste effekter
               index = c("Country", "Year"))

summary(fe_model_A)


re_model_A <- plm(GDPpc.growth ~ lag(Active.Resource.SWF,1) + 
                    lag(Rule.of.Law, 1) + 
                    lag(Government.Spending, 1) +
                    lag(Education.Expenditure, 1) + 
                    lag(Resource.Rents, 1) + 
                    lag(Gross.Domestic.Savings, 1),
                  data = komplett,
                  model = "random",       
                  effect = "twoways",      # land- og år-faste effekter
                  index = c("Country", "Year"))

summary(re_model_A)

hausman <- phtest(fe_model_A, re_model_A)
print(hausman)


# Modell B - FE med SWF alder

model_B <- plm(GDPpc.growth ~ SWF.Age + I(SWF.Age^2) +
                  lag(Rule.of.Law, 1) + 
                  lag(Government.Spending, 1) +
                  lag(Education.Expenditure, 1) + 
                  lag(Resource.Rents, 1) + 
                  lag(Gross.Domestic.Savings, 1),
                data = komplett,
                model = "within",
                effect = "twoways",
                index = c("Country", "Year"))

summary(model_B)


# Deskriptiv statistikk____________________________________________________
library(vtable)

navn <- c("GDP per capita Growth",
          "SWF Age", "Rule of Law", "Government Spending",
          "Education Expenditure", "Resource Rents", "Gross Domestic Savings")

st(komplett, labels = navn,
   vars = c("GDPpc.growth", "SWF.Age", "Rule.of.Law",
            "Government.Spending", "Education.Expenditure", "Resource.Rents",
            "Gross.Domestic.Savings"),
   summ = c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'),
   digits = 4,
   summ.names = list(
     c('Number of Observations','Mean','Standard Deviation',
       'Min','Max')))

# Tabell__________________________________________________________________
library(modelsummary)

library(lmtest)
library(sandwich)

# A: lag SWF dummy, uten og med year FE
A_noY  <- plm(GDPpc.growth ~ lag(Active.Resource.SWF,1) +
                lag(Rule.of.Law,1) + lag(Government.Spending,1) +
                lag(Education.Expenditure,1) + lag(Resource.Rents,1) +
                lag(Gross.Domestic.Savings,1),
              data = komplett, model = "within", effect = "individual")

s <- summary(A_noY)

plm::r.squared(A_noY, type = "cor")

A_Y <- plm( 
  GDPpc.growth ~ lag(Active.Resource.SWF,1) +
    lag(Rule.of.Law,1) + lag(Government.Spending,1) +
    lag(Education.Expenditure,1) + lag(Resource.Rents,1) +
    lag(Gross.Domestic.Savings,1),
  data = komplett, model = "within", effect = "twoways"
) 


# B: SWF Age + age^2 uten og med year FE
B_noY <- plm(
  GDPpc.growth ~ SWF.Age + I(SWF.Age^2) +
    lag(Rule.of.Law,1) + lag(Government.Spending,1) +
    lag(Education.Expenditure,1) + lag(Resource.Rents,1) +
    lag(Gross.Domestic.Savings,1),
  data = komplett, model = "within", effect = "individual"
)

B_Y <- plm(
  GDPpc.growth ~ SWF.Age + I(SWF.Age^2) +
    lag(Rule.of.Law,1) + lag(Government.Spending,1) +
    lag(Education.Expenditure,1) + lag(Resource.Rents,1) +
    lag(Gross.Domestic.Savings,1),
  data = komplett, model = "within", effect = "twoways"
)


# Clustered SE
vc_A_noY  <- vcovHC(A_noY,  type = "HC1", cluster = "group")
vc_A_Y    <- vcovHC(A_Y,    type = "HC1", cluster = "group")
vc_B_noY <- vcovHC(B_noY, type = "HC1", cluster = "group")
vc_B_Y   <- vcovHC(B_Y,   type = "HC1", cluster = "group")


# TABELL 1: 4 kolonner 
library(gt)

tab1 <- modelsummary(
  list(
    "(1) No year FE" = A_noY,
    "(2) Year FE"    = A_Y,
    "(3) No year FE"   = B_noY,
    "(4) Year FE"      = B_Y
  ),
  output = "gt",
  vcov = list(vc_A_noY, vc_A_Y, vc_B_noY, vc_B_Y),
  coef_map = c(
    "lag(Active.Resource.SWF, 1)" = "SWF dummy (t-1)",
    "SWF.Age"             = "SWF age (t)",
    "I(SWF.Age^2)"        = "SWF age² (t)",
    "lag(Rule.of.Law, 1)"           = "Rule of Law (t−1)",
    "lag(Government.Spending, 1)"   = "Gov. Spending (t−1)",
    "lag(Education.Expenditure, 1)" = "Education Exp. (t−1)",
    "lag(Resource.Rents, 1)"        = "Resource Rents (t−1)",
    "lag(Gross.Domestic.Savings, 1)"= "Gross Savings (t−1)"
  ),
  gof_omit = "IC|Log|F|Adj|RMSE",
  add_rows = data.frame(
    rowname   = c("Country FE","Year FE"),
    `(1) SWF dummy, no year FE` = c("Yes","No"),
    `(2) SWF dummy, year FE`    = c("Yes","Yes"),
    `(3) SWF age, no year FE`   = c("Yes","No"),
    `(4) SWF age, year FE`      = c("Yes","Yes")
  ),
  stars = c('*'=.10,'**'=.05,'***'=.01)
)

tab1 <- tab1 %>% 
  tab_spanner(label = "SWF Dummy (Model A)", columns = 2:3) %>% 
  tab_spanner(label = "SWF Age (Model B)", columns = 4:5) %>% 
  tab_header(
    title = "Fixed effects panel regressions of GDP per capita growth",
    subtitle = "SWF dummy and SWF age specifications"
  )

tab1

gtsave(tab1, "FEresults.png")

# Supplerende analyse med ressursrike land_____________________________
# Minstekrav om naturressurser på 5% av BNP
komplett_resourcerich <- komplett %>% 
  arrange(Country, Year) %>% 
  group_by(Country) %>% 
  mutate(
    Resource.Rents.avg5 = rollapply(Resource.Rents, 
                                    width = 5, 
                                    FUN = mean, 
                                    fill = NA, 
                                    align = "right", 
                                    na.rm = TRUE),
    Resource.Rents.avg5 = ifelse(is.nan(Resource.Rents.avg5), NA, Resource.Rents.avg5)
  ) %>% 
  ungroup() %>% 
  filter(Resource.Rents.avg5 >= 5)

# sjekker antall land i datasett med kun ressursrike land
n_distinct(komplett_resourcerich$Country)
nrow(komplett_resourcerich)

# lager identiske modeller og tabell med kun ressursrike land
## Modeller
# A (RR): lag SWF dummy, uten og med year FE
RR_A_noY  <- plm(GDPpc.growth ~ lag(Active.Resource.SWF,1) +
                lag(Rule.of.Law,1) + lag(Government.Spending,1) +
                lag(Education.Expenditure,1) + lag(Resource.Rents,1) +
                lag(Gross.Domestic.Savings,1),
              data = komplett_resourcerich, model = "within", effect = "individual")


RR_A_Y <- plm( 
  GDPpc.growth ~ lag(Active.Resource.SWF,1) +
    lag(Rule.of.Law,1) + lag(Government.Spending,1) +
    lag(Education.Expenditure,1) + lag(Resource.Rents,1) +
    lag(Gross.Domestic.Savings,1),
  data = komplett_resourcerich, model = "within", effect = "twoways"
) 


# B (RR): SWF Age + age^2 uten og med year FE
RR_B_noY <- plm(
  GDPpc.growth ~ SWF.Age + I(SWF.Age^2) +
    lag(Rule.of.Law,1) + lag(Government.Spending,1) +
    lag(Education.Expenditure,1) + lag(Resource.Rents,1) +
    lag(Gross.Domestic.Savings,1),
  data = komplett_resourcerich, model = "within", effect = "individual"
)

RR_B_Y <- plm(
  GDPpc.growth ~ SWF.Age + I(SWF.Age^2) +
    lag(Rule.of.Law,1) + lag(Government.Spending,1) +
    lag(Education.Expenditure,1) + lag(Resource.Rents,1) +
    lag(Gross.Domestic.Savings,1),
  data = komplett_resourcerich, model = "within", effect = "twoways"
)


# Clustered SE
RR_vc_A_noY  <- vcovHC(RR_A_noY,  type = "HC1", cluster = "group")
RR_vc_A_Y    <- vcovHC(RR_A_Y,    type = "HC1", cluster = "group")
RR_vc_B_noY <- vcovHC(RR_B_noY, type = "HC1", cluster = "group")
RR_vc_B_Y   <- vcovHC(RR_B_Y,   type = "HC1", cluster = "group")

# tabell
tab_RR <- modelsummary(
  list(
    "(1) No year FE" = RR_A_noY,
    "(2) Year FE"    = RR_A_Y,
    "(3) No year FE"   = RR_B_noY,
    "(4) Year FE"      = RR_B_Y
  ),
  output = "gt",
  vcov = list(RR_vc_A_noY, RR_vc_A_Y, RR_vc_B_noY, RR_vc_B_Y),
  coef_map = c(
    "lag(Active.Resource.SWF, 1)" = "SWF dummy (t-1)",
    "SWF.Age"             = "SWF age (t)",
    "I(SWF.Age^2)"        = "SWF age² (t)",
    "lag(Rule.of.Law, 1)"           = "Rule of Law (t−1)",
    "lag(Government.Spending, 1)"   = "Gov. Spending (t−1)",
    "lag(Education.Expenditure, 1)" = "Education Exp. (t−1)",
    "lag(Resource.Rents, 1)"        = "Resource Rents (t−1)",
    "lag(Gross.Domestic.Savings, 1)"= "Gross Savings (t−1)"
  ),
  gof_omit = "IC|Log|F|Adj|RMSE",
  add_rows = data.frame(
    rowname   = c("Country FE","Year FE"),
    `(1) SWF dummy, no year FE` = c("Yes","No"),
    `(2) SWF dummy, year FE`    = c("Yes","Yes"),
    `(3) SWF age, no year FE`   = c("Yes","No"),
    `(4) SWF age, year FE`      = c("Yes","Yes")
  ),
  stars = c('*'=.10,'**'=.05,'***'=.01)
)

tab_RR <- tab_RR %>% 
  tab_spanner(label = "SWF Dummy (Model A)", columns = 2:3) %>% 
  tab_spanner(label = "SWF Age (Model B)", columns = 4:5) %>% 
  tab_header(
    title = "Fixed effects panel regressions of GDP per capita growth",
    subtitle = "Resource-rich countries only") %>% 
  tab_source_note(
    source_note = "Notes: The sample covers 3400 country-year observations across 100 resource-rich countries"
  )

tab_RR

gtsave(tab_RR, "FEresultsRR.png")



# Visuelle figurer_______________________________________________
library(ggplot2)

# Figur 2:

# 1) Definer treated/post og placebo-år for ikke-SWF-land
diff <- komplett %>% 
  select(Country, Year, GDPpc.growth, SWF.Est, Resource.Rents) %>%
  mutate(
    SWF_treated = ifelse(!is.na(SWF.Est), 1, 0),
    SWF_post    = ifelse(!is.na(SWF.Est) & Year >= SWF.Est, 1, 0)
  ) %>%
  group_by(Country) %>%
  mutate(
    # faktiske SWF-startår for behandlete
    SWF_start_real = ifelse(SWF_treated == 1, min(Year[SWF_post == 1], na.rm = TRUE), NA_real_),
    # placebo for ubehandlede: median observasjonsår
    SWF_placebo    = ifelse(SWF_treated == 0, floor(median(Year, na.rm = TRUE)), NA_real_),
    # event-år = reelt startår for treated, ellers placebo
    SWF_start      = ifelse(SWF_treated == 1, SWF_start_real, SWF_placebo),
    rel_year       = Year - SWF_start
  ) %>%
  ungroup()

# 2) Vindu og winsorize vekst (1 % i hver hale)
wins <- function(x, p=0.01){
  q <- quantile(x, probs = c(p, 1-p), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}

window <- 10

diff_ev <- diff %>%
  filter(!is.na(rel_year), rel_year >= -window, rel_year <= window) %>%
  mutate(growth_w = wins(GDPpc.growth, p = 0.01),
         group = ifelse(SWF_treated == 1, "SWF countries", "No-SWF countries"))

# 3A) Robust sammendrag: median + IQR
sum_median <- diff_ev %>%
  group_by(group, rel_year) %>%
  summarise(
    n    = sum(!is.na(growth_w)),
    med  = median(growth_w, na.rm = TRUE),
    p25  = quantile(growth_w, 0.25, na.rm = TRUE),
    p75  = quantile(growth_w, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

p_med <- ggplot(sum_median, aes(x = rel_year, y = med, color = group, fill = group)) +
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.15, color = NA) +
  geom_line(size = 1.1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "grey60") +
  scale_x_continuous(breaks = seq(-window, window, by = 2)) +
  labs(
    title = "Event study: GDP per capita growth around SWF establishment (year 0)",
    subtitle = "Median growth with IQR (winsorized at 1% tails).",
    x = "Years relative to SWF start (placebo year 0 for non-SWF = median sample year)",
    y = "Annual GDP per capita growth (pp)", color = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

# Glattet gjennomsnitt (3-års MA) + 95 % CI
sum_mean <- diff_ev %>%
  group_by(group, rel_year) %>%
  summarise(
    n    = sum(!is.na(growth_w)),
    mean = mean(growth_w, na.rm = TRUE),
    sd   = sd(growth_w, na.rm = TRUE),
    se   = sd / sqrt(n),
    lwr  = mean - 1.96*se,
    upr  = mean + 1.96*se,
    .groups = "drop"
  ) %>%
  arrange(group, rel_year) %>%
  group_by(group) %>%
  mutate(
    mean_ma = zoo::rollapply(mean, width = 3, FUN = mean, align = "center", fill = NA),
    lwr_ma  = zoo::rollapply(lwr,  width = 3, FUN = mean, align = "center", fill = NA),
    upr_ma  = zoo::rollapply(upr,  width = 3, FUN = mean, align = "center", fill = NA)
  ) %>% ungroup()

p_ma <- ggplot(sum_mean, aes(x = rel_year, y = mean_ma, color = group, fill = group)) +
  geom_ribbon(aes(ymin = lwr_ma, ymax = upr_ma), alpha = 0.12, color = NA) +
  geom_line(size = 1.1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "grey60") +
  scale_x_continuous(breaks = seq(-window, window, by = 2)) +
  labs(
    title = "Event study (smoothed): GDP per capita growth around SWF establishment",
    subtitle = "3-year moving average with 95% CI (winsorized at 1% tails).",
    x = "Years relative to SWF start (placebo for non-SWF = median sample year)",
    y = "Annual GDP per capita growth (pp)", color = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

p_med
p_ma # Bruker kun denne i oppgaven

ggsave("event_study_growth.png", plot = p_ma,
       width = 10, height = 6, dpi = 300)


# Robustness test____________________________________________________


# Driscoll–Kraay (robust mot tverrsnittsavhengighet + seriekorrelasjon)
se_DK_A <- vcovSCC(fe_model_A, type = "HC1", maxlag = 2)  # prøv 1–3
se_DK_B <- vcovSCC(model_B, type = "HC1", maxlag = 2)
coeftest(fe_model_A, vcov = se_DK_A)
coeftest(model_B, vcov = se_DK_B)

tab2 <- modelsummary(
  list("Model A (DK SE)" = fe_model_A,
       "Model B (DK SE)" = model_B),
  vcov     = list(se_DK_A, se_DK_B),
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Adj",
  output   = "gt",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  add_rows = data.frame(
    rowname = c("Country FE","Year FE","SE type"),
    `Model A (DK SE)` = c("Yes","Yes","Driscoll–Kraay"),
    `Model B (DK SE)` = c("Yes","Yes","Driscoll–Kraay")
  )
) |>
  gt::tab_header(
    title = "Fixed effects regressions with Driscoll–Kraay SE",
    subtitle = "Dependent variable: annual GDP per capita growth"
  )

tab2

gtsave(tab2, "Driscoll-Kraay.png")


# Landspesifikke lineære trender - kontrollere for ulike trendbaner
komplett$year_num <- as.numeric(komplett$Year)

A_trend <- plm(
  GDPpc.growth ~ lag(Active.Resource.SWF,1) +
    lag(Rule.of.Law,1) + lag(Government.Spending,1) +
    lag(Education.Expenditure,1) + lag(Resource.Rents,1) +
    lag(Gross.Domestic.Savings,1) + year_num,
  data = komplett, model = "within", effect = "individual"
)

B_trend <- plm(
  GDPpc.growth ~ SWF.Age + I(SWF.Age^2) +
    lag(Rule.of.Law,1) + lag(Government.Spending,1) +
    lag(Education.Expenditure,1) + lag(Resource.Rents,1) +
    lag(Gross.Domestic.Savings,1) + year_num,
  data = komplett, model = "within", effect = "individual"
)

coeftest(A_trend, vcov = vcovHC(A_trend, type="HC1", cluster="group"))
coeftest(B_trend, vcov = vcovHC(B_trend, type="HC1", cluster="group"))

vc_A_trend <- vcovHC(A_trend, type="HC1", cluster="group")
vc_B_trend <- vcovHC(B_trend, type="HC1", cluster="group")

tab3 <- modelsummary(
  list("Model A (+ trends)" = A_trend,
       "Model B (+ trends)" = B_trend),
  vcov     = list(vc_A_trend, vc_B_trend),
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Adj",
  output   = "gt",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  add_rows = data.frame(
    rowname = c("Country FE","Year FE","Country linear trend"),
    `Model A (+ trends)` = c("Yes","No","Yes"),
    `Model B (+ trends)` = c("Yes","No","Yes")
  )
) |>
  gt::tab_header(
    title = "Fixed effects with country-specific linear trends",
    subtitle = "Dependent variable: annual GDP per capita growth"
  )

tab3

gtsave(tab3, "Linear trends.png")

# Outliers i årlig vekst
wins <- function(x, p=0.01){
  q <- stats::quantile(x, c(p, 1-p), na.rm=TRUE)
  pmin(pmax(x, q[1]), q[2])
}
komplett$GDPpc.growth_w <- wins(komplett$GDPpc.growth, 0.01)

A_w <- update(fe_model_A, formula = GDPpc.growth_w ~ .)
B_w <- update(model_B,    formula = GDPpc.growth_w ~ .)

coeftest(A_w, vcov = vcovHC(A_w, type="HC1", cluster="group"))
coeftest(B_w, vcov = vcovHC(B_w, type="HC1", cluster="group"))

vc_A_w <- vcovHC(A_w, type="HC1", cluster="group")
vc_B_w <- vcovHC(B_w, type="HC1", cluster="group")

tab4 <- modelsummary(
  list("Model A (winsorized DV)" = A_w,
       "Model B (winsorized DV)" = B_w),
  vcov     = list(vc_A_w, vc_B_w),
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Adj",
  output   = "gt",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  add_rows = data.frame(
    rowname = c("Country FE","Year FE","Winsorization"),
    `Model A (winsorized DV)` = c("Yes","Yes","1st/99th pct."),
    `Model B (winsorized DV)` = c("Yes","Yes","1st/99th pct.")
  )
) |>
  gt::tab_header(
    title = "Fixed effects with winsorized GDP per capita growth",
    subtitle = "Dependent variable winsorized at the 1st and 99th percentiles"
  ) |>
  gt::tab_source_note(
    source_note = "Notes: Winsorization reduces leverage of extreme growth years without dropping observations."
  )

tab4

gtsave(tab4, "Winsorized.png")

## Bytt Rule of Law med Control of Corruption (WGI)
B_corrupt <- plm(
  GDPpc.growth ~ SWF.Age + I(SWF.Age^2) +
    lag(Control.of.Corruption,1) + lag(Government.Spending,1) +
    lag(Education.Expenditure,1) + lag(Resource.Rents,1) +
    lag(Gross.Domestic.Savings,1),
  data = komplett, model = "within", effect = "twoways"
)

coeftest(B_corrupt, vcov = vcovHC(B_corrupt, type="HC1", cluster="group"))

vc_B_base    <- vcovHC(model_B,    type="HC1", cluster="group")  # baseline m/ Rule of Law
vc_B_corrupt <- vcovHC(B_corrupt,  type="HC1", cluster="group")  # alternativ indikator

tab5 <- modelsummary(
  list("Model B: Rule of Law"          = model_B,
       "Model B: Control of Corruption"= B_corrupt),
  vcov     = list(vc_B_base, vc_B_corrupt),
  gof_omit = "AIC|BIC|Log.Lik|F|RMSE|Adj",
  output   = "gt",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  add_rows = data.frame(
    rowname = c("Country FE","Year FE"),
    `Model B: Rule of Law`           = c("Yes","Yes"),
    `Model B: Control of Corruption` = c("Yes","Yes")
  )
) |>
  gt::tab_header(
    title = "Fixed effects with alternative institutional indicator",
    subtitle = "Model B with Rule of Law instead of Control of Corruption"
  )

tab5

gtsave(tab5, "Alternative_indidicator.png")

