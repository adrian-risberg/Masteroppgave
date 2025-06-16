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
         Indicator.Name %in% c("GDP per capita (current US$)", 
                               "Total natural resources rents (% of GDP)",
                               "General government final consumption expenditure (% of GDP)",
                               "Government expenditure on education, total (% of GDP)"))

wdi_filter2 <- WDI_long %>% 
  filter(Indicator.Name %in% c("GDP per capita (current US$)", 
                               "Total natural resources rents (% of GDP)",
                               "General government final consumption expenditure (% of GDP)",
                               "Government expenditure on education, total (% of GDP)"),
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
  select(Country.Name, Year, Indicator.Name, Value) %>% 
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
  select(Year, Country.Name, Control.of.Corruption, 
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
  select(Country.Name, Est, Assets.under.Management)

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

# lager dummmy for om landet har SWF eller ikke
# INKLUDERT MINIMUM GRENSE PÅ SWF
komplett <- komplett %>% 
  mutate(SWF = ifelse(!is.na(Total.Assets.under.Management) 
                      & Total.Assets.under.Management > 20, 1, 0))


# lager dummy for om landet har SWF ENDA eller ikke! 0 før SWF blir implementert
komplett <- komplett %>% 
  mutate(Active.SWF = ifelse(SWF == 0, 0,
                             ifelse(!is.na(SWF) & Year >= SWF.Est, 1, 0)))

# Forenkler kolonnenavn
komplett <- komplett %>% 
  rename(
    Country = Country.Name,
    GDPpc = `GDP per capita (current US$)`,
    Government.Spending = `General government final consumption expenditure (% of GDP)`,
    Education.Expenditure = `Government expenditure on education, total (% of GDP)`,
    Resource.Rents = `Total natural resources rents (% of GDP)`
  )

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

komplett$Active.Resource.SWF <- ifelse(is.na(komplett$Active.Resource.SWF), 0, komplett$Active.Resource.SWF)

# teller antall land og år
komplett %>% count(Country)

komplett %>% count(Year)

# Skaper lagged variabler for Active Resource SWF med 2, 5, og 10 år

komplett <- komplett %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate(
    SWF.Resource.lag2 = lag(Active.Resource.SWF, 2),
    SWF.Resource.lag5 = lag(Active.Resource.SWF, 5),
    SWF.Resource.lag10 = lag(Active.Resource.SWF, 10)
  ) %>%
  ungroup()

table(komplett$SWF.Resource.lag2, komplett$SWF.Resource.lag5)
table(komplett$SWF.Resource.lag5, komplett$SWF.Resource.lag10)

# samme verdier. Prøver totalt antall år med SWF
komplett <- komplett %>%
  mutate(Years.with.SWF = ifelse(!is.na(SWF.Est),
                            pmax(0, Year - SWF.Est),
                            0))

# Teller antall forskjellige land med SWF 
komplett %>%
  filter(Active.Resource.SWF == 1) %>%
  distinct(Country) %>%
  nrow()

# Regresjon

colnames(komplett)

komplett$Country <- relevel(factor(komplett$Country), ref = "Mexico")

ols <- lm(log(GDPpc) ~ Years.with.SWF + sqrt(Years.with.SWF) + Government.Spending +
            Education.Expenditure + Resource.Rents + Rule.of.Law + factor(Country),
          data = komplett)


# Lag tabell
tab_model(ols, show.ci = FALSE, show.std = TRUE, title = "OLS Results", p.style = "numeric")

n_countries <- length(unique(komplett$Country[!is.na(komplett$GDPpc)]))

# presenterer ny tabell uten alle land-spesifikke faktorer
# MODELL 1!
tab_model(ols, 
          show.est = TRUE,
          show.ci = FALSE,
          show.se = TRUE,
          show.stat = FALSE,
          show.p = TRUE,
          show.obs = TRUE,
          dv.labels = "GDP per capita",
          collapse.ci = F,
          # Fjern land-koeffisientene ved å bruke "string" matching:
          transform = NULL,
          terms = c("Years.with.SWF", "sqrt(Years.with.SWF)", "Government.Spending", "Education.Expenditure", 
                    "Resource.Rents", "Rule.of.Law"))

# tabell med kun land-spesifikke faktorer

# Lag tabellen, men skjul alle andre variabler manuelt
tab_model(ols,
          terms = grep("^factor\\(Country\\)", names(coef(ols)), value = TRUE),
          title = "Appendix: Country Fixed Effects from OLS Model")


# Tester regresjon uten factor(country)
ols2 <- lm(log(GDPpc) ~ Years.with.SWF + sqrt(Years.with.SWF) + Government.Spending +
             Education.Expenditure + Resource.Rents + Rule.of.Law,
          data = komplett)

tab_model(ols2, show.ci = FALSE, show.std = TRUE, title = "OLS Results", p.style = "numeric")




# Andre regresjonsmodeller
## Med lag
ols3 <- lm(log(GDPpc) ~ Government.Spending + Education.Expenditure + Resource.Rents +
             Rule.of.Law + SWF.Resource.lag2 + factor(Country),
           data = komplett)

tab_model(ols3, show.ci = FALSE, show.std = TRUE, title = "OLS Results", p.style = "numeric")

ols4 <- lm(log(GDPpc) ~ Government.Spending + Education.Expenditure + Resource.Rents +
             Rule.of.Law + SWF.Resource.lag5 + factor(Country),
           data = komplett)

tab_model(ols4, show.ci = FALSE, show.std = TRUE, title = "OLS Results", p.style = "numeric")

ols5 <- lm(log(GDPpc) ~ Government.Spending + Education.Expenditure + Resource.Rents +
             Rule.of.Law + SWF.Resource.lag10 + factor(Country),
           data = komplett)

tab_model(ols5, show.ci = FALSE, show.std = TRUE, title = "OLS Results", p.style = "numeric")

## Years with SWF
komplett <- komplett %>%
  mutate(Years.with.SWF.category = case_when(
    Years.with.SWF == 0 ~ "a. 0 years",
    Years.with.SWF %in% 1:4 ~ "b. 1 to 4 years",
    Years.with.SWF %in% 5:9 ~ "c. 5 to 9 years",
    Years.with.SWF >= 10 ~ "d. 10+ years"
  ))

ols6 <- lm(log(GDPpc) ~ factor(Years.with.SWF.category) +
     Government.Spending +
     Education.Expenditure +
     Resource.Rents +
     Rule.of.Law +
     factor(Country),
   data = komplett)

tab_model(ols6, show.ci = FALSE, show.std = TRUE, title = "OLS Results", p.style = "numeric")

swf_terms <- grep("^factor\\(Years.with.SWF.category\\)", names(coef(ols6)), value = TRUE)

tab_model(ols6, 
          show.est = TRUE,
          show.ci = FALSE,
          show.se = TRUE,
          show.stat = FALSE,
          show.p = TRUE,
          show.obs = TRUE,
          dv.labels = "GDP per capita",
          collapse.ci = T,
          # Fjern land-koeffisientene ved å bruke "string" matching:
          transform = NULL,
          terms = c(swf_terms, "Government.Spending", 
                    "Education.Expenditure", 
                    "Resource.Rents", "Rule.of.Law"))

# MODELL 2
ols7 <- lm(log(GDPpc) ~ factor(Years.with.SWF) +
             Government.Spending +
             Education.Expenditure +
             Resource.Rents +
             Rule.of.Law +
             factor(Country),
           data = komplett)

vars <- names(coef(ols7))

# Velg de som matcher "factor(Years.with.SWF)"
years_terms <- vars[grep("factor\\(Years.with.SWF\\)", vars)]

tab_model(ols7, 
          show.est = TRUE,
          show.ci = FALSE,
          show.se = TRUE,
          show.stat = FALSE,
          show.p = TRUE,
          show.obs = TRUE,
          dv.labels = "GDP per capita",
          collapse.ci = F,
          terms = c(years_terms, 
                    "Government.Spending", "Education.Expenditure", 
                    "Resource.Rents", "Rule.of.Law"))


# Robustness test

## Outliers

mod <- lm(log(GDPpc) ~ Government.Spending + Education.Expenditure + Resource.Rents +
            Rule.of.Law + Years.with.SWF + factor(Country),
          data = komplett)

komplett_used <- model.frame(mod)  # Henter datasettet brukt i modellen
komplett_used$resid_std <- rstandard(mod)

outliers <- komplett_used %>% filter(abs(resid_std) > 3)
nrow(outliers)  # hvor mange outliers som finnes

komplett_no_outliers <- komplett_used %>% filter(abs(resid_std) <= 3)

komplett_no_outliers <- komplett_no_outliers %>% 
  rename(logGDPpc = `log(GDPpc)`)


ols_no.outliers <- lm(logGDPpc ~ Government.Spending + Education.Expenditure + Resource.Rents +
                        Rule.of.Law + Years.with.SWF,
                      data = komplett_no_outliers)
tab_model(ols_no.outliers, show.ci = FALSE, show.std = TRUE, title = "No Outliers")

## Fixed effects vs. Random effects:
library(plm)

### fixed effects
fe_model <- plm(log(GDPpc) ~ Government.Spending + Education.Expenditure + Resource.Rents + Rule.of.Law + Active.Resource.SWF, 
                data = komplett, 
                model = "within")

re_model <- plm(log(GDPpc) ~ Government.Spending + Education.Expenditure + Resource.Rents + Rule.of.Law + Active.Resource.SWF, 
                data = komplett, 
                model = "random")

hausman_test <- phtest(fe_model, re_model)
print(hausman_test)

## Robust standardfeil
library(lmtest)
library(sandwich)
library(stargazer)

coeftest(ols6, vcov = vcovCL(ols6, cluster = ~Country))


robust_se <- sqrt(diag(vcovCL(ols6, cluster = ~Country)))

stargazer(ols7, 
          se = list(robust_se),
          type = "text",
          title = "OLS med robuste standardfeil (clustered på land)",
          dep.var.labels = "BNP per innbygger",
          digits = 3,
          notes = "Standard errors are clustered at country level.")

# Lag tabell med clustered SE
modelsummary(
  list("OLS with Clustered SEs" = ols6),
  vcov = ~Country,
  statistic = "std.error",
  stars = TRUE,
  gof_omit = "AIC|BIC|Log.Lik",  # Fjerner unødvendige GOF
  output = "flextable",
  title = "OLS Regression with Clustered Standard Errors (by Country)"
) |>
  autofit() |> 
  add_footer_lines("Note: Standard errors are clustered at the country level.")



# Lager deskriptiv statistikk
library(vtable)

navn <- c("GDP per capita", "Years with SWF", "Government Spending", "Education Expenditure", 
          "Resource Rents", "Rule of Law")

st(komplett, labels = navn,
   vars = c("GDPpc", "Years.with.SWF", "Government.Spending","Education.Expenditure", "Resource.Rents",
            "Rule.of.Law"),
   summ = c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'),
   digits = 4,
   summ.names = list(
     c('Number of Observations','Mean','Standard Deviation',
       'Min','Max')))

# Plot ?
library(ggplot2)

# Filtrer bort land uten SWF
plotdata <- komplett %>%
  filter(!is.na(Years.with.SWF)) %>%
  group_by(Years.with.SWF) %>%
  summarise(mean_log_GDPpc = mean(log(GDPpc), na.rm = TRUE),
            n = n())

# Hent prediksjoner som vektor
preds <- predict(ols6)

# Hent radnumre brukt i modellen
model_index <- as.numeric(rownames(model.frame(ols6)))

# Hent tilsvarende rader fra komplett
komplett_used <- komplett[model_index, ]

# Legg til predikerte verdier
komplett_used$predicted_log_GDP <- preds

plotdata_pred <- komplett_used %>%
  filter(!is.na(Years.with.SWF)) %>%
  group_by(Years.with.SWF) %>%
  summarise(mean_pred_log_GDPpc = mean(predicted_log_GDP, na.rm = TRUE),
            n = n())

# Plot 1 - LINE PLOT MED GJ.SNITTLIG PREDIKERTE VERDIER AV LOG GDP
ggplot(plotdata_pred, aes(x = Years.with.SWF, y = mean_pred_log_GDPpc)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, color = "red", linetype = "dashed") +
  labs(title = "Predicted average GDP per capita by years with SWF",
       x = "Years with a Sovereign Wealth Fund",
       y = "Mean predicted value of log(GDP per capita)") +
  scale_x_continuous(breaks = seq(0, max(plotdata_pred$Years.with.SWF, na.rm = T),
                                  by = 10)) +
  theme_light()

head(plotdata_pred)


# Plot 2 - BOX PLOT MED PREDIKERTE RVERDIER
model_data <- model.frame(ols6) %>%
  mutate(predicted_log_GDP = predict(ols6)) %>% 
  rename(Years.with.SWF.category = `factor(Years.with.SWF.category)`)

ggplot(model_data, aes(x = factor(Years.with.SWF.category), y = predicted_log_GDP)) +
  geom_boxplot() +
  labs(title = "Predicted GDP per capita by SWF duration category",
       x = "SWF duration category",
       y = "Predicted log(GDP per capita)") +
  theme_light()





