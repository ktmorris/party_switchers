
anes_pre <- read.dta13("../washington_covid/raw_data/anes_timeseries_2020_stata_20210324/anes_timeseries_2020_stata_20210324.dta")


anes_pre <- anes_pre %>% 
  mutate(ideol = V201200,
         stopped = V202456,
         age = V201507x,
         covid = V201624 == 1 | V201625 == 1,
         to = (V201022 == 1 & V201023 == 1) | V202066 == 4,
         white = V201549x == 1,
         black = V201549x == 2,
         latino = V201549x == 3,
         asian = V201549x == 4,
         appr = ifelse(V201142 == 1 & V201143 == 1, "Strongly Approve",
                       ifelse(V201142 == 1 & V201143 == 2, "Weakly Approve",
                              ifelse(V201142 == 2 & V201143 == 1, "Strongly Dissaprove", "Weakly Dissaprove"))),
         party = ifelse(V201018 == 1, "DEM",
                        ifelse(V201018 == 2, "REP", "OTH")))

anes_pre$party <- factor(anes_pre$party, levels = c("DEM", "REP", "OTH"))
anes_pre$appr <- factor(anes_pre$appr, levels = rev(c("Strongly Approve", "Weakly Approve",
                                                      "Weakly Dissaprove", "Strongly Dissaprove")), ordered = T)

income <- fread("../washington_covid/raw_data/income_lu.csv")

anes_pre <- left_join(anes_pre, income)

#############################################3
m1 <- lm(to ~ stopped*black +party + age + income + ideol, anes_pre, weight = V200010a)
summary(m1)
############################################

reg_d <- select(anes_pre, covid, ideol, white, black, latino, party,
                age, income, appr, V200010a, to, asian, pres = V201127,
                state = V201011) %>% 
  filter(pres %in% c(1, 2)) %>% 
  mutate(pres = pres == 1) %>% 
  mutate_at(vars(white, black, latino, asian, to, covid, pres), ~ ifelse(., 1, 0)) %>% 
  mutate(appr_int = as.numeric(appr),
         strong = ifelse(appr_int %in% c(1, 4), 1, 0)) %>% 
  filter(ideol > 0, ideol < 8)

m1 <- lm(to ~ covid*ideol*black + white + latino + asian + age + income, anes_pre, weight = V200010a)
summary(m1)
stargazer(m1,
          type = "text",
          covariate.labels = c("Someone in HH with COVID",
                               "Other Party (rel. to Dem)",
                               "Republican Party (rel. to Dem)",
                               "White",
                               "Black",
                               "Latino",
                               "Asian",
                               "Age",
                               "Income",
                               "COVID * Other Party",
                               "COVID * Republican"),
          dep.var.labels = "Already Voted / Intends to Vote")



stargazer(m, type = "text",
          covariate.labels = c("Someone in HH with COVID",
                               "Other Party (rel. to Dem)",
                               "Republican Party (rel. to Dem)",
                               "White",
                               "Black",
                               "Latino",
                               "Asian",
                               "Age",
                               "Income",
                               "COVID * Other Party",
                               "COVID * Republican"),
          dep.var.labels = "Approval of President's Response to COVID")

reg_d$app_int <- as.numeric(reg_d$appr)

reg_d$b <- ifelse(reg_d$black, "Black", "Non-Black")

m1 <- lm(to ~ app_int*white*covid + pres + age + income + ideol, reg_d, weight = V200010a)
summary(m1)
library(ggeffects)

mydf <- ggpredict(m1, terms = c("app_int", "covid", "white"))

ggplot(mydf, aes(x, predicted, colour = group)) + geom_line() + facet_grid(~ facet)
plot(mydf)
