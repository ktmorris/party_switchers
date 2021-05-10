
cces <- fread("../regular_data/cces/CCES20_Common_OUTPUT.csv") %>% 
  mutate(diagnosed = CC20_309a_5 == 2,
         diagnosed_close = CC20_309a_1 == 1 | CC20_309a_2 == 1,
         died_close = (CC20_309b_2 == 1) * 1,
         died = CC20_309b_4 == 2,
         died = ifelse(is.na(died), 0, died),
         died_close = ifelse(is.na(died_close), 0, died_close)) %>% 
  select(ideo = CC20_340a,
         pid7,
         approval = CC20_320a,
         birthyr,
         gender,
         educ,
         race,
         diagnosed,
         faminc_new,
         died,
         died_close,
         diagnosed_close,
         voted = CC20_401,
         commonpostweight,
         party = CC20_433a,
         state = inputstate,
         pres_choice = CC20_410,
         presvote16post) %>% 
  mutate(voted = voted == 5) %>% 
  mutate_at(vars(gender, educ, race, state, party), as.factor) %>% 
  # filter(approval != 5) %>% 
  mutate(white = race == 1,
         strong = ifelse(approval %in% c(1, 4), 1, 0),
         diagnosed = as.integer(diagnosed),
         party_n = party,
         party = ifelse(party == 1, "Democrat", ifelse(party == 2, "Republican", "Other")),
         race = ifelse(race == 1, "White",
                       ifelse(race == 2, "Black",
                              ifelse(race == 3, "Latinx",
                                     ifelse(race == 4, "Asian", "Other")))),
         age = 2020 - birthyr,
         pid7 = ifelse(pid7 == 1, "Strong Democrat",
                       ifelse(pid7 == 2, "Democrat",
                              ifelse(pid7 == 7, "Strong Republican",
                                     ifelse(pid7 == 6, "Republican",
                                            ifelse(pid7 == 3, "Lean Democrat",
                                                   ifelse(pid7 == 5, "Lean Republican",
                                                          ifelse(pid7 == 4, "Neither", "Other"))))))))
cces$pid7 <- factor(cces$pid7,
                    levels = c("Strong Democrat", "Democrat", "Lean Democrat", "Neither",
                               "Lean Republican", "Republican", "Strong Republican", "Other"))
income_lu <- data.frame("faminc_new" = c(1:16),
                        "income" = c(5000, 15000, 25000, 35000, 45000, 55000, 65000, 75000,
                                     90000, 110000, 135000, 175000, 225000, 300000, 425000, 500000))

cces <- left_join(cces, income_lu)
########################################

demosp <- cces %>% 
  group_by(party) %>% 
  summarize_at(vars(died, diagnosed), ~ weighted.mean(., commonpostweight)) %>% 
  rename(Group = party) %>% 
  mutate(Group = ifelse(Group == "Other", "Other Party", Group))

demosi <- cces %>% 
  filter(!is.na(commonpostweight)) %>% 
  mutate(income = ifelse(income > 225000, 225000, income)) %>%
  mutate(income2 = ifelse(income < 50000, "Less than $50k", ifelse(income < 100000, "$50k - $100k",
                         "More than $100k"))) %>% 
  group_by(income2) %>% 
  summarize_at(vars(died, diagnosed), ~ weighted.mean(., commonpostweight, na.rm = T)) %>% 
  rename(Group = income2)

demosr <- cces %>% 
  filter(!is.na(commonpostweight)) %>% 
  group_by(race) %>% 
  summarize_at(vars(died, diagnosed), ~ weighted.mean(., commonpostweight)) %>% 
  rename(Group = race) %>% 
  mutate(Group = ifelse(Group == "Other", "Other Race", Group))

demosa <- cces %>% 
  filter(!is.na(commonpostweight)) %>% 
  mutate(age = ifelse(age < 45, "Less than 45", ifelse(age < 65, "45 - 64", "65 or Older"))) %>% 
  group_by(age) %>% 
  summarize_at(vars(died, diagnosed), ~ weighted.mean(., commonpostweight)) %>% 
  rename(Group = age) %>% 
  mutate(Group = ifelse(Group == "Other", "Other Race", Group))

demos <- bind_rows(demosr, demosp, demosi, demosa) %>% 
  filter(!is.na(Group)) %>% 
  mutate_at(vars(died, diagnosed), scales::percent, accuracy = 0.1)


demos$Group <- factor(demos$Group,
                      levels = c("Asian",
                                 "Black", "Latinx", "Other Race", "White",
                                 "Democrat", "Republican", "Other Party",
                                 "Less than $50k", "$50k - $100k", "More than $100k",
                                 "Less than 45", "45 - 64", "65 or Older"))

colnames(demos) <- c("Group", "Died", "Was Diagnosed")

demos <- arrange(demos, Group)

saveRDS(demos, "temp/demos.rds")

inco <- cces %>%
  filter(!is.na(commonpostweight)) %>%
  mutate(income = ifelse(income > 225000, 225000, income)) %>%
  group_by(income) %>%
  summarize_at(vars(died, diagnosed), ~weighted.mean(., commonpostweight)) %>%
  pivot_longer(cols = c("died", "diagnosed"))

ggplot(inco, aes(x = income, y = value, color = name)) + geom_point() + geom_line() + theme_bc() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::dollar) +
  labs(x = "Income", y = "Share Who Knew Someone Who... From COVID",
       color = "Group")
###########################################

cces$party <- factor(cces$party, levels = c("Republican", "Democrat", "Other"))

############################################

cces$action <- with(cces,
                    ifelse(!voted, "Did not vote",
                           ifelse((pres_choice == 1 & grepl("Rep", pid7)) |
                                    (pres_choice == 2 & grepl("Dem", pid7)), "Vote for Other",
                                  "Vote for Candidate")))


cces$action <- factor(cces$action, levels = c("Vote for Candidate", "Did not vote", "Vote for Other"))
cces$diagnosed <- factor(cces$diagnosed)
cces$died <- factor(cces$died)
cces$presvote16post <- factor(cces$presvote16post)
cces$ideo <- factor(cces$ideo)
reps <- filter(cces)


m2 <- multinom(action ~ diagnosed*party + age + gender + educ + income + ideo +
                 race, reps, weight = commonpostweight)

h <- ggeffect(model = m2, terms = c("diagnosed", "party"))

vp <- mutate(h, type = "Knew Someone Diagnosed")

m3 <- multinom(action ~ died*party + age + gender + educ + income + ideo +
                 race, reps, weight = commonpostweight)

h <- ggeffect(model = m3, terms = c("died", "party"))

vp <- bind_rows(vp,
                mutate(h, type = "Knew Someone Who Died"))

vp$group <- factor(vp$group, levels = c("Democrat", "Republican", "Other"))
# vp$group <- factor(vp$group,
#                     levels = c("Strong Democrat", "Democrat", "Lean Democrat", "Neither",
#                                "Lean Republican", "Republican", "Strong Republican", "Other"))

vp$response.level <- gsub("[.]", " ", vp$response.level)

ggplot(data = filter(vp, response.level == "Did not vote")) + 
  facet_grid(. ~ type) +
  geom_line(aes(x = as.integer(x)-1, y = predicted, color = group)) +
  geom_ribbon(aes(x = as.integer(x)-1, ymin = conf.low, ymax = conf.high, fill = group), alpha=0.25) +
  scale_color_manual(values = c("blue", "red", "green")) +
  scale_fill_manual(values = c("blue", "red", "green")) +
  theme_bc(base_family = "LM Roman 10") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
  labs(x = NULL, y = "Predicted Probability of Abstaining",
       fill = "Party", color = "Party")

p1 <- ggplot(data = filter(vp, response.level %in% c("Vote for Other", "Did not vote"),
                           response.level == "Did not vote" | !(group %in% c("Other", "Neither")))) + 
  facet_grid(type ~ response.level) +
  geom_line(aes(x = as.integer(x)-1, y = predicted, color = group)) +
  geom_errorbar(aes(x = as.integer(x)-1, ymin = conf.low, ymax = conf.high, color = group), width = 0.05) +
  scale_color_manual(values = c("blue", "red", "dark green")) +
  scale_fill_manual(values = c("blue", "red", "dark green")) +
  theme_bc(base_family = "LM Roman 10") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
  labs(x = NULL, y = "Predicted Probability of Action",
       fill = "Party", color = "Party",
       caption = "Note: Covariates include age; gender; education; income; 7-point political ideology; race / ethnicity.")
p1
saveRDS(p1, "temp/multi_out.rds")
#####
reps <- cces %>%
  filter(!is.na(commonpostweight)) %>%
  mutate(ideo = factor(ideo),
         died_close = as.integer(died_close),
         diagnosed_close = as.integer(diagnosed_close))

reps$race <- relevel(factor(reps$race), ref = "White")

m2 <- multinom(action ~ diagnosed*ideo + age + gender + educ + income + party +
                 race, reps, weight = commonpostweight)

h <- ggeffect(model = m2, terms = c("diagnosed", "ideo"))

vp <- mutate(h, type = "Knew Someone Diagnosed")

m3 <- multinom(action ~ died*ideo + age + gender + educ + income + party +
                 race, reps, weight = commonpostweight)

h <- ggeffect(model = m3, terms = c("died", "ideo"))

vp <- bind_rows(vp,
                mutate(h, type = "Knew Someone Who Died"))

##
vp1 <- filter(vp, group %in% c(5, 6, 7)) %>% 
  mutate(group = ifelse(group == 5, "Somewhat Convervative",
                       ifelse(group == 6, "Conservative", "Very Conservative")))

vp1$group <- factor(vp1$group, levels = c("Somewhat Convervative", "Conservative", "Very Conservative"))


fig <- ggplot(data = filter(vp1, response.level == "Did.not.vote")) +
  facet_grid( ~ type) +
  geom_line(aes(x = as.integer(x)-1, y = predicted, color = group)) +
  geom_errorbar(aes(x = as.integer(x)-1, ymin = conf.low, ymax = conf.high, color = group), width = 0.05) +
  scale_color_manual(values = c("black", "purple", "goldenrod")) +
  theme_bc(base_family = "LM Roman 10") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
  labs(x = NULL, y = "Predicted Probability of Abstaining",
       fill = "Ideology", color = "Ideology",
       caption = "Note: Covariates include age; gender; education; income; party affiliation; race / ethnicity.")
fig
saveRDS(fig, "temp/ideo_mef.rds")

####################################################

reps <- cces %>%
  filter(!is.na(commonpostweight),
         action != "Did not vote") %>%
  mutate(ideo = factor(ideo),
         vote_for_cand = pres_choice == 2)

reps$ideo <- relevel(factor(reps$ideo), ref = "7")

reps$race <- relevel(factor(reps$race), ref = "White")

m2 <- lm(vote_for_cand ~ diagnosed*ideo + age + gender + educ + income + party +
                 race, reps, weight = commonpostweight)

h <- ggeffect(model = m2, terms = c("diagnosed", "ideo"))

vp <- mutate(h, type = "Knew Someone Diagnosed")

m3 <- lm(vote_for_cand ~ died*ideo + age + gender + educ + income + party + 
                 race, reps, weight = commonpostweight)

h <- ggeffect(model = m3, terms = c("died", "ideo"))

vp <- bind_rows(vp,
                mutate(h, type = "Knew Someone Who Died"))

##
vp1 <- filter(vp, group %in% c(5, 6, 7)) %>% 
  mutate(group = ifelse(group == 5, "Somewhat Convervative",
                        ifelse(group == 6, "Conservative", "Very Conservative")))

vp1$group <- factor(vp1$group, levels = c("Somewhat Convervative", "Conservative", "Very Conservative"))

fig <- ggplot(data = filter(vp1)) +
  facet_grid(. ~ type) +
  geom_line(aes(x = as.integer(x)-1, y = predicted, color = group)) +
  geom_errorbar(aes(x = as.integer(x)-1, ymin = conf.low, ymax = conf.high, color = group), width = 0.05) +
  scale_color_manual(values = c("black", "purple", "goldenrod")) +
  theme_bc(base_family = "LM Roman 10") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
  labs(x = NULL, y = "Predicted Probability of Voting for Trump",
       fill = "Ideology", color = "Ideology",
       caption = "Note: Covariates include age; gender; education; income; party affiliation; race / ethnicity.")
fig
saveRDS(fig, "temp/ideo_vc_mef.rds")
