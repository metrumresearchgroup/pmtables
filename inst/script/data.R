
library(tidyverse)
data <- read_csv("inst/datasets/analysis1.csv", na = '.')
data <- select(data, -LDOS,-TAFD,-TAD,-MDV)
data <- rename(data, BQL = BLQ)
set.seed(112233)
id <- data %>% filter(SEQ==0) %>% distinct(ID,STUDY,WT,ALB,CRCL)
id <- mutate(
  id,
  SEX = case_when(
    STUDY <=2 ~ rbinom(n(),1, 0.6),
    STUDY > 2 ~ rbinom(n(),1,0.45)
  ),
  RACE = sample(1:3, n(), prob = c(0.5, 0.4, 0.1),replace = TRUE),
  STUDY = NULL,
  FORM = sample(c("tablet", "capsule", "troche"), n(), prob = c(0.8, 0.1, 0.1), replace = TRUE),
  WT = ifelse(rbinom(n(),1,0.01) ==1, NA_real_, WT),
  ALB = ifelse(rbinom(n(),1,0.02) ==1, NA_real_, ALB),
  CRCL = ifelse(rbinom(n(),1,0.07)==1, NA_real_,CRCL),
  ASIAN = sample(c("Asian", "non-Asian"), n(), prob = c(0.4,0.6), replace = TRUE)

)

data$WT <- NULL
data$ALB <- NULL
data$CRCL <- NULL

data <- left_join(data,id)

set.seed(22929)
data <- mutate(
  data,
  BQL = rbinom(n(),1, 0.035),
  DV = ifelse(BQL==1, NA_real_, DV)
)


# pd endpoint
end1 <- filter(data,SEQ==1,STUDY>2) %>% group_by(ID) %>% slice(1) %>% ungroup()
end1 <- mutate(end1,SEQ=2)
set.seed(282829)


set.seed(1009)
end2 <- filter(data,SEQ==1,STUDY > 2) %>% group_by(ID) %>% slice(sample(seq(n()),3)) %>% ungroup()
end2 <- mutate(end2,SEQ=3)
end2 <- mutate(
  end2,
  BQL = rbinom(n(),1,0.02),
  DV = ifelse(BQL==1, NA_real_, DV)
)
data <- bind_rows(data,end1,end2) %>% arrange(ID,TIME,SEQ)

data <- ungroup(data)

set.seed(112022)
pmiss <- 0.01
data <- mutate(
  data,
  DV = ifelse(rbinom(n(),1,pmiss)==1, NA_real_, DV)
)

data <- mutate(data, STUDYN = STUDY)
spec <- yspec::ys_load("inst/datasets/analysis1.yml")
data <- select(data, names(spec))
data <- yspec::yspec_add_factors(data,spec,RF,SEX,CP,SEQ,STUDY,FORM,ASIAN,.suffix="f")


saveRDS(file = "inst/datasets/all.RDS",data)
id <- distinct(data,ID,.keep_all=TRUE)
saveRDS(file = "inst/datasets/id.RDS",id)
obs <- filter(data,SEQ > 0)
saveRDS(file = "inst/datasets/obs.RDS",obs)

data <- pmtables:::data("id")

data <- group_by(data,SEXf,STUDYf) %>%
  summarise(WT = mean(WT,na.rm=TRUE), SCR = mean(SCR,na.rm=TRUE),
            ALB = mean(ALB,na.rm=TRUE), N = n(), .groups="drop")
data <- mutate(data, across(WT:ALB, .fns=sig))

saveRDS(file = "inst/datasets/ptdata.RDS", data)
