
library(tidyverse)
library(usethis)

data <- read_csv("analysis1.csv", na = '.')
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

data <- select(data, -c(WT,ALB,CRCL))

data <- left_join(data,id, by = "ID")

set.seed(22929)
data <- mutate(
  data,
  BQL = rbinom(n(),1, 0.035),
  DV = ifelse(BQL==1, NA_real_, DV)
)


# pd endpoint
end1 <- filter(data,SEQ==1,STUDY>2) %>%
  group_by(ID) %>% slice(1) %>% ungroup()
end1 <- mutate(end1,SEQ=2)

set.seed(1009)
end2 <- filter(data,SEQ==1,STUDY > 2) %>%
  group_by(ID) %>% slice(sample(seq(n()),3)) %>%
  ungroup()
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
spec <- yspec::ys_load("analysis1.yml")
data <- select(data, names(spec))
data <- yspec::yspec_add_factors(data,spec,RF,SEX,CP,SEQ,STUDY,FORM,ASIAN,.suffix="f")

analysis1 <- data
usethis::use_data(analysis1, overwrite = TRUE)

pmt.first <- distinct(data,ID,.keep_all=TRUE)
usethis::use_data(pmt.first, overwrite = TRUE)

pmt.obs <- filter(data,SEQ > 0)

usethis::use_data(pmt.obs, overwrite = TRUE)

pmt.pk <- filter(data, SEQ==1)
usethis::use_data(pmt.pk, overwrite = TRUE)

data <- analysis1
summ <-
  data %>%
  group_by(ID) %>%
  mutate(DOSE = first(AMT[!is.na(AMT)])) %>%
  ungroup

summ <- mutate(summ, DOSEf = factor(DOSE, unique(DOSE), paste0(unique(DOSE), " mg")))
summ <- filter(summ, DOSE >= 50)
summ <- group_by(summ, STUDYf,DOSEf,FORMf)
summ  <- summarise(summ,N = n(),  WT = mean(WT,na.rm=TRUE), CRCL = mean(CRCL,na.rm=TRUE), AGE = mean(AGE,na.rm=TRUE),
                   ALB = mean(ALB,na.rm=TRUE),
                   SCR = mean(SCR,na.rm=TRUE), .groups="drop")

summ <- mutate(summ,across(c(WT,CRCL,AGE,ALB,SCR), ~ pmtables:::sig(.x)))
pmdata <- modify(summ,as.character) %>% ungroup()
pmdata <- arrange(pmdata, STUDYf,DOSEf,FORMf)
pmdata <- rename(pmdata, STUDY = STUDYf, DOSE = DOSEf, FORM = FORMf)
pmt.summ <- pmdata
usethis::use_data(pmt.summ, overwrite = TRUE)
