#
# across <- c(Formulation =  "FORMf", Sex = "SEXf")
# down <- c(Renal = "RFf", `Race` = "ASIANf", Study = "STUDYf")
# nn_factor <- function(x, col) {
#   factor(x, levels = col, labels = names(col))
# }
#
#
# data <- select(data, all_of(unname(c(across, down, "ID"))))
# data <- mutate(data, NNN = n())
# NNN <- data$NNN[1]
#
# long <- pivot_longer(data, cols = all_of(unname(down)))
# long2 <- pivot_longer(long, cols = all_of(unname(across)),
#                       names_to="name2", values_to="value2")
#
# summ <- group_by(long2, name, name2, value, value2)
# summ <- count(summ)
# summ <- ungroup(summ)
#
# summ <- mutate(summ, name = nn_factor(name, down))
# summ <- mutate(summ, name2 = nn_factor(name2, across))
#
# summ <- mutate(summ, name2 = paste(name2, value2, sep = '__'))
#
#
# summ$value2 <- NULL
#
# summ1 <- mutate(
#   summ,
#   summary = paste0(n, " (",  pmtables:::digit1(100*n/NNN), ")")
# )
# summ1 <- pivot_wider(
#   select(summ1, -n),
#   names_from="name2",
#   values_from="summary"
# )
#
# summ2 <- distinct(
#   summ,
#   name, value, n
# )
# summ2 <- group_by(summ2, name, value) %>%
#   summarise(n = sum(n)) %>% group_by(name) %>%
#   mutate(N = sum(n), pct = 100*n/N,
#          Summary = paste0(n, " (", pmtables:::digit1(pct), ")")) %>%
#   select(-n, -N, -pct) %>%
#   ungroup()
#
# summ1 <- left_join(summ1, summ2)
# summ1 <- select(summ1, name, value, everything())
#
#
# bot <- pivot_longer(data, cols = all_of(unname(across)))
# bot <- group_by(bot, name, value, NNN)
# bot <- count(bot)
# bot <- ungroup(bot)
# bot <- mutate(bot, name = nn_factor(name, across))
# bot <- mutate(bot, name = paste0(name, "__", value))
# bot$value <- NULL
# bot <- mutate(bot, Summary = paste0(n, " (", pmtables:::digit1(100*n/NNN), ")"))
#
# bot <- pivot_wider(select(bot, -n), values_from = "Summary")
#
# bot <- mutate(bot, Summary = paste0(NNN, " (100.0)"))
#
# bot$name <- NA
# bot$value <- tex_bold("Summary")
#
# final <- summ1 %>% mutate(NNN= NULL)
# bot <- select(bot, names(final))
# final <- bind_rows(bot, final)
# names(final) <- sub("Summary", tex_bold("Summary"), names(final), fixed = TRUE)
