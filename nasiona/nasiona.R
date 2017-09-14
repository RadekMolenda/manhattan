library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
rep_sample_n <- function(tbl, size, replace = FALSE, reps = 1)
{
    n <- nrow(tbl)
    i <- unlist(replicate(reps, sample.int(n, size, replace = replace), simplify = FALSE))

    rep_tbl <- cbind(replicate = rep(1:reps,rep(size,reps)), tbl[i,])

    dplyr::group_by(rep_tbl, replicate)
}

seeds = read.csv('nasiona_2015_2017.csv')
seeds = seeds[1:5]

seeds$exp..treatment <- factor(seeds$exp..treatment)
seeds$year <- factor(seeds$year)

#filter out data with missing treatment
seeds <- seeds %>% filter(!is.na(exp..treatment)) %>% droplevels()

# Roczny rozkład próbek

seeds <- seeds %>% mutate(standardSeedNo = ifelse(exp..treatment == 1 | exp..treatment == 2, log10(seed.no + 0.01), seed.no))


seeds %>%
    ggplot(aes(x = seed.no, fill=exp..treatment)) +
    geom_density(alpha = .3) +
    facet_wrap(~year)

seeds %>%
    filter(exp..treatment == 1) %>%
    droplevels() %>%
    ggplot(aes(x = log(seed.no + 0.01), fill=exp..treatment)) +
    geom_density(alpha = .3) +
    facet_wrap(~year)

seeds %>%
    filter(exp..treatment == 2) %>%
    droplevels() %>%
    ggplot(aes(x = seed.no + 0.01, fill=exp..treatment)) +
    scale_x_log10() +
    geom_density(alpha = .3) +
    facet_wrap(~year)

seed_summary <- seeds %>%
    group_by(year, exp..treatment) %>%
    summarize(mean(seed.no, na.rm = TRUE), sd(seed.no, na.rm = TRUE), median(seed.no, na.rm = TRUE))

grouped <- seeds %>%
    filter(year == 2016 | year == 2017) %>%
    group_by(year, exp..treatment) %>%
    summarize(cor(pop..size, seed.no, use="complete.obs"))

jedynka <- seeds %>%
    filter(exp..treatment == 1)

czworka <- seeds %>%
    filter(exp..treatment == 4)

jedynka %>%
    rep_sample_n(size = nrow(czworka), reps = 1000) %>%
    group_by(replicate) %>%
    summarize(cc = cor(seed.no, czworka$seed.no, use="pairwise.complete.obs")) %>%
    ggplot(aes(x = cc)) + geom_dotplot(binwidth = 0.004)


n1 <- sample_n(women, 15)
n2 <- sample_n(women, 10)

n1 %>%
    rep_sample_n(size = nrow(n2), reps = 1000) %>%
    group_by(replicate) %>%
    summarize(cc = cor(n2$height, weight, use="complete.obs")) %>%
    ggplot(aes(x = cc)) + geom_dotplot(binwidth = 0.01)

n3 <- sample_n(n1, 10)

x <- data.frame(a = n2$hp, b =n3$mpg)
ggplot(x, aes(x = a, y = b)) + geom_point()

ggplot(mtcars, aes(x = hp, y = mpg)) + geom_point()

cor(mtcars$hp, mtcars$mpg)

cor(n3)

cor(women)

one_and_four = data.frame(one = jedynka$seed.no, two = czworka$seed.no)

boxplot(jedynka$seed.no)

ggplot(jedynka, aes(x = seed.no)) + geom_density()

ggplot(czworka, aes(x = seed.no)) + geom_density()

czworka %>%
    filter(seed.no != 0) %>%
    ggplot(aes(x = seed.no)) + geom_density()

ssummary(seeds)

filtered <- seeds %>%
    filter(!is.na(seed.no) && !is.na(pop..size)) %>%
    droplevels()

nrow(filtered)
nrow(seeds)

cor(filtered$pop..size, filtered$seed.no)
