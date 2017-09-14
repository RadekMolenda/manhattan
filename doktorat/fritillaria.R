library(dplyr)
library(ggplot2)
library(corrgram)

library(car)

data = read.csv("fritillaria_full.csv", stringsAsFactors = FALSE)

humming_bird <- c('F. recurva', 'F. gentneri')
sparrow <- c('F. imperialis', 'F.eduardi')

data$zapylacz <- ifelse(data$Gatunek %in% humming_bird, 'humming_bird', ifelse(data$Gatunek %in% sparrow, 'sparrow', 'insect'))

# Szukaj nieliczbowych opisów w danych
data$Gatunek <- factor(data$Gatunek)
data$zapylacz <- factor(data$zapylacz)
data$PL <- as.numeric(data$PL)
data$K.J <- as.numeric(data$K.J)
data$PYLZN <- as.numeric(data$PYLZN)
data$ZN <- as.numeric(data$ZN)
data$SZY <- as.numeric(data$SZY)
data$KAT <- as.numeric(data$KAT)
data$KON <- as.numeric(data$KON)
data$MASA <- as.numeric(data$MASA)

lapply(data, class)

data.numeric <- data.frame(sapply(data, is.numeric))
corrgram(data.numeric, lower.panel = panel.pie, upper.panel = panel.pts, diag.panel = panel.density)

toTest <- data %>%
    select(V, zapylacz) %>%
    filter(!is.na(V))

pairs(data)

# H0: wariancje w grupach sa takie same
leveneTest(toTest$V ~ toTest$zapylacz)

#H0: srednie sie nie roznia
t.test(toTest$V[toTest$zapylacz == "insect"], toTest$V[toTest$zapylacz == "sparrow"], var.equal = F)

#H0: srednie sie nie roznia
t.test(toTest$V[toTest$zapylacz == "insect"], toTest$V[toTest$zapylacz == "humming_bird"], var.equal = F)

#H0: srednie sie nie roznia
wilcox.test(toTest$V[toTest$zapylacz == "insect"], toTest$V[toTest$zapylacz == "sparrow"], conf.int = T)

#H0: srednie sie nie roznia
wilcox.test(toTest$V[toTest$zapylacz == "insect"], toTest$V[toTest$zapylacz == "humming_bird"], conf.int = T)

toTestKON <- data %>%
    select(KON, zapylacz) %>%
    filter(!is.na(KON))

t.test(x = toTestKON$KON[toTestKON$zapylacz == "insect"], y = toTestKON$KON[toTestKON$zapylacz == "sparrow"])

t.test(x = toTestKON$KON[toTestKON$zapylacz == "insect"], y = toTestKON$KON[toTestKON$zapylacz == "humming_bird"])

wilcox.test(x = toTestKON$KON[toTestKON$zapylacz == "insect"], y = toTestKON$KON[toTestKON$zapylacz == "sparrow"], conf.int = T)

wilcox.test(x = toTestKON$KON[toTestKON$zapylacz == "insect"], y = toTestKON$KON[toTestKON$zapylacz == "humming_bird"], conf.int = T)

data %>%
    filter(!is.na(V)) %>%
    leveneTest(V ~ zapylacz, data = .)

ANOVA_V <- data %>%
    filter(!is.na(V)) %>%
    aov(formula = V ~ zapylacz, data = .)

ANOVA_V$coefficients

plot(TukeyHSD(ANOVA_V))

ANOVA_KON <- data %>%
    filter(!is.na(KON)) %>%
    aov(formula = KON ~ zapylacz, data = .)

ANOVA_KON$coefficients

plot(TukeyHSD(ANOVA_KON))


data$sparrowPollinator <- as.factor(data$zapylacz == 'sparrow')

sparrowData <- data %>%
    filter(!is.na(logV)) %>%
    select(logV, sparrowPollinator)

data_perm %>%
    summarize(mean(diff_perm <= diff_orig))

                                        # wartość średnia i odchylenia starndardowe logV ze względu na grupę zapylaczy
data %>%
    filter(!is.na(logV)) %>%
    group_by(zapylacz) %>%
    summarize(n(), mean(logV), sd(logV))

                                        # Wykres gęstości ln(V) dla całej populacji
data %>%
    filter(!is.na(logV)) %>%
    ggplot(aes(x = logV)) +
    geom_density()

                                        # Wykres gęstości ln(V) ze względu na grupy zapylaczy
data %>%
    filter(!is.na(logV)) %>%
    ggplot(aes(x = logV, fill = zapylacz)) +
    geom_density(alpha = .3)

                                        # Wykres pudełkowy ln(V) ze względu na grupy zapylaczy
data %>%
    filter(!is.na(logV)) %>%
    ggplot(aes(y = logV, x = zapylacz)) +
    geom_boxplot()

                                        # Wykres pudełkowy objętości
data %>%
    select(V, zapylacz) %>%
    filter(!is.na(V)) %>%
    ggplot(aes(x = zapylacz, y = V)) +
    geom_boxplot()

                                        # histogram objętości
data %>%
    filter(!is.na(V) & V > 0) %>%
    select(V, zapylacz) %>%
    ggplot(aes(x = V)) +
    geom_histogram()

                                        # Wykres gęstości objętości per Gatunek
data %>%
    filter(!is.na(V)) %>%
    filter(Gatunek == c(humming_bird, sparrow)) %>%
    select(V, zapylacz, Gatunek) %>%
    ggplot(aes(x = V, fill = zapylacz)) +
    geom_dotplot() +
    geom_density(alpha = .3) +
    facet_wrap(~zapylacz)

                                        # Podsumowanie objętości
data %>%
    filter(!is.na(V)) %>%
    group_by(zapylacz) %>%
    summarize(
        n(),
        mean(V, na.rm = TRUE),
        sd(V, na.rm = TRUE),
        median(V, na.rm = TRUE),
        IQR(V, na.rm = TRUE)) %>%
    tbl_df() %>%
    as.data.frame

                                        # Liczba istotnych obserwacji objętości per Gatunek
data %>%
    filter(!is.na(V)) %>%
    filter(Gatunek == c(humming_bird, sparrow)) %>%
    select(V, zapylacz, Gatunek) %>%
    group_by(Gatunek) %>%
    summarize(n(), mean(V), sd(V), median(V), IQR(V)) %>%
    as.data.frame


                                        # podsumowanie koncentracji
data %>%
    filter(!is.na(KON)) %>%
    filter(Gatunek == c(humming_bird, sparrow)) %>%
    select(KON, zapylacz, Gatunek) %>%
    group_by(Gatunek) %>%
    summarize(n(), mean(KON), sd(KON), median(KON), IQR(KON)) %>%
    as.data.frame

                                        # Wykres gęstości koncentracji per Gatunek
data %>%
    filter(!is.na(KON)) %>%
    filter(Gatunek == c(humming_bird, sparrow)) %>%
    select(KON, zapylacz, Gatunek) %>%
    ggplot(aes(x = KON, fill = zapylacz)) +
    geom_dotplot() +
    geom_density(alpha = .3) +
    facet_wrap(~Gatunek)

                                        # Wykres pudełkowy koncentracji w rozbiciu na rodzaj zapylacza
data %>%
    filter(!is.na(KON)) %>%
    select(KON, zapylacz, Gatunek) %>%
    ggplot(aes(x = zapylacz, y = KON)) +
    geom_boxplot()

                                        # Wykres gęstości Długość płatka per Gatunek
data %>%
    filter(!is.na(PL)) %>%
    select(PL, zapylacz, Gatunek) %>%
    ggplot(aes(fill = zapylacz, x = PL)) +
    geom_dotplot(binwidth = .1) +
    geom_density(alpha = .3)

                                        # Wykres pudełkowy Długość płatka w zależności od zapylacza
data %>%
    filter(!is.na(PL)) %>%
    select(PL, zapylacz, Gatunek) %>%
    ggplot(aes(x = zapylacz, y = PL)) +
    geom_boxplot()

data %>%
    filter(!is.na(PL)) %>%
    select(PL, zapylacz) %>%
    group_by(zapylacz) %>%
    summarize(n(), median(PL), sd(PL))
