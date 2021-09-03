###############################################################################
###########   ANALISE - SERIES TEMPORAIS FINANCEIRAS  - COMIM   ###############
###############################################################################

### DIRETORIO ------------------------------------------------------------

#setwd(paste0("C:\\Users\\Admin\\Nel-Matrix Dropbox\\Iman Ghodratitoostani (iman.ghodrati)\\My PC (LAPTOP-1JUOMSSF)\\Desktop\\Comim_papers_4_5"))

#setwd(paste0("C:\\Users\\Admin\\Nel-Matrix Dropbox\\Iman Ghodratitoostani (iman.ghodrati)\\My PC (LAPTOP-1JUOMSSF)\\Documents\\R\\win-library\\4.0"))

setwd(paste0("C:\\Users\\Admin\\Nel-Matrix Dropbox\\Iman Ghodratitoostani (iman.ghodrati)\\My PC (LAPTOP-1JUOMSSF)\\Desktop\\WORK\\Comim_papers_4_5"))


options("install.lock"=FALSE)

install.packages(c("bmem", "ggimage", "ggstatsplot", "ggthemes", "gvlma", "MBESS", 
                   "mediation", "powerMediation", "psych", "remotes", "RMediation", 
                   "systemfit", "WriteXLS"), dependencies = TRUE, INSTALL_opts = '--no-lock')

### PACOTES --------------------------------------------------------------

library(car)
library(systemfit)
library(bmem)
library(mediation)
library(powerMediation)
library(RMediation)
library(psych)
library(MBESS)
library(tidyverse)
library(ggplot2)
library(remotes)
library(ggstatsplot)
library(plotly)
library(lubridate)
library(hrbrthemes)
library(ggfortify)
library(ggpmisc)
library(cowplot)
library(ggpubr)
library(gridExtra)
library(ggimage)
library(colorspace)
library(stringr)
library(dplyr)
library(tidyr)
library(mediation)
library(gvlma)
library(ggthemes)
library(viridis)
library(xlsx)
library(WriteXLS)
library(ggpubr)
library(stringr)
library(NLP)
library(ggridges)
library(strucchange)
library(writexl)

## ------- 
## DADOS    
## ------- 

#dados_base <- read.csv2("dados1.csv", header = T, sep = ",")
dados_base <- read.csv2(file.choose(), header = T, sep = ",")


dados <- dados_base
head(dados)
dim(dados)
names(dados)

dados <- dados %>%
          mutate(Going_Global1.0 = as.numeric(dados$Year %in% c(2005,2006,2007,2008,2009,2010,2011))) %>%
          mutate(Going_Global2.0 = as.numeric(dados$Year %in% c(2012,2013,2014,2015,2016))) %>%
          mutate(Going_Global3.0 = as.numeric(dados$Year %in% c(2017,2018,2019,2020))) %>%
          mutate(Change_Startegy = as.numeric(dados$Year %in% c(2016,2017,2018,2019,2020)))

head(dados)

#write_xlsx(dados,"work_data.xlsx")
## ----------------------------
## TRANSFORMACAO DE VARIÁVEIS - Quantity.in.Millions em float
## ---------------------------- 

montante <- rep(NA, nrow(dados))

for (ii in 1:nrow(dados)) {
  aux <- as.String(dados$Quantity.in.Millions[ii])
  montante[ii] <- as.numeric(str_remove(aux, "[$]"))
}

montante

dados$Quantity.in.Millions <- montante

ggcorrmat(
  data = dados,
  colors = c("#B2182B", "white", "#4D4D4D"),
  title = "Correlalogram of variable in study",
  subtitle = "Tracker 2020"
)

## -------
## Year 
## -------

invest_year <-
  dados %>%
    select(Year, Quantity.in.Millions) %>%
    group_by(Year)

invest_year

# Boxplot
ggbetweenstats(
  data = invest_year,
  x = Year,
  y = Quantity.in.Millions,
  xlab = "Year",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)

# Pie graph
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                "#000000", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

invest_year %>%
  group_by(Year) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions)) %>%
  ggplot(aes(x="", y=Amount_investment, colour = Year)) +
    geom_bar(stat="identity", width=1) +
    #scale_fill_manual(values = cbbPalette)
    coord_polar("y", start=0)


doughnut <-
  function (x, labels = names(x), edges = 200, outer.radius = 0.8,
            inner.radius=0.6, clockwise = FALSE,
            init.angle = if (clockwise) 90 else 0, density = NULL,
            angle = 45, col = NULL, border = FALSE, lty = NULL,
            main = NULL, ...)
  {
    if (!is.numeric(x) || any(is.na(x) | x < 0))
      stop("'x' values must be positive.")
    if (is.null(labels))
      labels <- as.character(seq_along(x))
    else labels <- as.graphicsAnnot(labels)
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
    plot.new()
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1L] > pin[2L])
      xlim <- (pin[1L]/pin[2L]) * xlim
    else ylim <- (pin[2L]/pin[1L]) * ylim
    plot.window(xlim, ylim, "", asp = 1)
    if (is.null(col))
      col <- if (is.null(density))
        palette()
    else par("fg")
    col <- rep(col, length.out = nx)
    border <- rep(border, length.out = nx)
    lty <- rep(lty, length.out = nx)
    angle <- rep(angle, length.out = nx)
    density <- rep(density, length.out = nx)
    twopi <- if (clockwise)
      -2 * pi
    else 2 * pi
    t2xy <- function(t, radius) {
      t2p <- twopi * t + init.angle * pi/180
      list(x = radius * cos(t2p),
           y = radius * sin(t2p))
    }
    for (i in 1L:nx) {
      n <- max(2, floor(edges * dx[i]))
      P <- t2xy(seq.int(x[i], x[i + 1], length.out = n),
                outer.radius)
      polygon(c(P$x, 0), c(P$y, 0), density = density[i],
              angle = angle[i], border = border[i],
              col = col[i], lty = lty[i])
      Pout <- t2xy(mean(x[i + 0:1]), outer.radius)
      lab <- as.character(labels[i])
      if (!is.na(lab) && nzchar(lab)) {
        lines(c(1, 1.05) * Pout$x, c(1, 1.05) * Pout$y)
        text(1.1 * Pout$x, 1.1 * Pout$y, labels[i],
             xpd = TRUE, adj = ifelse(Pout$x < 0, 1, 0),
             ...)
      }
      ## Add white disc          
      Pin <- t2xy(seq.int(0, 1, length.out = n*nx),
                  inner.radius)
      polygon(Pin$x, Pin$y, density = density[i],
              angle = angle[i], border = border[i],
              col = "white", lty = lty[i])
    }
    
    title(main = main, ...)
    invisible(NULL)
  }

invest_year <-
  invest_year %>%
    group_by(Year) %>%
    mutate(Amount_investment = sum(Quantity.in.Millions)) %>%
    select(Year, Amount_investment) 

doughnut(unique(invest_year$Amount_investment), 
         labels = unique(invest_year$Year),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4)))

# Histogram 

  gghistostats(
    data = invest_year,
    x = Year,
    test.value = 30
  )

hist_2005 <- 
  invest_year %>%
    filter(Year == 2005) %>%
    gghistostats(
      x = Quantity.in.Millions,
      xlab = "Quantity in Millions",
      test.value = 30
    )
  
hist_2006 <- 
  invest_year %>%
  filter(Year == 2006) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2007 <- 
  invest_year %>%
  filter(Year == 2007) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2008 <- 
  invest_year %>%
  filter(Year == 2008) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2009 <- 
  invest_year %>%
  filter(Year == 2009) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2010 <- 
  invest_year %>%
  filter(Year == 2010) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2011 <- 
  invest_year %>%
  filter(Year == 2011) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2012 <- 
  invest_year %>%
  filter(Year == 2012) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2013 <- 
  invest_year %>%
  filter(Year == 2013) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2014 <- 
  invest_year %>%
  filter(Year == 2014) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2015 <- 
  invest_year %>%
  filter(Year == 2015) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2016 <- 
  invest_year %>%
  filter(Year == 2016) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2017 <- 
  invest_year %>%
  filter(Year == 2017) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2018 <- 
  invest_year %>%
  filter(Year == 2018) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2019 <- 
  invest_year %>%
  filter(Year == 2019) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2020 <- 
  invest_year %>%
  filter(Year == 2020) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )


plots <- list(hist_2005, hist_2006, hist_2007, hist_2008, hist_2009,
              hist_2010, hist_2011, hist_2012, hist_2013, hist_2014,
              hist_2015, hist_2016, hist_2017, hist_2018, hist_2019,
              hist_2020)

ggstatsplot::combine_plots(
  plotlist = plots,
  annotation.args = list(title = "Quantity of Investment per year - in Million"),
  plotgrid.args = list(nrow = 5)
)


# Cumulative investiment per year

invest_year_sum <-
  dados %>%
  select(Year, Quantity.in.Millions) %>%
  group_by(Year) %>%
  mutate(Cumulative = sum(Quantity.in.Millions)) %>%
  select(Year, Cumulative) 

invest_year_sum <- as.data.frame(cbind(unique(invest_year_sum$Year), unique(invest_year_sum$Cumulative)))
colnames(invest_year_sum) <- c("Year", "Amount_Investment")


# plot
invest_year_sum %>% 
  ggplot(aes(x=Year, y=Amount_Investment, label=Year)) +
    geom_point() +
    geom_segment(aes(
      xend=c(tail(Year, n=-1), NA), 
      yend=c(tail(Amount_Investment, n=-1), NA)
    )
    ) +
    xlab("Year") +
    ylab("Amount Investment - in Million of US$")


# STRUCTURAL CHANGE

myts1 <- invest_year_sum$Amount_Investment

test2 <- Fstats(myts1~1) 
myts1.fs <- test2$Fstats 
bp.myts1 <- breakpoints(myts1~1) 
plot(myts1) 
lines(bp.myts1) 
bd.myts1 <- breakdates(bp.myts1) 
sctest(test2) 
ci.myts1 <- confint(bp.myts1) 
plot(myts1)
lines(ci.myts1) 


## ----------
## Investor 
## ----------

investor_data <-
  dados %>%
    select(Quantity.in.Millions, Investor, Year)

labels(investor_data$Investor)



ggplot(investor_data, aes(x = `Quantity.in.Millions`, y = `Investor`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )



## --------------------
## Transaction.Party 
## --------------------

trans_party_data <-
  dados %>%
    select(Quantity.in.Millions, Transaction.Party)


ggplot(trans_party_data, aes(x = `Quantity.in.Millions`, y = `Transaction.Party`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )



## ----------
## Sector 
## ----------

sector_data <-
  dados %>%
    select(Quantity.in.Millions, Sector)


ggplot(sector_data, aes(x = `Quantity.in.Millions`, y = `Sector`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = sector_data,
  x = Sector,
  y = Quantity.in.Millions,
  xlab = "Sector",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)

dados %>%
  filter(Sector == "Real estate") %>%
  group_by(Year) %>%
  mutate(Amount_invest = sum(Quantity.in.Millions)) %>%
  ggplot(
    aes(x=Year, y=Amount_invest, label=Year)) +
  geom_point() +
  geom_segment(aes(
    xend=c(tail(Year, n=-1), NA), 
    yend=c(tail(Amount_invest, n=-1), NA)
  )
  ) +
  xlab("Year") +
  ylab("Amount Investment - in Million of US$")


sector_data <-
  dados %>%
    select(Quantity.in.Millions, Sector) %>%
    group_by(Sector) %>%
    mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(sector_data$Amount_investment), 
         labels = unique(sector_data$Sector),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4)))

## ----------
## Subsector 
## ----------
subsector_data <-
  dados %>%
  select(Quantity.in.Millions, Subsector)


ggplot(subsector_data, aes(x = `Quantity.in.Millions`, y = `Subsector`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = subsector_data,
  x = Subsector,
  y = Quantity.in.Millions,
  xlab = "Subsector",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


subsector_data <-
  dados %>%
  select(Quantity.in.Millions, Subsector) %>%
  group_by(Subsector) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(subsector_data$Amount_investment), 
         labels = unique(subsector_data$Subsector),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4)))

## ----------
## Region
## ----------


region_data <-
  dados %>%
    select(Quantity.in.Millions, Region)

ggplot(region_data, aes(x = `Quantity.in.Millions`, y = `Region`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = region_data,
  x = Region,
  y = Quantity.in.Millions,
  xlab = "Region",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)



region_data <-
  dados %>%
  select(Quantity.in.Millions, Region) %>%
  group_by(Region) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(region_data$Amount_investment), 
         labels = unique(region_data$Region),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))



## ----------
## Country 
## ----------


country_data <-
  dados %>%
    select(Quantity.in.Millions, Country)

# Density
ggplot(country_data, aes(x = `Quantity.in.Millions`, y = `Country`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )



## ------
## BRI 
## ------

BRI_data <-
  dados %>%
  select(Quantity.in.Millions, BRI) %>%
  filter(!is.na(BRI)) %>%
  mutate(bri = ifelse(BRI == 1, "BRI", "Others"))


ggplot(BRI_data, aes(x = `Quantity.in.Millions`, y = `bri`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = BRI_data,
  x = bri,
  y = Quantity.in.Millions,
  xlab = "bri",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


BRI_data <-
  BRI_data %>%
  select(Quantity.in.Millions, bri) %>%
  group_by(bri) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(BRI_data$Amount_investment), 
         labels = unique(BRI_data$bri),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))

## ------
## Greenfield 
## ------

Greenfield_data <-
  dados %>%
  select(Quantity.in.Millions, Greenfield) %>%
  mutate(GF = ifelse(Greenfield == 'G', "Greenfield", "Other"))


ggplot(Greenfield_data, aes(x = `Quantity.in.Millions`, y = `GF`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = Greenfield_data,
  x = GF,
  y = Quantity.in.Millions,
  xlab = "Greenfield",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


Greenfield_data <-
  Greenfield_data %>%
  select(Quantity.in.Millions, GF) %>%
  group_by(GF) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(Greenfield_data$Amount_investment), 
         labels = unique(Greenfield_data$GF),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))



## -------------
## Share.Size 
## -------------

Share.Size_data <-
  dados %>%
  select(Quantity.in.Millions, Share.Size)


ggplot(Share.Size_data, aes(x = `Quantity.in.Millions`, y = `Share.Size`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = Share.Size_data,
  x = Share.Size,
  y = Quantity.in.Millions,
  xlab = "Share.Size",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


Share.Size_data <-
  Share.Size_data %>%
  select(Quantity.in.Millions, Share.Size) %>%
  group_by(GF) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(Share.Size_data$Amount_investment), 
         labels = unique(Share.Size_data$GF),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))


## --------
## Month
## --------
month_data <-
  dados %>%
    select(Month, Quantity.in.Millions) %>%
    group_by(Month)


# Density
ggplot(month_data, aes(x = `Quantity.in.Millions`, y = `Month`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = month_data,
  x = Month,
  y = Quantity.in.Millions,
  xlab = "Month",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


month_data <-
  month_data %>%
  select(Quantity.in.Millions, Month) %>%
  group_by(Month) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(month_data$Amount_investment), 
         labels = unique(month_data$Month),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))


##-----------------------------------------------------##
##  MODELS    
##-----------------------------------------------------##


# HIPOTESE 1

Xc    <- dados$Year #Centering IV; hours of sleep
Zc    <- dados$Transaction.Party #Centering moderator; coffee consumption
Y     <- dados$Quantity.in.Millions
#Moderation "By Hand"
library(gvlma)
fitMod <- lm(Y ~ Xc + Zc + Xc*Zc) #Model interacts IV & moderator
summary(fitMod)

coef(summary(fitMod))
gvlma(fitMod)

library(stargazer)
stargazer(fitMod,type="text", title = "Sleep and Coffee on Attention")

# HIPOTESE 2


Xc    <- dados$Year #Centering IV; hours of sleep
Zc    <- dados$Region#Centering moderator; coffee consumption
Y     <- dados$Quantity.in.Millions
#Moderation "By Hand"
library(gvlma)
fitMod <- lm(Y ~ Xc + Zc + Xc*Zc) #Model interacts IV & moderator
summary(fitMod)

coef(summary(fitMod))
gvlma(fitMod)

library(stargazer)
stargazer(fitMod,type="text", title = "Sleep and Coffee on Attention")

# HIPOTESE 3

Xc    <- dados$Year #Centering IV; hours of sleep
Zc    <- dados$Sector #Centering moderator; coffee consumption
Y     <- dados$Quantity.in.Millions
#Moderation "By Hand"
library(gvlma)
fitMod <- lm(Y ~ Xc + Zc + Xc*Zc) #Model interacts IV & moderator
summary(fitMod)

coef(summary(fitMod))
gvlma(fitMod)

library(stargazer)
stargazer(fitMod,type="text", title = "Sleep and Coffee on Attention")

# HIPOTESE 4

Xc    <- dados$Year #Centering IV; hours of sleep
Zc    <- dados$Share.Size #Centering moderator; coffee consumption
Y     <- dados$Quantity.in.Millions
#Moderation "By Hand"
library(gvlma)
fitMod <- lm(Y ~ Xc + Zc + Xc*Zc) #Model interacts IV & moderator
summary(fitMod)

coef(summary(fitMod))
gvlma(fitMod)

library(stargazer)
stargazer(fitMod,type="text", title = "Sleep and Coffee on Attention")

# HIPOTESE 5

Xc    <- dados$Year #Centering IV; hours of sleep
Zc    <- dados$Greenfield #Centering moderator; coffee consumption
Y     <- dados$Quantity.in.Millions
#Moderation "By Hand"
library(gvlma)
fitMod <- lm(Y ~ Xc + Zc + Xc*Zc) #Model interacts IV & moderator
summary(fitMod)

coef(summary(fitMod))
gvlma(fitMod)

library(stargazer)
stargazer(fitMod,type="text", title = "Sleep and Coffee on Attention")

# HIPOTESE 6

Xc    <- dados$Year #Centering IV; hours of sleep
Zc    <- dados$Subsector #Centering moderator; coffee consumption
Y     <- dados$Quantity.in.Millions
#Moderation "By Hand"
library(gvlma)
fitMod <- lm(Y ~ Xc + Zc + Xc*Zc) #Model interacts IV & moderator
summary(fitMod)

coef(summary(fitMod))
gvlma(fitMod)

library(stargazer)
stargazer(fitMod,type="text", title = "Sleep and Coffee on Attention")

# HIPOTESE 7
dados_bri <- dados %>%
  filter(!is.na(BRI))
Xc    <- dados_bri$BRI #Centering IV; hours of sleep
Zc    <- dados_bri$Subsector #Centering moderator; coffee consumption
Y     <- dados_bri$Quantity.in.Millions
#Moderation "By Hand"
library(gvlma)
fitMod <- lm(Y ~ Xc + Zc + Xc*Zc) #Model interacts IV & moderator
summary(fitMod)

coef(summary(fitMod))
gvlma(fitMod)

library(stargazer)
stargazer(fitMod,type="text", title = "Sleep and Coffee on Attention")

# HIPOTESE 8

dados_bri <- dados %>%
  filter(!is.na(BRI))
Xc    <- dados_bri$BRI #Centering IV; hours of sleep
Zc    <- dados_bri$Year #Centering moderator; coffee consumption
Y     <- dados_bri$Quantity.in.Millions
#Moderation "By Hand"
library(gvlma)
fitMod <- lm(Y ~ Xc + Zc + Xc*Zc) #Model interacts IV & moderator
summary(fitMod)

coef(summary(fitMod))
gvlma(fitMod)

library(stargazer)
stargazer(fitMod,type="text", title = "Sleep and Coffee on Attention")


################################################################
###          GOING GLOBAL 1.0 - FROM 2005 TO 2012           ####
################################################################

# FILTER DATA
dados_gg1 <-
  dados %>%
    filter(Year %in% c(seq(2005, 2012, 1)))

## -------
## Year 
## -------

invest_year <-
  dados_gg1 %>%
  select(Year, Quantity.in.Millions) %>%
  group_by(Year)

invest_year

# Boxplot
ggbetweenstats(
  data = invest_year,
  x = Year,
  y = Quantity.in.Millions,
  xlab = "Year",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)

# Histogram 

gghistostats(
  data = invest_year,
  x = Year,
  test.value = 30
)

hist_2005 <- 
  invest_year %>%
  filter(Year == 2005) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2006 <- 
  invest_year %>%
  filter(Year == 2006) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2007 <- 
  invest_year %>%
  filter(Year == 2007) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2008 <- 
  invest_year %>%
  filter(Year == 2008) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2009 <- 
  invest_year %>%
  filter(Year == 2009) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2010 <- 
  invest_year %>%
  filter(Year == 2010) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2011 <- 
  invest_year %>%
  filter(Year == 2011) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2012 <- 
  invest_year %>%
  filter(Year == 2012) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )



plots <- list(hist_2005, hist_2006, hist_2007, hist_2008, hist_2009,
              hist_2010, hist_2011, hist_2012)

ggstatsplot::combine_plots(
  plotlist = plots,
  annotation.args = list(title = "Quantity of Investment per year - in Million"),
  plotgrid.args = list(nrow = 5)
)


# Cumulative investiment per year

invest_year_sum <-
  dados_gg1 %>%
  select(Year, Quantity.in.Millions) %>%
  group_by(Year) %>%
  mutate(Cumulative = sum(Quantity.in.Millions)) %>%
  select(Year, Cumulative) 

invest_year_sum <- as.data.frame(cbind(unique(invest_year_sum$Year), unique(invest_year_sum$Cumulative)))
colnames(invest_year_sum) <- c("Year", "Amount_Investment")


# plot
invest_year_sum %>% 
  ggplot(aes(x=Year, y=Amount_Investment, label=Year)) +
  geom_point() +
  geom_segment(aes(
    xend=c(tail(Year, n=-1), NA), 
    yend=c(tail(Amount_Investment, n=-1), NA)
  )
  ) +
  xlab("Year") +
  ylab("Amount Investment - in Million of US$")


# STRUCTURAL CHANGE

myts1 <- invest_year_sum$Amount_Investment

test2 <- Fstats(myts1~1) 
myts1.fs <- test2$Fstats 
bp.myts1 <- breakpoints(myts1~1) 
plot(myts1) 
lines(bp.myts1) 
bd.myts1 <- breakdates(bp.myts1) 
sctest(test2) 
ci.myts1 <- confint(bp.myts1) 
plot(myts1)
lines(ci.myts1) 

# Pie graph
invest_year <-
  invest_year %>%
  group_by(Year) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions)) %>%
  select(Year, Amount_investment) 

doughnut(unique(invest_year$Amount_investment), 
         labels = unique(invest_year$Year),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4)))

## ----------
## Investor 
## ----------

investor_data <-
  dados_gg1 %>%
  select(Quantity.in.Millions, Investor) %>%
  group_by(Investor) %>%
  filter(sd(Quantity.in.Millions) > 0)



ggplot(investor_data, aes(x = `Quantity.in.Millions`, y = `Investor`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )



## --------------------
## Transaction.Party 
## --------------------

trans_party_data <-
  dados_gg1 %>%
  select(Quantity.in.Millions, Transaction.Party) %>%
  group_by(Transaction.Party) %>%
  filter(sd(Quantity.in.Millions) > 0)


ggplot(trans_party_data, aes(x = `Quantity.in.Millions`, y = `Transaction.Party`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = trans_party_data,
  x = Transaction.Party,
  y = Quantity.in.Millions,
  xlab = "Transaction Party",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


sector_data <-
  dados_gg1 %>%
  select(Quantity.in.Millions, Transaction.Party) %>%
  group_by(Transaction.Party) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(sector_data$Amount_investment), 
         labels = unique(sector_data$Transaction.Party),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4)))



## ----------
## Sector 
## ----------

sector_data <-
  dados_gg1 %>%
  select(Quantity.in.Millions, Sector)


ggplot(sector_data, aes(x = `Quantity.in.Millions`, y = `Sector`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = sector_data,
  x = Sector,
  y = Quantity.in.Millions,
  xlab = "Sector",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)

dados_gg1 %>%
  filter(Sector == "Real estate") %>%
  group_by(Year) %>%
  mutate(Amount_invest = sum(Quantity.in.Millions)) %>%
  ggplot(
    aes(x=Year, y=Amount_invest, label=Year)) +
  geom_point() +
  geom_segment(aes(
    xend=c(tail(Year, n=-1), NA), 
    yend=c(tail(Amount_invest, n=-1), NA)
  )
  ) +
  xlab("Year") +
  ylab("Amount Investment - in Million of US$")


sector_data <-
  dados_gg1 %>%
  select(Quantity.in.Millions, Sector) %>%
  group_by(Sector) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(sector_data$Amount_investment), 
         labels = unique(sector_data$Sector),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4)))

## ----------
## Subsector 
## ----------
subsector_data <-
  dados_gg1 %>%
  select(Quantity.in.Millions, Subsector)


ggplot(subsector_data, aes(x = `Quantity.in.Millions`, y = `Subsector`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = subsector_data,
  x = Subsector,
  y = Quantity.in.Millions,
  xlab = "Subsector",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


subsector_data <-
  dados_gg1 %>%
  select(Quantity.in.Millions, Subsector) %>%
  group_by(Subsector) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(subsector_data$Amount_investment), 
         labels = unique(subsector_data$Subsector),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4)))

## ----------
## Region
## ----------


region_data <-
  dados_gg1 %>%
  select(Quantity.in.Millions, Region)

ggplot(region_data, aes(x = `Quantity.in.Millions`, y = `Region`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = region_data,
  x = Region,
  y = Quantity.in.Millions,
  xlab = "Region",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)



region_data <-
  dados_gg1 %>%
  select(Quantity.in.Millions, Region) %>%
  group_by(Region) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(region_data$Amount_investment), 
         labels = unique(region_data$Region),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))



## ----------
## Country 
## ----------


country_data <-
  dados_gg1 %>%
  select(Quantity.in.Millions, Country)

# Density
ggplot(country_data, aes(x = `Quantity.in.Millions`, y = `Country`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )



## ------
## BRI 
## ------

BRI_data <-
  dados_gg1 %>%
  select(Quantity.in.Millions, BRI) %>%
  filter(!is.na(BRI)) %>%
  mutate(bri = ifelse(BRI == 1, "BRI", "Others"))


ggplot(BRI_data, aes(x = `Quantity.in.Millions`, y = `bri`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = BRI_data,
  x = bri,
  y = Quantity.in.Millions,
  xlab = "bri",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


BRI_data <-
  BRI_data %>%
  select(Quantity.in.Millions, bri) %>%
  group_by(bri) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(BRI_data$Amount_investment), 
         labels = unique(BRI_data$bri),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))

## ------
## Greenfield 
## ------

Greenfield_data <-
  dados_gg1 %>%
  select(Quantity.in.Millions, Greenfield) %>%
  mutate(GF = ifelse(Greenfield == 'G', "Greenfield", "Other"))


ggplot(Greenfield_data, aes(x = `Quantity.in.Millions`, y = `GF`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = Greenfield_data,
  x = GF,
  y = Quantity.in.Millions,
  xlab = "Greenfield",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


Greenfield_data <-
  Greenfield_data %>%
  select(Quantity.in.Millions, GF) %>%
  group_by(GF) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(Greenfield_data$Amount_investment), 
         labels = unique(Greenfield_data$GF),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))



## -------------
## Share.Size 
## -------------

Share.Size_data <-
  dados_gg1 %>%
  select(Quantity.in.Millions, Share.Size)


ggplot(Share.Size_data, aes(x = `Quantity.in.Millions`, y = `Share.Size`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = Share.Size_data,
  x = Share.Size,
  y = Quantity.in.Millions,
  xlab = "Share.Size",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


Share.Size_data <-
  Share.Size_data %>%
  select(Quantity.in.Millions, Share.Size) %>%
  group_by(Share.Size) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(Share.Size_data$Amount_investment), 
         labels = unique(Share.Size_data$Share.Size),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))


## --------
## Month
## --------
month_data <-
  dados_gg1 %>%
  select(Month, Quantity.in.Millions) %>%
  group_by(Month)


# Density
ggplot(month_data, aes(x = `Quantity.in.Millions`, y = `Month`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = month_data,
  x = Month,
  y = Quantity.in.Millions,
  xlab = "Month",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


month_data <-
  month_data %>%
  select(Quantity.in.Millions, Month) %>%
  group_by(Month) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(month_data$Amount_investment), 
         labels = unique(month_data$Month),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))



################################################################
###          GOING GLOBAL 2.0 - FROM 2013 TO 2016           ####
################################################################

# FILTER DATA
dados_gg2 <-
  dados %>%
  filter(Year %in% c(seq(2013, 2016, 1)))

## -------
## Year 
## -------

invest_year <-
  dados_gg2 %>%
  select(Year, Quantity.in.Millions) %>%
  group_by(Year)

invest_year

# Boxplot
ggbetweenstats(
  data = invest_year,
  x = Year,
  y = Quantity.in.Millions,
  xlab = "Year",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)

# Histogram 

gghistostats(
  data = invest_year,
  x = Year,
  test.value = 30
)

hist_2013 <- 
  invest_year %>%
  filter(Year == 2013) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2014 <- 
  invest_year %>%
  filter(Year == 2014) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2015 <- 
  invest_year %>%
  filter(Year == 2015) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2016 <- 
  invest_year %>%
  filter(Year == 2016) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )




plots <- list(hist_2013, hist_2014, 
              hist_2015, hist_2016)

ggstatsplot::combine_plots(
  plotlist = plots,
  annotation.args = list(title = "Quantity of Investment per year - in Million"),
  plotgrid.args = list(nrow = 2)
)


# Cumulative investiment per year

invest_year_sum <-
  dados_gg2 %>%
  select(Year, Quantity.in.Millions) %>%
  group_by(Year) %>%
  mutate(Cumulative = sum(Quantity.in.Millions)) %>%
  select(Year, Cumulative) 

invest_year_sum <- as.data.frame(cbind(unique(invest_year_sum$Year), unique(invest_year_sum$Cumulative)))
colnames(invest_year_sum) <- c("Year", "Amount_Investment")


# plot
invest_year_sum %>% 
  ggplot(aes(x=Year, y=Amount_Investment, label=Year)) +
  geom_point() +
  geom_segment(aes(
    xend=c(tail(Year, n=-1), NA), 
    yend=c(tail(Amount_Investment, n=-1), NA)
  )
  ) +
  xlab("Year") +
  ylab("Amount Investment - in Million of US$")


# STRUCTURAL CHANGE

myts1 <- invest_year_sum$Amount_Investment

test2 <- Fstats(myts1~1) 
myts1.fs <- test2$Fstats 
bp.myts1 <- breakpoints(myts1~1) 
plot(myts1) 
lines(bp.myts1) 
bd.myts1 <- breakdates(bp.myts1) 
sctest(test2) 
ci.myts1 <- confint(bp.myts1) 
plot(myts1)
lines(ci.myts1) 

# Pie graph
invest_year <-
  invest_year %>%
  group_by(Year) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions)) %>%
  select(Year, Amount_investment) 

doughnut(unique(invest_year$Amount_investment), 
         labels = unique(invest_year$Year),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4)))

## ----------
## Investor 
## ----------

investor_data <-
  dados_gg2 %>%
  select(Quantity.in.Millions, Investor) %>%
  group_by(Investor) %>%
  filter(sd(Quantity.in.Millions) > 0)



ggplot(investor_data, aes(x = `Quantity.in.Millions`, y = `Investor`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )



## --------------------
## Transaction.Party 
## --------------------

trans_party_data <-
  dados_gg2 %>%
  select(Quantity.in.Millions, Transaction.Party) %>%
  group_by(Transaction.Party) %>%
  filter(sd(Quantity.in.Millions) > 0)


ggplot(trans_party_data, aes(x = `Quantity.in.Millions`, y = `Transaction.Party`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = trans_party_data,
  x = Transaction.Party,
  y = Quantity.in.Millions,
  xlab = "Transaction Party",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


sector_data <-
  dados_gg2 %>%
  select(Quantity.in.Millions, Transaction.Party) %>%
  group_by(Transaction.Party) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(sector_data$Amount_investment), 
         labels = unique(sector_data$Transaction.Party),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4)))



## ----------
## Sector 
## ----------

sector_data <-
  dados_gg2 %>%
  select(Quantity.in.Millions, Sector)


ggplot(sector_data, aes(x = `Quantity.in.Millions`, y = `Sector`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = sector_data,
  x = Sector,
  y = Quantity.in.Millions,
  xlab = "Sector",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)

dados_gg2 %>%
  filter(Sector == "Real estate") %>%
  group_by(Year) %>%
  mutate(Amount_invest = sum(Quantity.in.Millions)) %>%
  ggplot(
    aes(x=Year, y=Amount_invest, label=Year)) +
  geom_point() +
  geom_segment(aes(
    xend=c(tail(Year, n=-1), NA), 
    yend=c(tail(Amount_invest, n=-1), NA)
  )
  ) +
  xlab("Year") +
  ylab("Amount Investment - in Million of US$")


sector_data <-
  dados_gg2 %>%
  select(Quantity.in.Millions, Sector) %>%
  group_by(Sector) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(sector_data$Amount_investment), 
         labels = unique(sector_data$Sector),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4)))

## ----------
## Subsector 
## ----------
subsector_data <-
  dados_gg2 %>%
  select(Quantity.in.Millions, Subsector)


ggplot(subsector_data, aes(x = `Quantity.in.Millions`, y = `Subsector`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = subsector_data,
  x = Subsector,
  y = Quantity.in.Millions,
  xlab = "Subsector",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


subsector_data <-
  dados_gg2 %>%
  select(Quantity.in.Millions, Subsector) %>%
  group_by(Subsector) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(subsector_data$Amount_investment), 
         labels = unique(subsector_data$Subsector),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4)))

## ----------
## Region
## ----------


region_data <-
  dados_gg2 %>%
  select(Quantity.in.Millions, Region)

ggplot(region_data, aes(x = `Quantity.in.Millions`, y = `Region`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = region_data,
  x = Region,
  y = Quantity.in.Millions,
  xlab = "Region",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)



region_data <-
  dados_gg2 %>%
  select(Quantity.in.Millions, Region) %>%
  group_by(Region) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(region_data$Amount_investment), 
         labels = unique(region_data$Region),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))



## ----------
## Country 
## ----------


country_data <-
  dados_gg2 %>%
  select(Quantity.in.Millions, Country)

# Density
ggplot(country_data, aes(x = `Quantity.in.Millions`, y = `Country`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )



## ------
## BRI 
## ------

BRI_data <-
  dados_gg2 %>%
  select(Quantity.in.Millions, BRI) %>%
  filter(!is.na(BRI)) %>%
  mutate(bri = ifelse(BRI == 1, "BRI", "Others"))


ggplot(BRI_data, aes(x = `Quantity.in.Millions`, y = `bri`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = BRI_data,
  x = bri,
  y = Quantity.in.Millions,
  xlab = "bri",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


BRI_data <-
  BRI_data %>%
  select(Quantity.in.Millions, bri) %>%
  group_by(bri) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(BRI_data$Amount_investment), 
         labels = unique(BRI_data$bri),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))

## ------
## Greenfield 
## ------

Greenfield_data <-
  dados_gg2 %>%
  select(Quantity.in.Millions, Greenfield) %>%
  mutate(GF = ifelse(Greenfield == 'G', "Greenfield", "Other"))


ggplot(Greenfield_data, aes(x = `Quantity.in.Millions`, y = `GF`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = Greenfield_data,
  x = GF,
  y = Quantity.in.Millions,
  xlab = "Greenfield",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


Greenfield_data <-
  Greenfield_data %>%
  select(Quantity.in.Millions, GF) %>%
  group_by(GF) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(Greenfield_data$Amount_investment), 
         labels = unique(Greenfield_data$GF),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))



## -------------
## Share.Size 
## -------------

Share.Size_data <-
  dados_gg2 %>%
  select(Quantity.in.Millions, Share.Size)


ggplot(Share.Size_data, aes(x = `Quantity.in.Millions`, y = `Share.Size`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = Share.Size_data,
  x = Share.Size,
  y = Quantity.in.Millions,
  xlab = "Share.Size",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


Share.Size_data <-
  Share.Size_data %>%
  select(Quantity.in.Millions, Share.Size) %>%
  group_by(Share.Size) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(Share.Size_data$Amount_investment), 
         labels = unique(Share.Size_data$Share.Size),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))


## --------
## Month
## --------
month_data <-
  dados_gg2 %>%
  select(Month, Quantity.in.Millions) %>%
  group_by(Month)


# Density
ggplot(month_data, aes(x = `Quantity.in.Millions`, y = `Month`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = month_data,
  x = Month,
  y = Quantity.in.Millions,
  xlab = "Month",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


month_data <-
  month_data %>%
  select(Quantity.in.Millions, Month) %>%
  group_by(Month) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(month_data$Amount_investment), 
         labels = unique(month_data$Month),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))



################################################################
###          GOING GLOBAL 3.0 - FROM 2017 TO 2020           ####
################################################################

# FILTER DATA
dados_gg3 <-
  dados %>%
  filter(Year %in% c(seq(2017, 2020, 1)))

## -------
## Year 
## -------

invest_year <-
  dados_gg3 %>%
  select(Year, Quantity.in.Millions) %>%
  group_by(Year)

invest_year

# Boxplot
ggbetweenstats(
  data = invest_year,
  x = Year,
  y = Quantity.in.Millions,
  xlab = "Year",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)

# Histogram 

gghistostats(
  data = invest_year,
  x = Year,
  test.value = 30
)

hist_2017 <- 
  invest_year %>%
  filter(Year == 2017) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2018 <- 
  invest_year %>%
  filter(Year == 2018) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2019 <- 
  invest_year %>%
  filter(Year == 2019) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2020 <- 
  invest_year %>%
  filter(Year == 2020) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )




plots <- list(hist_2017, hist_2018, 
              hist_2019, hist_2020)

ggstatsplot::combine_plots(
  plotlist = plots,
  annotation.args = list(title = "Quantity of Investment per year - in Million"),
  plotgrid.args = list(nrow = 2)
)


# Cumulative investiment per year

invest_year_sum <-
  dados_gg3 %>%
  select(Year, Quantity.in.Millions) %>%
  group_by(Year) %>%
  mutate(Cumulative = sum(Quantity.in.Millions)) %>%
  select(Year, Cumulative) 

invest_year_sum <- as.data.frame(cbind(unique(invest_year_sum$Year), unique(invest_year_sum$Cumulative)))
colnames(invest_year_sum) <- c("Year", "Amount_Investment")


# plot
invest_year_sum %>% 
  ggplot(aes(x=Year, y=Amount_Investment, label=Year)) +
  geom_point() +
  geom_segment(aes(
    xend=c(tail(Year, n=-1), NA), 
    yend=c(tail(Amount_Investment, n=-1), NA)
  )
  ) +
  xlab("Year") +
  ylab("Amount Investment - in Million of US$")


# STRUCTURAL CHANGE

myts1 <- invest_year_sum$Amount_Investment

test2 <- Fstats(myts1~1) 
myts1.fs <- test2$Fstats 
bp.myts1 <- breakpoints(myts1~1) 
plot(myts1) 
lines(bp.myts1) 
bd.myts1 <- breakdates(bp.myts1) 
sctest(test2) 
ci.myts1 <- confint(bp.myts1) 
plot(myts1)
lines(ci.myts1) 

# Pie graph
invest_year <-
  invest_year %>%
  group_by(Year) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions)) %>%
  select(Year, Amount_investment) 

doughnut(unique(invest_year$Amount_investment), 
         labels = unique(invest_year$Year),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4)))

## ----------
## Investor 
## ----------

investor_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Investor) %>%
  group_by(Investor) %>%
  filter(sd(Quantity.in.Millions) > 0)



ggplot(investor_data, aes(x = `Quantity.in.Millions`, y = `Investor`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )



## --------------------
## Transaction.Party 
## --------------------

trans_party_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Transaction.Party) %>%
  group_by(Transaction.Party) %>%
  filter(sd(Quantity.in.Millions) > 0)


ggplot(trans_party_data, aes(x = `Quantity.in.Millions`, y = `Transaction.Party`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = trans_party_data,
  x = Transaction.Party,
  y = Quantity.in.Millions,
  xlab = "Transaction Party",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


sector_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Transaction.Party) %>%
  group_by(Transaction.Party) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(sector_data$Amount_investment), 
         labels = unique(sector_data$Transaction.Party),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4)))



## ----------
## Sector 
## ----------

sector_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Sector)


ggplot(sector_data, aes(x = `Quantity.in.Millions`, y = `Sector`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = sector_data,
  x = Sector,
  y = Quantity.in.Millions,
  xlab = "Sector",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)

dados_gg3 %>%
  filter(Sector == "Real estate") %>%
  group_by(Year) %>%
  mutate(Amount_invest = sum(Quantity.in.Millions)) %>%
  ggplot(
    aes(x=Year, y=Amount_invest, label=Year)) +
  geom_point() +
  geom_segment(aes(
    xend=c(tail(Year, n=-1), NA), 
    yend=c(tail(Amount_invest, n=-1), NA)
  )
  ) +
  xlab("Year") +
  ylab("Amount Investment - in Million of US$")


sector_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Sector) %>%
  group_by(Sector) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(sector_data$Amount_investment), 
         labels = unique(sector_data$Sector),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4)))

## ----------
## Subsector 
## ----------
subsector_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Subsector)


ggplot(subsector_data, aes(x = `Quantity.in.Millions`, y = `Subsector`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = subsector_data,
  x = Subsector,
  y = Quantity.in.Millions,
  xlab = "Subsector",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


subsector_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Subsector) %>%
  group_by(Subsector) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(subsector_data$Amount_investment), 
         labels = unique(subsector_data$Subsector),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4)))

## ----------
## Region
## ----------


region_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Region)

ggplot(region_data, aes(x = `Quantity.in.Millions`, y = `Region`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = region_data,
  x = Region,
  y = Quantity.in.Millions,
  xlab = "Region",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)



region_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Region) %>%
  group_by(Region) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(region_data$Amount_investment), 
         labels = unique(region_data$Region),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))



## ----------
## Country 
## ----------


country_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Country)

# Density
ggplot(country_data, aes(x = `Quantity.in.Millions`, y = `Country`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )



## ------
## BRI 
## ------

BRI_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, BRI) %>%
  filter(!is.na(BRI)) %>%
  mutate(bri = ifelse(BRI == 1, "BRI", "Others"))


ggplot(BRI_data, aes(x = `Quantity.in.Millions`, y = `bri`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = BRI_data,
  x = bri,
  y = Quantity.in.Millions,
  xlab = "bri",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


BRI_data <-
  BRI_data %>%
  select(Quantity.in.Millions, bri) %>%
  group_by(bri) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(BRI_data$Amount_investment), 
         labels = unique(BRI_data$bri),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))

## ------
## Greenfield 
## ------

Greenfield_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Greenfield) %>%
  mutate(GF = ifelse(Greenfield == 'G', "Greenfield", "Other"))


ggplot(Greenfield_data, aes(x = `Quantity.in.Millions`, y = `GF`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = Greenfield_data,
  x = GF,
  y = Quantity.in.Millions,
  xlab = "Greenfield",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


Greenfield_data <-
  Greenfield_data %>%
  select(Quantity.in.Millions, GF) %>%
  group_by(GF) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(Greenfield_data$Amount_investment), 
         labels = unique(Greenfield_data$GF),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))



## -------------
## Share.Size 
## -------------

Share.Size_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Share.Size)


ggplot(Share.Size_data, aes(x = `Quantity.in.Millions`, y = `Share.Size`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = Share.Size_data,
  x = Share.Size,
  y = Quantity.in.Millions,
  xlab = "Share.Size",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


Share.Size_data <-
  Share.Size_data %>%
  select(Quantity.in.Millions, Share.Size) %>%
  group_by(Share.Size) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(Share.Size_data$Amount_investment), 
         labels = unique(Share.Size_data$Share.Size),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))


## --------
## Month
## --------
month_data <-
  dados_gg3 %>%
  select(Month, Quantity.in.Millions) %>%
  group_by(Month)


# Density
ggplot(month_data, aes(x = `Quantity.in.Millions`, y = `Month`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = month_data,
  x = Month,
  y = Quantity.in.Millions,
  xlab = "Month",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


month_data <-
  month_data %>%
  select(Quantity.in.Millions, Month) %>%
  group_by(Month) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(month_data$Amount_investment), 
         labels = unique(month_data$Month),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))




#############################################################
###          THE YEAR OF CHAGE STRATEGY - 2016           ####
#############################################################

# FILTER DATA
dados_gg3 <-
  dados %>%
  filter(Year %in% c(seq(2017, 2020, 1)))

## -------
## Year 
## -------

invest_year <-
  dados_gg3 %>%
  select(Year, Quantity.in.Millions) %>%
  group_by(Year)

invest_year

# Boxplot
ggbetweenstats(
  data = invest_year,
  x = Year,
  y = Quantity.in.Millions,
  xlab = "Year",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)

# Histogram 

gghistostats(
  data = invest_year,
  x = Year,
  test.value = 30
)

hist_2017 <- 
  invest_year %>%
  filter(Year == 2017) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2018 <- 
  invest_year %>%
  filter(Year == 2018) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2019 <- 
  invest_year %>%
  filter(Year == 2019) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )

hist_2020 <- 
  invest_year %>%
  filter(Year == 2020) %>%
  gghistostats(
    x = Quantity.in.Millions,
    xlab = "Quantity in Millions",
    test.value = 30
  )




plots <- list(hist_2017, hist_2018, 
              hist_2019, hist_2020)

ggstatsplot::combine_plots(
  plotlist = plots,
  annotation.args = list(title = "Quantity of Investment per year - in Million"),
  plotgrid.args = list(nrow = 2)
)


# Cumulative investiment per year

invest_year_sum <-
  dados_gg3 %>%
  select(Year, Quantity.in.Millions) %>%
  group_by(Year) %>%
  mutate(Cumulative = sum(Quantity.in.Millions)) %>%
  select(Year, Cumulative) 

invest_year_sum <- as.data.frame(cbind(unique(invest_year_sum$Year), unique(invest_year_sum$Cumulative)))
colnames(invest_year_sum) <- c("Year", "Amount_Investment")


# plot
invest_year_sum %>% 
  ggplot(aes(x=Year, y=Amount_Investment, label=Year)) +
  geom_point() +
  geom_segment(aes(
    xend=c(tail(Year, n=-1), NA), 
    yend=c(tail(Amount_Investment, n=-1), NA)
  )
  ) +
  xlab("Year") +
  ylab("Amount Investment - in Million of US$")


# STRUCTURAL CHANGE

myts1 <- invest_year_sum$Amount_Investment

test2 <- Fstats(myts1~1) 
myts1.fs <- test2$Fstats 
bp.myts1 <- breakpoints(myts1~1) 
plot(myts1) 
lines(bp.myts1) 
bd.myts1 <- breakdates(bp.myts1) 
sctest(test2) 
ci.myts1 <- confint(bp.myts1) 
plot(myts1)
lines(ci.myts1) 

# Pie graph
invest_year <-
  invest_year %>%
  group_by(Year) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions)) %>%
  select(Year, Amount_investment) 

doughnut(unique(invest_year$Amount_investment), 
         labels = unique(invest_year$Year),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4)))

## ----------
## Investor 
## ----------

investor_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Investor) %>%
  group_by(Investor) %>%
  filter(sd(Quantity.in.Millions) > 0)



ggplot(investor_data, aes(x = `Quantity.in.Millions`, y = `Investor`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )



## --------------------
## Transaction.Party 
## --------------------

trans_party_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Transaction.Party) %>%
  group_by(Transaction.Party) %>%
  filter(sd(Quantity.in.Millions) > 0)


ggplot(trans_party_data, aes(x = `Quantity.in.Millions`, y = `Transaction.Party`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = trans_party_data,
  x = Transaction.Party,
  y = Quantity.in.Millions,
  xlab = "Transaction Party",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


sector_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Transaction.Party) %>%
  group_by(Transaction.Party) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(sector_data$Amount_investment), 
         labels = unique(sector_data$Transaction.Party),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4)))



## ----------
## Sector 
## ----------

sector_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Sector)


ggplot(sector_data, aes(x = `Quantity.in.Millions`, y = `Sector`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = sector_data,
  x = Sector,
  y = Quantity.in.Millions,
  xlab = "Sector",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)

dados_gg3 %>%
  filter(Sector == "Real estate") %>%
  group_by(Year) %>%
  mutate(Amount_invest = sum(Quantity.in.Millions)) %>%
  ggplot(
    aes(x=Year, y=Amount_invest, label=Year)) +
  geom_point() +
  geom_segment(aes(
    xend=c(tail(Year, n=-1), NA), 
    yend=c(tail(Amount_invest, n=-1), NA)
  )
  ) +
  xlab("Year") +
  ylab("Amount Investment - in Million of US$")


sector_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Sector) %>%
  group_by(Sector) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(sector_data$Amount_investment), 
         labels = unique(sector_data$Sector),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4)))

## ----------
## Subsector 
## ----------
subsector_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Subsector)


ggplot(subsector_data, aes(x = `Quantity.in.Millions`, y = `Subsector`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = subsector_data,
  x = Subsector,
  y = Quantity.in.Millions,
  xlab = "Subsector",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


subsector_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Subsector) %>%
  group_by(Subsector) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(subsector_data$Amount_investment), 
         labels = unique(subsector_data$Subsector),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4)))

## ----------
## Region
## ----------


region_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Region)

ggplot(region_data, aes(x = `Quantity.in.Millions`, y = `Region`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = region_data,
  x = Region,
  y = Quantity.in.Millions,
  xlab = "Region",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)



region_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Region) %>%
  group_by(Region) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(region_data$Amount_investment), 
         labels = unique(region_data$Region),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))



## ----------
## Country 
## ----------


country_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Country)

# Density
ggplot(country_data, aes(x = `Quantity.in.Millions`, y = `Country`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )



## ------
## BRI 
## ------

BRI_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, BRI) %>%
  filter(!is.na(BRI)) %>%
  mutate(bri = ifelse(BRI == 1, "BRI", "Others"))


ggplot(BRI_data, aes(x = `Quantity.in.Millions`, y = `bri`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = BRI_data,
  x = bri,
  y = Quantity.in.Millions,
  xlab = "bri",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


BRI_data <-
  BRI_data %>%
  select(Quantity.in.Millions, bri) %>%
  group_by(bri) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(BRI_data$Amount_investment), 
         labels = unique(BRI_data$bri),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))

## ------
## Greenfield 
## ------

Greenfield_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Greenfield) %>%
  mutate(GF = ifelse(Greenfield == 'G', "Greenfield", "Other"))


ggplot(Greenfield_data, aes(x = `Quantity.in.Millions`, y = `GF`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = Greenfield_data,
  x = GF,
  y = Quantity.in.Millions,
  xlab = "Greenfield",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


Greenfield_data <-
  Greenfield_data %>%
  select(Quantity.in.Millions, GF) %>%
  group_by(GF) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(Greenfield_data$Amount_investment), 
         labels = unique(Greenfield_data$GF),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))



## -------------
## Share.Size 
## -------------

Share.Size_data <-
  dados_gg3 %>%
  select(Quantity.in.Millions, Share.Size)


ggplot(Share.Size_data, aes(x = `Quantity.in.Millions`, y = `Share.Size`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = Share.Size_data,
  x = Share.Size,
  y = Quantity.in.Millions,
  xlab = "Share.Size",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


Share.Size_data <-
  Share.Size_data %>%
  select(Quantity.in.Millions, Share.Size) %>%
  group_by(Share.Size) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(Share.Size_data$Amount_investment), 
         labels = unique(Share.Size_data$Share.Size),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))


## --------
## Month
## --------
month_data <-
  dados_gg3 %>%
  select(Month, Quantity.in.Millions) %>%
  group_by(Month)


# Density
ggplot(month_data, aes(x = `Quantity.in.Millions`, y = `Month`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Amount of Investment - in Millions') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

# Boxplot
ggbetweenstats(
  data = month_data,
  x = Month,
  y = Quantity.in.Millions,
  xlab = "Month",
  ylab = "Quantity in Millions",
  type = "p", 
  pairwise.display = "s",
  ggtheme = ggthemes::theme_tufte(),
  package = "ggsci",
  palette = "default_jco",
  outlier.tagging = TRUE,
)


month_data <-
  month_data %>%
  select(Quantity.in.Millions, Month) %>%
  group_by(Month) %>%
  mutate(Amount_investment = sum(Quantity.in.Millions))

doughnut(unique(month_data$Amount_investment), 
         labels = unique(month_data$Month),
         inner.radius=0.5, col=c(rgb(0.2,0.2,0.4,0.5), rgb(0.8,0.2,0.4,0.5), 
                                 rgb(0.2,0.9,0.4,0.4), rgb(0.0,0.9,0.8,0.4),
                                 rgb(0.2,0.8,0.4,0.9)))


################################################
## Modelos Mediator-Moderators - TARGET 'POE' ## 
## Other variable and many MO/ME as feature   ##
################################################


## MODERATOR: Transaction.Party ==================================

names(dados)
GoingGlobal1.0 <-
  dados %>%
    select(Year) %>%
    filter(Year %in% seq(2005, 2011, 1)) %>%
    mutate(GoingGlobal = "GoingGlobal1.0")

GoingGlobal2.0 <-
  dados %>%
  select(Year) %>%
  filter(Year %in% seq(2012, 2016, 1)) %>%
  mutate(GoingGlobal = "GoingGlobal2.0")

GoingGlobal3.0 <-
  dados %>%
  select(Year) %>%
  filter(Year %in% seq(2017, 2020, 1)) %>%
  mutate(GoingGlobal = "GoingGlobal3.0")


GoingGlobal <- rbind(GoingGlobal1.0, GoingGlobal2.0, GoingGlobal3.0)

dadosGG = dados

attach(dados)

dadosGG$Year = Year
dadosGG$Transaction.Party = as.numeric(as.factor(Transaction.Party))
dadosGG$Region = as.numeric(as.factor(Region))
dadosGG$Sector = as.numeric(as.factor(Sector))
dadosGG$Share.Size = as.numeric(as.factor(Share.Size))
dadosGG$Greenfield = as.numeric(Greenfield == "G")
dadosGG$GoingGlobal <- GoingGlobal[,2]

dim(dados)
dim(GoingGlobal)

head(dadosGG)
dim(dadosGG)
names(dadosGG)

## HIPOTESE 1 ========================================
## GOING GLOBAL - Transaction.Party
mod_med1 <- lm(Transaction.Party ~ GoingGlobal, 
               data = dadosGG)

summary(mod_med1)

mod_out1 <- lm(Quantity.in.Millions ~ Transaction.Party + GoingGlobal, 
               data = dadosGG) 

summary(mod_out1)


gvlma(mod_med1) 
gvlma(mod_out1) 

modelo_mediator1 <- 
  mediation::mediate(mod_med1, mod_out1, 
                     treat = c("GoingGlobal"), 
                     mediator = "Transaction.Party")



summary(modelo_mediator1) 

figura_modelo_mediator1 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator1,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 1 - Mediator variable: Transaction Party",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator1)


# Bootstrap

modelo_mediator_Boot1 <- 
  mediation::mediate(mod_med1, mod_out1, 
                     boot = TRUE, sims = 999, 
                     treat = c("GoingGlobal"), 
                     mediator = "Transaction.Party")

summary(modelo_mediator_Boot1)

figura_modelo_mediator_Boot1 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot1,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 1 - Mediator variable: Transaction Party. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot1)



## Hipotese 1.4

## BRI - Year
mod_med14 <- lm(Year ~ GoingGlobal, 
                data = dadosGG)
summary(mod_med82)

mod_out14 <- lm(Quantity.in.Millions ~ Year + GoingGlobal, 
                data = dadosGG) 
summary(mod_out14)

gvlma(mod_med14) 
gvlma(mod_out14) 

modelo_mediator14 <- 
  mediation::mediate(mod_med14, mod_out14, 
                     treat = c("GoingGlobal"), 
                     mediator = "Year")



summary(modelo_mediator14) 

figura_modelo_mediator14 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator14,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 1 - Mediator variable: Year",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator14)


# Bootstrap

modelo_mediator_Boot14 <- 
  mediation::mediate(mod_med14, mod_out14, 
                     boot = TRUE, sims = 999, 
                     treat = c("GoingGlobal"), 
                     mediator = "Year")

summary(modelo_mediator_Boot14)

figura_modelo_mediator_Boot14 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot14,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 1 - Mediator variable: Year. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot14)




## Hipotese 1.5

## BRI - Transaction.Party
mod_med15 <- lm(Transaction.Party ~ BRI, 
                data = dadosGG)
summary(mod_med15)

mod_out15 <- lm(Quantity.in.Millions ~ Transaction.Party + BRI, 
                data = dadosGG) 
summary(mod_out15)

gvlma(mod_med15) 
gvlma(mod_out15) 

modelo_mediator15 <- 
  mediation::mediate(mod_med15, mod_out15, 
                     treat = c("BRI"), 
                     mediator = "Transaction.Party")



summary(modelo_mediator71) 

figura_modelo_mediator71 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator71,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 71 - Mediator variable: Transaction Party",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator71)


# Bootstrap

modelo_mediator_Boot71 <- 
  mediation::mediate(mod_med71, mod_out71, 
                     boot = TRUE, sims = 999, 
                     treat = c("BRI"), 
                     mediator = "Transaction.Party")

summary(modelo_mediator_Boot71)

figura_modelo_mediator_Boot71 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot71,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 71 - Mediator variable: Transaction Party. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot71)

## HIPOTESE 2 ========================================

## GOING GLOBAL - Region
mod_med2 <- lm(Region ~ GoingGlobal, 
               data = dadosGG)

summary(mod_med2)

mod_out2 <- lm(Quantity.in.Millions ~ Region + GoingGlobal, 
               data = dadosGG) 

summary(mod_out2)
gvlma(mod_med2) 
gvlma(mod_out2) 

modelo_mediator2 <- 
  mediation::mediate(mod_med2, mod_out2, 
                     treat = c("GoingGlobal"), 
                     mediator = "Region")



summary(modelo_mediator2) 

figura_modelo_mediator2 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator2,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 1 - Mediator variable: Region",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator2)


# Bootstrap

modelo_mediator_Boot2 <- 
  mediation::mediate(mod_med2, mod_out2, 
                     boot = TRUE, sims = 999, 
                     treat = c("GoingGlobal"), 
                     mediator = "Region")

summary(modelo_mediator_Boot2)

figura_modelo_mediator_Boot2 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot2,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 2 - Mediator variable: Region. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot2)




## HIPOTESE 3 =========================


## GOING GLOBAL - Sector
mod_med3 <- lm(Sector ~ GoingGlobal, 
               data = dadosGG)
summary(mod_med3)

mod_out3 <- lm(Quantity.in.Millions ~ Sector + GoingGlobal, 
               data = dadosGG) 

summary(mod_out3)

gvlma(mod_med3) 
gvlma(mod_out3) 

modelo_mediator3 <- 
  mediation::mediate(mod_med3, mod_out3, 
                     treat = c("GoingGlobal"), 
                     mediator = "Sector")



summary(modelo_mediator3) 

figura_modelo_mediator3 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator3,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 3 - Mediator variable: Sector",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator3)


# Bootstrap

modelo_mediator_Boot3 <- 
  mediation::mediate(mod_med3, mod_out3, 
                     boot = TRUE, sims = 999, 
                     treat = c("GoingGlobal"), 
                     mediator = "Sector")

summary(modelo_mediator_Boot3)

figura_modelo_mediator_Boot3 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot3,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 3 - Mediator variable: Sector. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot3)



## HIPOTESE 4 =========================


## GOING GLOBAL - Share.Size
mod_med4 <- lm(Share.Size ~ GoingGlobal, 
               data = dadosGG)
summary(mod_med4)

mod_out4 <- lm(Quantity.in.Millions ~ Share.Size + GoingGlobal, 
               data = dadosGG) 
summary(mod_med4)


gvlma(mod_med4) 
gvlma(mod_out4) 

modelo_mediator4 <- 
  mediation::mediate(mod_med4, mod_out4, 
                     treat = c("GoingGlobal"), 
                     mediator = "Share.Size")



summary(modelo_mediator4) 

figura_modelo_mediator4 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator4,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 4 - Mediator variable: Share.Size",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator4)


# Bootstrap

modelo_mediator_Boot4 <- 
  mediation::mediate(mod_med4, mod_out4, 
                     boot = TRUE, sims = 999, 
                     treat = c("GoingGlobal"), 
                     mediator = "Share.Size")

summary(modelo_mediator_Boot4)

figura_modelo_mediator_Boot4 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot4,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 4 - Mediator variable: Share.Size. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot4)



## HIPOTESE 5 =========================


## GOING GLOBAL - Greenfield
mod_med5 <- lm(Greenfield ~ GoingGlobal, 
               data = dadosGG)

summary(mod_med5)

mod_out5 <- lm(Quantity.in.Millions ~ Greenfield + GoingGlobal, 
               data = dadosGG) 
summary(mod_out5)

gvlma(mod_med5) 
gvlma(mod_out5) 


summary(mod_med5)
summary(mod_out5)

modelo_mediator5 <- 
  mediation::mediate(mod_med1, mod_out1, 
                     treat = c("GoingGlobal"), 
                     mediator = "Greenfield")

#head(dadosGG)

summary(modelo_mediator5) 

figura_modelo_mediator5 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator1,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 5 - Mediator variable: Greenfield",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator5)


# Bootstrap

modelo_mediator_Boot5 <- 
  mediation::mediate(mod_med5, mod_out5, 
                     boot = TRUE, sims = 999, 
                     treat = c("GoingGlobal"), 
                     mediator = "Greenfield")

summary(modelo_mediator_Boot1)

figura_modelo_mediator_Boot5 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot5,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 5 - Mediator variable: Greenfield. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot5)



## HIPOTESE 6 =========================


## Hipotese 6.1 ----------------------------------------------

## GOING GLOBAL - Transaction.Party
mod_med61 <- lm(Transaction.Party ~ GoingGlobal, 
               data = dadosGG)

summary(mod_med61)

mod_out61 <- lm(Quantity.in.Millions ~ Transaction.Party + GoingGlobal, 
               data = dadosGG) 

summary(mod_out61)

gvlma(mod_med61) 
gvlma(mod_out61) 

modelo_mediator61 <- 
  mediation::mediate(mod_med61, mod_out61, 
                     treat = c("GoingGlobal"), 
                     mediator = "Transaction.Party")



summary(modelo_mediator61) 

figura_modelo_mediator61 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator61,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 6.1 - Mediator variable: Transaction Party",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator61)


# Bootstrap

modelo_mediator_Boot61 <- 
  mediation::mediate(mod_med61, mod_out61, 
                     boot = TRUE, sims = 999, 
                     treat = c("GoingGlobal"), 
                     mediator = "Transaction.Party")

summary(modelo_mediator_Boot61)

figura_modelo_mediator_Boot61 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot61,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 6.1 - Mediator variable: Transaction Party. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot61)



## Hipotese 6.2 ----------------------------------------------

## GOING GLOBAL - Transaction.Party
mod_med62 <- lm(Transaction.Party ~ GoingGlobal, 
               data = dadosGG)

mod_out62 <- lm(Quantity.in.Millions ~ Transaction.Party + GoingGlobal, 
               data = dadosGG) 

gvlma(mod_med62) 
gvlma(mod_out62) 

modelo_mediator62 <- 
  mediation::mediate(mod_med62, mod_out62, 
                     treat = c("GoingGlobal"), 
                     mediator = "Transaction.Party")



summary(modelo_mediator62) 

figura_modelo_mediator62 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator62,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 62 - Mediator variable: Transaction Party",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator62)


# Bootstrap

modelo_mediator_Boot62 <- 
  mediation::mediate(mod_med62, mod_out62, 
                     boot = TRUE, sims = 999, 
                     treat = c("GoingGlobal"), 
                     mediator = "Transaction.Party")

summary(modelo_mediator_Boot62)

figura_modelo_mediator_Boot62 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot62,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 62 - Mediator variable: Transaction Party. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot62)



## HIPOTESE 7 =========================


## Hipotese 7.1

## BRI - Transaction.Party
mod_med71 <- lm(Transaction.Party ~ BRI, 
               data = dadosGG)
summary(mod_med71)

mod_out71 <- lm(Quantity.in.Millions ~ Transaction.Party + BRI, 
               data = dadosGG) 
summary(mod_out71)

gvlma(mod_med71) 
gvlma(mod_out71) 

modelo_mediator71 <- 
  mediation::mediate(mod_med71, mod_out71, 
                     treat = c("BRI"), 
                     mediator = "Transaction.Party")



summary(modelo_mediator71) 

figura_modelo_mediator71 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator71,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 71 - Mediator variable: Transaction Party",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator71)


# Bootstrap

modelo_mediator_Boot71 <- 
  mediation::mediate(mod_med71, mod_out71, 
                     boot = TRUE, sims = 999, 
                     treat = c("BRI"), 
                     mediator = "Transaction.Party")

summary(modelo_mediator_Boot71)

figura_modelo_mediator_Boot71 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot71,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 71 - Mediator variable: Transaction Party. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot71)



## Hipotese 7.2

## BRI - Subsector
mod_med72 <- lm(Subsector ~ BRI, 
                data = dadosGG)

mod_out72 <- lm(Quantity.in.Millions ~ Subsector + BRI, 
                data = dadosGG) 

gvlma(mod_med72) 
gvlma(mod_out72) 

modelo_mediator72 <- 
  mediation::mediate(mod_med72, mod_out72, 
                     treat = c("BRI"), 
                     mediator = "Subsector")



summary(modelo_mediator72) 

figura_modelo_mediator72 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator72,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 72 - Mediator variable: Subsector",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator72)


# Bootstrap

modelo_mediator_Boot72 <- 
  mediation::mediate(mod_med72, mod_out72, 
                     boot = TRUE, sims = 999, 
                     treat = c("GoingGlobal"), 
                     mediator = "Subsector")

summary(modelo_mediator_Boot72)

figura_modelo_mediator_Boot72 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot72,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 72 - Mediator variable: Subsector. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot72)

## HIPOTESE 8 =========================



## Hipotese 8.1



## Hipotese 8.2

## BRI - Year
mod_med82 <- lm(Year ~ GoingGlobal, 
               data = dadosGG)
summary(mod_med82)

mod_out82 <- lm(Quantity.in.Millions ~ Year + GoingGlobal, 
               data = dadosGG) 
summary(mod_out82)

gvlma(mod_med82) 
gvlma(mod_out82) 

modelo_mediator82 <- 
  mediation::mediate(mod_med82, mod_out82, 
                     treat = c("GoingGlobal"), 
                     mediator = "Year")



summary(modelo_mediator82) 

figura_modelo_mediator82 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator82,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 1 - Mediator variable: Year",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator82)


# Bootstrap

modelo_mediator_Boot82 <- 
  mediation::mediate(mod_med82, mod_out82, 
                     boot = TRUE, sims = 999, 
                     treat = c("GoingGlobal"), 
                     mediator = "Year")

summary(modelo_mediator_Boot82)

figura_modelo_mediator_Boot82 <- 
  ggstatsplot::ggcoefstats(
    x = modelo_mediator_Boot82,
    point.args = list(color = "red", size = 3, shape = 15),
    vline.args = list(size = 1, color = "#CC79A7", linetype = "dotdash"),
    title = "Estimated coefficient with conffident interval in mediator model",
    subtitle = "Model 1 - Mediator variable: Year. Bootstraped Model",
    ggtheme = hrbrthemes::theme_ipsum_ps(),
    ggstatsplot.layer = FALSE
  ) + # note the order in which the labels are entered
  ggplot2::scale_y_discrete(labels = c("ACME", "ADE", "Total Effect", "Prop. Mediated")) +
  ggplot2::labs(x = "regression coefficient", y = NULL)


ggstatsplot::ggcoefstats(modelo_mediator_Boot82)







