library("ggplot2")

# Parsing dataframe
HDI <- read.csv("HDI.csv")
SIZE <- dim(HDI)[1]

# Loop to slice the years
YEARS <- vector()
for (i in 1990:2017) {
    VAR <- paste(c("X", i), collapse = "")
    YEARS <- c(YEARS, VAR)
}

# Get global value
G_VAL <- HDI[YEARS]
colnames(G_VAL) <- c(1990:2017)

# Reshaping the dataframe
HDI_R <- data.frame(rep(0, 28))

for (i in 1:SIZE) {
    LOCAL <- G_VAL[i, ]
    LOCAL <- Reduce(c, LOCAL)
    HDI_R[i] <- LOCAL
}

# Parameters for ribbon plot
LOWER <- vector(); UPPER <- vector()
for (i in 1:dim(HDI_R)[1]) {
    LOWER <- c(LOWER, min(HDI_R[i, ], na.rm = T))
    UPPER <- c(UPPER, max(HDI_R[i, ], na.rm = T))
}

HDI_R$lower <- LOWER
HDI_R$upper <- UPPER
HDI_R$year <- c(1990:2017)

# Comparison Plot
P <- ggplot(HDI_R, aes(x = year)) +
    geom_line(aes(y = V79, colour = "Indonesia")) +
    labs(title = "Human Development Index",
         subtitle = "Comparison Between Indonesia and Global Population",
         x = "Years",
         y = "Human Development Index") +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = "Global HDI Range"),
                alpha = 0.3) +
    scale_colour_manual("", values = "blue") +
    scale_fill_manual("", values = "grey12") +
    geom_vline(xintercept = c(2009, 2013.97), color = "coral", size = 1) +
    geom_vline(xintercept = c(2014, 2017), color = "slateblue", size = 1) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

# Divide HDI based on presidential era
INA <- HDI_R[79]

r_sub <- function(vec) {
    # Do recursive susbtraction within a vec
    i <- 1
    bins <- vector()
    while (i+1 <= length(vec)) {
        bins <- c(bins, vec[i+1] - vec[i])
        i <- i + 1
    }
    bins
}

# Calculate HDI growth
hdi_growth <- function(vec) {
    ERA <- list(9:10, 10:12, 12:15, 15:20, 20:25, 25:28)
    # Empty list as classifier
    GOV <- list("HBB" = "", "GUS" = "", "MEG" = "", "SBY1" = "", "SBY2" = "", "JKW" = "")
    LOG_GOV <- GOV

    k <- 1
    for (i in ERA) {
        GOV[[k]] <- r_sub(vec[i, ])         # HDI changes thorough specific era
        LOG_GOV[[k]] <- log(GOV[[k]])       # Log value of such changes
        k <- k + 1
    }
    list("GOV" = GOV, "LOG_GOV" = LOG_GOV)
}

# Empty list to contain all calculations
RES_GOV <- list(); RES_GOV <- append(RES_GOV, c(1:SIZE))
RES_LOG <- list(); RES_LOG <- append(RES_LOG, c(1:SIZE))

for (i in 1:SIZE) {
    RES_GOV[[i]] <- hdi_growth(HDI_R[i])[["GOV"]]
    RES_LOG[[i]] <- hdi_growth(HDI_R[i])[["LOG_GOV"]]
}

# Categorize acquired results
GOV <- RES_GOV[[79]]; LOG_GOV <- RES_LOG[[79]]

# Comparing global HDI growth during JKW and SBY2 eras
SBY <- list(); SBY <- append(SBY, c(1:SIZE))
JKW <- list(); JKW <- append(JKW, c(1:SIZE))

for (i in 1:SIZE) {
    SBY[[i]] <- RES_LOG[[i]]$SBY2
    JKW[[i]] <- RES_LOG[[i]]$JKW
}

# Preparing a new dataframe
growth_compare <- function(list_1, list_2, label_1, label_2) {
    # Vector containing country name
    CNAME <- vector()
    for (i in HDI$Country) CNAME <- c(CNAME, i)

    # Empty containers
    CLAB1 <- vector(); CLAB2 <- vector(); CLAB <- vector()
    LAB1 <- vector(); LAB2 <- vector(); LAB <- vector()
    VAL1 <- vector(); VAL2 <- vector(); VAL <- vector()
    
    # Iterators
    LEN1 <- length(list_1[[1]]); LEN2 <- length(list_2[[1]])

    for (i in 1:SIZE) {
        # For list_1
        CNAME1 <- paste(c(CNAME[i], "_1"), collapse="")
        CLAB1 <- c(CLAB1, rep(CNAME1, LEN1))
        LAB1 <- c(LAB1, rep(label_1, LEN1))
        VAL1 <- c(VAL1, list_1[[i]])

        # For list_2
        CNAME2 <- paste(c(CNAME[i], "_2"), collapse="")
        CLAB2 <- c(CLAB2, rep(CNAME2, LEN2))
        LAB2 <- c(LAB2, rep(label_2, LEN2))
        VAL2 <- c(VAL2, list_2[[i]])
    }
    CLAB <- c(CLAB1, CLAB2); LAB <- c(LAB1, LAB2); VAL <- c(VAL1, VAL2)
    data.frame(CLAB, LAB, VAL)              # Returning dataframe
}

GROWTH <- growth_compare(SBY, JKW, "2009-2014", "2014-Now")
MED1 <- median(Reduce(c, SBY), na.rm=T); MED2 <- median(Reduce(c, JKW), na.rm=T)

B1 <- ggplot(GROWTH, aes(x = CLAB, y = VAL, fill = LAB)) +
    geom_boxplot() +
    geom_hline(yintercept = MED1, color = "coral", size = 1) +
    geom_hline(yintercept = MED2, color = "slateblue", size = 1) +
    facet_wrap(~LAB, ncol=1) +
    labs(title = "HDI Growth Rate",
         subtitle = "Global Comparison",
         x = "Country Lists",
         y = "HDI Log") +
    theme(axis.text.x = element_blank())

B2 <- ggplot(GROWTH, aes(x = LAB, y = VAL, fill = LAB)) +
    geom_boxplot() +
    labs(title = "HDI Growth Rate",
         subtitle = "Global Comparison",
         x = "Period",
         y = "HDI Log")
