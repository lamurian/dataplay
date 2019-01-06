library(ggplot2)

# Parse csv into dataframe
WHR <- read.csv('WHR.csv')
SIZE <- dim(WHR)[2]

# Build dataframe for each country
CNAME <- unique(WHR$country)            # Unique set of country name (duplicate removed)
SIZE2 <- length(CNAME)

# Life ladder = Happiness scale (0-10)
YEAR_MIN <- min(WHR$year); YEAR_MAX <- max(WHR$year)
YEAR <- c(YEAR_MIN:YEAR_MAX)
SIZE3 <- length(YEAR)

mkdf <- function(name) {
    # Create another dataframe based on given indicators
    DF <- data.frame(rep(NaN, rep(SIZE3)))
    LOWER <- vector(); UPPER <- vector()
    for (i in 1:SIZE2) {
        VEC <- vector()
        for (k in 1:SIZE3) {
            VAR <- name[WHR$country == CNAME[i] & WHR$year == YEAR[k]]
            # Condition to check for null character
            if (length(VAR) == 0) VAR <- NaN
            VEC <- c(VEC, VAR)
        }
        DF[i] <- VEC
    }
    for (i in 1:SIZE3) {
        LOWER <- c(LOWER, min(DF[i, ], na.rm=T))
        UPPER <- c(UPPER, max(DF[i, ], na.rm=T))
    }
    colnames(DF) <- CNAME
    DF$lower <- LOWER
    DF$upper <- UPPER
    DF$year <- YEAR
    DF
}

LADDER <- mkdf(WHR$Life.Ladder)

r_sub <- function(data) {
    # Do recursive substraction
    i <- 1; RES <- vector()
    while (i + 1 <= length(data)) {
        RES <- c(RES, data[i + 1] - data[i])
        i <- i + 1
    }
    RES
}

# Make comparison plot
P <- ggplot(LADDER, aes(x = year)) +
    geom_line(aes(y = Indonesia)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
    labs(title = "World Happiness Report",
         subtitle = "Comparison Between Indonesia and Global Population",
         x = "Years",
         y = "Life Ladder Scale") +
    geom_vline(xintercept = c(2009, 2013.97), color = "coral", size = 1) +
    geom_vline(xintercept = c(2014, 2017), color = "slateblue", size = 1) +
    scale_x_continuous(breaks = seq(2005, 2017, 1)) +
    scale_y_continuous(breaks = seq(1, 10, 1), limit = c(1, 10))

# Analysze data changes over certain period
stat_dyn <- function(vec) {
    ERA <- list(1:5, 5:10, 10:13)
    # Empty list as classifier
    GOV <- list("SBY1" = "", "SBY2" = "", "JKW" = "")

    k <- 1
    for (i in ERA) {
        GOV[[k]] <- r_sub(vec[i,])            # Changes thorough specific era
        k <- k + 1
    }
    GOV
}

# Empty list to contain all calculations
RES_GOV <- list(); RES_GOV <- append(RES_GOV, c(1:SIZE2))

for (i in 1:SIZE2) {
    RES_GOV[[i]] <- stat_dyn(LADDER[i])
}

# Vector containing different presidential eras
SBY1 <- list(); SBY1 <- append(SBY1, c(1:SIZE2))
SBY2 <- list(); SBY2 <- append(SBY2, c(1:SIZE2))
JKW <- list(); JKW <- append(JKW, c(1:SIZE2))

for (i in 1:SIZE2) {
    SBY1[[i]] <- RES_GOV[[i]]$SBY1
    SBY2[[i]] <- RES_GOV[[i]]$SBY2
    JKW[[i]] <- RES_GOV[[i]]$JKW
}

dyn_compare <- function(list_1, list_2, label_1, label_2) {
    # Empty containers
    CLAB1 <- vector(); CLAB2 <- vector(); CLAB <- vector()
    LAB1 <- vector(); LAB2 <- vector(); LAB <- vector()
    VAL1 <- vector(); VAL2 <- vector(); VAL <- vector()
    
    # Iterators
    LEN1 <- length(list_1[[1]]); LEN2 <- length(list_2[[1]])

    for (i in 1:SIZE2) {
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

LAD_DYN <- dyn_compare(SBY2, JKW, "2009-2014", "2014-Now")
MED1 <- median(Reduce(c, SBY2), na.rm = T)
MED2 <- median(Reduce(c, JKW), na.rm = T)

# Boxplot to compare differences
B1 <- ggplot(LAD_DYN, aes(x = CLAB, y = VAL, fill = LAB)) +
    geom_boxplot() +
    geom_hline(yintercept = MED1, color = "coral", size = 1) +
    geom_hline(yintercept = MED2, color = "slateblue", size = 1) +
    facet_wrap(~LAB, ncol=1) +
    labs(title = "Life Ladder Growth Rate",
         subtitle = "Global Comparison",
         x = "Country Lists",
         y = "Life Ladder") +
    theme(axis.text.x = element_blank())

B2 <- ggplot(LAD_DYN, aes(x = LAB, y = VAL, fill = LAB)) +
    geom_boxplot() +
    labs(title = "Life Ladder Growth Rate",
         subtitle = "Global Comparison",
         x = "Period",
         y = "Life Ladder")
