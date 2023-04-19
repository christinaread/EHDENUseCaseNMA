library(readxl)
ResultsFrequentFormat <- read_excel("ResultsFrequentFormat.xlsx")
View(ResultsFrequentFormat)

install.packages("netmeta")
library(netmeta)

m.netmeta <- netmeta(TE = TE,
                     seTE = seTE,
                     treat1 = treat1,
                     treat2 = treat2,
                     studlab = Author,
                     data = ResultsFrequentFormat,
                     sm = "OR",
                     fixed = FALSE,
                     random = TRUE,
                     reference.group = "SC",
                     details.chkmultiarm = TRUE,
                     sep.trts = " vs ",
                     tol.multiarm = 1)
summary(m.netmeta)

forest(m.netmeta, 
       reference.group = "SC",
       labels = m.netmeta$trts,
       xlim = c(-1.5, 0.5))

netmeta::netheat(m.netmeta)
netsplit(m.netmeta) %>% forest()
funnel(m.netmeta, 
       order = c("sc", "remdesivir", "tocilizumab"), 
       linreg = TRUE)




#################################################################################################
#is it calculating oR correctly?
#################################################################################################
ResultsFrequentFormatFlipped <- read_excel("ResultsFrequentFormatFlipped.xlsx")
View(ResultsFrequentFormatFlipped)


m.netmetaFlipped <- netmeta(TE = TE,
                     seTE = seTE,
                     treat1 = treat1,
                     treat2 = treat2,
                     studlab = Author,
                     data = ResultsFrequentFormatFlipped,
                     sm = "OR",
                     fixed = FALSE,
                     random = TRUE,
                     reference.group = "SC",
                     details.chkmultiarm = TRUE,
                     sep.trts = " vs ",
                     tol.multiarm = 1)
summary(m.netmetaFlipped)

forest(m.netmeta, 
       reference.group = "SC",
       labels = m.netmeta$trts,
       xlim = c(-1.5, 0.5))

netmeta::netheat(m.netmeta)
netsplit(m.netmeta) %>% forest()
funnel(m.netmeta, 
       order = c("sc", "remdesivir", "tocilizumab"), 
       linreg = TRUE)