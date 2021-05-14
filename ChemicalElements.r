library(readr)
library(ggplot2)
train <- read_csv("superconductivity/superconduct/train.csv")
chem = read_csv("superconductivity/superconduct/unique_m.csv")

chem <- unique_m
names(chem)
chem$critical_temp
chem["CT"] = NA
chem$CT[chem$critical_temp > 20] = "Low"
chem$CT[chem$critical_temp <= 20] = "High"

summary(chem)
View(chem)
str(chem)

amount = unlist(lapply(chem[1:86], sum))
both = data.frame(tail(sort(amount/sum(chem[1:86])*100), 10)) # 220588.3
names(both) = "percent"
both = data.frame(overall_top, both[order(both$percent, decreasing = T),])
names(both) = c("element", "percent")
both$percent = round(both$percent, digits = 2)
both
barplot(both)
par(mfrow = c(1, 1))
both_figure = ggplot(both, aes(x = reorder(element, percent), y = percent)) + coord_flip()+
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label= percent), hjust = 1.1,vjust=.5, color="white", size=4)+
  theme_minimal() + ylim(c(0,45))+
  labs(x = "All CT, 10 Most Common Element", y = "Percent %") +
  theme(axis.title=element_text(size=16,face="bold"))
both_figure ######################
# Next, let's look only at the low CT's
chem_low = chem[chem$CT %in% "Low",]
amount_low = unlist(lapply(chem_low[1:86], sum))
low = data.frame(tail(sort(amount_low/sum(chem_low[1:86])*100), 10)) # 122412.3
names(low) = "percent"
low
overall_low = c("O", "Cu", "Ba", "Sr", "Ca", "Bi", "Y", "C", "La", "Fe")
low = data.frame(overall_low, low[order(low$percent, decreasing = T),])
names(low) = c("element", "percent")
low$percent = round(low$percent, digits = 2)
low_figure = ggplot(low, aes(x = reorder(element, percent), y = percent)) + coord_flip()+
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label= percent), hjust = 1.1,vjust=.5, color="white", size=3)+
  theme_minimal() + ylim(c(0,45))+
  labs(x = "Low CT, 10 Most Common Element", y = "Percent %") +
  theme(axis.title=element_text(size=16,face="bold"))
low_figure#####################


# Next, let's look only at the high CT's
chem_high = chem[chem$CT %in% "High",]
amount_high = unlist(lapply(chem_high[1:86], sum))
high = data.frame(tail(sort(amount_high/sum(chem_high[1:86])*100), 10)) # 98176.04
names(high) = "percent"
high
overall_high = c("Nb", "O", "Zr", "C", "V", "Si", "Ti", "Cu", "La", "Mo")
high = data.frame(overall_high, high[order(high$percent, decreasing = T),])
names(high) = c("element", "percent")
high$percent = round(high$percent, digits = 2)
high_figure = ggplot(high, aes(x = reorder(element, percent), y = percent)) + coord_flip()+
  geom_bar(stat="identity", fill="steelblue")+ ylim(c(0, 45)) +
  geom_text(aes(label= percent), hjust = 1.1,vjust=.5, color="white", size=4)+
  theme_minimal() + 
  labs(x = "High CT, 10 Most Common Element", y = "Percent %") +
  theme(axis.title=element_text(size=16,face="bold"))
high_figure#####################

