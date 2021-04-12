#analysis after data wrangling, cleaning and multiple imputation imputation
#load subsetted mids object
data_list1 <- readRDS("dataimp1.RDs")
dataimp1 <- miceadds::datlist2mids(data_list1)
#correlations
dataComplete <- complete(dataimp1, "long")
dataComplete <- dataComplete[,-c(1:2)]
match(c("spirituality_importance", "spirituality_practice", "ptg", "ptgRO", "ptgNP", "ptgPS", "ptgSCH", "ptgAL"), names(dataComplete))
miceadds::micombine.cor(dataimp1, 
                        variables = c(7:14), 
                        conf.level=0.95, method="pearson", nested=FALSE, partial=NULL)

#descriptive statistic------------------------------------------
#posttraumatic growth
ptg_descriptive <- with(dataimp1, expr=c("ptg(mean)"=mean(ptg), 
                                         "ptg(SD)"=stats::sd(ptg), 
                                         "ptg(S.E)"=sd(ptg)/sqrt(length(ptg)), 
                                         "ptg(min)"=min(ptg), 
                                         "ptg(max)"=max(ptg)))
# pool estimates
withPool_MI(ptg_descriptive)

#posttraumatic growth - related to others
ptgRO_descriptive <- with(dataimp1, expr=c("ptg(mean)"=mean(ptgRO), 
                                         "ptg(SD)"=stats::sd(ptgRO), 
                                         "ptg(S.E)"=sd(ptgRO)/sqrt(length(ptgRO)), 
                                         "ptg(min)"=min(ptgRO), 
                                         "ptg(max)"=max(ptgRO)))
# pool estimates
withPool_MI(ptgRO_descriptive)

#posttraumatic growth - new possibilities
ptgNP_descriptive <- with(dataimp1, expr=c("ptg(mean)"=mean(ptgNP), 
                                         "ptg(SD)"=stats::sd(ptgNP), 
                                         "ptg(S.E)"=sd(ptgNP)/sqrt(length(ptgNP)), 
                                         "ptg(min)"=min(ptgNP), 
                                         "ptg(max)"=max(ptgNP)))
# pool estimates
withPool_MI(ptgNP_descriptive)

#posttraumatic growth - personal strength
ptgPS_descriptive <- with(dataimp1, expr=c("ptg(mean)"=mean(ptgPS), 
                                         "ptg(SD)"=stats::sd(ptgPS), 
                                         "ptg(S.E)"=sd(ptgPS)/sqrt(length(ptgPS)), 
                                         "ptg(min)"=min(ptgPS), 
                                         "ptg(max)"=max(ptgPS)))
# pool estimates
withPool_MI(ptgPS_descriptive)

#posttraumatic growth - spiritual change
ptgSCH_descriptive <- with(dataimp1, expr=c("ptg(mean)"=mean(ptgSCH), 
                                         "ptg(SD)"=stats::sd(ptgSCH), 
                                         "ptg(S.E)"=sd(ptgSCH)/sqrt(length(ptgSCH)), 
                                         "ptg(min)"=min(ptgSCH), 
                                         "ptg(max)"=max(ptgSCH)))
# pool estimates
withPool_MI(ptgSCH_descriptive)

#posttraumatic growth - appreciation of life
ptgAL_descriptive <- with(dataimp1, expr=c("ptg(mean)"=mean(ptgAL), 
                                         "ptg(SD)"=stats::sd(ptgAL), 
                                         "ptg(S.E)"=sd(ptgAL)/sqrt(length(ptgAL)), 
                                         "ptg(min)"=min(ptgAL), 
                                         "ptg(max)"=max(ptgAL)))
# pool estimates
withPool_MI(ptgAL_descriptive)


library(MKmisc)
mi.t.test(data_list1, x = "ptg", y = "faith", var.equal = FALSE)
mi.t.test(data_list1, x = "ptgRO", y = "faith", var.equal = FALSE)
mi.t.test(data_list1, x = "ptgNP", y = "faith", var.equal = FALSE)
mi.t.test(data_list1, x = "ptgPS", y = "faith", var.equal = FALSE)
mi.t.test(data_list1, x = "ptgSCH", y = "faith", var.equal = FALSE)
mi.t.test(data_list1, x = "ptgAL", y = "faith", var.equal = FALSE)

miceadds::mi.anova(dataimp1, "ptg ~ religion_type")
miceadds::mi.anova(dataimp1, "ptgRO ~ religion_type")
miceadds::mi.anova(dataimp1, "ptgSCH ~ religion_type")

miceadds::mi.anova(dataimp1, "spirituality_importance ~ religion_type")
miceadds::mi.anova(dataimp1, "spirituality_practice ~ religion_type")

##data for mdescriptives in subgroups
dataComplete1 <- complete(dataimp1, "long")
dataComplete1 <- dataComplete1[,c(2,9:16)]
dataComplete2 <- lapply(data_list1, function(x) x%>% select(religion_type, faith))
dataComplete2 <- dataComplete2[[1]]
dataComplete1 <- dataComplete1 %>%
  group_by(.id) %>% 
  summarise_all(.funs = mean, digits = 0) 

dataComplete1 <- dataComplete1[,-1]
dataComplete1 <- round(dataComplete1, digits = 0)
dataComplete <- cbind(dataComplete2, dataComplete1)

t.test(spirituality_importance ~ faith, dataComplete)
t.test(spirituality_practice ~ faith, dataComplete)

###faith PTG

dataComplete %>% 
  group_by(faith) %>% 
  summarize(mean(ptg), sd(ptg), min(ptg), max(ptg), n())

dataComplete %>% 
  group_by(faith) %>% 
  summarize(mean(ptgRO), sd(ptgRO), min(ptgRO), max(ptgRO), n())

dataComplete %>% 
  group_by(faith) %>% 
  summarize(mean(ptgNP), sd(ptgNP), min(ptgNP), max(ptgNP), n())

dataComplete %>% 
  group_by(faith) %>% 
  summarize(mean(ptgPS), sd(ptgPS), min(ptgPS), max(ptgPS), n())

dataComplete %>% 
  group_by(faith) %>% 
  summarize(mean(ptgSCH), sd(ptgSCH), min(ptgSCH), max(ptgSCH), n())

dataComplete %>% 
  group_by(faith) %>% 
  summarize(mean(ptgAL), sd(ptgAL), min(ptgAL), max(ptgAL), n())

spirit <- select(data, spirituality_importance, spirituality_practice, faith)
spirit <- spirit[,-1]
describeBy(spirit, spirit$faith)


###religion type PTG

dataComplete %>% 
  group_by(religion_type) %>% 
  summarize(mean(ptg), sd(ptg), min(ptg), max(ptg), n())

dataComplete %>% 
  group_by(religion_type) %>% 
  summarize(mean(ptgRO), sd(ptgRO), min(ptgRO), max(ptgRO), n())

dataComplete %>% 
  group_by(religion_type) %>% 
  summarize(mean(ptgNP), sd(ptgNP), min(ptgNP), max(ptgNP), n())

dataComplete %>% 
  group_by(religion_type) %>% 
  summarize(mean(ptgPS), sd(ptgPS), min(ptgPS), max(ptgPS), n())

dataComplete %>% 
  group_by(religion_type) %>% 
  summarize(mean(ptgSCH), sd(ptgSCH), min(ptgSCH), max(ptgSCH), n())

dataComplete %>% 
  group_by(religion_type) %>% 
  summarize(mean(ptgAL), sd(ptgAL), min(ptgAL), max(ptgAL), n())

#bayes correlations
library(BayesFactor)
match(c("spirituality_importance", "spirituality_practice", "ptg", "ptgRO", "ptgNP", "ptgPS", "ptgSCH", "ptgAL"), names(dataComplete))

b <- dataComplete[,c(3:10)]

correlationBF(b$ptg, b$ptgRO)
correlationBF(b$ptg, b$ptgNP)
correlationBF(b$ptg, b$ptgPS)
correlationBF(b$ptg, b$ptgSCH)
correlationBF(b$ptg, b$ptgAL)
correlationBF(b$ptg, b$spirituality_importance)
correlationBF(b$ptg, b$spirituality_practice)
correlationBF(b$ptgRO, b$ptgNP)
correlationBF(b$ptgRO, b$ptgPS)
correlationBF(b$ptgRO, b$ptgSCH)
correlationBF(b$ptgRO, b$ptgAL)
correlationBF(b$ptgRO, b$spirituality_importance)
correlationBF(b$ptgRO, b$spirituality_practice)
correlationBF(b$ptgNP, b$ptgPS)
correlationBF(b$ptgNP, b$ptgSCH)
correlationBF(b$ptgNP, b$ptgAL)
correlationBF(b$ptgNP, b$spirituality_importance)
correlationBF(b$ptgNP, b$spirituality_practice)
correlationBF(b$ptgPS, b$ptgSCH)
correlationBF(b$ptgPS, b$ptgAL)
correlationBF(b$ptgPS, b$spirituality_importance)
correlationBF(b$ptgPS, b$spirituality_practice)
correlationBF(b$ptgSCH, b$ptgAL)
correlationBF(b$ptgSCH, b$spirituality_importance)
correlationBF(b$ptgSCH, b$spirituality_practice)
correlationBF(b$ptgAL, b$spirituality_importance)
correlationBF(b$ptgAL, b$spirituality_practice)
correlationBF(b$spirituality_importance, b$spirituality_practice)

#Tukey post hoc analysis for anova
model1 <- aov(ptg ~ religion_type, data = dataComplete)
TukeyHSD(model1, ordered = TRUE)
model2 <- aov(ptgRO ~ religion_type, data = dataComplete)
TukeyHSD(model2, ordered = TRUE)
model3 <- aov(ptgSCH ~ religion_type, data = dataComplete)
TukeyHSD(model3, ordered = TRUE)
model4 <- aov(spirituality_importance ~ religion_type, data = dataComplete)
TukeyHSD(model4, ordered = TRUE)
model5 <- aov(spirituality_practice ~ religion_type, data = dataComplete)
TukeyHSD(model5, ordered = TRUE)

#plots
library(yarrr)
#posttraumatic growth
yarrr::pirateplot(ptg ~ faith, 
                  data = dataComplete, 
                  theme = 2, 
                  cap.beans = TRUE,
                  cex.lab = 1.3,
                  cex.names = 1.3,
                  ylab = "Posttraumatic growth", xlab = "Faith",
                  main = "Posttraumatic growth in cancer survivors with faith and without faith")

yarrr::pirateplot(ptg ~ religion_type, 
                  data = dataComplete, 
                  theme = 2, 
                  cap.beans = TRUE,
                  cex.lab = 1.3,
                  cex.names = 1.3,
                  ylab = "Posttraumatic growth", xlab = "Religion type",
                  main = "Posttraumatic growth in cancer survivors based on different types of religion")
#ptg relating to others
yarrr::pirateplot(ptgRO ~ faith, 
                  data = dataComplete, 
                  theme = 2, 
                  cap.beans = TRUE,
                  cex.lab = 1.3,
                  cex.names = 1.3,
                  ylab = "Relating to others (PTG)", xlab = "Faith",
                  main = "Relating to others dimension of PTG in cancer survivors with faith and without faith")

yarrr::pirateplot(ptgRO ~ religion_type, 
                  data = dataComplete, 
                  theme = 2, 
                  cap.beans = TRUE,
                  cex.lab = 1.3,
                  cex.names = 1.3,
                  ylab = "Relating to others (PTG)", xlab = "Religion type",
                  main = "Relating to others dimension of PTG in cancer survivors based on different types of religion")
#ptg new possibilities
yarrr::pirateplot(ptgNP ~ faith, 
                  data = dataComplete, 
                  theme = 2, 
                  cap.beans = TRUE,
                  cex.lab = 1.3,
                  cex.names = 1.3,
                  ylab = "New possibilities (PTG)", xlab = "Faith",
                  main = "Relating to others dimension of PTG in cancer survivors with faith and without faith")

yarrr::pirateplot(ptgNP ~ religion_type, 
                  data = dataComplete, 
                  theme = 2, 
                  cap.beans = TRUE,
                  cex.lab = 1.3,
                  cex.names = 1.3,
                  ylab = "New possibilities (PTG)", xlab = "Religion type",
                  main = "New possibilities dimension of PTG in cancer survivors based on different types of religion")
#ptg personal strength
yarrr::pirateplot(ptgPS ~ faith, 
                  data = dataComplete, 
                  theme = 2, 
                  cap.beans = TRUE,
                  cex.lab = 1.3,
                  cex.names = 1.3,
                  ylab = "Personal strength (PTG)", xlab = "Faith",
                  main = "Personal strength dimension of PTG in cancer survivors with faith and without faith")

yarrr::pirateplot(ptgPS ~ religion_type, 
                  data = dataComplete, 
                  theme = 2, 
                  cap.beans = TRUE,
                  cex.lab = 1.3,
                  cex.names = 1.3,
                  ylab = "Personal strength (PTG)", xlab = "Religion type",
                  main = "Personal strength dimension of PTG in cancer survivors based on different types of religion")
#ptg spiritual change
yarrr::pirateplot(ptgSCH ~ faith, 
                  data = dataComplete, 
                  theme = 2, 
                  cap.beans = TRUE,
                  cex.lab = 1.3,
                  cex.names = 1.3,
                  ylab = "Spiritual change (PTG)", xlab = "Faith",
                  main = "Spiritual change dimension of PTG in cancer survivors with faith and without faith")

yarrr::pirateplot(ptgSCH ~ religion_type, 
                  data = dataComplete, 
                  theme = 2, 
                  cap.beans = TRUE,
                  cex.lab = 1.3,
                  cex.names = 1.3,
                  ylab = "Spiritual change (PTG)", xlab = "Religion type",
                  main = "Spiritual change dimension of PTG in cancer survivors based on different types of religion")
#ptg appreciation of life
yarrr::pirateplot(ptgAL ~ faith, 
                  data = dataComplete, 
                  theme = 2, 
                  cap.beans = TRUE,
                  cex.lab = 1.3,
                  cex.names = 1.3,
                  ylab = "Appreciation of life (PTG)", xlab = "Faith",
                  main = "Appreciation of life of PTG in cancer survivors with faith and without faith")

yarrr::pirateplot(ptgAL ~ religion_type, 
                  data = dataComplete, 
                  theme = 2, 
                  cap.beans = TRUE,
                  cex.lab = 1.3,
                  cex.names = 1.3,
                  ylab = "Appreciation of life (PTG)", xlab = "Religion type",
                  main = "Appreciation of life of PTG in cancer survivors based on different types of religion")
#spiritual importance
yarrr::pirateplot(spirituality_importance ~ faith, 
                  data = dataComplete, 
                  theme = 2, 
                  cap.beans = TRUE,
                  cex.lab = 1.3,
                  cex.names = 1.3,
                  ylab = "Spirituality importance", xlab = "Faith",
                  main = "Spirituality importance in cancer survivors with faith and without faith")

yarrr::pirateplot(spirituality_importance ~ religion_type, 
                  data = dataComplete, 
                  theme = 2, 
                  cap.beans = TRUE,
                  cex.lab = 1.3,
                  cex.names = 1.3,
                  ylab = "Spirituality importance", xlab = "Religion type",
                  main = "Spirituality importance in cancer survivors based on different types of religion")
#spiritual practice
yarrr::pirateplot(spirituality_practice ~ faith, 
                  data = dataComplete, 
                  theme = 2, 
                  cap.beans = TRUE,
                  cex.lab = 1.3,
                  cex.names = 1.3,
                  ylab = "Spirituality practice", xlab = "Faith",
                  main = "Spirituality practice in cancer survivors with faith and without faith")

yarrr::pirateplot(spirituality_practice ~ religion_type, 
                  data = dataComplete, 
                  theme = 2, 
                  cap.beans = TRUE,
                  cex.lab = 1.3,
                  cex.names = 1.3,
                  ylab = "Spirituality practice", xlab = "Religion type",
                  main = "Spirituality practice in cancer survivors based on different types of religion")

