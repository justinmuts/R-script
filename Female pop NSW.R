### Jay's information on Suburb locality of single females living in NSW.

#Packages to load
library(tidyverse) 


#Check the current directory
getwd()

#Change the directory to Downloads
setwd("/Users/Justin_Mah/Downloads/ssc2016shpfile")

#list the files in that directory
list.files()

# Or Alternatively
dir(".")

# import the NSW population of females living according to suburbs 2016 dataset from ABS

fempop <- read_csv("pop_24_35_Seg_3.csv") # From the tidyverse package
glimpse(fempop)
str(fempop)
View(fempop)

#Column headings of fempop dataset
names(fempop)

# Lets rename the column headings:

fempop2 <-rename(fempop,ID=X1,SSC_2016=SSC_CODE_2016,pop=population)

names(fempop2)
View(fempop2)


#Excluding the "SSC" from the SSC codes column list 
fempop2 <- fempop2 %>%
  separate(SSC_2016, into = c("SSC","SSC_codes"),sep="SSC")

# Removing SSC column 
fempop2 <- select(fempop2,-SSC)

#View fempop dataset in a spreadsheet format
View(fempop2)
summary(fempop2)

#creating new columns with numbers rather than percentages
fempop2$indian <-fempop2$pop * fempop2$F_India_prop
fempop2$australian <-fempop2$pop * fempop2$F_Australia_prop
fempop2$canadian <-fempop2$pop * fempop2$F_Canada_prop
fempop2$chile <-fempop2$pop * fempop2$F_Chile_prop
fempop2$croatia <-fempop2$pop * fempop2$F_Croatia_prop
fempop2$england <-fempop2$pop * fempop2$F_England_prop
fempop2$germany <-fempop2$pop * fempop2$F_Germany_prop
fempop2$greece <-fempop2$pop * fempop2$F_greece_prop
fempop2$ireland <-fempop2$pop * fempop2$F_ireland_prop
fempop2$italy <-fempop2$pop * fempop2$F_italy_prop
fempop2$nz <-fempop2$pop * fempop2$F_New_Zealand_prop
fempop2$philippines <-fempop2$pop * fempop2$F_Philippines_prop
fempop2$poland <-fempop2$pop * fempop2$F_Poland_prop
fempop2$scotland <-fempop2$pop * fempop2$F_Scotland_prop
fempop2$wales <-fempop2$pop * fempop2$F_Wales_prop
fempop2$usa <-fempop2$pop * fempop2$F_Wales_prop
fempop2$china <-fempop2$pop * fempop2$F_China_prop
View(fempop2)

names(fempop2)

#remove columns in the dataset
fpop <-select(fempop2,ID:F_Tot_Never_Married,indian:china)
View(fpop)
names(fpop)

test.a <-fpop %>%
  gather(australian,canadian,chile,china,croatia,england,germany,greece,indian,ireland,italy,nz,
                philippines,poland,scotland,wales,usa,key="nationality",
         value="PopByNationality") 
View(test.a)

unique(test.b$SSC_NAME16)

OA.nsw16A <-inner_join(test.a,Output.Areas@data,by=c("SSC_codes"="SSC_CODE16"))
names(OA.nsw16A)
View(OA.nsw16A)

test.b <-OA.nsw16A
names(test.b)
test.b <-rename(test.b,suburb=SSC_NAME16)

View(test.b)

#nesting data (Machine Learning in the tidyverse)
testb_nested <-test.b %>%
  group_by(nationality) %>% #ethnicity
  nest()
testb_nested$data
head(testb_nested)

# Create the unnested dataframe called gap_unnnested
testb_unnested <- testb_nested %>% 
  unnest()
names(testb_unnested)
View(testb_unnested)
setequal(testb_nested,testb_unnested)
identical(testb_nested,testb_unnested)


# Calculate the mean population for each country
testb_nested <- testb_nested %>%
  mutate(mean_pop = map(.x=testb_nested$data,.f= ~mean(.x$pop)))

testb_nested$data
names(testb_nested)
head(testb_nested)

### ggplot

names(test.b)
View(test.b)

ggplot(test.b)+geom_point(aes(x=suburb,y=PopByNationality))+facet_grid(~nationality)
ggplot(test.b)+geom_point(aes(y=suburb,x=PopByNationality,colour=nationality))+facet_grid(~nationality)+xlab()
?xlab

ggplot(test.b)+geom_boxplot(aes(y=suburb,x=PopByNationality))+facet_grid(~nationality)

ggplot(data = test.b) + 
  geom_point(aes(y = suburb, x = PopByNationality, color = nationality))

ggplot(data = test.b) + 
  geom_hex(aes(y = suburb, x = PopByNationality, color = nationality))

ggplot(data = test.b) + 
  geom_boxplot(aes(y = suburb, x = PopByNationality, color = nationality))

china<-filter(test.b,nationality=="china")
View(china)

china <- test.b %>%
  gather("24-26","27-29","30-32",key="age_band",value="band_pop")
ggplot(china)+geom_boxplot(aes(x=suburb,y=PopByNationality,fill=age_band))+theme_classic()

ggplot(china)+geom_point(aes(y=suburb,x=band_pop,colour=age_band))

## BARCHART for the female population based on nationality according to suburbs:

#China

# we first create a dataset for the specific nationality
cha<-filter(test.b,nationality=="china")

#View the result of the dataset in a spreadsheet format 
View(cha)

# Gather the age band group "24-26","27-29" and "30-32" columns into one 
cha <- cha %>%
  gather("24-26","27-29","30-32",key="age_band",value="band_pop")

# Geom Jitter of age band group 
ggplot(cha,aes(x=band_pop,y=suburb))+geom_jitter(aes(colour=age_band))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: China",y="No. of People",x="Suburb")

# Geom bar of age band group 
ggplot(cha,aes(x=reorder(suburb,band_pop),y=band_pop))+geom_histogram(aes(fill=age_band),stat = "identity",position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: China",y="No. of People",x="Suburb")+facet_wrap(~age_band)

str(cha)

#Histogram
ggplot(cha,aes(x=reorder(suburb,PopByNationality),y=PopByNationality))+geom_histogram(aes(fill=PopByNationality),binwidth = 1,position="dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney",subtitle = "Nationality: China",y="No. of People",x="Suburb")



######## WORK IN PROGRESS ###### DO NOT USE!!!

#we first create a dataset for the specific nationality
china<-filter(test.b,nationality=="china")

#View the result of the dataset in a spreadsheet format 
View(china)

# Gather the age band group "24-26","27-29" and "30-32" columns into one 
china <- test.b %>%
  gather("24-26","27-29","30-32",key="age_band",value="band_pop")
ggplot(china)+geom_boxplot(aes(x=suburb,y=PopByNationality,fill=age_band))+theme_classic()+coord_flip()

ggplot(china)+geom_point(aes(y=suburb,x=band_pop,colour=age_band))

ggplot(china,aes(x=suburb,y=PopByNationality))+geom_bar(aes(fill=age_band),position="dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
facet_wrap(~age_band)+coord_flip()+
  labs(title = "Analysis of young female population in Sydney",subtitle = "Nationality: China",y="No. of People")

ggplot(china,aes(y=suburb,x=PopByNationality))+geom_bin2d(aes(fill=age_band))

############

#Australian

# we first create a dataset for the specific nationality
aus<-filter(test.b,nationality=="australian")

#View the result of the dataset in a spreadsheet format 
View(aus)

# Gather the age band group "24-26","27-29" and "30-32" columns into one 
aus <- aus %>%
  gather("24-26","27-29","30-32",key="age_band",value="band_pop")
View(test.b)
# Geom Jitter of age band group 
ggplot(aus,aes(x=reorder(suburb,band_pop),y=band_pop))+geom_jitter(aes(colour=age_band))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Australian",y="No. of People",x="Suburb")

# Geom bar of age band group 
ggplot(aus,aes(x=reorder(suburb,band_pop),y=band_pop))+geom_histogram(aes(fill=age_band),stat = "identity",position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Australian",y="No. of People",x="Suburb")+facet_wrap(~age_band)

str(aus)

#Histogram
ggplot(aus)+geom_histogram(aes(x=suburb,y=PopByNationality,fill=PopByNationality),binwidth = 1,position="dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney",subtitle = "Nationality: Australian",y="No. of People",x="Suburb")

# Histogram improved
ggplot(aus,aes(x=reorder(suburb,PopByNationality),y=PopByNationality))+geom_histogram(aes(fill=PopByNationality),binwidth = 1,position="dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney",subtitle = "Nationality: Australian",y="No. of People",x="Suburb")


# (Do not use) ggplot(aus,aes(y=suburb,x=PopByNationality))+geom_bin2d(aes(fill=age_band))

#Canadian

# we first create a dataset for the specific nationality
cad<-filter(test.b,nationality=="canadian")

#View the result of the dataset in a spreadsheet format 
View(cad)

# Gather the age band group "24-26","27-29" and "30-32" columns into one 
cad <- cad %>%
  gather("24-26","27-29","30-32",key="age_band",value="band_pop")

# Geom Jitter of age band group 
ggplot(cad,aes(y=suburb,x=band_pop))+geom_jitter(aes(colour=age_band))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Canadian",y="No. of People",x="Suburb")

# Geom bar of age band group 
ggplot(cad,aes(x=reorder(suburb,band_pop),y=band_pop))+geom_histogram(aes(fill=age_band),stat = "identity",position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Canadian",y="No. of People",x="Suburb")+facet_wrap(~age_band)

# Histogram 
ggplot(cad)+geom_histogram(aes(x=suburb,y=PopByNationality,fill=PopByNationality),binwidth = 1,position="dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney",subtitle = "Nationality: Canadian",y="No. of People",x="Suburb")

#Chile

chile<-filter(test.b,nationality=="chile")

#View the result of the dataset in a spreadsheet format 
View(chile)

# Gather the age band group "24-26","27-29" and "30-32" columns into one and 
# create a dataset for the specific nationality
chile <- chile %>%
  gather("24-26","27-29","30-32",key="age_band",value="band_pop") %>%
  filter(nationality=="chile")
View(chile)

# Geom Jitter of age band group 
ggplot(chile,aes(y=suburb,x=band_pop))+geom_jitter(aes(colour=age_band))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Chile",y="No. of People",x="Suburb")

# Geom bar of age band group 
ggplot(chile,aes(x=reorder(suburb,band_pop),y=band_pop))+geom_histogram(aes(fill=age_band),stat = "identity",position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Chile",y="No. of People",x="Suburb")+facet_wrap(~age_band)

# Histogram
ggplot(chile,aes(x=reorder(suburb,PopByNationality),y=PopByNationality))+geom_histogram(aes(fill=PopByNationality),binwidth = 1,position="dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney",subtitle = "Nationality: Chile",y="No. of People",x="Suburb")


#Croatia

# we first create a dataset for the specific nationality
cro<-filter(test.b,nationality=="croatia")

#View the result of the dataset in a spreadsheet format 
View(cro)

# Gather the age band group "24-26","27-29" and "30-32" columns into one 
cro <- cro %>%
  gather("24-26","27-29","30-32",key="age_band",value="band_pop")

# Geom Jitter of age band group 
ggplot(cro,aes(y=suburb,x=band_pop))+geom_jitter(aes(colour=age_band))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Croatian",y="No. of People",x="Suburb")


# Geom bar of age band group 
ggplot(cro,aes(x=reorder(suburb,band_pop),y=band_pop))+geom_histogram(aes(fill=age_band),stat = "identity",position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Croatia",y="No. of People",x="Suburb")+facet_wrap(~age_band)

str(cro)

#Histogram
ggplot(cro,aes(x=reorder(suburb,PopByNationality),y=PopByNationality))+geom_histogram(aes(fill=PopByNationality),binwidth = 1,position="dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney",subtitle = "Nationality: Croatian",y="No. of People",x="Suburb")



#England

# we first create a dataset for the specific nationality
eng<-filter(test.b,nationality=="england")

#View the result of the dataset in a spreadsheet format 
View(eng)

# Gather the age band group "24-26","27-29" and "30-32" columns into one 
eng <- eng %>%
  gather("24-26","27-29","30-32",key="age_band",value="band_pop")

# Geom Jitter of age band group 
ggplot(eng,aes(x=band_pop,y=suburb))+geom_jitter(aes(colour=age_band))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: English",y="No. of People",x="Suburb")

# Geom bar of age band group 
ggplot(eng,aes(x=reorder(suburb,band_pop),y=band_pop))+geom_histogram(aes(fill=age_band),stat = "identity",position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: English",y="No. of People",x="Suburb")+facet_wrap(~age_band)

str(eng)

#Histogram
ggplot(eng,aes(x=reorder(suburb,PopByNationality),y=PopByNationality))+geom_histogram(aes(fill=PopByNationality),binwidth = 1,position="dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney",subtitle = "Nationality: English",y="No. of People",x="Suburb")



#Germany

# we first create a dataset for the specific nationality
ger<-filter(test.b,nationality=="germany")

#View the result of the dataset in a spreadsheet format 
View(ger)

# Gather the age band group "24-26","27-29" and "30-32" columns into one 
ger <- ger %>%
  gather("24-26","27-29","30-32",key="age_band",value="band_pop")

# Geom Jitter of age band group 
ggplot(ger,aes(y=suburb,x=band_pop))+geom_jitter(aes(colour=age_band))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Germany",y="No. of People",x="Suburb")

# Geom bar of age band group 
ggplot(ger,aes(x=reorder(suburb,band_pop),y=band_pop))+geom_histogram(aes(fill=age_band),stat = "identity",position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Germany",y="No. of People",x="Suburb")+facet_wrap(~age_band)

str(ger)

#Histogram
ggplot(ger,aes(x=reorder(suburb,PopByNationality),y=PopByNationality))+geom_histogram(aes(fill=PopByNationality),binwidth = 1,position="dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney",subtitle = "Nationality: Germany",y="No. of People",x="Suburb")



#Greece

# we first create a dataset for the specific nationality
greece<-filter(test.b,nationality=="greece")

#View the result of the dataset in a spreadsheet format 
View(greece)

# Gather the age band group "24-26","27-29" and "30-32" columns into one 
greece <- greece %>%
  gather("24-26","27-29","30-32",key="age_band",value="band_pop")

# Geom Jitter of age band group 
ggplot(greece,aes(y=suburb,x=band_pop))+geom_jitter(aes(colour=age_band))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Greece",y="No. of People",x="Suburb")

# Geom bar of age band group 
ggplot(greece,aes(x=reorder(suburb,band_pop),y=band_pop))+geom_histogram(aes(fill=age_band),stat = "identity",position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Greece",y="No. of People",x="Suburb")+facet_wrap(~age_band)

str(greece)

#Histogram
ggplot(greece,aes(x=reorder(suburb,PopByNationality),y=PopByNationality))+geom_histogram(aes(fill=PopByNationality),binwidth = 1,position="dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney",subtitle = "Nationality: Greece",y="No. of People",x="Suburb")



#India

# we first create a dataset for the specific nationality
ind<-filter(test.b,nationality=="indian")

#View the result of the dataset in a spreadsheet format 
View(ind)

# Gather the age band group "24-26","27-29" and "30-32" columns into one 
ind <- ind %>%
  gather("24-26","27-29","30-32",key="age_band",value="band_pop")

# Geom Jitter of age band group 
ggplot(ind,aes(y=suburb,x=band_pop))+geom_jitter(aes(colour=age_band))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Indian",y="No. of People",x="Suburb")

# Geom bar of age band group 
ggplot(ind,aes(x=reorder(suburb,band_pop),y=band_pop))+geom_histogram(aes(fill=age_band),stat = "identity",position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: India",y="No. of People",x="Suburb")+facet_wrap(~age_band)

str(ind)

#Histogram
ggplot(ind,aes(x=reorder(suburb,PopByNationality),y=PopByNationality))+geom_histogram(aes(fill=PopByNationality),binwidth = 1,position="dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney",subtitle = "Nationality: Indian",y="No. of People",x="Suburb")



#Ireland

# we first create a dataset for the specific nationality
ire<-filter(test.b,nationality=="ireland")

#View the result of the dataset in a spreadsheet format 
View(ire)

# Gather the age band group "24-26","27-29" and "30-32" columns into one 
ire <- ire %>%
  gather("24-26","27-29","30-32",key="age_band",value="band_pop")

# Geom Jitter of age band group 
ggplot(ire,aes(y=suburb,x=band_pop))+geom_jitter(aes(colour=age_band))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Ireland",y="No. of People",x="Suburb")

# Geom bar of age band group 
ggplot(ire,aes(x=reorder(suburb,band_pop),y=band_pop))+geom_histogram(aes(fill=age_band),stat = "identity",position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Ireland",y="No. of People",x="Suburb")+facet_wrap(~age_band)


str(ire)

#Histogram
ggplot(ire,aes(x=reorder(suburb,PopByNationality),y=PopByNationality))+geom_histogram(aes(fill=PopByNationality),binwidth = 1,position="dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney",subtitle = "Nationality: Ireland",y="No. of People",x="Suburb")


#Italy

# we first create a dataset for the specific nationality
ita<-filter(test.b,nationality=="italy")

#View the result of the dataset in a spreadsheet format 
View(ita)

# Gather the age band group "24-26","27-29" and "30-32" columns into one 
ita <- ita %>%
  gather("24-26","27-29","30-32",key="age_band",value="band_pop")

# Geom Jitter of age band group 
ggplot(ita,aes(y=suburb,x=band_pop))+geom_jitter(aes(colour=age_band))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Italy",y="No. of People",x="Suburb")

# Geom bar of age band group 
ggplot(ita,aes(x=reorder(suburb,band_pop),y=band_pop))+geom_histogram(aes(fill=age_band),stat = "identity",position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Italy",y="No. of People",x="Suburb")+facet_wrap(~age_band)

str(ita)

#Histogram
ggplot(ita,aes(x=reorder(suburb,PopByNationality),y=PopByNationality))+geom_histogram(aes(fill=PopByNationality),binwidth = 1,position="dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney",subtitle = "Nationality: Italy",y="No. of People",x="Suburb")



#NZ

# we first create a dataset for the specific nationality
nz<-filter(test.b,nationality=="nz")

#View the result of the dataset in a spreadsheet format 
View(nz)

# Gather the age band group "24-26","27-29" and "30-32" columns into one 
nz <- nz %>%
  gather("24-26","27-29","30-32",key="age_band",value="band_pop")

# Geom Jitter of age band group 
ggplot(nz,aes(y=suburb,x=band_pop))+geom_jitter(aes(colour=age_band))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: New Zealand",y="No. of People",x="Suburb")

# Geom bar of age band group 
ggplot(nz,aes(x=reorder(suburb,band_pop),y=band_pop))+geom_histogram(aes(fill=age_band),stat = "identity",position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: New Zealand",y="No. of People",x="Suburb")+facet_wrap(~age_band)

str(nz)

#Histogram
ggplot(nz,aes(x=reorder(suburb,PopByNationality),y=PopByNationality))+geom_histogram(aes(fill=PopByNationality),binwidth = 1,position="dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney",subtitle = "Nationality: New Zealand",y="No. of People",x="Suburb")



#Philippines


# we first create a dataset for the specific nationality
phi<-filter(test.b,nationality=="philippines")

#View the result of the dataset in a spreadsheet format 
View(phi)

# Gather the age band group "24-26","27-29" and "30-32" columns into one 
phi <-phi %>%
  gather("24-26","27-29","30-32",key="age_band",value="band_pop")

# Geom Jitter of age band group 
ggplot(phi,aes(y=suburb,x=band_pop))+geom_jitter(aes(colour=age_band))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Philippines",y="No. of People",x="Suburb")

# Geom bar of age band group 
ggplot(phi,aes(x=reorder(suburb,band_pop),y=band_pop))+geom_histogram(aes(fill=age_band),stat = "identity",position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Philippines",y="No. of People",x="Suburb")+facet_wrap(~age_band)

str(nz)

#Histogram
ggplot(phi,aes(x=reorder(suburb,PopByNationality),y=PopByNationality))+geom_histogram(aes(fill=PopByNationality),binwidth = 1,position="dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney",subtitle = "Nationality: Philippines ",y="No. of People",x="Suburb")


#Poland

# we first create a dataset for the specific nationality
pol<-filter(test.b,nationality=="poland")

#View the result of the dataset in a spreadsheet format 
View(pol)

# Gather the age band group "24-26","27-29" and "30-32" columns into one 
pol <-pol %>%
  gather("24-26","27-29","30-32",key="age_band",value="band_pop")

# Geom Jitter of age band group 
ggplot(pol,aes(y=suburb,x=band_pop))+geom_jitter(aes(colour=age_band))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Poland",y="No. of People",x="Suburb")

# Geom bar of age band group 
ggplot(pol,aes(x=reorder(suburb,band_pop),y=band_pop))+geom_histogram(aes(fill=age_band),stat = "identity",position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Poland",y="No. of People",x="Suburb")+facet_wrap(~age_band)

str(pol)

#Histogram
ggplot(pol,aes(x=reorder(suburb,PopByNationality),y=PopByNationality))+geom_histogram(aes(fill=PopByNationality),binwidth = 1,position="dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney",subtitle = "Nationality: Poland ",y="No. of People",x="Suburb")



#Scotland

# we first create a dataset for the specific nationality
scot<-filter(test.b,nationality=="scotland")

#View the result of the dataset in a spreadsheet format 
View(scot)

# Gather the age band group "24-26","27-29" and "30-32" columns into one 
scot <-scot %>%
  gather("24-26","27-29","30-32",key="age_band",value="band_pop")

# Geom Jitter of age band group 
ggplot(scot,aes(y=suburb,x=band_pop))+geom_jitter(aes(colour=age_band))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Scotland",y="No. of People",x="Suburb")

# Geom bar of age band group 
ggplot(scot,aes(x=reorder(suburb,band_pop),y=band_pop))+geom_histogram(aes(fill=age_band),stat = "identity",position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Scotland",y="No. of People",x="Suburb")+facet_wrap(~age_band)

str(scot)

#Histogram
ggplot(scot,aes(x=reorder(suburb,PopByNationality),y=PopByNationality))+geom_histogram(aes(fill=PopByNationality),binwidth = 1,position="dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney",subtitle = "Nationality: Scotland ",y="No. of People",x="Suburb")



#Wales

# we first create a dataset for the specific nationality
wales<-filter(test.b,nationality=="Wales")

#View the result of the dataset in a spreadsheet format 
View(wales)

# Gather the age band group "24-26","27-29" and "30-32" columns into one 
wales <-wales %>%
  gather("24-26","27-29","30-32",key="age_band",value="band_pop")

# Geom Jitter of age band group 
ggplot(wales,aes(y=suburb,x=band_pop))+geom_jitter(aes(colour=age_band))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Wales",y="No. of People",x="Suburb")

# Geom bar of age band group 
ggplot(wales,aes(x=reorder(suburb,band_pop),y=band_pop))+geom_histogram(aes(fill=age_band),stat = "identity",position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Wales",y="No. of People",x="Suburb")+facet_wrap(~age_band)

str(wales)

#Histogram
ggplot(wales,aes(x=reorder(suburb,PopByNationality),y=PopByNationality))+geom_histogram(aes(fill=PopByNationality),binwidth = 1,position="dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney",subtitle = "Nationality: Wales ",y="No. of People",x="Suburb")



#USA

# we first create a dataset for the specific nationality
usa<-filter(test.b,nationality=="usa")

#View the result of the dataset in a spreadsheet format 
View(usa)

# Gather the age band group "24-26","27-29" and "30-32" columns into one 
usa <-usa %>%
  gather("24-26","27-29","30-32",key="age_band",value="band_pop")

# Geom Jitter of age band group 
ggplot(usa,aes(y=suburb,x=band_pop))+geom_jitter(aes(colour=age_band))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: Philippines",y="No. of People",x="Suburb")

# Geom bar of age band group 
ggplot(usa,aes(x=reorder(suburb,band_pop),y=band_pop))+geom_histogram(aes(fill=age_band),stat = "identity",position = "dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney by age band",
       subtitle = "Nationality: United States",y="No. of People",x="Suburb")+facet_wrap(~age_band)

str(usa)

#Histogram
ggplot(usa,aes(x=reorder(suburb,PopByNationality),y=PopByNationality))+geom_histogram(aes(fill=PopByNationality),binwidth = 1,position="dodge",stat = "identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))+coord_flip()+
  labs(title = "Analysis of young female population in Sydney",subtitle = "Nationality: United States ",y="No. of People",x="Suburb")


##### THE END #####


#Packages to load
library(sp) # Spatial data Mapping: polygons, line, points and grids
library(rgdal) # because it has the readOGR () function
library(tmap) #for static and interactive maps
library(ggmap)
library(spdep)
library(RColorBrewer)

### Mapping many models

# Build a linear model for each country
gap_models <- testb_nested %>%
  mutate(model = map(.x=data, ~lm(formula = pop_by_enthicity~pop, data = .x)))

# Extract the model for Algeria    
algeria_model <- gap_models$model[[1]]

# View the summary for the Algeria model
summary(algeria_model)



testa <- select(fempop2,24-26:)
testa <-select(fpop,`24-26`:SSC_Codes)
View(testa)
####
names(fempop2)
ggplot(fempop2,aes())


# SSC spatial mapping polygon data
Output.Areas<- readOGR(".","SSC_2016_AUST")
head(Output.Areas)
str(Output.Areas)
names(Output.Areas)
plot(Output.Areas)
qtm(Output.Areas)

#list the SSC names and the SSC codes
write.table(Output.Areas@data, file = "jay_ssc.csv", sep=",", row.names = F)
#This will detail class for Output.Areas as SpatialPolygonsDataFrame with 15286 features 
Output.Areas

# The @ will give you access to the data of the SpatialPolygonsDataFrame
Output.Areas@proj4string

# SSC code level:
# NOTE: all.x= FALSE (means Inner Join, that is accept information only that matches with two
# of the datasets, if not then exclude the variables)
OA.nsw16 <- merge(Output.Areas, fempop2, by.x="SSC_CODE16", by.y="SSC_codes",all.x=FALSE)

OA.nsw16A <-inner_join(Output.Areas@data,fempop2,by=c("SSC_CODE16"="SSC_codes"))
View(OA.nsw16A)
summary(OA.nsw16A)
names(OA.nsw16)
names(OA.nsw16A)

?inner_join
?inner_join.sf

#####TIDYVERSE way in dplyr's join() function
Output.Areas@data <-as.character(Output.Areas@data["lga_code"])

plot(Output.Areas)


names(OA.nsw16)
View(OA.nsw16)
View(OA.nsw16@data)
# As inner join has solved the problem
proj4string(Output.Areas) <- CRS("+init=EPSG:3308") #EPSG:4283 came from the AURIN website


# This shows Rental assistance on "Quantile" style for NSW
tm_shape(OA.nsw16) + tm_fill("F_Australia_prop",style = "quantile",title="Australian Females") 



# Total Females Never married 2016
tm_shape(OA.nsw16) + tm_fill("F_Tot_Never_Married") + tm_borders(alpha=.4)+ 
  tm_fill(palette = "Reds", style = "quantile",title = "Australian Females",text = "SSC_NAME16")

#Rental assistance from the Australian government 2016  GOOD but the suburbs will overlap!!!
tm_shape(OA.nsw16,line.center = "midpoint") + tm_fill("prop_F_Tot_Never_Married")+ tm_borders(alpha=.4)+
  tm_fill("prop_F_Tot_Never_Married")+tm_fill(palette = "Reds", style = "quantile",
                                        title = "prop_F_Tot_Never_Married")+
  tm_text(text = "SSC_NAME16",size = 0.18)

#Female Population 2016  GOOD no suburb names
tm_shape(OA.nsw16) + tm_borders(alpha=.4)+
  tm_shape(OA.nsw16)+tm_fill("F_Tot_Never_Married",palette = "Reds", style = "quantile",
                             title = "Female total never married")

#Rental assistance from the Australian government 2016 plus Mortgage stress  Good!!!
tm_shape(OA.nsw16) + tm_borders(alpha=.4)+
  tm_shape(OA.nsw16)+tm_fill("RentAssistAustGovt",palette = "Oranges", style = "quantile",
                             title = "Rental Assistance from Govt")+
  tm_dots(col="MrtgStressPrvteDwl", palette = "Greens",size=0.1,
          style = "quantile", title="Mortgage Stress")

#Low income households Private Dwelling 2016 plus Low income Stress Mort/Rent  Good!!!
tm_shape(OA.nsw16) + tm_borders(alpha=.5)+
  tm_shape(OA.nsw16)+tm_fill("LowIncHsldsPrvteDwllg",palette = "Reds", style = "quantile",
                             title = "Low Income Household Private Dwellings")+
  tm_dots(col="LowIncfinStressMortRent", palette = "Blues",size=0.1,
          style = "quantile", title="Low Income Rent/Mortgage Stress")


#,legend.hist=T in tm_fill or dots
# style = "fixed",breaks=c(0,20000,40000,60000), title="Mortgage Stress",)

# tm_shape(OA.Census) + tm_fill("Qualification", palette = "Reds", style = "quantile", title = "% with a Qualification") + tm_borders(alpha=.4)
tm_shape(OA.nsw16) + tm_borders(alpha=.4) +
  tm_shape(OA.nsw16) + tm_dots(col = "RentAssistAustGovt", palette = "Reds", size="RentAssistAustGovt",
                               style = "quantile", title="Rental Assist from Government")


#Mortgage Stress Private Dwellings 2016  
tm_shape(OA.nsw16) + tm_fill("MrtgStressPrvteDwl")+ tm_borders(alpha=.4)+
  +tm_fill(palette = "Reds", style = "quantile", title = "Mortgage Stress", text="lga_name.x", size=2.8)+
  tm_layout(title="Mortgage Stress",)

View(OA.nsw16@data)



test <- OA.nsw16
OA.nsw16@data <-na.omit(OA.nsw16@data)
View(OA.nsw16@polygons)
names(Output.Areas)
library(RColorBrewer)
display.brewer.all()


library(spdep)
# The function builds a neighbours list based on regions with contiguous boundaries,
# that is sharing one or more boundary point. 
# queen = F means a single shared boundary meets the contiguity (bordering or in contact)
# condition. FALSE means more than one shared point is required but does not 
# mean shared boundary line
neighbours <- poly2nb(OA.nsw16, queen = FALSE)

neighbours
#plotting the networks
plot(OA.nsw16, border = 'lightgrey')
plot(neighbours, coordinates(OA.nsw16), add=TRUE, col='red')


?poly2nb
listw <-nb2listw(neighbours,zero.policy = T)
print(listw,zero.policy=T)
?nb2listw # Spatial Weights for neighbours lists

moran.test(OA.nsw16$RentAssistAustGovt, listw,zero.policy = T)

moran <- moran.plot(OA.nsw16$RentAssistAustGovt, listw = nb2listw(neighbours, style = "W",zero.policy = T))

local <- localmoran(x = OA.nsw16$RentAssistAustGovt, listw = nb2listw(neighbours, style = "W",zero.policy = T))
head(local)

# binds results to our polygon shapefile
moran.map <- cbind(OA.nsw16, local)
# maps the results
tm_shape(moran.map) + tm_fill(col = "Ii", style = "quantile", title = "local moran statistic")

names(moran.map@data)

# maps the p-values
tm_shape(moran.map) + tm_fill(col = "Pr.z...0.", style = "fixed",
                              breaks=c(0.001,0.01,0.05,0.1,0.2,1), title = "p-values")+tm_borders(alpha=0.4)+tm_text(text = "lga_name.x",size = 0.28)


# Geographically Weighted Regression
# Estimate of a relationship Analysis of independent predictor variables (x axis)
# for a response variable (y-axis) 
lm.model <- lm(OA.nsw16$RentAssistAustGovt ~ OA.nsw16$LowIncfinStressMortRent+
                 OA.nsw16$LowIncHsldsPrvteDwllg+ OA.nsw16$LowIncfinStressMortRent+
                 OA.nsw16$medianAge)
summary(lm.model)
?par
#Graphical Parameters matrix frame rows 2x2
par(mfrow=c(2,2))
plot(lm.model)
resids<-residuals(lm.model)
map.resids <- cbind(OA.nsw16, resids) 
names(map.resids)
# we need to rename the column header from the resids file - in this case its the 9th column of map.resids
names(map.resids)[12] <- "resids"
# maps the residuals using the quickmap function from tmap
qtm(map.resids, fill = "resids")

#residuals mapping with names
tm_shape(map.resids) + tm_fill("resids")+tm_text(text = "lga_name.x", size=0.28)+tm_borders(alpha = 0.5) 
# Residuals without names
tm_shape(map.resids) + tm_fill("resids") 

library("spgwr")
example("gwr")

#calculate kernel bandwidth
GWRbandwidth <- gwr.sel(OA.nsw16$RentAssistAustGovt ~ OA.nsw16$LowIncfinStressMortRent+
                          OA.nsw16$LowIncHsldsPrvteDwllg+ OA.nsw16$LowIncfinStressMortRent+
                          OA.nsw16$medianAge,data=OA.nsw16,adapt=T)


#fit the gwr model (note it has the same formula as before)
gwr.model = gwr(OA.nsw16$RentAssistAustGovt ~ OA.nsw16$LowIncfinStressMortRent+
                  OA.nsw16$LowIncHsldsPrvteDwllg+ OA.nsw16$LowIncfinStressMortRent+
                  OA.nsw16$medianAge,data=OA.nsw16, adapt=GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE) 
#OK. What did it do?
gwr.model

names(lm.model)

names(gwr.model)

lm.model$coefficients

results <-as.data.frame(gwr.model$SDF)
results
#SDF means Spatial Points Data Frame (SDF)
gwr.model$SDF@data

#thats crap ( a long time to load) NO!!!!
spplot(gwr.model$SDF)

gwr.map <- cbind(OA.nsw16, as.matrix(results))

# R2 the measure of model fit
tm_shape(gwr.map) + tm_fill("localR2")

plot(OA.nsw16, border = 'lightgrey')
plot(neighbours, coordinates(OA.nsw16), add=TRUE, col='red')

#A contour plot
plot(OA.nsw16,border='lightgrey')
contour(gwr.model$SDF,'PROF',lwd=3,add=TRUE)
plot(londonhp,add=TRUE,pch=16,col=adjustcolor('blueviolet',alpha.f=0.4))

# creates a coloured dot map
# Point Pattern Analysis

plot(coordinates(OA.nsw16))
OA.nsw16
House.Points <-SpatialPointsDataFrame(houses[,8:9], houses, proj4string = CRS("+init=EPSG:4283"))
House.Points <-SpatialPointsDataFrame(coordinates(OA.nsw16),match.ID = F, data=OA.nsw16, proj4string = CRS("+init=EPSG:4283"))
House.Points
?SpatialPointsDataFrame
head(OA.nsw16@polygons,1)
OA.nsw16@polygons$polygon@labpt[1:2]
coordinates(OA.nsw16)

# Spatial attribute Analysis (because using LGA)
# tm_shape(OA.Census) + tm_fill("Qualification", palette = "Reds", style = "quantile", title = "% with a Qualification") + tm_borders(alpha=.4)
tm_shape(OA.nsw16) + tm_borders(alpha=.4) +
  tm_shape(OA.nsw16) + tm_dots(col = "RentAssistAustGovt", palette = "Reds", size="RentAssistAustGovt",
                               style = "quantile", title="Rental Assist from Government")+labs(title="Neighbours list sharing common boundaries")
?tm_dots()

# save the dataset to a file
write.csv(rent,file="rental stress nsw 2016.csv")

