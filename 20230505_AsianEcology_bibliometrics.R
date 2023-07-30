###### AsianEcology: Bibliometric Analysis
###### Author: Masahiro Ryo
###### Date: 05.05.2023 

###### Code is adopted by this tutorial: https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html

################################################################################
### LIBRARIES
################################################################################
# load libraries
library(tidyverse)
library(bibliometrix)
library(Matrix)
library(igraph)
library(ggpubr)


################################################################################
### MANUAL TERM MANIPULATION
################################################################################
#-------------------------------------------------------------------------
# create a regular expression for Asian country names

asian_regex <- "^(AFGANISTAN|ARMENIA|AZERBAIJAN|BAHRAIN|BANGLADESH|BHUTAN|BRUNEI|CAMBODIA|CHINA|
CYPRUS|EAST TIMOR|GEORGIA|INDIA|INDONESIA|IRAN|IRAQ|ISRAEL|JAPAN|JORDAN|KAZAKHSTAN|KUWAIT|KYRGYZSTAN|
LAOS|LEBANON|MALAYSIA|MALDIVES|MONGOLIA|MYANMAR|NEPAL|KOREA|OMAN|PAKISTAN|PHILIPPINES|QATAR|RUSSIA|
SAUDI ARABIA|SINGAPORE|SRI LANKA|SYRIA|TAIWAN|TAJIKISTAN|THAILAND|TURKEY|TURKMENISTAN|
U ARAB EMIRATES|UZBEKISTAN|VIETNAM|YEMEN)$"

#-------------------------------------------------------------------------
# synonym

synonym_list <- c( 
  "biodiversity;diversity;biobiodiversity", 
  "communities;community",
  "convolutional neural networks;convolutional neural network",
  "ecosystems;ecosystem",
  "forests;forest",
  "impacts;impact",
  "models;model", 
  "neural network;networks;neural-networks;neural-network;neural networks;artificial neural network;artificial neural-network;artificial neural-networks;artificial neural",
  "plant;plants",
  "population;populations",
  "pattern;patterns",
  "random forest;random forests",
  "response;responses",
  "system;systems", 
  "scale;scales", 
  "species distribution;distribution;distributions;potential distribution;species distributions",
  "species distribution model;species distribution models;distribution models",
  "tree;trees",
  "vegetation;vegetations"
)

#-------------------------------------------------------------------------
# remove.terms

remove_list <- c("area",
                 "ecology",
                 "environment",
                 "information",
                 "learning",
                 "machine",
                 "method",
                 "model",
                 "models",
                 "modelling",
                 "modeling",
                 "random",
                 "system",
                 "simulation",
                 "variables"
)
#-------------------------------------------------------------------------


################################################################################
### DATA
################################################################################
# load in data
getwd()
file1 <- "../data/biblio0001_1000.bib"
file2 <- "../data/biblio1001_1550.bib"

M_tmp1 <- convert2df(file = file1, dbsource = "wos", format = "bibtex")
M_tmp2 <- convert2df(file = file2, dbsource = "wos", format = "bibtex")

M <- bind_rows(M_tmp1,M_tmp2)


# Extracting the country names of the corresponding authors' addresses
CO <- metaTagExtraction(M,Field="AU1_CO", ";")$AU1_CO


# use grep() with the regular expression to extract the Asian country names
Asia <- unique(CO)[grep(asian_regex, unique(CO))]

M_Asia <- M[which(CO%in%Asia),]
M_nonAsia <- setdiff(M, M_Asia)

rm(M_tmp1, M_tmp2)

################################################################################
### OVERVIEW TRENDS 
################################################################################
summary_nonAsia <-M_nonAsia %>% biblioAnalysis() %>% summary(k=10) 
summary_Asia <-M_Asia %>% biblioAnalysis() %>% summary(k=30)

plot(M_nonAsia %>% biblioAnalysis())
plot(M_Asia %>% biblioAnalysis())

################################################################################
### KEYWORD TRENDS ANALYSIS
################################################################################
# function
keyword_trend <- function(M,n.items=5, field="AU", min.freq = 1, timespan=c(2021,2030)){
fieldByYear(M,
            field=field,
            n.items=n.items,
            synonyms=synonym_list,
            remove.terms = remove_list,
            timespan=timespan)
}


# WoS-defined keywords: both emerging and historical
trends_nonAsia_from2020 <-M_nonAsia %>% keyword_trend(field="ID", n.items=5,timespan=c(2020,2022))
trends_nonAsia_from2001 <-M_nonAsia %>% keyword_trend(field="ID", n.items=1,timespan=c(2001,2022))
trends_Asia_from2020 <-M_Asia %>% keyword_trend(field="ID", n.items=5,timespan=c(2020,2022))
trends_Asia_from2001 <-M_Asia %>% keyword_trend(field="ID", n.items=1,timespan=c(2001,2022))

# Author-defined keywords: both emerging and historical
trends_nonAsia_from2020 <-M_nonAsia %>% keyword_trend(field="DE", n.items=5,timespan=c(2020,2022))
trends_nonAsia_from2001 <-M_nonAsia %>% keyword_trend(field="DE", n.items=1,timespan=c(2001,2022))
trends_Asia_from2020 <-M_Asia %>% keyword_trend(field="DE", n.items=5,timespan=c(2020,2022))
trends_Asia_from2001 <-M_Asia %>% keyword_trend(field="DE", n.items=1,timespan=c(2001,2022))

################################################################################
#  KEYWORD TRENDS ANALYSIS (BUT UNUSED IN THE END)
################################################################################
# field = "DE"; synonyms=synonym_list; remove.terms = remove_list
# A <- cocMatrix(M, Field = field, binary = FALSE, type="matrix",remove.terms = remove.terms, synonyms = synonyms)
# tmp <- A %>% as.data.frame %>%  mutate(Year = M$PY)
# tmp2 <- 
#   tmp %>% 
#   drop_na %>% 
#   select(-V1) %>% 
#   group_by(Year) %>% 
#   summarize(across(everything(), sum, na.rm = T))
# 
# tmp3 <-
#   tmp2 %>% 
#   pivot_longer(cols = -Year, names_to = "Column", values_to = "Value") %>%
#   group_by(Year) %>% 
#   filter(Value == max(Value)) %>% 
#   slice_head(n = 1) %>% 
#   select(Year, MaxColumn = Column)


################################################################################
### KEYWORD COOCCURRENCE OVER TIME
################################################################################
# function
Keyword_cooccurrence = function(M,com.rep=0.1){
  windowsFonts("Arial" = windowsFont("Arial"))
  set.seed(1)
  M %>% 
    biblioNetwork(., 
                  analysis = "co-occurrences", 
                  network = "author_keywords",  # it becomes much more fragmented.
#                  network = "keywords",
                  sep = ";",
                  synonyms = synonym_list,
                 remove.terms = remove_list) %>% 
    networkPlot(.,  
                normalize = "jaccard",
                weighted=T, 
                n = 40,
                cluster="louvain",
                Title = "Keyword Co-occurrences", 
                community.repulsion = com.rep,
                remove.isolates = T, 
                remove.multiple = T,
                type = "fruchterman", 
                size=T, 
                labelsize=1.5,
                label.cex = T,
                curved = 0.2,
                alpha=0.3,
                verbose = F) %>% 
    .$graph %>% 
    plot(.,  # with igraph for more flexibility
         vertex.label.color = rgb(0, 0, 0, alpha = 0.8),
         vertex.label.family = "Arial",
         vertex.frame.color="NA") 
}


# Asia
# keyword_Asia    <- M_Asia %>% Keyword_cooccurrence()
# keyword_Asia_upto2010    <- M_Asia %>% filter(PY>=1000,PY<2011)  %>% Keyword_cooccurrence(com.rep=0.0)
# keyword_Asia_upto2015    <- M_Asia %>% filter(PY>=1000,PY<2016)  %>% Keyword_cooccurrence(com.rep=0.0)
# keyword_Asia_upto2018    <- M_Asia %>% filter(PY>=1000,PY<2019)  %>% Keyword_cooccurrence(com.rep=0.0)
keyword_Asia_upto2020    <- M_Asia %>% filter(PY>=2000,PY<2020)  %>% Keyword_cooccurrence(com.rep=0.05)
keyword_Asia_from2020    <- M_Asia %>% filter(PY>=2020,PY<2024)  %>% Keyword_cooccurrence(com.rep=0.05)

# nonAsia
# keyword_nonAsia <- M_nonAsia %>% Keyword_cooccurrence()
# keyword_nonAsia_upto2010    <- M_nonAsia %>% filter(PY>=1000,PY<2011)  %>% Keyword_cooccurrence(com.rep=0.1)
# keyword_nonAsia_upto2015    <- M_nonAsia %>% filter(PY>=1000,PY<2016)  %>% Keyword_cooccurrence(com.rep=0.15)
# keyword_nonAsia_upto2018    <- M_nonAsia %>% filter(PY>=1000,PY<2019)  %>% Keyword_cooccurrence(com.rep=0.15)
keyword_nonAsia_upto2020    <- M_nonAsia %>% filter(PY>=2000,PY<2020)  %>% Keyword_cooccurrence(com.rep=0.15)
keyword_nonAsia_from2020    <- M_nonAsia %>% filter(PY>=2020,PY<2024)  %>% Keyword_cooccurrence(com.rep=0.15)



################################################################################
### COUNTRY COLLABORATION OVER TIME
################################################################################
# function
Collab = function(M,com.rep=0.0){
  windowsFonts("Arial" = windowsFont("Arial"))
  set.seed(1)
  M %>% 
    metaTagExtraction(Field = "AU_CO", sep = ";") %>%
    biblioNetwork(analysis = "collaboration", network = "countries", sep = ";") %>%
    networkPlot(., 
                normalize = "jaccard",
                weighted=T, 
                n = dim(.)[1],
                Title = "Country Collaboration", 
                size=TRUE, 
                remove.multiple=T,
                remove.isolates=T, 
                type = "fruchterman", 
                community.repulsion = com.rep,
                labelsize=1,
                label.cex = T,
                curved = 0.2,
                alpha = 0.3,
                verbose = F) %>% 
    .$graph %>% 
    plot(.,  # with igraph for more flexibility
         vertex.label.color = rgb(0, 0, 0, alpha = 0.8),
         vertex.label.family = "Arial",
         vertex.frame.color="NA"
         ) 
}

# Asia
collab_Asia    <- M_Asia %>% Collab()
collab_Asia_upto2010    <- M_Asia %>% filter(PY>=1000,PY<2011)  %>% Collab(com.rep=0.0)
collab_Asia_upto2015    <- M_Asia %>% filter(PY>=1000,PY<2016)  %>% Collab(com.rep=0.0)
collab_Asia_upto2018    <- M_Asia %>% filter(PY>=1000,PY<2019)  %>% Collab(com.rep=0.0)
collab_Asia_upto2023    <- M_Asia %>% filter(PY>=1000,PY<2024)  %>% Collab(com.rep=0.0)
collab_Asia_from2021    <- M_Asia %>% filter(PY>=2021,PY<2024)  %>% Collab(com.rep=0.0)

# nonAsia
collab_nonAsia <- M_nonAsia %>% Collab()
collab_nonAsia_upto2010    <- M_nonAsia %>% filter(PY>=1000,PY<2011)  %>% Collab(com.rep=0.0)
collab_nonAsia_upto2015    <- M_nonAsia %>% filter(PY>=1000,PY<2016)  %>% Collab(com.rep=0)
collab_nonAsia_upto2018    <- M_nonAsia %>% filter(PY>=1000,PY<2019)  %>% Collab(com.rep=0)
collab_nonAsia_upto2023    <- M_nonAsia %>% filter(PY>=1000,PY<2024)  %>% Collab(com.rep=0.0)
collab_nonAsia_from2021    <- M_nonAsia %>% filter(PY>=2022,PY<2024)  %>% Collab(com.rep=0.0)




