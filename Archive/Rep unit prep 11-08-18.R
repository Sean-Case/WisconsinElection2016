# setwd("https://github.com/Sean-Case/WisconsinElection2016")
library(XML); require(rvest); require(stringr);require(xlsx);require(dplyr);require(plyr)
require(agricolae); require(ggplot2); library(gridExtra); library(tidyr); require(corrplot);
require(reshape2); require(janitor); require(nlme); require(car); require(effects); require(digest)
require(servr)

# Be careful that rstudio git integration does not work on files with spaces in them...
# http://stackoverflow.com/questions/34105129/using-git-in-r-studio-cannot-stage-modified-code-files

# Information on how to use Plotly can be found here: https://www.r-bloggers.com/plot-with-ggplot2-and-plotly-within-knitr-reports/

# Long debate fixed vs random http://stats.stackexchange.com/questions/120964/fixed-effect-vs-random-effect-when-all-possibilities-are-included-in-a-mixed-eff?rq=1

options(scipen=999) # Effectively disables scientific numbering
options(mc.cores = parallel::detectCores()) # This is used for the poll downloads

one.graph.width = 20;one.graph.height = 15

# Function to take rightmost digit
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

## Capitalising first letters of sentence
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# setwd("D:/DocumentsD/Perso/WI/All prep together")
setwd("C:/Users/s_cas/Dropbox/Perso/2016 voting election county results/Wisconsin/All prep together/All processed files")

# Reporting unit level data 2008 2012 and 2016 original ----------------------------------------------------
ward.2016 = read.csv(
  "wardwisconsin2016andrecount.csv",
  stringsAsFactors = F,
  header = T,
  strip.white = T
)
{ # Preparing basic data
  ward.2016 = subset(ward.2016, Total.Votes > 0)
  
  ward.2016 = clean_names(ward.2016)
  ward.2016$reporting_unit = toupper(ward.2016$reporting_unit)
  ward.2016$municipality_name = toupper(ward.2016$municipality_name)
  ward.2016$county_name = toupper(ward.2016$county_name)
  
  ward.2016$muni_county = paste(ward.2016$municipality_name, ward.2016$county_name)
  ward.2016$reporting_unit = paste(ward.2016$reporting_unit, ward.2016$county_name)
  
  ward.2016.look = with(ward.2016, data.frame(county_name, municipality_name,
                                              reporting_unit, muni_county))
  colnames(ward.2016.look) = c("county","municipality","reporting_unit",
                               "muni_county")

  ward.2016.v = ward.2016
  ward.2016.v$county_name = NULL
  ward.2016.v$municipality_name = NULL
  ward.2016.v$total_votes = NULL
  # ward.2016.v$reporting_unit = NULL
  
  ward.2016.r = subset(ward.2016.v, original_or_recount == "Recount")
  ward.2016.o = subset(ward.2016.v, original_or_recount == "Original")
  
  ward.2016.r$original_or_recount = NULL
  ward.2016.o$original_or_recount = NULL

    ward.2016.o.l <- ward.2016.o
  ward.2016.o.l$muni_county = NULL
  ward.2016.o.l$reporting_unit <- NULL
  ward.2016.o.l = gather(ward.2016.o.l, cand, votes.rec)
  ward.2016.o.l$reporting_unit <- rep(ward.2016.o$reporting_unit)
  # colnames(ward.2016.o.l) = c("reporting_unit", "cand", "votes.rec")
 
  ward.2016.o.l$cand.group = NA
  ward.2016.o.l$cand.group[grep("hillary",ward.2016.o.l$cand)] = "democrat"
  ward.2016.o.l$cand.group[grep("donald",ward.2016.o.l$cand)] = "republican"
  ward.2016.o.l$cand.group[grep("darrell",ward.2016.o.l$cand)] = "other"
  ward.2016.o.l$cand.group[grep("rocky",ward.2016.o.l$cand)] = "other"
  ward.2016.o.l$cand.group[grep("gary",ward.2016.o.l$cand)] = "other"
  ward.2016.o.l$cand.group[grep("monica",ward.2016.o.l$cand)] = "other"
  ward.2016.o.l$cand.group[grep("jill",ward.2016.o.l$cand)] = "other"
  
  # There were a bunch of write-in names for the Reporting unit level results
  ward.2016.o.l$cand.group[grep("cherunda",ward.2016.o.l$cand)] = "other"
  ward.2016.o.l$cand.group[grep("evan",ward.2016.o.l$cand)] = "other"
  ward.2016.o.l$cand.group[grep("michael_a",ward.2016.o.l$cand)] = "other"
  ward.2016.o.l$cand.group[grep("marshall",ward.2016.o.l$cand)] = "other"
  ward.2016.o.l$cand.group[grep("chris",ward.2016.o.l$cand)] = "other"
  ward.2016.o.l$cand.group[grep("laurence",ward.2016.o.l$cand)] = "other"
  ward.2016.o.l$cand.group[grep("hoefling",ward.2016.o.l$cand)] = "other"
  ward.2016.o.l$cand.group[grep("joseph",ward.2016.o.l$cand)] = "other"
  ward.2016.o.l$cand.group[grep("emidio",ward.2016.o.l$cand)] = "other"
  ward.2016.o.l$cand.group[grep("scatter",ward.2016.o.l$cand)] = "other"
  
  
    # Joining back county and municipality names
  ward.2016.o.l = plyr::join(ward.2016.o.l, ward.2016.look, by = "reporting_unit", match = "first")
  
  ward.2016.o.l$votes.rec = as.numeric(as.integer(ward.2016.o.l$votes.rec))
  # table(is.na(ward.2016.o.l$votes.rec))
  
  tot.votes.look.group = dplyr::group_by(ward.2016.o.l,reporting_unit)
  tot.votes.look = dplyr::summarise(tot.votes.look.group,
                                    votes.tot = sum(votes.rec,na.rm=T)
  )
  ward.2016.o.l = plyr::join(ward.2016.o.l,tot.votes.look,by="reporting_unit",match="first")
  ward.2016.o.l$votes.perc = (ward.2016.o.l$votes.rec / ward.2016.o.l$votes.tot) * 100
  
  ward.2016.o.l$muni_county = paste(ward.2016.o.l$municipality,
                                    ward.2016.o.l$county)
  
  ward.2016.o.repu.group = dplyr::group_by(ward.2016.o.l,reporting_unit, municipality,muni_county,county,cand.group)
  ward.2016.o.repu = dplyr::summarise(ward.2016.o.repu.group,
                                     votes.rec2 = sum(votes.rec),
                                     votes.tot2 = mean(votes.tot)
  )
  colnames(ward.2016.o.repu)[6] = "votes.rec"
  colnames(ward.2016.o.repu)[7] = "votes.tot"
  
  ward.2016.o.repu$votes.perc = (ward.2016.o.repu$votes.rec / ward.2016.o.repu$votes.tot) * 100
  
  ward.2016.o.repu = ward.2016.o.repu[!is.na(ward.2016.o.repu$cand.group),]
} # Preparing basic data 2016
{
## Now bringing in the registered voter data.
# ward.2016.regvote = read.csv(
#   "registeredvotersbywards_xlsx_19539MODnov16.csv",
#   stringsAsFactors = F,
#   header = T,
#   strip.white = T
# )
# {
#   ward.2016.regvote = clean_names(ward.2016.regvote)
#   ward.2016.regvote$ward = toupper(ward.2016.regvote$ward)
#   ward.2016.regvote$county = toupper(ward.2016.regvote$county)
#   ward.2016.regvote$muni = toupper(ward.2016.regvote$muni)
#   
#   ward.2016.regvote$county = gsub(" COUNTY","",ward.2016.regvote$county)
#   ward.2016.regvote$muni = gsub(" -.*","",ward.2016.regvote$muni)
#   ward.2016.regvote$muni_county = paste(ward.2016.regvote$muni, ward.2016.regvote$county)
#   
#   colnames(ward.2016.regvote)[2] = "municipality"
#   
#   ### Limiting to municipality level
#   ward.2016.regvote.repu.group = dplyr::group_by(ward.2016.regvote,reporting_unit,muni_county,municipality,county)
#   ward.2016.regvote.repu = dplyr::summarise(ward.2016.regvote.repu.group,
#                                            voter_count2 = sum(voter_count)
#   )
#   colnames(ward.2016.regvote.repu)[4] = "voters.tot"
#   
#   
#   
#   ###
#   # Seeing what is differnt according to muni county level
#   ###
#   voter.reg.repu = data.frame(unique(ward.2016.regvote.repu$muni_county))
#   votes.repu = data.frame(unique(ward.2016.o.l$muni_county))
#   colnames(voter.reg.repu) = "voter.reg.repu"
#   colnames(votes.repu) = "votes.repu"
#   
#   # Ugh, I need to change the name of these muni_counties to make sure I have a full match
#   setdiff(unique(voter.reg.repu$voter.reg.repu), unique(votes.repu$votes.repu))
#   setdiff(unique(votes.repu$votes.repu), unique(voter.reg.repu$voter.reg.repu))
#   
#   differences = setdiff(unique(votes.repu$votes.repu),unique(voter.reg.repu$voter.reg.repu))
#   voter.reg.diffs = subset(ward.2016.o.l, muni_county %in% differences)
#   
#   voter.reg.diffs.join = with(voter.reg.diffs, data.frame(municipality, county, muni_county, reporting_unit))
#   voter.reg.diffs.join = voter.reg.diffs.join[!duplicated(voter.reg.diffs.join),]
#   
#   ward.2016.regvote$ward = gsub(" -","",ward.2016.regvote$ward)
#   
#   ###
#   # Seeing what is different according to Reporting unit level
#   ###
#   matched.wards = ward.2016.o.l[match(ward.2016.regvote$ward, ward.2016.o.l$reporting_unit),]
#   matched.wards = matched.wards[!is.na(matched.wards),]
#   
#   
#   ###
#   # Municipalities with exceptions
#   ##
#   {
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF WRIGHTSTOWN WARD 4"] = "OUTAGAMIE"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 5"] = "ADAMS"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF NEW AUBURN WARD 2"] = "BARRON"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF NEW AUBURN WARD 3"] = "BARRON"
#     
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 1"] = "MANITOWOC"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 2"] = "MANITOWOC"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 3"] = "MANITOWOC"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 4"] = "MANITOWOC"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 5"] = "MANITOWOC"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 6"] = "MANITOWOC"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 8"] = "MANITOWOC"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 7"] = "CALUMET"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 31"] = "WINNEBAGO"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MENASHA WARD 16"] = "CALUMET"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MENASHA WARD 17"] = "CALUMET"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF EAU CLAIRE WARD 16"] = "CHIPPEWA"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF UNITY WARD 2"] = "CLARK"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF STANLEY WARD 5"] = "CLARK"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF RANDOLPH WARD 3"] = "COLUMBIA"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF DE SOTO WARD 2"] = "CRAWFORD"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF EDGERTON WARD 7"] = "DANE"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 1"] = "DODGE"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 2"] = "DODGE"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 3"] = "DODGE"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 4"] = "DODGE"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 5"] = "DODGE"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 6"] = "DODGE"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 7"] = "DODGE"
#     
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WAUPUN WARD 9"] = "FOND DU LAC"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WAUPUN WARD 10"] = "FOND DU LAC"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WAUPUN WARD 11"] = "FOND DU LAC"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WAUPUN WARD 12"] = "FOND DU LAC"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF BELLEVILLE WARD 3"] = "GREEN"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF BROOKLYN WARD 2"] = "GREEN"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF BLANCHARDVILLE WARD 2"] = "IOWA"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF LIVINGSTON WARD 2"] = "IOWA"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF MONTFORT WARD 2"] = "IOWA"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF MUSCODA WARD 3"] = "IOWA"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF CAMBRIDGE WARD 1"] = "JEFFERSON"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WHITEWATER WARD 10"] = "JEFFERSON"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WHITEWATER WARD 11"] = "JEFFERSON"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WHITEWATER WARD 12"] = "JEFFERSON"
#     
#     
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF HAZEL GREEN WARD 3"] = "LAFAYETTE"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF CUBA CITY WARD 5"] = "LAFAYETTE"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF BIRNAMWOOD WARD 2"] = "MARATHON"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF DORCHESTER WARD 2"] = "MARATHON"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF ABBOTSFORD WARD 1"] = "MARATHON"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF COLBY WARD 1"] = "MARATHON"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARSHFIELD WARD 12"] = "MARATHON"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARSHFIELD WARD 20"] = "MARATHON"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARSHFIELD WARD 21"] = "MARATHON"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARSHFIELD WARD 24"] = "MARATHON"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF NEW LONDON WARD 1"] = "OUTAGAMIE"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF NEW LONDON WARD 2"] = "OUTAGAMIE"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF BAYSIDE WARD 6"] = "OZAUKEE"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF NEWBURG WARD 3"] = "OZAUKEE"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF TURTLE LAKE WARD 2A"] = "POLK"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF MILLADORE WARD 2"] = "PORTAGE"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF BRODHEAD WARD 7"] = "ROCK"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF BRODHEAD WARD 8"] = "ROCK"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF SPRING VALLEY WARD 3"] = "ST. CROIX"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF RIVER FALLS WARD 1"] = "ST. CROIX"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF RIVER FALLS WARD 2"] = "ST. CROIX"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF RIVER FALLS WARD 3"] = "ST. CROIX"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF RIVER FALLS WARD 4"] = "ST. CROIX"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF CAZENOVIA WARD 2"] = "SAUK"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 4"] = "SAUK"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 8"] = "SAUK"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 10"] = "SAUK"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 5"] = "ADAMS"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 9"] = "ADAMS"
#     
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF PULASKI WARD 4"] = "SHAWANO"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARION WARD 4"] = "SHAWANO"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARION WARD 5"] = "SHAWANO"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARION WARD 6"] = "SHAWANO"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF VIOLA WARD 1"] = "VERNON"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF MUKWONAGO WARD 11"] = "WALWORTH"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF BURLINGTON WARD 9"] = "WALWORTH"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF BURLINGTON WARD 10"] = "WALWORTH"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF BERLIN WARD 7"] = "WAUSHARA"
#     
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF MUKWONAGO WARD 11"] = "WALWORTH"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF WRIGHTSTOWN WARD 1"] = "BROWN"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF WRIGHTSTOWN WARD 2"] = "BROWN"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF WRIGHTSTOWN WARD 3"] = "BROWN"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 12"] = "CALUMET"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 13"] = "CALUMET"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 14"] = "CALUMET"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 26"] = "CALUMET"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 44"] = "CALUMET"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 45"] = "CALUMET"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 46"] = "CALUMET"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 47"] = "CALUMET"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 31"] = "WINNEBAGO"
#     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 32"] = "WINNEBAGO"
#   }
#   ward.2016.regvote$muni_county = paste(ward.2016.regvote$municipality, ward.2016.regvote$county)
#   
#   setdiff(unique(votes.repu$votes.repu), unique( ward.2016.regvote$muni_county))
#   
#   ## Joining the reg.vote summarised municipalities back onto the voting municipalities
#   ward.2016.regvote.join = with(ward.2016.regvote, data.frame(municipality,muni_county))
#   colnames(ward.2016.regvote.join)[2] = "muni_county_regvote_agreed"
#   ward.2016.regvote.join = ward.2016.regvote.join[!duplicated(ward.2016.regvote.join$muni_county_regvote),]
#   #
#   # voter.reg.diffs.join = plyr::join(voter.reg.diffs.join,ward.2016.regvote.join,by = "municipality")
#   #
#   # back.to.votes.df.join = with(voter.reg.diffs.join, data.frame(muni_county,muni_county_regvote_agreed))
#   #
#   # ward.2016.o.l = plyr::join(ward.2016.o.l, back.to.votes.df.join, by = "muni_county")
#   #
#   # unique(ward.2016.o.l$muni_county_regvote_agreed)
#   #
#   # ward.2016.o.l$muni_county_regvote_agreed = as.character(ward.2016.o.l$muni_county_regvote_agre
#   
#   ward.2016.o.l$muni_county_regvote_agreed = as.character(ward.2016.o.l$muni_county)
#   ward.2016.o.l$muni_county_regvote_agreed = ifelse(is.na(ward.2016.o.l$muni_county_regvote_agreed),
#                                                     ward.2016.o.l$muni_county,ward.2016.o.l$muni_county_regvote_agreed)
#   ## Now renaming the regvote column for the final join
#   colnames(ward.2016.regvote)[6] = "muni_county_regvote_agreed"
#   
#   # Trying to match by muni_county level
#   ## Need to sum up the votes by county
#   ward.2016.regvote.votes.group = dplyr::group_by(ward.2016.regvote,muni_county_regvote_agreed)
#   ward.2016.regvote.votes.g = dplyr::summarise(ward.2016.regvote.votes.group,
#                                                voter_count2 = sum(voter_count)
#   )
#   colnames(ward.2016.regvote.votes.g)[2] = "voters.tot"
#   
#   # Trying to match by Reporting unit level
#   ward.2016.o.l = plyr::join(ward.2016.o.l, ward.2016.regvote.votes.g, by = "muni_county_regvote_agreed")
#   colnames(ward.2016.o.l)[11] = "voters.tot"
#   
#   setdiff(ward.2016.regvote.votes.g$muni_county_regvote_agreed,
#           ward.2016.o.l$muni_county_regvote_agreed)
#   
#   setdiff(ward.2016.o.l$muni_county_regvote_agreed,
#           ward.2016.regvote.votes.g$muni_county_regvote_agreed)
#   
#   ward.2016.o.l$turnout = with(ward.2016.o.l, votes.tot / voters.tot)
#   
#   ### Joining this back to ward.2016.o.repu
#   ward.2016.o.l.join = with(ward.2016.o.l,
#                             data.frame(
#                               muni_county,voters.tot
#                             ))
#   colnames(ward.2016.o.l.join)[1] = "muni_county"
#   ward.2016.o.l.join = ward.2016.o.l.join[!duplicated(ward.2016.o.l.join$muni_county),]
#   
#   ward.2016.o.repu = plyr::join(ward.2016.o.repu, ward.2016.o.l.join, by ="muni_county", match = "first")
#   
# } # Adding in registered vote data
} # Registered to vote data 2016 NOT USEFUL ANYMORE
{   # 2012 ward data
ward.12 = read.csv(
  "ward.2012.votes.csv",
  stringsAsFactors = F,
  header = T,
  strip.white = T
)

{ # Loading 2012 ward vote data
  
  ward.12 = clean_names(ward.12)
  
  colnames(ward.12)[1:3] = c("county","municipality_name","reporting_unit")
  
  ward.12$reporting_unit = toupper(ward.12$reporting_unit)
  ward.12$municipality_name = toupper(ward.12$municipality_name)
  ward.12$county = toupper(ward.12$county)
  
  # The following is fixing a problem I noticed later when trying to match with the 2016 voter df
  ward.12$municipality_name[grep("TOWN OF WINDSOR", ward.12$municipality_name)] = "VILLAGE OF WINDSOR"
  ward.12$reporting_unit = gsub("TOWN OF WINDSOR","VILLAGE OF WINDSOR", ward.12$reporting_unit)
  
  # The town in Maine became a village in 2015, and is now in marathon county
  # ward.12$county[grep("TOWN OF MAINE", ward.12$municipality_name)] = "MARATHON"
  ward.12$municipality_name[grepl("TOWN OF MAINE", ward.12$municipality_name) &
                              grepl("MARATHON", ward.12$county)
                            ] = "VILLAGE OF MAINE"
  # ward.12$reporting_unit = gsub("TOWN OF MAINE","VILLAGE OF MAINE", ward.12$reporting_unit)
  
  # the town of Menasha was retained only for wards (8-10, 11-13). The rest (1-7) became the village of
  # Fox Crossing, Winnebago
  ward.12$municipality_name[grepl("TOWN OF MENASHA WARDS 1, 2, 4, 7", ward.12$reporting_unit)
                            ] = "VILLAGE OF FOX CROSSING"
  ward.12$municipality_name[grepl("TOWN OF MENASHA WARDS 3, 5, 6", ward.12$reporting_unit)
                            ] = "VILLAGE OF FOX CROSSING"
  ward.12$reporting_unit[grepl("TOWN OF MENASHA WARDS 1, 2, 4, 7", ward.12$reporting_unit)
                         ] = "VILLAGE OF FOX CROSSING WARDS 1-2,4,7"
  ward.12$reporting_unit[grepl("TOWN OF MENASHA WARDS 3, 5, 6", ward.12$reporting_unit)
                         ] = "VILLAGE OF FOX CROSSING WARDS 3,5-6"
  
  ward.12$muni_county = paste(ward.12$municipality_name, ward.12$county)
  ward.12$reporting_unit = paste(ward.12$reporting_unit, ward.12$county)
  
  ward.12.look = with(ward.12, data.frame(county, municipality_name,
                                          reporting_unit, muni_county))
  colnames(ward.12.look) = c("county","municipality","reporting_unit",
                             "muni_county")
  
  ward.12.v = ward.12
  ward.12.v$county = NULL
  ward.12.v$municipality_name = NULL
  ward.12.v$total_votes = NULL
  # ward.12.v$reporting_unit = NULL
  ward.12.v$congressional = NULL
  ward.12.v$senatedistrict = NULL
  ward.12.v$assemblydistrict = NULL
  
  ward.12.l <- ward.12.v
  ward.12.l$muni_county = NULL
  ward.12.l$reporting_unit <- NULL
  ward.12.l = gather(ward.12.l, cand, votes.rec)
  ward.12.l$reporting_unit <- rep(ward.12.v$reporting_unit)
  # colnames(ward.12.l) = c("reporting_unit", "cand", "votes.rec")
  
  ward.12.l$cand.group = NA
  ward.12.l$cand.group[grep("obama",ward.12.l$cand)] = "democrat"
  ward.12.l$cand.group[grep("romney",ward.12.l$cand)] = "republican"
  ward.12.l$cand.group[grep("virgil",ward.12.l$cand)] = "other"
  ward.12.l$cand.group[grep("johnson",ward.12.l$cand)] = "other"
  ward.12.l$cand.group[grep("gloria",ward.12.l$cand)] = "other"
  ward.12.l$cand.group[grep("jerry",ward.12.l$cand)] = "other"
  ward.12.l$cand.group[grep("stein",ward.12.l$cand)] = "other"
  ward.12.l$cand.group[grep("rocky",ward.12.l$cand)] = "other"
  ward.12.l$cand.group[grep("roseanne",ward.12.l$cand)] = "other"
  ward.12.l$cand.group[grep("scatter",ward.12.l$cand)] = "other"
  
  ward.12.l$votes.rec = as.integer(ward.12.l$votes.rec)
  
  # Joining back county and municipality names
  ward.12.l = plyr::join(ward.12.l, ward.12.look, by = "reporting_unit", match = "first")
  
  tot.votes.look.group = dplyr::group_by(ward.12.l,reporting_unit)
  tot.votes.look = dplyr::summarise(tot.votes.look.group,
                                    votes.tot = sum(as.integer(votes.rec),na.rm=T)
  )
  
  ward.12.l = plyr::join(ward.12.l,tot.votes.look,by="reporting_unit")
  ward.12.l$votes.perc = (ward.12.l$votes.rec / ward.12.l$votes.tot) * 100
  
  ward.12.l$muni_county = paste(ward.12.l$municipality, ward.12.l$county)
  
  ward.12.repu.group = dplyr::group_by(ward.12.l,reporting_unit,municipality,muni_county,county,cand.group)
  ward.12.repu = dplyr::summarise(ward.12.repu.group,
                                 votes.rec2 = sum(votes.rec,na.rm=T),
                                 votes.tot2 = mean(votes.tot,na.rm=T)
  )
  colnames(ward.12.repu)[6] = "votes.rec"
  colnames(ward.12.repu)[7] = "votes.tot"
  
  ward.12.repu$votes.perc = (ward.12.repu$votes.rec / ward.12.repu$votes.tot) * 100
  
  ward.12.repu = ward.12.repu[!is.na(ward.12.repu$cand.group),]
} # 2012 data basic
{  # Now joining the 2012 data back into the 2016 data
  grep("WARDS",ward.2016.o.repu$reporting_unit)
  
  ward.2016.o.repu$reporting_unit_orig =ward.2016.o.repu$reporting_unit
  ward.12.repu$reporting_unit_orig =ward.12.repu$reporting_unit 
  
  ward.2016.o.repu$reporting_unit = gsub("WARDS","WARD",ward.2016.o.repu$reporting_unit_orig)
  ward.12.repu$reporting_unit = gsub("WARDS","WARD",ward.12.repu$reporting_unit_orig)
  ward.2016.o.repu$reporting_unit = gsub("  "," ",ward.2016.o.repu$reporting_unit)
  ward.12.repu$reporting_unit = gsub("  "," ",ward.12.repu$reporting_unit)
  ward.2016.o.repu$reporting_unit = gsub("&","-",ward.2016.o.repu$reporting_unit)
  ward.12.repu$reporting_unit = gsub("&","-",ward.12.repu$reporting_unit)
  
  ward.2016.o.repu$reporting_unit = gsub("AND","-",ward.2016.o.repu$reporting_unit)
  ward.12.repu$reporting_unit = gsub("AND","-",ward.12.repu$reporting_unit)
  
  ward.2016.o.repu$reporting_unit = gsub(",","-",ward.2016.o.repu$reporting_unit)
  ward.12.repu$reporting_unit = gsub(",","-",ward.12.repu$reporting_unit)
  ward.2016.o.repu$reporting_unit = gsub(" - ","-",ward.2016.o.repu$reporting_unit)
  ward.12.repu$reporting_unit = gsub(" - ","-",ward.12.repu$reporting_unit)
  ward.2016.o.repu$reporting_unit = gsub(" -","-",ward.2016.o.repu$reporting_unit)
  ward.12.repu$reporting_unit = gsub(" -","-",ward.12.repu$reporting_unit)
  ward.2016.o.repu$reporting_unit = gsub("- ","-",ward.2016.o.repu$reporting_unit)
  ward.12.repu$reporting_unit = gsub("- ","-",ward.12.repu$reporting_unit)
  
  ward.2016.o.repu$reporting_unit = gsub(" ","-",ward.2016.o.repu$reporting_unit)
  ward.12.repu$reporting_unit = gsub(" ","-",ward.12.repu$reporting_unit)
  
  ward.2016.o.repu$reporting_unit = gsub("WD","",ward.2016.o.repu$reporting_unit)
  ward.12.repu$reporting_unit = gsub("WD","",ward.12.repu$reporting_unit)
  
  ward.2016.o.repu$reporting_unit = gsub("COMBINED","",ward.2016.o.repu$reporting_unit)
  ward.12.repu$reporting_unit = gsub("COMBINED","",ward.12.repu$reporting_unit)
  
  ward.2016.o.repu$reporting_unit = gsub("--","-",ward.2016.o.repu$reporting_unit)
  ward.12.repu$reporting_unit = gsub("--","-",ward.12.repu$reporting_unit)
  
  setdiff(unique(ward.12.repu$reporting_unit), unique(ward.2016.o.repu$reporting_unit))
  setdiff(unique(ward.2016.o.repu$reporting_unit), unique(ward.12.repu$reporting_unit))
  
  # Village of Harrison Calumet did not exist in 2012, was split off from the town in 2013.
  # It is impossible to know the boundaries exactly.
  # Therefore the swing from 2012 to 2016 cannot be accurately calculated.
  
  # Same of the town / village of Somers, Kenosha. The town / village is completely mixed and messed up
  
  # the town of Menasha was retained only for wards (8-10, 11-13). The rest (1-7) became the village of
  # Fox Crossing, Winnebago
  
  differences = setdiff(unique(ward.2016.o.repu$reporting_unit),unique(ward.12.repu$reporting_unit))
  vote.2012.diffs = subset(ward.2016.o.repu, reporting_unit %in% differences)
  
  vote.2012.diffs.join = with(vote.2012.diffs, data.frame(municipality, county, reporting_unit))
  vote.2012.diffs.join = vote.2012.diffs.join[!duplicated(vote.2012.diffs.join),]
  
  # ## Joining the reg.vote summarised municipalities back onto the voting municipalities
  # ward.12.repu.join = with(ward.12.repu, data.frame(municipality,reporting_unit))
  # colnames(ward.12.repu.join)[2] = "reporting_unit_2012_agreed"
  # ward.12.repu.join = ward.12.repu.join[!duplicated(ward.12.repu.join$reporting_unit_2012),]
  # 
  # 
  # 
  # vote.2012.diffs.join = plyr::join(vote.2012.diffs.join,ward.12.repu.join,by = "reporting_unit")
  # 
  # back.to.votes.df.join = with(vote.2012.diffs.join, data.frame(reporting_unit,reporting_unit_2012_agreed))
  # 
  # ward.2016.o.repu = plyr::join(ward.2016.o.repu, back.to.votes.df.join, by = "reporting_unit")
  # 
  # ward.2016.o.repu$reporting_unit_2012_agreed = as.character(ward.2016.o.repu$reporting_unit_2012_agreed)
  # 
  # ward.2016.o.repu$reporting_unit_2012_agreed = ifelse(is.na(ward.2016.o.repu$reporting_unit_2012_agreed),
  #                                                  ward.2016.o.repu$reporting_unit,ward.2016.o.repu$reporting_unit_2012_agreed)
  # 
  ## Now renaming the 2012 column for the final join
  # colnames(ward.12.repu)[2] = "muni_county_2012_agreed"
  
  ## Need to sum up the votes by county in 2012
  ward.12.repu.votes.group = dplyr::group_by(ward.12.repu,cand.group,reporting_unit,reporting_unit_orig,municipality, county)
  ward.12.repu.votes.g = dplyr::summarise(ward.12.repu.votes.group,
                                         votes.rec2012 = sum(votes.rec),
                                         votes.tot2012 = mean(votes.tot)
                                         
  )
  
  # Melting the 2012 df to put the candidates on top
  ward.12.repu.votes.g.melt = melt(ward.12.repu.votes.g)
  ward.12.repu.votes.g.cast = dcast(ward.12.repu.votes.g.melt,
                                   reporting_unit ~ cand.group + variable, sum
  )
  ward.12.repu.votes.g.cast$democrat_votes.tot2012 = NULL
  ward.12.repu.votes.g.cast$republican_votes.tot2012 = NULL
  
  ward.12.repu.votes.g.cast.mean = dcast(ward.12.repu.votes.g.melt,
                                        reporting_unit ~ cand.group + variable, mean
  )
  ward.12.repu.votes.g.cast$other_votes.tot2012 = ward.12.repu.votes.g.cast.mean$other_votes.tot2012
  colnames(ward.12.repu.votes.g.cast)[grepl("votes.tot",colnames(ward.12.repu.votes.g.cast))] =
    "votes.tot2012"
  
  # Also need to melt the 2016 mun df by cand.group
  ward.2016.o.repu.votes.g.melt = melt(ward.2016.o.repu)
  ward.2016.o.repu.votes.g.cast = dcast(ward.2016.o.repu.votes.g.melt,
                                       reporting_unit + reporting_unit_orig + muni_county + municipality + county ~
                                         cand.group + variable, sum
  )
  
  ward.2016.o.repu.votes.g.cast$democrat_votes.tot = NULL
  ward.2016.o.repu.votes.g.cast$republican_votes.tot = NULL
  ward.2016.o.repu.votes.g.cast$democrat_voters.tot = NULL
  ward.2016.o.repu.votes.g.cast$republican_voters.tot = NULL
  
  ward.2016.o.repu.votes.g.cast.mean = dcast(ward.2016.o.repu.votes.g.melt,
                                            reporting_unit + reporting_unit_orig + muni_county + municipality + county ~
                                        cand.group + variable, mean
  )
  ward.2016.o.repu.votes.g.cast$other_votes.tot = ward.2016.o.repu.votes.g.cast.mean$other_votes.tot
  ward.2016.o.repu.votes.g.cast$other_voters.tot = ward.2016.o.repu.votes.g.cast.mean$other_voters.tot
  colnames(ward.2016.o.repu.votes.g.cast)[grepl("votes.tot",colnames(ward.2016.o.repu.votes.g.cast))] =
    "votes.tot2016"
  colnames(ward.2016.o.repu.votes.g.cast)[grepl("voters.tot",colnames(ward.2016.o.repu.votes.g.cast))] =
    "voters.tot2016"
  
  # Finally joining the dfs together
  
  ward.2016.o.repu.g = plyr::join(ward.2016.o.repu.votes.g.cast, ward.12.repu.votes.g.cast, by = "reporting_unit")
  
  # 2012 turnout is calculated on the (big?) assumption that the number of registered
  # voters was the same in 2012 as in 2016
  # ward.2016.o.repu.g$turnout.2012 =
    # with(ward.2016.o.repu.g, votes.tot2012 / voters.tot2016) * 100
  
  ward.2016.o.repu.g$dem.perc2012 =
    with(ward.2016.o.repu.g,
         democrat_votes.rec2012 / votes.tot2012) * 100
  ward.2016.o.repu.g$rep.perc2012 =
    with(ward.2016.o.repu.g,
         republican_votes.rec2012 / votes.tot2012) * 100
  ward.2016.o.repu.g$oth.perc2012 =
    with(ward.2016.o.repu.g,
         other_votes.rec2012 / votes.tot2012) * 100
  
  ward.2016.o.repu.g$dem.change.num =
    with(ward.2016.o.repu.g,
         democrat_votes.rec - democrat_votes.rec2012)
  ward.2016.o.repu.g$rep.change.num =
    with(ward.2016.o.repu.g,
         republican_votes.rec - republican_votes.rec2012)
  ward.2016.o.repu.g$oth.change.num =
    with(ward.2016.o.repu.g,
         other_votes.rec - other_votes.rec2012)
  
  ward.2016.o.repu.g$dem.change.perc =
    with(ward.2016.o.repu.g,
         democrat_votes.perc -  dem.perc2012)
  ward.2016.o.repu.g$rep.change.perc =
    with(ward.2016.o.repu.g,
         republican_votes.perc - rep.perc2012)
  ward.2016.o.repu.g$oth.change.perc =
    with(ward.2016.o.repu.g,
         other_votes.perc - oth.perc2012)
  # 
  # ward.2016.o.repu.g$turnout.2016.perc =
  #   with(ward.2016.o.repu.g,
  #        votes.tot2016 / voters.tot2016) * 100
  
  # ward.2016.o.repu.g$turnout.change.perc =
  #   with(ward.2016.o.repu.g,
  #        turnout.2016.perc - turnout.2012)
  
  
  repunit.2016.2012.o = ward.2016.o.repu.g
  
  repunit.2016.2012.o$demwinner = NA
  repunit.2016.2012.o$demwinner = ifelse(repunit.2016.2012.o$democrat_votes.perc >
                                        repunit.2016.2012.o$republican_votes.perc &
                                        !is.na(repunit.2016.2012.o$democrat_votes.perc), 1,0)
  repunit.2016.2012.o$demwinner = as.integer(repunit.2016.2012.o$demwinner)
  
  repunit.2016.2012.o$demwinner2012 = NA
  repunit.2016.2012.o$demwinner2012 = ifelse(repunit.2016.2012.o$dem.perc2012 >
                                            repunit.2016.2012.o$rep.perc2012 &
                                            !is.na(repunit.2016.2012.o$dem.perc2012), 1,0)
  repunit.2016.2012.o$demwinner2012 = as.integer(repunit.2016.2012.o$demwinner2012)
} # Now joining the 2012 data back into the 2016 data
} # Loading 2012 ward vote data
{   # 2008 ward data
  ward.08 = read.csv(
    "C:/Users/s_cas/Dropbox/Perso/2016 voting election county results/Wisconsin/All prep together/2008 results/2008_FallElection_President_WardbyWard.csv",
    stringsAsFactors = F,
    header = T,
    strip.white = T
  )
  
  { # Loading 2008 ward vote data
    
    ward.08 = ward.08[,11:27]
    
    ward.08 = clean_names(ward.08)
    
    ward.08$reporting_unit = ifelse(ward.08$reporting_unit == "","Ward 1",ward.08$reporting_unit)
    ward.08$municipality_name = with(ward.08, paste(municipality_type, "of", municipality,sep=" "))
    ward.08$reporting_unit = with(ward.08, paste(municipality_name, reporting_unit))
    
    # colnames(ward.08)[1:3] = c("county","municipality_name","reporting_unit")
    
    ward.08$reporting_unit = toupper(ward.08$reporting_unit)
    ward.08$municipality_name = toupper(ward.08$municipality_name)
    ward.08$county = toupper(ward.08$county)
    
    # The following is fixing a problem I noticed later when trying to match with the 2016 voter df
    ward.08$municipality_name[grep("TOWN OF WINDSOR", ward.08$municipality_name)] = "VILLAGE OF WINDSOR"
    ward.08$reporting_unit = gsub("TOWN OF WINDSOR","VILLAGE OF WINDSOR", ward.08$reporting_unit)
    
    # The town in Maine became a village in 2015, and is now in marathon county
    # ward.08$county[grep("TOWN OF MAINE", ward.08$municipality_name)] = "MARATHON"
    ward.08$municipality_name[grepl("TOWN OF MAINE", ward.08$municipality_name) &
                                grepl("MARATHON", ward.08$county)
                              ] = "VILLAGE OF MAINE"
    # ward.08$reporting_unit = gsub("TOWN OF MAINE","VILLAGE OF MAINE", ward.08$reporting_unit)
    
    # the town of Menasha was retained only for wards (8-10, 11-13). The rest (1-7) became the village of
    # Fox Crossing, Winnebago
    ward.08$municipality_name[grepl("TOWN OF MENASHA WARDS 1, 2, 4, 7", ward.08$reporting_unit)
                              ] = "VILLAGE OF FOX CROSSING"
    ward.08$municipality_name[grepl("TOWN OF MENASHA WARDS 3, 5, 6", ward.08$reporting_unit)
                              ] = "VILLAGE OF FOX CROSSING"
    ward.08$reporting_unit[grepl("TOWN OF MENASHA WARDS 1, 2, 4, 7", ward.08$reporting_unit)
                           ] = "VILLAGE OF FOX CROSSING WARDS 1-2,4,7"
    ward.08$reporting_unit[grepl("TOWN OF MENASHA WARDS 3, 5, 6", ward.08$reporting_unit)
                           ] = "VILLAGE OF FOX CROSSING WARDS 3,5-6"
    
    ward.08$muni_county = paste(ward.08$municipality_name, ward.08$county)
    ward.08$reporting_unit = paste(ward.08$reporting_unit, ward.08$county)
    
    ward.08.look = with(ward.08, data.frame(county, municipality_name,
                                            reporting_unit, muni_county))
    colnames(ward.08.look) = c("county","municipality","reporting_unit",
                               "muni_county")
    
    ward.08.v = ward.08
    # ward.08.v$county = NULL
    ward.08.v$county = NULL
    ward.08.v$municipality_name = NULL
    ward.08.v$municipality_type = NULL
    ward.08.v$municipalityno = NULL
    ward.08.v$municipality = NULL
    # ward.08.v$reporting_unit = NULL
    ward.08.v$order = NULL
    ward.08.v$hindi = NULL
    # ward.08.v$assemblydistrict = NULL
    
    ward.08.l <- ward.08.v
    ward.08.l$muni_county = NULL
    ward.08.l$reporting_unit <- NULL
    ward.08.l = gather(ward.08.l, cand, votes.rec)
    ward.08.l$reporting_unit <- rep(ward.08.v$reporting_unit)
    # colnames(ward.08.l) = c("reporting_unit", "cand", "votes.rec")
    
    ward.08.l$cand.group = NA
    ward.08.l$cand.group[grep("obama",ward.08.l$cand)] = "democrat"
    ward.08.l$cand.group[grep("mc_cain",ward.08.l$cand)] = "republican"
    ward.08.l$cand.group[grep("rosa",ward.08.l$cand)] = "other"
    ward.08.l$cand.group[grep("wayne",ward.08.l$cand)] = "other"
    ward.08.l$cand.group[grep("stewart",ward.08.l$cand)] = "other"
    ward.08.l$cand.group[grep("robert",ward.08.l$cand)] = "other"
    ward.08.l$cand.group[grep("gonzalez",ward.08.l$cand)] = "other"
    ward.08.l$cand.group[grep("castle",ward.08.l$cand)] = "other"
    ward.08.l$cand.group[grep("darrell",ward.08.l$cand)] = "other"
    ward.08.l$cand.group[grep("klimisch",ward.08.l$cand)] = "other"
    ward.08.l$cand.group[grep("scatter",ward.08.l$cand)] = "other"
    
    ward.08.l$votes.rec = as.integer(ward.08.l$votes.rec)
    
    # Joining back county and municipality names
    ward.08.l = plyr::join(ward.08.l, ward.08.look, by = "reporting_unit", match = "first")
    
    tot.votes.look.group = dplyr::group_by(ward.08.l,reporting_unit)
    tot.votes.look = dplyr::summarise(tot.votes.look.group,
                                      votes.tot = sum(as.integer(votes.rec),na.rm=T)
    )
    
    ward.08.l = plyr::join(ward.08.l,tot.votes.look,by="reporting_unit")
    ward.08.l$votes.perc = (ward.08.l$votes.rec / ward.08.l$votes.tot) * 100
    
    ward.08.l$muni_county = paste(ward.08.l$municipality, ward.08.l$county)
    
    ward.08.repu.group = dplyr::group_by(ward.08.l,reporting_unit,municipality,muni_county,county,cand.group)
    ward.08.repu = dplyr::summarise(ward.08.repu.group,
                                   votes.rec2 = sum(votes.rec,na.rm=T),
                                   votes.tot2 = mean(votes.tot,na.rm=T)
    )
    colnames(ward.08.repu)[6] = "votes.rec"
    colnames(ward.08.repu)[7] = "votes.tot"
    
    ward.08.repu$votes.perc = (ward.08.repu$votes.rec / ward.08.repu$votes.tot) * 100
    
    ward.08.repu = ward.08.repu[!is.na(ward.08.repu$cand.group),]
  } # 2008 data basic
  {  # Now joining the 2008 data back into the 2016 data
    # grep("WARDS",ward.2016.o.repu$reporting_unit)
    
    ward.2016.o.repu$reporting_unit_orig =ward.2016.o.repu$reporting_unit_orig
    ward.08.repu$reporting_unit_orig =ward.08.repu$reporting_unit 
    
    ward.2016.o.repu$reporting_unit = gsub("WARDS","WARD",ward.2016.o.repu$reporting_unit_orig)
    ward.08.repu$reporting_unit = gsub("WARDS","WARD",ward.08.repu$reporting_unit_orig)
    ward.2016.o.repu$reporting_unit = gsub("  "," ",ward.2016.o.repu$reporting_unit)
    ward.08.repu$reporting_unit = gsub("  "," ",ward.08.repu$reporting_unit)
    ward.2016.o.repu$reporting_unit = gsub("&","-",ward.2016.o.repu$reporting_unit)
    ward.08.repu$reporting_unit = gsub("&","-",ward.08.repu$reporting_unit)
    
    ward.2016.o.repu$reporting_unit = gsub("AND","-",ward.2016.o.repu$reporting_unit)
    ward.08.repu$reporting_unit = gsub("AND","-",ward.08.repu$reporting_unit)
    
    ward.2016.o.repu$reporting_unit = gsub(",","-",ward.2016.o.repu$reporting_unit)
    ward.08.repu$reporting_unit = gsub(",","-",ward.08.repu$reporting_unit)
    ward.2016.o.repu$reporting_unit = gsub(" - ","-",ward.2016.o.repu$reporting_unit)
    ward.08.repu$reporting_unit = gsub(" - ","-",ward.08.repu$reporting_unit)
    ward.2016.o.repu$reporting_unit = gsub(" -","-",ward.2016.o.repu$reporting_unit)
    ward.08.repu$reporting_unit = gsub(" -","-",ward.08.repu$reporting_unit)
    ward.2016.o.repu$reporting_unit = gsub("- ","-",ward.2016.o.repu$reporting_unit)
    ward.08.repu$reporting_unit = gsub("- ","-",ward.08.repu$reporting_unit)
    
    ward.2016.o.repu$reporting_unit = gsub(" ","-",ward.2016.o.repu$reporting_unit)
    ward.08.repu$reporting_unit = gsub(" ","-",ward.08.repu$reporting_unit)
    
    ward.2016.o.repu$reporting_unit = gsub("WD","",ward.2016.o.repu$reporting_unit)
    ward.08.repu$reporting_unit = gsub("WD","",ward.08.repu$reporting_unit)
    
    ward.2016.o.repu$reporting_unit = gsub("COMBINED","",ward.2016.o.repu$reporting_unit)
    ward.08.repu$reporting_unit = gsub("COMBINED","",ward.08.repu$reporting_unit)
    
    ward.2016.o.repu$reporting_unit = gsub("--","-",ward.2016.o.repu$reporting_unit)
    ward.08.repu$reporting_unit = gsub("--","-",ward.08.repu$reporting_unit)
    
    setdiff(unique(ward.08.repu$reporting_unit), unique(ward.2016.o.repu$reporting_unit))
    setdiff(unique(ward.2016.o.repu$reporting_unit), unique(ward.08.repu$reporting_unit))
    
    # Village of Harrison Calumet did not exist in 2008, was split off from the town in 2013.
    # It is impossible to know the boundaries exactly.
    # Therefore the swing from 2008 to 2016 cannot be accurately calculated.
    
    # Same of the town / village of Somers, Kenosha. The town / village is completely mixed and messed up
    
    # the town of Menasha was retained only for wards (8-10, 11-13). The rest (1-7) became the village of
    # Fox Crossing, Winnebago
    
    differences = setdiff(unique(ward.2016.o.repu$reporting_unit),unique(ward.08.repu$reporting_unit))
    vote.2008.diffs = subset(ward.2016.o.repu, reporting_unit %in% differences)
    
    vote.2008.diffs.join = with(vote.2008.diffs, data.frame(municipality, county, reporting_unit))
    vote.2008.diffs.join = vote.2008.diffs.join[!duplicated(vote.2008.diffs.join),]
    
    # ## Joining the reg.vote summarised municipalities back onto the voting municipalities
    # ward.08.repu.join = with(ward.08.repu, data.frame(municipality,reporting_unit))
    # colnames(ward.08.repu.join)[2] = "reporting_unit_2008_agreed"
    # ward.08.repu.join = ward.08.repu.join[!duplicated(ward.08.repu.join$reporting_unit_2008),]
    # 
    # 
    # 
    # vote.2008.diffs.join = plyr::join(vote.2008.diffs.join,ward.08.repu.join,by = "reporting_unit")
    # 
    # back.to.votes.df.join = with(vote.2008.diffs.join, data.frame(reporting_unit,reporting_unit_2008_agreed))
    # 
    # ward.2016.o.repu = plyr::join(ward.2016.o.repu, back.to.votes.df.join, by = "reporting_unit")
    # 
    # ward.2016.o.repu$reporting_unit_2008_agreed = as.character(ward.2016.o.repu$reporting_unit_2008_agreed)
    # 
    # ward.2016.o.repu$reporting_unit_2008_agreed = ifelse(is.na(ward.2016.o.repu$reporting_unit_2008_agreed),
    #                                                  ward.2016.o.repu$reporting_unit,ward.2016.o.repu$reporting_unit_2008_agreed)
    # 
    ## Now renaming the 2008 column for the final join
    # colnames(ward.08.repu)[2] = "muni_county_2008_agreed"
    
    ## Need to sum up the votes by county in 2008
    ward.08.repu.votes.group = dplyr::group_by(ward.08.repu,cand.group,reporting_unit,reporting_unit_orig,municipality, county)
    ward.08.repu.votes.g = dplyr::summarise(ward.08.repu.votes.group,
                                           votes.rec2008 = sum(votes.rec),
                                           votes.tot2008 = mean(votes.tot)
                                           
    )
    
    # Melting the 2008 df to put the candidates on top
    ward.08.repu.votes.g.melt = melt(ward.08.repu.votes.g)
    ward.08.repu.votes.g.cast = dcast(ward.08.repu.votes.g.melt,
                                     reporting_unit ~ cand.group + variable, sum
    )
    ward.08.repu.votes.g.cast$democrat_votes.tot2008 = NULL
    ward.08.repu.votes.g.cast$republican_votes.tot2008 = NULL
    
    ward.08.repu.votes.g.cast.mean = dcast(ward.08.repu.votes.g.melt,
                                          reporting_unit ~ cand.group + variable, mean
    )
    ward.08.repu.votes.g.cast$other_votes.tot2008 = ward.08.repu.votes.g.cast.mean$other_votes.tot2008
    colnames(ward.08.repu.votes.g.cast)[grepl("votes.tot",colnames(ward.08.repu.votes.g.cast))] =
      "votes.tot2008"
    
    # Also need to melt the 2016 mun df by cand.group
    ward.2016.o.repu.votes.g.melt = melt(ward.2016.o.repu)
    ward.2016.o.repu.votes.g.cast = dcast(ward.2016.o.repu.votes.g.melt,
                                         reporting_unit + reporting_unit_orig + muni_county + municipality + county ~
                                           cand.group + variable, sum
    )
    
    ward.2016.o.repu.votes.g.cast$democrat_votes.tot = NULL
    ward.2016.o.repu.votes.g.cast$republican_votes.tot = NULL
    ward.2016.o.repu.votes.g.cast$democrat_voters.tot = NULL
    ward.2016.o.repu.votes.g.cast$republican_voters.tot = NULL
    
    ward.2016.o.repu.votes.g.cast.mean = dcast(ward.2016.o.repu.votes.g.melt,
                                              reporting_unit + reporting_unit_orig + muni_county + municipality + county ~
                                                cand.group + variable, mean
    )
    ward.2016.o.repu.votes.g.cast$other_votes.tot = ward.2016.o.repu.votes.g.cast.mean$other_votes.tot
    ward.2016.o.repu.votes.g.cast$other_voters.tot = ward.2016.o.repu.votes.g.cast.mean$other_voters.tot
    colnames(ward.2016.o.repu.votes.g.cast)[grepl("votes.tot",colnames(ward.2016.o.repu.votes.g.cast))] =
      "votes.tot2016"
    colnames(ward.2016.o.repu.votes.g.cast)[grepl("voters.tot",colnames(ward.2016.o.repu.votes.g.cast))] =
      "voters.tot2016"
    
    # Finally joining the dfs together
    
    ward.2016.o.repu.g = plyr::join(ward.2016.o.repu.votes.g.cast, ward.08.repu.votes.g.cast, by = "reporting_unit")
    
    # 2008 turnout is calculated on the (big?) assumption that the number of registered
    # voters was the same in 2008 as in 2016
    # ward.2016.o.repu.g$turnout.2008 =
    # with(ward.2016.o.repu.g, votes.tot2008 / voters.tot2016) * 100
    
    ward.2016.o.repu.g$dem.perc2008 =
      with(ward.2016.o.repu.g,
           democrat_votes.rec2008 / votes.tot2008) * 100
    ward.2016.o.repu.g$rep.perc2008 =
      with(ward.2016.o.repu.g,
           republican_votes.rec2008 / votes.tot2008) * 100
    ward.2016.o.repu.g$oth.perc2008 =
      with(ward.2016.o.repu.g,
           other_votes.rec2008 / votes.tot2008) * 100
    
    ward.2016.o.repu.g$dem.change.2008.num =
      with(ward.2016.o.repu.g,
           democrat_votes.rec - democrat_votes.rec2008)
    ward.2016.o.repu.g$rep.change.2008.num =
      with(ward.2016.o.repu.g,
           republican_votes.rec - republican_votes.rec2008)
    ward.2016.o.repu.g$oth.change.2008.num =
      with(ward.2016.o.repu.g,
           other_votes.rec - other_votes.rec2008)
    
    ward.2016.o.repu.g$dem.change.2008.perc =
      with(ward.2016.o.repu.g,
           democrat_votes.perc -  dem.perc2008)
    ward.2016.o.repu.g$rep.change.2008.perc =
      with(ward.2016.o.repu.g,
           republican_votes.perc - rep.perc2008)
    ward.2016.o.repu.g$oth.change.2008.perc =
      with(ward.2016.o.repu.g,
           other_votes.perc - oth.perc2008)
    # 
    # ward.2016.o.repu.g$turnout.2016.perc =
    #   with(ward.2016.o.repu.g,
    #        votes.tot2016 / voters.tot2016) * 100
    
    # ward.2016.o.repu.g$turnout.change.perc =
    #   with(ward.2016.o.repu.g,
    #        turnout.2016.perc - turnout.2008)
    
    
    repunit.2016.2008.o = ward.2016.o.repu.g
    
    repunit.2016.2008.o$demwinner = NA
    repunit.2016.2008.o$demwinner = ifelse(repunit.2016.2008.o$democrat_votes.perc >
                                             repunit.2016.2008.o$republican_votes.perc &
                                             !is.na(repunit.2016.2008.o$democrat_votes.perc), 1,0)
    repunit.2016.2008.o$demwinner = as.integer(repunit.2016.2008.o$demwinner)
    
    repunit.2016.2008.o$demwinner2008 = NA
    repunit.2016.2008.o$demwinner2008 = ifelse(repunit.2016.2008.o$dem.perc2008 >
                                                 repunit.2016.2008.o$rep.perc2008 &
                                                 !is.na(repunit.2016.2008.o$dem.perc2008), 1,0)
    repunit.2016.2008.o$demwinner2008 = as.integer(repunit.2016.2008.o$demwinner2008)
  } # Now joining the 2008 data back into the 2016 data
} # Loading 2008 ward vote data

# Rejoining the county data to the ward 2016 2012 data
{
  repunit.2016.2012.o = plyr::join(repunit.2016.2012.o, ward.2016.look, by = "reporting_unit", match = "first")
  
  repunit.2016.2008.o = plyr::join(repunit.2016.2008.o, ward.2016.look, by = "reporting_unit", match = "first")
}

# Loading voting machine data 2016 original vs 2012 (Wisconsin official) ---------------------------------------------
# This file from here: http://elections.wi.gov/elections-voting/voting-equipment/voting-equipment-use
# Voting machine equipment for 13 September 2016
vot.equip = read.xlsx(
  "voting_equipment_by_municipality_09_2016_xlsx_78114.xlsx1207162619.xlsx",
  sheetIndex = 1,
  header = TRUE,
  colClasses = NA,
  stringsAsFactors = F
)
colnames(vot.equip) = c(
  "county.long",
  "municipality",
  "machine.vendor.dealer.model",
  "accessible.vendor.dealer.model"
)

# The following is fixing a problem I noticed later when trying to match with the voter df
vot.equip$municipality[grep("TOWN OF WINDSOR",
                            vot.equip$municipality)] = "VILLAGE OF WINDSOR"

# Following from
# http://stackoverflow.com/questions/15895050/using-gsub-to-extract-character-string-before-white-space-in-r
county.split.att2 = gsub(" COUNTY.*$", "", vot.equip$county.long)
vot.equip$county = county.split.att2
vot.equip = vot.equip[!is.na(vot.equip$county.long), ]

vot.equip$use.machines = NA
vot.equip$use.machines = ifelse(vot.equip$machine.vendor.dealer.model == "None ", 0, 1)

vot.equip$machine.vendor.dealer.spec = gsub("Optech-", "Optech ", vot.equip$machine.vendor.dealer.model)
vot.equip$machine.vendor.dealer.spec = gsub("Optech/", "Optech ", vot.equip$machine.vendor.dealer.spec)
vot.equip$machine.vendor.dealer.spec = gsub(" .*$", "", vot.equip$machine.vendor.dealer.spec)
vot.equip$machine.vendor.dealer.spec = gsub("None", NA, vot.equip$machine.vendor.dealer.spec)

vot.equip.county.grouped = dplyr::group_by(vot.equip, county)
vot.equip.county = dplyr::summarise(
  vot.equip.county.grouped,
  use.machines.prop = sum(use.machines, na.rm = TRUE) /
    length(use.machines)
)

# vot.equip.withmachines = subset(vot.equip, use.machines > 0)
vot.equip.withmachines = vot.equip

vot.equip.county.grouped.machines = dplyr::group_by(vot.equip.withmachines, county)
vot.equip.county.machines = dplyr::summarise(
  vot.equip.county.grouped.machines,
  use.machines.prop = sum(use.machines, na.rm = TRUE) /
    length(use.machines)
)

vot.equip.county.machines$use.machines.prop = NULL

max.vendor = as.character(1:length(vot.equip.county.machines$county))
for (i in 1:length(unique(vot.equip.county.machines$county))) {
  a = count(vot.equip.withmachines$machine.vendor.dealer.spec[vot.equip.withmachines$county ==
                                                                vot.equip.withmachines$county[i]])
  a$x = as.character(a$x)
  max.vendor[i] = a$x[a$freq == max(a$freq)]
}

vot.equip.county.machines$machine.most.used = max.vendor
vot.equip.county = plyr::join(vot.equip.county,
                        vot.equip.county.machines,
                        by = "county",
                        match = "first")

test = tolower(vot.equip.county$county)
test = capwords(test)
vot.equip.county$county = test

machine.join = data.frame(vot.equip.county$county, vot.equip.county$use.machines.prop, vot.equip.county$machine.most.used)
colnames(machine.join) = c("county","use.machines.prop","machine.most.used")

machine.join$all.machines = with(machine.join, ifelse(use.machines.prop > 0.75, "Mostly optical scanners",
                                                      "Some or no optical scanners"))
machine.join$county = as.character(machine.join$county)
machine.join$county[machine.join$county == "Fond Du Lac"] = "Fond du Lac"

rm(vote.equip.county.machines,vote.equip.county.grouped,voting.age.people,test,
   historical.turnout.join,a)

#### Now joining this to the ward data
vot.equip.withmachines$muni_county = gsub(" -.*", "",vot.equip.withmachines$municipality)

vot.equip.withmachines$muni_county = paste(vot.equip.withmachines$muni_county, vot.equip.withmachines$county)
vot.equip.withmachines$muni_county = gsub("  "," ",vot.equip.withmachines$muni_county)


vot.equip.join = with(vot.equip.withmachines,
                      data.frame(muni_county,
                                 use.machines,
                                 machine.vendor.dealer.spec,
                                 machine.vendor.dealer.model,
                                 accessible.vendor.dealer.model))

setdiff(vot.equip.withmachines$muni_county, repunit.2016.2012.o$muni_county)

# [1] TOWN OF MAINE MARATHON"

setdiff(repunit.2016.2012.o$muni_county, vot.equip.withmachines$muni_county)



# repunit.2016.2012.o$muni_county = repunit.2016.2012.o$muni_county

repunit.2016.2012.o$muni_county[grep("VILLAGE OF WRIGHTSTOWN",
                                               repunit.2016.2012.o$muni_county)] = "VILLAGE OF WRIGHTSTOWN BROWN"
repunit.2016.2012.o$muni_county[grep("TOWN OF MAINE",
                                               repunit.2016.2012.o$muni_county)] = "TOWN OF MAINE MARATHON"
colnames(vot.equip.join)[1] = "muni_county"

setdiff(vot.equip.join$muni_county, repunit.2016.2012.o$muni_county)

repunit.2016.2012.o = plyr::join(repunit.2016.2012.o,vot.equip.join, by = "muni_county")
repunit.2016.2012.o$use.machines[is.na(repunit.2016.2012.o$use.machines)] = 0

colnames(repunit.2016.2012.o)[grep("use.machines",colnames(repunit.2016.2012.o))] = "county_use_opt_scan"
colnames(repunit.2016.2012.o)[grep("machine.vendor.dealer.spec",colnames(repunit.2016.2012.o))] = "county_machine_vendor_dealer"

####
# doing the same for the 2016 2012 data
####
county.machines.join = with(repunit.2016.2012.o, data.frame(
  muni_county, county_use_opt_scan, county_machine_vendor_dealer, machine.vendor.dealer.model, accessible.vendor.dealer.model
))

ward.2016.2012.test = plyr::join(repunit.2016.2012.o, county.machines.join, by = "muni_county", match = "first")

repunit.2016.2012.o = ward.2016.2012.test
repunit.2016.2012.o$county_use_opt_scan[is.na(repunit.2016.2012.o$county_use_opt_scan)] = 0

# and the 2016 2008 data
ward.2016.2008.test = plyr::join(repunit.2016.2008.o, county.machines.join, by = "muni_county", match = "first")

repunit.2016.2008.o = ward.2016.2008.test
repunit.2016.2008.o$county_use_opt_scan[is.na(repunit.2016.2008.o$county_use_opt_scan)] = 0

rm(ward.2016.2012.test, ward.2016.2008.test)

# Reporting unit level data 2008 2012 and 2016 recount ----------------------------------------------------
ward.2016 = read.csv(
  "wardwisconsin2016andrecount.csv",
  stringsAsFactors = F,
  header = T,
  strip.white = T
)
{ # Preparing basic data
  ward.2016 = subset(ward.2016, Total.Votes > 0)
  
  ward.2016 = clean_names(ward.2016)
  ward.2016$reporting_unit = toupper(ward.2016$reporting_unit)
  ward.2016$municipality_name = toupper(ward.2016$municipality_name)
  ward.2016$county_name = toupper(ward.2016$county_name)
  
  ward.2016$muni_county = paste(ward.2016$municipality_name, ward.2016$county_name)
  ward.2016$reporting_unit = paste(ward.2016$reporting_unit, ward.2016$county_name)
  
  ward.2016.look = with(ward.2016, data.frame(county_name, municipality_name,
                                              reporting_unit, muni_county))
  colnames(ward.2016.look) = c("county","municipality","reporting_unit",
                               "muni_county")
  
  ward.2016.v = ward.2016
  ward.2016.v$county_name = NULL
  ward.2016.v$municipality_name = NULL
  ward.2016.v$total_votes = NULL
  # ward.2016.v$reporting_unit = NULL
  
  ward.2016.r = subset(ward.2016.v, original_or_recount == "Recount")
  ward.2016.r$original_or_recount = NULL

  ward.2016.r.l <- ward.2016.r
  ward.2016.r.l$muni_county = NULL
  ward.2016.r.l$reporting_unit <- NULL
  ward.2016.r.l = gather(ward.2016.r.l, cand, votes.rec)
  ward.2016.r.l$reporting_unit <- rep(ward.2016.r$reporting_unit)
  # colnames(ward.2016.r.l) = c("reporting_unit", "cand", "votes.rec")
  
  ward.2016.r.l$cand.group = NA
  ward.2016.r.l$cand.group[grep("hillary",ward.2016.r.l$cand)] = "democrat"
  ward.2016.r.l$cand.group[grep("donald",ward.2016.r.l$cand)] = "republican"
  ward.2016.r.l$cand.group[grep("darrell",ward.2016.r.l$cand)] = "other"
  ward.2016.r.l$cand.group[grep("rocky",ward.2016.r.l$cand)] = "other"
  ward.2016.r.l$cand.group[grep("gary",ward.2016.r.l$cand)] = "other"
  ward.2016.r.l$cand.group[grep("monica",ward.2016.r.l$cand)] = "other"
  ward.2016.r.l$cand.group[grep("jill",ward.2016.r.l$cand)] = "other"
  
  # There were a bunch of write-in names for the Reporting unit level results
  ward.2016.r.l$cand.group[grep("cherunda",ward.2016.r.l$cand)] = "other"
  ward.2016.r.l$cand.group[grep("evan",ward.2016.r.l$cand)] = "other"
  ward.2016.r.l$cand.group[grep("michael_a",ward.2016.r.l$cand)] = "other"
  ward.2016.r.l$cand.group[grep("marshall",ward.2016.r.l$cand)] = "other"
  ward.2016.r.l$cand.group[grep("chris",ward.2016.r.l$cand)] = "other"
  ward.2016.r.l$cand.group[grep("laurence",ward.2016.r.l$cand)] = "other"
  ward.2016.r.l$cand.group[grep("hoefling",ward.2016.r.l$cand)] = "other"
  ward.2016.r.l$cand.group[grep("joseph",ward.2016.r.l$cand)] = "other"
  ward.2016.r.l$cand.group[grep("emidio",ward.2016.r.l$cand)] = "other"
  ward.2016.r.l$cand.group[grep("scatter",ward.2016.r.l$cand)] = "other"
  
  
  # Joining back county and municipality names
  ward.2016.r.l = plyr::join(ward.2016.r.l, ward.2016.look, by = "reporting_unit", match = "first")
  
  ward.2016.r.l$votes.rec = as.numeric(as.integer(ward.2016.r.l$votes.rec))
  # table(is.na(ward.2016.r.l$votes.rec))
  
  tot.votes.look.group = dplyr::group_by(ward.2016.r.l,reporting_unit)
  tot.votes.look = dplyr::summarise(tot.votes.look.group,
                                    votes.tot = sum(votes.rec,na.rm=T)
  )
  ward.2016.r.l = plyr::join(ward.2016.r.l,tot.votes.look,by="reporting_unit",match="first")
  ward.2016.r.l$votes.perc = (ward.2016.r.l$votes.rec / ward.2016.r.l$votes.tot) * 100
  
  ward.2016.r.l$muni_county = paste(ward.2016.r.l$municipality,
                                    ward.2016.r.l$county)
  
  ward.2016.r.repu.group = dplyr::group_by(ward.2016.r.l,reporting_unit, municipality,muni_county,county,cand.group)
  ward.2016.r.repu = dplyr::summarise(ward.2016.r.repu.group,
                                      votes.rec2 = sum(votes.rec),
                                      votes.tot2 = mean(votes.tot)
  )
  colnames(ward.2016.r.repu)[6] = "votes.rec"
  colnames(ward.2016.r.repu)[7] = "votes.tot"
  
  ward.2016.r.repu$votes.perc = (ward.2016.r.repu$votes.rec / ward.2016.r.repu$votes.tot) * 100
  
  ward.2016.r.repu = ward.2016.r.repu[!is.na(ward.2016.r.repu$cand.group),]
} # Preparing basic data 2016
{
  ## Now bringing in the registered voter data.
  # ward.2016.regvote = read.csv(
  #   "registeredvotersbywards_xlsx_19539MODnov16.csv",
  #   stringsAsFactors = F,
  #   header = T,
  #   strip.white = T
  # )
  # {
  #   ward.2016.regvote = clean_names(ward.2016.regvote)
  #   ward.2016.regvote$ward = toupper(ward.2016.regvote$ward)
  #   ward.2016.regvote$county = toupper(ward.2016.regvote$county)
  #   ward.2016.regvote$muni = toupper(ward.2016.regvote$muni)
  #   
  #   ward.2016.regvote$county = gsub(" COUNTY","",ward.2016.regvote$county)
  #   ward.2016.regvote$muni = gsub(" -.*","",ward.2016.regvote$muni)
  #   ward.2016.regvote$muni_county = paste(ward.2016.regvote$muni, ward.2016.regvote$county)
  #   
  #   colnames(ward.2016.regvote)[2] = "municipality"
  #   
  #   ### Limiting to municipality level
  #   ward.2016.regvote.repu.group = dplyr::group_by(ward.2016.regvote,reporting_unit,muni_county,municipality,county)
  #   ward.2016.regvote.repu = dplyr::summarise(ward.2016.regvote.repu.group,
  #                                            voter_count2 = sum(voter_count)
  #   )
  #   colnames(ward.2016.regvote.repu)[4] = "voters.tot"
  #   
  #   
  #   
  #   ###
  #   # Seeing what is differnt according to muni county level
  #   ###
  #   voter.reg.repu = data.frame(unique(ward.2016.regvote.repu$muni_county))
  #   votes.repu = data.frame(unique(ward.2016.r.l$muni_county))
  #   colnames(voter.reg.repu) = "voter.reg.repu"
  #   colnames(votes.repu) = "votes.repu"
  #   
  #   # Ugh, I need to change the name of these muni_counties to make sure I have a full match
  #   setdiff(unique(voter.reg.repu$voter.reg.repu), unique(votes.repu$votes.repu))
  #   setdiff(unique(votes.repu$votes.repu), unique(voter.reg.repu$voter.reg.repu))
  #   
  #   differences = setdiff(unique(votes.repu$votes.repu),unique(voter.reg.repu$voter.reg.repu))
  #   voter.reg.diffs = subset(ward.2016.r.l, muni_county %in% differences)
  #   
  #   voter.reg.diffs.join = with(voter.reg.diffs, data.frame(municipality, county, muni_county, reporting_unit))
  #   voter.reg.diffs.join = voter.reg.diffs.join[!duplicated(voter.reg.diffs.join),]
  #   
  #   ward.2016.regvote$ward = gsub(" -","",ward.2016.regvote$ward)
  #   
  #   ###
  #   # Seeing what is different according to Reporting unit level
  #   ###
  #   matched.wards = ward.2016.r.l[match(ward.2016.regvote$ward, ward.2016.r.l$reporting_unit),]
  #   matched.wards = matched.wards[!is.na(matched.wards),]
  #   
  #   
  #   ###
  #   # Municipalities with exceptions
  #   ##
  #   {
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF WRIGHTSTOWN WARD 4"] = "OUTAGAMIE"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 5"] = "ADAMS"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF NEW AUBURN WARD 2"] = "BARRON"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF NEW AUBURN WARD 3"] = "BARRON"
  #     
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 1"] = "MANITOWOC"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 2"] = "MANITOWOC"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 3"] = "MANITOWOC"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 4"] = "MANITOWOC"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 5"] = "MANITOWOC"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 6"] = "MANITOWOC"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 8"] = "MANITOWOC"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 7"] = "CALUMET"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 31"] = "WINNEBAGO"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MENASHA WARD 16"] = "CALUMET"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MENASHA WARD 17"] = "CALUMET"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF EAU CLAIRE WARD 16"] = "CHIPPEWA"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF UNITY WARD 2"] = "CLARK"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF STANLEY WARD 5"] = "CLARK"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF RANDOLPH WARD 3"] = "COLUMBIA"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF DE SOTO WARD 2"] = "CRAWFORD"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF EDGERTON WARD 7"] = "DANE"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 1"] = "DODGE"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 2"] = "DODGE"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 3"] = "DODGE"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 4"] = "DODGE"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 5"] = "DODGE"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 6"] = "DODGE"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 7"] = "DODGE"
  #     
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WAUPUN WARD 9"] = "FOND DU LAC"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WAUPUN WARD 10"] = "FOND DU LAC"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WAUPUN WARD 11"] = "FOND DU LAC"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WAUPUN WARD 12"] = "FOND DU LAC"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF BELLEVILLE WARD 3"] = "GREEN"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF BROOKLYN WARD 2"] = "GREEN"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF BLANCHARDVILLE WARD 2"] = "IOWA"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF LIVINGSTON WARD 2"] = "IOWA"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF MONTFORT WARD 2"] = "IOWA"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF MUSCODA WARD 3"] = "IOWA"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF CAMBRIDGE WARD 1"] = "JEFFERSON"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WHITEWATER WARD 10"] = "JEFFERSON"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WHITEWATER WARD 11"] = "JEFFERSON"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WHITEWATER WARD 12"] = "JEFFERSON"
  #     
  #     
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF HAZEL GREEN WARD 3"] = "LAFAYETTE"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF CUBA CITY WARD 5"] = "LAFAYETTE"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF BIRNAMWOOD WARD 2"] = "MARATHON"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF DORCHESTER WARD 2"] = "MARATHON"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF ABBOTSFORD WARD 1"] = "MARATHON"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF COLBY WARD 1"] = "MARATHON"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARSHFIELD WARD 12"] = "MARATHON"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARSHFIELD WARD 20"] = "MARATHON"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARSHFIELD WARD 21"] = "MARATHON"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARSHFIELD WARD 24"] = "MARATHON"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF NEW LONDON WARD 1"] = "OUTAGAMIE"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF NEW LONDON WARD 2"] = "OUTAGAMIE"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF BAYSIDE WARD 6"] = "OZAUKEE"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF NEWBURG WARD 3"] = "OZAUKEE"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF TURTLE LAKE WARD 2A"] = "POLK"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF MILLADORE WARD 2"] = "PORTAGE"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF BRODHEAD WARD 7"] = "ROCK"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF BRODHEAD WARD 8"] = "ROCK"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF SPRING VALLEY WARD 3"] = "ST. CROIX"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF RIVER FALLS WARD 1"] = "ST. CROIX"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF RIVER FALLS WARD 2"] = "ST. CROIX"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF RIVER FALLS WARD 3"] = "ST. CROIX"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF RIVER FALLS WARD 4"] = "ST. CROIX"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF CAZENOVIA WARD 2"] = "SAUK"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 4"] = "SAUK"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 8"] = "SAUK"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 10"] = "SAUK"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 5"] = "ADAMS"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 9"] = "ADAMS"
  #     
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF PULASKI WARD 4"] = "SHAWANO"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARION WARD 4"] = "SHAWANO"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARION WARD 5"] = "SHAWANO"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARION WARD 6"] = "SHAWANO"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF VIOLA WARD 1"] = "VERNON"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF MUKWONAGO WARD 11"] = "WALWORTH"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF BURLINGTON WARD 9"] = "WALWORTH"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF BURLINGTON WARD 10"] = "WALWORTH"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF BERLIN WARD 7"] = "WAUSHARA"
  #     
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF MUKWONAGO WARD 11"] = "WALWORTH"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF WRIGHTSTOWN WARD 1"] = "BROWN"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF WRIGHTSTOWN WARD 2"] = "BROWN"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF WRIGHTSTOWN WARD 3"] = "BROWN"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 12"] = "CALUMET"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 13"] = "CALUMET"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 14"] = "CALUMET"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 26"] = "CALUMET"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 44"] = "CALUMET"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 45"] = "CALUMET"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 46"] = "CALUMET"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 47"] = "CALUMET"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 31"] = "WINNEBAGO"
  #     ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 32"] = "WINNEBAGO"
  #   }
  #   ward.2016.regvote$muni_county = paste(ward.2016.regvote$municipality, ward.2016.regvote$county)
  #   
  #   setdiff(unique(votes.repu$votes.repu), unique( ward.2016.regvote$muni_county))
  #   
  #   ## Joining the reg.vote summarised municipalities back onto the voting municipalities
  #   ward.2016.regvote.join = with(ward.2016.regvote, data.frame(municipality,muni_county))
  #   colnames(ward.2016.regvote.join)[2] = "muni_county_regvote_agreed"
  #   ward.2016.regvote.join = ward.2016.regvote.join[!duplicated(ward.2016.regvote.join$muni_county_regvote),]
  #   #
  #   # voter.reg.diffs.join = plyr::join(voter.reg.diffs.join,ward.2016.regvote.join,by = "municipality")
  #   #
  #   # back.to.votes.df.join = with(voter.reg.diffs.join, data.frame(muni_county,muni_county_regvote_agreed))
  #   #
  #   # ward.2016.r.l = plyr::join(ward.2016.r.l, back.to.votes.df.join, by = "muni_county")
  #   #
  #   # unique(ward.2016.r.l$muni_county_regvote_agreed)
  #   #
  #   # ward.2016.r.l$muni_county_regvote_agreed = as.character(ward.2016.r.l$muni_county_regvote_agre
  #   
  #   ward.2016.r.l$muni_county_regvote_agreed = as.character(ward.2016.r.l$muni_county)
  #   ward.2016.r.l$muni_county_regvote_agreed = ifelse(is.na(ward.2016.r.l$muni_county_regvote_agreed),
  #                                                     ward.2016.r.l$muni_county,ward.2016.r.l$muni_county_regvote_agreed)
  #   ## Now renaming the regvote column for the final join
  #   colnames(ward.2016.regvote)[6] = "muni_county_regvote_agreed"
  #   
  #   # Trying to match by muni_county level
  #   ## Need to sum up the votes by county
  #   ward.2016.regvote.votes.group = dplyr::group_by(ward.2016.regvote,muni_county_regvote_agreed)
  #   ward.2016.regvote.votes.g = dplyr::summarise(ward.2016.regvote.votes.group,
  #                                                voter_count2 = sum(voter_count)
  #   )
  #   colnames(ward.2016.regvote.votes.g)[2] = "voters.tot"
  #   
  #   # Trying to match by Reporting unit level
  #   ward.2016.r.l = plyr::join(ward.2016.r.l, ward.2016.regvote.votes.g, by = "muni_county_regvote_agreed")
  #   colnames(ward.2016.r.l)[11] = "voters.tot"
  #   
  #   setdiff(ward.2016.regvote.votes.g$muni_county_regvote_agreed,
  #           ward.2016.r.l$muni_county_regvote_agreed)
  #   
  #   setdiff(ward.2016.r.l$muni_county_regvote_agreed,
  #           ward.2016.regvote.votes.g$muni_county_regvote_agreed)
  #   
  #   ward.2016.r.l$turnout = with(ward.2016.r.l, votes.tot / voters.tot)
  #   
  #   ### Joining this back to ward.2016.r.repu
  #   ward.2016.r.l.join = with(ward.2016.r.l,
  #                             data.frame(
  #                               muni_county,voters.tot
  #                             ))
  #   colnames(ward.2016.r.l.join)[1] = "muni_county"
  #   ward.2016.r.l.join = ward.2016.r.l.join[!duplicated(ward.2016.r.l.join$muni_county),]
  #   
  #   ward.2016.r.repu = plyr::join(ward.2016.r.repu, ward.2016.r.l.join, by ="muni_county", match = "first")
  #   
  # } # Adding in registered vote data
} # Registered to vote data 2016 NOT USEFUL ANYMORE
{   # 2012 ward data
  ward.12 = read.csv(
    "ward.2012.votes.csv",
    stringsAsFactors = F,
    header = T,
    strip.white = T
  )
  
  { # Loading 2012 ward vote data
    
    ward.12 = clean_names(ward.12)
    
    colnames(ward.12)[1:3] = c("county","municipality_name","reporting_unit")
    
    ward.12$reporting_unit = toupper(ward.12$reporting_unit)
    ward.12$municipality_name = toupper(ward.12$municipality_name)
    ward.12$county = toupper(ward.12$county)
    
    # The following is fixing a problem I noticed later when trying to match with the 2016 voter df
    ward.12$municipality_name[grep("TOWN OF WINDSOR", ward.12$municipality_name)] = "VILLAGE OF WINDSOR"
    ward.12$reporting_unit = gsub("TOWN OF WINDSOR","VILLAGE OF WINDSOR", ward.12$reporting_unit)
    
    # The town in Maine became a village in 2015, and is now in marathon county
    # ward.12$county[grep("TOWN OF MAINE", ward.12$municipality_name)] = "MARATHON"
    ward.12$municipality_name[grepl("TOWN OF MAINE", ward.12$municipality_name) &
                                grepl("MARATHON", ward.12$county)
                              ] = "VILLAGE OF MAINE"
    # ward.12$reporting_unit = gsub("TOWN OF MAINE","VILLAGE OF MAINE", ward.12$reporting_unit)
    
    # the town of Menasha was retained only for wards (8-10, 11-13). The rest (1-7) became the village of
    # Fox Crossing, Winnebago
    ward.12$municipality_name[grepl("TOWN OF MENASHA WARDS 1, 2, 4, 7", ward.12$reporting_unit)
                              ] = "VILLAGE OF FOX CROSSING"
    ward.12$municipality_name[grepl("TOWN OF MENASHA WARDS 3, 5, 6", ward.12$reporting_unit)
                              ] = "VILLAGE OF FOX CROSSING"
    ward.12$reporting_unit[grepl("TOWN OF MENASHA WARDS 1, 2, 4, 7", ward.12$reporting_unit)
                           ] = "VILLAGE OF FOX CROSSING WARDS 1-2,4,7"
    ward.12$reporting_unit[grepl("TOWN OF MENASHA WARDS 3, 5, 6", ward.12$reporting_unit)
                           ] = "VILLAGE OF FOX CROSSING WARDS 3,5-6"
    
    ward.12$muni_county = paste(ward.12$municipality_name, ward.12$county)
    ward.12$reporting_unit = paste(ward.12$reporting_unit, ward.12$county)
    
    ward.12.look = with(ward.12, data.frame(county, municipality_name,
                                            reporting_unit, muni_county))
    colnames(ward.12.look) = c("county","municipality","reporting_unit",
                               "muni_county")
    
    ward.12.v = ward.12
    ward.12.v$county = NULL
    ward.12.v$municipality_name = NULL
    ward.12.v$total_votes = NULL
    # ward.12.v$reporting_unit = NULL
    ward.12.v$congressional = NULL
    ward.12.v$senatedistrict = NULL
    ward.12.v$assemblydistrict = NULL
    
    ward.12.l <- ward.12.v
    ward.12.l$muni_county = NULL
    ward.12.l$reporting_unit <- NULL
    ward.12.l = gather(ward.12.l, cand, votes.rec)
    ward.12.l$reporting_unit <- rep(ward.12.v$reporting_unit)
    # colnames(ward.12.l) = c("reporting_unit", "cand", "votes.rec")
    
    ward.12.l$cand.group = NA
    ward.12.l$cand.group[grep("obama",ward.12.l$cand)] = "democrat"
    ward.12.l$cand.group[grep("romney",ward.12.l$cand)] = "republican"
    ward.12.l$cand.group[grep("virgil",ward.12.l$cand)] = "other"
    ward.12.l$cand.group[grep("johnson",ward.12.l$cand)] = "other"
    ward.12.l$cand.group[grep("gloria",ward.12.l$cand)] = "other"
    ward.12.l$cand.group[grep("jerry",ward.12.l$cand)] = "other"
    ward.12.l$cand.group[grep("stein",ward.12.l$cand)] = "other"
    ward.12.l$cand.group[grep("rocky",ward.12.l$cand)] = "other"
    ward.12.l$cand.group[grep("roseanne",ward.12.l$cand)] = "other"
    ward.12.l$cand.group[grep("scatter",ward.12.l$cand)] = "other"
    
    ward.12.l$votes.rec = as.integer(ward.12.l$votes.rec)
    
    # Joining back county and municipality names
    ward.12.l = plyr::join(ward.12.l, ward.12.look, by = "reporting_unit", match = "first")
    
    tot.votes.look.group = dplyr::group_by(ward.12.l,reporting_unit)
    tot.votes.look = dplyr::summarise(tot.votes.look.group,
                                      votes.tot = sum(as.integer(votes.rec),na.rm=T)
    )
    
    ward.12.l = plyr::join(ward.12.l,tot.votes.look,by="reporting_unit")
    ward.12.l$votes.perc = (ward.12.l$votes.rec / ward.12.l$votes.tot) * 100
    
    ward.12.l$muni_county = paste(ward.12.l$municipality, ward.12.l$county)
    
    ward.12.repu.group = dplyr::group_by(ward.12.l,reporting_unit,municipality,muni_county,county,cand.group)
    ward.12.repu = dplyr::summarise(ward.12.repu.group,
                                    votes.rec2 = sum(votes.rec,na.rm=T),
                                    votes.tot2 = mean(votes.tot,na.rm=T)
    )
    colnames(ward.12.repu)[6] = "votes.rec"
    colnames(ward.12.repu)[7] = "votes.tot"
    
    ward.12.repu$votes.perc = (ward.12.repu$votes.rec / ward.12.repu$votes.tot) * 100
    
    ward.12.repu = ward.12.repu[!is.na(ward.12.repu$cand.group),]
  } # 2012 data basic
  {  # Now joining the 2012 data back into the 2016 data
    grep("WARDS",ward.2016.r.repu$reporting_unit)
    
    ward.2016.r.repu$reporting_unit_orig =ward.2016.r.repu$reporting_unit
    ward.12.repu$reporting_unit_orig =ward.12.repu$reporting_unit 
    
    ward.2016.r.repu$reporting_unit = gsub("WARDS","WARD",ward.2016.r.repu$reporting_unit_orig)
    ward.12.repu$reporting_unit = gsub("WARDS","WARD",ward.12.repu$reporting_unit_orig)
    ward.2016.r.repu$reporting_unit = gsub("  "," ",ward.2016.r.repu$reporting_unit)
    ward.12.repu$reporting_unit = gsub("  "," ",ward.12.repu$reporting_unit)
    ward.2016.r.repu$reporting_unit = gsub("&","-",ward.2016.r.repu$reporting_unit)
    ward.12.repu$reporting_unit = gsub("&","-",ward.12.repu$reporting_unit)
    
    ward.2016.r.repu$reporting_unit = gsub("AND","-",ward.2016.r.repu$reporting_unit)
    ward.12.repu$reporting_unit = gsub("AND","-",ward.12.repu$reporting_unit)
    
    ward.2016.r.repu$reporting_unit = gsub(",","-",ward.2016.r.repu$reporting_unit)
    ward.12.repu$reporting_unit = gsub(",","-",ward.12.repu$reporting_unit)
    ward.2016.r.repu$reporting_unit = gsub(" - ","-",ward.2016.r.repu$reporting_unit)
    ward.12.repu$reporting_unit = gsub(" - ","-",ward.12.repu$reporting_unit)
    ward.2016.r.repu$reporting_unit = gsub(" -","-",ward.2016.r.repu$reporting_unit)
    ward.12.repu$reporting_unit = gsub(" -","-",ward.12.repu$reporting_unit)
    ward.2016.r.repu$reporting_unit = gsub("- ","-",ward.2016.r.repu$reporting_unit)
    ward.12.repu$reporting_unit = gsub("- ","-",ward.12.repu$reporting_unit)
    
    ward.2016.r.repu$reporting_unit = gsub(" ","-",ward.2016.r.repu$reporting_unit)
    ward.12.repu$reporting_unit = gsub(" ","-",ward.12.repu$reporting_unit)
    
    ward.2016.r.repu$reporting_unit = gsub("WD","",ward.2016.r.repu$reporting_unit)
    ward.12.repu$reporting_unit = gsub("WD","",ward.12.repu$reporting_unit)
    
    ward.2016.r.repu$reporting_unit = gsub("COMBINED","",ward.2016.r.repu$reporting_unit)
    ward.12.repu$reporting_unit = gsub("COMBINED","",ward.12.repu$reporting_unit)
    
    ward.2016.r.repu$reporting_unit = gsub("--","-",ward.2016.r.repu$reporting_unit)
    ward.12.repu$reporting_unit = gsub("--","-",ward.12.repu$reporting_unit)
    
    setdiff(unique(ward.12.repu$reporting_unit), unique(ward.2016.r.repu$reporting_unit))
    setdiff(unique(ward.2016.r.repu$reporting_unit), unique(ward.12.repu$reporting_unit))
    
    # Village of Harrison Calumet did not exist in 2012, was split off from the town in 2013.
    # It is impossible to know the boundaries exactly.
    # Therefore the swing from 2012 to 2016 cannot be accurately calculated.
    
    # Same of the town / village of Somers, Kenosha. The town / village is completely mixed and messed up
    
    # the town of Menasha was retained only for wards (8-10, 11-13). The rest (1-7) became the village of
    # Fox Crossing, Winnebago
    
    differences = setdiff(unique(ward.2016.r.repu$reporting_unit),unique(ward.12.repu$reporting_unit))
    vote.2012.diffs = subset(ward.2016.r.repu, reporting_unit %in% differences)
    
    vote.2012.diffs.join = with(vote.2012.diffs, data.frame(municipality, county, reporting_unit))
    vote.2012.diffs.join = vote.2012.diffs.join[!duplicated(vote.2012.diffs.join),]
    
    # ## Joining the reg.vote summarised municipalities back onto the voting municipalities
    # ward.12.repu.join = with(ward.12.repu, data.frame(municipality,reporting_unit))
    # colnames(ward.12.repu.join)[2] = "reporting_unit_2012_agreed"
    # ward.12.repu.join = ward.12.repu.join[!duplicated(ward.12.repu.join$reporting_unit_2012),]
    # 
    # 
    # 
    # vote.2012.diffs.join = plyr::join(vote.2012.diffs.join,ward.12.repu.join,by = "reporting_unit")
    # 
    # back.to.votes.df.join = with(vote.2012.diffs.join, data.frame(reporting_unit,reporting_unit_2012_agreed))
    # 
    # ward.2016.r.repu = plyr::join(ward.2016.r.repu, back.to.votes.df.join, by = "reporting_unit")
    # 
    # ward.2016.r.repu$reporting_unit_2012_agreed = as.character(ward.2016.r.repu$reporting_unit_2012_agreed)
    # 
    # ward.2016.r.repu$reporting_unit_2012_agreed = ifelse(is.na(ward.2016.r.repu$reporting_unit_2012_agreed),
    #                                                  ward.2016.r.repu$reporting_unit,ward.2016.r.repu$reporting_unit_2012_agreed)
    # 
    ## Now renaming the 2012 column for the final join
    # colnames(ward.12.repu)[2] = "muni_county_2012_agreed"
    
    ## Need to sum up the votes by county in 2012
    ward.12.repu.votes.group = dplyr::group_by(ward.12.repu,cand.group,reporting_unit,reporting_unit_orig,municipality, county)
    ward.12.repu.votes.g = dplyr::summarise(ward.12.repu.votes.group,
                                            votes.rec2012 = sum(votes.rec),
                                            votes.tot2012 = mean(votes.tot)
                                            
    )
    
    # Melting the 2012 df to put the candidates on top
    ward.12.repu.votes.g.melt = melt(ward.12.repu.votes.g)
    ward.12.repu.votes.g.cast = dcast(ward.12.repu.votes.g.melt,
                                      reporting_unit ~ cand.group + variable, sum
    )
    ward.12.repu.votes.g.cast$democrat_votes.tot2012 = NULL
    ward.12.repu.votes.g.cast$republican_votes.tot2012 = NULL
    
    ward.12.repu.votes.g.cast.mean = dcast(ward.12.repu.votes.g.melt,
                                           reporting_unit ~ cand.group + variable, mean
    )
    ward.12.repu.votes.g.cast$other_votes.tot2012 = ward.12.repu.votes.g.cast.mean$other_votes.tot2012
    colnames(ward.12.repu.votes.g.cast)[grepl("votes.tot",colnames(ward.12.repu.votes.g.cast))] =
      "votes.tot2012"
    
    # Also need to melt the 2016 mun df by cand.group
    ward.2016.r.repu.votes.g.melt = melt(ward.2016.r.repu)
    ward.2016.r.repu.votes.g.cast = dcast(ward.2016.r.repu.votes.g.melt,
                                          reporting_unit + reporting_unit_orig + muni_county + municipality + county ~
                                            cand.group + variable, sum
    )
    
    ward.2016.r.repu.votes.g.cast$democrat_votes.tot = NULL
    ward.2016.r.repu.votes.g.cast$republican_votes.tot = NULL
    ward.2016.r.repu.votes.g.cast$democrat_voters.tot = NULL
    ward.2016.r.repu.votes.g.cast$republican_voters.tot = NULL
    
    ward.2016.r.repu.votes.g.cast.mean = dcast(ward.2016.r.repu.votes.g.melt,
                                               reporting_unit + reporting_unit_orig + muni_county + municipality + county ~
                                                 cand.group + variable, mean
    )
    
    
    ward.2016.r.repu.votes.g.cast$other_votes.tot = ward.2016.r.repu.votes.g.cast.mean$other_votes.tot
    ward.2016.r.repu.votes.g.cast$other_voters.tot = ward.2016.r.repu.votes.g.cast.mean$other_voters.tot
    colnames(ward.2016.r.repu.votes.g.cast)[grepl("votes.tot",colnames(ward.2016.r.repu.votes.g.cast))] =
      "votes.tot2016"
    colnames(ward.2016.r.repu.votes.g.cast)[grepl("voters.tot",colnames(ward.2016.r.repu.votes.g.cast))] =
      "voters.tot2016"
    
    # Finally joining the dfs together
    
    ward.2016.r.repu.g = plyr::join(ward.2016.r.repu.votes.g.cast, ward.12.repu.votes.g.cast, by = "reporting_unit")
    
    # 2012 turnout is calculated on the (big?) assumption that the number of registered
    # voters was the same in 2012 as in 2016
    # ward.2016.r.repu.g$turnout.2012 =
    # with(ward.2016.r.repu.g, votes.tot2012 / voters.tot2016) * 100
    
    ward.2016.r.repu.g$dem.perc2012 =
      with(ward.2016.r.repu.g,
           democrat_votes.rec2012 / votes.tot2012) * 100
    ward.2016.r.repu.g$rep.perc2012 =
      with(ward.2016.r.repu.g,
           republican_votes.rec2012 / votes.tot2012) * 100
    ward.2016.r.repu.g$oth.perc2012 =
      with(ward.2016.r.repu.g,
           other_votes.rec2012 / votes.tot2012) * 100
    
    ward.2016.r.repu.g$dem.change.num =
      with(ward.2016.r.repu.g,
           democrat_votes.rec - democrat_votes.rec2012)
    ward.2016.r.repu.g$rep.change.num =
      with(ward.2016.r.repu.g,
           republican_votes.rec - republican_votes.rec2012)
    ward.2016.r.repu.g$oth.change.num =
      with(ward.2016.r.repu.g,
           other_votes.rec - other_votes.rec2012)
    
    ward.2016.r.repu.g$dem.change.perc =
      with(ward.2016.r.repu.g,
           democrat_votes.perc -  dem.perc2012)
    ward.2016.r.repu.g$rep.change.perc =
      with(ward.2016.r.repu.g,
           republican_votes.perc - rep.perc2012)
    ward.2016.r.repu.g$oth.change.perc =
      with(ward.2016.r.repu.g,
           other_votes.perc - oth.perc2012)
    # 
    # ward.2016.r.repu.g$turnout.2016.perc =
    #   with(ward.2016.r.repu.g,
    #        votes.tot2016 / voters.tot2016) * 100
    
    # ward.2016.r.repu.g$turnout.change.perc =
    #   with(ward.2016.r.repu.g,
    #        turnout.2016.perc - turnout.2012)
    
    
    repunit.2016.2012.r = ward.2016.r.repu.g
    
    repunit.2016.2012.r$demwinner = NA
    repunit.2016.2012.r$demwinner = ifelse(repunit.2016.2012.r$democrat_votes.perc >
                                             repunit.2016.2012.r$republican_votes.perc &
                                             !is.na(repunit.2016.2012.r$democrat_votes.perc), 1,0)
    repunit.2016.2012.r$demwinner = as.integer(repunit.2016.2012.r$demwinner)
    
    repunit.2016.2012.r$demwinner2012 = NA
    repunit.2016.2012.r$demwinner2012 = ifelse(repunit.2016.2012.r$dem.perc2012 >
                                                 repunit.2016.2012.r$rep.perc2012 &
                                                 !is.na(repunit.2016.2012.r$dem.perc2012), 1,0)
    repunit.2016.2012.r$demwinner2012 = as.integer(repunit.2016.2012.r$demwinner2012)
  } # Now joining the 2012 data back into the 2016 data
} # Loading 2012 ward vote data
{   # 2008 ward data
  ward.08 = read.csv(
    "C:/Users/s_cas/Dropbox/Perso/2016 voting election county results/Wisconsin/All prep together/2008 results/2008_FallElection_President_WardbyWard.csv",
    stringsAsFactors = F,
    header = T,
    strip.white = T
  )
  
  { # Loading 2008 ward vote data
    
    ward.08 = ward.08[,11:27]
    
    ward.08 = clean_names(ward.08)
    
    ward.08$reporting_unit = ifelse(ward.08$reporting_unit == "","Ward 1",ward.08$reporting_unit)
    ward.08$municipality_name = with(ward.08, paste(municipality_type, "of", municipality,sep=" "))
    ward.08$reporting_unit = with(ward.08, paste(municipality_name, reporting_unit))
    
    # colnames(ward.08)[1:3] = c("county","municipality_name","reporting_unit")
    
    ward.08$reporting_unit = toupper(ward.08$reporting_unit)
    ward.08$municipality_name = toupper(ward.08$municipality_name)
    ward.08$county = toupper(ward.08$county)
    
    # The following is fixing a problem I noticed later when trying to match with the 2016 voter df
    ward.08$municipality_name[grep("TOWN OF WINDSOR", ward.08$municipality_name)] = "VILLAGE OF WINDSOR"
    ward.08$reporting_unit = gsub("TOWN OF WINDSOR","VILLAGE OF WINDSOR", ward.08$reporting_unit)
    
    # The town in Maine became a village in 2015, and is now in marathon county
    # ward.08$county[grep("TOWN OF MAINE", ward.08$municipality_name)] = "MARATHON"
    ward.08$municipality_name[grepl("TOWN OF MAINE", ward.08$municipality_name) &
                                grepl("MARATHON", ward.08$county)
                              ] = "VILLAGE OF MAINE"
    # ward.08$reporting_unit = gsub("TOWN OF MAINE","VILLAGE OF MAINE", ward.08$reporting_unit)
    
    # the town of Menasha was retained only for wards (8-10, 11-13). The rest (1-7) became the village of
    # Fox Crossing, Winnebago
    ward.08$municipality_name[grepl("TOWN OF MENASHA WARDS 1, 2, 4, 7", ward.08$reporting_unit)
                              ] = "VILLAGE OF FOX CROSSING"
    ward.08$municipality_name[grepl("TOWN OF MENASHA WARDS 3, 5, 6", ward.08$reporting_unit)
                              ] = "VILLAGE OF FOX CROSSING"
    ward.08$reporting_unit[grepl("TOWN OF MENASHA WARDS 1, 2, 4, 7", ward.08$reporting_unit)
                           ] = "VILLAGE OF FOX CROSSING WARDS 1-2,4,7"
    ward.08$reporting_unit[grepl("TOWN OF MENASHA WARDS 3, 5, 6", ward.08$reporting_unit)
                           ] = "VILLAGE OF FOX CROSSING WARDS 3,5-6"
    
    ward.08$muni_county = paste(ward.08$municipality_name, ward.08$county)
    ward.08$reporting_unit = paste(ward.08$reporting_unit, ward.08$county)
    
    ward.08.look = with(ward.08, data.frame(county, municipality_name,
                                            reporting_unit, muni_county))
    colnames(ward.08.look) = c("county","municipality","reporting_unit",
                               "muni_county")
    
    ward.08.v = ward.08
    # ward.08.v$county = NULL
    ward.08.v$county = NULL
    ward.08.v$municipality_name = NULL
    ward.08.v$municipality_type = NULL
    ward.08.v$municipalityno = NULL
    ward.08.v$municipality = NULL
    # ward.08.v$reporting_unit = NULL
    ward.08.v$order = NULL
    ward.08.v$hindi = NULL
    # ward.08.v$assemblydistrict = NULL
    
    ward.08.l <- ward.08.v
    ward.08.l$muni_county = NULL
    ward.08.l$reporting_unit <- NULL
    ward.08.l = gather(ward.08.l, cand, votes.rec)
    ward.08.l$reporting_unit <- rep(ward.08.v$reporting_unit)
    # colnames(ward.08.l) = c("reporting_unit", "cand", "votes.rec")
    
    ward.08.l$cand.group = NA
    ward.08.l$cand.group[grep("obama",ward.08.l$cand)] = "democrat"
    ward.08.l$cand.group[grep("mc_cain",ward.08.l$cand)] = "republican"
    ward.08.l$cand.group[grep("rosa",ward.08.l$cand)] = "other"
    ward.08.l$cand.group[grep("wayne",ward.08.l$cand)] = "other"
    ward.08.l$cand.group[grep("stewart",ward.08.l$cand)] = "other"
    ward.08.l$cand.group[grep("robert",ward.08.l$cand)] = "other"
    ward.08.l$cand.group[grep("gonzalez",ward.08.l$cand)] = "other"
    ward.08.l$cand.group[grep("castle",ward.08.l$cand)] = "other"
    ward.08.l$cand.group[grep("darrell",ward.08.l$cand)] = "other"
    ward.08.l$cand.group[grep("klimisch",ward.08.l$cand)] = "other"
    ward.08.l$cand.group[grep("scatter",ward.08.l$cand)] = "other"
    
    ward.08.l$votes.rec = as.integer(ward.08.l$votes.rec)
    
    # Joining back county and municipality names
    ward.08.l = plyr::join(ward.08.l, ward.08.look, by = "reporting_unit", match = "first")
    
    tot.votes.look.group = dplyr::group_by(ward.08.l,reporting_unit)
    tot.votes.look = dplyr::summarise(tot.votes.look.group,
                                      votes.tot = sum(as.integer(votes.rec),na.rm=T)
    )
    
    ward.08.l = plyr::join(ward.08.l,tot.votes.look,by="reporting_unit")
    ward.08.l$votes.perc = (ward.08.l$votes.rec / ward.08.l$votes.tot) * 100
    
    ward.08.l$muni_county = paste(ward.08.l$municipality, ward.08.l$county)
    
    ward.08.repu.group = dplyr::group_by(ward.08.l,reporting_unit,municipality,muni_county,county,cand.group)
    ward.08.repu = dplyr::summarise(ward.08.repu.group,
                                    votes.rec2 = sum(votes.rec,na.rm=T),
                                    votes.tot2 = mean(votes.tot,na.rm=T)
    )
    colnames(ward.08.repu)[6] = "votes.rec"
    colnames(ward.08.repu)[7] = "votes.tot"
    
    ward.08.repu$votes.perc = (ward.08.repu$votes.rec / ward.08.repu$votes.tot) * 100
    
    ward.08.repu = ward.08.repu[!is.na(ward.08.repu$cand.group),]
  } # 2008 data basic
  {  # Now joining the 2008 data back into the 2016 data
    # grep("WARDS",ward.2016.r.repu$reporting_unit)
    
    ward.2016.r.repu$reporting_unit_orig =ward.2016.r.repu$reporting_unit_orig
    ward.08.repu$reporting_unit_orig =ward.08.repu$reporting_unit 
    
    ward.2016.r.repu$reporting_unit = gsub("WARDS","WARD",ward.2016.r.repu$reporting_unit_orig)
    ward.08.repu$reporting_unit = gsub("WARDS","WARD",ward.08.repu$reporting_unit_orig)
    ward.2016.r.repu$reporting_unit = gsub("  "," ",ward.2016.r.repu$reporting_unit)
    ward.08.repu$reporting_unit = gsub("  "," ",ward.08.repu$reporting_unit)
    ward.2016.r.repu$reporting_unit = gsub("&","-",ward.2016.r.repu$reporting_unit)
    ward.08.repu$reporting_unit = gsub("&","-",ward.08.repu$reporting_unit)
    
    ward.2016.r.repu$reporting_unit = gsub("AND","-",ward.2016.r.repu$reporting_unit)
    ward.08.repu$reporting_unit = gsub("AND","-",ward.08.repu$reporting_unit)
    
    ward.2016.r.repu$reporting_unit = gsub(",","-",ward.2016.r.repu$reporting_unit)
    ward.08.repu$reporting_unit = gsub(",","-",ward.08.repu$reporting_unit)
    ward.2016.r.repu$reporting_unit = gsub(" - ","-",ward.2016.r.repu$reporting_unit)
    ward.08.repu$reporting_unit = gsub(" - ","-",ward.08.repu$reporting_unit)
    ward.2016.r.repu$reporting_unit = gsub(" -","-",ward.2016.r.repu$reporting_unit)
    ward.08.repu$reporting_unit = gsub(" -","-",ward.08.repu$reporting_unit)
    ward.2016.r.repu$reporting_unit = gsub("- ","-",ward.2016.r.repu$reporting_unit)
    ward.08.repu$reporting_unit = gsub("- ","-",ward.08.repu$reporting_unit)
    
    ward.2016.r.repu$reporting_unit = gsub(" ","-",ward.2016.r.repu$reporting_unit)
    ward.08.repu$reporting_unit = gsub(" ","-",ward.08.repu$reporting_unit)
    
    ward.2016.r.repu$reporting_unit = gsub("WD","",ward.2016.r.repu$reporting_unit)
    ward.08.repu$reporting_unit = gsub("WD","",ward.08.repu$reporting_unit)
    
    ward.2016.r.repu$reporting_unit = gsub("COMBINED","",ward.2016.r.repu$reporting_unit)
    ward.08.repu$reporting_unit = gsub("COMBINED","",ward.08.repu$reporting_unit)
    
    ward.2016.r.repu$reporting_unit = gsub("--","-",ward.2016.r.repu$reporting_unit)
    ward.08.repu$reporting_unit = gsub("--","-",ward.08.repu$reporting_unit)
    
    setdiff(unique(ward.08.repu$reporting_unit), unique(ward.2016.r.repu$reporting_unit))
    setdiff(unique(ward.2016.r.repu$reporting_unit), unique(ward.08.repu$reporting_unit))
    
    # Village of Harrison Calumet did not exist in 2008, was split off from the town in 2013.
    # It is impossible to know the boundaries exactly.
    # Therefore the swing from 2008 to 2016 cannot be accurately calculated.
    
    # Same of the town / village of Somers, Kenosha. The town / village is completely mixed and messed up
    
    # the town of Menasha was retained only for wards (8-10, 11-13). The rest (1-7) became the village of
    # Fox Crossing, Winnebago
    
    differences = setdiff(unique(ward.2016.r.repu$reporting_unit),unique(ward.08.repu$reporting_unit))
    vote.2008.diffs = subset(ward.2016.r.repu, reporting_unit %in% differences)
    
    vote.2008.diffs.join = with(vote.2008.diffs, data.frame(municipality, county, reporting_unit))
    vote.2008.diffs.join = vote.2008.diffs.join[!duplicated(vote.2008.diffs.join),]
    
    # ## Joining the reg.vote summarised municipalities back onto the voting municipalities
    # ward.08.repu.join = with(ward.08.repu, data.frame(municipality,reporting_unit))
    # colnames(ward.08.repu.join)[2] = "reporting_unit_2008_agreed"
    # ward.08.repu.join = ward.08.repu.join[!duplicated(ward.08.repu.join$reporting_unit_2008),]
    # 
    # 
    # 
    # vote.2008.diffs.join = plyr::join(vote.2008.diffs.join,ward.08.repu.join,by = "reporting_unit")
    # 
    # back.to.votes.df.join = with(vote.2008.diffs.join, data.frame(reporting_unit,reporting_unit_2008_agreed))
    # 
    # ward.2016.r.repu = plyr::join(ward.2016.r.repu, back.to.votes.df.join, by = "reporting_unit")
    # 
    # ward.2016.r.repu$reporting_unit_2008_agreed = as.character(ward.2016.r.repu$reporting_unit_2008_agreed)
    # 
    # ward.2016.r.repu$reporting_unit_2008_agreed = ifelse(is.na(ward.2016.r.repu$reporting_unit_2008_agreed),
    #                                                  ward.2016.r.repu$reporting_unit,ward.2016.r.repu$reporting_unit_2008_agreed)
    # 
    ## Now renaming the 2008 column for the final join
    # colnames(ward.08.repu)[2] = "muni_county_2008_agreed"
    
    ## Need to sum up the votes by county in 2008
    ward.08.repu.votes.group = dplyr::group_by(ward.08.repu,cand.group,reporting_unit,reporting_unit_orig,municipality, county)
    ward.08.repu.votes.g = dplyr::summarise(ward.08.repu.votes.group,
                                            votes.rec2008 = sum(votes.rec),
                                            votes.tot2008 = mean(votes.tot)
                                            
    )
    
    # Melting the 2008 df to put the candidates on top
    ward.08.repu.votes.g.melt = melt(ward.08.repu.votes.g)
    ward.08.repu.votes.g.cast = dcast(ward.08.repu.votes.g.melt,
                                      reporting_unit ~ cand.group + variable, sum
    )
    ward.08.repu.votes.g.cast$democrat_votes.tot2008 = NULL
    ward.08.repu.votes.g.cast$republican_votes.tot2008 = NULL
    
    ward.08.repu.votes.g.cast.mean = dcast(ward.08.repu.votes.g.melt,
                                           reporting_unit ~ cand.group + variable, mean
    )
    ward.08.repu.votes.g.cast$other_votes.tot2008 = ward.08.repu.votes.g.cast.mean$other_votes.tot2008
    colnames(ward.08.repu.votes.g.cast)[grepl("votes.tot",colnames(ward.08.repu.votes.g.cast))] =
      "votes.tot2008"
    
    # Also need to melt the 2016 mun df by cand.group
    ward.2016.r.repu.votes.g.melt = melt(ward.2016.r.repu)
    ward.2016.r.repu.votes.g.cast = dcast(ward.2016.r.repu.votes.g.melt,
                                          reporting_unit + reporting_unit_orig + muni_county + municipality + county ~
                                            cand.group + variable, sum
    )
    
    ward.2016.r.repu.votes.g.cast$democrat_votes.tot = NULL
    ward.2016.r.repu.votes.g.cast$republican_votes.tot = NULL
    ward.2016.r.repu.votes.g.cast$democrat_voters.tot = NULL
    ward.2016.r.repu.votes.g.cast$republican_voters.tot = NULL
    
    ward.2016.r.repu.votes.g.cast.mean = dcast(ward.2016.r.repu.votes.g.melt,
                                               reporting_unit + reporting_unit_orig + muni_county + municipality + county ~
                                                 cand.group + variable, mean
    )
    ward.2016.r.repu.votes.g.cast$other_votes.tot = ward.2016.r.repu.votes.g.cast.mean$other_votes.tot
    ward.2016.r.repu.votes.g.cast$other_voters.tot = ward.2016.r.repu.votes.g.cast.mean$other_voters.tot
    colnames(ward.2016.r.repu.votes.g.cast)[grepl("votes.tot",colnames(ward.2016.r.repu.votes.g.cast))] =
      "votes.tot2016"
    colnames(ward.2016.r.repu.votes.g.cast)[grepl("voters.tot",colnames(ward.2016.r.repu.votes.g.cast))] =
      "voters.tot2016"
    
    # Finally joining the dfs together
    
    ward.2016.r.repu.g = plyr::join(ward.2016.r.repu.votes.g.cast, ward.08.repu.votes.g.cast, by = "reporting_unit")
    
    # 2008 turnout is calculated on the (big?) assumption that the number of registered
    # voters was the same in 2008 as in 2016
    # ward.2016.r.repu.g$turnout.2008 =
    # with(ward.2016.r.repu.g, votes.tot2008 / voters.tot2016) * 100
    
    ward.2016.r.repu.g$dem.perc2008 =
      with(ward.2016.r.repu.g,
           democrat_votes.rec2008 / votes.tot2008) * 100
    ward.2016.r.repu.g$rep.perc2008 =
      with(ward.2016.r.repu.g,
           republican_votes.rec2008 / votes.tot2008) * 100
    ward.2016.r.repu.g$oth.perc2008 =
      with(ward.2016.r.repu.g,
           other_votes.rec2008 / votes.tot2008) * 100
    
    ward.2016.r.repu.g$dem.change.2008.num =
      with(ward.2016.r.repu.g,
           democrat_votes.rec - democrat_votes.rec2008)
    ward.2016.r.repu.g$rep.change.2008.num =
      with(ward.2016.r.repu.g,
           republican_votes.rec - republican_votes.rec2008)
    ward.2016.r.repu.g$oth.change.2008.num =
      with(ward.2016.r.repu.g,
           other_votes.rec - other_votes.rec2008)
    
    ward.2016.r.repu.g$dem.change.2008.perc =
      with(ward.2016.r.repu.g,
           democrat_votes.perc -  dem.perc2008)
    ward.2016.r.repu.g$rep.change.2008.perc =
      with(ward.2016.r.repu.g,
           republican_votes.perc - rep.perc2008)
    ward.2016.r.repu.g$oth.change.2008.perc =
      with(ward.2016.r.repu.g,
           other_votes.perc - oth.perc2008)
    # 
    # ward.2016.r.repu.g$turnout.2016.perc =
    #   with(ward.2016.r.repu.g,
    #        votes.tot2016 / voters.tot2016) * 100
    
    # ward.2016.r.repu.g$turnout.change.perc =
    #   with(ward.2016.r.repu.g,
    #        turnout.2016.perc - turnout.2008)
    
    
    repunit.2016.2008.r = ward.2016.r.repu.g
    
    repunit.2016.2008.r$demwinner = NA
    repunit.2016.2008.r$demwinner = ifelse(repunit.2016.2008.r$democrat_votes.perc >
                                             repunit.2016.2008.r$republican_votes.perc &
                                             !is.na(repunit.2016.2008.r$democrat_votes.perc), 1,0)
    repunit.2016.2008.r$demwinner = as.integer(repunit.2016.2008.r$demwinner)
    
    repunit.2016.2008.r$demwinner2008 = NA
    repunit.2016.2008.r$demwinner2008 = ifelse(repunit.2016.2008.r$dem.perc2008 >
                                                 repunit.2016.2008.r$rep.perc2008 &
                                                 !is.na(repunit.2016.2008.r$dem.perc2008), 1,0)
    repunit.2016.2008.r$demwinner2008 = as.integer(repunit.2016.2008.r$demwinner2008)
  } # Now joining the 2008 data back into the 2016 data
} # Loading 2008 ward vote data

# Rejoining the county data to the ward 2016 2012 data
{
  repunit.2016.2012.r = plyr::join(repunit.2016.2012.r, ward.2016.look, by = "reporting_unit", match = "first")
  
  repunit.2016.2008.r = plyr::join(repunit.2016.2008.r, ward.2016.look, by = "reporting_unit", match = "first")
}

# Loading voting machine data 2016 recount vs 2012 (Wisconsin official) ---------------------------------------------
# This file from here: http://elections.wi.gov/elections-voting/voting-equipment/voting-equipment-use
# Voting machine equipment for 13 September 2016
vot.equip = read.xlsx(
  "voting_equipment_by_municipality_09_2016_xlsx_78114.xlsx1207162619.xlsx",
  sheetIndex = 1,
  header = TRUE,
  colClasses = NA,
  stringsAsFactors = F
)
colnames(vot.equip) = c(
  "county.long",
  "municipality",
  "machine.vendor.dealer.model",
  "accessible.vendor.dealer.model"
)

# The following is fixing a problem I noticed later when trying to match with the voter df
vot.equip$municipality[grep("TOWN OF WINDSOR",
                            vot.equip$municipality)] = "VILLAGE OF WINDSOR"

# Following from
# http://stackoverflow.com/questions/15895050/using-gsub-to-extract-character-string-before-white-space-in-r
county.split.att2 = gsub(" COUNTY.*$", "", vot.equip$county.long)
vot.equip$county = county.split.att2
vot.equip = vot.equip[!is.na(vot.equip$county.long), ]

vot.equip$use.machines = NA
vot.equip$use.machines = ifelse(vot.equip$machine.vendor.dealer.model == "None ", 0, 1)

vot.equip$machine.vendor.dealer.spec = gsub("Optech-", "Optech ", vot.equip$machine.vendor.dealer.model)
vot.equip$machine.vendor.dealer.spec = gsub("Optech/", "Optech ", vot.equip$machine.vendor.dealer.spec)
vot.equip$machine.vendor.dealer.spec = gsub(" .*$", "", vot.equip$machine.vendor.dealer.spec)
vot.equip$machine.vendor.dealer.spec = gsub("None", NA, vot.equip$machine.vendor.dealer.spec)

vot.equip.county.grouped = dplyr::group_by(vot.equip, county)
vot.equip.county = dplyr::summarise(
  vot.equip.county.grouped,
  use.machines.prop = sum(use.machines, na.rm = TRUE) /
    length(use.machines)
)

# vot.equip.withmachines = subset(vot.equip, use.machines > 0)
vot.equip.withmachines = vot.equip

vot.equip.county.grouped.machines = dplyr::group_by(vot.equip.withmachines, county)
vot.equip.county.machines = dplyr::summarise(
  vot.equip.county.grouped.machines,
  use.machines.prop = sum(use.machines, na.rm = TRUE) /
    length(use.machines)
)

vot.equip.county.machines$use.machines.prop = NULL

max.vendor = as.character(1:length(vot.equip.county.machines$county))
for (i in 1:length(unique(vot.equip.county.machines$county))) {
  a = count(vot.equip.withmachines$machine.vendor.dealer.spec[vot.equip.withmachines$county ==
                                                                vot.equip.withmachines$county[i]])
  a$x = as.character(a$x)
  max.vendor[i] = a$x[a$freq == max(a$freq)]
}

vot.equip.county.machines$machine.most.used = max.vendor
vot.equip.county = plyr::join(vot.equip.county,
                              vot.equip.county.machines,
                              by = "county",
                              match = "first")

test = tolower(vot.equip.county$county)
test = capwords(test)
vot.equip.county$county = test

machine.join = data.frame(vot.equip.county$county, vot.equip.county$use.machines.prop, vot.equip.county$machine.most.used)
colnames(machine.join) = c("county","use.machines.prop","machine.most.used")

machine.join$all.machines = with(machine.join, ifelse(use.machines.prop > 0.75, "Mostly optical scanners",
                                                      "Some or no optical scanners"))
machine.join$county = as.character(machine.join$county)
machine.join$county[machine.join$county == "Fond Du Lac"] = "Fond du Lac"

rm(vote.equip.county.machines,vote.equip.county.grouped,voting.age.people,test,
   historical.turnout.join,a)

#### Now joining this to the ward data
vot.equip.withmachines$muni_county = gsub(" -.*", "",vot.equip.withmachines$municipality)

vot.equip.withmachines$muni_county = paste(vot.equip.withmachines$muni_county, vot.equip.withmachines$county)
vot.equip.withmachines$muni_county = gsub("  "," ",vot.equip.withmachines$muni_county)


vot.equip.join = with(vot.equip.withmachines,
                      data.frame(muni_county,
                                 use.machines,
                                 machine.vendor.dealer.spec,
                                 machine.vendor.dealer.model,
                                 accessible.vendor.dealer.model))

setdiff(vot.equip.withmachines$muni_county, repunit.2016.2012.r$muni_county)

# [1] TOWN OF MAINE MARATHON"

setdiff(repunit.2016.2012.r$muni_county, vot.equip.withmachines$muni_county)



# repunit.2016.2012.r$muni_county = repunit.2016.2012.r$muni_county

repunit.2016.2012.r$muni_county[grep("VILLAGE OF WRIGHTSTOWN",
                                     repunit.2016.2012.r$muni_county)] = "VILLAGE OF WRIGHTSTOWN BROWN"
repunit.2016.2012.r$muni_county[grep("TOWN OF MAINE",
                                     repunit.2016.2012.r$muni_county)] = "TOWN OF MAINE MARATHON"
colnames(vot.equip.join)[1] = "muni_county"

setdiff(vot.equip.join$muni_county, repunit.2016.2012.r$muni_county)

repunit.2016.2012.r = plyr::join(repunit.2016.2012.r,vot.equip.join, by = "muni_county")
repunit.2016.2012.r$use.machines[is.na(repunit.2016.2012.r$use.machines)] = 0

colnames(repunit.2016.2012.r)[grep("use.machines",colnames(repunit.2016.2012.r))] = "county_use_opt_scan"
colnames(repunit.2016.2012.r)[grep("machine.vendor.dealer.spec",colnames(repunit.2016.2012.r))] = "county_machine_vendor_dealer"

####
# doing the same for the 2016 2012 data
####
county.machines.join = with(repunit.2016.2012.r, data.frame(
  muni_county, county_use_opt_scan, county_machine_vendor_dealer, machine.vendor.dealer.model, accessible.vendor.dealer.model
))

ward.2016.2012.test = plyr::join(repunit.2016.2012.r, county.machines.join, by = "muni_county", match = "first")

repunit.2016.2012.r = ward.2016.2012.test
repunit.2016.2012.r$county_use_opt_scan[is.na(repunit.2016.2012.r$county_use_opt_scan)] = 0

# and the 2016 2008 data
ward.2016.2008.test = plyr::join(repunit.2016.2008.r, county.machines.join, by = "muni_county", match = "first")

repunit.2016.2008.r = ward.2016.2008.test
repunit.2016.2008.r$county_use_opt_scan[is.na(repunit.2016.2008.r$county_use_opt_scan)] = 0

rm(ward.2016.2012.test, ward.2016.2008.test)

# Reporting unit level 2014 Governor vs 2016 recount (Paul Ryan district to do) NOT NEC USE THE ONE FURTHER BELOW ------------------------------------

# Sheet2  is for Governor Walker, Sheet 6 is for Paul Ryan's district

getwd()

{   # 2014 ward data
  ward.14 = read.csv(
    "2014/11.4.2014 Election Results - all offices w x w report gov.csv",
    skip = 10,
    strip.white = T,
    stringsAsFactors = F,
    header = T
  )
  
  { # Loading 2014 ward vote data
    ward.14 = clean_names(ward.14)
    names(ward.14)[1:3] <- c("county","reporting_unit","votes.tot")
    
    ward.14$county[ward.14$county == ""] <- NA
    
    require(zoo)
    ward.14$county <- na.locf(ward.14$county)
    
    ward.14$reporting_unit = ifelse(ward.14$reporting_unit == "","Wards 1",ward.14$reporting_unit)
    ward.14$reporting_unit = toupper(ward.14$reporting_unit)
    ward.14$county = toupper(ward.14$county)
    
    ward.14 <- subset(ward.14, reporting_unit != "COUNTY TOTALS:")
    ward.14 <- subset(ward.14, county != "OFFICE TOTALS:")
    
    # Get the municipality names from the reporting unit names    
    odd_indexes<-seq(1,length(ward.14$reporting_unit)*2,2)
    
    ward.14$reporting_unit <- gsub("WARD WD", "WARD", ward.14$reporting_unit)
    ward.14$reporting_unit <- gsub("\\bWD\\b", "WARD", ward.14$reporting_unit)
    ward.14$reporting_unit <- gsub("\\bWDS\\b", "WARDS", ward.14$reporting_unit)
    ward.14$reporting_unit <- gsub("\\bWARD\\b", "WARD", ward.14$reporting_unit)
    
    # https://stackoverflow.com/questions/26896971/add-space-between-two-letters-in-a-string-in-r
    
    ward.14$reporting_unit <- gsub("WARD([0-9])", "WARD \\1", ward.14$reporting_unit)
    ward.14$reporting_unit <- gsub("WARDS([0-9])", "WARDS \\1", ward.14$reporting_unit)
    
    require(stringr)
    
    ward.14$municipality_name <- unlist(str_split(str = ward.14$reporting_unit, " [0-9]", n = 2))[odd_indexes]
    ward.14$municipality_name <- gsub(" WARDS","", ward.14$municipality_name)
    ward.14$municipality_name <- gsub(" WARD","", ward.14$municipality_name)
    ward.14$municipality_name <- na.locf(ward.14$municipality_name)
    
    
    # ward.14$municipality_name = with(ward.14, paste(municipality_type, "of", municipality,sep=" "))
    # ward.14$reporting_unit = with(ward.14, paste(municipality_name, reporting_unit))
    # colnames(ward.14)[1:3] = c("county","municipality_name","reporting_unit")
    # ward.14$municipality_name = toupper(ward.14$municipality_name)
    
    
    # The following is fixing a problem I noticed later when trying to match with the 2016 voter df
    ward.14$municipality_name[grep("TOWN OF WINDSOR", ward.14$municipality_name)] = "VILLAGE OF WINDSOR"
    ward.14$reporting_unit = gsub("TOWN OF WINDSOR","VILLAGE OF WINDSOR", ward.14$reporting_unit)
    
    # The town in Maine became a village in 2015, and is now in marathon county
    # ward.14$county[grep("TOWN OF MAINE", ward.14$municipality_name)] = "MARATHON"
    ward.14$municipality_name[grepl("TOWN OF MAINE", ward.14$municipality_name) &
                                grepl("MARATHON", ward.14$county)
                              ] = "VILLAGE OF MAINE"
    # ward.14$reporting_unit = gsub("TOWN OF MAINE","VILLAGE OF MAINE", ward.14$reporting_unit)
    
    # the town of Menasha was retained only for wards (8-10, 11-13). The rest (1-7) became the village of
    # Fox Crossing, Winnebago
    ward.14$municipality_name[grepl("TOWN OF MENASHA WARDS 1, 2, 4, 7", ward.14$reporting_unit)
                              ] = "VILLAGE OF FOX CROSSING"
    ward.14$municipality_name[grepl("TOWN OF MENASHA WARDS 3, 5, 6", ward.14$reporting_unit)
                              ] = "VILLAGE OF FOX CROSSING"
    ward.14$reporting_unit[grepl("TOWN OF MENASHA WARDS 1, 2, 4, 7", ward.14$reporting_unit)
                           ] = "VILLAGE OF FOX CROSSING WARDS 1-2,4,7"
    ward.14$reporting_unit[grepl("TOWN OF MENASHA WARDS 3, 5, 6", ward.14$reporting_unit)
                           ] = "VILLAGE OF FOX CROSSING WARDS 3,5-6"
    
    ward.14$muni_county = paste(ward.14$municipality_name, ward.14$county)
    ward.14$reporting_unit = paste(ward.14$reporting_unit, ward.14$county)
    
    ward.14.look = with(ward.14, data.frame(county, municipality_name,
                                            reporting_unit, muni_county))
    colnames(ward.14.look) = c("county","municipality","reporting_unit",
                               "muni_county")
    
    ward.14.v = ward.14
    # ward.14.v$county = NULL
    ward.14.v$county = NULL
    ward.14.v$municipality_name = NULL
    ward.14.v$municipality_type = NULL
    ward.14.v$municipalityno = NULL
    ward.14.v$municipality = NULL
    # ward.14.v$reporting_unit = NULL
    ward.14.v$order = NULL
    ward.14.v$hindi = NULL
    # ward.14.v$assemblydistrict = NULL
    
    ward.14.l <- ward.14.v
    ward.14.l$muni_county = NULL
    ward.14.l$reporting_unit <- NULL
    ward.14.l$votes.tot <- NULL
    ward.14.l$x_3 <- NULL
    ward.14.l = gather(ward.14.l, cand, votes.rec)
    ward.14.l$reporting_unit <- rep(ward.14.v$reporting_unit)
    # colnames(ward.14.l) = c("reporting_unit", "cand", "votes.rec")
    
    names(ward.14.v)
    
    ward.14.l$cand.group = NA
    ward.14.l$cand.group[grep("mary_burke",ward.14.l$cand)] = "democrat"
    ward.14.l$cand.group[grep("walker",ward.14.l$cand)] = "republican"
    ward.14.l$cand.group[grep("fehr",ward.14.l$cand)] = "other"
    ward.14.l$cand.group[grep("robert_burke",ward.14.l$cand)] = "other"
    ward.14.l$cand.group[grep("mary_jo",ward.14.l$cand)] = "other"
    ward.14.l$cand.group[grep("steve_r",ward.14.l$cand)] = "other"
    ward.14.l$cand.group[grep("jumoka",ward.14.l$cand)] = "other"
    ward.14.l$cand.group[grep("brett_d_",ward.14.l$cand)] = "other"
    ward.14.l$cand.group[grep("jessica",ward.14.l$cand)] = "other"
    ward.14.l$cand.group[grep("susan_p",ward.14.l$cand)] = "other"
    ward.14.l$cand.group[grep("scatter",ward.14.l$cand)] = "other"
    
    table(is.na(ward.14.l$cand.group)) # no NAs
    
    ward.14.l$votes.rec = as.integer(ward.14.l$votes.rec)
    
    # Joining back county and municipality names
    ward.14.l = plyr::join(ward.14.l, ward.14.look, by = "reporting_unit", match = "first")
    
    tot.votes.look.group = dplyr::group_by(ward.14.l,reporting_unit)
    tot.votes.look = dplyr::summarise(tot.votes.look.group,
                                      votes.tot = sum(as.integer(votes.rec),na.rm=T)
    )
    
    ward.14.l = plyr::join(ward.14.l,tot.votes.look,by="reporting_unit")
    ward.14.l$votes.perc = (ward.14.l$votes.rec / ward.14.l$votes.tot) * 100
    
    ward.14.l$muni_county = paste(ward.14.l$municipality, ward.14.l$county)
    
    ward.14.repu.group = dplyr::group_by(ward.14.l,reporting_unit,municipality,muni_county,county,cand.group)
    ward.14.repu = dplyr::summarise(ward.14.repu.group,
                                    votes.rec2 = sum(votes.rec,na.rm=T),
                                    votes.tot2 = mean(votes.tot,na.rm=T)
    )
    colnames(ward.14.repu)[6] = "votes.rec"
    colnames(ward.14.repu)[7] = "votes.tot"
    
    ward.14.repu$votes.perc = (ward.14.repu$votes.rec / ward.14.repu$votes.tot) * 100
    
    ward.14.repu = ward.14.repu[!is.na(ward.14.repu$cand.group),]
  } # 2014 data basic
  {  # Now joining the 2014 data back into the 2016 data
    # grep("WARDS",ward.2016.r.repu$reporting_unit)
    
    ward.2016.r.repu$reporting_unit_orig =ward.2016.r.repu$reporting_unit_orig
    ward.14.repu$reporting_unit_orig =ward.14.repu$reporting_unit 
    
    ward.2016.r.repu$reporting_unit = gsub("WARDS","WARD",ward.2016.r.repu$reporting_unit_orig)
    ward.14.repu$reporting_unit = gsub("WARDS","WARD",ward.14.repu$reporting_unit_orig)
    ward.2016.r.repu$reporting_unit = gsub("  "," ",ward.2016.r.repu$reporting_unit)
    ward.14.repu$reporting_unit = gsub("  "," ",ward.14.repu$reporting_unit)
    ward.2016.r.repu$reporting_unit = gsub("&","-",ward.2016.r.repu$reporting_unit)
    ward.14.repu$reporting_unit = gsub("&","-",ward.14.repu$reporting_unit)
    
    ward.2016.r.repu$reporting_unit = gsub("AND","-",ward.2016.r.repu$reporting_unit)
    ward.14.repu$reporting_unit = gsub("AND","-",ward.14.repu$reporting_unit)
    
    ward.2016.r.repu$reporting_unit = gsub(",","-",ward.2016.r.repu$reporting_unit)
    ward.14.repu$reporting_unit = gsub(",","-",ward.14.repu$reporting_unit)
    ward.2016.r.repu$reporting_unit = gsub(" - ","-",ward.2016.r.repu$reporting_unit)
    ward.14.repu$reporting_unit = gsub(" - ","-",ward.14.repu$reporting_unit)
    ward.2016.r.repu$reporting_unit = gsub(" -","-",ward.2016.r.repu$reporting_unit)
    ward.14.repu$reporting_unit = gsub(" -","-",ward.14.repu$reporting_unit)
    ward.2016.r.repu$reporting_unit = gsub("- ","-",ward.2016.r.repu$reporting_unit)
    ward.14.repu$reporting_unit = gsub("- ","-",ward.14.repu$reporting_unit)
    
    ward.2016.r.repu$reporting_unit = gsub(" ","-",ward.2016.r.repu$reporting_unit)
    ward.14.repu$reporting_unit = gsub(" ","-",ward.14.repu$reporting_unit)
    
    ward.2016.r.repu$reporting_unit = gsub("WD","",ward.2016.r.repu$reporting_unit)
    ward.14.repu$reporting_unit = gsub("WD","",ward.14.repu$reporting_unit)
    
    ward.2016.r.repu$reporting_unit = gsub("COMBINED","",ward.2016.r.repu$reporting_unit)
    ward.14.repu$reporting_unit = gsub("COMBINED","",ward.14.repu$reporting_unit)
    
    ward.2016.r.repu$reporting_unit = gsub("--","-",ward.2016.r.repu$reporting_unit)
    ward.14.repu$reporting_unit = gsub("--","-",ward.14.repu$reporting_unit)
    
    setdiff(unique(ward.14.repu$reporting_unit), unique(ward.2016.r.repu$reporting_unit))
    setdiff(unique(ward.2016.r.repu$reporting_unit), unique(ward.14.repu$reporting_unit))
    
    # Village of Harrison Calumet did not exist in 2014, was split off from the town in 2013.
    # It is impossible to know the boundaries exactly.
    # Therefore the swing from 2014 to 2016 cannot be accurately calculated.
    
    # Same of the town / village of Somers, Kenosha. The town / village is completely mixed and messed up
    
    # the town of Menasha was retained only for wards (8-10, 11-13). The rest (1-7) became the village of
    # Fox Crossing, Winnebago
    
    differences = setdiff(unique(ward.2016.r.repu$reporting_unit),unique(ward.14.repu$reporting_unit))
    vote.2014.diffs = subset(ward.2016.r.repu, reporting_unit %in% differences)
    
    vote.2014.diffs.join = with(vote.2014.diffs, data.frame(municipality, county, reporting_unit))
    vote.2014.diffs.join = vote.2014.diffs.join[!duplicated(vote.2014.diffs.join),]
    
    # ## Joining the reg.vote summarised municipalities back onto the voting municipalities
    # ward.14.repu.join = with(ward.14.repu, data.frame(municipality,reporting_unit))
    # colnames(ward.14.repu.join)[2] = "reporting_unit_2014_agreed"
    # ward.14.repu.join = ward.14.repu.join[!duplicated(ward.14.repu.join$reporting_unit_2014),]
    # 
    # 
    # 
    # vote.2014.diffs.join = plyr::join(vote.2014.diffs.join,ward.14.repu.join,by = "reporting_unit")
    # 
    # back.to.votes.df.join = with(vote.2014.diffs.join, data.frame(reporting_unit,reporting_unit_2014_agreed))
    # 
    # ward.2016.r.repu = plyr::join(ward.2016.r.repu, back.to.votes.df.join, by = "reporting_unit")
    # 
    # ward.2016.r.repu$reporting_unit_2014_agreed = as.character(ward.2016.r.repu$reporting_unit_2014_agreed)
    # 
    # ward.2016.r.repu$reporting_unit_2014_agreed = ifelse(is.na(ward.2016.r.repu$reporting_unit_2014_agreed),
    #                                                  ward.2016.r.repu$reporting_unit,ward.2016.r.repu$reporting_unit_2014_agreed)
    # 
    ## Now renaming the 2014 column for the final join
    # colnames(ward.14.repu)[2] = "muni_county_2014_agreed"
    
    ## Need to sum up the votes by county in 2014
    ward.14.repu.votes.group = dplyr::group_by(ward.14.repu,cand.group,reporting_unit,reporting_unit_orig,municipality, county)
    ward.14.repu.votes.g = dplyr::summarise(ward.14.repu.votes.group,
                                            votes.rec2014 = sum(votes.rec),
                                            votes.tot2014 = mean(votes.tot)
                                            
    )
    
    # Melting the 2014 df to put the candidates on top
    ward.14.repu.votes.g.melt = melt(ward.14.repu.votes.g)
    ward.14.repu.votes.g.cast = dcast(ward.14.repu.votes.g.melt,
                                      reporting_unit ~ cand.group + variable, sum
    )
    ward.14.repu.votes.g.cast$democrat_votes.tot2014 = NULL
    ward.14.repu.votes.g.cast$republican_votes.tot2014 = NULL
    
    ward.14.repu.votes.g.cast.mean = dcast(ward.14.repu.votes.g.melt,
                                           reporting_unit ~ cand.group + variable, mean
    )
    ward.14.repu.votes.g.cast$other_votes.tot2014 = ward.14.repu.votes.g.cast.mean$other_votes.tot2014
    colnames(ward.14.repu.votes.g.cast)[grepl("votes.tot",colnames(ward.14.repu.votes.g.cast))] =
      "votes.tot2014"
    
    # Also need to melt the 2016 mun df by cand.group
    ward.2016.r.repu.votes.g.melt = melt(ward.2016.r.repu)
    ward.2016.r.repu.votes.g.cast = dcast(ward.2016.r.repu.votes.g.melt,
                                          reporting_unit + reporting_unit_orig + muni_county + municipality + county ~
                                            cand.group + variable, sum
    )
    
    ward.2016.r.repu.votes.g.cast$democrat_votes.tot = NULL
    ward.2016.r.repu.votes.g.cast$republican_votes.tot = NULL
    ward.2016.r.repu.votes.g.cast$democrat_voters.tot = NULL
    ward.2016.r.repu.votes.g.cast$republican_voters.tot = NULL
    
    ward.2016.r.repu.votes.g.cast.mean = dcast(ward.2016.r.repu.votes.g.melt,
                                               reporting_unit + reporting_unit_orig + muni_county + municipality + county ~
                                                 cand.group + variable, mean
    )
    ward.2016.r.repu.votes.g.cast$other_votes.tot = ward.2016.r.repu.votes.g.cast.mean$other_votes.tot
    ward.2016.r.repu.votes.g.cast$other_voters.tot = ward.2016.r.repu.votes.g.cast.mean$other_voters.tot
    colnames(ward.2016.r.repu.votes.g.cast)[grepl("votes.tot",colnames(ward.2016.r.repu.votes.g.cast))] =
      "votes.tot2016"
    colnames(ward.2016.r.repu.votes.g.cast)[grepl("voters.tot",colnames(ward.2016.r.repu.votes.g.cast))] =
      "voters.tot2016"
    
    # Finally joining the dfs together
    
    ward.2016.r.repu.g = plyr::join(ward.2016.r.repu.votes.g.cast, ward.14.repu.votes.g.cast, by = "reporting_unit")
    
    # 2014 turnout is calculated on the (big?) assumption that the number of registered
    # voters was the same in 2014 as in 2016
    # ward.2016.r.repu.g$turnout.2014 =
    # with(ward.2016.r.repu.g, votes.tot2014 / voters.tot2016) * 100
    
    ward.2016.r.repu.g$dem.perc2014 =
      with(ward.2016.r.repu.g,
           democrat_votes.rec2014 / votes.tot2014) * 100
    ward.2016.r.repu.g$rep.perc2014 =
      with(ward.2016.r.repu.g,
           republican_votes.rec2014 / votes.tot2014) * 100
    ward.2016.r.repu.g$oth.perc2014 =
      with(ward.2016.r.repu.g,
           other_votes.rec2014 / votes.tot2014) * 100
    
    ward.2016.r.repu.g$dem.change.2014.num =
      with(ward.2016.r.repu.g,
           democrat_votes.rec - democrat_votes.rec2014)
    ward.2016.r.repu.g$rep.change.2014.num =
      with(ward.2016.r.repu.g,
           republican_votes.rec - republican_votes.rec2014)
    ward.2016.r.repu.g$oth.change.2014.num =
      with(ward.2016.r.repu.g,
           other_votes.rec - other_votes.rec2014)
    
    ward.2016.r.repu.g$dem.change.2014.perc =
      with(ward.2016.r.repu.g,
           democrat_votes.perc -  dem.perc2014)
    ward.2016.r.repu.g$rep.change.2014.perc =
      with(ward.2016.r.repu.g,
           republican_votes.perc - rep.perc2014)
    ward.2016.r.repu.g$oth.change.2014.perc =
      with(ward.2016.r.repu.g,
           other_votes.perc - oth.perc2014)
    # 
    # ward.2016.r.repu.g$turnout.2016.perc =
    #   with(ward.2016.r.repu.g,
    #        votes.tot2016 / voters.tot2016) * 100
    
    # ward.2016.r.repu.g$turnout.change.perc =
    #   with(ward.2016.r.repu.g,
    #        turnout.2016.perc - turnout.2014)
    
    
    repunit.2016.2014.r = ward.2016.r.repu.g
    
    repunit.2016.2014.r$demwinner = NA
    repunit.2016.2014.r$demwinner = ifelse(repunit.2016.2014.r$democrat_votes.perc >
                                             repunit.2016.2014.r$republican_votes.perc &
                                             !is.na(repunit.2016.2014.r$democrat_votes.perc), 1,0)
    repunit.2016.2014.r$demwinner = as.integer(repunit.2016.2014.r$demwinner)
    
    repunit.2016.2014.r$demwinner2014 = NA
    repunit.2016.2014.r$demwinner2014 = ifelse(repunit.2016.2014.r$dem.perc2014 >
                                                 repunit.2016.2014.r$rep.perc2014 &
                                                 !is.na(repunit.2016.2014.r$dem.perc2014), 1,0)
    repunit.2016.2014.r$demwinner2014 = as.integer(repunit.2016.2014.r$demwinner2014)
  } # Now joining the 2014 data back into the 2016 data
} # Loading 2014 ward vote data

# Loading voting machine data 2016 recount vs 2014 Governor (Wisconsin official) NOT NEC USE THE ONE FURTHER BELOW ---------------------------------------------
# This file from here: http://elections.wi.gov/elections-voting/voting-equipment/voting-equipment-use
# Voting machine equipment for 13 September 2016
vot.equip = read.xlsx(
  "voting_equipment_by_municipality_09_2016_xlsx_78114.xlsx1207162619.xlsx",
  sheetIndex = 1,
  header = TRUE,
  colClasses = NA,
  stringsAsFactors = F
)
colnames(vot.equip) = c(
  "county.long",
  "municipality",
  "machine.vendor.dealer.model",
  "accessible.vendor.dealer.model"
)

# The following is fixing a problem I noticed later when trying to match with the voter df
vot.equip$municipality[grep("TOWN OF WINDSOR",
                            vot.equip$municipality)] = "VILLAGE OF WINDSOR"

# Following from
# http://stackoverflow.com/questions/15895050/using-gsub-to-extract-character-string-before-white-space-in-r
county.split.att2 = gsub(" COUNTY.*$", "", vot.equip$county.long)
vot.equip$county = county.split.att2
vot.equip = vot.equip[!is.na(vot.equip$county.long), ]

vot.equip$use.machines = NA
vot.equip$use.machines = ifelse(vot.equip$machine.vendor.dealer.model == "None ", 0, 1)

vot.equip$machine.vendor.dealer.spec = gsub("Optech-", "Optech ", vot.equip$machine.vendor.dealer.model)
vot.equip$machine.vendor.dealer.spec = gsub("Optech/", "Optech ", vot.equip$machine.vendor.dealer.spec)
vot.equip$machine.vendor.dealer.spec = gsub(" .*$", "", vot.equip$machine.vendor.dealer.spec)
vot.equip$machine.vendor.dealer.spec = gsub("None", NA, vot.equip$machine.vendor.dealer.spec)

vot.equip.county.grouped = dplyr::group_by(vot.equip, county)
vot.equip.county = dplyr::summarise(
  vot.equip.county.grouped,
  use.machines.prop = sum(use.machines, na.rm = TRUE) /
    length(use.machines)
)

# vot.equip.withmachines = subset(vot.equip, use.machines > 0)
vot.equip.withmachines = vot.equip

vot.equip.county.grouped.machines = dplyr::group_by(vot.equip.withmachines, county)
vot.equip.county.machines = dplyr::summarise(
  vot.equip.county.grouped.machines,
  use.machines.prop = sum(use.machines, na.rm = TRUE) /
    length(use.machines)
)

vot.equip.county.machines$use.machines.prop = NULL

max.vendor = as.character(1:length(vot.equip.county.machines$county))
for (i in 1:length(unique(vot.equip.county.machines$county))) {
  a = count(vot.equip.withmachines$machine.vendor.dealer.spec[vot.equip.withmachines$county ==
                                                                vot.equip.withmachines$county[i]])
  a$x = as.character(a$x)
  max.vendor[i] = a$x[a$freq == max(a$freq)]
}

vot.equip.county.machines$machine.most.used = max.vendor
vot.equip.county = plyr::join(vot.equip.county,
                              vot.equip.county.machines,
                              by = "county",
                              match = "first")

test = tolower(vot.equip.county$county)
test = capwords(test)
vot.equip.county$county = test

machine.join = data.frame(vot.equip.county$county, vot.equip.county$use.machines.prop, vot.equip.county$machine.most.used)
colnames(machine.join) = c("county","use.machines.prop","machine.most.used")

machine.join$all.machines = with(machine.join, ifelse(use.machines.prop > 0.75, "Mostly optical scanners",
                                                      "Some or no optical scanners"))
machine.join$county = as.character(machine.join$county)
machine.join$county[machine.join$county == "Fond Du Lac"] = "Fond du Lac"

rm(vote.equip.county.machines,vote.equip.county.grouped,voting.age.people,test,
   historical.turnout.join,a)

#### Now joining this to the ward data
vot.equip.withmachines$muni_county = gsub(" -.*", "",vot.equip.withmachines$municipality)

vot.equip.withmachines$muni_county = paste(vot.equip.withmachines$muni_county, vot.equip.withmachines$county)
vot.equip.withmachines$muni_county = gsub("  "," ",vot.equip.withmachines$muni_county)


vot.equip.join = with(vot.equip.withmachines,
                      data.frame(muni_county,
                                 use.machines,
                                 machine.vendor.dealer.spec,
                                 machine.vendor.dealer.model,
                                 accessible.vendor.dealer.model))

setdiff(vot.equip.withmachines$muni_county, repunit.2016.2012.o$muni_county)

# [1] TOWN OF MAINE MARATHON"

setdiff(repunit.2016.2014.o$muni_county, vot.equip.withmachines$muni_county)



repunit.2016.2014.o$muni_county = repunit.2016.2014.o$muni_county

repunit.2016.2014.o$muni_county[grep("VILLAGE OF WRIGHTSTOWN",
                                     repunit.2016.2014.o$muni_county)] = "VILLAGE OF WRIGHTSTOWN BROWN"
repunit.2016.2014.o$muni_county[grep("TOWN OF MAINE",
                                     repunit.2016.2014.o$muni_county)] = "TOWN OF MAINE MARATHON"
colnames(vot.equip.join)[1] = "muni_county"

setdiff(vot.equip.join$muni_county,
        repunit.2016.2014.o$muni_county)

repunit.2016.2014.o = plyr::join(repunit.2016.2014.o,vot.equip.join, by = "muni_county")
repunit.2016.2014.o$use.machines[is.na(repunit.2016.2014.o$use.machines)] = 0

colnames(repunit.2016.2014.o)[grep("use.machines",colnames(repunit.2016.2014.o))] = "county_use_opt_scan"
colnames(repunit.2016.2014.o)[grep("machine.vendor.dealer.spec",colnames(repunit.2016.2014.o))] = "county_machine_vendor_dealer"

####
# doing the same for the 2016 2014 data
####
county.machines.join = with(repunit.2016.2014.o, data.frame(
  muni_county, county_use_opt_scan, county_machine_vendor_dealer, machine.vendor.dealer.model, accessible.vendor.dealer.model
))

ward.2016.2014.test = plyr::join(repunit.2016.2014.o, county.machines.join, by = "muni_county", match = "first")

repunit.2016.2014.o = ward.2016.2014.test
repunit.2016.2014.o$county_use_opt_scan[is.na(repunit.2016.2014.o$county_use_opt_scan)] = 0


# Loading in spatial data for original 2016 original vs 2012 data -------------------------------------------------
require(rgdal); require(plotKML); require(sp); require(rgeos)

wisc.spat <-
  readOGR(
    dsn = "WI_MunicipalWards_2016\\WI_MunicipalWards_2016.shx",
    layer = "WI_MunicipalWards_2016",
    stringsAsFactors = F
  )
# wisc.spat <- spTransform(wisc.spat, CRS("+proj=longlat +datum=WGS84"))

wisc.spat@data$MCD_FIPS[wisc.spat@data$MCD_NAME == "POUND" & wisc.spat@data$CNTY_NAME == "MARINETTE"] = 5507564775

wisc.spat.df = as.data.frame(wisc.spat)

require(plyr)
colnames(repunit.2016.2012.o)[grep("mcd_fips",colnames(repunit.2016.2012.o))] = "MCD_FIPS"

# Dissolving the polygons to the municipal level
# I got the following from: https://philmikejones.wordpress.com/2015/09/03/dissolve-polygons-in-r/

# Ensure shapefile row.names and polygon IDs are sensible
row.names(wisc.spat) <- row.names(wisc.spat@data)
wisc.spat <- spChFIDs(wisc.spat, row.names(wisc.spat))

# Now the dissolve
wisc.spat.d <- gUnaryUnion(wisc.spat, id = wisc.spat@data$MCD_FIPS)

# If you want to recreate an object with a data frame
# make sure row names match
row.names(wisc.spat.d) <- as.character(1:length(wisc.spat.d))

# Extract the data you want (the larger geography)
lu <- unique(wisc.spat@data$MCD_FIPS)
lu <- as.data.frame(lu)
colnames(lu) <- "MCD_FIPS"  # your data will probably have more than 1 row!

lu.grp = dplyr::group_by(wisc.spat@data, MCD_FIPS, MCD_NAME, CNTY_NAME, CNTY_FIPS, CTV)
lu.grps = dplyr::summarise(lu.grp)

setdiff(lu.grps$MCD_FIPS, wisc.spat@data$MCD_FIPS)
setdiff(wisc.spat@data$MCD_FIPS, lu.grps$MCD_FIPS)

lu.grps = as.data.frame(lu.grps)

# And add the data back in
wisc.spat.d.df <- SpatialPolygonsDataFrame(wisc.spat.d, lu.grps)

# Check it's all worked
# plot(wisc.spat.d)

## Dealing with NAs

# City of washington dells Juneau doesn't exist in 2016.2012.o

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "TOWN OF SPRINGFIELD ST. CROIX"] =
  with(wisc.spat.df, (MCD_FIPS[MCD_NAME == "SPRINGFIELD" & CNTY_NAME == "ST_CROIX"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF UNITY CLARK"] =
  with(wisc.spat.df, (MCD_FIPS[MCD_NAME == "UNITY" & CNTY_NAME == "CLARK"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF UNITY MARATHON"] =
  with(wisc.spat.df, (MCD_FIPS[MCD_NAME == "Unity" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF COLBY MARATHON"] =
  with(wisc.spat.df, (MCD_FIPS[MCD_NAME == "Colby" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF ABBOTSFORD MARATHON"] =
  with(wisc.spat.df, unique((MCD_FIPS[MCD_NAME == "Abbotsford" & CNTY_NAME == "MARATHON" & CTV == "C"])))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF DORCHESTER MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Dorchester" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF DORCHESTER MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Dorchester" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "TOWN OF WASHINGTON DOOR"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "WASHINGTON" & CNTY_NAME == "DOOR"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF PULASKI BROWN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Pulaski" & CNTY_NAME == "BROWN"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF HOBART BROWN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Hobart" & CNTY_NAME == "BROWN"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF PULASKI SHAWANO"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Pulaski" & CNTY_NAME == "SHAWANO"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF WRIGHTSTOWN OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wrightstown" & CNTY_NAME == "OUTAGAMIE"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF WRIGHTSTOWN OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wrightstown" & CNTY_NAME == "OUTAGAMIE"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF WRIGHTSTOWN OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wrightstown" & CNTY_NAME == "OUTAGAMIE"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF MARSHALL DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Marshall" & CNTY_NAME == "DANE"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF APPLETON CALUMET"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Appleton" & CNTY_NAME == "CALUMET"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF APPLETON WINNEBAGO"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Appleton" & CNTY_NAME == "WINNEBAGO"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF MARION SHAWANO"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Marion" & CNTY_NAME == "SHAWANO"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF BIRNAMWOOD MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Birnamwood" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF MARATHON CITY MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Marathon City" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "TOWN OF MARATHON MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MARATHON" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "TOWN OF MARATHON MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MARATHON" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF STANLEY CLARK"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Stanley" & CNTY_NAME == "CLARK"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF EAU CLAIRE CHIPPEWA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Eau Claire" & CNTY_NAME == "CHIPPEWA"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF NEW AUBURN BARRON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "New Auburn" & CNTY_NAME == "BARRON"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF TURTLE LAKE POLK"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Turtle Lake" & CNTY_NAME == "POLK"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF RIVER FALLS ST. CROIX"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "River Falls" & CNTY_NAME == "ST_CROIX"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF SPRING VALLEY ST. CROIX"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Spring Valley" & CNTY_NAME == "ST_CROIX"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF KIEL CALUMET"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Kiel" & CNTY_NAME == "CALUMET"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF BERLIN WAUSHARA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Berlin" & CNTY_NAME == "WAUSHARA"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF WISCONSIN DELLS ADAMS"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wisconsin Dells" & CNTY_NAME == "ADAMS"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF RANDOLPH COLUMBIA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Randolph" & CNTY_NAME == "COLUMBIA"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF KEWASKUM WASHINGTON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Kewaskum" & CNTY_NAME == "WASHINGTON"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF NEWBURG OZAUKEE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Newburg" & CNTY_NAME == "OZAUKEE"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF CAZENOVIA SAUK"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Cazenovia" & CNTY_NAME == "SAUK"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF VIOLA VERNON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Viola" & CNTY_NAME == "VERNON"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF DE SOTO CRAWFORD"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "De Soto" & CNTY_NAME == "CRAWFORD"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF MUSCODA IOWA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Muscoda" & CNTY_NAME == "IOWA"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF MAINE MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MAINE" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "TOWN OF MAINE MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MAINE" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF MILLADORE PORTAGE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Milladore" & CNTY_NAME == "PORTAGE"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF MENASHA CALUMET"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Menasha" & CNTY_NAME == "CALUMET"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF WINDSOR DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Windsor" & CNTY_NAME == "DANE"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF MAZOMANIE DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MAZOMANIE" & CNTY_NAME == "DANE"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "TOWN OF WISCONSIN JUNEAU"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wisonsin" & CNTY_NAME == "JUNEAU"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF HARTFORD WASHINGTON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Hartford" & CNTY_NAME == "WASHINGTON" & CTV == "C"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF COLUMBUS COLUMBIA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Columbus" & CNTY_NAME == "COLUMBIA"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF BAYSIDE OZAUKEE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Bayside" & CNTY_NAME == "OZAUKEE"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF BAYSIDE OZAUKEE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Bayside" & CNTY_NAME == "OZAUKEE"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF WISCONSIN DELLS SAUK"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wisconsin Dells" & CNTY_NAME == "SAUK"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF LAC LA BELLE WAUKESHA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Lac La Belle" & CNTY_NAME == "WAUKESHA"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF WATERTOWN DODGE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Watertown" & CNTY_NAME == "DODGE"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF WINDSOR DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Windsor" & CNTY_NAME == "DANE"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF MONTFORT IOWA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Montfort" & CNTY_NAME == "IOWA"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF LIVINGSTON IOWA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Livingston" & CNTY_NAME == "IOWA"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF CUBA CITY LAFAYETTE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Cuba City" & CNTY_NAME == "LAFAYETTE"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF HAZEL GREEN LAFAYETTE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Hazel Green" & CNTY_NAME == "LAFAYETTE"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "TOWN OF SPRING GROVE GREEN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "SPRING GROVE" & CNTY_NAME == "GREEN"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF BLANCHARDVILLE IOWA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Blanchardville" & CNTY_NAME == "IOWA"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF BELLEVILLE GREEN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Belleville" & CNTY_NAME == "GREEN"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF BROOKLYN GREEN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Brooklyn" & CNTY_NAME == "GREEN"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF EDGERTON DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Edgerton" & CNTY_NAME == "DANE"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF CAMBRIDGE JEFFERSON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Cambridge" & CNTY_NAME == "JEFFERSON"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF WHITEWATER JEFFERSON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Whitewater" & CNTY_NAME == "JEFFERSON"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF MUKWONAGO WALWORTH"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Mukwonago" & CNTY_NAME == "WALWORTH"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "TOWN OF BURLINGTON RACINE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "BURLINGTON" & CNTY_NAME == "RACINE"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF BURLINGTON WALWORTH"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Burlington" & CNTY_NAME == "WALWORTH"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF BRODHEAD ROCK"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Brodhead" & CNTY_NAME == "ROCK"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "TOWN OF SPRING GROVE GREEN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "SPRING GROVE" & CNTY_NAME == "GREEN"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF BRISTOL KENOSHA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Bristol" & CNTY_NAME == "KENOSHA"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF BLOOMFIELD WALWORTH"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "BLOOMFIELD" & CNTY_NAME == "WALWORTH" & CTV == "V"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "TOWN OF BLOOMFIELD WALWORTH"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "BLOOMFIELD" & CNTY_NAME == "WALWORTH" & CTV == "T"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF WAUPUN FOND DU LAC"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Waupun" & CNTY_NAME == "FOND_DU_LAC"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "TOWN OF MCMILLAN MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MCMILLAN" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF MARSHFIELD MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Marshfield" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF ROCKLAND LA CROSSE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Rockland" & CNTY_NAME == "LA_CROSSE"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF MAPLE BLUFF DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Maple Bluff" & CNTY_NAME == "DANE"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF ONTARIO VERNON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Ontario" & CNTY_NAME == "VERNON"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF PULASKI BROWN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Pulaski" & CNTY_NAME == "BROWN"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "TOWN OF MADISON DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Madison" & CNTY_NAME == "DANE" & CTV == "T"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF MADISON DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Madison" & CNTY_NAME == "DANE" & CTV == "C"]))

###
# Places that fail before #
###

# Pulaski OCONTO apparently doesn't have any people in it
repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF PULASKI OCONTO"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Pulaski" & CNTY_NAME == "OCONTO"]))

# CITY OF NEW LONDON JUST DOESN'T EXIST IN VERIFIED VOTING DATA. I SUBBED IN THE OFFICIAL WISC ELEC BOARD STATS
repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF NEW LONDON OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "New London" & CNTY_NAME == "OUTAGAMIE"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF NEW LONDON WAUPACA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "New London" & CNTY_NAME == "WAUPACA"]))

# Milwaukee districts outside of Milwaukee. Maybe all have a population of 0
repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF MILWAUKEE WAUKESHA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Milwaukee" & CNTY_NAME == "WAUKESHA"]))

repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF MILWAUKEE WASHINGTON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Milwaukee" & CNTY_NAME == "WASHINGTON"]))

# Rockland Monroe just doesn't exist. 0 population?
repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF ROCKLAND MONROE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Rockland" & CNTY_NAME == "MONROE"]))

# Ontario Monroe doesn't exist in my db. 0 population?
repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF ONTARIO MONROE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Ontario" & CNTY_NAME == "MONROE"]))

# Wisconsin Dells Juneau doesn't exist in my db. Maybe a 0 population.
repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF WISCONSIN DELLS JUNEAU"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wisconsin Dells" & CNTY_NAME == "JUNEAU"]))

# Harrison Outagamie doesn't exist. Probably 0 population in Outagamie
repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "TOWN OF HARRISON OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "HARRISON" & CNTY_NAME == "OUTAGAMIE"]))

# Hartford Dodge doesn't exist, as 0 people live there
repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF HARTFORD DODGE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Hartford" & CNTY_NAME == "DODGE" & CTV == "C"]))

# Almost all of the city live in the Columbia side of the party
repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF COLUMBUS DODGE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Columbus" & CNTY_NAME == "DODGE"]))

# Kewaskum fond du lac doesn't exist - all population is in Washington
repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF KEWASKUM FOND DU LAC"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Kewaskum" & CNTY_NAME == "FOND_DU_LAC"]))

# All residents of Howard live in the Brown side of the town
repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF HOWARD OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Howard" & CNTY_NAME == "OUTAGAMIE"]))

# The Calumet part of Kaukauna has a population of 0
repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF KAUKAUNA CALUMET"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Kaukauna" & CNTY_NAME == "CALUMET"]))

# Genoa city, Kenosha part only has 6 residents
repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF GENOA KENOSHA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Genoa" & CNTY_NAME == "KENOSHA"]))####

# Lac La Belle, Jefferson, has only ONE resident. The Waukesha side has the rest (289)
repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "VILLAGE OF LAC LA BELLE JEFFERSON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Lac La Belle" & CNTY_NAME == "JEFFERSON"]))####

# The Bayfield side of the town has 0 residents
repunit.2016.2012.o$MCD_FIPS[repunit.2016.2012.o$muni_county == "CITY OF ASHLAND BAYFIELD"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Ashland" & CNTY_NAME == "BAYFIELD"]))


# Milwaukee city in other counties doesn't exist

####
####

####
# Joining data

wisc.spat.j = wisc.spat.d.df

# Joining data together
wisc.spat.j@data = plyr::join(wisc.spat.d.df@data, repunit.2016.2012.o, by = "MCD_FIPS", match = "first")


# Geojsons fail if there is an 'NAN' in the data
wisc.spat.j@data[is.na(wisc.spat.j@data)] <- "NA"

wisc.spat.j.point = SpatialPointsDataFrame(wisc.spat.j,wisc.spat.j@data)

# plot(wisc.spat.j)
wisc.spat.j.df = as.data.frame(wisc.spat.j)
writeOGR(
  wisc.spat.j,
  "wisc.spat.orig.repunit.geojson",
  layer = "wisc.spat.j",
  driver = "GeoJSON"
)

# wisc.spat.j.na.votes = subset(wisc.spat.j, wisc.spat.j@data$democrat_votes.oec == "NA")
# writeOGR(
#   wisc.spat.j.na.votes,
#   "wisc.spat.orig.navotes.geojson",
#   layer = "wisc.spat.j.na.votes",
#   driver = "GeoJSON"
# )
#
# wisc.spat.j.na.machines = subset(wisc.spat.j, bmd_makedominion_voting_systems == "NA")
# writeOGR(
#   wisc.spat.j.na.machines,
#   "wisc.spat.orig.namachines.geojson",
#   layer = "wisc.spat.j.na.machines",
#   driver = "GeoJSON"
# )

# writeOGR(attacksite.poly.df, paste(getwd(),"/Outputs",sep = ""),"attacksite.poly.df", driver="GeoJSON")

# writeOGR(
#   wisc.spat.j.point,
#   paste(getwd(),"/wiscjpoint.geojson",sep=""),
#   layer = "wisc.spat.j.point",
#   driver = "GeoJSON"
# )
#
#
# # colnames(wisc.spat.j@data) = make.unique(colnames(wisc.spat.j@data))
#
# writeOGR(
#   wisc.spat.j,
#   "wisc.spat.j",
#   layer = "wisc.spat.j",
#   driver = "ESRI Shapefile"
# )




# Loading in spatial data for 2016 recount vs 2012 data -------------------------------------------------
require(rgdal); require(plotKML); require(sp); require(rgeos)

wisc.spat <-
  readOGR(
    dsn = "WI_MunicipalWards_2016\\WI_MunicipalWards_2016.shx",
    layer = "WI_MunicipalWards_2016",
    stringsAsFactors = F
  )
# wisc.spat <- spTransform(wisc.spat, CRS("+proj=longlat +datum=WGS84"))

wisc.spat@data$MCD_FIPS[wisc.spat@data$MCD_NAME == "POUND" & wisc.spat@data$CNTY_NAME == "MARINETTE"] = 5507564775

wisc.spat.df = as.data.frame(wisc.spat)

require(plyr)
colnames(repunit.2016.2012.r)[grep("mcd_fips",colnames(repunit.2016.2012.r))] = "MCD_FIPS"

# Dissolving the polygons to the municipal level
# I got the following from: https://philmikejones.wordpress.com/2015/09/03/dissolve-polygons-in-r/

# Ensure shapefile row.names and polygon IDs are sensible
row.names(wisc.spat) <- row.names(wisc.spat@data)
wisc.spat <- spChFIDs(wisc.spat, row.names(wisc.spat))

# Now the dissolve
wisc.spat.d <- gUnaryUnion(wisc.spat, id = wisc.spat@data$MCD_FIPS)

# If you want to recreate an object with a data frame
# make sure row names match
row.names(wisc.spat.d) <- as.character(1:length(wisc.spat.d))

# Extract the data you want (the larger geography)
lu <- unique(wisc.spat@data$MCD_FIPS)
lu <- as.data.frame(lu)
colnames(lu) <- "MCD_FIPS"  # your data will probably have more than 1 row!

lu.grp = dplyr::group_by(wisc.spat@data, MCD_FIPS, MCD_NAME, CNTY_NAME, CNTY_FIPS, CTV)
lu.grps = dplyr::summarise(lu.grp)

setdiff(lu.grps$MCD_FIPS, wisc.spat@data$MCD_FIPS)
setdiff(wisc.spat@data$MCD_FIPS, lu.grps$MCD_FIPS)

lu.grps = as.data.frame(lu.grps)

# And add the data back in
wisc.spat.d.df <- SpatialPolygonsDataFrame(wisc.spat.d, lu.grps)

# Check it's all worked
# plot(wisc.spat.d)

## Dealing with NAs

# City of washington dells Juneau doesn't exist in 2016.2012.r

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "TOWN OF SPRINGFIELD ST. CROIX"] =
  with(wisc.spat.df, (MCD_FIPS[MCD_NAME == "SPRINGFIELD" & CNTY_NAME == "ST_CROIX"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF UNITY CLARK"] =
  with(wisc.spat.df, (MCD_FIPS[MCD_NAME == "UNITY" & CNTY_NAME == "CLARK"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF UNITY MARATHON"] =
  with(wisc.spat.df, (MCD_FIPS[MCD_NAME == "Unity" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF COLBY MARATHON"] =
  with(wisc.spat.df, (MCD_FIPS[MCD_NAME == "Colby" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF ABBOTSFORD MARATHON"] =
  with(wisc.spat.df, unique((MCD_FIPS[MCD_NAME == "Abbotsford" & CNTY_NAME == "MARATHON" & CTV == "C"])))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF DORCHESTER MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Dorchester" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF DORCHESTER MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Dorchester" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "TOWN OF WASHINGTON DOOR"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "WASHINGTON" & CNTY_NAME == "DOOR"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF PULASKI BROWN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Pulaski" & CNTY_NAME == "BROWN"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF HOBART BROWN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Hobart" & CNTY_NAME == "BROWN"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF PULASKI SHAWANO"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Pulaski" & CNTY_NAME == "SHAWANO"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF WRIGHTSTOWN OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wrightstown" & CNTY_NAME == "OUTAGAMIE"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF WRIGHTSTOWN OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wrightstown" & CNTY_NAME == "OUTAGAMIE"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF WRIGHTSTOWN OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wrightstown" & CNTY_NAME == "OUTAGAMIE"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF MARSHALL DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Marshall" & CNTY_NAME == "DANE"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF APPLETON CALUMET"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Appleton" & CNTY_NAME == "CALUMET"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF APPLETON WINNEBAGO"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Appleton" & CNTY_NAME == "WINNEBAGO"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF MARION SHAWANO"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Marion" & CNTY_NAME == "SHAWANO"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF BIRNAMWOOD MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Birnamwood" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF MARATHON CITY MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Marathon City" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "TOWN OF MARATHON MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MARATHON" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "TOWN OF MARATHON MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MARATHON" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF STANLEY CLARK"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Stanley" & CNTY_NAME == "CLARK"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF EAU CLAIRE CHIPPEWA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Eau Claire" & CNTY_NAME == "CHIPPEWA"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF NEW AUBURN BARRON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "New Auburn" & CNTY_NAME == "BARRON"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF TURTLE LAKE POLK"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Turtle Lake" & CNTY_NAME == "POLK"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF RIVER FALLS ST. CROIX"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "River Falls" & CNTY_NAME == "ST_CROIX"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF SPRING VALLEY ST. CROIX"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Spring Valley" & CNTY_NAME == "ST_CROIX"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF KIEL CALUMET"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Kiel" & CNTY_NAME == "CALUMET"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF BERLIN WAUSHARA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Berlin" & CNTY_NAME == "WAUSHARA"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF WISCONSIN DELLS ADAMS"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wisconsin Dells" & CNTY_NAME == "ADAMS"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF RANDOLPH COLUMBIA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Randolph" & CNTY_NAME == "COLUMBIA"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF KEWASKUM WASHINGTON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Kewaskum" & CNTY_NAME == "WASHINGTON"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF NEWBURG OZAUKEE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Newburg" & CNTY_NAME == "OZAUKEE"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF CAZENOVIA SAUK"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Cazenovia" & CNTY_NAME == "SAUK"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF VIOLA VERNON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Viola" & CNTY_NAME == "VERNON"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF DE SOTO CRAWFORD"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "De Soto" & CNTY_NAME == "CRAWFORD"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF MUSCODA IOWA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Muscoda" & CNTY_NAME == "IOWA"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF MAINE MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MAINE" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "TOWN OF MAINE MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MAINE" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF MILLADORE PORTAGE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Milladore" & CNTY_NAME == "PORTAGE"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF MENASHA CALUMET"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Menasha" & CNTY_NAME == "CALUMET"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF WINDSOR DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Windsor" & CNTY_NAME == "DANE"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF MAZOMANIE DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MAZOMANIE" & CNTY_NAME == "DANE"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "TOWN OF WISCONSIN JUNEAU"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wisonsin" & CNTY_NAME == "JUNEAU"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF HARTFORD WASHINGTON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Hartford" & CNTY_NAME == "WASHINGTON" & CTV == "C"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF COLUMBUS COLUMBIA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Columbus" & CNTY_NAME == "COLUMBIA"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF BAYSIDE OZAUKEE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Bayside" & CNTY_NAME == "OZAUKEE"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF BAYSIDE OZAUKEE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Bayside" & CNTY_NAME == "OZAUKEE"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF WISCONSIN DELLS SAUK"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wisconsin Dells" & CNTY_NAME == "SAUK"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF LAC LA BELLE WAUKESHA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Lac La Belle" & CNTY_NAME == "WAUKESHA"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF WATERTOWN DODGE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Watertown" & CNTY_NAME == "DODGE"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF WINDSOR DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Windsor" & CNTY_NAME == "DANE"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF MONTFORT IOWA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Montfort" & CNTY_NAME == "IOWA"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF LIVINGSTON IOWA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Livingston" & CNTY_NAME == "IOWA"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF CUBA CITY LAFAYETTE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Cuba City" & CNTY_NAME == "LAFAYETTE"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF HAZEL GREEN LAFAYETTE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Hazel Green" & CNTY_NAME == "LAFAYETTE"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "TOWN OF SPRING GROVE GREEN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "SPRING GROVE" & CNTY_NAME == "GREEN"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF BLANCHARDVILLE IOWA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Blanchardville" & CNTY_NAME == "IOWA"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF BELLEVILLE GREEN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Belleville" & CNTY_NAME == "GREEN"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF BROOKLYN GREEN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Brooklyn" & CNTY_NAME == "GREEN"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF EDGERTON DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Edgerton" & CNTY_NAME == "DANE"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF CAMBRIDGE JEFFERSON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Cambridge" & CNTY_NAME == "JEFFERSON"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF WHITEWATER JEFFERSON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Whitewater" & CNTY_NAME == "JEFFERSON"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF MUKWONAGO WALWORTH"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Mukwonago" & CNTY_NAME == "WALWORTH"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "TOWN OF BURLINGTON RACINE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "BURLINGTON" & CNTY_NAME == "RACINE"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF BURLINGTON WALWORTH"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Burlington" & CNTY_NAME == "WALWORTH"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF BRODHEAD ROCK"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Brodhead" & CNTY_NAME == "ROCK"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "TOWN OF SPRING GROVE GREEN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "SPRING GROVE" & CNTY_NAME == "GREEN"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF BRISTOL KENOSHA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Bristol" & CNTY_NAME == "KENOSHA"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF BLOOMFIELD WALWORTH"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "BLOOMFIELD" & CNTY_NAME == "WALWORTH" & CTV == "V"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "TOWN OF BLOOMFIELD WALWORTH"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "BLOOMFIELD" & CNTY_NAME == "WALWORTH" & CTV == "T"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF WAUPUN FOND DU LAC"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Waupun" & CNTY_NAME == "FOND_DU_LAC"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "TOWN OF MCMILLAN MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MCMILLAN" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF MARSHFIELD MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Marshfield" & CNTY_NAME == "MARATHON"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF ROCKLAND LA CROSSE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Rockland" & CNTY_NAME == "LA_CROSSE"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF MAPLE BLUFF DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Maple Bluff" & CNTY_NAME == "DANE"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF ONTARIO VERNON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Ontario" & CNTY_NAME == "VERNON"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF PULASKI BROWN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Pulaski" & CNTY_NAME == "BROWN"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "TOWN OF MADISON DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Madison" & CNTY_NAME == "DANE" & CTV == "T"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF MADISON DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Madison" & CNTY_NAME == "DANE" & CTV == "C"]))

###
# Places that fail before #
###

# Pulaski OCONTO apparently doesn't have any people in it
repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF PULASKI OCONTO"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Pulaski" & CNTY_NAME == "OCONTO"]))

# CITY OF NEW LONDON JUST DOESN'T EXIST IN VERIFIED VOTING DATA. I SUBBED IN THE OFFICIAL WISC ELEC BOARD STATS
repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF NEW LONDON OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "New London" & CNTY_NAME == "OUTAGAMIE"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF NEW LONDON WAUPACA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "New London" & CNTY_NAME == "WAUPACA"]))

# Milwaukee districts outside of Milwaukee. Maybe all have a population of 0
repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF MILWAUKEE WAUKESHA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Milwaukee" & CNTY_NAME == "WAUKESHA"]))

repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF MILWAUKEE WASHINGTON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Milwaukee" & CNTY_NAME == "WASHINGTON"]))

# Rockland Monroe just doesn't exist. 0 population?
repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF ROCKLAND MONROE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Rockland" & CNTY_NAME == "MONROE"]))

# Ontario Monroe doesn't exist in my db. 0 population?
repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF ONTARIO MONROE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Ontario" & CNTY_NAME == "MONROE"]))

# Wisconsin Dells Juneau doesn't exist in my db. Maybe a 0 population.
repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF WISCONSIN DELLS JUNEAU"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wisconsin Dells" & CNTY_NAME == "JUNEAU"]))

# Harrison Outagamie doesn't exist. Probably 0 population in Outagamie
repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "TOWN OF HARRISON OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "HARRISON" & CNTY_NAME == "OUTAGAMIE"]))

# Hartford Dodge doesn't exist, as 0 people live there
repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF HARTFORD DODGE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Hartford" & CNTY_NAME == "DODGE" & CTV == "C"]))

# Almost all of the city live in the Columbia side of the party
repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF COLUMBUS DODGE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Columbus" & CNTY_NAME == "DODGE"]))

# Kewaskum fond du lac doesn't exist - all population is in Washington
repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF KEWASKUM FOND DU LAC"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Kewaskum" & CNTY_NAME == "FOND_DU_LAC"]))

# All residents of Howard live in the Brown side of the town
repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF HOWARD OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Howard" & CNTY_NAME == "OUTAGAMIE"]))

# The Calumet part of Kaukauna has a population of 0
repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF KAUKAUNA CALUMET"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Kaukauna" & CNTY_NAME == "CALUMET"]))

# Genoa city, Kenosha part only has 6 residents
repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF GENOA KENOSHA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Genoa" & CNTY_NAME == "KENOSHA"]))####

# Lac La Belle, Jefferson, has only ONE resident. The Waukesha side has the rest (289)
repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "VILLAGE OF LAC LA BELLE JEFFERSON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Lac La Belle" & CNTY_NAME == "JEFFERSON"]))####

# The Bayfield side of the town has 0 residents
repunit.2016.2012.r$MCD_FIPS[repunit.2016.2012.r$muni_county == "CITY OF ASHLAND BAYFIELD"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Ashland" & CNTY_NAME == "BAYFIELD"]))


# Milwaukee city in other counties doesn't exist

####
####

####
# Joining data

wisc.spat.j = wisc.spat.d.df

# Joining data together
wisc.spat.j@data = plyr::join(wisc.spat.d.df@data, repunit.2016.2012.r, by = "MCD_FIPS", match = "first")


# Geojsons fail if there is an 'NAN' in the data
wisc.spat.j@data[is.na(wisc.spat.j@data)] <- "NA"

wisc.spat.j.point = SpatialPointsDataFrame(wisc.spat.j,wisc.spat.j@data)

# plot(wisc.spat.j)
wisc.spat.j.df = as.data.frame(wisc.spat.j)
writeOGR(
  wisc.spat.j,
  "wisc.spat.rcnt.repunit.geojson",
  layer = "wisc.spat.j",
  driver = "GeoJSON"
)

# wisc.spat.j.na.votes = subset(wisc.spat.j, wisc.spat.j@data$democrat_votes.rec == "NA")
# writeOGR(
#   wisc.spat.j.na.votes,
#   "wisc.spat.rcnt.navotes.geojson",
#   layer = "wisc.spat.j.na.votes",
#   driver = "GeoJSON"
# )
#
# wisc.spat.j.na.machines = subset(wisc.spat.j, bmd_makedominion_voting_systems == "NA")
# writeOGR(
#   wisc.spat.j.na.machines,
#   "wisc.spat.rcnt.namachines.geojson",
#   layer = "wisc.spat.j.na.machines",
#   driver = "GeoJSON"
# )

# writeOGR(attacksite.poly.df, paste(getwd(),"/Outputs",sep = ""),"attacksite.poly.df", driver="GeoJSON")

# writeOGR(
#   wisc.spat.j.point,
#   paste(getwd(),"/wiscjpoint.geojson",sep=""),
#   layer = "wisc.spat.j.point",
#   driver = "GeoJSON"
# )
#
#
# # colnames(wisc.spat.j@data) = make.unique(colnames(wisc.spat.j@data))
#
# writeOGR(
#   wisc.spat.j,
#   "wisc.spat.j",
#   layer = "wisc.spat.j",
#   driver = "ESRI Shapefile"
# )
# Write final csvs --------------------------------------------------------

setwd("C:/Users/s_cas/Dropbox/Perso/2016 voting election county results/Wisconsin/All prep together/All processed files/Prepped files")

# write.csv(election.2016.grouped,"candidate.2016.summary.csv") # I don't want to overwrite these
# write.csv(voting.df.2016,"counties.2016.bycand.csv")
# write.csv(county.summary.df,"county.2016.vs.2012.csv")
# write.csv(counties.2000.2016,"counties.2000.2016.csv")
# write.csv(all_polls_2012,"all_polls_2012.csv")
# write.csv(all_polls_2016,"all_polls_2016.csv")
# write.csv(selected_polls_2012.sub,"selected_polls_2012.sub.csv")
# write.csv(selected_polls_2016.sub,"selected_polls_2016.sub.csv")
# write.csv(elections.100.years,"elections.100.years.csv")
# write.csv(ward.2016.o.l, "wards.data.2016.o.csv")
# write.csv(ward.2016.r.l, "wards.data.2016.r.csv")
# # write.csv(ward.2016.2012, "ward.2016.2012.csv")
# write.csv(ward.2016.2012.o, "ward.2016.2012.o.csv")
# write.csv(ward.2016.2012.r, "ward.2016.2012.r.csv")

write.csv(repunit.2016.2012.o, "2018/repunit.2016.2012.o.csv")
write.csv(repunit.2016.2008.o, "2018/repunit.2016.2008.o.csv")

write.csv(repunit.2016.2012.r, "2018/repunit.2016.2012.r.csv")
write.csv(repunit.2016.2008.r, "2018/repunit.2016.2008.r.csv")

# Read in csvs and subset for use ------------------------------------------------------------
require(ggplot2); library(plotly); require(corrplot)
# require("devtools")  # so we can install from GitHub
# devtools::install_github("ropensci/plotly")  # plotly is part of rOpenSci

# I got this from here: http://stackoverflow.com/questions/31337922/adding-italicised-r-with-correlation-coefficient-to-a-scatter-plot-chart-in-ggpl
corr_eqn <- function(x,y, digits = 2) {
  corr_coef <- round(cor(x, y), digits = digits)
  paste("italic(r) == ", corr_coef)
}

# setwd("C:\\Users\\s_cas\\Dropbox\\Perso\\2016 voting election county results\\Wisconsin")
setwd("C:/Users/s_cas/Dropbox/Perso/2016 voting election county results/Wisconsin/All prep together/All processed files")

repunit.2016.2012.o = read.csv("Prepped files/2018/repunit.2016.2012.o.csv", stringsAsFactors = F)
repunit.2016.2008.o = read.csv("Prepped files/2018/repunit.2016.2008.o.csv", stringsAsFactors = F)
repunit.2016.2012.r = read.csv("Prepped files/2018/repunit.2016.2012.r.csv", stringsAsFactors = F)
repunit.2016.2008.r = read.csv("Prepped files/2018/repunit.2016.2008.r.csv", stringsAsFactors = F)
# The above demographics from here: https://econsnapshot.com/2016/12/06/electronic-voting-machines-and-the-election/
# Same for RepUnit data
# Demographics from David Griffen and co
# repunit.2016.2012.o.dem = plyr::join(repunit.2016.2012.o, demographics.caps, by = "county")
# 
# # Also joining the data from the Election snapshot
# repunit.2016.2012.o.dem = plyr::join(repunit.2016.2012.o.dem, elec.snapshot.dem.wi, by = "county")
# 
# # Demographics from David Griffen and co
# repunit.2016.2008.o.dem = plyr::join(repunit.2016.2008.o, demographics.caps, by = "county")
# 
# # Also joining the data from the Election snapshot
# repunit.2016.2008.o.dem = plyr::join(repunit.2016.2008.o.dem, elec.snapshot.dem.wi, by = "county")
# #
# 
# # RECOUNT
# # Demographics from David Griffen and co
# repunit.2016.2012.r.dem = plyr::join(repunit.2016.2012.r, demographics.caps, by = "county")
# 
# # Also joining the data from the Election snapshot
# repunit.2016.2012.r.dem = plyr::join(repunit.2016.2012.r.dem, elec.snapshot.dem.wi, by = "county")
# 
# # Demographics from David Griffen and co
# repunit.2016.2008.r.dem = plyr::join(repunit.2016.2008.r, demographics.caps, by = "county")
# 
# # Also joining the data from the Election snapshot
# repunit.2016.2008.r.dem = plyr::join(repunit.2016.2008.r.dem, elec.snapshot.dem.wi, by = "county")

# TIGER demographics ------------------------------------------------------
tig_dem = data.frame(c(1:1926))

setwd("C:/Users/s_cas/Dropbox/Perso/2016 voting election county results/Wisconsin/All prep together/Wisconsin municipal stats/Tiger data/")

income_tig = read.csv2("SubMCD_2010Census_DP1 INCOME.csv",sep=",")

tig_dem$geoid = income_tig$GEOID; tig_dem[,1] = NULL

## INCOME

tig_dem$house_inc_2015 = income_tig$B19001e1 #  B19001e1	HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2015 INFLATION-ADJUSTED DOLLARS): Total: Households -- (Estimate)
tig_dem$percap_inc_2015 = income_tig$B19301e1 # B19301e1	PER CAPITA INCOME IN THE PAST 12 MONTHS (IN 2015 INFLATION-ADJUSTED DOLLARS): Total: Total population -- (Estimate)
tig_dem$agg_inc_15older = income_tig$B19313e1 # B19313e1	AGGREGATE INCOME IN THE PAST 12 MONTHS (IN 2015 INFLATION-ADJUSTED DOLLARS): Total: Population 15 years and over -- (Estimate)
tig_dem$med_inc_15older = income_tig$B19326e1 # B19326e1	MEDIAN INCOME IN THE PAST 12 MONTHS (IN 2015 INFLATION-ADJUSTED DOLLARS) BY SEX BY WORK EXPERIENCE IN THE PAST 12 MONTHS FOR THE
# POPULATION 15 YEARS AND OVER WITH INCOME: Total (dollars): Population 15 years and over with income in the past 12 months -- (Estimate)

rm(income_tig)

## EARNINGS

earn_tig = read.csv2("SubMCD_2010Census_DP1 EARNINGS.csv",sep=",")

tig_dem$pop_16older = earn_tig$B20005e1 # B20005e1	SEX BY WORK EXPERIENCE IN THE PAST 12 MONTHS BY EARNINGS IN THE PAST 12 MONTHS (IN 2015 INFLATION-ADJUSTED DOLLARS) FOR THE POPULATION 16 YEARS AND OVER: Total: Population 16 years and over -- (Estimate)
tig_dem$pop_16older_totearnings = earn_tig$B20017e1 # B20017e1	MEDIAN EARNINGS IN THE PAST 12 MONTHS (IN 2015 INFLATION-ADJUSTED DOLLARS) BY SEX BY WORK EXPERIENCE IN THE PAST 12 MONTHS FOR THE POPULATION 16 YEARS AND OVER WITH EARNINGS IN THE PAST 12 MONTHS: Total (dollars): Population 16 years and over with earnings -- (Estimate)

rm(earn_tig)

## EDUCATION

ed_tig = read.csv2("SubMCD_2010Census_DP1 EDUCATION.csv",sep=",")

tig_dem$bach_25older = ed_tig$B15003e22 # B15003e22	EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER: Bachelors degree: Population 25 years and over -- (Estimate)

rm(ed_tig)

# EMPLOYMENT STATUS

emp_tig = read.csv2("SubMCD_2010Census_DP1 EMPLOYMENT.csv",sep=",")

tig_dem$pop_25older = emp_tig$B23006e1 # B23006e1	EDUCATIONAL ATTAINMENT BY EMPLOYMENT STATUS FOR THE POPULATION 25 TO 64 YEARS: Total: Population 25 to 64 years -- (Estimate)

tig_dem$pop_16older = emp_tig$B23025e1 # B23025e1	EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER: Total: Population 16 years and over -- (Estimate)
tig_dem$notworking_16older = emp_tig$B23025e7 # B23025e7	EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER: Not in labor force: Population 16 years and over -- (Estimate)

tig_dem$unemp_perc = (tig_dem$notworking_16older / tig_dem$pop_16older) * 100 # Probably better to do this with 18 and older ??
tig_dem$unemp_perc[tig_dem$unemp_perc > 100] = 100
tig_dem$bach_perc = (tig_dem$bach_25older / tig_dem$pop_25older) * 100
tig_dem$bach_perc[tig_dem$bach_perc > 100] = 100

rm(emp_tig)

# AGE AND SEX

agesex_tig = read.csv2("SubMCD_2010Census_DP1 AGE AND SEX.csv",sep=",")

tig_dem$pop = agesex_tig$B01001e1	# B01001e1	SEX BY AGE: Total: Total population -- (Estimate)
tig_dem$male_pop = agesex_tig$B01001e2 # B01001e2	SEX BY AGE: Male: Total population -- (Estimate)

tig_dem$male_perc = (tig_dem$male_pop / tig_dem$pop) * 100
tig_dem$male_perc[tig_dem$male_perc > 100] = 100

tig_dem$age_med = agesex_tig$B01002e1 # B01002e1 MEDIAN AGE BY SEX: Total: Total population -- (Estimate)
tig_dem$age_med[tig_dem$age_med == 0 | tig_dem$age_med == ""] = NA; tig_dem$age_med = as.numeric(as.character(tig_dem$age_med))

rm(agesex_tig)

# RACE
race_tig = read.csv2("SubMCD_2010Census_DP1 RACE.csv",sep=",")
tig_dem$white_pop = race_tig$B02001e2 # B02001e2	RACE: White alone: Total population -- (Estimate)
tig_dem$black_pop = race_tig$B02001e3 # B02001e3	RACE: Black or African American alone: Total population -- (Estimate)
tig_dem$native_pop = race_tig$B02001e4 # B02001e4	American Indian and Alaska Native alone: Total population -- (Estimate)
tig_dem$asian_pop = race_tig$B02001e5 # B02001e5	RACE: Asian alone: Total population -- (Estimate)

tig_dem$white_perc = (tig_dem$white_pop / tig_dem$pop) * 100; tig_dem$white_perc[tig_dem$white_perc > 100] = 100
tig_dem$black_perc = (tig_dem$black_pop / tig_dem$pop) * 100; tig_dem$black_perc[tig_dem$black_perc > 100] = 100
tig_dem$native_perc = (tig_dem$native_pop / tig_dem$pop) * 100; tig_dem$native_perc[tig_dem$native_perc > 100] = 100
tig_dem$asian_perc = (tig_dem$asian_pop / tig_dem$pop) * 100; tig_dem$asian_perc[tig_dem$asian_perc > 100] = 100

rm(race_tig)

# HISPANIC
hisp_tig = read.csv2("SubMCD_2010Census_DP1 HISPANIC.csv",sep=",")
tig_dem$hisp_pop = hisp_tig$B03001e3  # B03001e3	HISPANIC OR LATINO ORIGIN BY SPECIFIC ORIGIN: Hispanic or Latino: Total population -- (Estimate)
tig_dem$hisp_perc = (tig_dem$hisp_pop / tig_dem$pop) * 100; tig_dem$hisp_perc[tig_dem$hisp_perc > 100] = 100

rm(hisp_tig)

# POVERTY
pov_tig = read.csv2("SubMCD_2010Census_DP1 SCHOOL POVERTY.csv",sep=",")
tig_dem$pov_det = pov_tig$B14006e1  # B14006e1	POVERTY STATUS IN THE PAST 12 MONTHS BY SCHOOL ENROLLMENT BY LEVEL OF SCHOOL FOR THE POPULATION 3 YEARS AND OVER: Total: Population 3 years and over for whom poverty status is determined -- (Estimate)
tig_dem$pov_det_perc = (tig_dem$pov_det / tig_dem$pop) * 100

pov_tig = read.csv2("SubMCD_2010Census_DP1 POVERTY.csv",sep=",")

tig_dem$pov_inc_lesshalf = pov_tig$C17002e2	# C17002e2 RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS: Under .50: Population for whom poverty status is determined -- (Estimate)
tig_dem$pov_inc_halftolevel = pov_tig$C17002e3 # C17002e3	RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS: .50 to .99: Population for whom poverty status is determined -- (Estimate)

tig_dem$pov_tot = tig_dem$pov_inc_lesshalf + tig_dem$pov_inc_halftolevel
tig_dem$pov_perc = (tig_dem$pov_tot / tig_dem$pov_det) * 100; tig_dem$pov_perc[tig_dem$pov_perc > 100] = 100

rm(pov_tig)

# FOOD STAMPS
stamps_tig = read.csv2("SubMCD_2010Census_DP1 FOOD STAMPS.csv",sep=",")

tig_dem$households = stamps_tig$B22001e1 #	B22001e1 RECEIPT OF FOOD STAMPS/SNAP IN THE PAST 12 MONTHS BY PRESENCE OF PEOPLE 60 YEARS AND OVER FOR HOUSEHOLDS: Total: Households -- (Estimate)
tig_dem$stamps = stamps_tig$B22001e2 #B22001e2	RECEIPT OF FOOD STAMPS/SNAP IN THE PAST 12 MONTHS BY PRESENCE OF PEOPLE 60 YEARS AND OVER FOR HOUSEHOLDS: Household received Food Stamps/SNAP in the past 12 months: Households -- (Estimate)
tig_dem$stamps_perc = (tig_dem$stamps / tig_dem$households) * 100; tig_dem$stamps_perc[tig_dem$stamps_perc > 100] = 100
rm(stamps_tig)

# TRUE EMPLOYMENT FROM HEALTH INSURANCE AND DISABILITY DATA
insure_tig = read.csv2("SubMCD_2010Census_DP1 INSURANCE.csv",sep=",")

tig_dem$unemp_1864 = insure_tig$B27011e15 #	B27011e15	HEALTH INSURANCE COVERAGE STATUS AND TYPE BY EMPLOYMENT STATUS BY AGE: In labor force: Unemployed: 18 to 64 years: Civilian noninstitutionalized population 18 years and over -- (Estimate)

dis_tig = read.csv2("SubMCD_2010Census_DP1 DISABILITY.csv",sep=",")
tig_dem$pop_1864 = dis_tig$B18135e13 #	B18135e13	AGE BY DISABILITY STATUS BY HEALTH INSURANCE COVERAGE STATUS: 18 to 64 years: Civilian Noninstitutionalized Population -- (Estimate)
tig_dem$unemp_1864_perc = (tig_dem$unemp_1864 / tig_dem$pop_1864) * 100; tig_dem$unemp_1864_perc[tig_dem$unemp_1864_perc > 100] = 100

rm(insure_tig, dis_tig)

# HOUSING
house_tig = read.csv2("SubMCD_2010Census_DP1 HOUSING.csv",sep=",")

tig_dem$house_size_ave = house_tig$B25010e1 #	B25010e1	AVERAGE HOUSEHOLD SIZE OF OCCUPIED HOUSING UNITS BY TENURE: Total: Occupied housing units -- (Estimate)
rm(house_tig)

tig_dem$house_size_ave = as.numeric(as.character(tig_dem$house_size_ave))


tig_dem[is.na(tig_dem) | tig_dem == ""] = NA

# Making final dataframe
tig_pres_dem = with(tig_dem, data.frame(geoid,
  pop, percap_inc_2015, med_inc_15older, unemp_1864_perc, bach_perc, male_perc, age_med, white_perc, black_perc, asian_perc, 
  hisp_perc, native_perc, pov_perc, stamps_perc, house_size_ave))

# Finally, remove all places with population == 0

tig_pres_dem_rm = tig_pres_dem[tig_pres_dem$pop > 0,]

tig_pres_dem_rm_20 = tig_pres_dem[tig_pres_dem$pop > 20,]

# Correlations
{ # Correlations
corr.table = with(
  tig_pres_dem_rm_20,
  data.frame(
    percap_inc_2015, med_inc_15older, unemp_1864_perc, bach_perc, male_perc, age_med, white_perc, black_perc, asian_perc, 
    hisp_perc, native_perc, pov_perc, stamps_perc, house_size_ave
  )
)
colnames(corr.table) = abbreviate( colnames(corr.table), minlength = 20 )
correlationmatrix = cor(corr.table, use = "pairwise.complete.obs")
corrplot.all = corrplot(correlationmatrix, method = "number", type = "upper")
} # Correlations

setwd("C:/Users/s_cas/Dropbox/Perso/2016 voting election county results/Wisconsin/All prep together/All processed files")
write.csv(tig_pres_dem, "tiger_demographics.csv")

# April 2017 municipal GAB data for 2016 election NEED TO RUN -----------------------------------------------------------
ward.maydata.2016 = read.csv(
  "2016 Presidential and General Election EL-190 2017-10-05.csv",
  stringsAsFactors = F,
  header = T,
  strip.white = T
)

# Weird stuff:
# Ward 83, City of Madison, Dane has 709 late ballots in registered voter pop of 1698
  # str(ward.maydata.2016)

 # Preparing basic data
  ward.maydata.2016 = clean_names(ward.maydata.2016)
  ward.maydata.2016$reporting_unit = toupper(ward.maydata.2016$reporting_unit)
  ward.maydata.2016$municipality = toupper(ward.maydata.2016$municipality)
  ward.maydata.2016$county = toupper(ward.maydata.2016$county)

  ward.maydata.2016$county = gsub(" COUNTY","", ward.maydata.2016$county)

  ward.maydata.2016$muni_county = paste(ward.maydata.2016$municipality, ward.maydata.2016$county)

  ward.maydata.2016$registrants = as.integer(ward.maydata.2016$registrants)
  ward.maydata.2016$election_day_registrants = as.integer(ward.maydata.2016$election_day_registrants)
  ward.maydata.2016$total_ballots = as.integer(ward.maydata.2016$total_ballots)
  ward.maydata.2016$total_voters = as.integer(ward.maydata.2016$total_voters)
  ward.maydata.2016$paper_ballots = as.integer(ward.maydata.2016$paper_ballots)
  ward.maydata.2016$optical_scan_ballots = as.integer(ward.maydata.2016$optical_scan_ballots)
  ward.maydata.2016$dre = as.integer(ward.maydata.2016$dre)
  ward.maydata.2016$auto_mark = as.integer(ward.maydata.2016$auto_mark)
  ward.maydata.2016$absentee_issued = as.integer(ward.maydata.2016$absentee_issued)
  ward.maydata.2016$absentee_in_person = as.integer(ward.maydata.2016$absentee_in_person)
  ward.maydata.2016$received_by_election_day = as.integer(ward.maydata.2016$received_by_election_day)
  ward.maydata.2016$counted_2 = as.integer(ward.maydata.2016$counted_2)
  
  ## Need to change some municipality - county names to match my other data frame
  ward.maydata.2016$county[grep("CITY OF ASHLAND BAYFIELD",ward.maydata.2016$muni_county)] = "ASHLAND"
  ward.maydata.2016$county[grep("CITY OF COLUMBUS DODGE",ward.maydata.2016$muni_county)] = "COLUMBIA"
  ward.maydata.2016$county[grep("CITY OF HARTFORD DODGE",ward.maydata.2016$muni_county)] = "WASHINGTON"
  ward.maydata.2016$county[grep("CITY OF KAUKAUNA CALUMET",ward.maydata.2016$muni_county)] = "OUTAGAMIE"
  ward.maydata.2016$county[grep("CITY OF MILWAUKEE WASHINGTON",ward.maydata.2016$muni_county)] = "MILWAUKEE"
  ward.maydata.2016$county[grep("CITY OF MILWAUKEE WAUKESHA",ward.maydata.2016$muni_county)] = "MILWAUKEE"

  # Wisconsin Dells Juneau doesn't exist in my db. Maybe a 0 population. Has a pop of 0 also in feb 17 data
  ward.maydata.2016$county[grep("CITY OF WISCONSIN DELLS JUNEAU",ward.maydata.2016$muni_county)] = "JUNEAU"

  ward.maydata.2016$county[grep("VILLAGE OF GENOA CITY KENOSHA",ward.maydata.2016$muni_county)] = "WALWORTH"
  ward.maydata.2016$county[grep("VILLAGE OF HARRISON OUTAGAMIE",ward.maydata.2016$muni_county)] = "CALUMET"
  ward.maydata.2016$county[grep("VILLAGE OF HOWARD OUTAGAMIE",ward.maydata.2016$muni_county)] = "BROWN"
  ward.maydata.2016$county[grep("VILLAGE OF KEWASKUM FOND DU LAC",ward.maydata.2016$muni_county)] = "WASHINGTON"
  ward.maydata.2016$county[grep("VILLAGE OF ONTARIO MONROE",ward.maydata.2016$muni_county)] = "VERNON"
  ward.maydata.2016$county[grep("VILLAGE OF PULASKI OCONTO",ward.maydata.2016$muni_county)] = "BROWN"
  ward.maydata.2016$county[grep("VILLAGE OF ROCKLAND MONROE",ward.maydata.2016$muni_county)] = "LA CROSSE"

  ward.maydata.2016$muni_county = paste(ward.maydata.2016$municipality, ward.maydata.2016$county)
  
  ward.maydata.2016$reporting_unit = paste(ward.maydata.2016$municipality,ward.maydata.2016$reporting_unit, ward.maydata.2016$county)
  ward.maydata.2016$reporting_unit_orig = ward.maydata.2016$reporting_unit
  
  
  ward.maydata.2016$reporting_unit = gsub("WARDS","WARD",ward.maydata.2016$reporting_unit_orig)
  # ward.12.mun$reporting_unit = gsub("WARDS","WARD",ward.12.mun$reporting_unit_orig)
  ward.maydata.2016$reporting_unit = gsub("  "," ",ward.maydata.2016$reporting_unit)
  # ward.12.mun$reporting_unit = gsub("  "," ",ward.12.mun$reporting_unit)
  ward.maydata.2016$reporting_unit = gsub("&","-",ward.maydata.2016$reporting_unit)
  # ward.12.mun$reporting_unit = gsub("&","-",ward.12.mun$reporting_unit)
  
  ward.maydata.2016$reporting_unit = gsub("AND","-",ward.maydata.2016$reporting_unit)
  # ward.12.mun$reporting_unit = gsub("AND","-",ward.12.mun$reporting_unit)
  
  ward.maydata.2016$reporting_unit = gsub(",","-",ward.maydata.2016$reporting_unit)
  # ward.12.mun$reporting_unit = gsub(",","-",ward.12.mun$reporting_unit)
  ward.maydata.2016$reporting_unit = gsub(" - ","-",ward.maydata.2016$reporting_unit)
  # ward.12.mun$reporting_unit = gsub(" - ","-",ward.12.mun$reporting_unit)
  ward.maydata.2016$reporting_unit = gsub(" -","-",ward.maydata.2016$reporting_unit)
  # ward.12.mun$reporting_unit = gsub(" -","-",ward.12.mun$reporting_unit)
  ward.maydata.2016$reporting_unit = gsub("- ","-",ward.maydata.2016$reporting_unit)
  # ward.12.mun$reporting_unit = gsub("- ","-",ward.12.mun$reporting_unit)
  
  ward.maydata.2016$reporting_unit = gsub(" ","-",ward.maydata.2016$reporting_unit)
  # ward.12.mun$reporting_unit = gsub(" ","-",ward.12.mun$reporting_unit)
  
  ward.maydata.2016$reporting_unit = gsub("WD","",ward.maydata.2016$reporting_unit)
  # ward.12.mun$reporting_unit = gsub("WD","",ward.12.mun$reporting_unit)
  
  ward.maydata.2016$reporting_unit = gsub("COMBINED","",ward.maydata.2016$reporting_unit)
  # ward.12.mun$reporting_unit = gsub("COMBINED","",ward.12.mun$reporting_unit)
  
  ward.maydata.2016$reporting_unit = gsub("--","-",ward.maydata.2016$reporting_unit)
  # ward.12.mun$reporting_unit = gsub("--","-",ward.12.mun$reporting_unit)

  # str(ward.maydata.2016)

  ward.maydata.group = dplyr::group_by(ward.maydata.2016,muni_county, county, municipality)
  maydata.municounty = dplyr::summarise(ward.maydata.group,
                                    registrants = sum(registrants),
                                    late_registrants = sum(late_registrants),
                                    election_day_registrants = sum(election_day_registrants),
                                    total_ballots = sum(total_ballots),
                                    total_voters = sum(total_voters),
                                    paper_ballots = sum(paper_ballots),
                                    optical_scan_ballots = sum(optical_scan_ballots),
                                    dre = sum(dre),
                                    # auto_mark = sum(auto_mark),
                                    total_election_inspectors = sum(total_election_inspectors),
                                    x16_17 = sum(x16_17),
                                    x18_25 = sum(x18_25),
                                    x26_40 = sum(x26_40),
                                    x41_60 = sum(x41_60),
                                    x61_70 = sum(x61_70),
                                    provisional_identification = sum(provisional_identification),
                                    provisional_no_dl = sum(provisional_no_dl),
                                    counted_id = sum(counted),
                                    rejected_id = sum(rejected),
                                    absentee_issued = sum(absentee_issued),
                                    absentee_in_person = sum(absentee_in_person),
                                    absentee_not_returned = sum(absentee_not_returned),
                                    absentee_undeliverable = sum(absentee_undeliverable),
                                    received_by_election_day = sum(received_by_election_day),
                                    counted_absent = sum(counted_2),
                                    rejected_absent = sum(rejected2),
                                    late_received_after_the_election = sum(late_received_after_the_election),
                                    fwab_received = sum(fwab_received),
                                    fwab_counted = sum(fwab_counted),
                                    fwab_rejected = sum(fwab_rejected),
                                    fwab_late = sum(fwab_late),
                                    military_issued = sum(military_issued),
                                    military_sent_but_not_returned = sum(military_sent_but_not_returned),
                                    military_undeliverable = sum(military_undeliverable),
                                    military_received_by_election_day = sum(military_received_by_election_day),
                                    military_counted = sum(military_counted),
                                    military_rejected = sum(military_rejected),
                                    military_late = sum(military_late),
                                    overseas_issued = sum(overseas_issued),
                                    overseas_sent_but_not_returned = sum(overseas_sent_but_not_returned),
                                    overseas_undeliverable = sum(overseas_undeliverable),
                                    overseas_received_by_election_day = sum(overseas_received_by_election_day),
                                    overseas_counted = sum(overseas_counted),
                                    overseas_rejected = sum(overseas_rejected),
                                    overseas_late = sum(overseas_late)
                                    )

                                    # character fields not included:
                                    # ballots_counted_at
                                    # split_shifts
                                    # difficulty obtaining
                                    # polling_place_name
                                    # shared

###
# Seeing what is differnt according to muni county level
###

# Ugh, I need to change the name of these muni_counties to make sure I have a full match
setdiff(unique(maydata.municounty$muni_county), unique(repunit.2016.2012.o$muni_county))
setdiff(unique(repunit.2016.2012.o$muni_county), unique(maydata.municounty$muni_county))

# differences = setdiff(unique(maydata.municounty$muni_county),unique(repunit.2016.2012.o$muni_county))
# maydata.diffs = subset(maydata.municounty, muni_county %in% differences)

differences = setdiff(unique(repunit.2016.2012.o$muni_county),unique(maydata.municounty$muni_county))
maydata.diffs = subset(repunit.2016.2012.o, muni_county %in% differences)

# Missing: TOWN OF CALEDONIA TREMPEALEAU (town in multiple counties),
# TOWN OF HAYWARD (all put in city of Hayward),
# TOWN OF RANDALL KENOSHA (doesn't exist),
# TOWN OF ROXBURY DANE (doesn't exist), TOWN OF UNION ROCK (doesn't exist),
# VILLAGE OF NORTH BAY RACINE (doesn't exist)
# TOWN OF BURLINGTON WALWORTH (doesnt exist)
# TOWN OF PLYMOUTH JUNEAU

# Missing according to website: http://elections.wi.gov/node/4952
# Town of Plymouth - Juneau County, 
# Town of Randall - Kenosha County, Village of North Bay - Racine County, 
# Town of Union - Rock County, Town of Hayward - Sawyer County, 
# and Town of Caledonia - Trempealeau County

# Made up variables for reporting unit level
maydata.repunit.join = with(ward.maydata.2016, data.frame(reporting_unit))
maydata.repunit.join$total_ballots = ifelse(ward.maydata.2016$total_ballots == 0,
                                    ward.maydata.2016$total_voters,
                                    ward.maydata.2016$total_ballots)
maydata.repunit.join$tch_prop = with(ward.maydata.2016, dre / total_ballots)
maydata.repunit.join$os_prop = with(ward.maydata.2016, optical_scan_ballots / total_ballots)
maydata.repunit.join$paper_prop = with(ward.maydata.2016, paper_ballots / total_ballots)
maydata.repunit.join$absentee_person_prop = with(ward.maydata.2016, absentee_in_person / absentee_issued)
maydata.repunit.join$inspector_prop = with(ward.maydata.2016, total_election_inspectors / total_ballots)
# maydata.repunit.join$tch_prop = ifelse(maydata.repunit.join$tch_prop>1, 1, maydata.repunit.join$tch_prop)

maydata.repunit.join$turnout_reg_2016 = with(ward.maydata.2016, total_ballots / registrants)
maydata.repunit.join$sameday_reg_prop = with(ward.maydata.2016, election_day_registrants / registrants)
maydata.repunit.join$counted_absent_prop = with(ward.maydata.2016,
                                        counted_2 / total_ballots)
maydata.repunit.join$rejected_absent_prop = with(ward.maydata.2016,
                                         rejected2 / total_ballots)
maydata.repunit.join$registrants = with(ward.maydata.2016, registrants)
maydata.repunit.join$late_registrants = with(ward.maydata.2016, late_registrants / total_ballots)

maydata.repunit.join$ballots_counted_at = ward.maydata.2016$ballots_counted_at
maydata.repunit.join$absentee_undeliverable = with(ward.maydata.2016, absentee_undeliverable / absentee_issued)


# Made up variables for municipality level
maydata.join = with(maydata.municounty, data.frame(muni_county))
maydata.join$total_ballots = ifelse(maydata.municounty$total_ballots == 0,
                                   maydata.municounty$total_voters,
                                   maydata.municounty$total_ballots)
maydata.join$tch_prop = with(maydata.municounty, dre / total_ballots)
# maydata.join$tch_prop = ifelse(maydata.join$tch_prop>1, 1, maydata.join$tch_prop)
maydata.join$os_prop = with(maydata.municounty, optical_scan_ballots / total_ballots)
maydata.join$paper_prop = with(maydata.municounty, paper_ballots / total_ballots)
maydata.join$absentee_person_prop = with(maydata.municounty, absentee_in_person / absentee_issued)
maydata.join$inspector_prop = with(maydata.municounty, total_election_inspectors / total_ballots)

maydata.join$turnout_reg_2016 = with(maydata.municounty, total_ballots / registrants)
maydata.join$sameday_reg_prop = with(maydata.municounty, election_day_registrants / registrants)
maydata.join$counted_absent_prop = with(maydata.municounty,
                                              counted_absent / total_ballots)
maydata.join$rejected_absent_prop = with(maydata.municounty,
                                              rejected_absent / total_ballots)
maydata.join$registrants = with(maydata.municounty, registrants)
maydata.join$late_registrants = with(maydata.municounty, late_registrants / total_ballots)

repunit.2016.2012.o = plyr::join(repunit.2016.2012.o, maydata.join, by = "muni_county")
repunit.2016.2012.r = plyr::join(repunit.2016.2012.r, maydata.join, by = "muni_county")

write.csv(maydata.municounty,"apr.2017.municounty.data.csv")

repunit.2016.2012.o = plyr::join(repunit.2016.2012.o, maydata.repunit.join, by = "reporting_unit")
repunit.2016.2012.r = plyr::join(repunit.2016.2012.r, maydata.repunit.join, by = "reporting_unit")

write.csv(ward.maydata.2016,"apr.2017.repunit.data.csv")

# 2012 election municipal GAB data NEED TO RUN -----------------------------------------------------------

ward.edldata.2012 = read.xlsx2(
  "20121106_gab190_statistics_xls_14031.xls",
  stringsAsFactors = F,
  header = T,
  sheetIndex = 1,
  colClasses = NA
)

# Replace blanks with NA
# ward.edldata.2012 = data.frame(apply(ward.edldata.2012, 2, function(x) gsub("^$|^ $", NA, x)))
ward.edldata.2012 = clean_names(ward.edldata.2012)

# Preparing basic data
ward.edldata.2012 = clean_names(ward.edldata.2012)
ward.edldata.2012$reporting_unit = toupper(ward.edldata.2012$reportingunit)
ward.edldata.2012$municipality = toupper(ward.edldata.2012$municipality)
ward.edldata.2012$county = toupper(ward.edldata.2012$county)

ward.edldata.2012$county = gsub(" COUNTY","", ward.edldata.2012$county)

# The following is fixing a problem I noticed later when trying to match with the 2016 voter df
ward.edldata.2012$municipality[grep("TOWN OF WINDSOR", ward.edldata.2012$municipality)] = "VILLAGE OF WINDSOR"
ward.edldata.2012$reporting_unit = gsub("TOWN OF WINDSOR","VILLAGE OF WINDSOR", ward.edldata.2012$reporting_unit)

# The town in Maine became a village in 2015, and is now in marathon county
# ward.edldata.2012$county[grep("TOWN OF MAINE", ward.edldata.2012$municipality)] = "MARATHON"
ward.edldata.2012$municipality[grepl("TOWN OF MAINE", ward.edldata.2012$municipality) &
                                 grepl("MARATHON", ward.edldata.2012$county)
                               ] = "VILLAGE OF MAINE"
# ward.edldata.2012$reporting_unit = gsub("TOWN OF MAINE","VILLAGE OF MAINE", ward.edldata.2012$reporting_unit)

# the town of Menasha was retained only for wards (8-10, 11-13). The rest (1-7) became the village of
# Fox Crossing, Winnebago
ward.edldata.2012$municipality[grepl("TOWN OF MENASHA WARDS 1, 2, 4, 7", ward.edldata.2012$reporting_unit)
                               ] = "VILLAGE OF FOX CROSSING"
ward.edldata.2012$municipality[grepl("TOWN OF MENASHA WARDS 3, 5, 6", ward.edldata.2012$reporting_unit)
                               ] = "VILLAGE OF FOX CROSSING"
ward.edldata.2012$reporting_unit[grepl("TOWN OF MENASHA WARDS 1, 2, 4, 7", ward.edldata.2012$reporting_unit)
                                 ] = "VILLAGE OF FOX CROSSING WARDS 1-2,4,7"
ward.edldata.2012$reporting_unit[grepl("TOWN OF MENASHA WARDS 3, 5, 6", ward.edldata.2012$reporting_unit)
                                 ] = "VILLAGE OF FOX CROSSING WARDS 3,5-6"

ward.edldata.2012$muni_county = paste(ward.edldata.2012$municipality, ward.edldata.2012$county)


ward.edldata.2012$reporting_unit = paste(ward.edldata.2012$municipality,ward.edldata.2012$reportingunit, ward.edldata.2012$county)
ward.edldata.2012$reporting_unit_orig = toupper(ward.edldata.2012$reporting_unit)


ward.edldata.2012$reporting_unit = gsub("WARDS","WARD",ward.edldata.2012$reporting_unit_orig)
# ward.12.mun$reporting_unit = gsub("WARDS","WARD",ward.12.mun$reporting_unit_orig)
ward.edldata.2012$reporting_unit = gsub("  "," ",ward.edldata.2012$reporting_unit)
# ward.12.mun$reporting_unit = gsub("  "," ",ward.12.mun$reporting_unit)
ward.edldata.2012$reporting_unit = gsub("&","-",ward.edldata.2012$reporting_unit)
# ward.12.mun$reporting_unit = gsub("&","-",ward.12.mun$reporting_unit)

ward.edldata.2012$reporting_unit = gsub("AND","-",ward.edldata.2012$reporting_unit)
# ward.12.mun$reporting_unit = gsub("AND","-",ward.12.mun$reporting_unit)

ward.edldata.2012$reporting_unit = gsub(",","-",ward.edldata.2012$reporting_unit)
# ward.12.mun$reporting_unit = gsub(",","-",ward.12.mun$reporting_unit)
ward.edldata.2012$reporting_unit = gsub(" - ","-",ward.edldata.2012$reporting_unit)
# ward.12.mun$reporting_unit = gsub(" - ","-",ward.12.mun$reporting_unit)
ward.edldata.2012$reporting_unit = gsub(" -","-",ward.edldata.2012$reporting_unit)
# ward.12.mun$reporting_unit = gsub(" -","-",ward.12.mun$reporting_unit)
ward.edldata.2012$reporting_unit = gsub("- ","-",ward.edldata.2012$reporting_unit)
# ward.12.mun$reporting_unit = gsub("- ","-",ward.12.mun$reporting_unit)

ward.edldata.2012$reporting_unit = gsub(" ","-",ward.edldata.2012$reporting_unit)
# ward.12.mun$reporting_unit = gsub(" ","-",ward.12.mun$reporting_unit)

ward.edldata.2012$reporting_unit = gsub("WD","",ward.edldata.2012$reporting_unit)
# ward.12.mun$reporting_unit = gsub("WD","",ward.12.mun$reporting_unit)

ward.edldata.2012$reporting_unit = gsub("COMBINED","",ward.edldata.2012$reporting_unit)
# ward.12.mun$reporting_unit = gsub("COMBINED","",ward.12.mun$reporting_unit)

ward.edldata.2012$reporting_unit = gsub("--","-",ward.edldata.2012$reporting_unit)
# ward.12.mun$reporting_unit = gsub("--","-",ward.12.mun$reporting_u

# Grouping the data into municipalities
ward.edl2012.group = dplyr::group_by(ward.edldata.2012,muni_county, county, municipality)
edl2012.municounty = dplyr::summarise(ward.edl2012.group,
                                      registrants = sum(registrants),
                                      late_registrants = sum(lateregistrants),
                                      edrs = sum(edrs),
                                      # election_day_registrants = sum(election_day_registrants),
                                      total_ballots = sum(totalballots),
                                      total_voters = sum(totalelectors),
                                      paper_ballots = sum(paperballots),
                                      optical_scan_ballots = sum(opticalscan),
                                      dre = sum(dre),
                                      auto_mark = sum(automark),
                                      total_election_inspectors = sum(totalelectioninspectors),
                                      x16_17 = sum(x16_17),
                                      x18_25 = sum(x18_25),
                                      x26_40 = sum(x26_40),
                                      x41_60 = sum(x41_60),
                                      x61_70 = sum(x61_70),
                                      x71 = sum(x71),
                                      # provisional_identification = sum(provisional_identification),
                                      # provisional_no_dl = sum(provisional_no_dl),
                                      # counted_id = sum(counted),
                                      # rejected_id = sum(rejected),
                                      absentee_issued = sum(absenteeissued),
                                      absentee_in_person = sum(absenteeissuedinperson),
                                      absentee_not_returned = sum(absenteenotreturned),
                                      absentee_undeliverable = sum(absenteeundeliverable),
                                      received_by_election_day = sum(absenteereturnedbyelectionday),
                                      counted_absent = sum(absenteecounted),
                                      rejected_absent = sum(absenteerejected),
                                      late_received_after_the_election = sum(absenteelate),
                                      fwab_received = sum(fwabreceived),
                                      fwab_counted = sum(fwabcounted),
                                      fwab_rejected = sum(fwabrejected),
                                      fwab_late = sum(fwablate),
                                      military_issued = sum(militaryissued),
                                      military_sent_but_not_returned = sum(militarynotreturned),
                                      military_undeliverable = sum(militaryundeliverable),
                                      military_received_by_election_day = sum(militaryreturnedbyelectionday),
                                      military_counted = sum(militarycounted),
                                      military_rejected = sum(militaryrejected),
                                      military_late = sum(militarylate),
                                      overseas_issued = sum(overseasissued),
                                      overseas_sent_but_not_returned = sum(overseasnotreturned),
                                      overseas_undeliverable = sum(overseasundeliverable),
                                      overseas_received_by_election_day = sum(overseasreturnedbyelectionday),
                                      overseas_counted = sum(overseascounted),
                                      overseas_rejected = sum(overseasrejected),
                                      overseas_late = sum(overseaslate)
)

# character fields not included:
# ballots_counted_at
# split_shifts
# difficulty obtaining
# polling_place_name
# shared


###
# Seeing what is differnt according to muni county level
###

# Ugh, I need to change the name of these muni_counties to make sure I have a full match
setdiff(unique(edl2012.municounty$muni_county), unique(repunit.2016.2012.o$muni_county))

# [1] "CITY OF ASHLAND BAYFIELD"    has 0 votes
#  "CITY OF COLUMBUS DODGE"        has 0 votes
# "CITY OF HARTFORD DODGE"    has 0 votes
# [4] "CITY OF KAUKAUNA CALUMET" has 0 votes
# "CITY OF MILWAUKEE WASHINGTON"  has 0 votes
# "CITY OF MILWAUKEE WAUKESHA" has 0 votes
# [7] "CITY OF WISCONSIN DELLS JUNEAU" has 0 votes
#    "VILLAGE OF GENOA CITY KENOSHA"    has only 2 votes, maybe can exclude
#  "VILLAGE OF HOWARD OUTAGAMIE" has 0 votes
#  [10] "VILLAGE OF KEWASKUM FOND DU LAC" has 0 votes
#   "VILLAGE OF LAC LA BELLE JEFFERSON" has 0 votes
# "VILLAGE OF PULASKI OCONTO" has 0 votes

setdiff(unique(repunit.2016.2012.o$muni_county), unique(edl2012.municounty$muni_county))

# Village of Harrison Calumet did not exist in 2012, was split off from the town in 2013.
# It is impossible to know the boundaries exactly.
# Therefore the swing from 2012 to 2016 cannot be accurately calculated.

# Same of the town / village of Somers, Kenosha. The town / village is completely mixed and messed up

# the town of Menasha was retained only for wards (8-10, 11-13). The rest (1-7) became the village of
# Fox Crossing, Winnebago

differences = setdiff(unique(repunit.2016.2012.o$muni_county),unique(edl2012.municounty$muni_county))
edl2012.diffs = subset(repunit.2016.2012.o, muni_county %in% differences)


# Made up variables for reporting unit level
repunit.2012.edl.join = with(ward.edldata.2012, data.frame(reporting_unit))
repunit.2012.edl.join$total_ballots_2012 = ifelse(ward.edldata.2012$totalballots == 0,
                                            ward.edldata.2012$totalelectors,
                                            ward.edldata.2012$totalballots)
repunit.2012.edl.join$tch_prop_2012 = with(ward.edldata.2012, dre / totalballots)
# repunit.2012.edl.join$tch_prop_2012 = ifelse(repunit.2012.edl.join$tch_prop>1, 1, repunit.2012.edl.join$tch_prop)

repunit.2012.edl.join$os_prop_2012 = with(ward.edldata.2012, opticalscan / totalballots)
repunit.2012.edl.join$paper_prop_2012 = with(ward.edldata.2012, paperballots / totalballots)
repunit.2012.edl.join$automark_prop_2012 = with(ward.edldata.2012, automark / totalballots)
repunit.2012.edl.join$absentee_person_prop_2012 = with(ward.edldata.2012, absenteeissuedinperson / absenteeissued)
repunit.2012.edl.join$inspector_prop_2012 = with(ward.edldata.2012, totalelectioninspectors / totalballots)

repunit.2012.edl.join$turnout_reg_2012 = with(ward.edldata.2012, totalballots / registrants)
# repunit.2012.edl.join$sameday_reg_prop = with(ward.edldata.2012, election_day_registrants / registrants)
repunit.2012.edl.join$counted_absent_prop_2012 = with(ward.edldata.2012,
                                                absenteecounted / totalballots)
repunit.2012.edl.join$rejected_absent_prop_2012 = with(ward.edldata.2012,
                                                 absenteerejected / totalballots)
repunit.2012.edl.join$registrants_2012 = with(ward.edldata.2012, registrants)
repunit.2012.edl.join$sameday_reg_prop_2012 = with(ward.edldata.2012, edrs / totalballots)
repunit.2012.edl.join$late_registrants_2012 = with(ward.edldata.2012, lateregistrants / totalballots)

repunit.2012.edl.join$ballots_counted_at_2012 = ward.edldata.2012$ballotscountedat
repunit.2012.edl.join$absentee_undeliverable_2012 = with(ward.edldata.2012, absenteeundeliverable / absenteeissued)

# For municipality level data
edl2012.join = with(edl2012.municounty, data.frame(muni_county))
edl2012.join$total_ballots_2012 = ifelse(edl2012.municounty$total_ballots == 0,
                                         edl2012.municounty$total_voters,
                                         edl2012.municounty$total_ballots)
edl2012.municounty$total_ballots_2012 = edl2012.join$total_ballots_2012
edl2012.join$tch_prop_2012 = with(edl2012.municounty, dre / total_ballots)
# edl2012.join$tch_prop_2012 = ifelse(edl2012.join$tch_prop_2012>1, 1, edl2012.join$tch_prop_2012)

edl2012.join$os_prop_2012 = with(edl2012.municounty, optical_scan_ballots / total_ballots)
edl2012.join$paper_prop_2012 = with(edl2012.municounty, paper_ballots / total_ballots)
edl2012.join$automark_prop_2012 = with(edl2012.municounty, auto_mark / total_ballots)
edl2012.join$absentee_person_prop_2012 = with(edl2012.municounty, absentee_in_person / absentee_issued)
edl2012.join$inspector_prop_2012 = with(edl2012.municounty, total_election_inspectors / total_ballots)


edl2012.join$turnout_reg_2012 = with(edl2012.municounty, total_ballots_2012 / registrants)
# edl2012.join$sameday_reg_prop = with(edl2012.municounty, election_day_registrants / registrants)
edl2012.join$counted_absent_prop_2012 = with(edl2012.municounty,
                                             counted_absent / total_ballots)
edl2012.join$rejected_absent_prop_2012 = with(edl2012.municounty,
                                              rejected_absent / total_ballots)
edl2012.join$sameday_reg_prop_2012 = with(edl2012.municounty, edrs / total_ballots)
edl2012.join$registrants_2012 = with(edl2012.municounty, registrants)
edl2012.join$late_registrants_2012 = with(edl2012.municounty, late_registrants / total_ballots)

repunit.2016.2012.o = plyr::join(repunit.2016.2012.o, edl2012.join, by = "muni_county")
repunit.2016.2012.r = plyr::join(repunit.2016.2012.r, edl2012.join, by = "muni_county")

write.csv(edl2012.municounty,"edl.2012.municounty.data.csv")

repunit.2016.2012.o = plyr::join(repunit.2016.2012.o, repunit.2012.edl.join, by = "reporting_unit")
repunit.2016.2012.r = plyr::join(repunit.2016.2012.r, repunit.2012.edl.join, by = "reporting_unit")

# write.csv(edl2012.repunit,"edl.2012.repunit.data.csv")

# 2008 election municipal GAB data NEED TO RUN -----------------------------------------------------------

ward.gabdata.2008 = read.xlsx2(
  "C:/Users/s_cas/Dropbox/Perso/2016 voting election county results/Wisconsin/All prep together/2008 results/2008nov_gab190_statistics_xls_13418MOD.xls",
  stringsAsFactors = F,
  header = T,
  sheetIndex = 1,
  colClasses = NA
)

# Replace blanks with NA
# ward.gabdata.2008 = data.frame(apply(ward.gabdata.2008, 2, function(x) gsub("^$|^ $", NA, x)))
ward.gabdata.2008 = clean_names(ward.gabdata.2008)

# Preparing basic data
# ward.gabdata.2008 = clean_names(ward.gabdata.2008)
ward.gabdata.2008$reporting_unit = toupper(ward.gabdata.2008$reporting_unit)
ward.gabdata.2008$municipality = toupper(ward.gabdata.2008$municipality)
ward.gabdata.2008$county = toupper(ward.gabdata.2008$county)

ward.gabdata.2008$county = gsub(" COUNTY","", ward.gabdata.2008$county)

# The following is fixing a problem I noticed later when trying to match with the 2016 voter df
ward.gabdata.2008$municipality[grep("TOWN OF WINDSOR", ward.gabdata.2008$municipality)] = "VILLAGE OF WINDSOR"
ward.gabdata.2008$reporting_unit = gsub("TOWN OF WINDSOR","VILLAGE OF WINDSOR", ward.gabdata.2008$reporting_unit)

# The town in Maine became a village in 2015, and is now in marathon county
# ward.gabdata.2008$county[grep("TOWN OF MAINE", ward.gabdata.2008$municipality)] = "MARATHON"
ward.gabdata.2008$municipality[grepl("TOWN OF MAINE", ward.gabdata.2008$municipality) &
                                 grepl("MARATHON", ward.gabdata.2008$county)
                               ] = "VILLAGE OF MAINE"
# ward.gabdata.2008$reporting_unit = gsub("TOWN OF MAINE","VILLAGE OF MAINE", ward.gabdata.2008$reporting_unit)

# the town of Menasha was retained only for wards (8-10, 11-13). The rest (1-7) became the village of
# Fox Crossing, Winnebago
ward.gabdata.2008$municipality[grepl("TOWN OF MENASHA WARDS 1, 2, 4, 7", ward.gabdata.2008$reporting_unit)
                               ] = "VILLAGE OF FOX CROSSING"
ward.gabdata.2008$municipality[grepl("TOWN OF MENASHA WARDS 3, 5, 6", ward.gabdata.2008$reporting_unit)
                               ] = "VILLAGE OF FOX CROSSING"
ward.gabdata.2008$reporting_unit[grepl("TOWN OF MENASHA WARDS 1, 2, 4, 7", ward.gabdata.2008$reporting_unit)
                                 ] = "VILLAGE OF FOX CROSSING WARDS 1-2,4,7"
ward.gabdata.2008$reporting_unit[grepl("TOWN OF MENASHA WARDS 3, 5, 6", ward.gabdata.2008$reporting_unit)
                                 ] = "VILLAGE OF FOX CROSSING WARDS 3,5-6"

ward.gabdata.2008$muni_county = paste(ward.gabdata.2008$municipality, ward.gabdata.2008$county)


ward.gabdata.2008$reporting_unit = with(ward.gabdata.2008, paste(municipality,reporting_unit, county))
ward.gabdata.2008$reporting_unit_orig = toupper(ward.gabdata.2008$reporting_unit)


ward.gabdata.2008$reporting_unit = gsub("WARDS","WARD",ward.gabdata.2008$reporting_unit_orig)
# ward.08.mun$reporting_unit = gsub("WARDS","WARD",ward.08.mun$reporting_unit_orig)
ward.gabdata.2008$reporting_unit = gsub("  "," ",ward.gabdata.2008$reporting_unit)
# ward.08.mun$reporting_unit = gsub("  "," ",ward.08.mun$reporting_unit)
ward.gabdata.2008$reporting_unit = gsub("&","-",ward.gabdata.2008$reporting_unit)
# ward.08.mun$reporting_unit = gsub("&","-",ward.08.mun$reporting_unit)

ward.gabdata.2008$reporting_unit = gsub("AND","-",ward.gabdata.2008$reporting_unit)
# ward.08.mun$reporting_unit = gsub("AND","-",ward.08.mun$reporting_unit)

ward.gabdata.2008$reporting_unit = gsub(",","-",ward.gabdata.2008$reporting_unit)
# ward.08.mun$reporting_unit = gsub(",","-",ward.08.mun$reporting_unit)
ward.gabdata.2008$reporting_unit = gsub(" - ","-",ward.gabdata.2008$reporting_unit)
# ward.08.mun$reporting_unit = gsub(" - ","-",ward.08.mun$reporting_unit)
ward.gabdata.2008$reporting_unit = gsub(" -","-",ward.gabdata.2008$reporting_unit)
# ward.08.mun$reporting_unit = gsub(" -","-",ward.08.mun$reporting_unit)
ward.gabdata.2008$reporting_unit = gsub("- ","-",ward.gabdata.2008$reporting_unit)
# ward.08.mun$reporting_unit = gsub("- ","-",ward.08.mun$reporting_unit)

ward.gabdata.2008$reporting_unit = gsub(" ","-",ward.gabdata.2008$reporting_unit)
# ward.08.mun$reporting_unit = gsub(" ","-",ward.08.mun$reporting_unit)

ward.gabdata.2008$reporting_unit = gsub("WD","",ward.gabdata.2008$reporting_unit)
# ward.08.mun$reporting_unit = gsub("WD","",ward.08.mun$reporting_unit)

ward.gabdata.2008$reporting_unit = gsub("COMBINED","",ward.gabdata.2008$reporting_unit)
# ward.08.mun$reporting_unit = gsub("COMBINED","",ward.08.mun$reporting_unit)

ward.gabdata.2008$reporting_unit = gsub("--","-",ward.gabdata.2008$reporting_unit)
# ward.08.mun$reporting_unit = gsub("--","-",ward.08.mun$reporting_u

# Grouping the data into municipalities
ward.edl2008.group = dplyr::group_by(ward.gabdata.2008,muni_county, county, municipality)
edl2008.municounty = dplyr::summarise(ward.edl2008.group,
                                      registrants = sum(earlyregistrants),
                                      late_registrants = sum(lateregistrants),
                                      edrs = sum(edrs),
                                      # election_day_registrants = sum(election_day_registrants),
                                      total_ballots = sum(total_ballots),
                                      total_voters = sum(total_electors),
                                      paper_ballots = sum(paper_ballots),
                                      optical_scan_ballots = sum(optical_scan),
                                      dre = sum(dre_touchscreen_automark),
                                      # auto_mark = sum(automark),
                                      # total_election_inspectors = sum(totalelectioninspectors),
                                      # x16_17 = sum(x16_17),
                                      # x18_25 = sum(x18_25),
                                      # x26_40 = sum(x26_40),
                                      # x41_60 = sum(x41_60),
                                      # x61_70 = sum(x61_70),
                                      # x71 = sum(x71),
                                      # provisional_identification = sum(provisional_identification),
                                      # provisional_no_dl = sum(provisional_no_dl),
                                      # counted_id = sum(counted),
                                      # rejected_id = sum(rejected),
                                      absentee_issued = sum(absenteeissued),
                                      # absentee_in_person = sum(absenteeissuedinperson),
                                      # absentee_not_returned = sum(absenteenotreturned),
                                      absentee_undeliverable = sum(absenteeundeliverable),
                                      # received_by_election_day = sum(absenteereturnedbyelectionday),
                                      counted_absent = sum(absenteecounted),
                                      rejected_absent = sum(absenteerejected),
                                      # late_received_after_the_election = sum(absenteelate),
                                      # fwab_received = sum(fwabreceived),
                                      fwab_counted = sum(fwabcounted),
                                      fwab_rejected = sum(fwabrejected),
                                      # fwab_late = sum(fwablate),
                                      military_issued = sum(militaryissued),
                                      # military_sent_but_not_returned = sum(militarynotreturned),
                                      military_undeliverable = sum(militaryundeliverable),
                                      # military_received_by_election_day = sum(militaryreturnedbyelectionday),
                                      military_counted = sum(militarycounted),
                                      military_rejected = sum(militaryrejected),
                                      # military_late = sum(militarylate),
                                      overseas_issued = sum(overseasissued),
                                      # overseas_sent_but_not_returned = sum(overseasnotreturned),
                                      overseas_undeliverable = sum(overseasundeliverable),
                                      # overseas_received_by_election_day = sum(overseasreturnedbyelectionday),
                                      overseas_counted = sum(overseascounted),
                                      overseas_rejected = sum(overseasrejected)
                                      # overseas_late = sum(overseaslate)
)

###
# Seeing what is differnt according to muni county level
###

# # Ugh, I need to change the name of these muni_counties to make sure I have a full match
# setdiff(unique(edl2008.municounty$muni_county), unique(ward.2016.2008.o.dem$muni_county))
# 
# # [1] "CITY OF ASHLAND BAYFIELD"    has 0 votes
# #  "CITY OF COLUMBUS DODGE"        has 0 votes
# # "CITY OF HARTFORD DODGE"    has 0 votes
# # [4] "CITY OF KAUKAUNA CALUMET" has 0 votes
# # "CITY OF MILWAUKEE WASHINGTON"  has 0 votes
# # "CITY OF MILWAUKEE WAUKESHA" has 0 votes
# # [7] "CITY OF WISCONSIN DELLS JUNEAU" has 0 votes
# #    "VILLAGE OF GENOA CITY KENOSHA"    has only 2 votes, maybe can exclude
# #  "VILLAGE OF HOWARD OUTAGAMIE" has 0 votes
# #  [10] "VILLAGE OF KEWASKUM FOND DU LAC" has 0 votes
# #   "VILLAGE OF LAC LA BELLE JEFFERSON" has 0 votes
# # "VILLAGE OF PULASKI OCONTO" has 0 votes
# 
# setdiff(unique(ward.2016.2008.o.dem$muni_county), unique(edl2008.municounty$muni_county))
# 
# # Village of Harrison Calumet did not exist in 2008, was split off from the town in 2013.
# # It is impossible to know the boundaries exactly.
# # Therefore the swing from 2008 to 2016 cannot be accurately calculated.
# 
# # Same of the town / village of Somers, Kenosha. The town / village is completely mixed and messed up
# 
# # the town of Menasha was retained only for wards (8-10, 11-13). The rest (1-7) became the village of
# # Fox Crossing, Winnebago
# 
# differences = setdiff(unique(ward.2016.2008.o.dem$muni_county),unique(edl2008.municounty$muni_county))
# edl2008.diffs = subset(ward.2016.2008.o.dem, muni_county %in% differences)


# Made up variables for reporting unit level
repunit.2008.edl.join = with(ward.gabdata.2008, data.frame(reporting_unit))
repunit.2008.edl.join$total_ballots_2008 = ifelse(ward.gabdata.2008$total_ballots == 0,
                                                  ward.gabdata.2008$total_electors,
                                                  ward.gabdata.2008$total_ballots)
repunit.2008.edl.join$tch_prop_2008 = with(ward.gabdata.2008, dre_touchscreen_automark / total_ballots)
# repunit.2008.edl.join$tch_prop_2008 = ifelse(repunit.2008.edl.join$tch_prop>1, 1, repunit.2008.edl.join$tch_prop)

repunit.2008.edl.join$os_prop_2008 = with(ward.gabdata.2008, optical_scan / total_ballots)
repunit.2008.edl.join$paper_prop_2008 = with(ward.gabdata.2008, paper_ballots / total_ballots)
# repunit.2008.edl.join$automark_prop_2008 = with(ward.gabdata.2008, automark / total_ballots)
# repunit.2008.edl.join$absentee_person_prop_2008 = with(ward.gabdata.2008, absenteeissuedinperson / absenteeissued)
# repunit.2008.edl.join$inspector_prop_2008 = with(ward.gabdata.2008, totalelectioninspectors / total_ballots)


repunit.2008.edl.join$turnout_reg_2008 = with(ward.gabdata.2008, total_ballots / earlyregistrants)
# repunit.2008.edl.join$sameday_reg_prop = with(ward.gabdata.2008, election_day_registrants / registrants)
repunit.2008.edl.join$counted_absent_prop_2008 = with(ward.gabdata.2008,
                                                      absenteecounted / total_ballots)
repunit.2008.edl.join$rejected_absent_prop_2008 = with(ward.gabdata.2008,
                                                       absenteerejected / total_ballots)
repunit.2008.edl.join$registrants_2008 = with(ward.gabdata.2008, earlyregistrants)
repunit.2008.edl.join$sameday_reg_prop_2008 = with(ward.gabdata.2008, edrs / total_ballots)
repunit.2008.edl.join$late_registrants_2008 = with(ward.gabdata.2008, lateregistrants / total_ballots)

repunit.2008.edl.join$ballots_counted_at_2008 = ward.gabdata.2008$ballots_counted_at
repunit.2008.edl.join$absentee_undeliverable_2008 = with(ward.gabdata.2008, absenteeundeliverable / absenteeissued)

# For municipality level data
edl2008.join = with(edl2008.municounty, data.frame(muni_county))
edl2008.join$total_ballots_2008 = ifelse(edl2008.municounty$total_ballots == 0,
                                         edl2008.municounty$total_voters,
                                         edl2008.municounty$total_ballots)
edl2008.municounty$total_ballots_2008 = edl2008.join$total_ballots_2008
edl2008.join$tch_prop_2008 = with(edl2008.municounty, dre / total_ballots)
# edl2008.join$tch_prop_2008 = ifelse(edl2008.join$tch_prop_2008>1, 1, edl2008.join$tch_prop_2008)

edl2008.join$os_prop_2008 = with(edl2008.municounty, optical_scan_ballots / total_ballots)
edl2008.join$paper_prop_2008 = with(edl2008.municounty, paper_ballots / total_ballots)
edl2008.join$automark_prop_2008 = with(edl2008.municounty, auto_mark / total_ballots)
edl2008.join$absentee_person_prop_2008 = with(edl2008.municounty, absentee_in_person / absentee_issued)
edl2008.join$inspector_prop_2008 = with(edl2008.municounty, total_election_inspectors / total_ballots)


edl2008.join$turnout_reg_2008 = with(edl2008.municounty, total_ballots_2008 / registrants)
# edl2008.join$sameday_reg_prop = with(edl2008.municounty, election_day_registrants / registrants)
edl2008.join$counted_absent_prop_2008 = with(edl2008.municounty,
                                             counted_absent / total_ballots)
edl2008.join$rejected_absent_prop_2008 = with(edl2008.municounty,
                                              rejected_absent / total_ballots)
edl2008.join$sameday_reg_prop_2008 = with(edl2008.municounty, edrs / total_ballots)
edl2008.join$registrants_2008 = with(edl2008.municounty, registrants)
edl2008.join$late_registrants_2008 = with(edl2008.municounty, late_registrants / total_ballots)

# ward.2016.2008.o.dem = plyr::join(ward.2016.2008.o.dem, edl2008.join, by = "muni_county")
# repunit.2016.2008.r = plyr::join(repunit.2016.2008.r, edl2008.join, by = "muni_county")

write.csv(edl2008.municounty,"edl.2008.municounty.data.csv")

repunit.2016.2008.o.dem = plyr::join(repunit.2016.2008.o.dem, repunit.2008.edl.join, by = "reporting_unit")
repunit.2016.2008.r = plyr::join(repunit.2016.2008.r, repunit.2008.edl.join, by = "reporting_unit")

# write.csv(edl2008.repunit,"edl.2008.repunit.data.csv")

# Spring 2016 Republican primary data ------------------------------------------------
setwd("C:/Users/s_cas/Dropbox/Perso/2016 voting election county results/Wisconsin/All prep together/All processed files/Spring primary")
getwd()

ward.prim.20162 = read.xlsx2(
  "Ward by Ward Report by CD-PresidentMOD.xlsx",
  stringsAsFactors = F,
  header = T,
  sheetIndex = 1
)
  # Replace blanks with NA
  ward.prim.20162 = data.frame(apply(ward.prim.20162, 2, function(x) gsub("^$|^ $", NA, x)))
  ward.prim.20162 = clean_names(ward.prim.20162)

{ # Preparing basic data
  ward.prim.20163 = subset(ward.prim.20162,grepl("Totals",ward.prim.20162$county) == F)
  ward.prim.20164 = subset(ward.prim.20163,grepl("Subtotals",ward.prim.20163$reporting_unit) == F)
  ward.prim.2016 = ward.prim.20164

  ward.prim.2016$municipality_name = ward.prim.2016$reporting_unit
  ward.prim.2016$municipality_name = gsub( " Ward.*$", "", ward.prim.2016$municipality_name)

  require(zoo)
  ward.prim.2016$congress_district = na.locf(ward.prim.2016$congress_district)
  ward.prim.2016$county = na.locf(ward.prim.2016$county)
  trim.trailing <- function (x) sub("\\s+$", "", x)
  ward.prim.2016$county = trim.trailing(ward.prim.2016$county)

  ward.prim.2016$municipality_name = toupper(ward.prim.2016$municipality_name)
  ward.prim.2016$county = toupper(ward.prim.2016$county)
  
  ward.prim.2016$muni_county = paste(ward.prim.2016$municipality_name, ward.prim.2016$county)
  
  ###
  ward.prim.2016$reporting_unit = toupper(ward.prim.2016$reporting_unit)
  
  ward.prim.2016$reporting_unit = paste(ward.prim.2016$reporting_unit, ward.prim.2016$county)
  ward.prim.2016$reporting_unit_orig = toupper(ward.prim.2016$reporting_unit)
  
  ward.prim.2016$reporting_unit = gsub("WARDS","WARD",ward.prim.2016$reporting_unit_orig)
  ward.prim.2016$reporting_unit = gsub("  "," ",ward.prim.2016$reporting_unit)
  ward.prim.2016$reporting_unit = gsub("&","-",ward.prim.2016$reporting_unit)
  ward.prim.2016$reporting_unit = gsub("AND","-",ward.prim.2016$reporting_unit)
  ward.prim.2016$reporting_unit = gsub(",","-",ward.prim.2016$reporting_unit)
  ward.prim.2016$reporting_unit = gsub(" - ","-",ward.prim.2016$reporting_unit)
  ward.prim.2016$reporting_unit = gsub(" -","-",ward.prim.2016$reporting_unit)
  ward.prim.2016$reporting_unit = gsub("- ","-",ward.prim.2016$reporting_unit)
  ward.prim.2016$reporting_unit = gsub(" ","-",ward.prim.2016$reporting_unit)
  ward.prim.2016$reporting_unit = gsub("WD","",ward.prim.2016$reporting_unit)
  ward.prim.2016$reporting_unit = gsub("COMBINED","",ward.prim.2016$reporting_unit)
  ward.prim.2016$reporting_unit = gsub("--","-",ward.prim.2016$reporting_unit)

  ward.prim.2016.look = with(ward.prim.2016, data.frame(county, municipality_name,
                                              reporting_unit, muni_county))
  colnames(ward.prim.2016.look) = c("county","municipality","reporting_unit",
                               "muni_county")

  rm(ward.prim.20162,ward.prim.20163,ward.prim.20164)

  ward.prim.2016.v = ward.prim.2016
  ward.prim.2016.v$county = NULL
  ward.prim.2016.v$municipality_name = NULL
  ward.prim.2016.v$total_votes = NULL
  # ward.prim.2016.v$reporting_unit = NULL
  ward.prim.2016.v$congress_district = NULL

  ward.prim.2016.l = gather(ward.prim.2016.v, reporting_unit)
  colnames(ward.prim.2016.l) = c("reporting_unit", "cand", "votes_rec")

  ward.prim.2016.l$cand.group = NA
  ward.prim.2016.l$cand.group[grep("donald_j_trump",ward.prim.2016.l$cand)] = "trump"
  ward.prim.2016.l$cand.group[grep("john_r_kasich",ward.prim.2016.l$cand)] = "kasich"
  ward.prim.2016.l$cand.group[grep("ted_cruz",ward.prim.2016.l$cand)] = "cruz"
  ward.prim.2016.l$cand.group[is.na(ward.prim.2016.l$cand.grou)] = "other"

  # Joining back county and municipality names
  ward.prim.2016.l = plyr::join(ward.prim.2016.l, ward.prim.2016.look, by = "reporting_unit", match = "first")

  ward.prim.2016.l$votes_rec = as.numeric(as.integer(ward.prim.2016.l$votes_rec))
  

  tot.votes.look.group = dplyr::group_by(ward.prim.2016.l,reporting_unit)
  tot.votes.look = dplyr::summarise(tot.votes.look.group,
                                    votes_tot = sum(votes_rec,na.rm=T)
  )

  ward.prim.2016.l = plyr::join(ward.prim.2016.l,tot.votes.look,by="reporting_unit",match="first")
  ward.prim.2016.l$votes_perc = (ward.prim.2016.l$votes_rec / ward.prim.2016.l$votes_tot) * 100

  ward.prim.2016.l$muni_county = paste(ward.prim.2016.l$municipality, ward.prim.2016.l$county)

   # ward.prim.2016.l$votes_perc = (ward.prim.2016.l$votes_rec / ward.prim.2016.l$votes_tot) * 100
  
  ward.prim.2016.melt = melt(ward.prim.2016.l)
  ward.prim.2016.cast = dcast(ward.prim.2016.melt, reporting_unit + muni_county + municipality + county ~ variable + cand.group, sum,na.rm=T)
  ward.prim.2016.l = ward.prim.2016.cast
  
  ward.prim.2016.l$trump_prim_won = with(ward.prim.2016.l,
                                           ifelse(votes_perc_trump > votes_perc_kasich &
                                                    votes_perc_trump > votes_perc_cruz &
                                                    votes_perc_trump > votes_perc_other,1,0))
  
  ward.prim.2016.l$votes_tot_cruz = NULL
  ward.prim.2016.l$votes_tot_trump = NULL
  ward.prim.2016.l$votes_tot_kasich = NULL
  
  colnames(ward.prim.2016.l)[grep("votes_tot_other",colnames(ward.prim.2016.l))] = "votes_tot"

  rm(ward.prim.2016.cast,ward.prim.2016.melt,
  ward.prim.2016.mun.group ,
  ward.prim.2016, ward.prim.2016.look, ward.prim.2016.v,
  tot.votes.look,tot.votes.look.group)

} # Preparing basic data
{ # Spring primary voting statistics June 2016
  ward.jundata.2016 = read.xlsx2(
    "2016_spring_primary_gab190nf_20160617_xlsx_20273.xlsx",
    stringsAsFactors = F,
    header = T,
    sheetIndex = 1,
    colClasses = NA # Thanks: http://www.briandalessandro.com/blog/how-to-read-numeric-columns-with-read-xlsx2-in-r/
  )

  # Preparing basic data
  ward.jundata.2016 = clean_names(ward.jundata.2016)
  ward.jundata.2016$reporting_unit = toupper(ward.jundata.2016$reporting_unit)
  ward.jundata.2016$municipality = toupper(ward.jundata.2016$municipality)
  ward.jundata.2016$county = toupper(ward.jundata.2016$county)

  ward.jundata.2016$county = gsub(" COUNTY","", ward.jundata.2016$county)
  ward.jundata.2016$muni_county = paste(ward.jundata.2016$municipality, ward.jundata.2016$county)
  
  ward.jundata.2016$reporting_unit = toupper(ward.jundata.2016$reporting_unit)
  
  ward.jundata.2016$reporting_unit = paste(ward.jundata.2016$municipality,ward.jundata.2016$reporting_unit, ward.jundata.2016$county)
  ward.jundata.2016$reporting_unit_orig = toupper(ward.jundata.2016$reporting_unit)
  
  ward.jundata.2016$reporting_unit = gsub("WARDS","WARD",ward.jundata.2016$reporting_unit_orig)
  ward.jundata.2016$reporting_unit = gsub("  "," ",ward.jundata.2016$reporting_unit)
  ward.jundata.2016$reporting_unit = gsub("&","-",ward.jundata.2016$reporting_unit)
  ward.jundata.2016$reporting_unit = gsub("AND","-",ward.jundata.2016$reporting_unit)
  ward.jundata.2016$reporting_unit = gsub(",","-",ward.jundata.2016$reporting_unit)
  ward.jundata.2016$reporting_unit = gsub(" - ","-",ward.jundata.2016$reporting_unit)
  ward.jundata.2016$reporting_unit = gsub(" -","-",ward.jundata.2016$reporting_unit)
  ward.jundata.2016$reporting_unit = gsub("- ","-",ward.jundata.2016$reporting_unit)
  ward.jundata.2016$reporting_unit = gsub(" ","-",ward.jundata.2016$reporting_unit)
  ward.jundata.2016$reporting_unit = gsub("WD","",ward.jundata.2016$reporting_unit)
  ward.jundata.2016$reporting_unit = gsub("COMBINED","",ward.jundata.2016$reporting_unit)
  ward.jundata.2016$reporting_unit = gsub("--","-",ward.jundata.2016$reporting_unit)

  ward.jundata.group = dplyr::group_by(ward.jundata.2016,reporting_unit, muni_county, county, municipality)
  jundata.municounty = dplyr::summarise(ward.jundata.group,
                                        registrants = sum(registrants),
                                        late_registrants = sum(late_registrants),
                                        election_day_registrants = sum(election_day_registrants),
                                        total_ballots = sum(total_ballots),
                                        total_voters = sum(total_voters),
                                        paper_ballots = sum(paper_ballots),
                                        optical_scan_ballots = sum(optical_scan_ballots),
                                        dre = sum(dre),
                                        auto_mark = sum(auto_mark),
                                        total_election_inspectors = sum(total_election_inspectors),
                                        x16_17 = sum(x16_17),
                                        x18_25 = sum(x18_25),
                                        x26_40 = sum(x26_40),
                                        x41_60 = sum(x41_60),
                                        x61_70 = sum(x61_70),
                                        provisional_no_dl = sum(provisional_no_dl),
                                        provisional_no_por = sum(provisional_no_por),
                                        counted_id = sum(counted),
                                        rejected_id = sum(rejected),
                                        absentee_issued = sum(absentee_issued),
                                        absentee_in_person = sum(absentee_in_person),
                                        absentee_not_returned = sum(absentee_not_returned),
                                        absentee_undeliverable = sum(absentee_undeliverable),
                                        received_by_election_day = sum(received_by_election_day),
                                        counted_absent = sum(absentee_counted),
                                        rejected_absent = sum(absentee_rejected),
                                        # late_received_after_the_election = sum(late_received_after_the_election),
                                        fwab_received = sum(fwab_received),
                                        fwab_counted = sum(fwab_counted),
                                        fwab_rejected = sum(fwab_rejected),
                                        fwab_late = sum(fwab_late),
                                        military_issued = sum(military_issued),
                                        military_unreturned = sum(military_unreturned),
                                        military_undeliverable = sum(military_undeliverable),
                                        military_by_electionday = sum(military_by_electionday),
                                        military_counted = sum(military_counted),
                                        military_rejected = sum(military_rejected),
                                        military_late = sum(military_late)
)

  # character fields not included:
  # ballots_counted_at
  # split_shifts
  # difficulty obtaining
  # polling_place_name
  # shared

  ward.jundata.group = NULL

  write.csv(jundata.municounty,"jun.2016.prim.repunit.data.csv")
} # Spring primary voting statistics June 2016
{ # Matching voting statistics with the election statistics

  # Maybe need to change the name of these muni_counties to make sure I have a full match
  setdiff(unique(jundata.municounty$muni_county), unique(ward.prim.2016.l$muni_county))
  setdiff(unique(ward.prim.2016.l$muni_county), unique(jundata.municounty$muni_county))

  ward.prim.2016.l$muni_county[grep("VILLAGE OF MAINE MARATHON",ward.prim.2016.l$muni_county)] = "TOWN OF MAINE MARATHON"
  ward.prim.2016.l$municipality[grep("TOWN OF MAINE MARATHON",ward.prim.2016.l$muni_county)] = "TOWN OF MAINE"

  differences = setdiff(unique(ward.prim.2016.l$muni_county),unique(jundata.municounty$muni_county))
  jundata.diffs = subset(ward.prim.2016.l, muni_county %in% differences)

  # Missing:
  # TOWN OF HAYWARD (all put in city of Hayward), Albion Trempeleau (no data),
  # Caledonia Trempeleau (no data), Hendren Clark (no data), Strum Trempeleau (no data)

  jundata.join = with(jundata.municounty, data.frame(reporting_unit,muni_county))
  jundata.join$total_ballots = ifelse(jundata.municounty$total_ballots == 0,
                                      jundata.municounty$total_voters,
                                      jundata.municounty$total_ballots)
  jundata.join$tch_prop = with(jundata.municounty, dre / total_ballots)
  jundata.join$tch_prop = ifelse(jundata.join$tch_prop>1, 1, jundata.join$tch_prop)

  jundata.join$turnout_perc = with(jundata.municounty, total_ballots / registrants)
  jundata.join$turnout_perc[jundata.join$turnout_perc == Inf] = NA
  jundata.join$sameday_reg_prop = with(jundata.municounty, election_day_registrants / registrants)
  jundata.join$sameday_reg_prop[jundata.join$sameday_reg_prop == Inf] = NA
  jundata.join$counted_absent_prop = with(jundata.municounty,
                                          counted_absent / total_ballots)
  jundata.join$counted_absent_prop[jundata.join$counted_absent_prop == Inf] = NA
  jundata.join$rejected_absent_prop = with(jundata.municounty,
                                           rejected_absent / total_ballots)
  jundata.join$registrants = with(jundata.municounty, registrants)
  jundata.join$late_registrants = with(jundata.municounty, late_registrants / total_ballots)

  jundata.join[is.na(jundata.join)] = NA

  ward.prim.2016.l2 = plyr::join(ward.prim.2016.l, jundata.join, by = "reporting_unit")
  ward.prim.2016.l = ward.prim.2016.l2

  ward.prim.2016.l[is.na(ward.prim.2016.l)] = NA
  rm(ward.prim.2016.l2)

} # Matching voting statistics with the election statistics
{
    # Adding demographic data to the ward data frame
    demographics.caps = demographics
    demographics.caps$county = toupper(demographics.caps$county)

    # Demographics from David Griffen and co
    ward.prim.2016.dem = plyr::join(ward.prim.2016.l, demographics.caps, by = "county", match = "first")

    # Also joining the data from the Election snapshot
    ward.prim.2016.dem = plyr::join(ward.prim.2016.dem, elec.snapshot.dem.wi, by = "county", match = "first")
  } # Pasting demographics onto the ward dataframe
{
  setwd("C:/Users/s_cas/Dropbox/Perso/2016 voting election county results/Wisconsin/All prep together/All processed files")
  off_vote_machines = read.xlsx2("voting_equipment_by_municipality_09_2016_xlsx_78114.xlsx1207162619.xlsx",
                                 sheetIndex = 1,
                                 stringsAsFactors=F)

  colnames(off_vote_machines)[3] = "os_model_off"
  colnames(off_vote_machines)[4] = "tch_bmd_model_off"

  off_vote_machines$tch_use_off = NA
  off_vote_machines$tch_use_off = ifelse(off_vote_machines$tch_bmd_model_off == "Dominion (Premier)-Accuvote TSX" |
                                           off_vote_machines$tch_bmd_model_off == "Dominion (Sequoia)/Command Central-Edge" |
                                           off_vote_machines$tch_bmd_model_off == "Dominion (Sequoia)/Command Central-Edge; ES&S Automark" |
                                           off_vote_machines$tch_bmd_model_off == "ES&S iVotronic",1, 0
  )
  off_vote_machines$bmd_use_off = NA
  off_vote_machines$bmd_use_off = ifelse(off_vote_machines$tch_bmd_model_off == "Dominion ImageCast Evolution" |
                                           off_vote_machines$tch_bmd_model_off == "ES&S Automark" |
                                           off_vote_machines$tch_bmd_model_off == "ES&S ExpressVote" |
                                           off_vote_machines$tch_bmd_model_off == "Populex-Populex 2.3" |
                                           off_vote_machines$tch_bmd_model_off == "Vote Pad-Vote Pad (non-electronic)"
                                         ,1, 0
  )
  off_vote_machines$os_use_off = NA
  off_vote_machines$os_use_off = ifelse(off_vote_machines$os_model_off == "None ",
                                        0, 1
  )
  off_vote_machines$os_model_off = ifelse(off_vote_machines$os_model_off == "None ",
                                          NA, off_vote_machines$os_model_off
  )

  off_vote_machines$tch_model_off = NA
  off_vote_machines$tch_model_off = ifelse(off_vote_machines$tch_use_off == 1,
                                           off_vote_machines$tch_bmd_model_off, NA  )

  off_vote_machines$bmd_model_off = NA
  off_vote_machines$bmd_model_off = ifelse(off_vote_machines$bmd_use_off == 1,
                                           off_vote_machines$tch_bmd_model_off, NA  )

  off_vote_machines$tch_use_off_true = NA
  off_vote_machines$tch_use_off_true = ifelse(off_vote_machines$os_use_off == 0,
                                              off_vote_machines$tch_use_off, 0  )

  code_2 = gsub( " COUNTY.*$", "", off_vote_machines$County)
  code_3 = gsub( " -.*$", "", off_vote_machines$Municipality)
  off_vote_machines$muni_county = paste(code_3,code_2, sep = " ")
  #
  # unique(grepl("550101201",ward.prim.2016.dem$MCD_FIPS))

  # setdiff(off_vote_machines$muni_county, ward.prim.2016.dem$muni_county)
  # off_vote_machines$muni_county[grep("TOWN OF GEORGETOWN  POLK", off_vote_machines$muni_county)] = "TOWN OF GEORGETOWN POLK"
  # off_vote_machines$muni_county[grep("TOWN OF WINDSOR DANE", off_vote_machines$muni_county)] = "VILLAGE OF WINDSOR DANE"
  # off_vote_machines$muni_county[grep("TOWN OF MAINE MARATHON", off_vote_machines$muni_county)] = "VILLAGE OF MAINE MARATHON"

  # setdiff(ward.prim.2016.dem$muni_county, off_vote_machines$muni_county) # Difference of 47
  # length(off_vote_machines$muni_county) # 1854
  # length(ward.prim.2016.dem$muni_county) # 1900 Difference of 46. Plus the errant space = 47. Therefore, these places are simply not included in the official list?

  off_vote_machines_cut = off_vote_machines
  off_vote_machines$County = NULL
  off_vote_machines$Municipality = NULL

  ward.prim.2016.dem = plyr::join(ward.prim.2016.dem, off_vote_machines, by = "muni_county")

  ward.prim.2016.dem$tch_model_off_full = NA
  ward.prim.2016.dem$tch_model_off_full = with(ward.prim.2016.dem, ifelse(tch_model_off == "Dominion (Premier)-Accuvote TSX",
                                                          "Dominion (Premier)-Accuvote TSX", ward.prim.2016.dem$tch_model_off_full))
  ward.prim.2016.dem$tch_model_off_full = with(ward.prim.2016.dem, ifelse(tch_model_off == "Dominion (Sequoia)/Command Central-Edge",
                                                          "Dominion (Sequoia)/Command Central-Edge",ward.prim.2016.dem$tch_model_off_full))
  ward.prim.2016.dem$tch_model_off_full = with(ward.prim.2016.dem, ifelse(tch_model_off == "Dominion (Sequoia)/Command Central-Edge; ES&S Automark",
                                                          "Dominion (Sequoia)/Command Central-Edge",ward.prim.2016.dem$tch_model_off_full))
  ward.prim.2016.dem$tch_model_off_full = with(ward.prim.2016.dem, ifelse(tch_model_off == "ES&S iVotronic",
                                                          "ES&S iVotronic",ward.prim.2016.dem$tch_model_off_full))
  ward.prim.2016.dem$tch_model_off_full[is.na(ward.prim.2016.dem$tch_model_off_full)] = "aNone"
  ward.prim.2016.dem$tch_model_off_full = with(ward.prim.2016.dem, ifelse(is.na(tch_use_off),
                                                          NA,ward.prim.2016.dem$tch_model_off_full))

  colnames(ward.prim.2016.dem)[grep("tch.use",colnames(ward.prim.2016.dem),fixed = T)] = "tch_use_verivote"
  colnames(ward.prim.2016.dem)[grep("os.use",colnames(ward.prim.2016.dem),fixed = T)] = "os_use_verivote"
  colnames(ward.prim.2016.dem)[grep("bmd.use",colnames(ward.prim.2016.dem),fixed = T)] = "bmd_use_verivote"
  colnames(ward.prim.2016.dem)[grep("X._collegedegreepersons25._2013",colnames(ward.prim.2016.dem))] = "coll_deg_plus25_2013"
  } # Putting in voting machines
{ # Making model df
  model.prim.repunit.df = ward.prim.2016.dem
  # model.prim.df = subset(model.prim.df, votes_tot > 19)

    # Following Nate Cohn's method: https://twitter.com/Nate_Cohn/status/801226924156719104/photo/1?ref_src=twsrc%5Etfw
    # I found it's better to use the non-logged version of pop.sq.mile.2010
   model.prim.repunit.df$log.pop.sq.mile.2010 = log(model.prim.repunit.df$pop_sq_mile_2010)

    # ratio college and income are both highly correlated so maybe don't use median income
    # population density is highly correlated with black population, so I removed population density
    # I was not able to effectively model Menonminee county with my data (it has a very high Amerindian population). So,
    # I replaced the correlated and generally not significant 'white_pct' variable with 'white_nonwhite*

    ##
   model.prim.repunit.df2 = with(model.prim.repunit.df, data.frame(reporting_unit,county, muni_county,
     votes_perc_trump, votes_perc_kasich, votes_perc_cruz,votes_perc_other,trump_prim_won,turnout_perc,
      tch_prop,counted_absent_prop,rejected_absent_prop,
      sameday_reg_prop, total_ballots,registrants,late_registrants,
      county_paper_or_paperplusmachine,
      pct_white, ratio_nocollege_college, log.pop.sq.mile.2010,
      pop_sq_mile_2010,tch_model_off_full, os_model_off, bmd_model_off,
      medianhouseholdincome_2009.2013, pct_graduate, coll_deg_plus25_2013,
      votes_tot, inc_2015,inc_2015, unemp_rate,
      pct_male, pct_old, pct_latino, pct_white,
      pct_black,pct_hs))

    rownames(model.prim.repunit.df2) = model.prim.repunit.df2$reporting_unit

    model.prim.repunit.df = model.prim.repunit.df2
  } # Making model df
{ # Getting municipality densities
    # require(rgdal); require(sp)
    # 
    # setwd("C:/Users/s_cas/Dropbox/Perso/2016 voting election county results/Wisconsin/All prep together/All processed files/")
    # getwd()
    # 
    # wisc.spat.o <-
    #   readOGR(
    #     dsn = "Prepped files/wisc.spat.orig.geojson",
    #     layer = "OGRGeoJSON",
    #     disambiguateFIDs = T,
    #     stringsAsFactors = F
    #   )
    # 
    # # First, getting population densities. Transforming to utm, zone 16 for Wisconsin
    # 
    # wisc.spat.o.trans = spTransform(
    #   wisc.spat.o,
    #   CRS(
    #     "+proj=utm +zone=16 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
    #   )
    # )
    # 
    # ur.area<-sapply(slot(wisc.spat.o.trans, "polygons"), function(x) sapply(slot(x, "Polygons"), slot, "area"))
    # ur.area.s = lapply(ur.area,sum)
    # 
    # wisc.spat.o@data$muni.area = unlist(ur.area.s) / 1000000
    # wisc.spat.o@data$muni.dense = as.numeric(wisc.spat.o@data$pop) / wisc.spat.o@data$muni.area
    # 
    # # head(wisc.spat.o@data$voters.tot2016)
    # # head(wisc.spat.o@data$muni.area)
    # 
    # density.join = with(wisc.spat.o@data, data.frame(muni_county,muni.dense))
    # 
    # model.prim.repunit.df = plyr::join(model.prim.repunit.df, density.join, by = "muni_county")

  } # Municipality densities
{ ### Creating a carto-presentable map
    require(rgdal); require(sp); require(geojson)

    # wisc.spat.o <-
    #   readOGR(
    #     dsn = "Prepped files/wisc.spat.orig.geojson",
    #     layer = "OGRGeoJSON",
    #     disambiguateFIDs = T,
    #     stringsAsFactors = F
    #   )

    # wisc.spat.o.cut = wisc.spat.o
    # wisc.spat.o.cut@data[7:length(wisc.spat.o.cut@data)] = NULL
    # wisc.spat.o.cut@data$MCD_NAME = toupper(wisc.spat.o.cut@data$MCD_NAME)
    # 
    # wisc.join = wisc.spat.o.cut
    # 
    # wisc.join@data = plyr::join(wisc.join@data, model.prim.repunit.df, by = "muni_county", match = "first")
    # colnames(wisc.join@data) = make.unique(colnames(wisc.join@data))
    # wisc.join@data$county = NULL
    # wisc.join@data$MCD_FIPS.1 = NULL
    # 
    # wisc.join@data[wisc.join@data == Inf] = NA
    # wisc.join@data[wisc.join@data == -Inf] = NA
    # 
    # wisc.join@data[is.na(wisc.join@data)] = NA
    # wisc.join.df = as.data.frame(wisc.join)

    # writeOGR(
    #   wisc.join,
    #   "Prepped files/wisc.present.repprim.geojson",
    #   layer = "wisc.join",
    #   driver = "GeoJSON"
    # )
  } # Creating a carto-presentable map

write.csv(model.prim.df,"model_reprim.csv")

write.csv(model.prim.repunit.df,"model_reprim_repunit.csv")

# Spring 2016 Democrat primary data ------------------------------------------------
setwd("C:/Users/s_cas/Dropbox/Perso/2016 voting election county results/Wisconsin/All prep together/All processed files/Spring primary")
getwd()

ward.prim.20162 = read.xlsx2(
  "Ward by Ward Report by CD-PresidentMOD.xlsx",
  stringsAsFactors = F,
  header = T,
  sheetIndex = 2
)
# Replace blanks with NA
ward.prim.20162 = data.frame(apply(ward.prim.20162, 2, function(x) gsub("^$|^ $", NA, x)))
ward.prim.20162 = clean_names(ward.prim.20162)

{ # Preparing basic data
  ward.prim.20163 = subset(ward.prim.20162,grepl("Totals",ward.prim.20162$county) == F)
  ward.prim.20164 = subset(ward.prim.20163,grepl("Subtotals",ward.prim.20163$reporting_unit) == F)
  ward.prim.2016 = ward.prim.20164
  
  ward.prim.2016$municipality_name = ward.prim.2016$reporting_unit
  ward.prim.2016$municipality_name = gsub( " Ward.*$", "", ward.prim.2016$municipality_name)
  
  require(zoo)
  ward.prim.2016$congress_district = na.locf(ward.prim.2016$congress_district)
  ward.prim.2016$county = na.locf(ward.prim.2016$county)
  trim.trailing <- function (x) sub("\\s+$", "", x)
  ward.prim.2016$county = trim.trailing(ward.prim.2016$county)
  
  ward.prim.2016$municipality_name = toupper(ward.prim.2016$municipality_name)
  ward.prim.2016$county = toupper(ward.prim.2016$county)
  
  ward.prim.2016$muni_county = paste(ward.prim.2016$municipality_name, ward.prim.2016$county)
  
  ###
  ward.prim.2016$reporting_unit = toupper(ward.prim.2016$reporting_unit)
  
  ward.prim.2016$reporting_unit = paste(ward.prim.2016$reporting_unit, ward.prim.2016$county)
  ward.prim.2016$reporting_unit_orig = toupper(ward.prim.2016$reporting_unit)
  
  ward.prim.2016$reporting_unit = gsub("WARDS","WARD",ward.prim.2016$reporting_unit_orig)
  ward.prim.2016$reporting_unit = gsub("  "," ",ward.prim.2016$reporting_unit)
  ward.prim.2016$reporting_unit = gsub("&","-",ward.prim.2016$reporting_unit)
  ward.prim.2016$reporting_unit = gsub("AND","-",ward.prim.2016$reporting_unit)
  ward.prim.2016$reporting_unit = gsub(",","-",ward.prim.2016$reporting_unit)
  ward.prim.2016$reporting_unit = gsub(" - ","-",ward.prim.2016$reporting_unit)
  ward.prim.2016$reporting_unit = gsub(" -","-",ward.prim.2016$reporting_unit)
  ward.prim.2016$reporting_unit = gsub("- ","-",ward.prim.2016$reporting_unit)
  ward.prim.2016$reporting_unit = gsub(" ","-",ward.prim.2016$reporting_unit)
  ward.prim.2016$reporting_unit = gsub("WD","",ward.prim.2016$reporting_unit)
  ward.prim.2016$reporting_unit = gsub("COMBINED","",ward.prim.2016$reporting_unit)
  ward.prim.2016$reporting_unit = gsub("--","-",ward.prim.2016$reporting_unit)
  
  ward.prim.2016.look = with(ward.prim.2016, data.frame(county, municipality_name,
                                                        reporting_unit, muni_county))
  colnames(ward.prim.2016.look) = c("county","municipality","reporting_unit",
                                    "muni_county")
  
  rm(ward.prim.20162,ward.prim.20163,ward.prim.20164)
  
  ward.prim.2016.v = ward.prim.2016
  ward.prim.2016.v$county = NULL
  ward.prim.2016.v$municipality_name = NULL
  ward.prim.2016.v$total_votes = NULL
  # ward.prim.2016.v$reporting_unit = NULL
  ward.prim.2016.v$congress_district = NULL
  
  ward.prim.2016.l = gather(ward.prim.2016.v, reporting_unit)
  colnames(ward.prim.2016.l) = c("reporting_unit", "cand", "votes_rec")
  
  ward.prim.2016.l$cand.group = NA
  # ward.prim.2016.l$cand.group = NA
  ward.prim.2016.l$cand.group[grep("hillary_clinton",ward.prim.2016.l$cand)] = "clinton"
  ward.prim.2016.l$cand.group[grep("bernie_sanders",ward.prim.2016.l$cand)] = "sanders"
  ward.prim.2016.l$cand.group[is.na(ward.prim.2016.l$cand.group)] = "other"
  
  # Joining back county and municipality names
  ward.prim.2016.l = plyr::join(ward.prim.2016.l, ward.prim.2016.look, by = "reporting_unit", match = "first")
  
  ward.prim.2016.l$votes_rec = as.numeric(as.integer(ward.prim.2016.l$votes_rec))
  
  
  tot.votes.look.group = dplyr::group_by(ward.prim.2016.l,reporting_unit)
  tot.votes.look = dplyr::summarise(tot.votes.look.group,
                                    votes_tot = sum(votes_rec,na.rm=T)
  )
  
  ward.prim.2016.l = plyr::join(ward.prim.2016.l,tot.votes.look,by="reporting_unit",match="first")
  ward.prim.2016.l$votes_perc = (ward.prim.2016.l$votes_rec / ward.prim.2016.l$votes_tot) * 100
  
  ward.prim.2016.l$muni_county = paste(ward.prim.2016.l$municipality, ward.prim.2016.l$county)
  
  ward.prim.2016.l$votes_perc = (ward.prim.2016.l$votes_rec / ward.prim.2016.l$votes_tot) * 100
  
  ward.prim.2016.melt = melt(ward.prim.2016.l)
  ward.prim.2016.cast = dcast(ward.prim.2016.melt, reporting_unit + muni_county + municipality + county ~ variable + cand.group, sum,na.rm=T)
  ward.prim.2016.l = ward.prim.2016.cast
  
  ward.prim.2016.l$clinton_prim_won = with(ward.prim.2016.l,
                                         ifelse(votes_perc_clinton > votes_perc_sanders &
                                                  votes_perc_clinton > votes_perc_other,1,0))
  
  ward.prim.2016.l$votes_tot_clinton = NULL
  ward.prim.2016.l$votes_tot_sanders = NULL
  
  colnames(ward.prim.2016.l)[grep("votes_tot_other",colnames(ward.prim.2016.l))] = "votes_tot"
  
  rm(ward.prim.2016.cast,ward.prim.2016.melt,
     ward.prim.2016.mun.group ,
     ward.prim.2016, ward.prim.2016.look, ward.prim.2016.v,
     tot.votes.look,tot.votes.look.group)
  
} # Preparing basic data
{ # Spring primary voting statistics June 2016
  ward.jundata.2016 = read.xlsx2(
    "2016_spring_primary_gab190nf_20160617_xlsx_20273.xlsx",
    stringsAsFactors = F,
    header = T,
    sheetIndex = 1,
    colClasses = NA # Thanks: http://www.briandalessandro.com/blog/how-to-read-numeric-columns-with-read-xlsx2-in-r/
  )
  
  # Preparing basic data
  ward.jundata.2016 = clean_names(ward.jundata.2016)
  ward.jundata.2016$reporting_unit = toupper(ward.jundata.2016$reporting_unit)
  ward.jundata.2016$municipality = toupper(ward.jundata.2016$municipality)
  ward.jundata.2016$county = toupper(ward.jundata.2016$county)
  
  ward.jundata.2016$county = gsub(" COUNTY","", ward.jundata.2016$county)
  ward.jundata.2016$muni_county = paste(ward.jundata.2016$municipality, ward.jundata.2016$county)
  
  ward.jundata.2016$reporting_unit = toupper(ward.jundata.2016$reporting_unit)
  
  ward.jundata.2016$reporting_unit = paste(ward.jundata.2016$municipality,ward.jundata.2016$reporting_unit, ward.jundata.2016$county)
  ward.jundata.2016$reporting_unit_orig = toupper(ward.jundata.2016$reporting_unit)
  
  ward.jundata.2016$reporting_unit = gsub("WARDS","WARD",ward.jundata.2016$reporting_unit_orig)
  ward.jundata.2016$reporting_unit = gsub("  "," ",ward.jundata.2016$reporting_unit)
  ward.jundata.2016$reporting_unit = gsub("&","-",ward.jundata.2016$reporting_unit)
  ward.jundata.2016$reporting_unit = gsub("AND","-",ward.jundata.2016$reporting_unit)
  ward.jundata.2016$reporting_unit = gsub(",","-",ward.jundata.2016$reporting_unit)
  ward.jundata.2016$reporting_unit = gsub(" - ","-",ward.jundata.2016$reporting_unit)
  ward.jundata.2016$reporting_unit = gsub(" -","-",ward.jundata.2016$reporting_unit)
  ward.jundata.2016$reporting_unit = gsub("- ","-",ward.jundata.2016$reporting_unit)
  ward.jundata.2016$reporting_unit = gsub(" ","-",ward.jundata.2016$reporting_unit)
  ward.jundata.2016$reporting_unit = gsub("WD","",ward.jundata.2016$reporting_unit)
  ward.jundata.2016$reporting_unit = gsub("COMBINED","",ward.jundata.2016$reporting_unit)
  ward.jundata.2016$reporting_unit = gsub("--","-",ward.jundata.2016$reporting_unit)
  
  ward.jundata.group = dplyr::group_by(ward.jundata.2016,reporting_unit, muni_county, county, municipality)
  jundata.municounty = dplyr::summarise(ward.jundata.group,
                                        registrants = sum(registrants),
                                        late_registrants = sum(late_registrants),
                                        election_day_registrants = sum(election_day_registrants),
                                        total_ballots = sum(total_ballots),
                                        total_voters = sum(total_voters),
                                        paper_ballots = sum(paper_ballots),
                                        optical_scan_ballots = sum(optical_scan_ballots),
                                        dre = sum(dre),
                                        auto_mark = sum(auto_mark),
                                        total_election_inspectors = sum(total_election_inspectors),
                                        x16_17 = sum(x16_17),
                                        x18_25 = sum(x18_25),
                                        x26_40 = sum(x26_40),
                                        x41_60 = sum(x41_60),
                                        x61_70 = sum(x61_70),
                                        provisional_no_dl = sum(provisional_no_dl),
                                        provisional_no_por = sum(provisional_no_por),
                                        counted_id = sum(counted),
                                        rejected_id = sum(rejected),
                                        absentee_issued = sum(absentee_issued),
                                        absentee_in_person = sum(absentee_in_person),
                                        absentee_not_returned = sum(absentee_not_returned),
                                        absentee_undeliverable = sum(absentee_undeliverable),
                                        received_by_election_day = sum(received_by_election_day),
                                        counted_absent = sum(absentee_counted),
                                        rejected_absent = sum(absentee_rejected),
                                        # late_received_after_the_election = sum(late_received_after_the_election),
                                        fwab_received = sum(fwab_received),
                                        fwab_counted = sum(fwab_counted),
                                        fwab_rejected = sum(fwab_rejected),
                                        fwab_late = sum(fwab_late),
                                        military_issued = sum(military_issued),
                                        military_unreturned = sum(military_unreturned),
                                        military_undeliverable = sum(military_undeliverable),
                                        military_by_electionday = sum(military_by_electionday),
                                        military_counted = sum(military_counted),
                                        military_rejected = sum(military_rejected),
                                        military_late = sum(military_late)
  )
  
  # character fields not included:
  # ballots_counted_at
  # split_shifts
  # difficulty obtaining
  # polling_place_name
  # shared
  
  ward.jundata.group = NULL
  
  write.csv(jundata.municounty,"jun.2016.prim.repunit.data.csv")
} # Spring primary voting statistics June 2016
{ # Matching voting statistics with the election statistics
  
  # Maybe need to change the name of these muni_counties to make sure I have a full match
  setdiff(unique(jundata.municounty$muni_county), unique(ward.prim.2016.l$muni_county))
  setdiff(unique(ward.prim.2016.l$muni_county), unique(jundata.municounty$muni_county))
  
  ward.prim.2016.l$muni_county[grep("VILLAGE OF MAINE MARATHON",ward.prim.2016.l$muni_county)] = "TOWN OF MAINE MARATHON"
  ward.prim.2016.l$municipality[grep("TOWN OF MAINE MARATHON",ward.prim.2016.l$muni_county)] = "TOWN OF MAINE"
  
  differences = setdiff(unique(ward.prim.2016.l$muni_county),unique(jundata.municounty$muni_county))
  jundata.diffs = subset(ward.prim.2016.l, muni_county %in% differences)
  
  # Missing:
  # TOWN OF HAYWARD (all put in city of Hayward), Albion Trempeleau (no data),
  # Caledonia Trempeleau (no data), Hendren Clark (no data), Strum Trempeleau (no data)
  
  jundata.join = with(jundata.municounty, data.frame(reporting_unit,muni_county))
  jundata.join$total_ballots = ifelse(jundata.municounty$total_ballots == 0,
                                      jundata.municounty$total_voters,
                                      jundata.municounty$total_ballots)
  jundata.join$tch_prop = with(jundata.municounty, dre / total_ballots)
  jundata.join$tch_prop = ifelse(jundata.join$tch_prop>1, 1, jundata.join$tch_prop)
  
  jundata.join$turnout_perc = with(jundata.municounty, total_ballots / registrants)
  jundata.join$turnout_perc[jundata.join$turnout_perc == Inf] = NA
  jundata.join$sameday_reg_prop = with(jundata.municounty, election_day_registrants / registrants)
  jundata.join$sameday_reg_prop[jundata.join$sameday_reg_prop == Inf] = NA
  jundata.join$counted_absent_prop = with(jundata.municounty,
                                          counted_absent / total_ballots)
  jundata.join$counted_absent_prop[jundata.join$counted_absent_prop == Inf] = NA
  jundata.join$rejected_absent_prop = with(jundata.municounty,
                                           rejected_absent / total_ballots)
  jundata.join$registrants = with(jundata.municounty, registrants)
  jundata.join$late_registrants = with(jundata.municounty, late_registrants / total_ballots)
  
  jundata.join[is.na(jundata.join)] = NA
  
  ward.prim.2016.l2 = plyr::join(ward.prim.2016.l, jundata.join, by = "reporting_unit")
  ward.prim.2016.l = ward.prim.2016.l2
  
  ward.prim.2016.l[is.na(ward.prim.2016.l)] = NA
  rm(ward.prim.2016.l2)
  
} # Matching voting statistics with the election statistics
{
  # Adding demographic data to the ward data frame
  demographics.caps = demographics
  demographics.caps$county = toupper(demographics.caps$county)
  
  # Demographics from David Griffen and co
  ward.prim.2016.dem = plyr::join(ward.prim.2016.l, demographics.caps, by = "county", match = "first")
  
  # Also joining the data from the Election snapshot
  ward.prim.2016.dem = plyr::join(ward.prim.2016.dem, elec.snapshot.dem.wi, by = "county", match = "first")
} # Pasting demographics onto the ward dataframe
{
  setwd("C:/Users/s_cas/Dropbox/Perso/2016 voting election county results/Wisconsin/All prep together/All processed files")
  off_vote_machines = read.xlsx2("voting_equipment_by_municipality_09_2016_xlsx_78114.xlsx1207162619.xlsx",
                                 sheetIndex = 1,
                                 stringsAsFactors=F)
  
  colnames(off_vote_machines)[3] = "os_model_off"
  colnames(off_vote_machines)[4] = "tch_bmd_model_off"
  
  off_vote_machines$tch_use_off = NA
  off_vote_machines$tch_use_off = ifelse(off_vote_machines$tch_bmd_model_off == "Dominion (Premier)-Accuvote TSX" |
                                           off_vote_machines$tch_bmd_model_off == "Dominion (Sequoia)/Command Central-Edge" |
                                           off_vote_machines$tch_bmd_model_off == "Dominion (Sequoia)/Command Central-Edge; ES&S Automark" |
                                           off_vote_machines$tch_bmd_model_off == "ES&S iVotronic",1, 0
  )
  off_vote_machines$bmd_use_off = NA
  off_vote_machines$bmd_use_off = ifelse(off_vote_machines$tch_bmd_model_off == "Dominion ImageCast Evolution" |
                                           off_vote_machines$tch_bmd_model_off == "ES&S Automark" |
                                           off_vote_machines$tch_bmd_model_off == "ES&S ExpressVote" |
                                           off_vote_machines$tch_bmd_model_off == "Populex-Populex 2.3" |
                                           off_vote_machines$tch_bmd_model_off == "Vote Pad-Vote Pad (non-electronic)"
                                         ,1, 0
  )
  off_vote_machines$os_use_off = NA
  off_vote_machines$os_use_off = ifelse(off_vote_machines$os_model_off == "None ",
                                        0, 1
  )
  off_vote_machines$os_model_off = ifelse(off_vote_machines$os_model_off == "None ",
                                          NA, off_vote_machines$os_model_off
  )
  
  off_vote_machines$tch_model_off = NA
  off_vote_machines$tch_model_off = ifelse(off_vote_machines$tch_use_off == 1,
                                           off_vote_machines$tch_bmd_model_off, NA  )
  
  off_vote_machines$bmd_model_off = NA
  off_vote_machines$bmd_model_off = ifelse(off_vote_machines$bmd_use_off == 1,
                                           off_vote_machines$tch_bmd_model_off, NA  )
  
  off_vote_machines$tch_use_off_true = NA
  off_vote_machines$tch_use_off_true = ifelse(off_vote_machines$os_use_off == 0,
                                              off_vote_machines$tch_use_off, 0  )
  
  code_2 = gsub( " COUNTY.*$", "", off_vote_machines$County)
  code_3 = gsub( " -.*$", "", off_vote_machines$Municipality)
  off_vote_machines$muni_county = paste(code_3,code_2, sep = " ")
  #
  # unique(grepl("550101201",ward.prim.2016.dem$MCD_FIPS))
  
  # setdiff(off_vote_machines$muni_county, ward.prim.2016.dem$muni_county)
  # off_vote_machines$muni_county[grep("TOWN OF GEORGETOWN  POLK", off_vote_machines$muni_county)] = "TOWN OF GEORGETOWN POLK"
  # off_vote_machines$muni_county[grep("TOWN OF WINDSOR DANE", off_vote_machines$muni_county)] = "VILLAGE OF WINDSOR DANE"
  # off_vote_machines$muni_county[grep("TOWN OF MAINE MARATHON", off_vote_machines$muni_county)] = "VILLAGE OF MAINE MARATHON"
  
  # setdiff(ward.prim.2016.dem$muni_county, off_vote_machines$muni_county) # Difference of 47
  # length(off_vote_machines$muni_county) # 1854
  # length(ward.prim.2016.dem$muni_county) # 1900 Difference of 46. Plus the errant space = 47. Therefore, these places are simply not included in the official list?
  
  off_vote_machines_cut = off_vote_machines
  off_vote_machines$County = NULL
  off_vote_machines$Municipality = NULL
  
  ward.prim.2016.dem = plyr::join(ward.prim.2016.dem, off_vote_machines, by = "muni_county")
  
  ward.prim.2016.dem$tch_model_off_full = NA
  ward.prim.2016.dem$tch_model_off_full = with(ward.prim.2016.dem, ifelse(tch_model_off == "Dominion (Premier)-Accuvote TSX",
                                                                          "Dominion (Premier)-Accuvote TSX", ward.prim.2016.dem$tch_model_off_full))
  ward.prim.2016.dem$tch_model_off_full = with(ward.prim.2016.dem, ifelse(tch_model_off == "Dominion (Sequoia)/Command Central-Edge",
                                                                          "Dominion (Sequoia)/Command Central-Edge",ward.prim.2016.dem$tch_model_off_full))
  ward.prim.2016.dem$tch_model_off_full = with(ward.prim.2016.dem, ifelse(tch_model_off == "Dominion (Sequoia)/Command Central-Edge; ES&S Automark",
                                                                          "Dominion (Sequoia)/Command Central-Edge",ward.prim.2016.dem$tch_model_off_full))
  ward.prim.2016.dem$tch_model_off_full = with(ward.prim.2016.dem, ifelse(tch_model_off == "ES&S iVotronic",
                                                                          "ES&S iVotronic",ward.prim.2016.dem$tch_model_off_full))
  ward.prim.2016.dem$tch_model_off_full[is.na(ward.prim.2016.dem$tch_model_off_full)] = "aNone"
  ward.prim.2016.dem$tch_model_off_full = with(ward.prim.2016.dem, ifelse(is.na(tch_use_off),
                                                                          NA,ward.prim.2016.dem$tch_model_off_full))
  
  colnames(ward.prim.2016.dem)[grep("tch.use",colnames(ward.prim.2016.dem),fixed = T)] = "tch_use_verivote"
  colnames(ward.prim.2016.dem)[grep("os.use",colnames(ward.prim.2016.dem),fixed = T)] = "os_use_verivote"
  colnames(ward.prim.2016.dem)[grep("bmd.use",colnames(ward.prim.2016.dem),fixed = T)] = "bmd_use_verivote"
  colnames(ward.prim.2016.dem)[grep("X._collegedegreepersons25._2013",colnames(ward.prim.2016.dem))] = "coll_deg_plus25_2013"
} # Putting in voting machines
{ # Making model df
  model.prim.repunit.df = ward.prim.2016.dem
  # model.prim.df = subset(model.prim.df, votes_tot > 19)
  
  # Following Nate Cohn's method: https://twitter.com/Nate_Cohn/status/801226924156719104/photo/1?ref_src=twsrc%5Etfw
  # I found it's better to use the non-logged version of pop.sq.mile.2010
  model.prim.repunit.df$log.pop.sq.mile.2010 = log(model.prim.repunit.df$pop_sq_mile_2010)
  
  # ratio college and income are both highly correlated so maybe don't use median income
  # population density is highly correlated with black population, so I removed population density
  # I was not able to effectively model Menonminee county with my data (it has a very high Amerindian population). So,
  # I replaced the correlated and generally not significant 'white_pct' variable with 'white_nonwhite*
  
  ##
  model.prim.repunit.df2 = with(model.prim.repunit.df, data.frame(reporting_unit,county, muni_county,
                                                                  votes_perc_clinton, votes_perc_sanders, votes_perc_other,clinton_prim_won,turnout_perc,
                                                                  tch_prop,counted_absent_prop,rejected_absent_prop,
                                                                  sameday_reg_prop, total_ballots,registrants,late_registrants,
                                                                  county_paper_or_paperplusmachine,
                                                                  pct_white, ratio_nocollege_college, log.pop.sq.mile.2010,
                                                                  pop_sq_mile_2010,tch_model_off_full, os_model_off, bmd_model_off,
                                                                  medianhouseholdincome_2009.2013, pct_graduate, coll_deg_plus25_2013,
                                                                  votes_tot, inc_2015,inc_2015, unemp_rate,
                                                                  pct_male, pct_old, pct_latino, pct_white,
                                                                  pct_black,pct_hs))
  
  rownames(model.prim.repunit.df2) = model.prim.repunit.df2$reporting_unit
  
  model.prim.repunit.df = model.prim.repunit.df2
} # Making model df
{ # Getting municipality densities
  require(rgdal); require(sp)
  # 
  # setwd("C:/Users/s_cas/Dropbox/Perso/2016 voting election county results/Wisconsin/All prep together/All processed files/")
  # getwd()
  # 
  # wisc.spat.o <-
  #   readOGR(
  #     dsn = "Prepped files/wisc.spat.orig.geojson",
  #     layer = "OGRGeoJSON",
  #     disambiguateFIDs = T,
  #     stringsAsFactors = F
  #   )
  # 
  # # First, getting population densities. Transforming to utm, zone 16 for Wisconsin
  # 
  # wisc.spat.o.trans = spTransform(
  #   wisc.spat.o,
  #   CRS(
  #     "+proj=utm +zone=16 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
  #   )
  # )
  # 
  # ur.area<-sapply(slot(wisc.spat.o.trans, "polygons"), function(x) sapply(slot(x, "Polygons"), slot, "area"))
  # ur.area.s = lapply(ur.area,sum)
  # 
  # wisc.spat.o@data$muni.area = unlist(ur.area.s) / 1000000
  # wisc.spat.o@data$muni.dense = as.numeric(wisc.spat.o@data$pop) / wisc.spat.o@data$muni.area
  # 
  # # head(wisc.spat.o@data$voters.tot2016)
  # # head(wisc.spat.o@data$muni.area)
  # 
  # density.join = with(wisc.spat.o@data, data.frame(muni_county,muni.dense))
  # 
  # model.prim.repunit.df = plyr::join(model.prim.repunit.df, density.join, by = "muni_county")
  # 
} # Municipality densities
{ ### Creating a carto-presentable map
  require(rgdal); require(sp); require(geojson)
  
  # wisc.spat.o <-
  #   readOGR(
  #     dsn = "Prepped files/wisc.spat.orig.geojson",
  #     layer = "OGRGeoJSON",
  #     disambiguateFIDs = T,
  #     stringsAsFactors = F
  #   )
  
  # wisc.spat.o.cut = wisc.spat.o
  # wisc.spat.o.cut@data[7:length(wisc.spat.o.cut@data)] = NULL
  # wisc.spat.o.cut@data$MCD_NAME = toupper(wisc.spat.o.cut@data$MCD_NAME)
  # 
  # wisc.join = wisc.spat.o.cut
  # 
  # wisc.join@data = plyr::join(wisc.join@data, model.prim.repunit.df, by = "muni_county", match = "first")
  # colnames(wisc.join@data) = make.unique(colnames(wisc.join@data))
  # wisc.join@data$county = NULL
  # wisc.join@data$MCD_FIPS.1 = NULL
  # 
  # wisc.join@data[wisc.join@data == Inf] = NA
  # wisc.join@data[wisc.join@data == -Inf] = NA
  # 
  # wisc.join@data[is.na(wisc.join@data)] = NA
  # wisc.join.df = as.data.frame(wisc.join)
  # 
  # # writeOGR(
  # #   wisc.join,
  # #   "Prepped files/wisc.present.repprim.geojson",
  # #   layer = "wisc.join",
  # #   driver = "GeoJSON"
  # )
} # Creating a carto-presentable map

write.csv(model.prim.df,"model_demprim.csv")

write.csv(model.prim.repunit.df,"model_demprim_repunit.csv")

# Governor 2014 data (I'm still working here, preparing basic data) ------------------------------------------------
setwd("C:/Users/s_cas/Dropbox/Perso/2016 voting election county results/Wisconsin/All prep together/All processed files")
getwd()

gov.2014 = read.csv(
  "2014/11.4.2014 Election Results - all offices w x w report gov.csv",
  skip = 10,
  strip.white = T,
  stringsAsFactors = F,
  header = T
)
# Replace blanks with NA
gov.2014 = data.frame(apply(gov.2014, 2, function(x) gsub("^$|^ $", NA, x)))
gov.2014 = clean_names(gov.2014)

{ # Preparing basic data
names(gov.2014)[1:3] <- c("county","reporting_unit","votes.tot")
  
  gov.2014$county[gov.2014$county == ""] <- NA
  
  require(zoo)
  gov.2014$county <- na.locf(gov.2014$county)
  
  gov.2014$reporting_unit = ifelse(gov.2014$reporting_unit == "","Wards 1",gov.2014$reporting_unit)
  gov.2014$reporting_unit = toupper(gov.2014$reporting_unit)
  gov.2014$county = toupper(gov.2014$county)
  
  gov.2014 <- subset(gov.2014, reporting_unit != "COUNTY TOTALS:")
  gov.2014 <- subset(gov.2014, county != "OFFICE TOTALS:")
  
  # Get the municipality names from the reporting unit names    
  odd_indexes<-seq(1,length(gov.2014$reporting_unit)*2,2)
  
  gov.2014$reporting_unit <- gsub("WARD WD", "WARD", gov.2014$reporting_unit)
  gov.2014$reporting_unit <- gsub("\\bWD\\b", "WARD", gov.2014$reporting_unit)
  gov.2014$reporting_unit <- gsub("\\bWDS\\b", "WARDS", gov.2014$reporting_unit)
  gov.2014$reporting_unit <- gsub("\\bWARD\\b", "WARD", gov.2014$reporting_unit)
  
  # https://stackoverflow.com/questions/26896971/add-space-between-two-letters-in-a-string-in-r
  
  gov.2014$reporting_unit <- gsub("WARD([0-9])", "WARD \\1", gov.2014$reporting_unit)
  gov.2014$reporting_unit <- gsub("WARDS([0-9])", "WARDS \\1", gov.2014$reporting_unit)
  
  require(stringr)
  
  gov.2014$municipality_name <- unlist(str_split(str = gov.2014$reporting_unit, " [0-9]", n = 2))[odd_indexes]
  gov.2014$municipality_name <- gsub(" WARDS","", gov.2014$municipality_name)
  gov.2014$municipality_name <- gsub(" WARD","", gov.2014$municipality_name)
  gov.2014$municipality_name <- na.locf(gov.2014$municipality_name)
  
  
  # gov.2014$municipality_name = with(gov.2014, paste(municipality_type, "of", municipality,sep=" "))
  # gov.2014$reporting_unit = with(gov.2014, paste(municipality_name, reporting_unit))
  # colnames(gov.2014)[1:3] = c("county","municipality_name","reporting_unit")
  # gov.2014$municipality_name = toupper(gov.2014$municipality_name)
  
  
  # The following is fixing a problem I noticed later when trying to match with the 2016 voter df
  gov.2014$municipality_name[grep("TOWN OF WINDSOR", gov.2014$municipality_name)] = "VILLAGE OF WINDSOR"
  gov.2014$reporting_unit = gsub("TOWN OF WINDSOR","VILLAGE OF WINDSOR", gov.2014$reporting_unit)
  
  # The town in Maine became a village in 2015, and is now in marathon county
  # gov.2014$county[grep("TOWN OF MAINE", gov.2014$municipality_name)] = "MARATHON"
  gov.2014$municipality_name[grepl("TOWN OF MAINE", gov.2014$municipality_name) &
                              grepl("MARATHON", gov.2014$county)
                            ] = "VILLAGE OF MAINE"
  # gov.2014$reporting_unit = gsub("TOWN OF MAINE","VILLAGE OF MAINE", gov.2014$reporting_unit)
  
  # the town of Menasha was retained only for wards (8-10, 11-13). The rest (1-7) became the village of
  # Fox Crossing, Winnebago
  gov.2014$municipality_name[grepl("TOWN OF MENASHA WARDS 1, 2, 4, 7", gov.2014$reporting_unit)
                            ] = "VILLAGE OF FOX CROSSING"
  gov.2014$municipality_name[grepl("TOWN OF MENASHA WARDS 3, 5, 6", gov.2014$reporting_unit)
                            ] = "VILLAGE OF FOX CROSSING"
  gov.2014$reporting_unit[grepl("TOWN OF MENASHA WARDS 1, 2, 4, 7", gov.2014$reporting_unit)
                         ] = "VILLAGE OF FOX CROSSING WARDS 1-2,4,7"
  gov.2014$reporting_unit[grepl("TOWN OF MENASHA WARDS 3, 5, 6", gov.2014$reporting_unit)
                         ] = "VILLAGE OF FOX CROSSING WARDS 3,5-6"
  
  # gov.2014.look = with(gov.2014, data.frame(county, municipality_name,
  #                                         reporting_unit, muni_county))
  # colnames(gov.2014.look) = c("county","municipality","reporting_unit",
  #                            "muni_county")
  # 
  # require(zoo)
  # gov.2014$congress_district = na.locf(gov.2014$congress_district)
  gov.2014$county = na.locf(gov.2014$county)
  trim.trailing <- function (x) sub("\\s+$", "", x)
  gov.2014$county = trim.trailing(gov.2014$county)
  
  gov.2014$municipality_name = toupper(gov.2014$municipality_name)
  gov.2014$county = toupper(gov.2014$county)
  
  gov.2014$muni_county = paste(gov.2014$municipality_name, gov.2014$county)
  
  ###
  gov.2014$reporting_unit = toupper(gov.2014$reporting_unit)
  
  gov.2014$reporting_unit = paste(gov.2014$reporting_unit, gov.2014$county)
  gov.2014$reporting_unit_orig = toupper(gov.2014$reporting_unit)
  
  gov.2014$reporting_unit = gsub("WARDS","WARD",gov.2014$reporting_unit_orig)
  gov.2014$reporting_unit = gsub("  "," ",gov.2014$reporting_unit)
  gov.2014$reporting_unit = gsub("&","-",gov.2014$reporting_unit)
  gov.2014$reporting_unit = gsub("AND","-",gov.2014$reporting_unit)
  gov.2014$reporting_unit = gsub(",","-",gov.2014$reporting_unit)
  gov.2014$reporting_unit = gsub(" - ","-",gov.2014$reporting_unit)
  gov.2014$reporting_unit = gsub(" -","-",gov.2014$reporting_unit)
  gov.2014$reporting_unit = gsub("- ","-",gov.2014$reporting_unit)
  gov.2014$reporting_unit = gsub(" ","-",gov.2014$reporting_unit)
  gov.2014$reporting_unit = gsub("WD","",gov.2014$reporting_unit)
  gov.2014$reporting_unit = gsub("COMBINED","",gov.2014$reporting_unit)
  gov.2014$reporting_unit = gsub("--","-",gov.2014$reporting_unit)
  
  gov.2014.look = with(gov.2014, data.frame(county, municipality_name,
                                                        reporting_unit, muni_county))
  colnames(gov.2014.look) = c("county","municipality","reporting_unit",
                                    "muni_county")
  
  rm(gov.20142,gov.20143,gov.20144)
  
  gov.2014.v = gov.2014
  # gov.2014.v$county = NULL
  gov.2014.v$county = NULL
  gov.2014.v$municipality_name = NULL
  gov.2014.v$municipality_type = NULL
  gov.2014.v$municipalityno = NULL
  gov.2014.v$municipality = NULL
  # gov.2014.v$reporting_unit = NULL
  gov.2014.v$order = NULL
  gov.2014.v$hindi = NULL
  # gov.2014.v$assemblydistrict = NULL
  
  gov.2014.l <- gov.2014.v
  gov.2014.l$muni_county = NULL
  gov.2014.l$reporting_unit <- NULL
  gov.2014.l$votes.tot <- NULL
  gov.2014.l$x_3 <- NULL
  gov.2014.l = gather(gov.2014.l, cand, votes.rec)
  gov.2014.l$reporting_unit <- rep(gov.2014.v$reporting_unit)
  # colnames(gov.2014.l) = c("reporting_unit", "cand", "votes.rec")
  
  names(gov.2014.v)
  
  gov.2014.l$cand.group = NA
  gov.2014.l$cand.group[grep("mary_burke",gov.2014.l$cand)] = "democrat"
  gov.2014.l$cand.group[grep("walker",gov.2014.l$cand)] = "republican"
  gov.2014.l$cand.group[grep("fehr",gov.2014.l$cand)] = "other"
  gov.2014.l$cand.group[grep("robert_burke",gov.2014.l$cand)] = "other"
  gov.2014.l$cand.group[grep("mary_jo",gov.2014.l$cand)] = "other"
  gov.2014.l$cand.group[grep("steve_r",gov.2014.l$cand)] = "other"
  gov.2014.l$cand.group[grep("jumoka",gov.2014.l$cand)] = "other"
  gov.2014.l$cand.group[grep("brett_d_",gov.2014.l$cand)] = "other"
  gov.2014.l$cand.group[grep("jessica",gov.2014.l$cand)] = "other"
  gov.2014.l$cand.group[grep("susan_p",gov.2014.l$cand)] = "other"
  gov.2014.l$cand.group[grep("scatter",gov.2014.l$cand)] = "other"
  
  # Joining back county and municipality names
  gov.2014.l = plyr::join(gov.2014.l, gov.2014.look, by = "reporting_unit", match = "first")
  
  gov.2014.l$votes_rec = as.numeric(as.integer(gov.2014.l$votes_rec))
  
  
  tot.votes.look.group = dplyr::group_by(gov.2014.l,reporting_unit)
  tot.votes.look = dplyr::summarise(tot.votes.look.group,
                                    votes_tot = sum(votes_rec,na.rm=T)
  )
  
  gov.2014.l = plyr::join(gov.2014.l,tot.votes.look,by="reporting_unit",match="first")
  gov.2014.l$votes_perc = (gov.2014.l$votes_rec / gov.2014.l$votes_tot) * 100
  
  gov.2014.l$muni_county = paste(gov.2014.l$municipality, gov.2014.l$county)
  
  # gov.2014.l$votes_perc = (gov.2014.l$votes_rec / gov.2014.l$votes_tot) * 100
  
  gov.2014.melt = melt(gov.2014.l)
  gov.2014.cast = dcast(gov.2014.melt, reporting_unit + muni_county + municipality + county ~ variable + cand.group, sum,na.rm=T)
  gov.2014.l = gov.2014.cast
  
  # gov.2014.l$trump_prim_won = with(gov.2014.l,
  #                                        ifelse(votes_perc_trump > votes_perc_kasich &
  #                                                 votes_perc_trump > votes_perc_cruz &
  #                                                 votes_perc_trump > votes_perc_other,1,0))
  # 
  # gov.2014.l$votes_tot_cruz = NULL
  # gov.2014.l$votes_tot_trump = NULL
  # gov.2014.l$votes_tot_kasich = NULL
  
  colnames(gov.2014.l)[grep("votes_tot_other",colnames(gov.2014.l))] = "votes_tot"
  
  rm(gov.2014.cast,gov.2014.melt,
     gov.2014.mun.group ,
     gov.2014, gov.2014.look, gov.2014.v,
     tot.votes.look,tot.votes.look.group)
  
} # Preparing basic data
{ # Spring primary voting statistics June 2016
  gabdata.2014.gov = read.xlsx2(
    "2014/2014_general_election_gab190f_20150226_xlsx_21131.xlsx",
    stringsAsFactors = F,
    header = T,
    sheetIndex = 1,
    colClasses = NA # Thanks: http://www.briandalessandro.com/blog/how-to-read-numeric-columns-with-read-xlsx2-in-r/
  )
  
  # Preparing basic data
  gabdata.2014.gov = clean_names(gabdata.2014.gov)
  gabdata.2014.gov$reporting_unit = toupper(gabdata.2014.gov$reporting_unit)
  gabdata.2014.gov$municipality = toupper(gabdata.2014.gov$municipality)
  gabdata.2014.gov$county = toupper(gabdata.2014.gov$county)
  
  gabdata.2014.gov$county = gsub(" COUNTY","", gabdata.2014.gov$county)
  gabdata.2014.gov$muni_county = paste(gabdata.2014.gov$municipality, gabdata.2014.gov$county)
  
  gabdata.2014.gov$reporting_unit = toupper(gabdata.2014.gov$reporting_unit)
  
  gabdata.2014.gov$reporting_unit = paste(gabdata.2014.gov$municipality,gabdata.2014.gov$reporting_unit, gabdata.2014.gov$county)
  gabdata.2014.gov$reporting_unit_orig = toupper(gabdata.2014.gov$reporting_unit)
  
  gabdata.2014.gov$reporting_unit = gsub("WARDS","WARD",gabdata.2014.gov$reporting_unit_orig)
  gabdata.2014.gov$reporting_unit = gsub("  "," ",gabdata.2014.gov$reporting_unit)
  gabdata.2014.gov$reporting_unit = gsub("&","-",gabdata.2014.gov$reporting_unit)
  gabdata.2014.gov$reporting_unit = gsub("AND","-",gabdata.2014.gov$reporting_unit)
  gabdata.2014.gov$reporting_unit = gsub(",","-",gabdata.2014.gov$reporting_unit)
  gabdata.2014.gov$reporting_unit = gsub(" - ","-",gabdata.2014.gov$reporting_unit)
  gabdata.2014.gov$reporting_unit = gsub(" -","-",gabdata.2014.gov$reporting_unit)
  gabdata.2014.gov$reporting_unit = gsub("- ","-",gabdata.2014.gov$reporting_unit)
  gabdata.2014.gov$reporting_unit = gsub(" ","-",gabdata.2014.gov$reporting_unit)
  gabdata.2014.gov$reporting_unit = gsub("WD","",gabdata.2014.gov$reporting_unit)
  gabdata.2014.gov$reporting_unit = gsub("COMBINED","",gabdata.2014.gov$reporting_unit)
  gabdata.2014.gov$reporting_unit = gsub("--","-",gabdata.2014.gov$reporting_unit)
  
  ward.jundata.group = dplyr::group_by(gabdata.2014.gov,reporting_unit, muni_county, county, municipality)
  jundata.municounty = dplyr::summarise(ward.jundata.group,
                                        registrants = sum(registrants),
                                        late_registrants = sum(late_registrants),
                                        election_day_registrants = sum(election_day_registrants),
                                        total_ballots = sum(total_ballots),
                                        total_voters = sum(total_voters),
                                        paper_ballots = sum(paper_ballots),
                                        optical_scan_ballots = sum(optical_scan_ballots),
                                        dre = sum(dre),
                                        auto_mark = sum(auto_mark),
                                        total_election_inspectors = sum(total_election_inspectors),
                                        x16_17 = sum(x16_17),
                                        x18_25 = sum(x18_25),
                                        x26_40 = sum(x26_40),
                                        x41_60 = sum(x41_60),
                                        x61_70 = sum(x61_70),
                                        provisional_no_dl = sum(provisional_no_dl),
                                        provisional_no_por = sum(provisional_no_por),
                                        counted_id = sum(counted),
                                        rejected_id = sum(rejected),
                                        absentee_issued = sum(absentee_issued),
                                        absentee_in_person = sum(absentee_in_person),
                                        absentee_not_returned = sum(absentee_not_returned),
                                        absentee_undeliverable = sum(absentee_undeliverable),
                                        received_by_election_day = sum(received_by_election_day),
                                        counted_absent = sum(absentee_counted),
                                        rejected_absent = sum(absentee_rejected),
                                        # late_received_after_the_election = sum(late_received_after_the_election),
                                        fwab_received = sum(fwab_received),
                                        fwab_counted = sum(fwab_counted),
                                        fwab_rejected = sum(fwab_rejected),
                                        fwab_late = sum(fwab_late),
                                        military_issued = sum(military_issued),
                                        military_unreturned = sum(military_unreturned),
                                        military_undeliverable = sum(military_undeliverable),
                                        military_by_electionday = sum(military_by_electionday),
                                        military_counted = sum(military_counted),
                                        military_rejected = sum(military_rejected),
                                        military_late = sum(military_late)
  )
  
  # character fields not included:
  # ballots_counted_at
  # split_shifts
  # difficulty obtaining
  # polling_place_name
  # shared
  
  ward.jundata.group = NULL
  
  write.csv(jundata.municounty,"jun.2016.prim.repunit.data.csv")
} # Governor GAB voting statistics
{ # Matching voting statistics with the election statistics
  
  # Maybe need to change the name of these muni_counties to make sure I have a full match
  setdiff(unique(jundata.municounty$muni_county), unique(gov.2014.l$muni_county))
  setdiff(unique(gov.2014.l$muni_county), unique(jundata.municounty$muni_county))
  
  gov.2014.l$muni_county[grep("VILLAGE OF MAINE MARATHON",gov.2014.l$muni_county)] = "TOWN OF MAINE MARATHON"
  gov.2014.l$municipality[grep("TOWN OF MAINE MARATHON",gov.2014.l$muni_county)] = "TOWN OF MAINE"
  
  differences = setdiff(unique(gov.2014.l$muni_county),unique(jundata.municounty$muni_county))
  jundata.diffs = subset(gov.2014.l, muni_county %in% differences)
  
  # Missing:
  # TOWN OF HAYWARD (all put in city of Hayward), Albion Trempeleau (no data),
  # Caledonia Trempeleau (no data), Hendren Clark (no data), Strum Trempeleau (no data)
  
  jundata.join = with(jundata.municounty, data.frame(reporting_unit,muni_county))
  jundata.join$total_ballots = ifelse(jundata.municounty$total_ballots == 0,
                                      jundata.municounty$total_voters,
                                      jundata.municounty$total_ballots)
  jundata.join$tch_prop = with(jundata.municounty, dre / total_ballots)
  jundata.join$tch_prop = ifelse(jundata.join$tch_prop>1, 1, jundata.join$tch_prop)
  
  jundata.join$turnout_perc = with(jundata.municounty, total_ballots / registrants)
  jundata.join$turnout_perc[jundata.join$turnout_perc == Inf] = NA
  jundata.join$sameday_reg_prop = with(jundata.municounty, election_day_registrants / registrants)
  jundata.join$sameday_reg_prop[jundata.join$sameday_reg_prop == Inf] = NA
  jundata.join$counted_absent_prop = with(jundata.municounty,
                                          counted_absent / total_ballots)
  jundata.join$counted_absent_prop[jundata.join$counted_absent_prop == Inf] = NA
  jundata.join$rejected_absent_prop = with(jundata.municounty,
                                           rejected_absent / total_ballots)
  jundata.join$registrants = with(jundata.municounty, registrants)
  jundata.join$late_registrants = with(jundata.municounty, late_registrants / total_ballots)
  
  jundata.join[is.na(jundata.join)] = NA
  
  gov.2014.l2 = plyr::join(gov.2014.l, jundata.join, by = "reporting_unit")
  gov.2014.l = gov.2014.l2
  
  gov.2014.l[is.na(gov.2014.l)] = NA
  rm(gov.2014.l2)
  
} # Matching voting statistics with demographics and the election statistics
{
  # Adding demographic data to the ward data frame
  demographics.caps = demographics
  demographics.caps$county = toupper(demographics.caps$county)
  
  # Demographics from David Griffen and co
  gov.2014.dem = plyr::join(gov.2014.l, demographics.caps, by = "county", match = "first")
  
  # Also joining the data from the Election snapshot
  gov.2014.dem = plyr::join(gov.2014.dem, elec.snapshot.dem.wi, by = "county", match = "first")
} # Pasting demographics onto the ward dataframe
{
  setwd("C:/Users/s_cas/Dropbox/Perso/2016 voting election county results/Wisconsin/All prep together/All processed files")
  off_vote_machines = read.xlsx2("voting_equipment_by_municipality_09_2016_xlsx_78114.xlsx1207162619.xlsx",
                                 sheetIndex = 1,
                                 stringsAsFactors=F)
  
  colnames(off_vote_machines)[3] = "os_model_off"
  colnames(off_vote_machines)[4] = "tch_bmd_model_off"
  
  off_vote_machines$tch_use_off = NA
  off_vote_machines$tch_use_off = ifelse(off_vote_machines$tch_bmd_model_off == "Dominion (Premier)-Accuvote TSX" |
                                           off_vote_machines$tch_bmd_model_off == "Dominion (Sequoia)/Command Central-Edge" |
                                           off_vote_machines$tch_bmd_model_off == "Dominion (Sequoia)/Command Central-Edge; ES&S Automark" |
                                           off_vote_machines$tch_bmd_model_off == "ES&S iVotronic",1, 0
  )
  off_vote_machines$bmd_use_off = NA
  off_vote_machines$bmd_use_off = ifelse(off_vote_machines$tch_bmd_model_off == "Dominion ImageCast Evolution" |
                                           off_vote_machines$tch_bmd_model_off == "ES&S Automark" |
                                           off_vote_machines$tch_bmd_model_off == "ES&S ExpressVote" |
                                           off_vote_machines$tch_bmd_model_off == "Populex-Populex 2.3" |
                                           off_vote_machines$tch_bmd_model_off == "Vote Pad-Vote Pad (non-electronic)"
                                         ,1, 0
  )
  off_vote_machines$os_use_off = NA
  off_vote_machines$os_use_off = ifelse(off_vote_machines$os_model_off == "None ",
                                        0, 1
  )
  off_vote_machines$os_model_off = ifelse(off_vote_machines$os_model_off == "None ",
                                          NA, off_vote_machines$os_model_off
  )
  
  off_vote_machines$tch_model_off = NA
  off_vote_machines$tch_model_off = ifelse(off_vote_machines$tch_use_off == 1,
                                           off_vote_machines$tch_bmd_model_off, NA  )
  
  off_vote_machines$bmd_model_off = NA
  off_vote_machines$bmd_model_off = ifelse(off_vote_machines$bmd_use_off == 1,
                                           off_vote_machines$tch_bmd_model_off, NA  )
  
  off_vote_machines$tch_use_off_true = NA
  off_vote_machines$tch_use_off_true = ifelse(off_vote_machines$os_use_off == 0,
                                              off_vote_machines$tch_use_off, 0  )
  
  code_2 = gsub( " COUNTY.*$", "", off_vote_machines$County)
  code_3 = gsub( " -.*$", "", off_vote_machines$Municipality)
  off_vote_machines$muni_county = paste(code_3,code_2, sep = " ")
  #
  # unique(grepl("550101201",gov.2014.dem$MCD_FIPS))
  
  # setdiff(off_vote_machines$muni_county, gov.2014.dem$muni_county)
  # off_vote_machines$muni_county[grep("TOWN OF GEORGETOWN  POLK", off_vote_machines$muni_county)] = "TOWN OF GEORGETOWN POLK"
  # off_vote_machines$muni_county[grep("TOWN OF WINDSOR DANE", off_vote_machines$muni_county)] = "VILLAGE OF WINDSOR DANE"
  # off_vote_machines$muni_county[grep("TOWN OF MAINE MARATHON", off_vote_machines$muni_county)] = "VILLAGE OF MAINE MARATHON"
  
  # setdiff(gov.2014.dem$muni_county, off_vote_machines$muni_county) # Difference of 47
  # length(off_vote_machines$muni_county) # 1854
  # length(gov.2014.dem$muni_county) # 1900 Difference of 46. Plus the errant space = 47. Therefore, these places are simply not included in the official list?
  
  off_vote_machines_cut = off_vote_machines
  off_vote_machines$County = NULL
  off_vote_machines$Municipality = NULL
  
  gov.2014.dem = plyr::join(gov.2014.dem, off_vote_machines, by = "muni_county")
  
  gov.2014.dem$tch_model_off_full = NA
  gov.2014.dem$tch_model_off_full = with(gov.2014.dem, ifelse(tch_model_off == "Dominion (Premier)-Accuvote TSX",
                                                                          "Dominion (Premier)-Accuvote TSX", gov.2014.dem$tch_model_off_full))
  gov.2014.dem$tch_model_off_full = with(gov.2014.dem, ifelse(tch_model_off == "Dominion (Sequoia)/Command Central-Edge",
                                                                          "Dominion (Sequoia)/Command Central-Edge",gov.2014.dem$tch_model_off_full))
  gov.2014.dem$tch_model_off_full = with(gov.2014.dem, ifelse(tch_model_off == "Dominion (Sequoia)/Command Central-Edge; ES&S Automark",
                                                                          "Dominion (Sequoia)/Command Central-Edge",gov.2014.dem$tch_model_off_full))
  gov.2014.dem$tch_model_off_full = with(gov.2014.dem, ifelse(tch_model_off == "ES&S iVotronic",
                                                                          "ES&S iVotronic",gov.2014.dem$tch_model_off_full))
  gov.2014.dem$tch_model_off_full[is.na(gov.2014.dem$tch_model_off_full)] = "aNone"
  gov.2014.dem$tch_model_off_full = with(gov.2014.dem, ifelse(is.na(tch_use_off),
                                                                          NA,gov.2014.dem$tch_model_off_full))
  
  colnames(gov.2014.dem)[grep("tch.use",colnames(gov.2014.dem),fixed = T)] = "tch_use_verivote"
  colnames(gov.2014.dem)[grep("os.use",colnames(gov.2014.dem),fixed = T)] = "os_use_verivote"
  colnames(gov.2014.dem)[grep("bmd.use",colnames(gov.2014.dem),fixed = T)] = "bmd_use_verivote"
  colnames(gov.2014.dem)[grep("X._collegedegreepersons25._2013",colnames(gov.2014.dem))] = "coll_deg_plus25_2013"
} # Putting in voting machines
{ # Making model df
  model.prim.repunit.df = gov.2014.dem
  # model.prim.df = subset(model.prim.df, votes_tot > 19)
  
  # Following Nate Cohn's method: https://twitter.com/Nate_Cohn/status/801226924156719104/photo/1?ref_src=twsrc%5Etfw
  # I found it's better to use the non-logged version of pop.sq.mile.2010
  model.prim.repunit.df$log.pop.sq.mile.2010 = log(model.prim.repunit.df$pop_sq_mile_2010)
  
  # ratio college and income are both highly correlated so maybe don't use median income
  # population density is highly correlated with black population, so I removed population density
  # I was not able to effectively model Menonminee county with my data (it has a very high Amerindian population). So,
  # I replaced the correlated and generally not significant 'white_pct' variable with 'white_nonwhite*
  
  ##
  model.prim.repunit.df2 = with(model.prim.repunit.df, data.frame(reporting_unit,county, muni_county,
                                                                  votes_perc_trump, votes_perc_kasich, votes_perc_cruz,votes_perc_other,trump_prim_won,turnout_perc,
                                                                  tch_prop,counted_absent_prop,rejected_absent_prop,
                                                                  sameday_reg_prop, total_ballots,registrants,late_registrants,
                                                                  county_paper_or_paperplusmachine,
                                                                  pct_white, ratio_nocollege_college, log.pop.sq.mile.2010,
                                                                  pop_sq_mile_2010,tch_model_off_full, os_model_off, bmd_model_off,
                                                                  medianhouseholdincome_2009.2013, pct_graduate, coll_deg_plus25_2013,
                                                                  votes_tot, inc_2015,inc_2015, unemp_rate,
                                                                  pct_male, pct_old, pct_latino, pct_white,
                                                                  pct_black,pct_hs))
  
  rownames(model.prim.repunit.df2) = model.prim.repunit.df2$reporting_unit
  
  model.prim.repunit.df = model.prim.repunit.df2
} # Making model df
{ # Getting municipality densities
  # require(rgdal); require(sp)
  # 
  # setwd("C:/Users/s_cas/Dropbox/Perso/2016 voting election county results/Wisconsin/All prep together/All processed files/")
  # getwd()
  # 
  # wisc.spat.o <-
  #   readOGR(
  #     dsn = "Prepped files/wisc.spat.orig.geojson",
  #     layer = "OGRGeoJSON",
  #     disambiguateFIDs = T,
  #     stringsAsFactors = F
  #   )
  # 
  # # First, getting population densities. Transforming to utm, zone 16 for Wisconsin
  # 
  # wisc.spat.o.trans = spTransform(
  #   wisc.spat.o,
  #   CRS(
  #     "+proj=utm +zone=16 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
  #   )
  # )
  # 
  # ur.area<-sapply(slot(wisc.spat.o.trans, "polygons"), function(x) sapply(slot(x, "Polygons"), slot, "area"))
  # ur.area.s = lapply(ur.area,sum)
  # 
  # wisc.spat.o@data$muni.area = unlist(ur.area.s) / 1000000
  # wisc.spat.o@data$muni.dense = as.numeric(wisc.spat.o@data$pop) / wisc.spat.o@data$muni.area
  # 
  # # head(wisc.spat.o@data$voters.tot2016)
  # # head(wisc.spat.o@data$muni.area)
  # 
  # density.join = with(wisc.spat.o@data, data.frame(muni_county,muni.dense))
  # 
  # model.prim.repunit.df = plyr::join(model.prim.repunit.df, density.join, by = "muni_county")
  
} # Municipality densities
{ ### Creating a carto-presentable map
  require(rgdal); require(sp); require(geojson)
  
  # wisc.spat.o <-
  #   readOGR(
  #     dsn = "Prepped files/wisc.spat.orig.geojson",
  #     layer = "OGRGeoJSON",
  #     disambiguateFIDs = T,
  #     stringsAsFactors = F
  #   )
  
  # wisc.spat.o.cut = wisc.spat.o
  # wisc.spat.o.cut@data[7:length(wisc.spat.o.cut@data)] = NULL
  # wisc.spat.o.cut@data$MCD_NAME = toupper(wisc.spat.o.cut@data$MCD_NAME)
  # 
  # wisc.join = wisc.spat.o.cut
  # 
  # wisc.join@data = plyr::join(wisc.join@data, model.prim.repunit.df, by = "muni_county", match = "first")
  # colnames(wisc.join@data) = make.unique(colnames(wisc.join@data))
  # wisc.join@data$county = NULL
  # wisc.join@data$MCD_FIPS.1 = NULL
  # 
  # wisc.join@data[wisc.join@data == Inf] = NA
  # wisc.join@data[wisc.join@data == -Inf] = NA
  # 
  # wisc.join@data[is.na(wisc.join@data)] = NA
  # wisc.join.df = as.data.frame(wisc.join)
  
  # writeOGR(
  #   wisc.join,
  #   "Prepped files/wisc.present.repprim.geojson",
  #   layer = "wisc.join",
  #   driver = "GeoJSON"
  # )
} # Creating a carto-presentable map

write.csv(model.prim.df,"model_2014_gov.csv")

write.csv(model.prim.repunit.df,"model_2014_gov_repunit.csv")

# Models repunit data 2012 2016 original --------------------------------------------------
  
  # NOTE: Multiplying the two voting method variables together improves the models, and reduces
  # The correlation with the other variables
  {
    # ## Touchscreen and BMD use are super highly correlated, as are use.machines and os.use
    # ## With turnout not logged
    # corr.table = with(
    #   repunit.2016.2012.o,
    #   data.frame(
    #     dem.change.perc,
    #     rep.change.perc,
    #     oth.change.perc,
    #     # turnout.change.perc,
    #     votes.tot2016,
    #     # county_use_opt_scan,
    #     tch_prop,
    #     # bmd.use,
    #     os.use,
    #     inc_2015,
    #     unemp_rate,
    #     pct_male,
    #     age_med,
    #     # pct_white,
    #     pct_latino,
    #     pct_black,
    #     pct_hs,
    #     pop_sq_mile_2010,
    #     pct_white,
    #     demwinner
    #   )
    # )
    # colnames(corr.table) = abbreviate( colnames(corr.table), minlength = 16 )
    # # colnames(corr.table)[6] = "voting.method"
    # # colnames(corr.table)[10] = "med.income"
    # correlationmatrix = cor(corr.table, use = "pairwise.complete.obs")
    # corrplot.all = corrplot(correlationmatrix, method = "number", type = "upper")
  } # Correlations
  { # Model data frame preparation
setwd("C:/Users/s_cas/Dropbox/Perso/2016 voting election county results/Wisconsin/All prep together/All processed files/")
    
    repunit.2016.2012.o$log.pop.sq.mile.2010 = log(repunit.2016.2012.o$pop_sq_mile_2010)
    
    repunit.2016.2012.o.mcdjoin = with(repunit.2016.2012.o, data.frame(muni_county, MCD_FIPS))
    
    repunit.2016.2012.o = plyr::join(repunit.2016.2012.o, repunit.2016.2012.o.mcdjoin, by = "muni_county")
    
    repunit.2016.2012.o = plyr::join(repunit.2016.2012.o, repunit.2016.2008.o.dem, by = "reporting_unit")
    
    # repunit.2016.2012.o$total
    
    model.o.repunit.df = with(repunit.2016.2012.o, data.frame(MCD_FIPS,reporting_unit,reporting_unit_orig,county, muni_county,
                                                       democrat_votes.perc, republican_votes.perc, other_votes.perc,
                                                       democrat_votes.rec, republican_votes.rec, other_votes.rec,
                                                       dem.change.perc,rep.change.perc,oth.change.perc,
                                                       tch_prop, os_prop, paper_prop, counted_absent_prop,rejected_absent_prop,
                                                       absentee_person_prop, absentee_undeliverable,
                                                       sameday_reg_prop,turnout_reg_2016,#turnout.2016.perc,
                                                       # county_paper_or_paperplusmachine, #county_use_opt_scan,
                                                       #county_machine_vendor_dealer, #turnout.change.perc,
                                                       # ratio_nocollege_college , log.pop.sq.mile.2010,
                                                       # pop_sq_mile_2010,#tch.use, os.use, bmd.use,
                                                       # medianhouseholdincome_2009.2013, #voters.tot2016, 
                                                       votes.tot2016, inspector_prop,
                                                       dem.perc2012, rep.perc2012, oth.perc2012, #turnout.2012,
                                                       democrat_votes.rec2012, republican_votes.rec2012, other_votes.rec2012,
                                                       votes.tot2012, 
                                                       # inc_2015,inc_2015, unemp_rate, X._collegedegreepersons25._2013, pct_graduate,
                                                       # pct_male, age_med, pct_latino, pct_white, pct_black, pct_white,
                                                       inspector_prop_2012,
                                                       total_ballots,registrants,late_registrants,tch_prop_2012,
                                                       os_prop_2012, paper_prop_2012, automark_prop_2012,
                                                       total_ballots_2012,turnout_reg_2012, counted_absent_prop_2012,
                                                      rejected_absent_prop_2012, absentee_person_prop_2012,
                                                      registrants_2012,late_registrants_2012, absentee_undeliverable_2012,ballots_counted_at_2012, 
                                                       sameday_reg_prop_2012,
                                                      inspector_prop_2012,
                                                      dem.perc2008, rep.perc2008, oth.perc2008, #turnout.2012,
                                                      votes.tot2008, 
                                                      tch_prop_2008,
                                                      os_prop_2008, paper_prop_2008, #automark_prop_2008,
                                                      total_ballots_2008,turnout_reg_2008, counted_absent_prop_2008,
                                                      rejected_absent_prop_2008, #absentee_person_prop_2008,
                                                      registrants_2008,late_registrants_2008,
                                                      sameday_reg_prop_2008,
                                                      ballots_counted_at, ballots_counted_at_2008,
                                                      absentee_undeliverable,  absentee_undeliverable_2008,pct_hs
))
    rownames(model.o.repunit.df) = model.o.repunit.df$reporting_unit
    
    model.o.repunit.df$MCD_FIPS = as.character(model.o.repunit.df$MCD_FIPS)
    
    tig_dems = read.csv("tiger_demographics.csv")
    tig_dems_join = with(tig_dems,data.frame(geoid))
    colnames(tig_dems_join) = "MCD_FIPS"
    tig_dems_join$MCD_FIPS = gsub("06000US","",tig_dems_join$MCD_FIPS)
    tig_dems_join$pop = tig_dems$pop
    tig_dems_join$inc_2015 = tig_dems$percap_inc_2015
    tig_dems_join$unemp_rate = tig_dems$unemp_1864_perc
    tig_dems_join$pct_graduate = tig_dems$bach_perc
    tig_dems_join$X._collegedegreepersons25._2013 = tig_dems$bach_perc
    tig_dems_join$pct_white = tig_dems$white_perc
    tig_dems_join$pct_black = tig_dems$black_perc
    tig_dems_join$pct_latino = tig_dems$hisp_perc
    tig_dems_join$pct_male = tig_dems$male_perc
    tig_dems_join$age_med = tig_dems$age_med # NOTE THIS IS NOW MEDIAN AGE NOT PCT OLD
    tig_dems_join$house_size_ave = tig_dems$house_size_ave
    
    model.o.repunit.df = plyr::join(model.o.repunit.df,tig_dems_join, by = "MCD_FIPS")
    
    # The following are the variables used by the Election snapshot in their models:
    # Income 2015, inc_2015
    # Unemployment rate Sept 2015, unemp_rate
    # Male ACS 5-year, pct_male
    # Age 62 + ACS 5 year, age_med
    # Hispanic or Latino ACS 5 year, pct_latino
    # White ACS 5-year, pct_white
    # Black ACS 5 year, pct_black
    # HS (high school) or less ACS 5 year, pct_hs
  } # Model data frame preparation
  { # Adding in vote change from Spring primary
    model.reprim.df = read.csv("model_reprim_repunit.csv")
    # model.reprim.df$rejected_absent_prop
    
    repprim_join = model.reprim.df[,c(2:21)]
    colnames(repprim_join)[2:20] <- paste(colnames(repprim_join)[2:20],"reprim", sep = "_")
    
    model.demprim.df = read.csv("model_demprim_repunit.csv")
    demprim_join = model.demprim.df[,c(2:21)]
    colnames(demprim_join)[2:20] <- paste(colnames(demprim_join)[2:20],"demprim", sep = "_")
    
    model.o.repunit.df = plyr::join(model.o.repunit.df,repprim_join,by="reporting_unit")
    model.o.repunit.df$trump_vote_diff = model.o.repunit.df$republican_votes.perc - model.o.repunit.df$votes_perc_trump_reprim
    model.o.repunit.df$tch_prop_reprim_diff = model.o.repunit.df$tch_prop - model.o.repunit.df$tch_prop_reprim
    
    model.o.repunit.df = plyr::join(model.o.repunit.df,demprim_join,by="reporting_unit")
    model.o.repunit.df$clinton_vote_diff = model.o.repunit.df$democrat_votes.perc - model.o.repunit.df$votes_perc_clinton_demprim
    model.o.repunit.df$tch_prop_demprim_diff = model.o.repunit.df$tch_prop - model.o.repunit.df$tch_prop_demprim
    
  } # Adding in vote change from Spring primary
  { # Adding in vote change from 2012 to primaries
    model.edl2012.df = read.csv("edl.2012.municounty.data.csv")
    # edl_join = model.edl2012.df[,c(2:12)]
    # colnames(edl_join)[4:11] <- paste(colnames(edl_join)[4:11],"2012_num", sep = "_")
    
    # model.o.repunit.df = plyr::join(model.o.repunit.df,edl_join,by="muni_county")
    model.o.repunit.df$trump_prim_2012_vote_diff = model.o.repunit.df$votes_perc_trump_reprim - model.o.repunit.df$rep.perc2012
    model.o.repunit.df$clinton_prim_2012_vote_diff = model.o.repunit.df$votes_perc_clinton_demprim - model.o.repunit.df$dem.perc2012
    model.o.repunit.df$tch_prop_2012_reprim_diff = model.o.repunit.df$tch_prop_reprim - model.o.repunit.df$tch_prop_2012
    model.o.repunit.df$tch_prop_2012_demprim_diff = model.o.repunit.df$tch_prop_demprim - model.o.repunit.df$tch_prop_2012
    model.o.repunit.df$tch_prop_2012_2016_diff = model.o.repunit.df$tch_prop - model.o.repunit.df$tch_prop_2012
    
  } # Adding in vote change from 2012 to primaries
  { # Adding in voting machines
    # getwd()
    # setwd("C:\\Users\\s_cas\\Dropbox\\Perso\\2016 voting election county results\\Wisconsin")
    off_vote_machines = read.xlsx2("voting_equipment_by_municipality_09_2016_xlsx_78114.xlsx1207162619.xlsx",
                                   sheetIndex = 1,
                                   stringsAsFactors=F)
    
    colnames(off_vote_machines)[3] = "os_model_off"
    colnames(off_vote_machines)[4] = "tch_bmd_model_off"
    
    off_vote_machines$tch_use_off = NA
    off_vote_machines$tch_use_off = ifelse(off_vote_machines$tch_bmd_model_off == "Dominion (Premier)-Accuvote TSX" |
                                             off_vote_machines$tch_bmd_model_off == "Dominion (Sequoia)/Command Central-Edge" |
                                             off_vote_machines$tch_bmd_model_off == "Dominion (Sequoia)/Command Central-Edge; ES&S Automark" |
                                             off_vote_machines$tch_bmd_model_off == "ES&S iVotronic",1, 0
    )
    off_vote_machines$bmd_use_off = NA
    off_vote_machines$bmd_use_off = ifelse(off_vote_machines$tch_bmd_model_off == "Dominion ImageCast Evolution" |
                                             off_vote_machines$tch_bmd_model_off == "ES&S Automark" |
                                             off_vote_machines$tch_bmd_model_off == "ES&S ExpressVote" |
                                             off_vote_machines$tch_bmd_model_off == "Populex-Populex 2.3" |
                                             off_vote_machines$tch_bmd_model_off == "Vote Pad-Vote Pad (non-electronic)"
                                           ,1, 0
    )
    off_vote_machines$os_use_off = NA
    off_vote_machines$os_use_off = ifelse(off_vote_machines$os_model_off == "None ",
                                          0, 1
    )
    # unique(off_vote_machines$os_model_off)
    off_vote_machines$os_model_off = ifelse(off_vote_machines$os_model_off == "None ",
                                            NA, off_vote_machines$os_model_off
    )
    
    off_vote_machines$tch_model_off = NA
    off_vote_machines$tch_model_off = ifelse(off_vote_machines$tch_use_off == 1,
                                             off_vote_machines$tch_bmd_model_off, NA
    )
    
    off_vote_machines$bmd_model_off = NA
    off_vote_machines$bmd_model_off = ifelse(off_vote_machines$bmd_use_off == 1,
                                             off_vote_machines$tch_bmd_model_off, NA
    )
    
    off_vote_machines$tch_use_off_true = NA
    off_vote_machines$tch_use_off_true = ifelse(off_vote_machines$os_use_off == 0,
                                                off_vote_machines$tch_use_off, 0
    )
    # table(off_vote_machines$tch_use_off_true)
    
    # #code_2 = substrRight(off_vote_machines$County, 2)
    code_2 = gsub( " COUNTY.*$", "", off_vote_machines$County)
    # #code_3 = substrRight(off_vote_machines$Municipality, 5)
    code_3 = gsub( " -.*$", "", off_vote_machines$Municipality)
    off_vote_machines$muni_county = paste(code_3,code_2, sep = " ")
    #
    # unique(grepl("550101201",model.o.repunit.df$MCD_FIPS))
    
    # setdiff(off_vote_machines$muni_county, model.o.repunit.df$muni_county)
    # off_vote_machines$muni_county[grep("TOWN OF GEORGETOWN  POLK", off_vote_machines$muni_county)] = "TOWN OF GEORGETOWN POLK"
    # off_vote_machines$muni_county[grep("TOWN OF WINDSOR DANE", off_vote_machines$muni_county)] = "VILLAGE OF WINDSOR DANE"
    # off_vote_machines$muni_county[grep("TOWN OF MAINE MARATHON", off_vote_machines$muni_county)] = "VILLAGE OF MAINE MARATHON"
    
    # setdiff(model.o.repunit.df$muni_county, off_vote_machines$muni_county) # Difference of 47
    # length(off_vote_machines$muni_county) # 1854
    # length(model.o.repunit.df$muni_county) # 1900 Difference of 46. Plus the errant space = 47. Therefore, these places are simply not included in the official list?
    
    off_vote_machines_cut = off_vote_machines
    off_vote_machines$County = NULL
    off_vote_machines$Municipality = NULL
    
    # off_vote_machines$reporting_unit = paste(off_vote_machines$municipality,off_vote_machines$reportingunit, off_vote_machines$county)
    # off_vote_machines$reporting_unit_orig = toupper(off_vote_machines$reporting_unit)
    # 
    # 
    # off_vote_machines$reporting_unit = gsub("WARDS","WARD",off_vote_machines$reporting_unit_orig)
    # # ward.12.mun$reporting_unit = gsub("WARDS","WARD",ward.12.mun$reporting_unit_orig)
    # off_vote_machines$reporting_unit = gsub("  "," ",off_vote_machines$reporting_unit)
    # # ward.12.mun$reporting_unit = gsub("  "," ",ward.12.mun$reporting_unit)
    # off_vote_machines$reporting_unit = gsub("&","-",off_vote_machines$reporting_unit)
    # # ward.12.mun$reporting_unit = gsub("&","-",ward.12.mun$reporting_unit)
    # 
    # off_vote_machines$reporting_unit = gsub("AND","-",off_vote_machines$reporting_unit)
    # # ward.12.mun$reporting_unit = gsub("AND","-",ward.12.mun$reporting_unit)
    # 
    # off_vote_machines$reporting_unit = gsub(",","-",off_vote_machines$reporting_unit)
    # # ward.12.mun$reporting_unit = gsub(",","-",ward.12.mun$reporting_unit)
    # off_vote_machines$reporting_unit = gsub(" - ","-",off_vote_machines$reporting_unit)
    # # ward.12.mun$reporting_unit = gsub(" - ","-",ward.12.mun$reporting_unit)
    # off_vote_machines$reporting_unit = gsub(" -","-",off_vote_machines$reporting_unit)
    # # ward.12.mun$reporting_unit = gsub(" -","-",ward.12.mun$reporting_unit)
    # off_vote_machines$reporting_unit = gsub("- ","-",off_vote_machines$reporting_unit)
    # # ward.12.mun$reporting_unit = gsub("- ","-",ward.12.mun$reporting_unit)
    # 
    # off_vote_machines$reporting_unit = gsub(" ","-",off_vote_machines$reporting_unit)
    # # ward.12.mun$reporting_unit = gsub(" ","-",ward.12.mun$reporting_unit)
    # 
    # off_vote_machines$reporting_unit = gsub("WD","",off_vote_machines$reporting_unit)
    # # ward.12.mun$reporting_unit = gsub("WD","",ward.12.mun$reporting_unit)
    # 
    # off_vote_machines$reporting_unit = gsub("COMBINED","",off_vote_machines$reporting_unit)
    # # ward.12.mun$reporting_unit = gsub("COMBINED","",ward.12.mun$reporting_unit)
    # 
    # off_vote_machines$reporting_unit = gsub("--","-",off_vote_machines$reporting_unit)
    # # ward.12.mun$reporting_unit = gsub("--","-",ward.12.mun$reporting_u
    # 
    
    model.o.repunit.df = plyr::join(model.o.repunit.df, off_vote_machines, by = "muni_county")
    
    model.o.repunit.df$tch_model_off_full = NA
    model.o.repunit.df$tch_model_off_full = with(model.o.repunit.df, ifelse(tch_model_off == "Dominion (Premier)-Accuvote TSX",
                                                            "Dominion (Premier)-Accuvote TSX", model.o.repunit.df$tch_model_off_full))
    model.o.repunit.df$tch_model_off_full = with(model.o.repunit.df, ifelse(tch_model_off == "Dominion (Sequoia)/Command Central-Edge",
                                                            "Dominion (Sequoia)/Command Central-Edge",model.o.repunit.df$tch_model_off_full))
    model.o.repunit.df$tch_model_off_full = with(model.o.repunit.df, ifelse(tch_model_off == "Dominion (Sequoia)/Command Central-Edge; ES&S Automark",
                                                            "Dominion (Sequoia)/Command Central-Edge",model.o.repunit.df$tch_model_off_full))
    model.o.repunit.df$tch_model_off_full = with(model.o.repunit.df, ifelse(tch_model_off == "ES&S iVotronic",
                                                            "ES&S iVotronic",model.o.repunit.df$tch_model_off_full))
    model.o.repunit.df$tch_model_off_full[is.na(model.o.repunit.df$tch_model_off_full)] = "aNone"
    model.o.repunit.df$tch_model_off_full = with(model.o.repunit.df, ifelse(is.na(tch_use_off),
                                                            NA,model.o.repunit.df$tch_model_off_full))
    
    # model.o.repunit.df[grep("tch_use_verivote.1",colnames(model.o.repunit.df))] = NULL
    # model.o.repunit.df[grep("tch_use_verivote.2",colnames(model.o.repunit.df))] = NULL
    #
    colnames(model.o.repunit.df)[grep("tch.use",colnames(model.o.repunit.df),fixed = T)] = "tch_use_verivote"
    colnames(model.o.repunit.df)[grep("os.use",colnames(model.o.repunit.df),fixed = T)] = "os_use_verivote"
    colnames(model.o.repunit.df)[grep("bmd.use",colnames(model.o.repunit.df),fixed = T)] = "bmd_use_verivote"
    colnames(model.o.repunit.df)[grep("X._collegedegreepersons25._2013",colnames(model.o.repunit.df))] = "coll_deg_plus25_2013"
  } # Adding in voting machines
  {
    model.o.repunit.df$demwinner = NA
    model.o.repunit.df$demwinner = ifelse(model.o.repunit.df$democrat_votes.perc >
                                    model.o.repunit.df$republican_votes.perc &
                                    !is.na(model.o.repunit.df$democrat_votes.perc), 1,0)
    model.o.repunit.df$demwinner = factor(model.o.repunit.df$demwinner)
    
    model.o.repunit.df$demwinner2012 = NA
    model.o.repunit.df$demwinner2012 = ifelse(model.o.repunit.df$dem.perc2012 >
                                        model.o.repunit.df$rep.perc2012 &
                                        !is.na(model.o.repunit.df$democrat_votes.perc), 1,0)
    model.o.repunit.df$demwinner2012 = factor(model.o.repunit.df$demwinner2012)
  } # Dem won in 2012
  { # Subsetting data for the models
    # Only keep municipalities with 20 or more people voting - below this is too skewed
    # model.o.repunit.df = subset(model.o.repunit.df, votes.tot2016 > 19)
    # 
    # model.o.repunit.df$turnout.2012[model.o.repunit.df$turnout.2012 > 120] = NA
    # model.o.repunit.df$turnout.2012[model.o.repunit.df$turnout.2012 < 30] = NA
    # 
    # model.o.repunit.df$turnout.2016.perc[model.o.repunit.df$votes.tot2016 < 20] = NA
    # model.o.repunit.df$turnout.2012[model.o.repunit.df$votes.tot2012 < 20] = NA
    # 
    # turnout.high.2012 = subset(model.o.repunit.df, turnout.2012 > 100)
    # 
    # model.o.repunit.df$dem.change.perc[model.o.repunit.df$dem.change.perc < - 49] = NA
    # model.o.repunit.df$rep.change.perc[model.o.repunit.df$rep.change.perc < - 49] = NA
    # model.o.repunit.df$rep.change.perc[model.o.repunit.df$rep.change.perc > 49] = NA
    # 
    # model.o.repunit.df$turnout.2012[model.o.repunit.df$turnout.2012 > 110] = NA
    # model.o.repunit.df$oth.perc2012[max(model.o.repunit.df$oth.perc2012,na.rm=T)] = NA
    # model.o.repunit.df$other_votes.perc[model.o.repunit.df$other_votes.perc ==
    #                               max(model.o.repunit.df$other_votes.perc,na.rm=T)] = NA
    # model.o.repunit.df$turnout.change.perc[model.o.repunit.df$turnout.change.perc < - 30] = NA
    # model.o.repunit.df = subset(model.o.repunit.df, log.pop.sq.mile.2010 > median(log.pop.sq.mile.2010,na.rm=T))
  } # Subsetting data for the models
  { # Getting municipality densities
    require(rgdal); require(sp)
    getwd()
    #
    # wisc.spat.o <-
    #   readOGR(
    #     dsn = "Prepped files/wisc.spat.orig.geojson",
    #     layer = "OGRGeoJSON",
    #     disambiguateFIDs = T,
    #     stringsAsFactors = F
    #   )
    
    # First, getting population densities. Transforming to utm, zone 16 for Wisconsin
    
    model.o.df = read.csv("modeldfward.o.csv")
    
    # wisc.spat.o.trans = spTransform(
    #   wisc.spat.o,
    #   CRS(
    #     "+proj=utm +zone=16 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
    #   )
    # )
    # 
    # ur.area<-sapply(slot(wisc.spat.o.trans, "polygons"), function(x) sapply(slot(x, "Polygons"), slot, "area"))
    # ur.area.s = lapply(ur.area,sum)
    # 
    # wisc.spat.o@data$muni.area = unlist(ur.area.s) / 1000000
    # 
    # pop.join = with(model.o.df, data.frame(muni_county,pop))
    # wisc.spat.o@data = plyr::join(wisc.spat.o@data, pop.join, by = "muni_county")
    # 
    # wisc.spat.o@data$muni.dense = as.numeric(wisc.spat.o@data$pop) / wisc.spat.o@data$muni.area
    # density.join = with(wisc.spat.o@data, data.frame(muni_county,muni.dense))
    
    density.join = with(model.o.df, data.frame(muni_county,muni.dense))
    
    model.o.repunit.df = plyr::join(model.o.repunit.df, density.join, by = "muni_county",match="first")
    
    # http://spatialreference.org/ref/epsg/4326/html/
    # GEOGCS["WGS 84",
    #        DATUM["WGS_1984",
    #              SPHEROID["WGS 84",6378137,298.257223563,
    #                       AUTHORITY["EPSG","7030"]],
    #              AUTHORITY["EPSG","6326"]],
    #        PRIMEM["Greenwich",0,
    #               AUTHORITY["EPSG","8901"]],
    #        UNIT["degree",0.01745329251994328,
    #             AUTHORITY["EPSG","9122"]],
    #        AUTHORITY["EPSG","4326"]]
    
  } # Municipality densities
  { ### Creating a carto-presentable map
    require(rgdal); require(sp); require(geojson)
    
    # wisc.spat.o <-
    #   readOGR(
    #     dsn = "Prepped files/wisc.present.orig.geojson",
    #     layer = "OGRGeoJSON",
    #     disambiguateFIDs = T,
    #     stringsAsFactors = F
    #   )
    
    # wisc.spat.o.cut = wisc.spat.o
    # wisc.spat.o.cut@data[7:length(wisc.spat.o.cut@data)] = NULL
    # wisc.spat.o.cut@data$MCD_NAME = toupper(wisc.spat.o.cut@data$MCD_NAME)
    # 
    # wisc.join = wisc.spat.o.cut
    # 
    # wisc.join@data = plyr::join(wisc.join@data, model.o.repunit.df, by = "muni_county", match = "first")
    # colnames(wisc.join@data) = make.unique(colnames(wisc.join@data))
    # wisc.join@data$county_machine_vendor_dealer = as.character( wisc.join@data$county_machine_vendor_dealer)
    # wisc.join@data$demwinner = as.character( wisc.join@data$demwinner)
    # wisc.join@data$demwinner2012 = as.character( wisc.join@data$demwinner2012)
    # 
    # wisc.join@data$county = NULL
    # wisc.join@data$MCD_FIPS.1 = NULL
    # 
    # wisc.join@data[wisc.join@data == Inf] = NA
    # wisc.join@data[wisc.join@data == -Inf] = NA
    # 
    # wisc.join@data[is.na(wisc.join@data)] = NA
    # 
    # # unique(wisc.join@data$muni_county=="NA")
    # # wisc.join = wisc.join[-wisc.join@data$muni_county=="NA",]
    # # wisc.join = subset(wisc.join,wisc.join@data$muni_county!="NA")
    # 
    # wisc.join.df = as.data.frame(wisc.join)
    # # str(wisc.join.df)
    # 
    # # wisc.join@data = clean_names(wisc.join@data)
    # # require(geojson)
    # 
    # writeOGR(
    #   wisc.join,
    #   "Prepped files/wisc.present.orig.repunit.geojson",
    #   layer = "wisc.join",
    #   driver = "GeoJSON"
    # )
    
    # geo_write(wisc.join, file = "~/Prepped files/wisc.present.orig.geojson")
    
    # writeOGR(
    #   wisc.join,
    #   "Prepped files/wisc.present.orig.shp",
    #   layer = "wisc.join",
    #   driver = "ESRI Shapefile"
    # )
    
  } # Creating a carto-presentable map
  
  write.csv(model.o.repunit.df,"modeldfward.repunit.o.csv")
  

# Models repunit data 2012 2016 recount --------------------------------------------------
  
  # NOTE: Multiplying the two voting method variables together improves the models, and reduces
  # The correlation with the other variables
  {
    ## Touchscreen and BMD use are super highly correlated, as are use.machines and os.use
    ## With turnout not logged
    # corr.table = with(
    #   repunit.2016.2012.r,
    #   data.frame(
    #     dem.change.perc,
    #     rep.change.perc,
    #     oth.change.perc,
    #     # turnout.change.perc,
    #     votes.tot2016,
    #     # county_use_opt_scan,
    #     tch_prop,
    #     # bmd.use,
    #     os.use,
    #     inc_2015,
    #     unemp_rate,
    #     pct_male,
    #     age_med,
    #     # pct_white,
    #     pct_latino,
    #     pct_black,
    #     pct_hs,
    #     pop_sq_mile_2010,
    #     pct_white,
    #     demwinner
    #   )
    # )
    # colnames(corr.table) = abbreviate( colnames(corr.table), minlength = 16 )
    # # colnames(corr.table)[6] = "voting.method"
    # # colnames(corr.table)[10] = "med.income"
    # correlationmatrix = cor(corr.table, use = "pairwise.complete.rbs")
    # corrplot.all = corrplot(correlationmatrix, method = "number", type = "upper")
  } # Correlations
  { # Model data frame preparation
    setwd("C:/Users/s_cas/Dropbox/Perso/2016 voting election county results/Wisconsin/All prep together/All processed files/")
    
    repunit.2016.2012.r$log.pop.sq.mile.2010 = log(repunit.2016.2012.r$pop_sq_mile_2010)
    
    repunit.2016.2012.r.mcdjoin = with(repunit.2016.2012.r, data.frame(muni_county, MCD_FIPS))
    
    repunit.2016.2012.r = plyr::join(repunit.2016.2012.r, repunit.2016.2012.r.mcdjoin, by = "muni_county")
    
    repunit.2016.2012.r = plyr::join(repunit.2016.2012.r, repunit.2016.2008.r, by = "reporting_unit")
    
    # repunit.2016.2012.r$total
    
    model.r.repunit.df = with(repunit.2016.2012.r, data.frame(MCD_FIPS,reporting_unit,reporting_unit_orig,county, muni_county,
                                                                  democrat_votes.perc, republican_votes.perc, other_votes.perc,
                                                                  democrat_votes.rec, republican_votes.rec, other_votes.rec,
                                                                  dem.change.perc,rep.change.perc,oth.change.perc,
                                                                  tch_prop, os_prop, paper_prop, counted_absent_prop,rejected_absent_prop,
                                                                  absentee_person_prop, absentee_undeliverable,
                                                                  sameday_reg_prop,turnout_reg_2016,#turnout.2016.perc,
                                                                  # county_paper_or_paperplusmachine, #county_use_opt_scan,
                                                                  #county_machine_vendor_dealer, #turnout.change.perc,
                                                                  # ratio_nocollege_college , log.pop.sq.mile.2010,
                                                                  # pop_sq_mile_2010,#tch.use, os.use, bmd.use,
                                                                  # medianhouseholdincome_2009.2013, #voters.tot2016, 
                                                                  votes.tot2016, inspector_prop,
                                                                  dem.perc2012, rep.perc2012, oth.perc2012, #turnout.2012,
                                                                  democrat_votes.rec2012, republican_votes.rec2012, other_votes.rec2012,
                                                                  votes.tot2012, 
                                                                  # inc_2015,inc_2015, unemp_rate, X._collegedegreepersons25._2013, pct_graduate,
                                                                  # pct_male, age_med, pct_latino, pct_white, pct_black, pct_white,
                                                                  inspector_prop_2012,
                                                                  total_ballots,registrants,late_registrants,tch_prop_2012,
                                                                  os_prop_2012, paper_prop_2012, automark_prop_2012,
                                                                  total_ballots_2012,turnout_reg_2012, counted_absent_prop_2012,
                                                                  rejected_absent_prop_2012, absentee_person_prop_2012,
                                                                  registrants_2012,late_registrants_2012, absentee_undeliverable_2012,ballots_counted_at_2012, 
                                                                  sameday_reg_prop_2012,
                                                                  inspector_prop_2012,
                                                                  dem.perc2008, rep.perc2008, oth.perc2008, #turnout.2012,
                                                                  votes.tot2008, 
                                                                  tch_prop_2008,
                                                                  os_prop_2008, paper_prop_2008, #automark_prop_2008,
                                                                  total_ballots_2008,turnout_reg_2008, counted_absent_prop_2008,
                                                                  rejected_absent_prop_2008, #absentee_person_prop_2008,
                                                                  registrants_2008,late_registrants_2008,
                                                                  sameday_reg_prop_2008,
                                                                  ballots_counted_at, ballots_counted_at_2008,
                                                                  absentee_undeliverable,  absentee_undeliverable_2008,pct_hs
    ))
    rownames(model.r.repunit.df) = model.r.repunit.df$reporting_unit
    
    model.r.repunit.df$MCD_FIPS = as.character(model.r.repunit.df$MCD_FIPS)
    
    tig_dems = read.csv("tiger_demographics.csv")
    tig_dems_join = with(tig_dems,data.frame(geoid))
    colnames(tig_dems_join) = "MCD_FIPS"
    tig_dems_join$MCD_FIPS = gsub("06000US","",tig_dems_join$MCD_FIPS)
    tig_dems_join$pop = tig_dems$pop
    tig_dems_join$inc_2015 = tig_dems$percap_inc_2015
    tig_dems_join$unemp_rate = tig_dems$unemp_1864_perc
    tig_dems_join$pct_graduate = tig_dems$bach_perc
    tig_dems_join$X._collegedegreepersons25._2013 = tig_dems$bach_perc
    tig_dems_join$pct_white = tig_dems$white_perc
    tig_dems_join$pct_black = tig_dems$black_perc
    tig_dems_join$pct_latino = tig_dems$hisp_perc
    tig_dems_join$pct_male = tig_dems$male_perc
    tig_dems_join$age_med = tig_dems$age_med # NOTE THIS IS NOW MEDIAN AGE NOT PCT OLD
    tig_dems_join$house_size_ave = tig_dems$house_size_ave
    
    model.r.repunit.df = plyr::join(model.r.repunit.df,tig_dems_join, by = "MCD_FIPS")
    
    # The following are the variables used by the Election snapshot in their models:
    # Income 2015, inc_2015
    # Unemployment rate Sept 2015, unemp_rate
    # Male ACS 5-year, pct_male
    # Age 62 + ACS 5 year, age_med
    # Hispanic or Latino ACS 5 year, pct_latino
    # White ACS 5-year, pct_white
    # Black ACS 5 year, pct_black
    # HS (high school) or less ACS 5 year, pct_hs
  } # Model data frame preparation
  { # Adding in vote change from Spring primary
    model.reprim.df = read.csv("model_reprim_repunit.csv")
    # model.reprim.df$rejected_absent_prop
    
    repprim_join = model.reprim.df[,c(2:21)]
    colnames(repprim_join)[2:20] <- paste(colnames(repprim_join)[2:20],"reprim", sep = "_")
    
    model.demprim.df = read.csv("model_demprim_repunit.csv")
    demprim_join = model.demprim.df[,c(2:21)]
    colnames(demprim_join)[2:20] <- paste(colnames(demprim_join)[2:20],"demprim", sep = "_")
    
    model.r.repunit.df = plyr::join(model.r.repunit.df,repprim_join,by="reporting_unit")
    model.r.repunit.df$trump_vote_diff = model.r.repunit.df$republican_votes.perc - model.r.repunit.df$votes_perc_trump_reprim
    model.r.repunit.df$tch_prop_reprim_diff = model.r.repunit.df$tch_prop - model.r.repunit.df$tch_prop_reprim
    
    model.r.repunit.df = plyr::join(model.r.repunit.df,demprim_join,by="reporting_unit")
    model.r.repunit.df$clinton_vote_diff = model.r.repunit.df$democrat_votes.perc - model.r.repunit.df$votes_perc_clinton_demprim
    model.r.repunit.df$tch_prop_demprim_diff = model.r.repunit.df$tch_prop - model.r.repunit.df$tch_prop_demprim
    
  } # Adding in vote change from Spring primary
  { # Adding in vote change from 2012 to primaries
    model.edl2012.df = read.csv("edl.2012.municounty.data.csv")
    # edl_join = model.edl2012.df[,c(2:12)]
    # colnames(edl_join)[4:11] <- paste(colnames(edl_join)[4:11],"2012_num", sep = "_")
    
    # model.r.repunit.df = plyr::join(model.r.repunit.df,edl_join,by="muni_county")
    model.r.repunit.df$trump_prim_2012_vote_diff = model.r.repunit.df$votes_perc_trump_reprim - model.r.repunit.df$rep.perc2012
    model.r.repunit.df$clinton_prim_2012_vote_diff = model.r.repunit.df$votes_perc_clinton_demprim - model.r.repunit.df$dem.perc2012
    model.r.repunit.df$tch_prop_2012_reprim_diff = model.r.repunit.df$tch_prop_reprim - model.r.repunit.df$tch_prop_2012
    model.r.repunit.df$tch_prop_2012_demprim_diff = model.r.repunit.df$tch_prop_demprim - model.r.repunit.df$tch_prop_2012
    model.r.repunit.df$tch_prop_2012_2016_diff = model.r.repunit.df$tch_prop - model.r.repunit.df$tch_prop_2012
    
  } # Adding in vote change from 2012 to primaries
  { # Adding in voting machines
    # getwd()
    # setwd("C:\\Users\\s_cas\\Dropbox\\Perso\\2016 voting election county results\\Wisconsin")
    off_vote_machines = read.xlsx2("voting_equipment_by_municipality_09_2016_xlsx_78114.xlsx1207162619.xlsx",
                                   sheetIndex = 1,
                                   stringsAsFactors=F)
    
    colnames(off_vote_machines)[3] = "os_model_off"
    colnames(off_vote_machines)[4] = "tch_bmd_model_off"
    
    off_vote_machines$tch_use_off = NA
    off_vote_machines$tch_use_off = ifelse(off_vote_machines$tch_bmd_model_off == "Dominion (Premier)-Accuvote TSX" |
                                             off_vote_machines$tch_bmd_model_off == "Dominion (Sequoia)/Command Central-Edge" |
                                             off_vote_machines$tch_bmd_model_off == "Dominion (Sequoia)/Command Central-Edge; ES&S Automark" |
                                             off_vote_machines$tch_bmd_model_off == "ES&S iVotronic",1, 0
    )
    off_vote_machines$bmd_use_off = NA
    off_vote_machines$bmd_use_off = ifelse(off_vote_machines$tch_bmd_model_off == "Dominion ImageCast Evolution" |
                                             off_vote_machines$tch_bmd_model_off == "ES&S Automark" |
                                             off_vote_machines$tch_bmd_model_off == "ES&S ExpressVote" |
                                             off_vote_machines$tch_bmd_model_off == "Populex-Populex 2.3" |
                                             off_vote_machines$tch_bmd_model_off == "Vote Pad-Vote Pad (non-electronic)"
                                           ,1, 0
    )
    off_vote_machines$os_use_off = NA
    off_vote_machines$os_use_off = ifelse(off_vote_machines$os_model_off == "None ",
                                          0, 1
    )
    # unique(off_vote_machines$os_model_off)
    off_vote_machines$os_model_off = ifelse(off_vote_machines$os_model_off == "None ",
                                            NA, off_vote_machines$os_model_off
    )
    
    off_vote_machines$tch_model_off = NA
    off_vote_machines$tch_model_off = ifelse(off_vote_machines$tch_use_off == 1,
                                             off_vote_machines$tch_bmd_model_off, NA
    )
    
    off_vote_machines$bmd_model_off = NA
    off_vote_machines$bmd_model_off = ifelse(off_vote_machines$bmd_use_off == 1,
                                             off_vote_machines$tch_bmd_model_off, NA
    )
    
    off_vote_machines$tch_use_off_true = NA
    off_vote_machines$tch_use_off_true = ifelse(off_vote_machines$os_use_off == 0,
                                                off_vote_machines$tch_use_off, 0
    )
    # table(off_vote_machines$tch_use_off_true)
    
    # #code_2 = substrRight(off_vote_machines$County, 2)
    code_2 = gsub( " COUNTY.*$", "", off_vote_machines$County)
    # #code_3 = substrRight(off_vote_machines$Municipality, 5)
    code_3 = gsub( " -.*$", "", off_vote_machines$Municipality)
    off_vote_machines$muni_county = paste(code_3,code_2, sep = " ")
    #
    # unique(grepl("550101201",model.r.repunit.df$MCD_FIPS))
    
    # setdiff(off_vote_machines$muni_county, model.r.repunit.df$muni_county)
    # off_vote_machines$muni_county[grep("TOWN OF GEORGETOWN  POLK", off_vote_machines$muni_county)] = "TOWN OF GEORGETOWN POLK"
    # off_vote_machines$muni_county[grep("TOWN OF WINDSOR DANE", off_vote_machines$muni_county)] = "VILLAGE OF WINDSOR DANE"
    # off_vote_machines$muni_county[grep("TOWN OF MAINE MARATHON", off_vote_machines$muni_county)] = "VILLAGE OF MAINE MARATHON"
    
    # setdiff(model.r.repunit.df$muni_county, off_vote_machines$muni_county) # Difference of 47
    # length(off_vote_machines$muni_county) # 1854
    # length(model.r.repunit.df$muni_county) # 1900 Difference of 46. Plus the errant space = 47. Therefore, these places are simply not included in the official list?
    
    off_vote_machines_cut = off_vote_machines
    off_vote_machines$County = NULL
    off_vote_machines$Municipality = NULL
    
    # off_vote_machines$reporting_unit = paste(off_vote_machines$municipality,off_vote_machines$reportingunit, off_vote_machines$county)
    # off_vote_machines$reporting_unit_orig = toupper(off_vote_machines$reporting_unit)
    # 
    # 
    # off_vote_machines$reporting_unit = gsub("WARDS","WARD",off_vote_machines$reporting_unit_orig)
    # # ward.12.mun$reporting_unit = gsub("WARDS","WARD",ward.12.mun$reporting_unit_orig)
    # off_vote_machines$reporting_unit = gsub("  "," ",off_vote_machines$reporting_unit)
    # # ward.12.mun$reporting_unit = gsub("  "," ",ward.12.mun$reporting_unit)
    # off_vote_machines$reporting_unit = gsub("&","-",off_vote_machines$reporting_unit)
    # # ward.12.mun$reporting_unit = gsub("&","-",ward.12.mun$reporting_unit)
    # 
    # off_vote_machines$reporting_unit = gsub("AND","-",off_vote_machines$reporting_unit)
    # # ward.12.mun$reporting_unit = gsub("AND","-",ward.12.mun$reporting_unit)
    # 
    # off_vote_machines$reporting_unit = gsub(",","-",off_vote_machines$reporting_unit)
    # # ward.12.mun$reporting_unit = gsub(",","-",ward.12.mun$reporting_unit)
    # off_vote_machines$reporting_unit = gsub(" - ","-",off_vote_machines$reporting_unit)
    # # ward.12.mun$reporting_unit = gsub(" - ","-",ward.12.mun$reporting_unit)
    # off_vote_machines$reporting_unit = gsub(" -","-",off_vote_machines$reporting_unit)
    # # ward.12.mun$reporting_unit = gsub(" -","-",ward.12.mun$reporting_unit)
    # off_vote_machines$reporting_unit = gsub("- ","-",off_vote_machines$reporting_unit)
    # # ward.12.mun$reporting_unit = gsub("- ","-",ward.12.mun$reporting_unit)
    # 
    # off_vote_machines$reporting_unit = gsub(" ","-",off_vote_machines$reporting_unit)
    # # ward.12.mun$reporting_unit = gsub(" ","-",ward.12.mun$reporting_unit)
    # 
    # off_vote_machines$reporting_unit = gsub("WD","",off_vote_machines$reporting_unit)
    # # ward.12.mun$reporting_unit = gsub("WD","",ward.12.mun$reporting_unit)
    # 
    # off_vote_machines$reporting_unit = gsub("COMBINED","",off_vote_machines$reporting_unit)
    # # ward.12.mun$reporting_unit = gsub("COMBINED","",ward.12.mun$reporting_unit)
    # 
    # off_vote_machines$reporting_unit = gsub("--","-",off_vote_machines$reporting_unit)
    # # ward.12.mun$reporting_unit = gsub("--","-",ward.12.mun$reporting_u
    # 
    
    model.r.repunit.df = plyr::join(model.r.repunit.df, off_vote_machines, by = "muni_county")
    
    model.r.repunit.df$tch_model_off_full = NA
    model.r.repunit.df$tch_model_off_full = with(model.r.repunit.df, ifelse(tch_model_off == "Dominion (Premier)-Accuvote TSX",
                                                                            "Dominion (Premier)-Accuvote TSX", model.r.repunit.df$tch_model_off_full))
    model.r.repunit.df$tch_model_off_full = with(model.r.repunit.df, ifelse(tch_model_off == "Dominion (Sequoia)/Command Central-Edge",
                                                                            "Dominion (Sequoia)/Command Central-Edge",model.r.repunit.df$tch_model_off_full))
    model.r.repunit.df$tch_model_off_full = with(model.r.repunit.df, ifelse(tch_model_off == "Dominion (Sequoia)/Command Central-Edge; ES&S Automark",
                                                                            "Dominion (Sequoia)/Command Central-Edge",model.r.repunit.df$tch_model_off_full))
    model.r.repunit.df$tch_model_off_full = with(model.r.repunit.df, ifelse(tch_model_off == "ES&S iVotronic",
                                                                            "ES&S iVotronic",model.r.repunit.df$tch_model_off_full))
    model.r.repunit.df$tch_model_off_full[is.na(model.r.repunit.df$tch_model_off_full)] = "aNone"
    model.r.repunit.df$tch_model_off_full = with(model.r.repunit.df, ifelse(is.na(tch_use_off),
                                                                            NA,model.r.repunit.df$tch_model_off_full))
    
    # model.r.repunit.df[grep("tch_use_verivote.1",colnames(model.r.repunit.df))] = NULL
    # model.r.repunit.df[grep("tch_use_verivote.2",colnames(model.r.repunit.df))] = NULL
    #
    colnames(model.r.repunit.df)[grep("tch.use",colnames(model.r.repunit.df),fixed = T)] = "tch_use_verivote"
    colnames(model.r.repunit.df)[grep("os.use",colnames(model.r.repunit.df),fixed = T)] = "os_use_verivote"
    colnames(model.r.repunit.df)[grep("bmd.use",colnames(model.r.repunit.df),fixed = T)] = "bmd_use_verivote"
    colnames(model.r.repunit.df)[grep("X._collegedegreepersons25._2013",colnames(model.r.repunit.df))] = "coll_deg_plus25_2013"
  } # Adding in voting machines
  {
    model.r.repunit.df$demwinner = NA
    model.r.repunit.df$demwinner = ifelse(model.r.repunit.df$democrat_votes.perc >
                                            model.r.repunit.df$republican_votes.perc &
                                            !is.na(model.r.repunit.df$democrat_votes.perc), 1,0)
    model.r.repunit.df$demwinner = factor(model.r.repunit.df$demwinner)
    
    model.r.repunit.df$demwinner2012 = NA
    model.r.repunit.df$demwinner2012 = ifelse(model.r.repunit.df$dem.perc2012 >
                                                model.r.repunit.df$rep.perc2012 &
                                                !is.na(model.r.repunit.df$democrat_votes.perc), 1,0)
    model.r.repunit.df$demwinner2012 = factor(model.r.repunit.df$demwinner2012)
  } # Dem won in 2012
  { # Subsetting data for the models
    # Only keep municipalities with 20 or more people voting - below this is too skewed
    # model.r.repunit.df = subset(model.r.repunit.df, votes.tot2016 > 19)
    # 
    # model.r.repunit.df$turnout.2012[model.r.repunit.df$turnout.2012 > 120] = NA
    # model.r.repunit.df$turnout.2012[model.r.repunit.df$turnout.2012 < 30] = NA
    # 
    # model.r.repunit.df$turnout.2016.perc[model.r.repunit.df$votes.tot2016 < 20] = NA
    # model.r.repunit.df$turnout.2012[model.r.repunit.df$votes.tot2012 < 20] = NA
    # 
    # turnout.high.2012 = subset(model.r.repunit.df, turnout.2012 > 100)
    # 
    # model.r.repunit.df$dem.change.perc[model.r.repunit.df$dem.change.perc < - 49] = NA
    # model.r.repunit.df$rep.change.perc[model.r.repunit.df$rep.change.perc < - 49] = NA
    # model.r.repunit.df$rep.change.perc[model.r.repunit.df$rep.change.perc > 49] = NA
    # 
    # model.r.repunit.df$turnout.2012[model.r.repunit.df$turnout.2012 > 110] = NA
    # model.r.repunit.df$oth.perc2012[max(model.r.repunit.df$oth.perc2012,na.rm=T)] = NA
    # model.r.repunit.df$other_votes.perc[model.r.repunit.df$other_votes.perc ==
    #                               max(model.r.repunit.df$other_votes.perc,na.rm=T)] = NA
    # model.r.repunit.df$turnout.change.perc[model.r.repunit.df$turnout.change.perc < - 30] = NA
    # model.r.repunit.df = subset(model.r.repunit.df, log.pop.sq.mile.2010 > median(log.pop.sq.mile.2010,na.rm=T))
  } # Subsetting data for the models
  { # Getting municipality densities
    require(rgdal); require(sp)
    getwd()
    #
    # wisc.spat.r <-
    #   readOGR(
    #     dsn = "Prepped files/wisc.spat.rrig.geojson",
    #     layer = "OGRGeoJSON",
    #     disambiguateFIDs = T,
    #     stringsAsFactors = F
    #   )
    
    # First, getting population densities. Transforming to utm, zone 16 for Wisconsin
    
    model.r.df = read.csv("modeldfward.r.csv")
    
    # wisc.spat.r.trans = spTransform(
    #   wisc.spat.r,
    #   CRS(
    #     "+proj=utm +zone=16 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
    #   )
    # )
    # 
    # ur.area<-sapply(slot(wisc.spat.r.trans, "polygons"), function(x) sapply(slot(x, "Polygons"), slot, "area"))
    # ur.area.s = lapply(ur.area,sum)
    # 
    # wisc.spat.r@data$muni.area = unlist(ur.area.s) / 1000000
    # 
    # pop.join = with(model.r.df, data.frame(muni_county,pop))
    # wisc.spat.r@data = plyr::join(wisc.spat.r@data, pop.join, by = "muni_county")
    # 
    # wisc.spat.r@data$muni.dense = as.numeric(wisc.spat.r@data$pop) / wisc.spat.r@data$muni.area
    # density.join = with(wisc.spat.r@data, data.frame(muni_county,muni.dense))
    
    density.join = with(model.r.df, data.frame(muni_county,muni.dense))
    
    model.r.repunit.df = plyr::join(model.r.repunit.df, density.join, by = "muni_county",match="first")
    
    # http://spatialreference.rrg/ref/epsg/4326/html/
    # GEOGCS["WGS 84",
    #        DATUM["WGS_1984",
    #              SPHEROID["WGS 84",6378137,298.257223563,
    #                       AUTHORITY["EPSG","7030"]],
    #              AUTHORITY["EPSG","6326"]],
    #        PRIMEM["Greenwich",0,
    #               AUTHORITY["EPSG","8901"]],
    #        UNIT["degree",0.01745329251994328,
    #             AUTHORITY["EPSG","9122"]],
    #        AUTHORITY["EPSG","4326"]]
    
  } # Municipality densities
  { ### Creating a carto-presentable map
    require(rgdal); require(sp); require(geojson)
    
    # wisc.spat.r <-
    #   readOGR(
    #     dsn = "Prepped files/wisc.present.rrig.geojson",
    #     layer = "OGRGeoJSON",
    #     disambiguateFIDs = T,
    #     stringsAsFactors = F
    #   )
    
    # wisc.spat.r.cut = wisc.spat.r
    # wisc.spat.r.cut@data[7:length(wisc.spat.r.cut@data)] = NULL
    # wisc.spat.r.cut@data$MCD_NAME = toupper(wisc.spat.r.cut@data$MCD_NAME)
    # 
    # wisc.join = wisc.spat.r.cut
    # 
    # wisc.join@data = plyr::join(wisc.join@data, model.r.repunit.df, by = "muni_county", match = "first")
    # colnames(wisc.join@data) = make.unique(colnames(wisc.join@data))
    # wisc.join@data$county_machine_vendor_dealer = as.character( wisc.join@data$county_machine_vendor_dealer)
    # wisc.join@data$demwinner = as.character( wisc.join@data$demwinner)
    # wisc.join@data$demwinner2012 = as.character( wisc.join@data$demwinner2012)
    # 
    # wisc.join@data$county = NULL
    # wisc.join@data$MCD_FIPS.1 = NULL
    # 
    # wisc.join@data[wisc.join@data == Inf] = NA
    # wisc.join@data[wisc.join@data == -Inf] = NA
    # 
    # wisc.join@data[is.na(wisc.join@data)] = NA
    # 
    # # unique(wisc.join@data$muni_county=="NA")
    # # wisc.join = wisc.join[-wisc.join@data$muni_county=="NA",]
    # # wisc.join = subset(wisc.join,wisc.join@data$muni_county!="NA")
    # 
    # wisc.join.df = as.data.frame(wisc.join)
    # # str(wisc.join.df)
    # 
    # # wisc.join@data = clean_names(wisc.join@data)
    # # require(geojson)
    # 
    # writeOGR(
    #   wisc.join,
    #   "Prepped files/wisc.present.rrig.repunit.geojson",
    #   layer = "wisc.join",
    #   driver = "GeoJSON"
    # )
    
    # geo_write(wisc.join, file = "~/Prepped files/wisc.present.rrig.geojson")
    
    # writeOGR(
    #   wisc.join,
    #   "Prepped files/wisc.present.rrig.shp",
    #   layer = "wisc.join",
    #   driver = "ESRI Shapefile"
    # )
    
  } # Creating a carto-presentable map
  
  write.csv(model.r.repunit.df,"modeldfward.repunit.r.csv")
  
  