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

# Loading 2016 results ----------------------------------------------------
# Loading in 2016 results
# Following guide here:
# https://www.r-bloggers.com/using-rvest-to-scrape-an-html-table/

# 2016 results from here: http://wisconsinvote.org/results/President%20-%20General/county-results

url <-
  "http://wisconsinvote.org/results/President%20-%20General/county-results"
voting <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@class="views-table cols-3"]') %>%
  html_table()

lapply(voting, '[[', 'Candidates')

voting.county <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@class="views-table cols-3"]/caption') %>%
  html_text()


voting.county = gsub("County: ", "", voting.county)
as.character(voting.county)

votinglist.test <-
  mapply(cbind,
         voting,
         "voting.county" = as.character(voting.county),
         SIMPLIFY = F)
voting = votinglist.test

voting.df.2016 = data.frame(1:504)
voting.df.2016$county = unlist(lapply(voting, '[[', 'voting.county'))
voting.df.2016$cand = unlist(lapply(voting, '[[', 'Candidates'))
voting.df.2016$votes.rec = unlist(lapply(voting, '[[', 'Votes Received'))
voting.df.2016$votes.perc = unlist(lapply(voting, '[[', 'Vote Percentage (In County)'))
voting.df.2016$votes.perc = as.numeric(gsub("%", "", voting.df.2016$votes.perc))
voting.df.2016$X1.504 = NULL
voting.df.2016$votes.rec = as.integer(gsub(",", "", voting.df.2016$votes.rec))

voting.df.2016$cand.group = NA
voting.df.2016$cand.group[grep("Hillary",voting.df.2016$cand)] = "Clinton"
voting.df.2016$cand.group[grep("Donald",voting.df.2016$cand)] = "Trump"
voting.df.2016$cand.group[grep("Darrell",voting.df.2016$cand)] = "Other"
voting.df.2016$cand.group[grep("Rocky",voting.df.2016$cand)] = "Other"
voting.df.2016$cand.group[grep("Gary",voting.df.2016$cand)] = "Other"
voting.df.2016$cand.group[grep("Monica",voting.df.2016$cand)] = "Other"
voting.df.2016$cand.group[grep("Jill",voting.df.2016$cand)] = "Other"

# Grouping by other
# Looking at county data
voting.df.2016.votecand.groups = group_by(voting.df.2016,county,cand.group)
voting.df.2016.votecand = dplyr::summarise(voting.df.2016.votecand.groups,
              votes.rec = sum(votes.rec),
              votes.perc = sum(votes.perc)
)

voting.df.2016.votecand$cand.group2 = voting.df.2016.votecand$cand.group

str(voting.df.2016$votecand)
voting.df.2016.test = spread(voting.df.2016.votecand,
                             key = cand.group, value = votes.rec)
colnames(voting.df.2016.test)[4:6] = c("dem.votes","oth.votes","rep.votes")
voting.df.2016.test = spread(voting.df.2016.test,
       key = cand.group2, value = votes.perc)
colnames(voting.df.2016.test)[5:7] = c("dem.votes.perc","oth.votes.perc","rep.votes.perc")

voting.df.2016.votecand.groups = group_by(voting.df.2016.test,county)
voting.df.2016.votecand = dplyr::summarise(voting.df.2016.votecand.groups,
                                           dem.votes = sum(na.rm=T,dem.votes),
                                           dem.votes.perc = sum(na.rm=T,dem.votes.perc),
                                           rep.votes = sum(na.rm=T,rep.votes),
                                           rep.votes.perc = sum(na.rm=T,rep.votes.perc),
                                           oth.votes = sum(na.rm=T,oth.votes),
                                           oth.votes.perc = sum(na.rm=T,oth.votes.perc)
)

voting.df.2016.votecand$turnout = with(voting.df.2016.votecand, dem.votes + rep.votes + oth.votes)

### 2016 results ###
# Overall results vs polls
election.2016.group = group_by(voting.df.2016, cand)
election.2016.grouped = dplyr::summarise(election.2016.group,
                                             votes.rec = sum(votes.rec))

election.2016.grouped$votes.perc = (election.2016.grouped$votes.rec / sum(election.2016.grouped$votes.rec)) * 100

election.2016.grouped$ordered.cands= c(5,1,3,2,4,6,7)
colours = c("grey", "lightcoral", "dark grey", "light blue", "light green","grey","grey")
election.2016.grouped = election.2016.grouped[order(election.2016.grouped$ordered.cands), ]
election.2016.grouped$cand.group = c("Donald J. Trump", "Hillary Clinton",rep("Other",5))

election.2016.grouped$votes.perc.group = with(election.2016.grouped, c(votes.perc[1],votes.perc[2], rep(sum(votes.perc[3:7]), 5)))
election.2016.grouped$votes.rec.group = with(election.2016.grouped, c(votes.rec[1],votes.rec[2], rep(sum(votes.rec[3:7]), 5)))

rm(voting.df.2016.test,voting.df.2016.votecand.groups)


# Ward level data 2012 and 2016 original ----------------------------------------------------
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

  ward.2016.look = with(ward.2016, data.frame(county_name, municipality_name,
                                              reporting_unit, muni_county))
  colnames(ward.2016.look) = c("county","municipality","reporting_unit",
                               "muni_county")

  ward.digits.2016 = ward.2016
  ward.digits.2016 = subset(ward.digits.2016, original_or_recount == "Original")

  # No differences in digits ending in 0 or 5 between Trump and Clinton

  ward.digits.2016$trump.digit = substrRight(ward.digits.2016$donald_j_trump, 1)
  ward.digits.2016$trump.digit = as.integer(ward.digits.2016$trump.digit)
  trump.dig.table = table(ward.digits.2016$trump.digit) / length(ward.digits.2016$trump.digit)

  ward.digits.2016$clinton.digit = substrRight(ward.digits.2016$hillary_clinton, 1)
  ward.digits.2016$clinton.digit = as.integer(ward.digits.2016$clinton.digit)
  clinton.dig.table = table(ward.digits.2016$clinton.digit) / length(ward.digits.2016$clinton.digit)

  # No difference in digits ending in 0 or 5 for Stein

  ward.digits.2016.stein = subset(ward.digits.2016, jill_stein > 9)
  ward.digits.2016.stein$stein.digit = substrRight(ward.digits.2016.stein$jill_stein, 1)
  ward.digits.2016.stein$stein.digit = as.integer(ward.digits.2016.stein$stein.digit)
  stein.dig.table = table(ward.digits.2016.stein$stein.digit) / length(ward.digits.2016.stein$stein.digit)
  table(ward.digits.2016.stein$stein.digit)

  ward.digits.2016$stein.digit = substrRight(ward.digits.2016$jill_stein, 1)
  ward.digits.2016$stein.digit = as.integer(ward.digits.2016$stein.digit)
  stein.dig.table = table(ward.digits.2016$stein.digit) / length(ward.digits.2016$stein.digit)
  table(ward.digits.2016$stein.digit)


  ward.2016.v = ward.2016
  ward.2016.v$county_name = NULL
  ward.2016.v$municipality_name = NULL
  ward.2016.v$total_votes = NULL
  ward.2016.v$reporting_unit = NULL

  ward.2016.r = subset(ward.2016.v, original_or_recount == "Recount")
  ward.2016.o = subset(ward.2016.v, original_or_recount == "Original")

  ward.2016.r$original_or_recount = NULL
  ward.2016.o$original_or_recount = NULL

  ward.2016.o.l = gather(ward.2016.o, muni_county);
  colnames(ward.2016.o.l) = c("muni_county", "cand", "votes.rec")

  ward.2016.o.l$cand.group = NA
  ward.2016.o.l$cand.group[grep("hillary",ward.2016.o.l$cand)] = "democrat"
  ward.2016.o.l$cand.group[grep("donald",ward.2016.o.l$cand)] = "republican"
  ward.2016.o.l$cand.group[grep("darrell",ward.2016.o.l$cand)] = "other"
  ward.2016.o.l$cand.group[grep("rocky",ward.2016.o.l$cand)] = "other"
  ward.2016.o.l$cand.group[grep("gary",ward.2016.o.l$cand)] = "other"
  ward.2016.o.l$cand.group[grep("monica",ward.2016.o.l$cand)] = "other"
  ward.2016.o.l$cand.group[grep("jill",ward.2016.o.l$cand)] = "other"

  # There were a bunch of write-in names for the ward level results
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
  ward.2016.o.l = join(ward.2016.o.l, ward.2016.look, by = "muni_county", match = "first")

  ward.2016.o.l$votes.rec = as.numeric(as.integer(ward.2016.o.l$votes.rec))
  table(is.na(ward.2016.o.l$votes.rec))

  tot.votes.look.group = group_by(ward.2016.o.l,muni_county)
  tot.votes.look = dplyr::summarise(tot.votes.look.group,
                                    votes.tot = sum(votes.rec)
  )
  ward.2016.o.l = join(ward.2016.o.l,tot.votes.look,by="muni_county",match="first")
  ward.2016.o.l$votes.perc = (ward.2016.o.l$votes.rec / ward.2016.o.l$votes.tot) * 100

  ward.2016.o.l$muni_county = paste(ward.2016.o.l$municipality,
                                    ward.2016.o.l$county)

  ward.2016.o.mun.group = group_by(ward.2016.o.l,municipality,muni_county,county,cand.group)
  ward.2016.o.mun = dplyr::summarise(ward.2016.o.mun.group,
                                     votes.rec2 = sum(votes.rec),
                                     votes.tot2 = mean(votes.tot)
  )
  colnames(ward.2016.o.mun)[5] = "votes.rec"
  colnames(ward.2016.o.mun)[6] = "votes.tot"

  ward.2016.o.mun$votes.perc = (ward.2016.o.mun$votes.rec / ward.2016.o.mun$votes.tot) * 100
} # Preparing basic data

## Now bringing in the registered voter data. need to move up to municipality level
ward.2016.regvote = read.csv(
  "registeredvotersbywards_xlsx_19539MODnov16.csv",
  stringsAsFactors = F,
  header = T,
  strip.white = T
)
{
  ward.2016.regvote = clean_names(ward.2016.regvote)
  ward.2016.regvote$ward = toupper(ward.2016.regvote$ward)
  ward.2016.regvote$county = toupper(ward.2016.regvote$county)
  ward.2016.regvote$muni = toupper(ward.2016.regvote$muni)

  ward.2016.regvote$county = gsub(" COUNTY","",ward.2016.regvote$county)
  ward.2016.regvote$muni = gsub(" -.*","",ward.2016.regvote$muni)
  ward.2016.regvote$muni_county = paste(ward.2016.regvote$muni, ward.2016.regvote$county)

  colnames(ward.2016.regvote)[2] = "municipality"

  ### Limiting to municipality level
  ward.2016.regvote.mun.group = group_by(ward.2016.regvote,muni_county,municipality,county)
  ward.2016.regvote.mun = dplyr::summarise(ward.2016.regvote.mun.group,
                                           voter_count2 = sum(voter_count)
  )
  colnames(ward.2016.regvote.mun)[4] = "voters.tot"



  ###
  # Seeing what is differnt according to muni county level
  ###
  voter.reg.mun = data.frame(unique(ward.2016.regvote.mun$muni_county))
  votes.mun = data.frame(unique(ward.2016.o.l$muni_county))
  colnames(voter.reg.mun) = "voter.reg.mun"
  colnames(votes.mun) = "votes.mun"

  # Ugh, I need to change the name of these muni_counties to make sure I have a full match
  setdiff(unique(voter.reg.mun$voter.reg.mun), unique(votes.mun$votes.mun))
  setdiff(unique(votes.mun$votes.mun), unique(voter.reg.mun$voter.reg.mun))

  differences = setdiff(unique(votes.mun$votes.mun),unique(voter.reg.mun$voter.reg.mun))
  voter.reg.diffs = subset(ward.2016.o.l, muni_county %in% differences)

  voter.reg.diffs.join = with(voter.reg.diffs, data.frame(municipality, county, muni_county, reporting_unit))
  voter.reg.diffs.join = voter.reg.diffs.join[!duplicated(voter.reg.diffs.join),]

  ward.2016.regvote$ward = gsub(" -","",ward.2016.regvote$ward)

  ###
  # Seeing what is different according to ward level
  ###
  matched.wards = ward.2016.o.l[match(ward.2016.regvote$ward, ward.2016.o.l$reporting_unit),]
  matched.wards = matched.wards[!is.na(matched.wards),]


  ###
  # Municipalities with exceptions
  ##
{
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF WRIGHTSTOWN WARD 4"] = "OUTAGAMIE"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 5"] = "ADAMS"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF NEW AUBURN WARD 2"] = "BARRON"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF NEW AUBURN WARD 3"] = "BARRON"

  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 1"] = "MANITOWOC"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 2"] = "MANITOWOC"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 3"] = "MANITOWOC"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 4"] = "MANITOWOC"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 5"] = "MANITOWOC"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 6"] = "MANITOWOC"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 8"] = "MANITOWOC"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 7"] = "CALUMET"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 31"] = "WINNEBAGO"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MENASHA WARD 16"] = "CALUMET"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MENASHA WARD 17"] = "CALUMET"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF EAU CLAIRE WARD 16"] = "CHIPPEWA"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF UNITY WARD 2"] = "CLARK"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF STANLEY WARD 5"] = "CLARK"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF RANDOLPH WARD 3"] = "COLUMBIA"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF DE SOTO WARD 2"] = "CRAWFORD"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF EDGERTON WARD 7"] = "DANE"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 1"] = "DODGE"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 2"] = "DODGE"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 3"] = "DODGE"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 4"] = "DODGE"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 5"] = "DODGE"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 6"] = "DODGE"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 7"] = "DODGE"

  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WAUPUN WARD 9"] = "FOND DU LAC"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WAUPUN WARD 10"] = "FOND DU LAC"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WAUPUN WARD 11"] = "FOND DU LAC"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WAUPUN WARD 12"] = "FOND DU LAC"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF BELLEVILLE WARD 3"] = "GREEN"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF BROOKLYN WARD 2"] = "GREEN"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF BLANCHARDVILLE WARD 2"] = "IOWA"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF LIVINGSTON WARD 2"] = "IOWA"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF MONTFORT WARD 2"] = "IOWA"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF MUSCODA WARD 3"] = "IOWA"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF CAMBRIDGE WARD 1"] = "JEFFERSON"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WHITEWATER WARD 10"] = "JEFFERSON"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WHITEWATER WARD 11"] = "JEFFERSON"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WHITEWATER WARD 12"] = "JEFFERSON"


  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF HAZEL GREEN WARD 3"] = "LAFAYETTE"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF CUBA CITY WARD 5"] = "LAFAYETTE"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF BIRNAMWOOD WARD 2"] = "MARATHON"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF DORCHESTER WARD 2"] = "MARATHON"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF ABBOTSFORD WARD 1"] = "MARATHON"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF COLBY WARD 1"] = "MARATHON"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARSHFIELD WARD 12"] = "MARATHON"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARSHFIELD WARD 20"] = "MARATHON"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARSHFIELD WARD 21"] = "MARATHON"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARSHFIELD WARD 24"] = "MARATHON"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF NEW LONDON WARD 1"] = "OUTAGAMIE"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF NEW LONDON WARD 2"] = "OUTAGAMIE"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF BAYSIDE WARD 6"] = "OZAUKEE"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF NEWBURG WARD 3"] = "OZAUKEE"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF TURTLE LAKE WARD 2A"] = "POLK"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF MILLADORE WARD 2"] = "PORTAGE"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF BRODHEAD WARD 7"] = "ROCK"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF BRODHEAD WARD 8"] = "ROCK"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF SPRING VALLEY WARD 3"] = "ST. CROIX"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF RIVER FALLS WARD 1"] = "ST. CROIX"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF RIVER FALLS WARD 2"] = "ST. CROIX"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF RIVER FALLS WARD 3"] = "ST. CROIX"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF RIVER FALLS WARD 4"] = "ST. CROIX"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF CAZENOVIA WARD 2"] = "SAUK"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 4"] = "SAUK"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 8"] = "SAUK"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 10"] = "SAUK"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 5"] = "ADAMS"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 9"] = "ADAMS"

  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF PULASKI WARD 4"] = "SHAWANO"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARION WARD 4"] = "SHAWANO"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARION WARD 5"] = "SHAWANO"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARION WARD 6"] = "SHAWANO"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF VIOLA WARD 1"] = "VERNON"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF MUKWONAGO WARD 11"] = "WALWORTH"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF BURLINGTON WARD 9"] = "WALWORTH"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF BURLINGTON WARD 10"] = "WALWORTH"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF BERLIN WARD 7"] = "WAUSHARA"

  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF MUKWONAGO WARD 11"] = "WALWORTH"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF WRIGHTSTOWN WARD 1"] = "BROWN"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF WRIGHTSTOWN WARD 2"] = "BROWN"
  ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF WRIGHTSTOWN WARD 3"] = "BROWN"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 12"] = "CALUMET"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 13"] = "CALUMET"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 14"] = "CALUMET"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 26"] = "CALUMET"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 44"] = "CALUMET"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 45"] = "CALUMET"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 46"] = "CALUMET"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 47"] = "CALUMET"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 31"] = "WINNEBAGO"
  ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 32"] = "WINNEBAGO"
}
  ward.2016.regvote$muni_county = paste(ward.2016.regvote$municipality, ward.2016.regvote$county)

  setdiff(unique(votes.mun$votes.mun), unique( ward.2016.regvote$muni_county))

  ## Joining the reg.vote summarised municipalities back onto the voting municipalities
  ward.2016.regvote.join = with(ward.2016.regvote, data.frame(municipality,muni_county))
  colnames(ward.2016.regvote.join)[2] = "muni_county_regvote_agreed"
  ward.2016.regvote.join = ward.2016.regvote.join[!duplicated(ward.2016.regvote.join$muni_county_regvote),]
  #
  # voter.reg.diffs.join = join(voter.reg.diffs.join,ward.2016.regvote.join,by = "municipality")
  #
  # back.to.votes.df.join = with(voter.reg.diffs.join, data.frame(muni_county,muni_county_regvote_agreed))
  #
  # ward.2016.o.l = join(ward.2016.o.l, back.to.votes.df.join, by = "muni_county")
  #
  # unique(ward.2016.o.l$muni_county_regvote_agreed)
  #
  # ward.2016.o.l$muni_county_regvote_agreed = as.character(ward.2016.o.l$muni_county_regvote_agre

  ward.2016.o.l$muni_county_regvote_agreed = as.character(ward.2016.o.l$muni_county)
  ward.2016.o.l$muni_county_regvote_agreed = ifelse(is.na(ward.2016.o.l$muni_county_regvote_agreed),
                                                    ward.2016.o.l$muni_county,ward.2016.o.l$muni_county_regvote_agreed)
  ## Now renaming the regvote column for the final join
  colnames(ward.2016.regvote)[6] = "muni_county_regvote_agreed"

  # Trying to match by muni_county level
  ## Need to sum up the votes by county
  ward.2016.regvote.votes.group = group_by(ward.2016.regvote,muni_county_regvote_agreed)
  ward.2016.regvote.votes.g = dplyr::summarise(ward.2016.regvote.votes.group,
                                               voter_count2 = sum(voter_count)
  )
  colnames(ward.2016.regvote.votes.g)[2] = "voters.tot"

  # Trying to match by ward level
  ward.2016.o.l = join(ward.2016.o.l, ward.2016.regvote.votes.g, by = "muni_county_regvote_agreed")
  colnames(ward.2016.o.l)[11] = "voters.tot"

  setdiff(ward.2016.regvote.votes.g$muni_county_regvote_agreed,
          ward.2016.o.l$muni_county_regvote_agreed)

  setdiff(ward.2016.o.l$muni_county_regvote_agreed,
          ward.2016.regvote.votes.g$muni_county_regvote_agreed)

  ward.2016.o.l$turnout = with(ward.2016.o.l, votes.tot / voters.tot)

  ### Joining this back to ward.2016.o.mun
  ward.2016.o.l.join = with(ward.2016.o.l,
                            data.frame(
                              muni_county,voters.tot
                            ))
  colnames(ward.2016.o.l.join)[1] = "muni_county"
  ward.2016.o.l.join = ward.2016.o.l.join[!duplicated(ward.2016.o.l.join$muni_county),]

  ward.2016.o.mun = join(ward.2016.o.mun, ward.2016.o.l.join, by ="muni_county", match = "first")

} # Adding in registered vote data

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

  ward.12.look = with(ward.12, data.frame(county, municipality_name,
                                          reporting_unit, muni_county))
  colnames(ward.12.look) = c("county","municipality","reporting_unit",
                             "muni_county")

  ward.12.v = ward.12
  ward.12.v$county = NULL
  ward.12.v$municipality_name = NULL
  ward.12.v$total_votes = NULL
  ward.12.v$reporting_unit = NULL
  ward.12.v$congressional = NULL
  ward.12.v$senatedistrict = NULL
  ward.12.v$assemblydistrict = NULL

  ward.12.l = gather(ward.12.v, muni_county);
  colnames(ward.12.l) = c("muni_county", "cand", "votes.rec")

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

  # Joining back county and municipality names
  ward.12.l = join(ward.12.l, ward.12.look, by = "muni_county", match = "first")

  tot.votes.look.group = group_by(ward.12.l,muni_county)
  tot.votes.look = dplyr::summarise(tot.votes.look.group,
                                    votes.tot = sum(votes.rec)
  )

  ward.12.l = join(ward.12.l,tot.votes.look,by="muni_county",match="first")
  ward.12.l$votes.perc = (ward.12.l$votes.rec / ward.12.l$votes.tot) * 100

  ward.12.l$muni_county = paste(ward.12.l$municipality, ward.12.l$county)

  ward.12.mun.group = group_by(ward.12.l,municipality,muni_county,county,cand,cand.group)
  ward.12.mun = dplyr::summarise(ward.12.mun.group,
                                 votes.rec2 = sum(votes.rec),
                                 votes.tot2 = mean(votes.tot)
  )
  colnames(ward.12.mun)[6] = "votes.rec"
  colnames(ward.12.mun)[7] = "votes.tot"

  ward.12.mun$votes.perc = (ward.12.mun$votes.rec / ward.12.mun$votes.tot) * 100
}
# Now joining the 2012 data back into the 2016 data
{
  # Now joining the 2012 data back into the 2016 data
  setdiff(unique(ward.12.mun$muni_county), unique(ward.2016.o.mun$muni_county))

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

  setdiff(unique(ward.2016.o.mun$muni_county), unique(ward.12.mun$muni_county))

  # Village of Harrison Calumet did not exist in 2012, was split off from the town in 2013.
  # It is impossible to know the boundaries exactly.
  # Therefore the swing from 2012 to 2016 cannot be accurately calculated.

  # Same of the town / village of Somers, Kenosha. The town / village is completely mixed and messed up

  # the town of Menasha was retained only for wards (8-10, 11-13). The rest (1-7) became the village of
  # Fox Crossing, Winnebago

  differences = setdiff(unique(ward.2016.o.mun$muni_county),unique(ward.12.mun$muni_county))
  vote.2012.diffs = subset(ward.2016.o.mun, muni_county %in% differences)

  vote.2012.diffs.join = with(vote.2012.diffs, data.frame(municipality, county, muni_county))
  vote.2012.diffs.join = vote.2012.diffs.join[!duplicated(vote.2012.diffs.join),]

  ## Joining the reg.vote summarised municipalities back onto the voting municipalities
  ward.12.mun.join = with(ward.12.mun, data.frame(municipality,muni_county))
  colnames(ward.12.mun.join)[2] = "muni_county_2012_agreed"
  ward.12.mun.join = ward.12.mun.join[!duplicated(ward.12.mun.join$muni_county_2012),]

  vote.2012.diffs.join = join(vote.2012.diffs.join,ward.12.mun.join,by = "municipality")

  back.to.votes.df.join = with(vote.2012.diffs.join, data.frame(muni_county,muni_county_2012_agreed))

  ward.2016.o.mun = join(ward.2016.o.mun, back.to.votes.df.join, by = "muni_county")

  ward.2016.o.mun$muni_county_2012_agreed = as.character(ward.2016.o.mun$muni_county_2012_agreed)

  ward.2016.o.mun$muni_county_2012_agreed = ifelse(is.na(ward.2016.o.mun$muni_county_2012_agreed),
                                                   ward.2016.o.mun$muni_county,ward.2016.o.mun$muni_county_2012_agreed)

  ## Now renaming the 2012 column for the final join
  # colnames(ward.12.mun)[2] = "muni_county_2012_agreed"

  ## Need to sum up the votes by county in 2012
  ward.12.mun.votes.group = group_by(ward.12.mun,cand.group,muni_county)
  ward.12.mun.votes.g = dplyr::summarise(ward.12.mun.votes.group,
                                         votes.rec2012 = sum(votes.rec),
                                         votes.tot2012 = mean(votes.tot)

  )

  # Melting the 2012 df to put the candidates on top
  ward.12.mun.votes.g.melt = melt(ward.12.mun.votes.g)
  ward.12.mun.votes.g.cast = dcast(ward.12.mun.votes.g.melt,
                                   muni_county ~ cand.group + variable, sum
  )
  ward.12.mun.votes.g.cast$democrat_votes.tot2012 = NULL
  ward.12.mun.votes.g.cast$republican_votes.tot2012 = NULL

  ward.12.mun.votes.g.cast.mean = dcast(ward.12.mun.votes.g.melt,
                                        muni_county ~ cand.group + variable, mean
  )
  ward.12.mun.votes.g.cast$other_votes.tot2012 = ward.12.mun.votes.g.cast.mean$other_votes.tot2012
  colnames(ward.12.mun.votes.g.cast)[grepl("votes.tot",colnames(ward.12.mun.votes.g.cast))] =
    "votes.tot2012"

  # Also need to melt the 2016 mun df by cand.group
  ward.2016.o.mun.votes.g.melt = melt(ward.2016.o.mun)
  ward.2016.o.mun.votes.g.cast = dcast(ward.2016.o.mun.votes.g.melt,
                                       muni_county ~ cand.group + variable, sum
  )

  ward.2016.o.mun.votes.g.cast$democrat_votes.tot = NULL
  ward.2016.o.mun.votes.g.cast$republican_votes.tot = NULL
  ward.2016.o.mun.votes.g.cast$democrat_voters.tot = NULL
  ward.2016.o.mun.votes.g.cast$republican_voters.tot = NULL

  ward.2016.o.mun.votes.g.cast.mean = dcast(ward.2016.o.mun.votes.g.melt,
                                            muni_county ~ cand.group + variable, mean
  )
  ward.2016.o.mun.votes.g.cast$other_votes.tot = ward.2016.o.mun.votes.g.cast.mean$other_votes.tot
  ward.2016.o.mun.votes.g.cast$other_voters.tot = ward.2016.o.mun.votes.g.cast.mean$other_voters.tot
  colnames(ward.2016.o.mun.votes.g.cast)[grepl("votes.tot",colnames(ward.2016.o.mun.votes.g.cast))] =
    "votes.tot2016"
  colnames(ward.2016.o.mun.votes.g.cast)[grepl("voters.tot",colnames(ward.2016.o.mun.votes.g.cast))] =
    "voters.tot2016"


  # Finally joining the dfs together

  ward.2016.o.mun.g = join(ward.2016.o.mun.votes.g.cast, ward.12.mun.votes.g.cast, by = "muni_county")

  # 2012 turnout is calculated on the (big?) assumption that the number of registered
  # voters was the same in 2012 as in 2016
  ward.2016.o.mun.g$turnout.2012 =
    with(ward.2016.o.mun.g, votes.tot2012 / voters.tot2016) * 100

  ward.2016.o.mun.g$dem.perc2012 =
    with(ward.2016.o.mun.g,
         democrat_votes.rec2012 / votes.tot2012) * 100
  ward.2016.o.mun.g$rep.perc2012 =
    with(ward.2016.o.mun.g,
         republican_votes.rec2012 / votes.tot2012) * 100
  ward.2016.o.mun.g$oth.perc2012 =
    with(ward.2016.o.mun.g,
         other_votes.rec2012 / votes.tot2012) * 100

  ward.2016.o.mun.g$dem.change.num =
    with(ward.2016.o.mun.g,
         democrat_votes.rec - democrat_votes.rec2012)
  ward.2016.o.mun.g$rep.change.num =
    with(ward.2016.o.mun.g,
         republican_votes.rec - republican_votes.rec2012)
  ward.2016.o.mun.g$oth.change.num =
    with(ward.2016.o.mun.g,
         other_votes.rec - other_votes.rec2012)

  ward.2016.o.mun.g$dem.change.perc =
    with(ward.2016.o.mun.g,
         democrat_votes.perc -  dem.perc2012)
  ward.2016.o.mun.g$rep.change.perc =
    with(ward.2016.o.mun.g,
         republican_votes.perc - rep.perc2012)
  ward.2016.o.mun.g$oth.change.perc =
    with(ward.2016.o.mun.g,
         other_votes.perc - oth.perc2012)

  ward.2016.o.mun.g$turnout.2016.perc =
    with(ward.2016.o.mun.g,
         votes.tot2016 / voters.tot2016) * 100


  ward.2016.o.mun.g$turnout.change.perc =
    with(ward.2016.o.mun.g,
         turnout.2016.perc - turnout.2012)


  ward.2016.2012.o = ward.2016.o.mun.g

  ward.2016.2012.o$demwinner = NA
  ward.2016.2012.o$demwinner = ifelse(ward.2016.2012.o$democrat_votes.perc >
                                            ward.2016.2012.o$republican_votes.perc &
                                            !is.na(ward.2016.2012.o$democrat_votes.perc), 1,0)
  ward.2016.2012.o$demwinner = as.integer(ward.2016.2012.o$demwinner)

  ward.2016.2012.o$demwinner2012 = NA
  ward.2016.2012.o$demwinner2012 = ifelse(ward.2016.2012.o$dem.perc2012 >
                                        ward.2016.2012.o$rep.perc2012 &
                                        !is.na(ward.2016.2012.o$dem.perc2012), 1,0)
  ward.2016.2012.o$demwinner2012 = as.integer(ward.2016.2012.o$demwinner2012)
}
# Loading 2012 ward vote data

# Rejoining the county data to the ward 2016 2012 data
{
  ward.2016.2012.o = join(ward.2016.2012.o, ward.2016.look, by = "muni_county", match = "first")
}

# Loading voting machine data original (Wisconsin official) ---------------------------------------------
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

vot.equip.county.grouped = group_by(vot.equip, county)
vot.equip.county = dplyr::summarise(
  vot.equip.county.grouped,
  use.machines.prop = sum(use.machines, na.rm = TRUE) /
    length(use.machines)
)

vot.equip.withmachines = subset(vot.equip, use.machines > 0)

vot.equip.county.grouped.machines = group_by(vot.equip.withmachines, county)
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
vot.equip.county = join(vot.equip.county,
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
vot.equip.withmachines$muni_county_regvote_agreed =
  gsub(" -.*", "",vot.equip.withmachines$municipality)

vot.equip.withmachines$muni_county_regvote_agreed =
  paste(vot.equip.withmachines$muni_county_regvote_agreed, vot.equip.withmachines$county)

vot.equip.join = with(vot.equip.withmachines,
                      data.frame(muni_county_regvote_agreed,
                                 use.machines,
                                 machine.vendor.dealer.spec,
                                 machine.vendor.dealer.model,
                                 accessible.vendor.dealer.model))

setdiff(vot.equip.withmachines$muni_county_regvote_agreed,
        ward.2016.o.l$muni_county_regvote_agreed)

# [1] "VILLAGE OF WRIGHTSTOWN BROWN" "TOWN OF WINDSOR DANE" "TOWN OF MAINE MARATHON"

setdiff(ward.2016.o.l$muni_county_regvote_agreed,
        vot.equip.withmachines$muni_county_regvote_agreed)

ward.2016.o.l$muni_county_machines_agreed = ward.2016.o.l$muni_county_regvote_agreed

ward.2016.o.l$muni_county_machines_agreed[grep("VILLAGE OF WRIGHTSTOWN",
                                               ward.2016.o.l$muni_county_machines_agreed)] = "VILLAGE OF WRIGHTSTOWN BROWN"
ward.2016.o.l$muni_county_machines_agreed[grep("TOWN OF MAINE",
                                               ward.2016.o.l$muni_county_machines_agreed)] = "TOWN OF MAINE MARATHON"
colnames(vot.equip.join)[1] = "muni_county_machines_agreed"

setdiff(vot.equip.join$muni_county_machines_agreed,
        ward.2016.o.l$muni_county_machines_agreed)

ward.2016.o.l = join(ward.2016.o.l,vot.equip.join, by = "muni_county_machines_agreed")
ward.2016.o.l$use.machines[is.na(ward.2016.o.l$use.machines)] = 0

colnames(ward.2016.o.l)[grep("use.machines",colnames(ward.2016.o.l))] = "county_use_opt_scan"
colnames(ward.2016.o.l)[grep("machine.vendor.dealer.spec",colnames(ward.2016.o.l))] = "county_machine_vendor_dealer"

####
# doing the same for the 2016 2012 data
####
county.machines.join = with(ward.2016.o.l, data.frame(
  muni_county, county_use_opt_scan, county_machine_vendor_dealer, machine.vendor.dealer.model, accessible.vendor.dealer.model
))

ward.2016.2012.test = join(ward.2016.2012.o, county.machines.join, by = "muni_county", match = "first")

ward.2016.2012.o = ward.2016.2012.test
ward.2016.2012.o$county_use_opt_scan[is.na(ward.2016.2012.o$county_use_opt_scan)] = 0

# Loading voting machine data original (Verified voting) ---------------------------
# vot.equip.veri = read.csv(
#   "Verified voting data\\verifier-search-wiMOD.csv",
#   header = TRUE,
#   colClasses = NA,
#   stringsAsFactors = F
# )
#
# colnames(vot.equip.veri)[1] = "MCD_FIPS"
#
# vot.equip.veri = vot.equip.veri[-grep("Wisconsin State",vot.equip.veri$Jurisdiction),]
#
# ## Need to change some municipality names to match my other data frame, exceptions which I found later on
# vot.equip.veri$Division[grep("Land O'Lakes",vot.equip.veri$Division)] = "Land O-Lakes"
# vot.equip.veri$Division[grep("Fontana-on-Geneva Lake",vot.equip.veri$Division)] = "Fontana"
# vot.equip.veri$Division[grep("Poysippi",vot.equip.veri$Division)] = "Poy Sippi"
# vot.equip.veri$Division[grep("St. Lawrence",vot.equip.veri$Division)] = "Saint Lawrence"
#
# vot.equip.veri$habitation = NA
# habitation = NA
# for  (i in 1:length(vot.equip.veri$Jurisdiction)) {
#   habitation[i] = tail(strsplit(vot.equip.veri$Jurisdiction[i],split=" ")[[1]],1)
# }
# vot.equip.veri$habitation = habitation
#
# vot.equip.veri$municipality = NA
# vot.equip.veri$municipality = paste(vot.equip.veri$habitation, "of", vot.equip.veri$Division)
# vot.equip.veri$municipality = toupper(vot.equip.veri$municipality)
#
# length(unique(vot.equip.veri$municipality))
#
# ## Need to change some municipality names to match my other data frame, exceptions which I found later on
# vot.equip.veri$municipality[grep("MOUNT STERLING",vot.equip.veri$municipality)] = "VILLAGE OF MT. STERLING"
# vot.equip.veri$municipality[grep("Richland City",vot.equip.veri$Jurisdiction)] = "CITY OF RICHLAND CENTER"
#
# county = NA
# for  (i in 1:length(vot.equip.veri$Jurisdiction)) {
#   county[i] = tail(strsplit(vot.equip.veri$Jurisdiction[i],split=" ")[[1]],2)
# }
# vot.equip.veri$county = county
# vot.equip.veri$county = toupper(vot.equip.veri$county)
#
# # need to change some counties because they are multiple-word counties
# vot.equip.veri$county[grep("CLAIRE",vot.equip.veri$county)] = "EAU CLAIRE"
# vot.equip.veri$county[grep("LAC",vot.equip.veri$county)] = "FOND DU LAC"
# vot.equip.veri$county[grep("CROIX",vot.equip.veri$county)] = "ST. CROIX"
# vot.equip.veri$county[grep("CROSSE",vot.equip.veri$county)] = "LA CROSSE"
# vot.equip.veri$county[grep("LAKE",vot.equip.veri$county)] = "GREEN LAKE"
#
# length(unique(vot.equip.veri$county))
#
# vot.equip.veri$muni_county = paste(vot.equip.veri$municipality, vot.equip.veri$county)
#
# # Dealing with some more exceptions
# orig = "TOWN OF MAINE MARATHON"
# vot.equip.veri$Jurisdiction[grep(orig,vot.equip.veri$muni_county)] = "MAINE VILLAGE"
# vot.equip.veri$municipality[grep(orig,vot.equip.veri$muni_county)] = "VILLAGE OF MAINE"
# vot.equip.veri$muni_county[grep(orig,vot.equip.veri$muni_county)] = "VILLAGE OF MAINE MARATHON"
#
# orig = "VILLAGE OF LA VALLE SAUK"
# vot.equip.veri$Jurisdiction[grep(orig,vot.equip.veri$muni_county)] = "LAVALLE VILLAGE"
# vot.equip.veri$municipality[grep(orig,vot.equip.veri$muni_county)] = "VILLAGE OF LAVALLE"
# vot.equip.veri$muni_county[grep(orig,vot.equip.veri$muni_county)] = "VILLAGE OF LAVALLE SAUK"
#
# orig = "TOWN OF BURLINGTON WALWORTH"
# vot.equip.veri$Jurisdiction[grep(orig,vot.equip.veri$muni_county)] = "RACINE TOWN"
# vot.equip.veri$municipality[grep(orig,vot.equip.veri$muni_county)] = "TOWN OF BURLINGTON"
# vot.equip.veri$muni_county[grep(orig,vot.equip.veri$muni_county)] = "TOWN OF BURLINGTON RACINE"
#
# # The town of Menasha has become the VILLAGE OF FOX CROSSING WINNEBAGO completely; but not according to the voting register
# # (some parts are in both). So, I will not change the name in the voting equipment df.
# orig = "TOWN OF MENASHA WINNEBAGO"
# vot.equip.veri$Jurisdiction[grep(orig,vot.equip.veri$muni_county)] = "WINNEBAGO VILLAGE"
# vot.equip.veri$municipality[grep(orig,vot.equip.veri$muni_county)] = "VILLAGE OF FOX CROSSING"
# vot.equip.veri$muni_county[grep(orig,vot.equip.veri$muni_county)] = "VILLAGE OF FOX CROSSING WINNEBAGO"
#
#
# setdiff(vot.equip.veri$muni_county, ward.2016.2012.o$muni_county)
# # DONE
#
# setdiff(ward.2016.2012.o$muni_county,vot.equip.veri$muni_county)
#
# # Later, it seems that these do not match with the verified voting data:
# # [1] CITY OF NEW LONDON WAUPACA. Maybe it was created from another town in Waupaca? I can't find it at all.
#
# unique(ward.2016.2012.o$muni_county == "CITY OF NEW LONDON WAUPACA")
# unique(vot.equip.veri$muni_county == "CITY OF NEW LONDON WAUPACA")
#
#
# # Ugh, I need to change the name of these muni_counties to make sure I have a full match
# setdiff(unique(vot.equip.veri$muni_county), unique(ward.2016.2012.o$muni_county))
# setdiff(unique(ward.2016.2012.o$muni_county), unique(vot.equip.veri$muni_county))
#
# differences = setdiff(unique(ward.2016.2012.o$muni_county),unique(vot.equip.veri$muni_county))
# veri.mach.diffs = subset(ward.2016.2012.o, muni_county %in% differences)
#
# veri.mach.diffs.join = with(veri.mach.diffs, data.frame(municipality, county, muni_county))
# veri.mach.diffs.join = veri.mach.diffs.join[!duplicated(veri.mach.diffs.join),]
#
# ## Joining the veri.mach summarised municipalities back onto the voting municipalities
# ward.2016.veri.mach.join = with(vot.equip.veri, data.frame(municipality,muni_county))
# colnames(ward.2016.veri.mach.join)[2] = "muni_county_veri_mach_agreed"
# ward.2016.veri.mach.join = ward.2016.veri.mach.join[!duplicated(ward.2016.veri.mach.join$muni_county_veri_mach_agreed),]
#
# veri.mach.diffs.join = join(veri.mach.diffs.join,ward.2016.veri.mach.join,by = "municipality")
# back.to.votes.df.join = with(veri.mach.diffs.join, data.frame(muni_county,muni_county_veri_mach_agreed))
# ward.2016.2012.o = join(ward.2016.2012.o, back.to.votes.df.join, by = "muni_county")
#
# ward.2016.2012.o$muni_county_veri_mach_agreed = as.character(ward.2016.2012.o$muni_county_veri_mach_agreed)
#
# ward.2016.2012.o$muni_county_veri_mach_agreed = ifelse(is.na(ward.2016.2012.o$muni_county_veri_mach_agreed),
#                                                        ward.2016.2012.o$muni_county,ward.2016.2012.o$muni_county_veri_mach_agreed)
# ## Now renaming the equip column for the final join
# colnames(vot.equip.veri)[grep("muni_county",colnames(vot.equip.veri))] = "muni_county_veri_mach_agreed"
#
# # Be careful of this problem with spread (duplicate rows): http://stackoverflow.com/questions/25960394/unexpected-behavior-with-tidyr
# vot.equip.veri$row <- 1:nrow(vot.equip.veri)
#
# # From here: http://stackoverflow.com/questions/35663580/using-tidyr-spread-function-to-create-columns-with-binary-value
# vot.equip.veri.spr =  vot.equip.veri %>% mutate(yesno = 1) %>% distinct %>%
#   spread(Make, yesno, fill = 0,sep = "")
# vot.equip.veri.spr =  vot.equip.veri.spr %>% mutate(yesno = 1) %>% distinct %>%
#   spread(Model, yesno, fill = 0,sep = "")
#
# vot.equip.veri.spr =  vot.equip.veri.spr %>% mutate(yesno = 1) %>% distinct %>%
#   spread(Accessible.Use, yesno, fill = 0,sep = "")
# vot.equip.veri.spr =  vot.equip.veri.spr %>% mutate(yesno = 1) %>% distinct %>%
#   spread(Early.Voting, yesno, fill = 0,sep = "")
# vot.equip.veri.spr =  vot.equip.veri.spr %>% mutate(yesno = 1) %>% distinct %>%
#   spread(Absentee.Ballots, yesno, fill = 0,sep = "")
# vot.equip.veri.spr =  vot.equip.veri.spr %>% mutate(yesno = 1) %>% distinct %>%
#   spread(VVPAT, yesno, fill = 0,sep = "")
#
# vot.equip.veri.spr = clean_names(vot.equip.veri.spr)
#
# # ## In the 'vot.equip.veri', need to make sure there are unique values of 'muni_county_veri_mach_agreed'
# # vot.equip.veri.group = group_by(vot.equip.veri.spr,muni_county_veri_mach_agreed)
# # vot.equip.veri.g = dplyr::summarise(vot.equip.veri.group,
# #                                               precincts = mean(Precincts,na.rm = T),
# #                                              vvf.voter.registration = mean(as.numeric(Total.Registration),na.rm = T)
# # )
#
# # Grouping all by muni county
# vot.equip.veri.prepmelt = vot.equip.veri.spr
# vot.equip.veri.prepmelt$polling_place = NULL;vot.equip.veri.prepmelt$mcd_fips = NULL;
# vot.equip.veri.prepmelt$precincts = NULL;vot.equip.veri.prepmelt$total_registration = NULL;
# vot.equip.veri.prepmelt$row = NULL; vot.equip.veri.prepmelt$row = NULL;
# vot.equip.veri.prepmelt$early_votingyes = NULL;
#
# vot.equip.veri.prepmelt$equipment_type[
#   grep("Optical Scan", vot.equip.veri.prepmelt$equipment_type)] ="OS"
# vot.equip.veri.prepmelt$equipment_type[
#   grep("Ballot Marking Device", vot.equip.veri.prepmelt$equipment_type)] ="BMD"
# vot.equip.veri.prepmelt$equipment_type[
#   grep("DRE", vot.equip.veri.prepmelt$equipment_type)] ="TCH"
# vot.equip.veri.prepmelt$equipment_type[
#   grep("Hand Counted", vot.equip.veri.prepmelt$equipment_type)] ="HAND"
#
# vot.equip.veri.melt = melt(vot.equip.veri.prepmelt)
# vot.equip.veri.cast = dcast(vot.equip.veri.melt, muni_county_veri_mach_agreed ~ equipment_type + variable,
#                             fun = sum,na.rm = T)
# vot.equip.veri.cast = clean_names(vot.equip.veri.cast)
#
# # vot.equip.veri.cast[vot.equip.veri.cast == 0] = 1
# # vot.equip.veri.cast[vot.equip.veri.cast == -Inf] = 0
#
# # Dealing with the other numbers
# vot.equip.veri.spr.base = vot.equip.veri.spr
# vot.equip.veri.spr.base[,13:length(colnames(vot.equip.veri.spr.base))] = NULL
# vot.equip.veri.spr.melt = melt(vot.equip.veri.spr.base)
# vot.equip.veri.cast.base = dcast(vot.equip.veri.spr.melt, muni_county_veri_mach_agreed ~ variable,
#                                  fun = max,na.rm = T)
# vot.equip.veri.cast.base = clean_names(vot.equip.veri.cast.base)
#
# vot.equip.veri.final = join(vot.equip.veri.cast,vot.equip.veri.cast.base, by = "muni_county_veri_mach_agreed", match = "first")
# vot.equip.veri.final = join(vot.equip.veri.final,vot.equip.veri.spr.base, by = "muni_county_veri_mach_agreed", match = "first")
#
# ward.2016.2012.o.test = join(ward.2016.2012.o, vot.equip.veri.final, by = "muni_county_veri_mach_agreed")
#
# ward.2016.2012.o = ward.2016.2012.o.test
#
# as.vector = as.vector(ward.2016.2012.o[,1]) # 1:length(ward.2016.2012.o[,1]),
# length(unique(as.vector))
#
# uniques = NA
# colno = NA
# for (i in 1:length(colnames(ward.2016.2012.o))){
#   as.vector = as.vector(ward.2016.2012.o[,i])
#   uniques[i] = length(unique(as.vector))
#   colno[i] = i
# }
# min(uniques)
#
# uniques.cols.df = data.frame(uniques,colno)
#
# hand_na = ward.2016.2012.o$hand_makenot_applicable
#
# ward.2016.2012.o[grep("hand_make",colnames(ward.2016.2012.o))] = NULL
# ward.2016.2012.o[grep("hand_model",colnames(ward.2016.2012.o))] = NULL
#
# ward.2016.2012.o$hand = hand_na
#
# { # For Optical scan systems (OS)
#
#   colnames(ward.2016.2012.o[grep("os_make",colnames(ward.2016.2012.o))])
#
#   ward.2016.2012.o$os.use = NA
#   ward.2016.2012.o$os.use =
#     ifelse(
#       ward.2016.2012.o$os_makedominion_voting_systems == 1 |
#         ward.2016.2012.o$os_makeelection_systems_software == 1 |
#         ward.2016.2012.o$os_makepopulex == 1 |
#         ward.2016.2012.o$os_makepremier_diebold_dominion == 1 |
#         ward.2016.2012.o$os_makesequoia_dominion == 1 |
#         ward.2016.2012.o$os_makevote_pad == 1,1,0
#     )
# } # For Optical scan systems (OS)
#
# { # For touchscreen systems
#   ward.2016.2012.o$tch.use = NA
#   ward.2016.2012.o$tch.use =
#     ifelse(
#       ward.2016.2012.o$tch_makedominion_voting_systems == 1 |
#         ward.2016.2012.o$tch_makeelection_systems_software == 1 |
#         ward.2016.2012.o$tch_makepopulex == 1 |
#         ward.2016.2012.o$tch_makepremier_diebold_dominion == 1 |
#         ward.2016.2012.o$tch_makesequoia_dominion == 1 |
#         ward.2016.2012.o$tch_makevote_pad == 1,1,0
#     )
# } # For touchscreen systems
#
# { # For Ballot Marking Device systems
#   ward.2016.2012.o$bmd.use = NA
#   ward.2016.2012.o$bmd.use =
#     ifelse(
#       ward.2016.2012.o$bmd_makedominion_voting_systems == 1 |
#         ward.2016.2012.o$bmd_makeelection_systems_software == 1 |
#         ward.2016.2012.o$bmd_makepopulex == 1 |
#         ward.2016.2012.o$bmd_makepremier_diebold_dominion == 1 |
#         ward.2016.2012.o$bmd_makesequoia_dominion == 1 |
#         ward.2016.2012.o$bmd_makevote_pad == 1,1,0
#     )
# } # For Ballot Marking Device systems
#
# # Later, it seems that these do not match with the verified voting data:
# # [1] CITY OF NEW LONDON WAUPACA. and CITY OF LONDON OUTAGAMIE. It simply doesn't exist.
#
# ward.2016.2012.o[grepl("os_",colnames(ward.2016.2012.o))][ward.2016.2012.o$muni_county == "CITY OF NEW LONDON WAUPACA",] = 0
# ward.2016.2012.o$os.use[ward.2016.2012.o$muni_county == "CITY OF NEW LONDON WAUPACA"] = 1
# ward.2016.2012.o$os_makeelection_systems_software[ward.2016.2012.o$muni_county == "CITY OF NEW LONDON WAUPACA"] = 1
# ward.2016.2012.o$os_modeloptech_iiip_eagle[ward.2016.2012.o$muni_county == "CITY OF NEW LONDON WAUPACA"] = 1
#
# ward.2016.2012.o[grepl("os_",colnames(ward.2016.2012.o))][ward.2016.2012.o$muni_county == "CITY OF NEW LONDON OUTAGAMIE",] = 0
# ward.2016.2012.o$os.use[ward.2016.2012.o$muni_county == "CITY OF NEW LONDON OUTAGAMIE"] = 1
# ward.2016.2012.o$os_makeelection_systems_software[ward.2016.2012.o$muni_county == "CITY OF NEW LONDON OUTAGAMIE"] = 1
# ward.2016.2012.o$os_modeloptech_iiip_eagle[ward.2016.2012.o$muni_county == "CITY OF NEW LONDON OUTAGAMIE"] = 1

# Loading in spatial data for original data -------------------------------------------------
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
colnames(ward.2016.2012.o)[grep("mcd_fips",colnames(ward.2016.2012.o))] = "MCD_FIPS"

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

lu.grp = group_by(wisc.spat@data, MCD_FIPS, MCD_NAME, CNTY_NAME, CNTY_FIPS, CTV)
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

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "TOWN OF SPRINGFIELD ST. CROIX"] =
  with(wisc.spat.df, (MCD_FIPS[MCD_NAME == "SPRINGFIELD" & CNTY_NAME == "ST_CROIX"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF UNITY CLARK"] =
  with(wisc.spat.df, (MCD_FIPS[MCD_NAME == "UNITY" & CNTY_NAME == "CLARK"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF UNITY MARATHON"] =
  with(wisc.spat.df, (MCD_FIPS[MCD_NAME == "Unity" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF COLBY MARATHON"] =
  with(wisc.spat.df, (MCD_FIPS[MCD_NAME == "Colby" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF ABBOTSFORD MARATHON"] =
  with(wisc.spat.df, unique((MCD_FIPS[MCD_NAME == "Abbotsford" & CNTY_NAME == "MARATHON" & CTV == "C"])))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF DORCHESTER MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Dorchester" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF DORCHESTER MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Dorchester" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "TOWN OF WASHINGTON DOOR"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "WASHINGTON" & CNTY_NAME == "DOOR"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF PULASKI BROWN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Pulaski" & CNTY_NAME == "BROWN"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF HOBART BROWN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Hobart" & CNTY_NAME == "BROWN"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF PULASKI SHAWANO"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Pulaski" & CNTY_NAME == "SHAWANO"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF WRIGHTSTOWN OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wrightstown" & CNTY_NAME == "OUTAGAMIE"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF WRIGHTSTOWN OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wrightstown" & CNTY_NAME == "OUTAGAMIE"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF WRIGHTSTOWN OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wrightstown" & CNTY_NAME == "OUTAGAMIE"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF MARSHALL DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Marshall" & CNTY_NAME == "DANE"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF APPLETON CALUMET"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Appleton" & CNTY_NAME == "CALUMET"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF APPLETON WINNEBAGO"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Appleton" & CNTY_NAME == "WINNEBAGO"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF MARION SHAWANO"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Marion" & CNTY_NAME == "SHAWANO"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF BIRNAMWOOD MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Birnamwood" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF MARATHON CITY MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Marathon City" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "TOWN OF MARATHON MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MARATHON" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "TOWN OF MARATHON MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MARATHON" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF STANLEY CLARK"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Stanley" & CNTY_NAME == "CLARK"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF EAU CLAIRE CHIPPEWA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Eau Claire" & CNTY_NAME == "CHIPPEWA"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF NEW AUBURN BARRON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "New Auburn" & CNTY_NAME == "BARRON"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF TURTLE LAKE POLK"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Turtle Lake" & CNTY_NAME == "POLK"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF RIVER FALLS ST. CROIX"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "River Falls" & CNTY_NAME == "ST_CROIX"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF SPRING VALLEY ST. CROIX"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Spring Valley" & CNTY_NAME == "ST_CROIX"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF KIEL CALUMET"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Kiel" & CNTY_NAME == "CALUMET"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF BERLIN WAUSHARA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Berlin" & CNTY_NAME == "WAUSHARA"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF WISCONSIN DELLS ADAMS"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wisconsin Dells" & CNTY_NAME == "ADAMS"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF RANDOLPH COLUMBIA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Randolph" & CNTY_NAME == "COLUMBIA"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF KEWASKUM WASHINGTON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Kewaskum" & CNTY_NAME == "WASHINGTON"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF NEWBURG OZAUKEE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Newburg" & CNTY_NAME == "OZAUKEE"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF CAZENOVIA SAUK"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Cazenovia" & CNTY_NAME == "SAUK"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF VIOLA VERNON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Viola" & CNTY_NAME == "VERNON"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF DE SOTO CRAWFORD"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "De Soto" & CNTY_NAME == "CRAWFORD"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF MUSCODA IOWA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Muscoda" & CNTY_NAME == "IOWA"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF MAINE MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MAINE" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "TOWN OF MAINE MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MAINE" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF MILLADORE PORTAGE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Milladore" & CNTY_NAME == "PORTAGE"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF MENASHA CALUMET"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Menasha" & CNTY_NAME == "CALUMET"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF WINDSOR DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Windsor" & CNTY_NAME == "DANE"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF MAZOMANIE DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MAZOMANIE" & CNTY_NAME == "DANE"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "TOWN OF WISCONSIN JUNEAU"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wisonsin" & CNTY_NAME == "JUNEAU"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF HARTFORD WASHINGTON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Hartford" & CNTY_NAME == "WASHINGTON" & CTV == "C"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF COLUMBUS COLUMBIA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Columbus" & CNTY_NAME == "COLUMBIA"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF BAYSIDE OZAUKEE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Bayside" & CNTY_NAME == "OZAUKEE"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF BAYSIDE OZAUKEE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Bayside" & CNTY_NAME == "OZAUKEE"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF WISCONSIN DELLS SAUK"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wisconsin Dells" & CNTY_NAME == "SAUK"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF LAC LA BELLE WAUKESHA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Lac La Belle" & CNTY_NAME == "WAUKESHA"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF WATERTOWN DODGE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Watertown" & CNTY_NAME == "DODGE"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF WINDSOR DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Windsor" & CNTY_NAME == "DANE"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF MONTFORT IOWA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Montfort" & CNTY_NAME == "IOWA"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF LIVINGSTON IOWA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Livingston" & CNTY_NAME == "IOWA"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF CUBA CITY LAFAYETTE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Cuba City" & CNTY_NAME == "LAFAYETTE"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF HAZEL GREEN LAFAYETTE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Hazel Green" & CNTY_NAME == "LAFAYETTE"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "TOWN OF SPRING GROVE GREEN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "SPRING GROVE" & CNTY_NAME == "GREEN"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF BLANCHARDVILLE IOWA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Blanchardville" & CNTY_NAME == "IOWA"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF BELLEVILLE GREEN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Belleville" & CNTY_NAME == "GREEN"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF BROOKLYN GREEN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Brooklyn" & CNTY_NAME == "GREEN"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF EDGERTON DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Edgerton" & CNTY_NAME == "DANE"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF CAMBRIDGE JEFFERSON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Cambridge" & CNTY_NAME == "JEFFERSON"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF WHITEWATER JEFFERSON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Whitewater" & CNTY_NAME == "JEFFERSON"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF MUKWONAGO WALWORTH"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Mukwonago" & CNTY_NAME == "WALWORTH"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "TOWN OF BURLINGTON RACINE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "BURLINGTON" & CNTY_NAME == "RACINE"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF BURLINGTON WALWORTH"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Burlington" & CNTY_NAME == "WALWORTH"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF BRODHEAD ROCK"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Brodhead" & CNTY_NAME == "ROCK"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "TOWN OF SPRING GROVE GREEN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "SPRING GROVE" & CNTY_NAME == "GREEN"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF BRISTOL KENOSHA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Bristol" & CNTY_NAME == "KENOSHA"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF BLOOMFIELD WALWORTH"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "BLOOMFIELD" & CNTY_NAME == "WALWORTH" & CTV == "V"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "TOWN OF BLOOMFIELD WALWORTH"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "BLOOMFIELD" & CNTY_NAME == "WALWORTH" & CTV == "T"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF WAUPUN FOND DU LAC"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Waupun" & CNTY_NAME == "FOND_DU_LAC"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "TOWN OF MCMILLAN MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MCMILLAN" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF MARSHFIELD MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Marshfield" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF ROCKLAND LA CROSSE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Rockland" & CNTY_NAME == "LA_CROSSE"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF MAPLE BLUFF DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Maple Bluff" & CNTY_NAME == "DANE"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF ONTARIO VERNON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Ontario" & CNTY_NAME == "VERNON"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF PULASKI BROWN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Pulaski" & CNTY_NAME == "BROWN"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "TOWN OF MADISON DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Madison" & CNTY_NAME == "DANE" & CTV == "T"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF MADISON DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Madison" & CNTY_NAME == "DANE" & CTV == "C"]))

###
# Places that fail before #
###

# Pulaski OCONTO apparently doesn't have any people in it
ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF PULASKI OCONTO"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Pulaski" & CNTY_NAME == "OCONTO"]))

# CITY OF NEW LONDON JUST DOESN'T EXIST IN VERIFIED VOTING DATA. I SUBBED IN THE OFFICIAL WISC ELEC BOARD STATS
ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF NEW LONDON OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "New London" & CNTY_NAME == "OUTAGAMIE"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF NEW LONDON WAUPACA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "New London" & CNTY_NAME == "WAUPACA"]))

# Milwaukee districts outside of Milwaukee. Maybe all have a population of 0
ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF MILWAUKEE WAUKESHA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Milwaukee" & CNTY_NAME == "WAUKESHA"]))

ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF MILWAUKEE WASHINGTON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Milwaukee" & CNTY_NAME == "WASHINGTON"]))

# Rockland Monroe just doesn't exist. 0 population?
ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF ROCKLAND MONROE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Rockland" & CNTY_NAME == "MONROE"]))

# Ontario Monroe doesn't exist in my db. 0 population?
ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF ONTARIO MONROE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Ontario" & CNTY_NAME == "MONROE"]))

# Wisconsin Dells Juneau doesn't exist in my db. Maybe a 0 population.
ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF WISCONSIN DELLS JUNEAU"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wisconsin Dells" & CNTY_NAME == "JUNEAU"]))

# Harrison Outagamie doesn't exist. Probably 0 population in Outagamie
ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "TOWN OF HARRISON OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "HARRISON" & CNTY_NAME == "OUTAGAMIE"]))

# Hartford Dodge doesn't exist, as 0 people live there
ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF HARTFORD DODGE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Hartford" & CNTY_NAME == "DODGE" & CTV == "C"]))

# Almost all of the city live in the Columbia side of the party
ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF COLUMBUS DODGE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Columbus" & CNTY_NAME == "DODGE"]))

# Kewaskum fond du lac doesn't exist - all population is in Washington
ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF KEWASKUM FOND DU LAC"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Kewaskum" & CNTY_NAME == "FOND_DU_LAC"]))

# All residents of Howard live in the Brown side of the town
ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF HOWARD OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Howard" & CNTY_NAME == "OUTAGAMIE"]))

# The Calumet part of Kaukauna has a population of 0
ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF KAUKAUNA CALUMET"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Kaukauna" & CNTY_NAME == "CALUMET"]))

# Genoa city, Kenosha part only has 6 residents
ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF GENOA KENOSHA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Genoa" & CNTY_NAME == "KENOSHA"]))####

# Lac La Belle, Jefferson, has only ONE resident. The Waukesha side has the rest (289)
ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "VILLAGE OF LAC LA BELLE JEFFERSON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Lac La Belle" & CNTY_NAME == "JEFFERSON"]))####

# The Bayfield side of the town has 0 residents
ward.2016.2012.o$MCD_FIPS[ward.2016.2012.o$muni_county == "CITY OF ASHLAND BAYFIELD"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Ashland" & CNTY_NAME == "BAYFIELD"]))


# Milwaukee city in other counties doesn't exist

####
####

####
# Joining data

wisc.spat.j = wisc.spat.d.df

# Joining data together
wisc.spat.j@data = join(wisc.spat.d.df@data, ward.2016.2012.o, by = "MCD_FIPS", match = "first")


# Geojsons fail if there is an 'NAN' in the data
wisc.spat.j@data[is.na(wisc.spat.j@data)] <- "NA"

wisc.spat.j.point = SpatialPointsDataFrame(wisc.spat.j,wisc.spat.j@data)

# plot(wisc.spat.j)
wisc.spat.j.df = as.data.frame(wisc.spat.j)
writeOGR(
  wisc.spat.j,
  "wisc.spat.orig.geojson",
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

# Ward level data 2012 and 2016 recount ----------------------------------------------------
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

  ward.2016.look = with(ward.2016, data.frame(county_name, municipality_name,
                                              reporting_unit, muni_county))
  colnames(ward.2016.look) = c("county","municipality","reporting_unit",
                               "muni_county")

  # ward.digits.2016 = ward.2016
  # ward.digits.2016 = subset(ward.digits.2016, original_or_recount == "recount")
  #
  # # No differences in digits ending in 0 or 5 between Trump and Clinton
  #
  # ward.digits.2016$trump.digit = substrRight(ward.digits.2016$donald_j_trump, 1)
  # ward.digits.2016$trump.digit = as.integer(ward.digits.2016$trump.digit)
  # trump.dig.table = table(ward.digits.2016$trump.digit) / length(ward.digits.2016$trump.digit)
  #
  # ward.digits.2016$clinton.digit = substrRight(ward.digits.2016$hillary_clinton, 1)
  # ward.digits.2016$clinton.digit = as.integer(ward.digits.2016$clinton.digit)
  # clinton.dig.table = table(ward.digits.2016$clinton.digit) / length(ward.digits.2016$clinton.digit)
  #
  # # No difference in digits ending in 0 or 5 for Stein
  #
  # ward.digits.2016.stein = subset(ward.digits.2016, jill_stein > 9)
  # ward.digits.2016.stein$stein.digit = substrRight(ward.digits.2016.stein$jill_stein, 1)
  # ward.digits.2016.stein$stein.digit = as.integer(ward.digits.2016.stein$stein.digit)
  # stein.dig.table = table(ward.digits.2016.stein$stein.digit) / length(ward.digits.2016.stein$stein.digit)
  # table(ward.digits.2016.stein$stein.digit)
  #
  # ward.digits.2016$stein.digit = substrRight(ward.digits.2016$jill_stein, 1)
  # ward.digits.2016$stein.digit = as.integer(ward.digits.2016$stein.digit)
  # stein.dig.table = table(ward.digits.2016$stein.digit) / length(ward.digits.2016$stein.digit)
  # table(ward.digits.2016$stein.digit)
  #

  ward.2016.v = ward.2016
  ward.2016.v$county_name = NULL
  ward.2016.v$municipality_name = NULL
  ward.2016.v$total_votes = NULL
  ward.2016.v$reporting_unit = NULL

  ward.2016.r = subset(ward.2016.v, original_or_recount == "Recount")
  # ward.2016.r = subset(ward.2016.v, original_or_recount == "recount")

  ward.2016.r$original_or_recount = NULL
  # ward.2016.r$original_or_recount = NULL

  ward.2016.r.l = gather(ward.2016.r, muni_county);
  colnames(ward.2016.r.l) = c("muni_county", "cand", "votes.rec")

  ward.2016.r.l$cand.group = NA
  ward.2016.r.l$cand.group[grep("hillary",ward.2016.r.l$cand)] = "democrat"
  ward.2016.r.l$cand.group[grep("donald",ward.2016.r.l$cand)] = "republican"
  ward.2016.r.l$cand.group[grep("darrell",ward.2016.r.l$cand)] = "other"
  ward.2016.r.l$cand.group[grep("rocky",ward.2016.r.l$cand)] = "other"
  ward.2016.r.l$cand.group[grep("gary",ward.2016.r.l$cand)] = "other"
  ward.2016.r.l$cand.group[grep("monica",ward.2016.r.l$cand)] = "other"
  ward.2016.r.l$cand.group[grep("jill",ward.2016.r.l$cand)] = "other"

  # There were a bunch of write-in names for the ward level results
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
  ward.2016.r.l = join(ward.2016.r.l, ward.2016.look, by = "muni_county", match = "first")

  ward.2016.r.l$votes.rec = as.numeric(as.integer(ward.2016.r.l$votes.rec))
  table(is.na(ward.2016.r.l$votes.rec))

  tot.votes.look.group = group_by(ward.2016.r.l,muni_county)
  tot.votes.look = dplyr::summarise(tot.votes.look.group,
                                    votes.tot = sum(votes.rec)
  )
  ward.2016.r.l = join(ward.2016.r.l,tot.votes.look,by="muni_county",match="first")
  ward.2016.r.l$votes.perc = (ward.2016.r.l$votes.rec / ward.2016.r.l$votes.tot) * 100

  ward.2016.r.l$muni_county = paste(ward.2016.r.l$municipality,
                                    ward.2016.r.l$county)

  ward.2016.r.mun.group = group_by(ward.2016.r.l,municipality,muni_county,county,cand.group)
  ward.2016.r.mun = dplyr::summarise(ward.2016.r.mun.group,
                                     votes.rec2 = sum(votes.rec),
                                     votes.tot2 = mean(votes.tot)
  )
  colnames(ward.2016.r.mun)[5] = "votes.rec"
  colnames(ward.2016.r.mun)[6] = "votes.tot"

  ward.2016.r.mun$votes.perc = (ward.2016.r.mun$votes.rec / ward.2016.r.mun$votes.tot) * 100
} # Preparing basic data

## Now bringing in the registered voter data. need to move up to municipality level
ward.2016.regvote = read.csv(
  "registeredvotersbywards_xlsx_19539MODnov16.csv",
  stringsAsFactors = F,
  header = T,
  strip.white = T
)
{
  ward.2016.regvote = clean_names(ward.2016.regvote)
  ward.2016.regvote$ward = toupper(ward.2016.regvote$ward)
  ward.2016.regvote$county = toupper(ward.2016.regvote$county)
  ward.2016.regvote$muni = toupper(ward.2016.regvote$muni)

  ward.2016.regvote$county = gsub(" COUNTY","",ward.2016.regvote$county)
  ward.2016.regvote$muni = gsub(" -.*","",ward.2016.regvote$muni)
  ward.2016.regvote$muni_county = paste(ward.2016.regvote$muni, ward.2016.regvote$county)

  colnames(ward.2016.regvote)[2] = "municipality"

  ### Limiting to municipality level
  ward.2016.regvote.mun.group = group_by(ward.2016.regvote,muni_county,municipality,county)
  ward.2016.regvote.mun = dplyr::summarise(ward.2016.regvote.mun.group,
                                           voter_count2 = sum(voter_count)
  )
  colnames(ward.2016.regvote.mun)[4] = "voters.tot"



  ###
  # Seeing what is differnt according to muni county level
  ###
  voter.reg.mun = data.frame(unique(ward.2016.regvote.mun$muni_county))
  votes.mun = data.frame(unique(ward.2016.r.l$muni_county))
  colnames(voter.reg.mun) = "voter.reg.mun"
  colnames(votes.mun) = "votes.mun"

  # Ugh, I need to change the name of these muni_counties to make sure I have a full match
  setdiff(unique(voter.reg.mun$voter.reg.mun), unique(votes.mun$votes.mun))
  setdiff(unique(votes.mun$votes.mun), unique(voter.reg.mun$voter.reg.mun))

  differences = setdiff(unique(votes.mun$votes.mun),unique(voter.reg.mun$voter.reg.mun))
  voter.reg.diffs = subset(ward.2016.r.l, muni_county %in% differences)

  voter.reg.diffs.join = with(voter.reg.diffs, data.frame(municipality, county, muni_county, reporting_unit))
  voter.reg.diffs.join = voter.reg.diffs.join[!duplicated(voter.reg.diffs.join),]

  ward.2016.regvote$ward = gsub(" -","",ward.2016.regvote$ward)

  ###
  # Seeing what is different according to ward level
  ###
  matched.wards = ward.2016.r.l[match(ward.2016.regvote$ward, ward.2016.r.l$reporting_unit),]
  matched.wards = matched.wards[!is.na(matched.wards),]


  ###
  # Municipalities with exceptions
  ##
  {
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF WRIGHTSTOWN WARD 4"] = "OUTAGAMIE"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 5"] = "ADAMS"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF NEW AUBURN WARD 2"] = "BARRON"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF NEW AUBURN WARD 3"] = "BARRON"

    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 1"] = "MANITOWOC"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 2"] = "MANITOWOC"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 3"] = "MANITOWOC"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 4"] = "MANITOWOC"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 5"] = "MANITOWOC"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 6"] = "MANITOWOC"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 8"] = "MANITOWOC"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF KIEL WARD 7"] = "CALUMET"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 31"] = "WINNEBAGO"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MENASHA WARD 16"] = "CALUMET"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MENASHA WARD 17"] = "CALUMET"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF EAU CLAIRE WARD 16"] = "CHIPPEWA"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF UNITY WARD 2"] = "CLARK"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF STANLEY WARD 5"] = "CLARK"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF RANDOLPH WARD 3"] = "COLUMBIA"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF DE SOTO WARD 2"] = "CRAWFORD"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF EDGERTON WARD 7"] = "DANE"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 1"] = "DODGE"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 2"] = "DODGE"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 3"] = "DODGE"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 4"] = "DODGE"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 5"] = "DODGE"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 6"] = "DODGE"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WATERTOWN WARD 7"] = "DODGE"

    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WAUPUN WARD 9"] = "FOND DU LAC"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WAUPUN WARD 10"] = "FOND DU LAC"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WAUPUN WARD 11"] = "FOND DU LAC"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WAUPUN WARD 12"] = "FOND DU LAC"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF BELLEVILLE WARD 3"] = "GREEN"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF BROOKLYN WARD 2"] = "GREEN"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF BLANCHARDVILLE WARD 2"] = "IOWA"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF LIVINGSTON WARD 2"] = "IOWA"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF MONTFORT WARD 2"] = "IOWA"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF MUSCODA WARD 3"] = "IOWA"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF CAMBRIDGE WARD 1"] = "JEFFERSON"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WHITEWATER WARD 10"] = "JEFFERSON"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WHITEWATER WARD 11"] = "JEFFERSON"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WHITEWATER WARD 12"] = "JEFFERSON"


    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF HAZEL GREEN WARD 3"] = "LAFAYETTE"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF CUBA CITY WARD 5"] = "LAFAYETTE"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF BIRNAMWOOD WARD 2"] = "MARATHON"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF DORCHESTER WARD 2"] = "MARATHON"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF ABBOTSFORD WARD 1"] = "MARATHON"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF COLBY WARD 1"] = "MARATHON"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARSHFIELD WARD 12"] = "MARATHON"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARSHFIELD WARD 20"] = "MARATHON"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARSHFIELD WARD 21"] = "MARATHON"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARSHFIELD WARD 24"] = "MARATHON"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF NEW LONDON WARD 1"] = "OUTAGAMIE"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF NEW LONDON WARD 2"] = "OUTAGAMIE"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF BAYSIDE WARD 6"] = "OZAUKEE"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF NEWBURG WARD 3"] = "OZAUKEE"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF TURTLE LAKE WARD 2A"] = "POLK"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF MILLADORE WARD 2"] = "PORTAGE"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF BRODHEAD WARD 7"] = "ROCK"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF BRODHEAD WARD 8"] = "ROCK"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF SPRING VALLEY WARD 3"] = "ST. CROIX"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF RIVER FALLS WARD 1"] = "ST. CROIX"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF RIVER FALLS WARD 2"] = "ST. CROIX"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF RIVER FALLS WARD 3"] = "ST. CROIX"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF RIVER FALLS WARD 4"] = "ST. CROIX"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF CAZENOVIA WARD 2"] = "SAUK"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 4"] = "SAUK"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 8"] = "SAUK"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 10"] = "SAUK"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 5"] = "ADAMS"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF WISCONSIN DELLS WARD 9"] = "ADAMS"

    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF PULASKI WARD 4"] = "SHAWANO"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARION WARD 4"] = "SHAWANO"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARION WARD 5"] = "SHAWANO"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF MARION WARD 6"] = "SHAWANO"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF VIOLA WARD 1"] = "VERNON"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF MUKWONAGO WARD 11"] = "WALWORTH"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF BURLINGTON WARD 9"] = "WALWORTH"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF BURLINGTON WARD 10"] = "WALWORTH"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF BERLIN WARD 7"] = "WAUSHARA"

    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF MUKWONAGO WARD 11"] = "WALWORTH"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF WRIGHTSTOWN WARD 1"] = "BROWN"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF WRIGHTSTOWN WARD 2"] = "BROWN"
    ward.2016.regvote$county[ward.2016.regvote$ward == "VILLAGE OF WRIGHTSTOWN WARD 3"] = "BROWN"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 12"] = "CALUMET"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 13"] = "CALUMET"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 14"] = "CALUMET"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 26"] = "CALUMET"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 44"] = "CALUMET"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 45"] = "CALUMET"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 46"] = "CALUMET"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 47"] = "CALUMET"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 31"] = "WINNEBAGO"
    ward.2016.regvote$county[ward.2016.regvote$ward == "CITY OF APPLETON WARD 32"] = "WINNEBAGO"
  }
  ward.2016.regvote$muni_county = paste(ward.2016.regvote$municipality, ward.2016.regvote$county)

  setdiff(unique(votes.mun$votes.mun), unique( ward.2016.regvote$muni_county))

  ## Joining the reg.vote summarised municipalities back onto the voting municipalities
  ward.2016.regvote.join = with(ward.2016.regvote, data.frame(municipality,muni_county))
  colnames(ward.2016.regvote.join)[2] = "muni_county_regvote_agreed"
  ward.2016.regvote.join = ward.2016.regvote.join[!duplicated(ward.2016.regvote.join$muni_county_regvote),]
  #
  # voter.reg.diffs.join = join(voter.reg.diffs.join,ward.2016.regvote.join,by = "municipality")
  #
  # back.to.votes.df.join = with(voter.reg.diffs.join, data.frame(muni_county,muni_county_regvote_agreed))
  #
  # ward.2016.r.l = join(ward.2016.r.l, back.to.votes.df.join, by = "muni_county")
  #
  # unique(ward.2016.r.l$muni_county_regvote_agreed)
  #
  # ward.2016.r.l$muni_county_regvote_agreed = as.character(ward.2016.r.l$muni_county_regvote_agre

  ward.2016.r.l$muni_county_regvote_agreed = as.character(ward.2016.r.l$muni_county)
  ward.2016.r.l$muni_county_regvote_agreed = ifelse(is.na(ward.2016.r.l$muni_county_regvote_agreed),
                                                    ward.2016.r.l$muni_county,ward.2016.r.l$muni_county_regvote_agreed)
  ## Now renaming the regvote column for the final join
  colnames(ward.2016.regvote)[6] = "muni_county_regvote_agreed"

  # Trying to match by muni_county level
  ## Need to sum up the votes by county
  ward.2016.regvote.votes.group = group_by(ward.2016.regvote,muni_county_regvote_agreed)
  ward.2016.regvote.votes.g = dplyr::summarise(ward.2016.regvote.votes.group,
                                               voter_count2 = sum(voter_count)
  )
  colnames(ward.2016.regvote.votes.g)[2] = "voters.tot"

  # Trying to match by ward level
  ward.2016.r.l = join(ward.2016.r.l, ward.2016.regvote.votes.g, by = "muni_county_regvote_agreed")
  colnames(ward.2016.r.l)[11] = "voters.tot"

  setdiff(ward.2016.regvote.votes.g$muni_county_regvote_agreed,
          ward.2016.r.l$muni_county_regvote_agreed)

  setdiff(ward.2016.r.l$muni_county_regvote_agreed,
          ward.2016.regvote.votes.g$muni_county_regvote_agreed)

  ward.2016.r.l$turnout = with(ward.2016.r.l, votes.tot / voters.tot)

  ### Joining this back to ward.2016.r.mun
  ward.2016.r.l.join = with(ward.2016.r.l,
                            data.frame(
                              muni_county,voters.tot
                            ))
  colnames(ward.2016.r.l.join)[1] = "muni_county"
  ward.2016.r.l.join = ward.2016.r.l.join[!duplicated(ward.2016.r.l.join$muni_county),]

  ward.2016.r.mun = join(ward.2016.r.mun, ward.2016.r.l.join, by ="muni_county", match = "first")

} # Adding in registered vote data

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

  ward.12.look = with(ward.12, data.frame(county, municipality_name,
                                          reporting_unit, muni_county))
  colnames(ward.12.look) = c("county","municipality","reporting_unit",
                             "muni_county")

  ward.12.v = ward.12
  ward.12.v$county = NULL
  ward.12.v$municipality_name = NULL
  ward.12.v$total_votes = NULL
  ward.12.v$reporting_unit = NULL
  ward.12.v$congressional = NULL
  ward.12.v$senatedistrict = NULL
  ward.12.v$assemblydistrict = NULL

  ward.12.l = gather(ward.12.v, muni_county);
  colnames(ward.12.l) = c("muni_county", "cand", "votes.rec")

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

  # Joining back county and municipality names
  ward.12.l = join(ward.12.l, ward.12.look, by = "muni_county", match = "first")

  tot.votes.look.group = group_by(ward.12.l,muni_county)
  tot.votes.look = dplyr::summarise(tot.votes.look.group,
                                    votes.tot = sum(votes.rec)
  )

  ward.12.l = join(ward.12.l,tot.votes.look,by="muni_county",match="first")
  ward.12.l$votes.perc = (ward.12.l$votes.rec / ward.12.l$votes.tot) * 100

  ward.12.l$muni_county = paste(ward.12.l$municipality, ward.12.l$county)

  ward.12.mun.group = group_by(ward.12.l,municipality,muni_county,county,cand,cand.group)
  ward.12.mun = dplyr::summarise(ward.12.mun.group,
                                 votes.rec2 = sum(votes.rec),
                                 votes.tot2 = mean(votes.tot)
  )
  colnames(ward.12.mun)[6] = "votes.rec"
  colnames(ward.12.mun)[7] = "votes.tot"

  ward.12.mun$votes.perc = (ward.12.mun$votes.rec / ward.12.mun$votes.tot) * 100
}
# Now joining the 2012 data back into the 2016 data
{
  # Now joining the 2012 data back into the 2016 data
  setdiff(unique(ward.12.mun$muni_county), unique(ward.2016.r.mun$muni_county))

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

  setdiff(unique(ward.2016.r.mun$muni_county), unique(ward.12.mun$muni_county))

  # Village of Harrison Calumet did not exist in 2012, was split off from the town in 2013.
  # It is impossible to know the boundaries exactly.
  # Therefore the swing from 2012 to 2016 cannot be accurately calculated.

  # Same of the town / village of Somers, Kenosha. The town / village is completely mixed and messed up

  # the town of Menasha was retained only for wards (8-10, 11-13). The rest (1-7) became the village of
  # Fox Crossing, Winnebago

  differences = setdiff(unique(ward.2016.r.mun$muni_county),unique(ward.12.mun$muni_county))
  vote.2012.diffs = subset(ward.2016.r.mun, muni_county %in% differences)

  vote.2012.diffs.join = with(vote.2012.diffs, data.frame(municipality, county, muni_county))
  vote.2012.diffs.join = vote.2012.diffs.join[!duplicated(vote.2012.diffs.join),]

  ## Joining the reg.vote summarised municipalities back onto the voting municipalities
  ward.12.mun.join = with(ward.12.mun, data.frame(municipality,muni_county))
  colnames(ward.12.mun.join)[2] = "muni_county_2012_agreed"
  ward.12.mun.join = ward.12.mun.join[!duplicated(ward.12.mun.join$muni_county_2012),]

  vote.2012.diffs.join = join(vote.2012.diffs.join,ward.12.mun.join,by = "municipality")

  back.to.votes.df.join = with(vote.2012.diffs.join, data.frame(muni_county,muni_county_2012_agreed))

  ward.2016.r.mun = join(ward.2016.r.mun, back.to.votes.df.join, by = "muni_county")

  ward.2016.r.mun$muni_county_2012_agreed = as.character(ward.2016.r.mun$muni_county_2012_agreed)

  ward.2016.r.mun$muni_county_2012_agreed = ifelse(is.na(ward.2016.r.mun$muni_county_2012_agreed),
                                                   ward.2016.r.mun$muni_county,ward.2016.r.mun$muni_county_2012_agreed)

  ## Now renaming the 2012 column for the final join
  # colnames(ward.12.mun)[2] = "muni_county_2012_agreed"

  ## Need to sum up the votes by county in 2012
  ward.12.mun.votes.group = group_by(ward.12.mun,cand.group,muni_county)
  ward.12.mun.votes.g = dplyr::summarise(ward.12.mun.votes.group,
                                         votes.rec2012 = sum(votes.rec),
                                         votes.tot2012 = mean(votes.tot)

  )

  # Melting the 2012 df to put the candidates on top
  ward.12.mun.votes.g.melt = melt(ward.12.mun.votes.g)
  ward.12.mun.votes.g.cast = dcast(ward.12.mun.votes.g.melt,
                                   muni_county ~ cand.group + variable, sum
  )
  ward.12.mun.votes.g.cast$democrat_votes.tot2012 = NULL
  ward.12.mun.votes.g.cast$republican_votes.tot2012 = NULL

  ward.12.mun.votes.g.cast.mean = dcast(ward.12.mun.votes.g.melt,
                                        muni_county ~ cand.group + variable, mean
  )
  ward.12.mun.votes.g.cast$other_votes.tot2012 = ward.12.mun.votes.g.cast.mean$other_votes.tot2012
  colnames(ward.12.mun.votes.g.cast)[grepl("votes.tot",colnames(ward.12.mun.votes.g.cast))] =
    "votes.tot2012"

  # Also need to melt the 2016 mun df by cand.group
  ward.2016.r.mun.votes.g.melt = melt(ward.2016.r.mun)
  ward.2016.r.mun.votes.g.cast = dcast(ward.2016.r.mun.votes.g.melt,
                                       muni_county ~ cand.group + variable, sum
  )

  ward.2016.r.mun.votes.g.cast$democrat_votes.tot = NULL
  ward.2016.r.mun.votes.g.cast$republican_votes.tot = NULL
  ward.2016.r.mun.votes.g.cast$democrat_voters.tot = NULL
  ward.2016.r.mun.votes.g.cast$republican_voters.tot = NULL

  ward.2016.r.mun.votes.g.cast.mean = dcast(ward.2016.r.mun.votes.g.melt,
                                            muni_county ~ cand.group + variable, mean
  )
  ward.2016.r.mun.votes.g.cast$other_votes.tot = ward.2016.r.mun.votes.g.cast.mean$other_votes.tot
  ward.2016.r.mun.votes.g.cast$other_voters.tot = ward.2016.r.mun.votes.g.cast.mean$other_voters.tot
  colnames(ward.2016.r.mun.votes.g.cast)[grepl("votes.tot",colnames(ward.2016.r.mun.votes.g.cast))] =
    "votes.tot2016"
  colnames(ward.2016.r.mun.votes.g.cast)[grepl("voters.tot",colnames(ward.2016.r.mun.votes.g.cast))] =
    "voters.tot2016"


  # Finally joining the dfs together

  ward.2016.r.mun.g = join(ward.2016.r.mun.votes.g.cast, ward.12.mun.votes.g.cast, by = "muni_county")

  # 2012 turnout is calculated on the (big?) assumption that the number of registered
  # voters was the same in 2012 as in 2016
  ward.2016.r.mun.g$turnout.2012 =
    with(ward.2016.r.mun.g, votes.tot2012 / voters.tot2016) * 100

  ward.2016.r.mun.g$dem.perc2012 =
    with(ward.2016.r.mun.g,
         democrat_votes.rec2012 / votes.tot2012) * 100
  ward.2016.r.mun.g$rep.perc2012 =
    with(ward.2016.r.mun.g,
         republican_votes.rec2012 / votes.tot2012) * 100
  ward.2016.r.mun.g$oth.perc2012 =
    with(ward.2016.r.mun.g,
         other_votes.rec2012 / votes.tot2012) * 100

  ward.2016.r.mun.g$dem.change.num =
    with(ward.2016.r.mun.g,
         democrat_votes.rec - democrat_votes.rec2012)
  ward.2016.r.mun.g$rep.change.num =
    with(ward.2016.r.mun.g,
         republican_votes.rec - republican_votes.rec2012)
  ward.2016.r.mun.g$oth.change.num =
    with(ward.2016.r.mun.g,
         other_votes.rec - other_votes.rec2012)

  ward.2016.r.mun.g$dem.change.perc =
    with(ward.2016.r.mun.g,
         democrat_votes.perc -  dem.perc2012)
  ward.2016.r.mun.g$rep.change.perc =
    with(ward.2016.r.mun.g,
         republican_votes.perc - rep.perc2012)
  ward.2016.r.mun.g$oth.change.perc =
    with(ward.2016.r.mun.g,
         other_votes.perc - oth.perc2012)

  ward.2016.r.mun.g$turnout.2016.perc =
    with(ward.2016.r.mun.g,
         votes.tot2016 / voters.tot2016) * 100


  ward.2016.r.mun.g$turnout.change.perc =
    with(ward.2016.r.mun.g,
         turnout.2016.perc - turnout.2012)


  ward.2016.2012.r = ward.2016.r.mun.g

  ward.2016.2012.r$demwinner = NA
  ward.2016.2012.r$demwinner = ifelse(ward.2016.2012.r$democrat_votes.perc >
                                        ward.2016.2012.r$republican_votes.perc &
                                        !is.na(ward.2016.2012.r$democrat_votes.perc), 1,0)
  ward.2016.2012.r$demwinner = as.integer(ward.2016.2012.r$demwinner)

  ward.2016.2012.r$demwinner2012 = NA
  ward.2016.2012.r$demwinner2012 = ifelse(ward.2016.2012.r$dem.perc2012 >
                                            ward.2016.2012.r$rep.perc2012 &
                                            !is.na(ward.2016.2012.r$dem.perc2012), 1,0)
  ward.2016.2012.r$demwinner2012 = as.integer(ward.2016.2012.r$demwinner2012)
}
# Loading 2012 ward vote data

# Rejoining the county data to the ward 2016 2012 data
{
  ward.2016.2012.r = join(ward.2016.2012.r, ward.2016.look, by = "muni_county", match = "first")
}

# Loading voting machine data recount (Wisconsin official) ---------------------------------------------
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

vot.equip.county.grouped = group_by(vot.equip, county)
vot.equip.county = dplyr::summarise(
  vot.equip.county.grouped,
  use.machines.prop = sum(use.machines, na.rm = TRUE) /
    length(use.machines)
)

vot.equip.withmachines = subset(vot.equip, use.machines > 0)

vot.equip.county.grouped.machines = group_by(vot.equip.withmachines, county)
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
vot.equip.county = join(vot.equip.county,
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
vot.equip.withmachines$muni_county_regvote_agreed =
  gsub(" -.*", "",vot.equip.withmachines$municipality)

vot.equip.withmachines$muni_county_regvote_agreed =
  paste(vot.equip.withmachines$muni_county_regvote_agreed, vot.equip.withmachines$county)

vot.equip.join = with(vot.equip.withmachines,
                      data.frame(muni_county_regvote_agreed,
                                 use.machines,
                                 machine.vendor.dealer.spec,
                                 machine.vendor.dealer.model,
                                 accessible.vendor.dealer.model))

setdiff(vot.equip.withmachines$muni_county_regvote_agreed,
        ward.2016.r.l$muni_county_regvote_agreed)

# [1] "VILLAGE OF WRIGHTSTOWN BROWN" "TOWN OF WINDSOR DANE" "TOWN OF MAINE MARATHON"

setdiff(ward.2016.r.l$muni_county_regvote_agreed,
        vot.equip.withmachines$muni_county_regvote_agreed)

ward.2016.r.l$muni_county_machines_agreed = ward.2016.r.l$muni_county_regvote_agreed

ward.2016.r.l$muni_county_machines_agreed[grep("VILLAGE OF WRIGHTSTOWN",
                                               ward.2016.r.l$muni_county_machines_agreed)] = "VILLAGE OF WRIGHTSTOWN BROWN"
ward.2016.r.l$muni_county_machines_agreed[grep("TOWN OF MAINE",
                                               ward.2016.r.l$muni_county_machines_agreed)] = "TOWN OF MAINE MARATHON"
colnames(vot.equip.join)[1] = "muni_county_machines_agreed"

setdiff(vot.equip.join$muni_county_machines_agreed,
        ward.2016.r.l$muni_county_machines_agreed)

ward.2016.r.l = join(ward.2016.r.l,vot.equip.join, by = "muni_county_machines_agreed")
ward.2016.r.l$use.machines[is.na(ward.2016.r.l$use.machines)] = 0

colnames(ward.2016.r.l)[grep("use.machines",colnames(ward.2016.r.l))] = "county_use_opt_scan"
colnames(ward.2016.r.l)[grep("machine.vendor.dealer.spec",colnames(ward.2016.r.l))] = "county_machine_vendor_dealer"

####
# doing the same for the 2016 2012 data
####
county.machines.join = with(ward.2016.r.l, data.frame(
  muni_county, county_use_opt_scan, county_machine_vendor_dealer, machine.vendor.dealer.model, accessible.vendor.dealer.model
))

ward.2016.2012.test = join(ward.2016.2012.r, county.machines.join, by = "muni_county", match = "first")

ward.2016.2012.r = ward.2016.2012.test
ward.2016.2012.r$county_use_opt_scan[is.na(ward.2016.2012.r$county_use_opt_scan)] = 0

# Loading voting machine data recount (Verified voting) ---------------------------
# vot.equip.veri = read.csv(
#   "verifier-search-wiMOD.csv",
#   header = TRUE,
#   colClasses = NA,
#   stringsAsFactors = F
# )
#
# colnames(vot.equip.veri)[1] = "MCD_FIPS"
#
# vot.equip.veri = vot.equip.veri[-grep("Wisconsin State",vot.equip.veri$Jurisdiction),]
#
# ## Need to change some municipality names to match my other data frame, exceptions which I found later on
# vot.equip.veri$Division[grep("Land O'Lakes",vot.equip.veri$Division)] = "Land O-Lakes"
# vot.equip.veri$Division[grep("Fontana-on-Geneva Lake",vot.equip.veri$Division)] = "Fontana"
# vot.equip.veri$Division[grep("Poysippi",vot.equip.veri$Division)] = "Poy Sippi"
# vot.equip.veri$Division[grep("St. Lawrence",vot.equip.veri$Division)] = "Saint Lawrence"
#
# vot.equip.veri$habitation = NA
# habitation = NA
# for  (i in 1:length(vot.equip.veri$Jurisdiction)) {
#   habitation[i] = tail(strsplit(vot.equip.veri$Jurisdiction[i],split=" ")[[1]],1)
# }
# vot.equip.veri$habitation = habitation
#
# vot.equip.veri$municipality = NA
# vot.equip.veri$municipality = paste(vot.equip.veri$habitation, "of", vot.equip.veri$Division)
# vot.equip.veri$municipality = toupper(vot.equip.veri$municipality)
#
# length(unique(vot.equip.veri$municipality))
#
# ## Need to change some municipality names to match my other data frame, exceptions which I found later on
# vot.equip.veri$municipality[grep("MOUNT STERLING",vot.equip.veri$municipality)] = "VILLAGE OF MT. STERLING"
# vot.equip.veri$municipality[grep("Richland City",vot.equip.veri$Jurisdiction)] = "CITY OF RICHLAND CENTER"
#
# county = NA
# for  (i in 1:length(vot.equip.veri$Jurisdiction)) {
#   county[i] = tail(strsplit(vot.equip.veri$Jurisdiction[i],split=" ")[[1]],2)
# }
# vot.equip.veri$county = county
# vot.equip.veri$county = toupper(vot.equip.veri$county)
#
# # need to change some counties because they are multiple-word counties
# vot.equip.veri$county[grep("CLAIRE",vot.equip.veri$county)] = "EAU CLAIRE"
# vot.equip.veri$county[grep("LAC",vot.equip.veri$county)] = "FOND DU LAC"
# vot.equip.veri$county[grep("CROIX",vot.equip.veri$county)] = "ST. CROIX"
# vot.equip.veri$county[grep("CROSSE",vot.equip.veri$county)] = "LA CROSSE"
# vot.equip.veri$county[grep("LAKE",vot.equip.veri$county)] = "GREEN LAKE"
#
# length(unique(vot.equip.veri$county))
#
# vot.equip.veri$muni_county = paste(vot.equip.veri$municipality, vot.equip.veri$county)
#
# # Dealing with some more exceptions
# rcnt = "TOWN OF MAINE MARATHON"
# vot.equip.veri$Jurisdiction[grep(rcnt,vot.equip.veri$muni_county)] = "MAINE VILLAGE"
# vot.equip.veri$municipality[grep(rcnt,vot.equip.veri$muni_county)] = "VILLAGE OF MAINE"
# vot.equip.veri$muni_county[grep(rcnt,vot.equip.veri$muni_county)] = "VILLAGE OF MAINE MARATHON"
#
# rcnt = "VILLAGE OF LA VALLE SAUK"
# vot.equip.veri$Jurisdiction[grep(rcnt,vot.equip.veri$muni_county)] = "LAVALLE VILLAGE"
# vot.equip.veri$municipality[grep(rcnt,vot.equip.veri$muni_county)] = "VILLAGE OF LAVALLE"
# vot.equip.veri$muni_county[grep(rcnt,vot.equip.veri$muni_county)] = "VILLAGE OF LAVALLE SAUK"
#
# rcnt = "TOWN OF BURLINGTON WALWORTH"
# vot.equip.veri$Jurisdiction[grep(rcnt,vot.equip.veri$muni_county)] = "RACINE TOWN"
# vot.equip.veri$municipality[grep(rcnt,vot.equip.veri$muni_county)] = "TOWN OF BURLINGTON"
# vot.equip.veri$muni_county[grep(rcnt,vot.equip.veri$muni_county)] = "TOWN OF BURLINGTON RACINE"
#
# # The town of Menasha has become the VILLAGE OF FOX CROSSING WINNEBAGO completely; but not according to the voting register
# # (some parts are in both). So, I will not change the name in the voting equipment df.
# rcnt = "TOWN OF MENASHA WINNEBAGO"
# vot.equip.veri$Jurisdiction[grep(rcnt,vot.equip.veri$muni_county)] = "WINNEBAGO VILLAGE"
# vot.equip.veri$municipality[grep(rcnt,vot.equip.veri$muni_county)] = "VILLAGE OF FOX CROSSING"
# vot.equip.veri$muni_county[grep(rcnt,vot.equip.veri$muni_county)] = "VILLAGE OF FOX CROSSING WINNEBAGO"
#
#
# setdiff(vot.equip.veri$muni_county, ward.2016.2012.r$muni_county)
# # DONE
#
# setdiff(ward.2016.2012.r$muni_county,vot.equip.veri$muni_county)
#
# # Later, it seems that these do not match with the verified voting data:
# # [1] CITY OF NEW LONDON WAUPACA. Maybe it was created from another town in Waupaca? I can't find it at all.
#
# unique(ward.2016.2012.r$muni_county == "CITY OF NEW LONDON WAUPACA")
# unique(vot.equip.veri$muni_county == "CITY OF NEW LONDON WAUPACA")
#
#
# # Ugh, I need to change the name of these muni_counties to make sure I have a full match
# setdiff(unique(vot.equip.veri$muni_county), unique(ward.2016.2012.r$muni_county))
# setdiff(unique(ward.2016.2012.r$muni_county), unique(vot.equip.veri$muni_county))
#
# differences = setdiff(unique(ward.2016.2012.r$muni_county),unique(vot.equip.veri$muni_county))
# veri.mach.diffs = subset(ward.2016.2012.r, muni_county %in% differences)
#
# veri.mach.diffs.join = with(veri.mach.diffs, data.frame(municipality, county, muni_county))
# veri.mach.diffs.join = veri.mach.diffs.join[!duplicated(veri.mach.diffs.join),]
#
# ## Joining the veri.mach summarised municipalities back onto the voting municipalities
# ward.2016.veri.mach.join = with(vot.equip.veri, data.frame(municipality,muni_county))
# colnames(ward.2016.veri.mach.join)[2] = "muni_county_veri_mach_agreed"
# ward.2016.veri.mach.join = ward.2016.veri.mach.join[!duplicated(ward.2016.veri.mach.join$muni_county_veri_mach_agreed),]
#
# veri.mach.diffs.join = join(veri.mach.diffs.join,ward.2016.veri.mach.join,by = "municipality")
# back.to.votes.df.join = with(veri.mach.diffs.join, data.frame(muni_county,muni_county_veri_mach_agreed))
# ward.2016.2012.r = join(ward.2016.2012.r, back.to.votes.df.join, by = "muni_county")
#
# ward.2016.2012.r$muni_county_veri_mach_agreed = as.character(ward.2016.2012.r$muni_county_veri_mach_agreed)
#
# ward.2016.2012.r$muni_county_veri_mach_agreed = ifelse(is.na(ward.2016.2012.r$muni_county_veri_mach_agreed),
#                                                        ward.2016.2012.r$muni_county,ward.2016.2012.r$muni_county_veri_mach_agreed)
# ## Now renaming the equip column for the final join
# colnames(vot.equip.veri)[grep("muni_county",colnames(vot.equip.veri))] = "muni_county_veri_mach_agreed"
#
# # Be careful of this problem with spread (duplicate rows): http://stackoverflow.com/questions/25960394/unexpected-behavior-with-tidyr
# vot.equip.veri$row <- 1:nrow(vot.equip.veri)
#
# # From here: http://stackoverflow.com/questions/35663580/using-tidyr-spread-function-to-create-columns-with-binary-value
# vot.equip.veri.spr =  vot.equip.veri %>% mutate(yesno = 1) %>% distinct %>%
#   spread(Make, yesno, fill = 0,sep = "")
# vot.equip.veri.spr =  vot.equip.veri.spr %>% mutate(yesno = 1) %>% distinct %>%
#   spread(Model, yesno, fill = 0,sep = "")
#
# vot.equip.veri.spr =  vot.equip.veri.spr %>% mutate(yesno = 1) %>% distinct %>%
#   spread(Accessible.Use, yesno, fill = 0,sep = "")
# vot.equip.veri.spr =  vot.equip.veri.spr %>% mutate(yesno = 1) %>% distinct %>%
#   spread(Early.Voting, yesno, fill = 0,sep = "")
# vot.equip.veri.spr =  vot.equip.veri.spr %>% mutate(yesno = 1) %>% distinct %>%
#   spread(Absentee.Ballots, yesno, fill = 0,sep = "")
# vot.equip.veri.spr =  vot.equip.veri.spr %>% mutate(yesno = 1) %>% distinct %>%
#   spread(VVPAT, yesno, fill = 0,sep = "")
#
# vot.equip.veri.spr = clean_names(vot.equip.veri.spr)
#
# # ## In the 'vot.equip.veri', need to make sure there are unique values of 'muni_county_veri_mach_agreed'
# # vot.equip.veri.group = group_by(vot.equip.veri.spr,muni_county_veri_mach_agreed)
# # vot.equip.veri.g = dplyr::summarise(vot.equip.veri.group,
# #                                               precincts = mean(Precincts,na.rm = T),
# #                                              vvf.voter.registration = mean(as.numeric(Total.Registration),na.rm = T)
# # )
#
# # Grouping all by muni county
# vot.equip.veri.prepmelt = vot.equip.veri.spr
# vot.equip.veri.prepmelt$polling_place = NULL;vot.equip.veri.prepmelt$mcd_fips = NULL;
# vot.equip.veri.prepmelt$precincts = NULL;vot.equip.veri.prepmelt$total_registration = NULL;
# vot.equip.veri.prepmelt$row = NULL; vot.equip.veri.prepmelt$row = NULL;
# vot.equip.veri.prepmelt$early_votingyes = NULL;
#
# vot.equip.veri.prepmelt$equipment_type[
#   grep("Optical Scan", vot.equip.veri.prepmelt$equipment_type)] ="OS"
# vot.equip.veri.prepmelt$equipment_type[
#   grep("Ballot Marking Device", vot.equip.veri.prepmelt$equipment_type)] ="BMD"
# vot.equip.veri.prepmelt$equipment_type[
#   grep("DRE", vot.equip.veri.prepmelt$equipment_type)] ="TCH"
# vot.equip.veri.prepmelt$equipment_type[
#   grep("Hand Counted", vot.equip.veri.prepmelt$equipment_type)] ="HAND"
#
# vot.equip.veri.melt = melt(vot.equip.veri.prepmelt)
# vot.equip.veri.cast = dcast(vot.equip.veri.melt, muni_county_veri_mach_agreed ~ equipment_type + variable,
#                             fun = sum,na.rm = T)
# vot.equip.veri.cast = clean_names(vot.equip.veri.cast)
#
# # vot.equip.veri.cast[vot.equip.veri.cast == 0] = 1
# # vot.equip.veri.cast[vot.equip.veri.cast == -Inf] = 0
#
# # Dealing with the other numbers
# vot.equip.veri.spr.base = vot.equip.veri.spr
# vot.equip.veri.spr.base[,13:length(colnames(vot.equip.veri.spr.base))] = NULL
# vot.equip.veri.spr.melt = melt(vot.equip.veri.spr.base)
# vot.equip.veri.cast.base = dcast(vot.equip.veri.spr.melt, muni_county_veri_mach_agreed ~ variable,
#                                  fun = max,na.rm = T)
# vot.equip.veri.cast.base = clean_names(vot.equip.veri.cast.base)
#
# vot.equip.veri.final = join(vot.equip.veri.cast,vot.equip.veri.cast.base, by = "muni_county_veri_mach_agreed", match = "first")
# vot.equip.veri.final = join(vot.equip.veri.final,vot.equip.veri.spr.base, by = "muni_county_veri_mach_agreed", match = "first")
#
# ward.2016.2012.r.test = join(ward.2016.2012.r, vot.equip.veri.final, by = "muni_county_veri_mach_agreed")
#
# ward.2016.2012.r = ward.2016.2012.r.test
#
# as.vector = as.vector(ward.2016.2012.r[,1]) # 1:length(ward.2016.2012.r[,1]),
# length(unique(as.vector))
#
# uniques = NA
# colno = NA
# for (i in 1:length(colnames(ward.2016.2012.r))){
#   as.vector = as.vector(ward.2016.2012.r[,i])
#   uniques[i] = length(unique(as.vector))
#   colno[i] = i
# }
# min(uniques)
#
# uniques.cols.df = data.frame(uniques,colno)
#
# hand_na = ward.2016.2012.r$hand_makenot_applicable
#
# ward.2016.2012.r[grep("hand_make",colnames(ward.2016.2012.r))] = NULL
# ward.2016.2012.r[grep("hand_model",colnames(ward.2016.2012.r))] = NULL
#
# ward.2016.2012.r$hand = hand_na
#
# { # For Optical scan systems (OS)
#
#   colnames(ward.2016.2012.r[grep("os_make",colnames(ward.2016.2012.r))])
#
#   ward.2016.2012.r$os.use = NA
#   ward.2016.2012.r$os.use =
#     ifelse(
#       ward.2016.2012.r$os_makedominion_voting_systems == 1 |
#         ward.2016.2012.r$os_makeelection_systems_software == 1 |
#         ward.2016.2012.r$os_makepopulex == 1 |
#         ward.2016.2012.r$os_makepremier_diebold_dominion == 1 |
#         ward.2016.2012.r$os_makesequoia_dominion == 1 |
#         ward.2016.2012.r$os_makevote_pad == 1,1,0
#     )
# } # For Optical scan systems (OS)
#
# { # For touchscreen systems
#   ward.2016.2012.r$tch.use = NA
#   ward.2016.2012.r$tch.use =
#     ifelse(
#       ward.2016.2012.r$tch_makedominion_voting_systems == 1 |
#         ward.2016.2012.r$tch_makeelection_systems_software == 1 |
#         ward.2016.2012.r$tch_makepopulex == 1 |
#         ward.2016.2012.r$tch_makepremier_diebold_dominion == 1 |
#         ward.2016.2012.r$tch_makesequoia_dominion == 1 |
#         ward.2016.2012.r$tch_makevote_pad == 1,1,0
#     )
# } # For touchscreen systems
#
# { # For Ballot Marking Device systems
#   ward.2016.2012.r$bmd.use = NA
#   ward.2016.2012.r$bmd.use =
#     ifelse(
#       ward.2016.2012.r$bmd_makedominion_voting_systems == 1 |
#         ward.2016.2012.r$bmd_makeelection_systems_software == 1 |
#         ward.2016.2012.r$bmd_makepopulex == 1 |
#         ward.2016.2012.r$bmd_makepremier_diebold_dominion == 1 |
#         ward.2016.2012.r$bmd_makesequoia_dominion == 1 |
#         ward.2016.2012.r$bmd_makevote_pad == 1,1,0
#     )
# } # For Ballot Marking Device systems
#
# # Later, it seems that these do not match with the verified voting data:
# # [1] CITY OF NEW LONDON WAUPACA. and CITY OF LONDON OUTAGAMIE. It simply doesn't exist.
#
# ward.2016.2012.r[grepl("os_",colnames(ward.2016.2012.r))][ward.2016.2012.r$muni_county == "CITY OF NEW LONDON WAUPACA",] = 0
# ward.2016.2012.r$os.use[ward.2016.2012.r$muni_county == "CITY OF NEW LONDON WAUPACA"] = 1
# ward.2016.2012.r$os_makeelection_systems_software[ward.2016.2012.r$muni_county == "CITY OF NEW LONDON WAUPACA"] = 1
# ward.2016.2012.r$os_modeloptech_iiip_eagle[ward.2016.2012.r$muni_county == "CITY OF NEW LONDON WAUPACA"] = 1
#
# ward.2016.2012.r[grepl("os_",colnames(ward.2016.2012.r))][ward.2016.2012.r$muni_county == "CITY OF NEW LONDON OUTAGAMIE",] = 0
# ward.2016.2012.r$os.use[ward.2016.2012.r$muni_county == "CITY OF NEW LONDON OUTAGAMIE"] = 1
# ward.2016.2012.r$os_makeelection_systems_software[ward.2016.2012.r$muni_county == "CITY OF NEW LONDON OUTAGAMIE"] = 1
# ward.2016.2012.r$os_modeloptech_iiip_eagle[ward.2016.2012.r$muni_county == "CITY OF NEW LONDON OUTAGAMIE"] = 1

# Loading in spatial data for recount data -------------------------------------------------
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
colnames(ward.2016.2012.r)[grep("mcd_fips",colnames(ward.2016.2012.r))] = "MCD_FIPS"

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

lu.grp = group_by(wisc.spat@data, MCD_FIPS, MCD_NAME, CNTY_NAME, CNTY_FIPS, CTV)
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

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "TOWN OF SPRINGFIELD ST. CROIX"] =
  with(wisc.spat.df, (MCD_FIPS[MCD_NAME == "SPRINGFIELD" & CNTY_NAME == "ST_CROIX"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF UNITY CLARK"] =
  with(wisc.spat.df, (MCD_FIPS[MCD_NAME == "UNITY" & CNTY_NAME == "CLARK"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF UNITY MARATHON"] =
  with(wisc.spat.df, (MCD_FIPS[MCD_NAME == "Unity" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF COLBY MARATHON"] =
  with(wisc.spat.df, (MCD_FIPS[MCD_NAME == "Colby" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF ABBOTSFORD MARATHON"] =
  with(wisc.spat.df, unique((MCD_FIPS[MCD_NAME == "Abbotsford" & CNTY_NAME == "MARATHON" & CTV == "C"])))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF DORCHESTER MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Dorchester" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF DORCHESTER MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Dorchester" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "TOWN OF WASHINGTON DOOR"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "WASHINGTON" & CNTY_NAME == "DOOR"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF PULASKI BROWN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Pulaski" & CNTY_NAME == "BROWN"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF HOBART BROWN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Hobart" & CNTY_NAME == "BROWN"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF PULASKI SHAWANO"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Pulaski" & CNTY_NAME == "SHAWANO"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF WRIGHTSTOWN OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wrightstown" & CNTY_NAME == "OUTAGAMIE"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF WRIGHTSTOWN OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wrightstown" & CNTY_NAME == "OUTAGAMIE"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF WRIGHTSTOWN OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wrightstown" & CNTY_NAME == "OUTAGAMIE"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF MARSHALL DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Marshall" & CNTY_NAME == "DANE"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF APPLETON CALUMET"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Appleton" & CNTY_NAME == "CALUMET"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF APPLETON WINNEBAGO"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Appleton" & CNTY_NAME == "WINNEBAGO"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF MARION SHAWANO"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Marion" & CNTY_NAME == "SHAWANO"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF BIRNAMWOOD MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Birnamwood" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF MARATHON CITY MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Marathon City" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "TOWN OF MARATHON MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MARATHON" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "TOWN OF MARATHON MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MARATHON" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF STANLEY CLARK"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Stanley" & CNTY_NAME == "CLARK"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF EAU CLAIRE CHIPPEWA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Eau Claire" & CNTY_NAME == "CHIPPEWA"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF NEW AUBURN BARRON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "New Auburn" & CNTY_NAME == "BARRON"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF TURTLE LAKE POLK"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Turtle Lake" & CNTY_NAME == "POLK"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF RIVER FALLS ST. CROIX"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "River Falls" & CNTY_NAME == "ST_CROIX"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF SPRING VALLEY ST. CROIX"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Spring Valley" & CNTY_NAME == "ST_CROIX"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF KIEL CALUMET"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Kiel" & CNTY_NAME == "CALUMET"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF BERLIN WAUSHARA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Berlin" & CNTY_NAME == "WAUSHARA"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF WISCONSIN DELLS ADAMS"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wisconsin Dells" & CNTY_NAME == "ADAMS"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF RANDOLPH COLUMBIA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Randolph" & CNTY_NAME == "COLUMBIA"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF KEWASKUM WASHINGTON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Kewaskum" & CNTY_NAME == "WASHINGTON"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF NEWBURG OZAUKEE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Newburg" & CNTY_NAME == "OZAUKEE"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF CAZENOVIA SAUK"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Cazenovia" & CNTY_NAME == "SAUK"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF VIOLA VERNON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Viola" & CNTY_NAME == "VERNON"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF DE SOTO CRAWFORD"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "De Soto" & CNTY_NAME == "CRAWFORD"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF MUSCODA IOWA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Muscoda" & CNTY_NAME == "IOWA"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF MAINE MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MAINE" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "TOWN OF MAINE MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MAINE" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF MILLADORE PORTAGE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Milladore" & CNTY_NAME == "PORTAGE"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF MENASHA CALUMET"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Menasha" & CNTY_NAME == "CALUMET"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF WINDSOR DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Windsor" & CNTY_NAME == "DANE"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF MAZOMANIE DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MAZOMANIE" & CNTY_NAME == "DANE"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "TOWN OF WISCONSIN JUNEAU"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wisonsin" & CNTY_NAME == "JUNEAU"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF HARTFORD WASHINGTON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Hartford" & CNTY_NAME == "WASHINGTON" & CTV == "C"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF COLUMBUS COLUMBIA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Columbus" & CNTY_NAME == "COLUMBIA"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF BAYSIDE OZAUKEE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Bayside" & CNTY_NAME == "OZAUKEE"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF BAYSIDE OZAUKEE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Bayside" & CNTY_NAME == "OZAUKEE"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF WISCONSIN DELLS SAUK"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wisconsin Dells" & CNTY_NAME == "SAUK"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF LAC LA BELLE WAUKESHA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Lac La Belle" & CNTY_NAME == "WAUKESHA"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF WATERTOWN DODGE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Watertown" & CNTY_NAME == "DODGE"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF WINDSOR DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Windsor" & CNTY_NAME == "DANE"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF MONTFORT IOWA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Montfort" & CNTY_NAME == "IOWA"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF LIVINGSTON IOWA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Livingston" & CNTY_NAME == "IOWA"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF CUBA CITY LAFAYETTE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Cuba City" & CNTY_NAME == "LAFAYETTE"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF HAZEL GREEN LAFAYETTE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Hazel Green" & CNTY_NAME == "LAFAYETTE"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "TOWN OF SPRING GROVE GREEN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "SPRING GROVE" & CNTY_NAME == "GREEN"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF BLANCHARDVILLE IOWA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Blanchardville" & CNTY_NAME == "IOWA"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF BELLEVILLE GREEN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Belleville" & CNTY_NAME == "GREEN"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF BROOKLYN GREEN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Brooklyn" & CNTY_NAME == "GREEN"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF EDGERTON DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Edgerton" & CNTY_NAME == "DANE"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF CAMBRIDGE JEFFERSON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Cambridge" & CNTY_NAME == "JEFFERSON"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF WHITEWATER JEFFERSON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Whitewater" & CNTY_NAME == "JEFFERSON"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF MUKWONAGO WALWORTH"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Mukwonago" & CNTY_NAME == "WALWORTH"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "TOWN OF BURLINGTON RACINE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "BURLINGTON" & CNTY_NAME == "RACINE"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF BURLINGTON WALWORTH"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Burlington" & CNTY_NAME == "WALWORTH"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF BRODHEAD ROCK"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Brodhead" & CNTY_NAME == "ROCK"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "TOWN OF SPRING GROVE GREEN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "SPRING GROVE" & CNTY_NAME == "GREEN"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF BRISTOL KENOSHA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Bristol" & CNTY_NAME == "KENOSHA"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF BLOOMFIELD WALWORTH"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "BLOOMFIELD" & CNTY_NAME == "WALWORTH" & CTV == "V"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "TOWN OF BLOOMFIELD WALWORTH"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "BLOOMFIELD" & CNTY_NAME == "WALWORTH" & CTV == "T"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF WAUPUN FOND DU LAC"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Waupun" & CNTY_NAME == "FOND_DU_LAC"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "TOWN OF MCMILLAN MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "MCMILLAN" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF MARSHFIELD MARATHON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Marshfield" & CNTY_NAME == "MARATHON"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF ROCKLAND LA CROSSE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Rockland" & CNTY_NAME == "LA_CROSSE"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF MAPLE BLUFF DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Maple Bluff" & CNTY_NAME == "DANE"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF ONTARIO VERNON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Ontario" & CNTY_NAME == "VERNON"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF PULASKI BROWN"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Pulaski" & CNTY_NAME == "BROWN"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "TOWN OF MADISON DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Madison" & CNTY_NAME == "DANE" & CTV == "T"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF MADISON DANE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Madison" & CNTY_NAME == "DANE" & CTV == "C"]))

###
# Places that fail before #
###

# Pulaski OCONTO apparently doesn't have any people in it
ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF PULASKI OCONTO"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Pulaski" & CNTY_NAME == "OCONTO"]))

# CITY OF NEW LONDON JUST DOESN'T EXIST IN VERIFIED VOTING DATA. I SUBBED IN THE OFFICIAL WISC ELEC BOARD STATS
ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF NEW LONDON OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "New London" & CNTY_NAME == "OUTAGAMIE"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF NEW LONDON WAUPACA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "New London" & CNTY_NAME == "WAUPACA"]))

# Milwaukee districts outside of Milwaukee. Maybe all have a population of 0
ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF MILWAUKEE WAUKESHA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Milwaukee" & CNTY_NAME == "WAUKESHA"]))

ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF MILWAUKEE WASHINGTON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Milwaukee" & CNTY_NAME == "WASHINGTON"]))

# Rockland Monroe just doesn't exist. 0 population?
ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF ROCKLAND MONROE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Rockland" & CNTY_NAME == "MONROE"]))

# Ontario Monroe doesn't exist in my db. 0 population?
ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF ONTARIO MONROE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Ontario" & CNTY_NAME == "MONROE"]))

# Wisconsin Dells Juneau doesn't exist in my db. Maybe a 0 population.
ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF WISCONSIN DELLS JUNEAU"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Wisconsin Dells" & CNTY_NAME == "JUNEAU"]))

# Harrison Outagamie doesn't exist. Probably 0 population in Outagamie
ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "TOWN OF HARRISON OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "HARRISON" & CNTY_NAME == "OUTAGAMIE"]))

# Hartford Dodge doesn't exist, as 0 people live there
ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF HARTFORD DODGE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Hartford" & CNTY_NAME == "DODGE" & CTV == "C"]))

# Almost all of the city live in the Columbia side of the party
ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF COLUMBUS DODGE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Columbus" & CNTY_NAME == "DODGE"]))

# Kewaskum fond du lac doesn't exist - all population is in Washington
ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF KEWASKUM FOND DU LAC"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Kewaskum" & CNTY_NAME == "FOND_DU_LAC"]))

# All residents of Howard live in the Brown side of the town
ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF HOWARD OUTAGAMIE"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Howard" & CNTY_NAME == "OUTAGAMIE"]))

# The Calumet part of Kaukauna has a population of 0
ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF KAUKAUNA CALUMET"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Kaukauna" & CNTY_NAME == "CALUMET"]))

# Genoa city, Kenosha part only has 6 residents
ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF GENOA KENOSHA"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Genoa" & CNTY_NAME == "KENOSHA"]))####

# Lac La Belle, Jefferson, has only ONE resident. The Waukesha side has the rest (289)
ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "VILLAGE OF LAC LA BELLE JEFFERSON"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Lac La Belle" & CNTY_NAME == "JEFFERSON"]))####

# The Bayfield side of the town has 0 residents
ward.2016.2012.r$MCD_FIPS[ward.2016.2012.r$muni_county == "CITY OF ASHLAND BAYFIELD"] =
  with(wisc.spat.df, unique(MCD_FIPS[MCD_NAME == "Ashland" & CNTY_NAME == "BAYFIELD"]))


# Milwaukee city in other counties doesn't exist

####
####

####
# Joining data

wisc.spat.j = wisc.spat.d.df

# Joining data together
wisc.spat.j@data = join(wisc.spat.d.df@data, ward.2016.2012.r, by = "MCD_FIPS", match = "first")


# Geojsons fail if there is an 'NAN' in the data
wisc.spat.j@data[is.na(wisc.spat.j@data)] <- "NA"

wisc.spat.j.point = SpatialPointsDataFrame(wisc.spat.j,wisc.spat.j@data)

# plot(wisc.spat.j)
wisc.spat.j.df = as.data.frame(wisc.spat.j)
writeOGR(
  wisc.spat.j,
  "wisc.spat.rcnt.geojson",
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



# Ward level data 2012 and 2016 original repunit ----------------------------------------------------
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
  
  # ward.digits.2016 = ward.2016
  # ward.digits.2016 = subset(ward.digits.2016, original_or_recount == "Original")
  # 
  # # No differences in digits ending in 0 or 5 between Trump and Clinton
  # 
  # ward.digits.2016$trump.digit = substrRight(ward.digits.2016$donald_j_trump, 1)
  # ward.digits.2016$trump.digit = as.integer(ward.digits.2016$trump.digit)
  # trump.dig.table = table(ward.digits.2016$trump.digit) / length(ward.digits.2016$trump.digit)
  # 
  # ward.digits.2016$clinton.digit = substrRight(ward.digits.2016$hillary_clinton, 1)
  # ward.digits.2016$clinton.digit = as.integer(ward.digits.2016$clinton.digit)
  # clinton.dig.table = table(ward.digits.2016$clinton.digit) / length(ward.digits.2016$clinton.digit)
  # 
  # # No difference in digits ending in 0 or 5 for Stein
  # 
  # ward.digits.2016.stein = subset(ward.digits.2016, jill_stein > 9)
  # ward.digits.2016.stein$stein.digit = substrRight(ward.digits.2016.stein$jill_stein, 1)
  # ward.digits.2016.stein$stein.digit = as.integer(ward.digits.2016.stein$stein.digit)
  # stein.dig.table = table(ward.digits.2016.stein$stein.digit) / length(ward.digits.2016.stein$stein.digit)
  # table(ward.digits.2016.stein$stein.digit)
  # 
  # ward.digits.2016$stein.digit = substrRight(ward.digits.2016$jill_stein, 1)
  # ward.digits.2016$stein.digit = as.integer(ward.digits.2016$stein.digit)
  # stein.dig.table = table(ward.digits.2016$stein.digit) / length(ward.digits.2016$stein.digit)
  # table(ward.digits.2016$stein.digit)
  
  
  ward.2016.v = ward.2016
  ward.2016.v$county_name = NULL
  ward.2016.v$municipality_name = NULL
  ward.2016.v$total_votes = NULL
  # ward.2016.v$reporting_unit = NULL
  
  ward.2016.r = subset(ward.2016.v, original_or_recount == "Recount")
  ward.2016.o = subset(ward.2016.v, original_or_recount == "Original")
  
  ward.2016.r$original_or_recount = NULL
  ward.2016.o$original_or_recount = NULL
  
  ward.2016.o.l = gather(ward.2016.o, reporting_unit);
  colnames(ward.2016.o.l) = c("reporting_unit", "cand", "votes.rec")
  
  unique(ward.2016.o.l$cand)
  unique(ward.2016.o.l$cand.group)
  
  ward.2016.o.l$cand.group = NA
  ward.2016.o.l$cand.group[grep("hillary",ward.2016.o.l$cand)] = "democrat"
  ward.2016.o.l$cand.group[grep("donald",ward.2016.o.l$cand)] = "republican"
  ward.2016.o.l$cand.group[grep("darrell",ward.2016.o.l$cand)] = "other"
  ward.2016.o.l$cand.group[grep("rocky",ward.2016.o.l$cand)] = "other"
  ward.2016.o.l$cand.group[grep("gary",ward.2016.o.l$cand)] = "other"
  ward.2016.o.l$cand.group[grep("monica",ward.2016.o.l$cand)] = "other"
  ward.2016.o.l$cand.group[grep("jill",ward.2016.o.l$cand)] = "other"
  
  # There were a bunch of write-in names for the ward level results
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
  ward.2016.o.l = join(ward.2016.o.l, ward.2016.look, by = "reporting_unit", match = "first")
  
  ward.2016.o.l$votes.rec = as.numeric(as.integer(ward.2016.o.l$votes.rec))
  # table(is.na(ward.2016.o.l$votes.rec))
  
  tot.votes.look.group = group_by(ward.2016.o.l,reporting_unit)
  tot.votes.look = dplyr::summarise(tot.votes.look.group,
                                    votes.tot = sum(votes.rec,na.rm=T)
  )
  ward.2016.o.l = join(ward.2016.o.l,tot.votes.look,by="reporting_unit",match="first")
  ward.2016.o.l$votes.perc = (ward.2016.o.l$votes.rec / ward.2016.o.l$votes.tot) * 100
  
  ward.2016.o.l$muni_county = paste(ward.2016.o.l$municipality,
                                    ward.2016.o.l$county)
  
  ward.2016.o.mun.group = group_by(ward.2016.o.l,reporting_unit, municipality,muni_county,county,cand.group)
  ward.2016.o.mun = dplyr::summarise(ward.2016.o.mun.group,
                                     votes.rec2 = sum(votes.rec),
                                     votes.tot2 = mean(votes.tot)
  )
  colnames(ward.2016.o.mun)[6] = "votes.rec"
  colnames(ward.2016.o.mun)[7] = "votes.tot"
  
  ward.2016.o.mun$votes.perc = (ward.2016.o.mun$votes.rec / ward.2016.o.mun$votes.tot) * 100
  
  ward.2016.o.mun = ward.2016.o.mun[!is.na(ward.2016.o.mun$cand.group),]
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
#   ward.2016.regvote.mun.group = group_by(ward.2016.regvote,reporting_unit,muni_county,municipality,county)
#   ward.2016.regvote.mun = dplyr::summarise(ward.2016.regvote.mun.group,
#                                            voter_count2 = sum(voter_count)
#   )
#   colnames(ward.2016.regvote.mun)[4] = "voters.tot"
#   
#   
#   
#   ###
#   # Seeing what is differnt according to muni county level
#   ###
#   voter.reg.mun = data.frame(unique(ward.2016.regvote.mun$muni_county))
#   votes.mun = data.frame(unique(ward.2016.o.l$muni_county))
#   colnames(voter.reg.mun) = "voter.reg.mun"
#   colnames(votes.mun) = "votes.mun"
#   
#   # Ugh, I need to change the name of these muni_counties to make sure I have a full match
#   setdiff(unique(voter.reg.mun$voter.reg.mun), unique(votes.mun$votes.mun))
#   setdiff(unique(votes.mun$votes.mun), unique(voter.reg.mun$voter.reg.mun))
#   
#   differences = setdiff(unique(votes.mun$votes.mun),unique(voter.reg.mun$voter.reg.mun))
#   voter.reg.diffs = subset(ward.2016.o.l, muni_county %in% differences)
#   
#   voter.reg.diffs.join = with(voter.reg.diffs, data.frame(municipality, county, muni_county, reporting_unit))
#   voter.reg.diffs.join = voter.reg.diffs.join[!duplicated(voter.reg.diffs.join),]
#   
#   ward.2016.regvote$ward = gsub(" -","",ward.2016.regvote$ward)
#   
#   ###
#   # Seeing what is different according to ward level
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
#   setdiff(unique(votes.mun$votes.mun), unique( ward.2016.regvote$muni_county))
#   
#   ## Joining the reg.vote summarised municipalities back onto the voting municipalities
#   ward.2016.regvote.join = with(ward.2016.regvote, data.frame(municipality,muni_county))
#   colnames(ward.2016.regvote.join)[2] = "muni_county_regvote_agreed"
#   ward.2016.regvote.join = ward.2016.regvote.join[!duplicated(ward.2016.regvote.join$muni_county_regvote),]
#   #
#   # voter.reg.diffs.join = join(voter.reg.diffs.join,ward.2016.regvote.join,by = "municipality")
#   #
#   # back.to.votes.df.join = with(voter.reg.diffs.join, data.frame(muni_county,muni_county_regvote_agreed))
#   #
#   # ward.2016.o.l = join(ward.2016.o.l, back.to.votes.df.join, by = "muni_county")
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
#   ward.2016.regvote.votes.group = group_by(ward.2016.regvote,muni_county_regvote_agreed)
#   ward.2016.regvote.votes.g = dplyr::summarise(ward.2016.regvote.votes.group,
#                                                voter_count2 = sum(voter_count)
#   )
#   colnames(ward.2016.regvote.votes.g)[2] = "voters.tot"
#   
#   # Trying to match by ward level
#   ward.2016.o.l = join(ward.2016.o.l, ward.2016.regvote.votes.g, by = "muni_county_regvote_agreed")
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
#   ### Joining this back to ward.2016.o.mun
#   ward.2016.o.l.join = with(ward.2016.o.l,
#                             data.frame(
#                               muni_county,voters.tot
#                             ))
#   colnames(ward.2016.o.l.join)[1] = "muni_county"
#   ward.2016.o.l.join = ward.2016.o.l.join[!duplicated(ward.2016.o.l.join$muni_county),]
#   
#   ward.2016.o.mun = join(ward.2016.o.mun, ward.2016.o.l.join, by ="muni_county", match = "first")
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
  
  ward.12.l = gather(ward.12.v, reporting_unit);
  colnames(ward.12.l) = c("reporting_unit", "cand", "votes.rec")
  
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
  ward.12.l = join(ward.12.l, ward.12.look, by = "reporting_unit", match = "first")
  
  tot.votes.look.group = group_by(ward.12.l,reporting_unit)
  tot.votes.look = dplyr::summarise(tot.votes.look.group,
                                    votes.tot = sum(as.integer(votes.rec),na.rm=T)
  )
  
  ward.12.l = join(ward.12.l,tot.votes.look,by="reporting_unit")
  ward.12.l$votes.perc = (ward.12.l$votes.rec / ward.12.l$votes.tot) * 100
  
  ward.12.l$muni_county = paste(ward.12.l$municipality, ward.12.l$county)
  
  ward.12.mun.group = group_by(ward.12.l,reporting_unit,municipality,muni_county,county,cand.group)
  ward.12.mun = dplyr::summarise(ward.12.mun.group,
                                 votes.rec2 = sum(votes.rec,na.rm=T),
                                 votes.tot2 = mean(votes.tot,na.rm=T)
  )
  colnames(ward.12.mun)[6] = "votes.rec"
  colnames(ward.12.mun)[7] = "votes.tot"
  
  ward.12.mun$votes.perc = (ward.12.mun$votes.rec / ward.12.mun$votes.tot) * 100
  
  ward.12.mun = ward.12.mun[!is.na(ward.12.mun$cand.group),]
} # 2012 data basic
{  # Now joining the 2012 data back into the 2016 data
  grep("WARDS",ward.2016.o.mun$reporting_unit)
  
  ward.2016.o.mun$reporting_unit_orig =ward.2016.o.mun$reporting_unit
  ward.12.mun$reporting_unit_orig =ward.12.mun$reporting_unit 
  
  ward.2016.o.mun$reporting_unit = gsub("WARDS","WARD",ward.2016.o.mun$reporting_unit_orig)
  ward.12.mun$reporting_unit = gsub("WARDS","WARD",ward.12.mun$reporting_unit_orig)
  ward.2016.o.mun$reporting_unit = gsub("  "," ",ward.2016.o.mun$reporting_unit)
  ward.12.mun$reporting_unit = gsub("  "," ",ward.12.mun$reporting_unit)
  ward.2016.o.mun$reporting_unit = gsub("&","-",ward.2016.o.mun$reporting_unit)
  ward.12.mun$reporting_unit = gsub("&","-",ward.12.mun$reporting_unit)
  
  ward.2016.o.mun$reporting_unit = gsub("AND","-",ward.2016.o.mun$reporting_unit)
  ward.12.mun$reporting_unit = gsub("AND","-",ward.12.mun$reporting_unit)
  
  ward.2016.o.mun$reporting_unit = gsub(",","-",ward.2016.o.mun$reporting_unit)
  ward.12.mun$reporting_unit = gsub(",","-",ward.12.mun$reporting_unit)
  ward.2016.o.mun$reporting_unit = gsub(" - ","-",ward.2016.o.mun$reporting_unit)
  ward.12.mun$reporting_unit = gsub(" - ","-",ward.12.mun$reporting_unit)
  ward.2016.o.mun$reporting_unit = gsub(" -","-",ward.2016.o.mun$reporting_unit)
  ward.12.mun$reporting_unit = gsub(" -","-",ward.12.mun$reporting_unit)
  ward.2016.o.mun$reporting_unit = gsub("- ","-",ward.2016.o.mun$reporting_unit)
  ward.12.mun$reporting_unit = gsub("- ","-",ward.12.mun$reporting_unit)
  
  ward.2016.o.mun$reporting_unit = gsub(" ","-",ward.2016.o.mun$reporting_unit)
  ward.12.mun$reporting_unit = gsub(" ","-",ward.12.mun$reporting_unit)
  
  ward.2016.o.mun$reporting_unit = gsub("WD","",ward.2016.o.mun$reporting_unit)
  ward.12.mun$reporting_unit = gsub("WD","",ward.12.mun$reporting_unit)
  
  ward.2016.o.mun$reporting_unit = gsub("COMBINED","",ward.2016.o.mun$reporting_unit)
  ward.12.mun$reporting_unit = gsub("COMBINED","",ward.12.mun$reporting_unit)
  
  ward.2016.o.mun$reporting_unit = gsub("--","-",ward.2016.o.mun$reporting_unit)
  ward.12.mun$reporting_unit = gsub("--","-",ward.12.mun$reporting_unit)
  
  setdiff(unique(ward.12.mun$reporting_unit), unique(ward.2016.o.mun$reporting_unit))
  setdiff(unique(ward.2016.o.mun$reporting_unit), unique(ward.12.mun$reporting_unit))
  
  # Village of Harrison Calumet did not exist in 2012, was split off from the town in 2013.
  # It is impossible to know the boundaries exactly.
  # Therefore the swing from 2012 to 2016 cannot be accurately calculated.
  
  # Same of the town / village of Somers, Kenosha. The town / village is completely mixed and messed up
  
  # the town of Menasha was retained only for wards (8-10, 11-13). The rest (1-7) became the village of
  # Fox Crossing, Winnebago
  
  differences = setdiff(unique(ward.2016.o.mun$reporting_unit),unique(ward.12.mun$reporting_unit))
  vote.2012.diffs = subset(ward.2016.o.mun, reporting_unit %in% differences)
  
  vote.2012.diffs.join = with(vote.2012.diffs, data.frame(municipality, county, reporting_unit))
  vote.2012.diffs.join = vote.2012.diffs.join[!duplicated(vote.2012.diffs.join),]
  
  # ## Joining the reg.vote summarised municipalities back onto the voting municipalities
  # ward.12.mun.join = with(ward.12.mun, data.frame(municipality,reporting_unit))
  # colnames(ward.12.mun.join)[2] = "reporting_unit_2012_agreed"
  # ward.12.mun.join = ward.12.mun.join[!duplicated(ward.12.mun.join$reporting_unit_2012),]
  # 
  # 
  # 
  # vote.2012.diffs.join = join(vote.2012.diffs.join,ward.12.mun.join,by = "reporting_unit")
  # 
  # back.to.votes.df.join = with(vote.2012.diffs.join, data.frame(reporting_unit,reporting_unit_2012_agreed))
  # 
  # ward.2016.o.mun = join(ward.2016.o.mun, back.to.votes.df.join, by = "reporting_unit")
  # 
  # ward.2016.o.mun$reporting_unit_2012_agreed = as.character(ward.2016.o.mun$reporting_unit_2012_agreed)
  # 
  # ward.2016.o.mun$reporting_unit_2012_agreed = ifelse(is.na(ward.2016.o.mun$reporting_unit_2012_agreed),
  #                                                  ward.2016.o.mun$reporting_unit,ward.2016.o.mun$reporting_unit_2012_agreed)
  # 
  ## Now renaming the 2012 column for the final join
  # colnames(ward.12.mun)[2] = "muni_county_2012_agreed"
  
  ## Need to sum up the votes by county in 2012
  ward.12.mun.votes.group = group_by(ward.12.mun,cand.group,reporting_unit,reporting_unit_orig,municipality, county)
  ward.12.mun.votes.g = dplyr::summarise(ward.12.mun.votes.group,
                                         votes.rec2012 = sum(votes.rec),
                                         votes.tot2012 = mean(votes.tot)
                                         
  )
  
  # Melting the 2012 df to put the candidates on top
  ward.12.mun.votes.g.melt = melt(ward.12.mun.votes.g)
  ward.12.mun.votes.g.cast = dcast(ward.12.mun.votes.g.melt,
                                   reporting_unit ~ cand.group + variable, sum
  )
  ward.12.mun.votes.g.cast$democrat_votes.tot2012 = NULL
  ward.12.mun.votes.g.cast$republican_votes.tot2012 = NULL
  
  ward.12.mun.votes.g.cast.mean = dcast(ward.12.mun.votes.g.melt,
                                        reporting_unit ~ cand.group + variable, mean
  )
  ward.12.mun.votes.g.cast$other_votes.tot2012 = ward.12.mun.votes.g.cast.mean$other_votes.tot2012
  colnames(ward.12.mun.votes.g.cast)[grepl("votes.tot",colnames(ward.12.mun.votes.g.cast))] =
    "votes.tot2012"
  
  # Also need to melt the 2016 mun df by cand.group
  ward.2016.o.mun.votes.g.melt = melt(ward.2016.o.mun)
  ward.2016.o.mun.votes.g.cast = dcast(ward.2016.o.mun.votes.g.melt,
                                       reporting_unit + reporting_unit_orig + muni_county + municipality + county ~
                                         cand.group + variable, sum
  )
  
  ward.2016.o.mun.votes.g.cast$democrat_votes.tot = NULL
  ward.2016.o.mun.votes.g.cast$republican_votes.tot = NULL
  ward.2016.o.mun.votes.g.cast$democrat_voters.tot = NULL
  ward.2016.o.mun.votes.g.cast$republican_voters.tot = NULL
  
  ward.2016.o.mun.votes.g.cast.mean = dcast(ward.2016.o.mun.votes.g.melt,
                                            reporting_unit + reporting_unit_orig + muni_county + municipality + county ~
                                        cand.group + variable, mean
  )
  ward.2016.o.mun.votes.g.cast$other_votes.tot = ward.2016.o.mun.votes.g.cast.mean$other_votes.tot
  ward.2016.o.mun.votes.g.cast$other_voters.tot = ward.2016.o.mun.votes.g.cast.mean$other_voters.tot
  colnames(ward.2016.o.mun.votes.g.cast)[grepl("votes.tot",colnames(ward.2016.o.mun.votes.g.cast))] =
    "votes.tot2016"
  colnames(ward.2016.o.mun.votes.g.cast)[grepl("voters.tot",colnames(ward.2016.o.mun.votes.g.cast))] =
    "voters.tot2016"
  
  # Finally joining the dfs together
  
  ward.2016.o.mun.g = join(ward.2016.o.mun.votes.g.cast, ward.12.mun.votes.g.cast, by = "reporting_unit")
  
  # 2012 turnout is calculated on the (big?) assumption that the number of registered
  # voters was the same in 2012 as in 2016
  # ward.2016.o.mun.g$turnout.2012 =
    # with(ward.2016.o.mun.g, votes.tot2012 / voters.tot2016) * 100
  
  ward.2016.o.mun.g$dem.perc2012 =
    with(ward.2016.o.mun.g,
         democrat_votes.rec2012 / votes.tot2012) * 100
  ward.2016.o.mun.g$rep.perc2012 =
    with(ward.2016.o.mun.g,
         republican_votes.rec2012 / votes.tot2012) * 100
  ward.2016.o.mun.g$oth.perc2012 =
    with(ward.2016.o.mun.g,
         other_votes.rec2012 / votes.tot2012) * 100
  
  ward.2016.o.mun.g$dem.change.num =
    with(ward.2016.o.mun.g,
         democrat_votes.rec - democrat_votes.rec2012)
  ward.2016.o.mun.g$rep.change.num =
    with(ward.2016.o.mun.g,
         republican_votes.rec - republican_votes.rec2012)
  ward.2016.o.mun.g$oth.change.num =
    with(ward.2016.o.mun.g,
         other_votes.rec - other_votes.rec2012)
  
  ward.2016.o.mun.g$dem.change.perc =
    with(ward.2016.o.mun.g,
         democrat_votes.perc -  dem.perc2012)
  ward.2016.o.mun.g$rep.change.perc =
    with(ward.2016.o.mun.g,
         republican_votes.perc - rep.perc2012)
  ward.2016.o.mun.g$oth.change.perc =
    with(ward.2016.o.mun.g,
         other_votes.perc - oth.perc2012)
  # 
  # ward.2016.o.mun.g$turnout.2016.perc =
  #   with(ward.2016.o.mun.g,
  #        votes.tot2016 / voters.tot2016) * 100
  
  # ward.2016.o.mun.g$turnout.change.perc =
  #   with(ward.2016.o.mun.g,
  #        turnout.2016.perc - turnout.2012)
  
  
  repunit.2016.2012.o = ward.2016.o.mun.g
  
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

# Rejoining the county data to the ward 2016 2012 data
{
  repunit.2016.2012.o = join(repunit.2016.2012.o, ward.2016.look, by = "muni_county", match = "first")
}

# Loading voting machine data original repunit (Wisconsin official) ---------------------------------------------
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

vot.equip.county.grouped = group_by(vot.equip, county)
vot.equip.county = dplyr::summarise(
  vot.equip.county.grouped,
  use.machines.prop = sum(use.machines, na.rm = TRUE) /
    length(use.machines)
)

# vot.equip.withmachines = subset(vot.equip, use.machines > 0)
vot.equip.withmachines = vot.equip

vot.equip.county.grouped.machines = group_by(vot.equip.withmachines, county)
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
vot.equip.county = join(vot.equip.county,
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



repunit.2016.2012.o$muni_county = repunit.2016.2012.o$muni_county

repunit.2016.2012.o$muni_county[grep("VILLAGE OF WRIGHTSTOWN",
                                               repunit.2016.2012.o$muni_county)] = "VILLAGE OF WRIGHTSTOWN BROWN"
repunit.2016.2012.o$muni_county[grep("TOWN OF MAINE",
                                               repunit.2016.2012.o$muni_county)] = "TOWN OF MAINE MARATHON"
colnames(vot.equip.join)[1] = "muni_county"

setdiff(vot.equip.join$muni_county,
        repunit.2016.2012.o$muni_county)

repunit.2016.2012.o = join(repunit.2016.2012.o,vot.equip.join, by = "muni_county")
repunit.2016.2012.o$use.machines[is.na(repunit.2016.2012.o$use.machines)] = 0

colnames(repunit.2016.2012.o)[grep("use.machines",colnames(repunit.2016.2012.o))] = "county_use_opt_scan"
colnames(repunit.2016.2012.o)[grep("machine.vendor.dealer.spec",colnames(repunit.2016.2012.o))] = "county_machine_vendor_dealer"

####
# doing the same for the 2016 2012 data
####
county.machines.join = with(repunit.2016.2012.o, data.frame(
  muni_county, county_use_opt_scan, county_machine_vendor_dealer, machine.vendor.dealer.model, accessible.vendor.dealer.model
))

ward.2016.2012.test = join(repunit.2016.2012.o, county.machines.join, by = "muni_county", match = "first")

repunit.2016.2012.o = ward.2016.2012.test
repunit.2016.2012.o$county_use_opt_scan[is.na(repunit.2016.2012.o$county_use_opt_scan)] = 0


# Loading in spatial data for original repunit data -------------------------------------------------
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

lu.grp = group_by(wisc.spat@data, MCD_FIPS, MCD_NAME, CNTY_NAME, CNTY_FIPS, CTV)
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
wisc.spat.j@data = join(wisc.spat.d.df@data, repunit.2016.2012.o, by = "MCD_FIPS", match = "first")


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




# Reading in county historical vote data ----------------------------------------------------
## General overview of Wisconsin elections from 1900 onwards
url <- "http://uselectionatlas.org/RESULTS/compare.php?year=2016&fips=55&f=0&off=0&elect=0&type=state"
voting.history <- url %>%
  read_html() %>%
  html_nodes(xpath='//table[@id="data"]') %>%
  html_table(fill=T)

elections.100.years = data.frame(1:44)
elections.100.years$year = unlist(lapply(voting.history, '[[', 'X3'))
elections.100.years$year = as.integer(elections.100.years$year)
elections.100.years$turnout = unlist(lapply(voting.history, '[[', 'X4'))
elections.100.years$turnout = as.integer(gsub(",", "", elections.100.years$turnout))
elections.100.years$dem.vote.perc = unlist(lapply(voting.history, '[[', 'X10'))
elections.100.years$dem.vote.perc = as.numeric(gsub("%", "", elections.100.years$dem.vote.perc))
elections.100.years$rep.vote.perc = unlist(lapply(voting.history, '[[', 'X11'))
elections.100.years$rep.vote.perc = as.numeric(gsub("%", "", elections.100.years$rep.vote.perc))

# Independents and others
elections.100.years$ind.vote.perc = unlist(lapply(voting.history, '[[', 'X12'))
elections.100.years$ind.vote.perc = as.numeric(gsub("%", "", elections.100.years$ind.vote.perc))
elections.100.years$oth.vote.perc = unlist(lapply(voting.history, '[[', 'X13'))
elections.100.years$oth.vote.perc = as.numeric(gsub("%", "", elections.100.years$oth.vote.perc))

elections.100.years$dem.vote = unlist(lapply(voting.history, '[[', 'X14'))
elections.100.years$dem.vote = as.integer(gsub(",", "", elections.100.years$dem.vote))
elections.100.years$rep.vote = unlist(lapply(voting.history, '[[', 'X15'))
elections.100.years$rep.vote = as.integer(gsub(",", "", elections.100.years$rep.vote))

elections.100.years$ind.vote = unlist(lapply(voting.history, '[[', 'X16'))
elections.100.years$ind.vote = as.integer(gsub(",", "", elections.100.years$ind.vote))
elections.100.years$oth.vote = unlist(lapply(voting.history, '[[', 'X17'))
elections.100.years$oth.vote = as.integer(gsub(",", "", elections.100.years$oth.vote))

elections.100.years$X1.44 = NULL
elections.100.years = elections.100.years[-c(1:3), ]

### Writing the 2016 results from my other input file, because the numbers there aren't quite the same
elections.100.years$rep.vote.perc[1] = election.2016.grouped$votes.perc[1]
elections.100.years$dem.vote.perc[1] = election.2016.grouped$votes.perc[2]
elections.100.years$oth.vote.perc[1] = sum(election.2016.grouped$votes.perc[3:6])
elections.100.years$ind.vote.perc[1] = election.2016.grouped$votes.perc[7]

elections.100.years$rep.vote[1] = election.2016.grouped$votes.rec[1]
elections.100.years$dem.vote[1] = election.2016.grouped$votes.rec[2]
elections.100.years$oth.vote[1] = sum(election.2016.grouped$votes.rec[3:6])
elections.100.years$ind.vote[1] = election.2016.grouped$votes.rec[7]

# Total others = independents + others
elections.100.years$tot.oth.vote.perc = elections.100.years$ind.vote.perc + elections.100.years$oth.vote.perc
elections.100.years$tot.oth.vote = elections.100.years$ind.vote + elections.100.years$oth.vote


elections.100.years$vote.perc.diff = elections.100.years$rep.vote.perc - elections.100.years$dem.vote.perc
elections.100.years$vote.num.diff = elections.100.years$rep.vote - elections.100.years$dem.vote
elections.100.years$winner = with(elections.100.years, ifelse(vote.perc.diff < 0, "Dem", "Rep"))

## File reference not working?
# From here: https://www.google.es/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=0ahUKEwjChuux5qfQAhWCrxoKHXcKDqYQFggdMAA&url=http%3A%2F%2Felections.wi.gov%2Fsites%2Fdefault%2Ffiles%2Fpage%2Fvoter_turnout_partisan_nonpartisan_xlsx_13632.xlsx&usg=AFQjCNHNxn4e1xNCCjaLXekHxuAKT_dFxg
historical.turnout = read.xlsx(
  file = "voter_turnout_partisan_nonpartisan_xlsx_13632.xlsx",
  sheetIndex = 1,
  header = TRUE,
  colClasses = NA,
  stringsAsFactors = F
)
colnames(historical.turnout) = tolower(colnames(historical.turnout))
historical.turnout.join = with(historical.turnout, data.frame(year, voting.age.population))
elections.100.years = join(elections.100.years, historical.turnout.join, by = "year")
elections.100.years$turnout.perc = with(elections.100.years, (turnout / voting.age.population)) * 100

elections.100.years$swing.turnout.perc = NA
for (i in (2:(length(elections.100.years$year) - 1))) {
  elections.100.years$swing.turnout.perc[i - 1] =  -(elections.100.years$turnout.perc[i] - elections.100.years$turnout.perc[i -1])
  elections.100.years$swing.vote.num[i - 1] =  -(elections.100.years$vote.num.diff[i] - elections.100.years$vote.num.diff[i - 1])
  elections.100.years$swing.vote.perc[i - 1] =  -(elections.100.years$vote.perc.diff[i] - elections.100.years$vote.perc.diff[i - 1])
  elections.100.years$swing.vote.dem.change.perc[i - 1] =  (elections.100.years$dem.vote.perc[i-1] - elections.100.years$dem.vote.perc[i])
  elections.100.years$swing.vote.dem.change.num[i - 1] =  (elections.100.years$dem.vote.num[i-1] - elections.100.years$dem.vote.num[i])
  elections.100.years$swing.vote.rep.change.perc[i - 1] =  (elections.100.years$rep.vote.perc[i-1] - elections.100.years$rep.vote.perc[i])
  elections.100.years$swing.vote.rep.change.num[i - 1] =  (elections.100.years$rep.vote.num[i-1] - elections.100.years$rep.vote.num[i])
  elections.100.years$swing.vote.tot.oth.change.perc[i - 1] =  (elections.100.years$tot.oth.vote.perc[i-1] - elections.100.years$tot.oth.vote.perc[i])
  elections.100.years$swing.vote.tot.oth.change.num[i - 1] =  (elections.100.years$tot.oth.vote.num[i-1] - elections.100.years$tot.oth.vote.num[i])
}
elections.100.years.postwar = subset(elections.100.years, year >= 1948)




# The following variables will be used when I scrape the polls from HuffPost API later
elections.100.years$mean.rep.poll.oneweek = NA
elections.100.years$mean.dem.poll.oneweek = NA
elections.100.years$mean.rep.poll.oneweek.divergence = NA
elections.100.years$mean.dem.poll.oneweek.divergence = NA

# Downloading county 2016 and 2012 polls ------------------------------------------------------
# Loading poll data for Wisconsin

# Gathering poll data scraping instructions from here: https://pkremp.github.io/report.html
# rm(list = ls())
library(rstan)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(curl)
library(shinystan)
library(rmarkdown)

# Useful functions #
corr_matrix <- function(m) {
  (diag(m) ^ -.5 * diag(nrow = nrow(m))) %*% m %*% (diag(m) ^ -.5 * diag(nrow = nrow(m)))
}

cov_matrix <- function(n, sigma2, rho) {
  m <- matrix(nrow = n, ncol = n)
  m[upper.tri(m)] <- rho
  m[lower.tri(m)] <- rho
  diag(m) <- 1
  (sigma2 ^ .5 * diag(n))  %*% m %*% (sigma2 ^ .5 * diag(n))
}

logit <- function(x)
  log(x / (1 - x))
inv_logit <- function(x)
  1 / (1 + exp(-x))

download_csv <- function(stub) {
  url <-
    paste("http://elections.huffingtonpost.com/pollster/",
          stub,
          ".csv",
          sep = "")
  connection <- curl(url, "r")
  df <- read.csv(connection, stringsAsFactors = FALSE)
  close(connection)
  message("Downloaded ", url)
  return(df)
}

## Downloading poll data from the HuffPost Pollster API ##

# Creating a vector of URL stubs, to fetch csv files for each state from HuffPost
# NOTE: I don't need all the states names as I am only interested in Wisconsin
# state_name <- datasets::state.name
# names(state_name) <- datasets::state.abb

{ # Getting the 2016 poll data
stubs_2016 <- c(
  "2016-general-election-trump-vs-clinton",
  "2016-wisconsin-president-trump-vs-clinton"
)
names(stubs_2016) <- c("--", "WI")
stubs_2016 <- stubs_2016[order(names(stubs_2016))]

start_date_2016 <-
  as.Date("2016-10-01") # Keeping all polls after October 1.

# Download the data and put everything in a single df
all_polls_2016 <- map_df(stubs_2016, download_csv, .id = "state")
colnames(all_polls_2016) <- colnames(all_polls_2016) %>% tolower

# I'm only interested in the overall likely voters, not partisan voters
all_polls_2016 = subset(
  all_polls_2016,
  population == "Likely Voters" | population == "Registered Voters" |
    population == "Adults"
)

all_polls_2016$start.date = as.Date(all_polls_2016$start.date)
all_polls_2016$other.nonas = ifelse(is.na(all_polls_2016$other),0,all_polls_2016$other)

all_polls_2016$decided.total = with(all_polls_2016, trump + clinton + other.nonas)
all_polls_2016$trump.defs = with(all_polls_2016, trump / decided.total) * 100
all_polls_2016$clinton.defs = with(all_polls_2016, clinton / decided.total) * 100
all_polls_2016$other.defs = with(all_polls_2016, other / decided.total) * 100

# Subsetting for polls started one week before election day. Election was on 08-11-16.
# THESE ARE ALL LIKELY VOTERS
selected_polls_2016.sub = subset(all_polls_2016, start.date > as.Date("2016-11-01"))

elections.100.years$mean.rep.poll.oneweek[elections.100.years$year == 2016] = mean(selected_polls_2016.sub$trump)
elections.100.years$mean.dem.poll.oneweek[elections.100.years$year == 2016] = mean(selected_polls_2016.sub$clinton)
elections.100.years$mean.oth.poll.oneweek[elections.100.years$year == 2016] = mean(selected_polls_2016.sub$other,na.rm = T)
elections.100.years$mean.und.poll.oneweek[elections.100.years$year == 2016] = mean(selected_polls_2016.sub$undecided,na.rm = T)

elections.100.years$mean.rep.poll.oneweek.divergence[elections.100.years$year == 2016] =
  elections.100.years$rep.vote.perc[elections.100.years$year == 2016] - elections.100.years$mean.rep.poll.oneweek[elections.100.years$year == 2016]
elections.100.years$mean.dem.poll.oneweek.divergence[elections.100.years$year == 2016] =
  elections.100.years$dem.vote.perc[elections.100.years$year == 2016] - elections.100.years$mean.dem.poll.oneweek[elections.100.years$year == 2016]
elections.100.years$mean.other.poll.oneweek.divergence[elections.100.years$year == 2016] =
  elections.100.years$tot.oth.vote.perc[elections.100.years$year == 2016] - elections.100.years$mean.oth.poll.oneweek[elections.100.years$year == 2016]
elections.100.years$mean.und.poll.oneweek.divergence[elections.100.years$year == 2016] =
  elections.100.years$tot.und.vote.perc[elections.100.years$year == 2016] - elections.100.years$mean.und.poll.oneweek[elections.100.years$year == 2016]

### Removing unsure voters from polls and reanalysing
elections.100.years$mean.rep.poll.oneweek.defs[elections.100.years$year == 2016] = mean(selected_polls_2016.sub$trump.defs,na.rm = T)
elections.100.years$mean.dem.poll.oneweek.defs[elections.100.years$year == 2016] = mean(selected_polls_2016.sub$clinton.defs,na.rm = T)
elections.100.years$mean.oth.poll.oneweek.defs[elections.100.years$year == 2016] = mean(selected_polls_2016.sub$other.defs,na.rm = T)

elections.100.years$mean.rep.poll.oneweek.defs.divergence[elections.100.years$year == 2016] =
  elections.100.years$rep.vote.perc[elections.100.years$year == 2016] - elections.100.years$mean.rep.poll.oneweek.defs[elections.100.years$year == 2016]
elections.100.years$mean.dem.poll.oneweek.defs.divergence[elections.100.years$year == 2016] =
  elections.100.years$dem.vote.perc[elections.100.years$year == 2016] - elections.100.years$mean.dem.poll.oneweek.defs[elections.100.years$year == 2016]
elections.100.years$mean.other.defs.poll.oneweek.defs.divergence[elections.100.years$year == 2016] =
  elections.100.years$tot.oth.vote.perc[elections.100.years$year == 2016] - elections.100.years$mean.oth.poll.oneweek.defs[elections.100.years$year == 2016]

# Plotting the polls over time one week before the 2016 election
polls.wisc.point.2016 = ggplot(selected_polls_2016.sub, aes(x = start.date, y = trump)) +
  geom_jitter(colour = "red") + geom_jitter(aes(y = selected_polls_2016.sub$clinton), colour = "blue") +
  geom_jitter(aes(y = selected_polls_2016.sub$other), colour = "grey") +
  geom_jitter(aes(y = selected_polls_2016.sub$und), colour = "orange")
polls.wisc.point.2016

# Plotting the polls over time one week before the 2016 election
polls.wisc.point.2016.defs = ggplot(selected_polls_2016.sub, aes(x = start.date, y = trump.defs)) +
  geom_jitter(colour = "red") + geom_jitter(aes(y = selected_polls_2016.sub$clinton.defs), colour = "blue") +
  geom_jitter(aes(y = selected_polls_2016.sub$other.defs), colour = "grey")
polls.wisc.point.2016.defs
} # Getting the 2016 poll data
{ ## Getting 2012 poll data
stubs_2012 <- c(
  "2012-general-election-romney-vs-obama",
  "2012-wisconsin-president-romney-vs-obama"
)
names(stubs_2012) <- c("--", "WI")
stubs_2012 <- stubs_2012[order(names(stubs_2012))]

start_date_2012 <-
  as.Date("2012-10-01") # Keeping all polls after October 1.

# Download the data and put everything in a single df
all_polls_2012 <- map_df(stubs_2012, download_csv, .id = "state")
colnames(all_polls_2012) <- colnames(all_polls_2012) %>% tolower

# I'm only interested in the overall likely voters, not partisan voters
all_polls_2012 = subset(
  all_polls_2012,
  population == "Likely Voters" | population == "Registered Voters" |
    population == "Adults"
)

all_polls_2012$start.date = as.Date(all_polls_2012$start.date)

all_polls_2012$other.nonas = ifelse(is.na(all_polls_2012$other),0,all_polls_2012$other)

all_polls_2012$decided.total = with(all_polls_2012, romney + obama + other.nonas)
all_polls_2012$romney.defs = with(all_polls_2012, romney / decided.total) * 100
all_polls_2012$obama.defs = with(all_polls_2012, obama / decided.total) * 100
all_polls_2012$other.defs = with(all_polls_2012, other / decided.total) * 100

# Subsetting for polls started one week before election day. Election was on 06-11-12
selected_polls_2012.sub = subset(all_polls_2012, start.date > as.Date("2012-10-30"))

elections.100.years$mean.rep.poll.oneweek[elections.100.years$year == 2012] = mean(selected_polls_2012.sub$romney)
elections.100.years$mean.dem.poll.oneweek[elections.100.years$year == 2012] = mean(selected_polls_2012.sub$obama)
elections.100.years$mean.oth.poll.oneweek[elections.100.years$year == 2012] = mean(selected_polls_2012.sub$other, na.rm = T)
elections.100.years$mean.und.poll.oneweek[elections.100.years$year == 2012] = mean(selected_polls_2012.sub$undecided,na.rm = T)

elections.100.years$mean.rep.poll.oneweek.divergence[elections.100.years$year == 2012] =
  elections.100.years$rep.vote.perc[elections.100.years$year == 2012] - elections.100.years$mean.rep.poll.oneweek[elections.100.years$year == 2012]
elections.100.years$mean.dem.poll.oneweek.divergence[elections.100.years$year == 2012] =
  elections.100.years$dem.vote.perc[elections.100.years$year == 2012] - elections.100.years$mean.dem.poll.oneweek[elections.100.years$year == 2012]
elections.100.years$mean.other.poll.oneweek.divergence[elections.100.years$year == 2012] =
  elections.100.years$tot.oth.vote.perc[elections.100.years$year == 2012] - elections.100.years$mean.oth.poll.oneweek[elections.100.years$year == 2012]
elections.100.years$mean.other.poll.oneweek.divergence[elections.100.years$year == 2012] =
  elections.100.years$tot.oth.vote.perc[elections.100.years$year == 2012] - elections.100.years$mean.oth.poll.oneweek[elections.100.years$year == 2012]

###
elections.100.years$mean.rep.poll.oneweek.defs[elections.100.years$year == 2012] = mean(selected_polls_2012.sub$romney.defs,na.rm = T)
elections.100.years$mean.dem.poll.oneweek.defs[elections.100.years$year == 2012] = mean(selected_polls_2012.sub$obama.defs,na.rm = T)
elections.100.years$mean.oth.poll.oneweek.defs[elections.100.years$year == 2012] = mean(selected_polls_2012.sub$other.defs,na.rm = T)

elections.100.years$mean.rep.poll.oneweek.defs.divergence[elections.100.years$year == 2012] =
  elections.100.years$rep.vote.perc[elections.100.years$year == 2012] - elections.100.years$mean.rep.poll.oneweek.defs[elections.100.years$year == 2012]
elections.100.years$mean.dem.poll.oneweek.defs.divergence[elections.100.years$year == 2012] =
  elections.100.years$dem.vote.perc[elections.100.years$year == 2012] - elections.100.years$mean.dem.poll.oneweek.defs[elections.100.years$year == 2012]
elections.100.years$mean.other.defs.poll.oneweek.defs.divergence[elections.100.years$year == 2012] =
  elections.100.years$tot.oth.vote.perc[elections.100.years$year == 2012] - elections.100.years$mean.oth.poll.oneweek.defs[elections.100.years$year == 2012]


# Plotting the polls over time one week before the 2016 election
polls.wisc.point.2012 = ggplot(selected_polls_2012.sub, aes(x = start.date, y = romney)) +
  geom_jitter(colour = "red") + geom_jitter(aes(y = selected_polls_2012.sub$obama), colour = "blue") +
geom_jitter(aes(y = selected_polls_2012.sub$other), colour = "grey") +
  geom_jitter(aes(y = selected_polls_2012.sub$und), colour = "orange") +
  scale_y_continuous(limits = c(0,55))
polls.wisc.point.2012
} # Getting the 2012 poll data


# Loading historic county data 2016 to 2000 ----------------------------------------
# http://elections.wi.gov/elections-voting/results/2000/fall-general
# http://elections.wi.gov/elections-voting/results/2004/fall-general
# http://elections.wi.gov/elections-voting/results/2008/fall-general

{ # 2016 data
county.summary.df = voting.df.2016.votecand
# colnames(county.summary.df) = "county"
county.summary.df$year = 2016

county.summary.df$perc.diff = county.summary.df$rep.votes.perc - county.summary.df$dem.votes.perc
county.summary.df$num.diff = county.summary.df$rep.votes - county.summary.df$dem.votes

county.summary.df$winner = ifelse(county.summary.df$perc.diff > 0, "Trump", "Clinton")
county.summary.df$winner.party = ifelse(county.summary.df$perc.diff > 0, "Rep", "Dem")

county.summary.df$mean.poll.trump = elections.100.years$mean.rep.poll.oneweek[elections.100.years$year == 2016]
county.summary.df$mean.poll.clinton = elections.100.years$mean.dem.poll.oneweek[elections.100.years$year == 2016]

county.summary.df$turnout.all = sum(county.summary.df$turnout)
# elections.100.years$turnout.all = sum(county.summary.df$turnout)

# Calculating turnout by county
# Calculating turnout of registered voters by November 2nd. BUT, this has been seen in past to
# increase by up to 11% on voting day, see 'general_election_voter_turnout...xlsx'. So maybe
# The turnout value here is an over-estimate

## NOTE: Here http://www.wisconsinvote.org/faq it says you can register on the day to vote,
## so maybe this registered to vote value is not accurate?
registered.voters = read.xlsx(
  "registeredvotersbycounty_xlsx_13527MODnov16.xlsx",
  sheetIndex = 1,
  stringsAsFactors = F,
  header = T
)
colnames(registered.voters) = tolower(colnames(registered.voters))

registered.voters = registered.voters[-c(73, 74), ]
registered.voters$NA. = NULL
registered.voters$county = gsub(" COUNTY.*$", "", registered.voters$county)
registered.voters$registered.voters = as.integer(registered.voters$registered.voters)

county.summary.df = county.summary.df[order(county.summary.df$county), ]
registered.voters = registered.voters[order(registered.voters$county), ]

county.summary.df$reg.voters.all = sum(registered.voters$registered.voters)
# elections.100.years$reg.voters.all = sum(registered.voters$registered.voters)

county.summary.df$registered.voters = registered.voters$registered.voters
county.summary.df$turnout.perc.reg = (county.summary.df$turnout / county.summary.df$registered.voters) * 100
county.summary.df$turnout.state.reg = (sum(county.summary.df$turnout) / sum(county.summary.df$registered.voters)) * 100
# elections.100.years$turnout.state.reg = mean(county.summary.df$turnout.state.reg)

## Calculating turnout of voting age population
## I can only estimate the turnout per county based on the 2012 voting age populations
# from file "general_election_voter_registration... this has the 2016 value"
# elections.100.years$voting.age.pop = 4449170
# elections.100.years$turnout.overall = sum(elections.100.years$votes.rec / elections.100.years$voting.age.pop)

voting.age.people = read.xlsx(
  "2012_presidential_general_election_turnout_xlsx_11916.xlsx",
  sheetIndex = 2,
  stringsAsFactors = F,
  header = T
)
voting.age.people$county = gsub(" COUNTY.*$", "", voting.age.people$County)

total.2012.pop.wi = sum(voting.age.people$Voting.Age.Estimate.2012)

# Estimated 1.01% increase in voting age population from 2012 to 2016. I will multiply the
# 2012 county voter data by 101.01% to try to account for this increase. Yes, this is an assumption,
# but newer data is not yet available!
prop.2016.voting.age.to.2012.est = mean(elections.100.years$voting.age.pop[elections.100.years$year == 2016]) / total.2012.pop.wi

voting.age.people$Voting.Age.Estimate.2016 = voting.age.people$Voting.Age.Estimate.2012 * prop.2016.voting.age.to.2012.est
voting.age.people$Voting.Age.Estimate.2016 = ceiling(voting.age.people$Voting.Age.Estimate.2016)

county.summary.df$total.voters.age.est = voting.age.people$Voting.Age.Estimate.2016
county.summary.df$turnout.perc.allage.est = (county.summary.df$turnout / county.summary.df$total.voters.age.est) *100

# Making final data frame to fit in with the other election days
county.summary.2016.final = with(
  county.summary.df,
  data.frame(
    county,
    year,
    dem.votes,
    dem.votes.perc,
    rep.votes,
    rep.votes.perc,
    oth.votes,
    oth.votes.perc,
    num.diff,
    perc.diff,
    winner.party,
    winner,
    turnout,
    turnout.perc.allage.est,
    total.voters.age.est
  )
)
colnames(county.summary.2016.final) = c(
  "county",
  "year",
  "dem.vote",
  "dem.vote.perc",
  "rep.vote",
  "rep.vote.perc",
  "oth.vote",
  "oth.vote.perc",
  "num.diff",
  "perc.diff",
  "winning.party",
  "winner",
  "turnout",
  "turnout.perc",
  "total.voting.age"
)
county.summary.2016.final$county = as.character(county.summary.2016.final$county)

} # 2016 data
{ # 2012 data
  county.summary.df = county.summary.df[order(county.summary.df$county), ]

    # This data comes from here: http://elections.wi.gov/elections-voting/results/2012/fall-general
  # 2012 county data
  results.2012 = read.xlsx(
    file = "County by County_11.6.12MOD.xls",
    sheetIndex = 2,
    header = TRUE,
    colClasses = NA,
    stringsAsFactors = F
  )
  results.2012 = results.2012[-c(73:74), ]

  county.summary.df$turnout.2012 = results.2012$Total.Votes.Cast
  county.summary.df$turnout.2012.perc = results.2012$Total.Votes.Cast / voting.age.people$Voting.Age.Estimate.2012 * 100
  county.summary.df$dem.2012 = results.2012$BARACK.OBAMA
  county.summary.df$rep.2012 = results.2012$MITT.ROMNEY
  county.summary.df$oth.2012 = results.2012$other.votes

  county.summary.df$diff.2012.num = results.2012$MITT.ROMNEY - results.2012$BARACK.OBAMA
  county.summary.df$diff.2012.perc = (county.summary.df$diff.2012.num / county.summary.df$turnout.2012) * 100

  county.summary.2012.df = county.summary.df
  county.summary.2012.df$year = 2012
  county.summary.2012.df$dem.2012.perc = county.summary.2012.df$dem.2012 / results.2012$Total.Votes.Cast *100
  county.summary.2012.df$rep.2012.perc = county.summary.2012.df$rep.2012 / results.2012$Total.Votes.Cast *100
  county.summary.2012.df$oth.2012.perc = county.summary.2012.df$oth.2012 / results.2012$Total.Votes.Cast *100

  county.summary.2012.df$winner.party = ifelse(
    county.summary.2012.df$rep.2012.perc >
      county.summary.2012.df$dem.2012.perc,
    "Rep",
    "Dem"
  )
  county.summary.2012.df$winner = ifelse(
    county.summary.2012.df$rep.2012.perc >
      county.summary.2012.df$dem.2012.perc,
    "Romney",
    "Obama"
  )
  county.summary.2012.df$total.poss.voters = voting.age.people$Voting.Age.Estimate.2012

  county.summary.2012.final = with(
    county.summary.2012.df,
    data.frame(
      county,
      year,
      dem.2012,
      dem.2012.perc,
      rep.2012,
      rep.2012.perc,
      oth.2012,
      oth.2012.perc,
      diff.2012.num,
      diff.2012.perc,
      winner.party,
      winner,
      turnout.2012,
      turnout.2012.perc,
      total.poss.voters
    )
  )
  colnames(county.summary.2012.final) = c(
    "county",
    "year",
    "dem.vote",
    "dem.vote.perc",
    "rep.vote",
    "rep.vote.perc",
    "oth.vote",
    "oth.vote.perc",
    "num.diff",
    "perc.diff",
    "winning.party",
    "winner",
    "turnout",
    "turnout.perc",
    "total.voting.age"
  )
  county.summary.2012.final$turnout = as.integer(as.character(county.summary.2012.final$turnout))
  county.summary.2012.final$county = as.character(county.summary.2012.final$county)
  ## Swing votes 2016 to 2012
  county.summary.2016.final$dem.change = county.summary.2016.final$dem.vote.perc -
                                        county.summary.2012.final$dem.vote.perc
  county.summary.2016.final$rep.change = county.summary.2016.final$rep.vote.perc -
    county.summary.2012.final$rep.vote.perc
  county.summary.2016.final$oth.change = county.summary.2016.final$oth.vote.perc -
    county.summary.2012.final$oth.vote.perc
  county.summary.2016.final$turnout.change = county.summary.2016.final$turnout.perc -
    county.summary.2012.final$turnout.perc

  county.summary.2016.final$swing.num = with(county.summary.df, num.diff - diff.2012.num)
  county.summary.2016.final$swing.perc = with(county.summary.df, perc.diff - diff.2012.perc)
  county.summary.2016.final$swing.turnout = with(county.summary.df, turnout - turnout.2012)
  county.summary.2016.final$swing.turnout.perc = with(county.summary.df,
                                              turnout.perc.allage.est - turnout.2012.perc)

} # 2012 county data frame
{ # 2008 county data frame
  election.2008 = read.csv("2008_FallElection_President_WardbyWardMOD.csv",
                           stringsAsFactors = F)

  election.2008.final.group = group_by(election.2008, COUNTY)

  election.2008.final = dplyr::summarise(
    election.2008.final.group,
    dem.vote = sum(Democrat),
    rep.vote = sum(Republican),
    oth.vote = sum(other.votes),
    turnout = sum(Total.votes)
  )
  election.2008.final = election.2008.final[-c(1), ]
  colnames(election.2008.final)[1] = c("county")
  election.2008.final$county[election.2008.final$county == "LaCrosse"] = "La Crosse"
  election.2008.final$county = as.character(election.2008.final$county)
  election.2008.final$year = 2008

  election.2008.final$dem.vote.perc = (election.2008.final$dem.vote / election.2008.final$turnout) * 100
  election.2008.final$rep.vote.perc = (election.2008.final$rep.vote / election.2008.final$turnout) * 100
  election.2008.final$oth.vote.perc = (election.2008.final$oth.vote / election.2008.final$turnout) * 100

  election.2008.final$num.diff = election.2008.final$rep.vote - election.2008.final$dem.vote
  election.2008.final$perc.diff = election.2008.final$rep.vote.perc - election.2008.final$dem.vote.perc

  election.2008.final$winner = ifelse(election.2008.final$rep.vote >
                                        election.2008.final$dem.vote,
                                      "McCain",
                                      "Obama")
  election.2008.final$winning.party = ifelse(election.2008.final$rep.vote >
                                               election.2008.final$dem.vote,
                                             "Rep",
                                             "Dem")
  election.2008.final$turnout.perc = NA
  election.2008.final$total.voting.age = NA

  election.2008.final$turnout = as.integer(as.character(election.2008.final$turnout))
  #### SWING IN VOTES 2012 to 2008
  county.summary.2012.final$swing.num = county.summary.2012.final$num.diff - election.2008.final$num.diff
  county.summary.2012.final$swing.perc =   county.summary.2012.final$perc.diff - election.2008.final$perc.diff
  county.summary.2012.final$swing.turnout = county.summary.2012.final$turnout - election.2008.final$turnout

  county.summary.2012.final$dem.change = county.summary.2012.final$dem.vote.perc -
    election.2008.final$dem.vote.perc
  county.summary.2012.final$rep.change = county.summary.2012.final$rep.vote.perc -
    election.2008.final$rep.vote.perc
  county.summary.2012.final$oth.change = county.summary.2012.final$oth.vote.perc -
    election.2008.final$oth.vote.perc

  election.2008.final$total.voting.age = county.summary.2012.final$total.voting.age
  election.2008.final$turnout.perc = (election.2008.final$turnout / election.2008.final$total.voting.age)*100

  county.summary.2012.final$swing.turnout.perc = (county.summary.2012.final$swing.turnout / county.summary.2012.final$total.voting.age)*100
  county.summary.2012.final$turnout.change = county.summary.2012.final$turnout.perc -
    election.2008.final$turnout.perc

  election.2008.final$swing.num = NA
  election.2008.final$swing.perc =   NA
  election.2008.final$swing.turnout = NA
  election.2008.final$swing.turnout.perc = NA
} # 2008 data frame
{ # 2004 county data frame
  election.2004 = read.csv("2004_FallElection_President_WardbyWardMOD.csv",stringsAsFactors = F)
  election.2004$COUNTY.1 = as.character(election.2004$COUNTY.1)

  election.2004.final.group = group_by(election.2004, COUNTY.1)
  election.2004.final = dplyr::summarise(
    election.2004.final.group,
    dem.vote = sum(Democrat),
    rep.vote = sum(Republican),
    oth.vote = sum(other.votes),
    turnout = sum(Total.votes)
  )
  election.2004.final = election.2004.final[-c(1), ]
  colnames(election.2004.final)[1] = c("county")
  election.2004.final$county[election.2004.final$county == "LaCrosse"] = "La Crosse"
  election.2004.final$county = as.character(election.2004.final$county)
  election.2004.final$year = 2004

  election.2004.final$dem.vote.perc = (election.2004.final$dem.vote / election.2004.final$turnout) * 100
  election.2004.final$rep.vote.perc = (election.2004.final$rep.vote / election.2004.final$turnout) * 100
  election.2004.final$oth.vote.perc = (election.2004.final$oth.vote / election.2004.final$turnout) * 100


  election.2004.final$num.diff = election.2004.final$rep.vote - election.2004.final$dem.vote
  election.2004.final$perc.diff = election.2004.final$rep.vote.perc - election.2004.final$dem.vote.perc

  election.2004.final$winner = ifelse(election.2004.final$rep.vote >
                                        election.2004.final$dem.vote,
                                      "Bush",
                                      "Kerry")
  election.2004.final$winning.party = ifelse(election.2004.final$rep.vote >
                                               election.2004.final$dem.vote,
                                             "Rep",
                                             "Dem")
  election.2004.final$turnout.perc = NA
  election.2004.final$total.voting.age = NA

  election.2004.final$turnout = as.integer(as.character(election.2004.final$turnout))
  #### SWING IN VOTES 2008 to 2004
  election.2008.final$swing.num = election.2008.final$num.diff - election.2004.final$num.diff
  election.2008.final$swing.perc =   election.2008.final$perc.diff - election.2004.final$perc.diff
  election.2008.final$swing.turnout = election.2008.final$turnout - election.2004.final$turnout
  election.2008.final$swing.turnout.perc = NA

  election.2008.final$dem.change = election.2008.final$dem.vote.perc -
    election.2004.final$dem.vote.perc
  election.2008.final$rep.change = election.2008.final$rep.vote.perc -
    election.2004.final$rep.vote.perc
  election.2008.final$oth.change = election.2008.final$oth.vote.perc -
    election.2004.final$oth.vote.perc

  election.2004.final$total.voting.age = election.2008.final$total.voting.age
  election.2004.final$turnout.perc = (election.2004.final$turnout / election.2004.final$total.voting.age)*100

  election.2008.final$swing.turnout.perc = (election.2008.final$swing.turnout / election.2008.final$total.voting.age)*100
  election.2008.final$turnout.change = election.2008.final$turnout.perc -
    election.2004.final$turnout.perc


  election.2004.final$swing.num = NA
  election.2004.final$swing.perc =   NA
  election.2004.final$swing.turnout = NA
  election.2004.final$swing.turnout.perc = NA
} # 2004 data frame
{ # 2000 county data frame
  election.2000 = read.csv("2000001107_PRES_SORTMOD.csv", stringsAsFactors = F)

  election.2000.final.group = group_by(election.2000, COUNTY)
  election.2000.final = dplyr::summarise(
    election.2000.final.group,
    dem.vote = sum(Democrat),
    rep.vote = sum(Republican),
    oth.vote = sum(other.votes),
    turnout = sum(Total.votes)
  )
  election.2000.final = election.2000.final[-c(1), ]
  colnames(election.2000.final)[1] = c("county")
  election.2000.final$county[election.2000.final$county == "LaCrosse"] = "La Crosse"
  election.2000.final$county = as.character(election.2000.final$county)
  election.2000.final$year = 2000

  election.2000.final$dem.vote.perc = (election.2000.final$dem.vote / election.2000.final$turnout) * 100
  election.2000.final$rep.vote.perc = (election.2000.final$rep.vote / election.2000.final$turnout) * 100
  election.2000.final$oth.vote.perc = (election.2000.final$oth.vote / election.2000.final$turnout) * 100


  election.2000.final$num.diff = election.2000.final$rep.vote - election.2000.final$dem.vote
  election.2000.final$perc.diff = election.2000.final$rep.vote.perc - election.2000.final$dem.vote.perc

  election.2000.final$winner = ifelse(election.2000.final$rep.vote >
                                        election.2000.final$dem.vote,
                                      "Bush",
                                      "Gore")
  election.2000.final$winning.party = ifelse(election.2000.final$rep.vote >
                                               election.2000.final$dem.vote,
                                             "Rep",
                                             "Dem")
  election.2000.final$turnout.perc = NA
  election.2000.final$total.voting.age = NA

  election.2000.final$turnout = as.integer(as.character(election.2000.final$turnout))
  #### SWING IN VOTES 2008 to 2004
  election.2004.final$swing.num = election.2004.final$num.diff - election.2000.final$num.diff
  election.2004.final$swing.perc =   election.2004.final$perc.diff - election.2000.final$perc.diff
  election.2004.final$swing.turnout = election.2004.final$turnout - election.2000.final$turnout
  election.2004.final$swing.turnout.perc = NA

  election.2004.final$dem.change = election.2004.final$dem.vote.perc -
    election.2000.final$dem.vote.perc
  election.2004.final$rep.change = election.2004.final$rep.vote.perc -
    election.2000.final$rep.vote.perc
  election.2004.final$oth.change = election.2004.final$oth.vote.perc -
    election.2000.final$oth.vote.perc


  election.2000.final$total.voting.age = election.2004.final$total.voting.age
  election.2000.final$turnout.perc = (election.2000.final$turnout / election.2000.final$total.voting.age)*100

  election.2004.final$swing.turnout.perc = (election.2004.final$swing.turnout / election.2004.final$total.voting.age)*100
  election.2004.final$turnout.change = election.2004.final$turnout.perc -
    election.2000.final$turnout.perc

  election.2000.final$swing.num = NA
  election.2000.final$swing.perc =   NA
  election.2000.final$swing.turnout = NA
  election.2000.final$swing.turnout.perc = NA

  election.2000.final$dem.change = NA
  election.2000.final$rep.change = NA
  election.2000.final$oth.change = NA
  election.2000.final$turnout.change = NA
} # 2000 data frame

# Making final length data frame
counties.2000.2016 = rbind(county.summary.2016.final,county.summary.2012.final,
                            election.2008.final,election.2004.final,election.2000.final)
counties.2000.2016$county = factor(counties.2000.2016$county)

# Adding extra columns
county.winners = as.data.frame(table(counties.2000.2016$county[counties.2000.2016$winning.party == "Dem"]))
colnames(county.winners) = c("county","dem.wins")

counties.2000.2016 = join(counties.2000.2016, county.winners, by = "county")
counties.2000.2016$mostly.dem = with(counties.2000.2016, ifelse(dem.wins >= 3, "Mostly Dem",
                                                                  "Mostly Rep"))
winner2016 = subset(counties.2000.2016, year == 2016)
winner2016$winner = ifelse(winner2016$winning.party == "Dem",
                          "Dem2016", "Rep2016")
counties.2000.2016$dem.2016 = with(counties.2000.2016, rep(winner2016$winner,5))
low.turnout.quant = quantile(counties.2000.2016$swing.turnout.perc[counties.2000.2016$year == 2016])[2]
low.turnout.quant = unname(low.turnout.quant)

counties.2000.2016$most.turnout.drop = with(counties.2000.2016, ifelse(swing.turnout.perc < low.turnout.quant,
                                                                         "Large turnout change 2016", "Little turnout change 2016"))

counties.2000.2016.2016 = counties.2000.2016
counties.2000.2016.2016 = subset(counties.2000.2016.2016, !is.na(most.turnout.drop))

counties.2000.2016.quant.join = data.frame(counties.2000.2016.2016$county,counties.2000.2016.2016$most.turnout.drop)
colnames(counties.2000.2016.quant.join) = c("county","most.turnout.drop")

counties.2000.2016 = join(counties.2000.2016, counties.2000.2016.quant.join, by = "county",match="first")

# Join county vote machine to other county dfs -----------------------------------------


counties.2000.2016 = join(counties.2000.2016,machine.join,by="county")
county.summary.2016.final = join(county.summary.2016.final,machine.join,by="county")
county.summary.df = join(county.summary.df,machine.join,by="county")

# Write final csvs --------------------------------------------------------

write.csv(election.2016.grouped,"candidate.2016.summary.csv")
write.csv(voting.df.2016,"counties.2016.bycand.csv")
write.csv(county.summary.df,"county.2016.vs.2012.csv")
write.csv(counties.2000.2016,"counties.2000.2016.csv")
write.csv(all_polls_2012,"all_polls_2012.csv")
write.csv(all_polls_2016,"all_polls_2016.csv")
write.csv(selected_polls_2012.sub,"selected_polls_2012.sub.csv")
write.csv(selected_polls_2016.sub,"selected_polls_2016.sub.csv")
write.csv(elections.100.years,"elections.100.years.csv")
write.csv(ward.2016.o.l, "wards.data.2016.o.csv")
write.csv(ward.2016.r.l, "wards.data.2016.r.csv")
# write.csv(ward.2016.2012, "ward.2016.2012.csv")
write.csv(ward.2016.2012.o, "ward.2016.2012.o.csv")
write.csv(ward.2016.2012.r, "ward.2016.2012.r.csv")

write.csv(repunit.2016.2012.o, "repunit.2016.2012.o.csv")








# Read in csvs and subset for use ------------------------------------------------------------
require(ggplot2); library(plotly); require(corrplot)
require("devtools")  # so we can install from GitHub
# devtools::install_github("ropensci/plotly")  # plotly is part of rOpenSci

# I got this from here: http://stackoverflow.com/questions/31337922/adding-italicised-r-with-correlation-coefficient-to-a-scatter-plot-chart-in-ggpl
corr_eqn <- function(x,y, digits = 2) {
  corr_coef <- round(cor(x, y), digits = digits)
  paste("italic(r) == ", corr_coef)
}

# setwd("C:\\Users\\s_cas\\Dropbox\\Perso\\2016 voting election county results\\Wisconsin")
setwd("C:/Users/s_cas/Dropbox/Perso/2016 voting election county results/Wisconsin/All prep together/All processed files")

candidate.2016.summary = read.csv("Prepped files/candidate.2016.summary.csv")
counties.2016.bycand = read.csv("Prepped files/counties.2016.bycand.csv")
ward.2016.r = read.csv("Prepped files/wards.data.2016.r.csv")
ward.2016.o = read.csv("Prepped files/wards.data.2016.o.csv")
county.2016.vs.2012 = read.csv("Prepped files/county.2016.vs.2012.csv")
counties.2000.2016 = read.csv("Prepped files/counties.2000.2016.csv")
elections.100.years = read.csv("Prepped files/elections.100.years.csv")
all_polls_2012 = read.csv("Prepped files/all_polls_2012.csv")
all_polls_2016 = read.csv("Prepped files/all_polls_2016.csv")
# selected_polls_2012.sub = read.csv("Prepped files/selected_polls_2012.sub.csv")
# selected_polls_2016.sub = read.csv("Prepped files/selected_polls_2016.sub.csv")
demographics = read.csv("Prepped files/voterdemographics.csv", stringsAsFactors = F)
# model.df = read.csv("Prepped files/modeldf.csv")
ward.2016.2012 = read.csv("Prepped files/ward.2016.2012.r.csv", stringsAsFactors = F)
ward.2016.2012.o = read.csv("Prepped files/ward.2016.2012.o.csv", stringsAsFactors = F)
ward.2016.r.l = read.csv("Prepped files/wards.data.2016.r.csv", stringsAsFactors = F)
ward.2016.2012.r = read.csv("Prepped files/ward.2016.2012.r.csv", stringsAsFactors = F)
elec.snapshot.dem = read.csv("Prepped files/outcomes_data_county.csv", stringsAsFactors = F)
repunit.2016.2012.o = read.csv("Prepped files/repunit.2016.2012.o.csv", stringsAsFactors = F)
# The above demographics from here: https://econsnapshot.com/2016/12/06/electronic-voting-machines-and-the-election/

elec.snapshot.dem.wi = subset(elec.snapshot.dem, state == "WI")
elec.snapshot.dem.wi$county = toupper(elec.snapshot.dem.wi$county)

colnames(demographics)[grep("use.machines",colnames(demographics))] = "county_use_opt_scan"
colnames(demographics)[grep("machine.vendor.dealer.spec",colnames(demographics))] = "county_machine_vendor_dealer"

colnames(demographics)[1] = "county"
demographics$medianhouseholdincome_2009.2013 = as.numeric(demographics$medianhouseholdincome_2009.2013)
demographics$pop_sq_mile_2010 = as.numeric(demographics$pop_sq_mile_2010)
colnames(demographics)[grep("Method_2016",colnames(demographics))] = "county_paper_or_paperplusmachine"

elections.100.years.summary.2016 = subset(elections.100.years, year == 2016)
elections.100.years.postwar = subset(elections.100.years, year >= 1948)

## Comparing the definite poll voters with the final result
poll.summary.2016 = data.frame(c(1:4))
poll.summary.2016$cand.group = c("Donald J. Trump", "Hillary Clinton", "Other","Undecided")
poll.summary.2016$polls.nov = with(selected_polls_2016.sub,
                                   c(mean(trump),mean(clinton),mean(other),mean(undecided,na.rm=T)))
poll.summary.2016$c.1.4. = NULL
poll.summary.2016$polls.nov.def = with(elections.100.years.summary.2016, c(mean.rep.poll.oneweek.defs, mean.dem.poll.oneweek.defs, mean.oth.poll.oneweek.defs,NA))

candidate.2016.summary$polls.nov = with(poll.summary.2016, c(polls.nov[1], polls.nov[2], rep(polls.nov[3],5)))
candidate.2016.summary$polls.nov.def = with(poll.summary.2016, c(polls.nov.def[1], polls.nov.def[2], rep(polls.nov.def[3],5)))

candidate.2016.summary$diff.from.poll = candidate.2016.summary$votes.perc.group - candidate.2016.summary$polls.nov
candidate.2016.summary$diff.from.poll.defs = candidate.2016.summary$votes.perc.group - candidate.2016.summary$polls.nov.def

### 2012 results ###
elections.100.years.summary.2012 = elections.100.years.postwar[2,]

poll.summary.2012 = data.frame(c(1:4))
poll.summary.2012$cand.group = c("Mitt Romney", "Barack Obama", "Other","Undecided")
poll.summary.2012$polls.nov = with(selected_polls_2012.sub,
                                   c(mean(romney),mean(obama),mean(other,na.rm=T),mean(undecided,na.rm=T)))
poll.summary.2012$c.1.4. = NULL
poll.summary.2012$polls.nov.def = with(elections.100.years.summary.2012, c(mean.rep.poll.oneweek.defs, mean.dem.poll.oneweek.defs, mean.oth.poll.oneweek.defs,NA))

poll.summary.2012$vote = with(elections.100.years.summary.2012, c(rep.vote, dem.vote, oth.vote,NA))
poll.summary.2012$vote.perc = with(elections.100.years.summary.2012, c(rep.vote.perc, dem.vote.perc, oth.vote.perc,NA))

poll.summary.2012$diff.from.polls = with(poll.summary.2012, vote.perc - polls.nov)
poll.summary.2012$diff.from.polls.defs = with(poll.summary.2012, vote.perc - polls.nov.def)

poll.summary.2012.nounds = poll.summary.2012[1:3,]

## VISUALISING THE 2016 RESULTS
county.2016.vs.2012 = join(county.2016.vs.2012, demographics, by = "county")
counties.2000.2016 = join(counties.2000.2016, demographics, by = "county")

county.summary.2016.final = subset(counties.2000.2016, year == 2016)
county.2016.vs.2012 = county.2016.vs.2012[order(county.2016.vs.2012$county), ]
added.columns = county.summary.2016.final[,20:28]
county.2016.vs.2012 = cbind(county.2016.vs.2012, added.columns)
county.2016.vs.2012$total.voting.age= as.integer(county.2016.vs.2012$total.voters.age.est)

county.2016.vs.2012.2000ppl = subset(county.2016.vs.2012, turnout > 2000)

county.2016.vs.2012.2000ppl.mildane= subset(county.2016.vs.2012.2000ppl, county != "Milwaukee" & county != "Dane")


county.summary.2016.final$mostly.dem.num = ifelse(county.summary.2016.final$mostly.dem == "Mostly Dem",1,0)
county.summary.2016.final$dem.other.change = county.summary.2016.final$dem.change - county.summary.2016.final$oth.change

county.summary.2016.final.highpop = subset(county.summary.2016.final, total.voting.age > mean(total.voting.age))
county.summary.2016.final$total.voting.age= as.integer(county.summary.2016.final$total.voting.age)
county.summary.2016.final.2000ppl= subset(county.summary.2016.final, turnout > 2000)

county.summary.2016.final.2000ppl.mildane= subset(county.summary.2016.final.2000ppl, county != "Milwaukee" & county != "Dane")

counties.2000.2016.2000ppl= subset(counties.2000.2016, turnout > 2000)
counties.2000.2016.2000ppl.mildane = subset(counties.2000.2016.2000ppl, county != "Milwaukee" & county != "Dane")

counties.2000.2016.2000ppl$medturnout = with(counties.2000.2016.2000ppl, ifelse(turnout > median(turnout),
                                                                                "High voter turnout county","Low voter turnout county"))

# Adding demographic data to the ward data frame
demographics.caps = demographics
demographics.caps$county = toupper(demographics.caps$county)

# Demographics from David Griffen and co
ward.2016.2012.o.dem = join(ward.2016.2012.o, demographics.caps, by = "county", match = "first")

# Also joining the data from the Election snapshot

ward.2016.2012.o.dem = join(ward.2016.2012.o.dem, elec.snapshot.dem.wi, by = "county", match = "first")

# Proportion of municipalities in counties that use voting machines
ward.prop.machines = with(ward.2016.2012.o.dem, data.frame(county, tch.use))

county.prop.machines.grp = group_by(ward.prop.machines, county)
county.prop.machines = dplyr::summarise(county.prop.machines.grp,
                                        tch.use.prop = mean(tch.use,na.rm = T))

county.summary.2016.final$county = toupper(county.summary.2016.final$county)
county.summary.2016.final = join(county.summary.2016.final, county.prop.machines)

county.summary.2016.final = join(county.summary.2016.final,elec.snapshot.dem.wi, by = "county")

# The following are the variables used by the Election snapshot in their models:
# Income 2015, inc_2015
# Unemployment rate Sept 2015, unemp_rate
# Male ACS 5-year, pct_male
# Age 62 + ACS 5 year, age_med
# Hispanic or Latino ACS 5 year, pct_latino
# White ACS 5-year, pct_white
# Black ACS 5 year, pct_black
# HS (high school) or less ACS 5 year, pct_hs
#
# ward.2016.2012.o.dem$demwinner = NA
# ward.2016.2012.o.dem$demwinner = ifelse(ward.2016.2012.o.dem$democrat_votes.perc >
#                                           ward.2016.2012.o.dem$republican_votes.perc &
#                                           !is.na(ward.2016.2012.o.dem$democrat_votes.perc), 1,0)
# ward.2016.2012.o.dem$demwinner = as.integer(ward.2016.2012.o.dem$demwinner)


# Same for recount data
# Demographics from David Griffen and co
ward.2016.2012.r.dem = join(ward.2016.2012.r, demographics.caps, by = "county", match = "first")

# Also joining the data from the Election snapshot
ward.2016.2012.r.dem = join(ward.2016.2012.r.dem, elec.snapshot.dem.wi, by = "county", match = "first")

# Same for RepUnit data
# Demographics from David Griffen and co
repunit.2016.2012.o.dem = join(repunit.2016.2012.o, demographics.caps, by = "county")

# Also joining the data from the Election snapshot
repunit.2016.2012.o.dem = join(repunit.2016.2012.o.dem, elec.snapshot.dem.wi, by = "county")
#
# ward.2016.2012.r.dem$demwinner = NA
# ward.2016.2012.r.dem$demwinner = ifelse(ward.2016.2012.r.dem$democrat_votes.perc >
#                                           ward.2016.2012.r.dem$republican_votes.perc &
#                                           !is.na(ward.2016.2012.r.dem$democrat_votes.perc), 1,0)
# ward.2016.2012.r.dem$demwinner = as.integer(ward.2016.2012.r.dem$demwinner)

# TIGER demographics ------------------------------------------------------
setwd("C:/Users/s_cas/Dropbox/Perso/2016 voting election county results/Wisconsin/All prep together/Wisconsin municipal stats/Tiger data")

tig_dem = data.frame(c(1:1926))

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

# March 2017 municipal EDL data for 2016 election NEED TO RUN -----------------------------------------------------------
ward.mardata.2016 = read.csv(
  "2016_presidential_and_general_election_el_190_2017_71702.csv",
  stringsAsFactors = F,
  header = T,
  strip.white = T
)

# Weird stuff:
# Ward 83, City of Madison, Dane has 709 late ballots in registered voter pop of 1698

 # Preparing basic data
  ward.mardata.2016 = clean_names(ward.mardata.2016)
  ward.mardata.2016$reporting_unit = toupper(ward.mardata.2016$reporting_unit)
  ward.mardata.2016$municipality = toupper(ward.mardata.2016$municipality)
  ward.mardata.2016$county = toupper(ward.mardata.2016$county)

  ward.mardata.2016$county = gsub(" COUNTY","", ward.mardata.2016$county)

  ward.mardata.2016$muni_county = paste(ward.mardata.2016$municipality, ward.mardata.2016$county)

  ## Need to change some municipality - county names to match my other data frame
  ward.mardata.2016$county[grep("CITY OF ASHLAND BAYFIELD",ward.mardata.2016$muni_county)] = "ASHLAND"
  ward.mardata.2016$county[grep("CITY OF COLUMBUS DODGE",ward.mardata.2016$muni_county)] = "COLUMBIA"
  ward.mardata.2016$county[grep("CITY OF HARTFORD DODGE",ward.mardata.2016$muni_county)] = "WASHINGTON"
  ward.mardata.2016$county[grep("CITY OF KAUKAUNA CALUMET",ward.mardata.2016$muni_county)] = "OUTAGAMIE"
  ward.mardata.2016$county[grep("CITY OF MILWAUKEE WASHINGTON",ward.mardata.2016$muni_county)] = "MILWAUKEE"
  ward.mardata.2016$county[grep("CITY OF MILWAUKEE WAUKESHA",ward.mardata.2016$muni_county)] = "MILWAUKEE"

  # Wisconsin Dells Juneau doesn't exist in my db. Maybe a 0 population. Has a pop of 0 also in feb 17 data
  ward.mardata.2016$county[grep("CITY OF WISCONSIN DELLS JUNEAU",ward.mardata.2016$muni_county)] = "JUNEAU"

  ward.mardata.2016$county[grep("VILLAGE OF GENOA CITY KENOSHA",ward.mardata.2016$muni_county)] = "WALWORTH"
  ward.mardata.2016$county[grep("VILLAGE OF HARRISON OUTAGAMIE",ward.mardata.2016$muni_county)] = "CALUMET"
  ward.mardata.2016$county[grep("VILLAGE OF HOWARD OUTAGAMIE",ward.mardata.2016$muni_county)] = "BROWN"
  ward.mardata.2016$county[grep("VILLAGE OF KEWASKUM FOND DU LAC",ward.mardata.2016$muni_county)] = "WASHINGTON"
  ward.mardata.2016$county[grep("VILLAGE OF ONTARIO MONROE",ward.mardata.2016$muni_county)] = "VERNON"
  ward.mardata.2016$county[grep("VILLAGE OF PULASKI OCONTO",ward.mardata.2016$muni_county)] = "BROWN"
  ward.mardata.2016$county[grep("VILLAGE OF ROCKLAND MONROE",ward.mardata.2016$muni_county)] = "LA CROSSE"

  ward.mardata.2016$muni_county = paste(ward.mardata.2016$municipality, ward.mardata.2016$county)
  
  ward.mardata.2016$reporting_unit = paste(ward.mardata.2016$municipality,ward.mardata.2016$reporting_unit, ward.mardata.2016$county)
  ward.mardata.2016$reporting_unit_orig = ward.mardata.2016$reporting_unit
  
  
  ward.mardata.2016$reporting_unit = gsub("WARDS","WARD",ward.mardata.2016$reporting_unit_orig)
  # ward.12.mun$reporting_unit = gsub("WARDS","WARD",ward.12.mun$reporting_unit_orig)
  ward.mardata.2016$reporting_unit = gsub("  "," ",ward.mardata.2016$reporting_unit)
  # ward.12.mun$reporting_unit = gsub("  "," ",ward.12.mun$reporting_unit)
  ward.mardata.2016$reporting_unit = gsub("&","-",ward.mardata.2016$reporting_unit)
  # ward.12.mun$reporting_unit = gsub("&","-",ward.12.mun$reporting_unit)
  
  ward.mardata.2016$reporting_unit = gsub("AND","-",ward.mardata.2016$reporting_unit)
  # ward.12.mun$reporting_unit = gsub("AND","-",ward.12.mun$reporting_unit)
  
  ward.mardata.2016$reporting_unit = gsub(",","-",ward.mardata.2016$reporting_unit)
  # ward.12.mun$reporting_unit = gsub(",","-",ward.12.mun$reporting_unit)
  ward.mardata.2016$reporting_unit = gsub(" - ","-",ward.mardata.2016$reporting_unit)
  # ward.12.mun$reporting_unit = gsub(" - ","-",ward.12.mun$reporting_unit)
  ward.mardata.2016$reporting_unit = gsub(" -","-",ward.mardata.2016$reporting_unit)
  # ward.12.mun$reporting_unit = gsub(" -","-",ward.12.mun$reporting_unit)
  ward.mardata.2016$reporting_unit = gsub("- ","-",ward.mardata.2016$reporting_unit)
  # ward.12.mun$reporting_unit = gsub("- ","-",ward.12.mun$reporting_unit)
  
  ward.mardata.2016$reporting_unit = gsub(" ","-",ward.mardata.2016$reporting_unit)
  # ward.12.mun$reporting_unit = gsub(" ","-",ward.12.mun$reporting_unit)
  
  ward.mardata.2016$reporting_unit = gsub("WD","",ward.mardata.2016$reporting_unit)
  # ward.12.mun$reporting_unit = gsub("WD","",ward.12.mun$reporting_unit)
  
  ward.mardata.2016$reporting_unit = gsub("COMBINED","",ward.mardata.2016$reporting_unit)
  # ward.12.mun$reporting_unit = gsub("COMBINED","",ward.12.mun$reporting_unit)
  
  ward.mardata.2016$reporting_unit = gsub("--","-",ward.mardata.2016$reporting_unit)
  # ward.12.mun$reporting_unit = gsub("--","-",ward.12.mun$reporting_unit)

  # REMEMBER TO CHANGE COUNTIES TOO!

  ward.mardata.group = group_by(ward.mardata.2016,muni_county, county, municipality)
  mardata.municounty = dplyr::summarise(ward.mardata.group,
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
setdiff(unique(mardata.municounty$muni_county), unique(ward.2016.2012.o.dem$muni_county))
setdiff(unique(ward.2016.2012.o.dem$muni_county), unique(mardata.municounty$muni_county))

# differences = setdiff(unique(mardata.municounty$muni_county),unique(ward.2016.2012.o.dem$muni_county))
# mardata.diffs = subset(mardata.municounty, muni_county %in% differences)

differences = setdiff(unique(ward.2016.2012.o.dem$muni_county),unique(mardata.municounty$muni_county))
mardata.diffs = subset(ward.2016.2012.o.dem, muni_county %in% differences)

# Missing: TOWN OF CALEDONIA TREMPEALEAU (town in multiple counties),
# TOWN OF HAYWARD (all put in city of Hayward),
# TOWN OF PRIMROSE DANE (doesn't exist), TOWN OF RANDALL KENOSHA (doesn't exist),
# TOWN OF ROXBURY DANE (doesn't exist), TOWN OF UNION ROCK (doesn't exist),
# VILLAGE OF NORTH BAY RACINE (doesn't exist)

# Made up variables for reporting unit level
mardata.repunit.join = with(ward.mardata.2016, data.frame(reporting_unit))
mardata.repunit.join$total_ballots = ifelse(ward.mardata.2016$total_ballots == 0,
                                    ward.mardata.2016$total_voters,
                                    ward.mardata.2016$total_ballots)
mardata.repunit.join$tch_prop = with(ward.mardata.2016, dre / total_ballots)
mardata.repunit.join$os_prop = with(ward.mardata.2016, optical_scan_ballots / total_ballots)
mardata.repunit.join$absentee_person_prop = with(ward.mardata.2016, absentee_in_person / absentee_issued)
mardata.repunit.join$inspector_prop = with(ward.mardata.2016, total_election_inspectors / total_ballots)
# mardata.repunit.join$tch_prop = ifelse(mardata.repunit.join$tch_prop>1, 1, mardata.repunit.join$tch_prop)

mardata.repunit.join$turnout_reg_feb = with(ward.mardata.2016, total_ballots / registrants)
mardata.repunit.join$sameday_reg_prop = with(ward.mardata.2016, election_day_registrants / registrants)
mardata.repunit.join$counted_absent_prop = with(ward.mardata.2016,
                                        counted_2 / total_ballots)
mardata.repunit.join$rejected_absent_prop = with(ward.mardata.2016,
                                         rejected2 / total_ballots)
mardata.repunit.join$registrants = with(ward.mardata.2016, registrants)
mardata.repunit.join$late_registrants = with(ward.mardata.2016, late_registrants / total_ballots)

# Made up variables for municipality level
mardata.join = with(mardata.municounty, data.frame(muni_county))
mardata.join$total_ballots = ifelse(mardata.municounty$total_ballots == 0,
                                   mardata.municounty$total_voters,
                                   mardata.municounty$total_ballots)
mardata.join$tch_prop = with(mardata.municounty, dre / total_ballots)
# mardata.join$tch_prop = ifelse(mardata.join$tch_prop>1, 1, mardata.join$tch_prop)
mardata.join$os_prop = with(mardata.municounty, optical_scan_ballots / total_ballots)
mardata.join$absentee_person_prop = with(mardata.municounty, absentee_in_person / absentee_issued)
mardata.join$inspector_prop = with(mardata.municounty, total_election_inspectors / total_ballots)

mardata.join$turnout_reg_feb = with(mardata.municounty, total_ballots / registrants)
mardata.join$sameday_reg_prop = with(mardata.municounty, election_day_registrants / registrants)
mardata.join$counted_absent_prop = with(mardata.municounty,
                                              counted_absent / total_ballots)
mardata.join$rejected_absent_prop = with(mardata.municounty,
                                              rejected_absent / total_ballots)
mardata.join$registrants = with(mardata.municounty, registrants)
mardata.join$late_registrants = with(mardata.municounty, late_registrants / total_ballots)

ward.2016.2012.o.dem = join(ward.2016.2012.o.dem, mardata.join, by = "muni_county")
ward.2016.2012.r.dem = join(ward.2016.2012.r.dem, mardata.join, by = "muni_county")

write.csv(mardata.municounty,"mar.2017.municounty.data.csv")

repunit.2016.2012.o.dem = join(repunit.2016.2012.o.dem, mardata.repunit.join, by = "reporting_unit")

write.csv(ward.mardata.2016,"mar.2017.repunit.data.csv")

# 2012 election municipal EDL data NEED TO RUN -----------------------------------------------------------

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
ward.edl2012.group = group_by(ward.edldata.2012,muni_county, county, municipality)
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
setdiff(unique(edl2012.municounty$muni_county), unique(ward.2016.2012.o.dem$muni_county))

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

setdiff(unique(ward.2016.2012.o.dem$muni_county), unique(edl2012.municounty$muni_county))

# Village of Harrison Calumet did not exist in 2012, was split off from the town in 2013.
# It is impossible to know the boundaries exactly.
# Therefore the swing from 2012 to 2016 cannot be accurately calculated.

# Same of the town / village of Somers, Kenosha. The town / village is completely mixed and messed up

# the town of Menasha was retained only for wards (8-10, 11-13). The rest (1-7) became the village of
# Fox Crossing, Winnebago

differences = setdiff(unique(ward.2016.2012.o.dem$muni_county),unique(edl2012.municounty$muni_county))
edl2012.diffs = subset(ward.2016.2012.o.dem, muni_county %in% differences)


# Made up variables for reporting unit level
repunit.2012.edl.join = with(ward.edldata.2012, data.frame(reporting_unit))
repunit.2012.edl.join$total_ballots_2012 = ifelse(ward.edldata.2012$totalballots == 0,
                                            ward.edldata.2012$totalelectors,
                                            ward.edldata.2012$totalballots)
repunit.2012.edl.join$tch_prop_2012 = with(ward.edldata.2012, dre / totalballots)
repunit.2012.edl.join$tch_prop_2012 = ifelse(repunit.2012.edl.join$tch_prop>1, 1, repunit.2012.edl.join$tch_prop)

repunit.2012.edl.join$os_prop_2012 = with(ward.edldata.2012, opticalscan / totalballots)
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

# For municipality level data
edl2012.join = with(edl2012.municounty, data.frame(muni_county))
edl2012.join$total_ballots_2012 = ifelse(edl2012.municounty$total_ballots == 0,
                                         edl2012.municounty$total_voters,
                                         edl2012.municounty$total_ballots)
edl2012.municounty$total_ballots_2012 = edl2012.join$total_ballots_2012
edl2012.join$tch_prop_2012 = with(edl2012.municounty, dre / total_ballots)
# edl2012.join$tch_prop_2012 = ifelse(edl2012.join$tch_prop_2012>1, 1, edl2012.join$tch_prop_2012)

edl2012.join$os_prop_2012 = with(edl2012.municounty, optical_scan_ballots / total_ballots)
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

ward.2016.2012.o.dem = join(ward.2016.2012.o.dem, edl2012.join, by = "muni_county")
ward.2016.2012.r.dem = join(ward.2016.2012.r.dem, edl2012.join, by = "muni_county")

write.csv(edl2012.municounty,"edl.2012.municounty.data.csv")


repunit.2016.2012.o.dem = join(repunit.2016.2012.o.dem, repunit.2012.edl.join, by = "reporting_unit")


# write.csv(edl2012.repunit,"edl.2012.repunit.data.csv")

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
  ward.prim.2016.l = join(ward.prim.2016.l, ward.prim.2016.look, by = "reporting_unit", match = "first")

  ward.prim.2016.l$votes_rec = as.numeric(as.integer(ward.prim.2016.l$votes_rec))
  

  tot.votes.look.group = group_by(ward.prim.2016.l,reporting_unit)
  tot.votes.look = dplyr::summarise(tot.votes.look.group,
                                    votes_tot = sum(votes_rec,na.rm=T)
  )

  ward.prim.2016.l = join(ward.prim.2016.l,tot.votes.look,by="reporting_unit",match="first")
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

  ward.jundata.group = group_by(ward.jundata.2016,reporting_unit, muni_county, county, municipality)
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

  ward.prim.2016.l2 = join(ward.prim.2016.l, jundata.join, by = "reporting_unit")
  ward.prim.2016.l = ward.prim.2016.l2

  ward.prim.2016.l[is.na(ward.prim.2016.l)] = NA
  rm(ward.prim.2016.l2)

} # Matching voting statistics with the election statistics
{
    # Adding demographic data to the ward data frame
    demographics.caps = demographics
    demographics.caps$county = toupper(demographics.caps$county)

    # Demographics from David Griffen and co
    ward.prim.2016.dem = join(ward.prim.2016.l, demographics.caps, by = "county", match = "first")

    # Also joining the data from the Election snapshot
    ward.prim.2016.dem = join(ward.prim.2016.dem, elec.snapshot.dem.wi, by = "county", match = "first")
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

  ward.prim.2016.dem = join(ward.prim.2016.dem, off_vote_machines, by = "muni_county")

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
    # model.prim.repunit.df = join(model.prim.repunit.df, density.join, by = "muni_county")

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
    # wisc.join@data = join(wisc.join@data, model.prim.repunit.df, by = "muni_county", match = "first")
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
  ward.prim.2016.l = join(ward.prim.2016.l, ward.prim.2016.look, by = "reporting_unit", match = "first")
  
  ward.prim.2016.l$votes_rec = as.numeric(as.integer(ward.prim.2016.l$votes_rec))
  
  
  tot.votes.look.group = group_by(ward.prim.2016.l,reporting_unit)
  tot.votes.look = dplyr::summarise(tot.votes.look.group,
                                    votes_tot = sum(votes_rec,na.rm=T)
  )
  
  ward.prim.2016.l = join(ward.prim.2016.l,tot.votes.look,by="reporting_unit",match="first")
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
  
  ward.jundata.group = group_by(ward.jundata.2016,reporting_unit, muni_county, county, municipality)
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
  
  ward.prim.2016.l2 = join(ward.prim.2016.l, jundata.join, by = "reporting_unit")
  ward.prim.2016.l = ward.prim.2016.l2
  
  ward.prim.2016.l[is.na(ward.prim.2016.l)] = NA
  rm(ward.prim.2016.l2)
  
} # Matching voting statistics with the election statistics
{
  # Adding demographic data to the ward data frame
  demographics.caps = demographics
  demographics.caps$county = toupper(demographics.caps$county)
  
  # Demographics from David Griffen and co
  ward.prim.2016.dem = join(ward.prim.2016.l, demographics.caps, by = "county", match = "first")
  
  # Also joining the data from the Election snapshot
  ward.prim.2016.dem = join(ward.prim.2016.dem, elec.snapshot.dem.wi, by = "county", match = "first")
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
  
  ward.prim.2016.dem = join(ward.prim.2016.dem, off_vote_machines, by = "muni_county")
  
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
  # model.prim.repunit.df = join(model.prim.repunit.df, density.join, by = "muni_county")
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
  # wisc.join@data = join(wisc.join@data, model.prim.repunit.df, by = "muni_county", match = "first")
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

# Models municipality data 2012 2016 original --------------------------------------------------

# NOTE: Multiplying the two voting method variables together improves the models, and reduces
# The correlation with the other variables
{
    ## Touchscreen and BMD use are super highly correlated, as are use.machines and os.use
    ## With turnout not logged
    corr.table = with(
      ward.2016.2012.o.dem,
      data.frame(
        dem.change.perc,
        rep.change.perc,
        oth.change.perc,
        turnout.change.perc,
        votes.tot2016,
        # county_use_opt_scan,
        tch_prop,
        # bmd.use,
        os.use,
        inc_2015,
        unemp_rate,
        pct_male,
        age_med,
        # pct_white,
        pct_latino,
        pct_black,
        pct_hs,
        pop_sq_mile_2010,
        pct_white,
        demwinner
      )
    )
    colnames(corr.table) = abbreviate( colnames(corr.table), minlength = 16 )
    # colnames(corr.table)[6] = "voting.method"
    # colnames(corr.table)[10] = "med.income"
    correlationmatrix = cor(corr.table, use = "pairwise.complete.obs")
    corrplot.all = corrplot(correlationmatrix, method = "number", type = "upper")
  } # Correlations
{ # Model data frame preparation
# Following Nate Cohn's method: https://twitter.com/Nate_Cohn/status/801226924156719104/photo/1?ref_src=twsrc%5Etfw
# I found it's better to use the non-logged version of pop.sq.mile.2010

# ratio college and income are both highly correlated so maybe don't use median income
# population density is highly correlated with black population, so I removed population density
# I was not able to effectively model Menonminee county with my data (it has a very high Amerindian population). So,
# I replaced the correlated and generally not significant 'white_pct' variable with 'white_nonwhite*

# tch_make =  colnames(ward.2016.2012.o.dem)[grep("tch_make",colnames(ward.2016.2012.o.dem))]
# make_tch = ward.2016.2012.o.dem[tch_make]
# 
# make_tch$tch_makedominion_voting_systems = NULL
# make_tch$tch_makenot_applicable = NULL
# make_tch$tch_makepopulex = NULL
# make_tch$tch_makevote_pad = NULL
# 
# os_make =  colnames(ward.2016.2012.o.dem)[grep("os_make",colnames(ward.2016.2012.o.dem))]
# make_os = ward.2016.2012.o.dem[os_make]
# 
# make_os$os_makenot_applicable = NULL
# make_os$os_makepopulex = NULL
# make_os$os_makevote_pad = NULL
  
ward.2016.2012.o.dem$log.pop.sq.mile.2010 = log(ward.2016.2012.o.dem$pop_sq_mile_2010)

model.o.df = with(ward.2016.2012.o.dem, data.frame(MCD_FIPS,county, muni_county,
                                                   democrat_votes.perc, republican_votes.perc, other_votes.perc,
                                                   dem.change.perc,rep.change.perc,oth.change.perc,
                                                   tch_prop, os_prop, counted_absent_prop,rejected_absent_prop,
                                                   absentee_person_prop,
                                                   sameday_reg_prop,turnout_reg_feb,#turnout.2016.perc,
                                                   # county_paper_or_paperplusmachine, #county_use_opt_scan,
                                                   #county_machine_vendor_dealer, #turnout.change.perc,
                                                   # ratio_nocollege_college , log.pop.sq.mile.2010,
                                                   # pop_sq_mile_2010,#tch.use, os.use, bmd.use,
                                                   # medianhouseholdincome_2009.2013, #voters.tot2016, 
                                                   votes.tot2016, inspector_prop,
                                                   dem.perc2012, rep.perc2012, oth.perc2012, #turnout.2012,
                                                   votes.tot2012, 
                                                   # inc_2015,inc_2015, unemp_rate, X._collegedegreepersons25._2013, pct_graduate,
                                                   # pct_male, age_med, pct_latino, pct_white, pct_black, pct_white,
                                                   pct_hs,
                                                   inspector_prop_2012,
                                                   total_ballots,registrants,late_registrants,tch_prop_2012,
                                                   os_prop_2012, automark_prop_2012,
                                                   total_ballots_2012,turnout_reg_2012, counted_absent_prop_2012,
                                                   rejected_absent_prop_2012, absentee_person_prop_2012,
                                                   registrants_2012,late_registrants_2012,
                                                   sameday_reg_prop_2012
))
rownames(model.o.df) = model.o.df$muni_county

model.o.df$MCD_FIPS = as.character(model.o.df$MCD_FIPS)

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

model.o.df = join(model.o.df,tig_dems_join, by = "MCD_FIPS")

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
  model.reprim.df = read.csv("model_reprim.csv")
  # model.reprim.df$rejected_absent_prop
  
  repprim_join = model.reprim.df[,c(3:22)]
  colnames(repprim_join)[2:20] <- paste(colnames(repprim_join)[2:20],"reprim", sep = "_")

  model.demprim.df = read.csv("model_demprim.csv")
  demprim_join = model.demprim.df[,c(3:22)]
  colnames(demprim_join)[2:20] <- paste(colnames(demprim_join)[2:20],"demprim", sep = "_")

  model.o.df = join(model.o.df,repprim_join,by="muni_county")
  model.o.df$trump_vote_diff = model.o.df$republican_votes.perc - model.o.df$votes_perc_trump_reprim
  model.o.df$tch_prop_reprim_diff = model.o.df$tch_prop - model.o.df$tch_prop_reprim

  model.o.df = join(model.o.df,demprim_join,by="muni_county")
  model.o.df$clinton_vote_diff = model.o.df$democrat_votes.perc - model.o.df$votes_perc_clinton_demprim
  model.o.df$tch_prop_demprim_diff = model.o.df$tch_prop - model.o.df$tch_prop_demprim

} # Adding in vote change from Spring primary
{ # Adding in vote change from 2012 to primaries
  model.edl2012.df = read.csv("edl.2012.municounty.data.csv")
  # edl_join = model.edl2012.df[,c(2:12)]
  # colnames(edl_join)[4:11] <- paste(colnames(edl_join)[4:11],"2012_num", sep = "_")
  
  # model.o.df = join(model.o.df,edl_join,by="muni_county")
  model.o.df$trump_prim_2012_vote_diff = model.o.df$votes_perc_trump_reprim - model.o.df$rep.perc2012
  model.o.df$clinton_prim_2012_vote_diff = model.o.df$votes_perc_clinton_demprim - model.o.df$dem.perc2012
  model.o.df$tch_prop_2012_reprim_diff = model.o.df$tch_prop_reprim - model.o.df$tch_prop_2012
  model.o.df$tch_prop_2012_demprim_diff = model.o.df$tch_prop_demprim - model.o.df$tch_prop_2012
  model.o.df$tch_prop_2012_2016_diff = model.o.df$tch_prop - model.o.df$tch_prop_2012

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
# unique(grepl("550101201",model.o.df$MCD_FIPS))

# setdiff(off_vote_machines$muni_county, model.o.df$muni_county)
# off_vote_machines$muni_county[grep("TOWN OF GEORGETOWN  POLK", off_vote_machines$muni_county)] = "TOWN OF GEORGETOWN POLK"
# off_vote_machines$muni_county[grep("TOWN OF WINDSOR DANE", off_vote_machines$muni_county)] = "VILLAGE OF WINDSOR DANE"
# off_vote_machines$muni_county[grep("TOWN OF MAINE MARATHON", off_vote_machines$muni_county)] = "VILLAGE OF MAINE MARATHON"

# setdiff(model.o.df$muni_county, off_vote_machines$muni_county) # Difference of 47
# length(off_vote_machines$muni_county) # 1854
# length(model.o.df$muni_county) # 1900 Difference of 46. Plus the errant space = 47. Therefore, these places are simply not included in the official list?

off_vote_machines_cut = off_vote_machines
off_vote_machines$County = NULL
off_vote_machines$Municipality = NULL

model.o.df = join(model.o.df, off_vote_machines, by = "muni_county")

model.o.df$tch_model_off_full = NA
model.o.df$tch_model_off_full = with(model.o.df, ifelse(tch_model_off == "Dominion (Premier)-Accuvote TSX",
                                                "Dominion (Premier)-Accuvote TSX", model.o.df$tch_model_off_full))
model.o.df$tch_model_off_full = with(model.o.df, ifelse(tch_model_off == "Dominion (Sequoia)/Command Central-Edge",
                                                        "Dominion (Sequoia)/Command Central-Edge",model.o.df$tch_model_off_full))
model.o.df$tch_model_off_full = with(model.o.df, ifelse(tch_model_off == "Dominion (Sequoia)/Command Central-Edge; ES&S Automark",
                                                        "Dominion (Sequoia)/Command Central-Edge",model.o.df$tch_model_off_full))
model.o.df$tch_model_off_full = with(model.o.df, ifelse(tch_model_off == "ES&S iVotronic",
                                                        "ES&S iVotronic",model.o.df$tch_model_off_full))
model.o.df$tch_model_off_full[is.na(model.o.df$tch_model_off_full)] = "aNone"
model.o.df$tch_model_off_full = with(model.o.df, ifelse(is.na(tch_use_off),
                                              NA,model.o.df$tch_model_off_full))

# model.o.df[grep("tch_use_verivote.1",colnames(model.o.df))] = NULL
# model.o.df[grep("tch_use_verivote.2",colnames(model.o.df))] = NULL
#
colnames(model.o.df)[grep("tch.use",colnames(model.o.df),fixed = T)] = "tch_use_verivote"
colnames(model.o.df)[grep("os.use",colnames(model.o.df),fixed = T)] = "os_use_verivote"
colnames(model.o.df)[grep("bmd.use",colnames(model.o.df),fixed = T)] = "bmd_use_verivote"
colnames(model.o.df)[grep("X._collegedegreepersons25._2013",colnames(model.o.df))] = "coll_deg_plus25_2013"
} # Adding in voting machines
{
model.o.df$demwinner = NA
model.o.df$demwinner = ifelse(model.o.df$democrat_votes.perc >
                                model.o.df$republican_votes.perc &
                                !is.na(model.o.df$democrat_votes.perc), 1,0)
model.o.df$demwinner = factor(model.o.df$demwinner)

model.o.df$demwinner2012 = NA
model.o.df$demwinner2012 = ifelse(model.o.df$dem.perc2012 >
                                    model.o.df$rep.perc2012 &
                                    !is.na(model.o.df$democrat_votes.perc), 1,0)
model.o.df$demwinner2012 = factor(model.o.df$demwinner2012)
} # Dem won in 2012
{ # Subsetting data for the models
# Only keep municipalities with 20 or more people voting - below this is too skewed
model.o.df = subset(model.o.df, votes.tot2016 > 19)

model.o.df$turnout.2012[model.o.df$turnout.2012 > 120] = NA
model.o.df$turnout.2012[model.o.df$turnout.2012 < 30] = NA

model.o.df$turnout.2016.perc[model.o.df$votes.tot2016 < 20] = NA
model.o.df$turnout.2012[model.o.df$votes.tot2012 < 20] = NA

turnout.high.2012 = subset(model.o.df, turnout.2012 > 100)

model.o.df$dem.change.perc[model.o.df$dem.change.perc < - 49] = NA
model.o.df$rep.change.perc[model.o.df$rep.change.perc < - 49] = NA
model.o.df$rep.change.perc[model.o.df$rep.change.perc > 49] = NA

model.o.df$turnout.2012[model.o.df$turnout.2012 > 110] = NA
model.o.df$oth.perc2012[max(model.o.df$oth.perc2012,na.rm=T)] = NA
model.o.df$other_votes.perc[model.o.df$other_votes.perc ==
                              max(model.o.df$other_votes.perc,na.rm=T)] = NA
model.o.df$turnout.change.perc[model.o.df$turnout.change.perc < - 30] = NA
# model.o.df = subset(model.o.df, log.pop.sq.mile.2010 > median(log.pop.sq.mile.2010,na.rm=T))
} # Subsetting data for the models
{ # Getting municipality densities
require(rgdal); require(sp)
  getwd()
#
wisc.spat.o <-
  readOGR(
    dsn = "Prepped files/wisc.spat.orig.geojson",
    layer = "OGRGeoJSON",
    disambiguateFIDs = T,
    stringsAsFactors = F
  )

# First, getting population densities. Transforming to utm, zone 16 for Wisconsin

wisc.spat.o.trans = spTransform(
  wisc.spat.o,
  CRS(
    "+proj=utm +zone=16 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
  )
)

ur.area<-sapply(slot(wisc.spat.o.trans, "polygons"), function(x) sapply(slot(x, "Polygons"), slot, "area"))
ur.area.s = lapply(ur.area,sum)

wisc.spat.o@data$muni.area = unlist(ur.area.s) / 1000000

pop.join = with(model.o.df, data.frame(muni_county,pop))
wisc.spat.o@data = join(wisc.spat.o@data, pop.join, by = "muni_county")

wisc.spat.o@data$muni.dense = as.numeric(wisc.spat.o@data$pop) / wisc.spat.o@data$muni.area
density.join = with(wisc.spat.o@data, data.frame(muni_county,muni.dense))

model.o.df = join(model.o.df, density.join, by = "muni_county")

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

 wisc.spat.o.cut = wisc.spat.o
 wisc.spat.o.cut@data[7:length(wisc.spat.o.cut@data)] = NULL
 wisc.spat.o.cut@data$MCD_NAME = toupper(wisc.spat.o.cut@data$MCD_NAME)

 wisc.join = wisc.spat.o.cut

 wisc.join@data = join(wisc.join@data, model.o.df, by = "muni_county", match = "first")
 colnames(wisc.join@data) = make.unique(colnames(wisc.join@data))
 # wisc.join@data$county_machine_vendor_dealer = as.character( wisc.join@data$county_machine_vendor_dealer)
 wisc.join@data$demwinner = as.character( wisc.join@data$demwinner)
 wisc.join@data$demwinner2012 = as.character( wisc.join@data$demwinner2012)

 wisc.join@data$county = NULL
 wisc.join@data$MCD_FIPS.1 = NULL

 wisc.join@data[wisc.join@data == Inf] = NA
 wisc.join@data[wisc.join@data == -Inf] = NA

 wisc.join@data[is.na(wisc.join@data)] = NA

 # unique(wisc.join@data$muni_county=="NA")
 # wisc.join = wisc.join[-wisc.join@data$muni_county=="NA",]
 # wisc.join = subset(wisc.join,wisc.join@data$muni_county!="NA")

 wisc.join.df = as.data.frame(wisc.join)
 # str(wisc.join.df)

 # wisc.join@data = clean_names(wisc.join@data)
 # require(geojson)

 writeOGR(
   wisc.join,
   "Prepped files/wisc.present.orig.geojson",
   layer = "wisc.join",
   driver = "GeoJSON"
 )

 # geo_write(wisc.join, file = "~/Prepped files/wisc.present.orig.geojson")

  # writeOGR(
 #   wisc.join,
 #   "Prepped files/wisc.present.orig.shp",
 #   layer = "wisc.join",
 #   driver = "ESRI Shapefile"
 # )

} # Creating a carto-presentable map

write.csv(model.o.df,"modeldfward.o.csv")

# Models municipality data 2012 2016 recount --------------------------------------------------

# NOTE: Multiplying the two voting method variables together improves the models, and reduces
# The correlation with the other variables
  {
    ## Touchscreen and BMD use are super highly correlated, as are use.machines and os.use
    ## With turnout not logged
    corr.table = with(
      ward.2016.2012.r.dem,
      data.frame(
        dem.change.perc,
        rep.change.perc,
        oth.change.perc,
        turnout.change.perc,
        votes.tot2016,
        # county_use_opt_scan,
        tch_prop,
        # bmd.use,
        os.use,
        inc_2015,
        unemp_rate,
        pct_male,
        age_med,
        # pct_white,
        pct_latino,
        pct_black,
        pct_hs,
        pop_sq_mile_2010,
        pct_white,
        demwinner
      )
    )
    colnames(corr.table) = abbreviate( colnames(corr.table), minlength = 16 )
    # colnames(corr.table)[6] = "voting.method"
    # colnames(corr.table)[10] = "med.income"
    correlationmatrix = cor(corr.table, use = "pairwise.complete.obs")
    corrplot.all = corrplot(correlationmatrix, method = "number", type = "upper")
  } # Correlations
  { # Model data frame preparation
  # Following Nate Cohn's method: https://twitter.com/Nate_Cohn/status/801226924156719104/photo/1?ref_src=twsrc%5Etfw
  # I found it's better to use the non-logged version of pop.sq.mile.2010
  ward.2016.2012.r.dem$log.pop.sq.mile.2010 = log(ward.2016.2012.r.dem$pop_sq_mile_2010)

  # ratio college and income are both highly correlated so maybe don't use median income
  # population density is highly correlated with black population, so I removed population density
  # I was not able to effectively model Menonminee county with my data (it has a very high Amerindian population). So,
  # I replaced the correlated and generally not significant 'white_pct' variable with 'white_nonwhite*

  tch_make =  colnames(ward.2016.2012.r.dem)[grep("tch_make",colnames(ward.2016.2012.r.dem))]
  make_tch = ward.2016.2012.r.dem[tch_make]

  # summary(make_tch)

  make_tch$tch_makedominion_voting_systems = NULL
  make_tch$tch_makenot_applicable = NULL
  make_tch$tch_makepopulex = NULL
  make_tch$tch_makevote_pad = NULL

  ##

  os_make =  colnames(ward.2016.2012.r.dem)[grep("os_make",colnames(ward.2016.2012.r.dem))]
  make_os = ward.2016.2012.r.dem[os_make]

  # summary(make_os)

  make_os$os_makenot_applicable = NULL
  make_os$os_makepopulex = NULL
  make_os$os_makevote_pad = NULL
  
  ward.2016.2012.o.dem$log.pop.sq.mile.2010 = log(ward.2016.2012.o.dem$pop_sq_mile_2010)
  
  model.r.df = with(ward.2016.2012.r.dem, data.frame(MCD_FIPS,county, muni_county,
                                                     democrat_votes.perc, republican_votes.perc, other_votes.perc,
                                                     dem.change.perc,rep.change.perc,oth.change.perc,
                                                     tch_prop, os_prop, counted_absent_prop,rejected_absent_prop,
                                                     absentee_person_prop,
                                                     sameday_reg_prop,turnout_reg_feb,#turnout.2016.perc,
                                                     # county_paper_or_paperplusmachine, #county_use_opt_scan,
                                                     #county_machine_vendor_dealer, #turnout.change.perc,
                                                     # ratio_nocollege_college , log.pop.sq.mile.2010,
                                                     # pop_sq_mile_2010,#tch.use, os.use, bmd.use,
                                                     # medianhouseholdincome_2009.2013, #voters.tot2016, 
                                                     votes.tot2016, inspector_prop,
                                                     dem.perc2012, rep.perc2012, oth.perc2012, #turnout.2012,
                                                     votes.tot2012, 
                                                     # inc_2015,inc_2015, unemp_rate, X._collegedegreepersons25._2013, pct_graduate,
                                                     # pct_male, age_med, pct_latino, pct_white, pct_black, pct_white,
                                                     pct_hs,
                                                     inspector_prop_2012,
                                                     total_ballots,registrants,late_registrants,tch_prop_2012,
                                                     os_prop_2012, automark_prop_2012,
                                                     total_ballots_2012,turnout_reg_2012, counted_absent_prop_2012,
                                                     rejected_absent_prop_2012, absentee_person_prop_2012,
                                                     registrants_2012,late_registrants_2012,
                                                     sameday_reg_prop_2012
  ))
  rownames(model.r.df) = model.r.df$muni_county
  
  model.r.df$MCD_FIPS = as.character(model.r.df$MCD_FIPS)
  
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

  model.r.df = join(model.r.df,tig_dems_join, by = "MCD_FIPS")
} # Creating df
  { # Adding in vote change from Spring primary
    model.reprim.df = read.csv("model_reprim.csv")
    repprim_join = model.reprim.df[,c(3:22)]
    colnames(repprim_join)[2:20] <- paste(colnames(repprim_join)[2:20],"reprim", sep = "_")
    
    model.demprim.df = read.csv("model_demprim.csv")
    demprim_join = model.demprim.df[,c(3:22)]
    colnames(demprim_join)[2:20] <- paste(colnames(demprim_join)[2:20],"demprim", sep = "_")

    model.r.df = join(model.r.df,repprim_join,by="muni_county")
    model.r.df$trump_vote_diff = model.r.df$republican_votes.perc - model.r.df$votes_perc_trump_reprim
    model.r.df$tch_prop_reprim_diff = model.r.df$tch_prop - model.r.df$tch_prop_reprim

    model.r.df = join(model.r.df,demprim_join,by="muni_county")
    model.r.df$clinton_vote_diff = model.r.df$democrat_votes.perc - model.r.df$votes_perc_clinton_demprim
    model.r.df$tch_prop_demprim_diff = model.r.df$tch_prop - model.r.df$tch_prop_demprim

  } # Adding in vote change from Spring primary
  { # Adding in vote change from 2012 to primaries
    # model.edl2012.df = read.csv("edl.2012.municounty.data.csv")
    # edl_join = model.edl2012.df[,c(2:12)]
    # colnames(edl_join)[4:11] <- paste(colnames(edl_join)[4:11],"2012_num", sep = "_")
    
    # model.r.df = join(model.r.df,edl_join,by="muni_county")
    model.r.df$trump_prim_2012_vote_diff = model.r.df$votes_perc_trump_reprim - model.r.df$rep.perc2012
    model.r.df$clinton_prim_2012_vote_diff = model.r.df$votes_perc_clinton_demprim - model.r.df$dem.perc2012
    model.r.df$tch_prop_2012_reprim_diff = model.r.df$tch_prop_reprim - model.r.df$tch_prop_2012
    model.r.df$tch_prop_2012_demprim_diff = model.r.df$tch_prop_demprim - model.r.df$tch_prop_2012
    model.r.df$tch_prop_2012_2016_diff = model.r.df$tch_prop - model.r.df$tch_prop_2012
    
  } # Adding in vote change from 2012 to primaries
  {
  # colnames(ward.2016.2012.r.dem)

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
  # unique(grepl("550101201",model.r.df$MCD_FIPS))

  # setdiff(off_vote_machines$muni_county, model.r.df$muni_county)
  # off_vote_machines$muni_county[grep("TOWN OF GEORGETOWN  POLK", off_vote_machines$muni_county)] = "TOWN OF GEORGETOWN POLK"
  # off_vote_machines$muni_county[grep("TOWN OF WINDSOR DANE", off_vote_machines$muni_county)] = "VILLAGE OF WINDSOR DANE"
  # off_vote_machines$muni_county[grep("TOWN OF MAINE MARATHON", off_vote_machines$muni_county)] = "VILLAGE OF MAINE MARATHON"

  # setdiff(model.r.df$muni_county, off_vote_machines$muni_county) # Difference of 47
  # length(off_vote_machines$muni_county) # 1854
  # length(model.r.df$muni_county) # 1900 Difference of 46. Plus the errant space = 47. Therefore, these places are simply not included in the official list?

  off_vote_machines_cut = off_vote_machines
  off_vote_machines$County = NULL
  off_vote_machines$Municipality = NULL

  model.r.df = join(model.r.df, off_vote_machines, by = "muni_county")

  model.r.df$tch_model_off_full = NA
  model.r.df$tch_model_off_full = with(model.r.df, ifelse(tch_model_off == "Dominion (Premier)-Accuvote TSX",
                                                          "Dominion (Premier)-Accuvote TSX", model.r.df$tch_model_off_full))
  model.r.df$tch_model_off_full = with(model.r.df, ifelse(tch_model_off == "Dominion (Sequoia)/Command Central-Edge",
                                                          "Dominion (Sequoia)/Command Central-Edge",model.r.df$tch_model_off_full))
  model.r.df$tch_model_off_full = with(model.r.df, ifelse(tch_model_off == "Dominion (Sequoia)/Command Central-Edge; ES&S Automark",
                                                          "Dominion (Sequoia)/Command Central-Edge",model.r.df$tch_model_off_full))
  model.r.df$tch_model_off_full = with(model.r.df, ifelse(tch_model_off == "ES&S iVotronic",
                                                          "ES&S iVotronic",model.r.df$tch_model_off_full))
  model.r.df$tch_model_off_full[is.na(model.r.df$tch_model_off_full)] = "aNone"
  model.r.df$tch_model_off_full = with(model.r.df, ifelse(is.na(tch_use_off),
                                                          NA,model.r.df$tch_model_off_full))

  # model.r.df[grep("tch_use_verivote.1",colnames(model.r.df))] = NULL
  # model.r.df[grep("tch_use_verivote.2",colnames(model.r.df))] = NULL
  #
  colnames(model.r.df)[grep("tch.use",colnames(model.r.df),fixed = T)] = "tch_use_verivote"
  colnames(model.r.df)[grep("os.use",colnames(model.r.df),fixed = T)] = "os_use_verivote"
  colnames(model.r.df)[grep("bmd.use",colnames(model.r.df),fixed = T)] = "bmd_use_verivote"
  colnames(model.r.df)[grep("X._collegedegreepersons25._2013",colnames(model.r.df))] = "coll_deg_plus25_2013"
} # voting machines
  {
  # The following are the variables used by the Election snapshot in their models:
  # Income 2015, inc_2015
  # Unemployment rate Sept 2015, unemp_rate
  # Male ACS 5-year, pct_male
  # Age 62 + ACS 5 year, age_med
  # Hispanic or Latino ACS 5 year, pct_latino
  # White ACS 5-year, pct_white
  # Black ACS 5 year, pct_black
  # HS (high school) or less ACS 5 year, pct_hs

  # Pop sq. mile is highly correlated with pct_black and so should be removed

  model.r.df$demwinner = NA
  model.r.df$demwinner = ifelse(model.r.df$democrat_votes.perc >
                                  model.r.df$republican_votes.perc &
                                  !is.na(model.r.df$democrat_votes.perc), 1,0)
  model.r.df$demwinner = factor(model.r.df$demwinner)

  model.r.df$demwinner2012 = NA
  model.r.df$demwinner2012 = ifelse(model.r.df$dem.perc2012 >
                                      model.r.df$rep.perc2012 &
                                      !is.na(model.r.df$democrat_votes.perc), 1,0)
  model.r.df$demwinner2012 = factor(model.r.df$demwinner2012)

  # model.r.df$tch.use = as.factor(as.character(model.r.df$tch.use))
  # model.r.df$os.use = as.factor(as.character(model.r.df$os.use))
  
} # Demwinner 2012
  {
    # Only keep municipalities with 20 or more people voting - below this is too skewed
    model.r.df = subset(model.r.df, votes.tot2016 > 19)

    model.r.df$turnout.2012[model.r.df$turnout.2012 > 120] = NA
    model.r.df$turnout.2012[model.r.df$turnout.2012 < 30] = NA

    model.r.df$turnout.2016.perc[model.r.df$votes.tot2016 < 20] = NA
    model.r.df$turnout.2012[model.r.df$votes.tot2012 < 20] = NA

    turnout.high.2012 = subset(model.r.df, turnout.2012 > 100)

    model.r.df$dem.change.perc[model.r.df$dem.change.perc < - 49] = NA
    model.r.df$rep.change.perc[model.r.df$rep.change.perc < - 49] = NA
    model.r.df$rep.change.perc[model.r.df$rep.change.perc > 49] = NA

    model.r.df$turnout.2012[model.r.df$turnout.2012 > 110] = NA
    model.r.df$oth.perc2012[max(model.r.df$oth.perc2012,na.rm=T)] = NA
    model.r.df$other_votes.perc[model.r.df$other_votes.perc ==
                                  max(model.r.df$other_votes.perc,na.rm=T)] = NA
    model.r.df$turnout.change.perc[model.r.df$turnout.change.perc < - 30] = NA
    # model.r.df = subset(model.r.df, log.pop.sq.mile.2010 > median(log.pop.sq.mile.2010,na.rm=T))
  } # Subsetting data for the model
  { # Getting municipality densities
    require(rgdal); require(sp)
    getwd()
    #
    wisc.spat.r <-
      readOGR(
        dsn = "Prepped files/wisc.spat.orig.geojson",
        layer = "OGRGeoJSON",
        disambiguateFIDs = T,
        stringsAsFactors = F
      )

    # First, getting population densities. Transforming to utm, zone 16 for Wisconsin

    wisc.spat.r.trans = spTransform(
      wisc.spat.r,
      CRS(
        "+proj=utm +zone=16 +datum=WGS84 +ellps=WGS84 +units=m +no_defs"
      )
    )

    ur.area<-sapply(slot(wisc.spat.r.trans, "polygons"), function(x) sapply(slot(x, "Polygons"), slot, "area"))
    ur.area.s = lapply(ur.area,sum)

    wisc.spat.o@data$muni.area = unlist(ur.area.s) / 1000000
    
    pop.join = with(model.r.df, data.frame(muni_county,pop))
    wisc.spat.o@data = join(wisc.spat.o@data, pop.join, by = "muni_county")
    
    wisc.spat.o@data$muni.dense = as.numeric(wisc.spat.o@data$pop) / wisc.spat.o@data$muni.area
    density.join = with(wisc.spat.o@data, data.frame(muni_county,muni.dense))
    
    model.r.df = join(model.r.df, density.join, by = "muni_county")
    
    
    density.join = with(wisc.spat.r@data, data.frame(muni_county,muni.dense))

    model.r.df = join(model.r.df, density.join, by = "muni_county")

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
  {
    require(rgdal); require(sp); require(geojson)

    # wisc.spat.r <-
    #   readOGR(
    #     dsn = "Prepped files/wisc.present.orig.geojson",
    #     layer = "OGRGeoJSON",
    #     disambiguateFIDs = T,
    #     stringsAsFactors = F
    #   )
    

    wisc.spat.r.cut = wisc.spat.r
    wisc.spat.r.cut@data[7:length(wisc.spat.r.cut@data)] = NULL
    wisc.spat.r.cut@data$MCD_NAME = toupper(wisc.spat.r.cut@data$MCD_NAME)

    wisc.join = wisc.spat.r.cut

    wisc.join@data = join(wisc.join@data, model.r.df, by = "muni_county", match = "first")
    colnames(wisc.join@data) = make.unique(colnames(wisc.join@data))
    # wisc.join@data$county_machine_vendor_dealer = as.character( wisc.join@data$county_machine_vendor_dealer)
    wisc.join@data$demwinner = as.character( wisc.join@data$demwinner)
    wisc.join@data$demwinner2012 = as.character( wisc.join@data$demwinner2012)

    wisc.join@data$county = NULL
    wisc.join@data$MCD_FIPS.1 = NULL

    wisc.join@data[wisc.join@data == Inf] = NA
    wisc.join@data[wisc.join@data == -Inf] = NA

    wisc.join@data[is.na(wisc.join@data)] = NA

    # unique(wisc.join@data$muni_county=="NA")
    # wisc.join = wisc.join[-wisc.join@data$muni_county=="NA",]
    # wisc.join = subset(wisc.join,wisc.join@data$muni_county!="NA")

    wisc.join.df = as.data.frame(wisc.join)
    # str(wisc.join.df)

    # wisc.join@data = clean_names(wisc.join@data)
    # require(geojson)

    writeOGR(
      wisc.join,
      "Prepped files/wisc.present.rcnt.geojson",
      layer = "wisc.join",
      driver = "GeoJSON"
    )

    # geo_write(wisc.join, file = "~/Prepped files/wisc.present.rcnt.geojson")

    # writeOGR(
    #   wisc.join,
    #   "Prepped files/wisc.present.rcnt.shp",
    #   layer = "wisc.join",
    #   driver = "ESRI Shapefile"
    # )

  } # Creating a carto-presentable map

  write.csv(model.r.df,"modeldfward.r.csv")

  # The following are the variables used by the Election snapshot in their models:
  # Income 2015, inc_2015
  # Unemployment rate Sept 2015, unemp_rate
  # Male ACS 5-year, pct_male
  # Age 62 + ACS 5 year, age_med
  # Hispanic or Latino ACS 5 year, pct_latino
  # White ACS 5-year, pct_white
  # Black ACS 5 year, pct_black
  # HS (high school) or less ACS 5 year, pct_hs

# Models repunit data 2012 2016 original --------------------------------------------------
  
  # NOTE: Multiplying the two voting method variables together improves the models, and reduces
  # The correlation with the other variables
  {
    ## Touchscreen and BMD use are super highly correlated, as are use.machines and os.use
    ## With turnout not logged
    corr.table = with(
      repunit.2016.2012.o.dem,
      data.frame(
        dem.change.perc,
        rep.change.perc,
        oth.change.perc,
        # turnout.change.perc,
        votes.tot2016,
        # county_use_opt_scan,
        tch_prop,
        # bmd.use,
        os.use,
        inc_2015,
        unemp_rate,
        pct_male,
        age_med,
        # pct_white,
        pct_latino,
        pct_black,
        pct_hs,
        pop_sq_mile_2010,
        pct_white,
        demwinner
      )
    )
    colnames(corr.table) = abbreviate( colnames(corr.table), minlength = 16 )
    # colnames(corr.table)[6] = "voting.method"
    # colnames(corr.table)[10] = "med.income"
    correlationmatrix = cor(corr.table, use = "pairwise.complete.obs")
    corrplot.all = corrplot(correlationmatrix, method = "number", type = "upper")
  } # Correlations
  { # Model data frame preparation
setwd("C:/Users/s_cas/Dropbox/Perso/2016 voting election county results/Wisconsin/All prep together/All processed files/")
    
    repunit.2016.2012.o.dem$log.pop.sq.mile.2010 = log(repunit.2016.2012.o.dem$pop_sq_mile_2010)
    
    ward.2016.2012.o.dem.mcdjoin = with(ward.2016.2012.o.dem, data.frame(muni_county, MCD_FIPS))
    
    repunit.2016.2012.o.dem = join(repunit.2016.2012.o.dem, ward.2016.2012.o.dem.mcdjoin, by = "muni_county")
    
    repunit.2016.2012.o.dem$total
    
    model.o.repunit.df = with(repunit.2016.2012.o.dem, data.frame(MCD_FIPS,reporting_unit,reporting_unit_orig,county, muni_county,
                                                       democrat_votes.perc, republican_votes.perc, other_votes.perc,
                                                       dem.change.perc,rep.change.perc,oth.change.perc,
                                                       tch_prop, os_prop, counted_absent_prop,rejected_absent_prop,
                                                       absentee_person_prop,
                                                       sameday_reg_prop,turnout_reg_feb,#turnout.2016.perc,
                                                       # county_paper_or_paperplusmachine, #county_use_opt_scan,
                                                       #county_machine_vendor_dealer, #turnout.change.perc,
                                                       # ratio_nocollege_college , log.pop.sq.mile.2010,
                                                       # pop_sq_mile_2010,#tch.use, os.use, bmd.use,
                                                       # medianhouseholdincome_2009.2013, #voters.tot2016, 
                                                       votes.tot2016, inspector_prop,
                                                       dem.perc2012, rep.perc2012, oth.perc2012, #turnout.2012,
                                                       votes.tot2012, 
                                                       # inc_2015,inc_2015, unemp_rate, X._collegedegreepersons25._2013, pct_graduate,
                                                       # pct_male, age_med, pct_latino, pct_white, pct_black, pct_white,
                                                       pct_hs,
                                                        inspector_prop_2012,
                                                       total_ballots,registrants,late_registrants,tch_prop_2012,
                                                       os_prop_2012, automark_prop_2012,
                                                       total_ballots_2012,turnout_reg_2012, counted_absent_prop_2012,
                                                      rejected_absent_prop_2012, absentee_person_prop_2012,
                                                      registrants_2012,late_registrants_2012,
                                                       sameday_reg_prop_2012
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
    
    model.o.repunit.df = join(model.o.repunit.df,tig_dems_join, by = "MCD_FIPS")
    
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
    
    model.o.repunit.df = join(model.o.repunit.df,repprim_join,by="reporting_unit")
    model.o.repunit.df$trump_vote_diff = model.o.repunit.df$republican_votes.perc - model.o.repunit.df$votes_perc_trump_reprim
    model.o.repunit.df$tch_prop_reprim_diff = model.o.repunit.df$tch_prop - model.o.repunit.df$tch_prop_reprim
    
    model.o.repunit.df = join(model.o.repunit.df,demprim_join,by="reporting_unit")
    model.o.repunit.df$clinton_vote_diff = model.o.repunit.df$democrat_votes.perc - model.o.repunit.df$votes_perc_clinton_demprim
    model.o.repunit.df$tch_prop_demprim_diff = model.o.repunit.df$tch_prop - model.o.repunit.df$tch_prop_demprim
    
  } # Adding in vote change from Spring primary
  { # Adding in vote change from 2012 to primaries
    model.edl2012.df = read.csv("edl.2012.municounty.data.csv")
    # edl_join = model.edl2012.df[,c(2:12)]
    # colnames(edl_join)[4:11] <- paste(colnames(edl_join)[4:11],"2012_num", sep = "_")
    
    # model.o.repunit.df = join(model.o.repunit.df,edl_join,by="muni_county")
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
    
    model.o.repunit.df = join(model.o.repunit.df, off_vote_machines, by = "muni_county")
    
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
    # wisc.spat.o@data = join(wisc.spat.o@data, pop.join, by = "muni_county")
    # 
    # wisc.spat.o@data$muni.dense = as.numeric(wisc.spat.o@data$pop) / wisc.spat.o@data$muni.area
    # density.join = with(wisc.spat.o@data, data.frame(muni_county,muni.dense))
    
    density.join = with(model.o.df, data.frame(muni_county,muni.dense))
    
    model.o.repunit.df = join(model.o.repunit.df, density.join, by = "muni_county",match="first")
    
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
    
    wisc.spat.o.cut = wisc.spat.o
    wisc.spat.o.cut@data[7:length(wisc.spat.o.cut@data)] = NULL
    wisc.spat.o.cut@data$MCD_NAME = toupper(wisc.spat.o.cut@data$MCD_NAME)
    
    wisc.join = wisc.spat.o.cut
    
    wisc.join@data = join(wisc.join@data, model.o.repunit.df, by = "muni_county", match = "first")
    colnames(wisc.join@data) = make.unique(colnames(wisc.join@data))
    wisc.join@data$county_machine_vendor_dealer = as.character( wisc.join@data$county_machine_vendor_dealer)
    wisc.join@data$demwinner = as.character( wisc.join@data$demwinner)
    wisc.join@data$demwinner2012 = as.character( wisc.join@data$demwinner2012)
    
    wisc.join@data$county = NULL
    wisc.join@data$MCD_FIPS.1 = NULL
    
    wisc.join@data[wisc.join@data == Inf] = NA
    wisc.join@data[wisc.join@data == -Inf] = NA
    
    wisc.join@data[is.na(wisc.join@data)] = NA
    
    # unique(wisc.join@data$muni_county=="NA")
    # wisc.join = wisc.join[-wisc.join@data$muni_county=="NA",]
    # wisc.join = subset(wisc.join,wisc.join@data$muni_county!="NA")
    
    wisc.join.df = as.data.frame(wisc.join)
    # str(wisc.join.df)
    
    # wisc.join@data = clean_names(wisc.join@data)
    # require(geojson)
    
    writeOGR(
      wisc.join,
      "Prepped files/wisc.present.orig.repunit.geojson",
      layer = "wisc.join",
      driver = "GeoJSON"
    )
    
    # geo_write(wisc.join, file = "~/Prepped files/wisc.present.orig.geojson")
    
    # writeOGR(
    #   wisc.join,
    #   "Prepped files/wisc.present.orig.shp",
    #   layer = "wisc.join",
    #   driver = "ESRI Shapefile"
    # )
    
  } # Creating a carto-presentable map
  
  write.csv(model.o.repunit.df,"modeldfward.repunit.o.csv")
  

# Comparing reporting units to mean without touchscreens ------------------
  model.o.repunit.df = read.csv("modeldfward.repunit.o.csv")
reporting_units_compared = with(model.o.repunit.df, data.frame(reporting_unit_orig, muni_county, county, 
                                                               rep.change.perc, republican_votes.perc, rep.perc2012, 
                                                               votes_perc_trump_reprim, other_votes.perc,
                                                               total_ballots, registrants, turnout_reg_feb,
                                                               tch_model_off_full, tch_prop))
reporting_units_compared$tch_users = with(model.o.repunit.df, tch_prop*total_ballots)
reporting_units_compared$avc_users = NA
reporting_units_compared$avc_users = with(reporting_units_compared,ifelse(tch_model_off_full == "Dominion (Sequoia)/Command Central-Edge",tch_prop*total_ballots, avc_users))

reporting_units_compared$ave_repchange_zero_tch_use = with(model.o.repunit.df,mean(rep.change.perc[tch_prop == 0],na.rm=T))
reporting_units_compared$ave_repchange_less25_avc_use = with(model.o.repunit.df,
                                                             mean(rep.change.perc[tch_prop < 0.25 &
                                                            tch_model_off_full == "Dominion (Sequoia)/Command Central-Edge"
                                                              ],na.rm=T))
reporting_units_compared$ave_repchange_more25_avc_use = with(model.o.repunit.df,
                                                             mean(rep.change.perc[tch_prop >= 0.25 &
                                                                                    tch_model_off_full == "Dominion (Sequoia)/Command Central-Edge"
                                                                                  ],na.rm=T))
reporting_units_compared$rep.change.perc.over.average_0 = with(reporting_units_compared,rep.change.perc - ave_repchange_zero_tch_use)

reporting_units_compared$rep.change.perc.people.increase = with(reporting_units_compared,(rep.change.perc / 100)*total_ballots)


reporting_units_compared$rep.change.perc.prop.of.avc.use.reqd = with(reporting_units_compared, rep.change.perc.people.increase / avc_users)

reporting_units_compared[reporting_units_compared == Inf] = NA
reporting_units_compared[reporting_units_compared == -Inf] = NA

colnames(reporting_units_compared) = c("Reporting unit name", "Municipality and county", "County", "Change in Rep. vote 2012 to 2016",
                                       "Rep. vote 2016", "Rep. vote 2012", "Trump vote in primary", "Vote for third party candidates 2016",
                                       "Voter number 2016", "Number of registered voters 2016", "Registered voter turnout 2016",
                                       "Touchscreen model installed in municipality", "Proportion of voters using voting machines",
                                       "Number of voters using voting machines", "Number of voters using AVC Edge",
                                       "Average Rep. vote change 2012-2016 in reporting units with 0% touchscreen use",
                                       "Average Rep. vote change 2012-2016 in reporting units with 0-25% AVC Edge use",
                                       "Average Rep. vote change 2012-2016 in reporting units with >25% AVC Edge use",
                                       "Rep. vote change 2012-2016 above average for 0% touchscreen use",
                                       "No. of voters voting Rep. above average vote change 2012-2016 with 0% touchscreen use",
                                       "Proportion of votes from AVC Edge required to flip to match difference from 0% touchscreen use average in Rep. vote change 2012-2016"                                       )
  
write.csv(reporting_units_compared,"reporting_units_avc.csv")

# Comparing 2016 candidate results overall -------------------------------------
# The majority of 'other' voters went for Gary Johnson. Hillary massively underperformed compared
# to decided voter polling, and Trump slightly overperformed. This means that more of the 'undecideds'
# went to Trump, or that many undecideds that would have voted Hillary chose not to turn up.

candidate.2016.summary.vis.defs = ggplot(candidate.2016.summary,
                                         aes(x = ordered.cands, y = votes.perc, fill = cand,group =1)) +
  geom_bar(stat = "identity") +
  geom_line(stat = "identity",
            aes(y = elections.100.years.summary.2016$mean.dem.poll.oneweek.defs),
            colour = "blue", linetype = 2,
            show.legend = F) +
  geom_line(stat = "identity",
            aes(y = elections.100.years.summary.2016$mean.rep.poll.oneweek.defs),
            colour = "red", linetype = 2,
            show.legend = F) +
  geom_line(stat = "identity",
            aes(y = elections.100.years.summary.2016$mean.oth.poll.oneweek.defs),
            colour = "black", linetype = 2,
            show.legend = F) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_y_continuous(
    name =
      "Percentage votes for the candidates.Lines indicate polling\n of decided voters,excluding those who were unsure."
  ) +
  scale_x_discrete(
    limit =  candidate.2016.summary$ordered.cands,
    labels = as.character(candidate.2016.summary$cand),
    name = NULL) +
  scale_fill_manual(values = colours)
candidate.2016.summary.vis.defs

# This is a cheesy way to remove all the other candidates apart from Johnson
candidate.2016.summary.small = subset(candidate.2016.summary, votes.rec > 100000)

# Comparing polls of ALL with results
# poll.2016.compare.vis.all = ggplot(candidate.2016.summary.small, aes(x = cand.group, y = diff.from.poll, fill = cand.group, group = 1)) +
#   # geom_line(stat = "identity",show.legend = F, aes(y = elections.100.years.summary.2016$polls.nov)) +
#   geom_bar(stat = "identity", show.legend = F) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   coord_cartesian(ylim = c(-6,6)) +
#   scale_y_continuous(
#     name =
#       "The % difference between all voter polls\n and the final result of the 2016 presidential election."
#   ) +
#   scale_x_discrete(
#     limit =  candidate.2016.summary.small$cand.group,
#     labels = as.character(candidate.2016.summary.small$cand.group),
#     name = NULL) +
#   scale_fill_manual(values = c("lightcoral","light blue","dark grey"))
# poll.2016.compare.vis.all

# The divergence between definite voters in polls and reality was greatest for Clinton,
# and the 'other' votes was not much different from the polls. So, either the turnout dropped
# significantly for Clinton, or the undecideds went to Trump
poll.2016.compare.vis.defs = ggplot(candidate.2016.summary.small, aes(x = cand.group, y = diff.from.poll.defs, fill = cand.group, group = 1)) +
  # geom_line(stat = "identity",show.legend = F, aes(y = elections.100.years.summary.2016$polls.nov)) +
  geom_bar(stat = "identity", show.legend = F) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(-5,5)) +
  scale_y_continuous(
    name =
      "The % difference between definite voter polls\n and the final result of the 2016 presidential election."
  ) +
  scale_x_discrete(
    limit =  candidate.2016.summary.small$cand.group,
    labels = as.character(candidate.2016.summary.small$cand.group),
    name = NULL) +
  scale_fill_manual(values = c("lightcoral","light blue","dark grey"))
poll.2016.compare.vis.defs


# Comparing the county 2012 and 2016 polls ---------------------------------------
# Were the polls in 2012 similarly off ?
## Comparing the definite poll voters with the final result

# Comparing polls of ALL with results
# poll.2012.compare.vis.all = ggplot(poll.summary.2012.nounds, aes(x = cand.group, y = diff.from.polls, fill = cand.group, group = 1)) +
#   # geom_line(stat = "identity",show.legend = F, aes(y = elections.100.years.summary.2016$polls.nov)) +
#   geom_bar(stat = "identity", show.legend = F) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   coord_cartesian(ylim = c(-6,6)) +
#   scale_y_continuous(
#     name =
#       "The % difference between all voter polls\n and the final result of the 2012 presidential election."
#   ) +
#   scale_x_discrete(
#     limit =  poll.summary.2012.nounds$cand.group,
#     labels = as.character(poll.summary.2012.nounds$cand.group),
#     name = NULL) +
#   scale_fill_manual(values = c("light blue","lightcoral","dark grey"))
# poll.2012.compare.vis.all


# Comparing polls of decideds with results
# This shows that the polls of the decided votes can be off by 3% ish
poll.2012.compare.vis.defs = ggplot(poll.summary.2012.nounds, aes(x = cand.group, y = diff.from.polls.defs, fill = cand.group, group = 1)) +
  # geom_line(stat = "identity",show.legend = F, aes(y = elections.100.years.summary.2016$polls.nov)) +
  geom_bar(stat = "identity", show.legend = F) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(ylim = c(-5,5)) +
  scale_y_continuous(
    name =
      "The % difference between definite voter polls\n and the final result of the 2012 presidential election."
  ) +
  scale_x_discrete(
    limit =  poll.summary.2012.nounds$cand.group,
    labels = as.character(poll.summary.2012.nounds$cand.group),
    name = NULL) +
  scale_fill_manual(values = c("light blue","lightcoral","dark grey"))
poll.2012.compare.vis.defs


# Compare decided voters for 2016 and 2012 with polls. This shows that it has happened before that
# the polls can be ~3% off
compare.2016.2012.polls.plotnames = c("poll.2016.compare.vis.defs",
                                      "poll.2012.compare.vis.defs")
compare.2016.2012.polls = marrangeGrob(
  grobs = mget(compare.2016.2012.polls.plotnames),
  nrow = 1,
  ncol = 2,
  top = NULL
)
compare.2016.2012.polls


# Overview of county historical elections 1900 onwards ---------------------------

# This graphs shows that the swing from the previous election in terms of democrat to republican
# was not significant
historical.election.data.swing = ggplot(elections.100.years.postwar,
                                  aes(
                                    x = year,
                                    y = swing.vote.perc,
                                    fill = winner,
                                    group = 1
                                  )) +
  geom_bar(stat = "identity") +
  geom_line(
    aes(y = elections.100.years.postwar$swing.turnout.perc),
    alpha = 0.5,
    stat = "identity",
    colour = "black"
  ) +
  scale_y_continuous(
    name =
      "Swing vote percentage from previous election with time.\nLine indicates change in % turnout compared with previous election."
    ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate(
    "text",
    x = mean(elections.100.years.postwar$year),
    y = min(elections.100.years.postwar$swing.vote.perc),
    label = "Vote swing towards Democrats",
    vjust = 1,
    hjust = 0.5
  ) +
  annotate(
    "text",
    x = mean(elections.100.years.postwar$year),
    y = max(elections.100.years.postwar$swing.vote.perc),
    label = "Vote swing towards Republicans",
    vjust = 1,
    hjust = 0.5
  ) +
  scale_fill_manual(values = c("light blue","lightcoral"))
historical.election.data.swing

## The swing is within historical limits, but perhaps the bigger story is the 'other' vote

# Change in Dem votes compared to last election with turnout swing line
historical.election.data.dem.votes = ggplot(elections.100.years.postwar,
                                  aes(
                                    x = year,
                                    y = dem.vote.perc,
                                    fill = winner,
                                    group = 1
                                  )) +
  geom_bar(stat = "identity") +
  geom_line(
    aes(y = elections.100.years.postwar$turnout.perc),
    alpha = 0.5,
    stat = "identity",
    colour = "black"
  ) +
  geom_bar(stat = "identity",fill = "grey", show.legend = FALSE,
           aes(y = oth.vote.perc)
           ) +
  scale_y_continuous(
    name = "Democrat and other votes with time (%). Grey bars indicate other votes.\nLine indicates % turnout.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = seq(1948,2016,4)) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
historical.election.data.dem.votes

# As overall voter numbers instead of percentages. Definitely can see that the 'other' vote
# is substantially larger than previous years. The polls saw this. But who is voting other, and why?
# Maybe I should compare this with country-wide other results to check that the other vote is not out
# of line.

# At time of writing (15-11-16), Trump has: 47.2% votes 60,834,437. Clinton has: 47.9% votes 61,782,016.
# So, other has 100 - (47.2 + 47.9) = 4.9%. Therefore, Wisconsin other vote is higher than average (6.26%).
# Also, the other vote was higher than what the polls predicted by about 1 %.


# Overall voter numbers
historical.election.data.dem.votes.num = ggplot(elections.100.years.postwar,
                                                aes(
                                                  x = year,
                                                  y = dem.vote
                                                )) +
  geom_bar(stat = "identity", aes(fill = winner)) +
  geom_bar(stat = "identity",fill = "grey", show.legend = FALSE,
           aes(y = oth.vote)
  ) +
  geom_line(
    aes(y = elections.100.years.postwar$turnout),group = 1
  ) +

  scale_y_continuous(
    name = "Democrat plus other votes with time (no. of voters).\nLine indicates % turnout."
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = seq(1948,2016,4)) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
historical.election.data.dem.votes.num

# Comparing by county 2016 -------------------------------------------------------------
county.2016.vs.2012 = county.2016.vs.2012[order(county.2016.vs.2012$dem.votes.perc), ]
county.2016.vs.2012$ordered.county.2016.dem.votes.perc = c(1:length(county.2016.vs.2012$dem.votes.perc))

# Visualising votes for clinton by county and by turnout. Turnout does not seem to differ drastically according to
# Republican won or Clinton won county
county.perc.winner = ggplot(
  county.2016.vs.2012,
  aes(
    x = county.2016.vs.2012$ordered.county.2016.dem.votes.perc,
    y = dem.votes.perc,
    fill = winner
  )) +
  geom_bar(
    aes(y = county.2016.vs.2012$turnout.perc.allage.est ),
    stat = "identity",
    fill = "grey"
  ) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(name =
  paste("Percentage vote for Clinton in Wisconsin counties.\nGrey bar behind indicates percentage turnout")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.2016.vs.2012$ordered.county.2016.dem.votes.perc,
    labels = as.character(county.2016.vs.2012$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.perc.winner


length(county.2016.vs.2012$county)
length(county.2016.vs.2012$dem.votes.perc)

county.perc.winner = ggplot(county.2016.vs.2012,
  aes(x = reorder(county, ~dem.votes.perc),     y = dem.votes.perc,     fill = winner)) +
  geom_bar( stat = "identity" )
county.perc.winner

county.2016.vs.2012 = county.2016.vs.2012[order(county.2016.vs.2012$oth.votes.perc), ]
county.2016.vs.2012$ordered.county.2016.oth.votes.perc = c(1:length(county.2016.vs.2012$oth.votes.perc))

# Visualising votes for other by county and by turnout. Turnout does not seem to differ drastically according to
# Republican won or Clinton won county
county.perc.winner.other = ggplot(
  county.2016.vs.2012,
  aes(
    x = county.2016.vs.2012$ordered.county.2016.oth.votes.perc,
    y = oth.votes.perc,
    fill = winner
  )) +
  geom_bar(
    aes(y = county.2016.vs.2012$swing.turnout.perc),
    stat = "identity",
    fill = "grey", width = 0.8
  ) +
  geom_bar(stat = "identity", alpha = 0.6) +
  scale_y_continuous(name =
                       paste("Percentage vote for 'Other' in Wisconsin counties.\nGrey bar behind indicates change in turnout from 2012 (%)")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.2016.vs.2012$ordered.county.2016.oth.votes.perc,
    labels = as.character(county.2016.vs.2012$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.perc.winner.other


# Visualising votes for clinton against turnout
# Votes for other parties was pretty consistent regardless of being mostly dem or mostly republican
county.perc.turnout.dems = ggplot(
  county.2016.vs.2012.2000ppl,
  aes(
    y = dem.votes.perc,
    x = turnout.perc.allage.est,
    colour = winner
  )
) +
  geom_point() +
  geom_smooth(colour = "black") +
  scale_y_continuous(name = "Percentage vote for Clinton in Wisconsin counties\nagainst turnout in county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # facet_wrap(~mostly.dem, ncol = 1, nrow = 2) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.perc.turnout.dems

correlation(county.2016.vs.2012.2000ppl$dem.votes.perc,county.2016.vs.2012.2000ppl$turnout.perc.allage.est)
# 0.1840505

# Visualising votes for clinton against other.
# Counties closest to middle most likely to vote other
county.perc.oth.vs.dem = ggplot(
  county.2016.vs.2012.2000ppl,
  aes(
    x = oth.votes.perc,
    y = dem.votes.perc,
    colour = winner
  )) +
  geom_point() +
  geom_smooth(colour = "black") +
  scale_y_continuous(name ="Percentage vote for Clinton in counties against votes for 'other'.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values = c("light blue","lightcoral"))
  county.perc.oth.vs.dem

# Visualising votes for other against turnout
# Votes for other parties was pretty consistent regardless of being mostly dem or mostly republican
county.perc.turnout.other = ggplot(
  county.2016.vs.2012.2000ppl,
  aes(
    x = turnout.perc.allage.est,
    y = oth.votes.perc,
    colour = winner
  )
) +
  geom_point() +
  geom_smooth(colour = "black") +
  scale_y_continuous(name = "Percentage vote for Clinton in Wisconsin counties\nagainst turnout in county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~mostly.dem, ncol = 1, nrow = 2) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.perc.turnout.other


# Visualising the swing in votes
county.summary.2016.final = county.summary.2016.final[order(county.summary.2016.final$swing.perc), ]
county.summary.2016.final$ordered.county.swing.perc = c(1:length(county.summary.2016.final$swing.perc))

# Percentage vote change
county.2012v2016.swing.perc = ggplot(county.summary.2016.final,
                                     aes(x = ordered.county.swing.perc, y = swing.perc, fill = winner)) +
  geom_bar(stat = "identity") + geom_bar(stat = "identity",
                                         aes(y = (
                                           county.summary.2016.final$swing.turnout.perc
                                         )),
                                         fill = "grey",
                                         width = 0.4) +
  scale_y_continuous(
    name = "Difference in 2016 vote between Democrats and Republicans for counties (%).<br> Grey bars: difference in percentage turnout from the 2012 election.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 4)) +
  scale_x_discrete(
    limit =  county.summary.2016.final$ordered.county.swing.perc,
    labels = as.character(county.summary.2016.final$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.2012v2016.swing.perc
# ggplotly(county.2012v2016.swing.perc, session = "knitr") %>% # , width = 800, height = 600
# layout(bargap = 3, autosize=T, margin = default.margin) # l, r, b, t, pad


# Visualising votes for other against turnout
# Votes for other parties was pretty consistent regardless of being mostly dem or mostly republican
county.num.turnout.other = ggplot(
  county.2016.vs.2012,
  aes(
    x = log10(turnout),
    y = use.machines.prop,
    colour = winner
  )) +
  geom_point() +
  # geom_smooth(colour = "black") +
  scale_y_continuous(name = "Percentage vote for Clinton in Wisconsin counties\nagainst turnout in county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~mostly.dem, ncol = 1, nrow = 2) +
  scale_colour_manual(values = c("light blue","lightcoral"))
county.num.turnout.other


# Visualising votes for other against percentage machines
# Votes for other parties was pretty consistent regardless of the proportion of machines
county.perc.machines.other = ggplot(
  county.2016.vs.2012,
  aes(
    x = use.machines.prop,
    y = oth.votes.perc,
    colour = winner
  )) +
  geom_point() +
  geom_smooth(colour = "black") +
  scale_y_continuous(name = "Percentage vote for other in counties\nagainst turnout in county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~mostly.dem, ncol = 1, nrow = 2) +
  scale_colour_manual(values = c("light blue","lightcoral"))
county.perc.machines.other


# Visualising votes for other against percentage machines
# Votes for other parties in total amount seemed to be higher in 100% machine states
county.perc.winner = ggplot(
  county.2016.vs.2012,
  aes(
    x = use.machines.prop,
    y = oth.votes,
    colour = winner
  )
) +
  geom_point() +
  geom_smooth(colour = "black") +
  scale_y_continuous(name =
                       paste("Percentage vote for Clinton in Wisconsin counties\nagainst turnout in county.")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~mostly.dem, ncol = 1, nrow = 2) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.perc.winner



county.summary.join = county.2016.vs.2012[c(2,9,23,32:39)]
voting.bycandcounty.2016.joined = join(voting.bycandcounty.2016,county.summary.join,by="county")

voting.bycandcounty.2016.joined.johnson = subset(voting.bycandcounty.2016.joined, cand == "Gary Johnson")

# Visualising votes for johnson against percentage machines
# Votes for johnson in total amount seemed to be higher in 100% machine states
county.perc.winner = ggplot(
  voting.bycandcounty.2016.joined.johnson,
  aes(
    x = use.machines.prop,
    y = votes.perc
  )
) +
  geom_point() +
  scale_y_continuous(name =
                       paste("Percentage vote for Johnson in Wisconsin counties\nagainst machines in county.")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # facet_wrap(~mostly.dem, ncol = 1, nrow = 2) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.perc.winner

correlation(voting.bycandcounty.2016.joined.johnson$use.machines.prop,voting.bycandcounty.2016.joined.johnson$votes.perc)

# Visualising votes for johnson against turnout
# Votes for other parties in total amount seemed to be higher in 100% machine states
county.perc.winner = ggplot(
  voting.bycandcounty.2016.joined.johnson,
  aes(
    x = turnout.perc.allage.est,
    y = votes.perc
  )
) +
  geom_point() +
  scale_y_continuous(name =
                       paste("Percentage vote for Clinton in Wisconsin counties\nagainst turnout in county.")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~mostly.dem, ncol = 1, nrow = 2) +
  scale_fill_manual(values = c("light blue","lightcoral"))
county.perc.winner


county.2016.vs.2012 = county.2016.vs.2012[order(county.2016.vs.2012$turnout.perc.allage.est), ]
county.2016.vs.2012$ordered.county.2016.turnout.perc.diff = c(1:length(county.2016.vs.2012$turnout.perc.allage.est))

# Visualise county turnout data as percentage
county.perc.turnout.winner = ggplot(
  county.2016.vs.2012,
  aes(x = ordered.county.2016.turnout.perc.diff, y = turnout.perc.allage.est,
      fill = winner), group = 1
) +
  geom_bar(stat = "identity") + scale_y_continuous(name = "Estimated turnout in Trump and Clinton won counties.") +
  geom_line(aes(y = elections.100.years.summary.2016$turnout.perc), linetype = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.2016.vs.2012$ordered.county.2016.turnout.perc.diff,
    labels = as.character(county.2016.vs.2012$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.perc.turnout.winner



## VISUALISING THE 2016 RESULTS
county.2016.vs.2012 = county.2016.vs.2012[order(county.2016.vs.2012$perc.diff), ]
county.2016.vs.2012$ordered.county.2016.perc.diff = c(1:length(county.2016.vs.2012$perc.diff))

# Absolute differece in votes as percentage
county.perc.diff = ggplot(county.2016.vs.2012,
                          aes(x = ordered.county.2016.perc.diff, y = perc.diff,
                              fill = winner)) +
  geom_bar(stat = "identity") + scale_y_continuous(name = "Percentage difference in vote between Trump and Clinton by county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.2016.vs.2012$ordered.county.2016.perc.diff,
    labels = as.character(county.2016.vs.2012$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.perc.diff

county.2016.vs.2012 = county.2016.vs.2012[order(county.2016.vs.2012$turnout), ]
county.2016.vs.2012$ordered.county.2016.turnout = c(1:length(county.2016.vs.2012$turnout))




# Votes as numbers
county.2016.trump.num = ggplot(county.2016.vs.2012,
                               aes(x = ordered.county.2016.turnout, y = rep.votes,
                                   fill = winner)) +
  geom_bar(
    stat = "identity",
    aes(y = county.2016.vs.2012$turnout),
    fill = "grey",
    alpha = 0.5
  ) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Number of votes for Trump by county, over turnout per county.") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.2016.vs.2012$ordered.county.2016.turnout,
    labels = as.character(county.2016.vs.2012$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.2016.trump.num

votes.2016.compare.plotnames = c("county.perc.diff", "county.2016.trump.num")
votes.2016.compare = marrangeGrob(
  grobs = mget(votes.2016.compare.plotnames),
  nrow = 1,
  ncol = 2,
  top = NULL
)
votes.2016.compare

# Comparing 2012 and 2016 results by county --------------------------------

# ABSOLUTE DIFFERENCES BETWEEN 2012 AND 2016 ##

# Visualising county data 2012 vs 2016
{
  county.2016.vs.2012 = county.2016.vs.2012[order(county.2016.vs.2012$diff.2012.perc), ]
  county.2016.vs.2012$ordered.county.2012.perc.diff = c(1:length(county.2016.vs.2012$diff.2012.perc))

county.2012v2016.perc.diff.2016ord = ggplot(county.2016.vs.2012,
                                            aes(x = ordered.county.2012.perc.diff, y = diff.2012.perc)) +
  geom_bar(stat = "identity") + geom_bar(
    stat = "identity",
    aes(y = county.2016.vs.2012$perc.diff),
    fill = "red",
    alpha = 0.5
  ) +
  scale_y_continuous(
    name = "Percentage vote difference in 2012 and 2016 by county.Grey bars are 2012 results,\nred bars are overlaid 2016 results. Negative = Democrat win, positive = Republican win"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.2016.vs.2012$ordered.county.2012.perc.diff,
    labels = as.character(county.2016.vs.2012$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.2012v2016.perc.diff.2016ord
}

# As number instead of percentage
{
# county.differences.years.num = ggplot(county.2016.vs.2012,
#                                       aes(x = ordered.county.2012.perc.diff, y = diff.2012.num)) +
#   geom_bar(stat = "identity") + geom_bar(
#     stat = "identity",
#     aes(y = county.2016.vs.2012$num.diff),
#     fill = "red",
#     alpha = 0.5
#   ) +
#   scale_y_continuous(
#     name = "Number of votes difference between Democrat and Republican parties in 2012 and 2016 by county. Grey bars are 2012 results,\nred bars are overlaid 2016 results. Negative = Democrat win, positive = Republican win"
#   ) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_x_discrete(
#     limit =  county.2016.vs.2012$ordered.county.2012.perc.diff,
#     labels = as.character(county.2016.vs.2012$county),
#     name = NULL
#   ) +
#   scale_fill_manual(values = c("light blue", "lightcoral"))
# county.differences.years.num
#
# absolute.2016.2012.compare.plotnames = c("county.2012v2016.perc.diff.2016ord",
#                                          "county.differences.years.num")
# absolute.2016.2012.compare = marrangeGrob(
#   grobs = mget(absolute.2016.2012.compare.plotnames),
#   nrow = 2,
#   ncol = 1,
#   top = NULL
# )
# absolute.2016.2012.compare
}

# Visualising the swing in votes
{
county.summary.2016.final = county.summary.2016.final[order(county.summary.2016.final$swing.perc), ]
county.summary.2016.final$ordered.county.swing.perc = c(1:length(county.summary.2016.final$swing.perc))

# Percentage vote change
county.2012v2016.swing.perc = ggplot(county.summary.2016.final,
                                     aes(x = ordered.county.swing.perc, y = swing.perc, fill = winner)) +
  geom_bar(stat = "identity") + geom_bar(stat = "identity",
                                         aes(y = (
                                           county.summary.2016.final$swing.turnout.perc
                                         )),
                                         fill = "grey",
                                         width = 0.4) +
  scale_y_continuous(
    name = "Swing percentage between 2012 and 2016 by county. Blue/red bars indicate percentage swing,
below 0 = swing to Democrat, above 0 = swing to Republican. Grey bars are percentage swing in turnout."
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(
    limit =  county.summary.2016.final$ordered.county.swing.perc,
    labels = as.character(county.summary.2016.final$county),
    name = NULL
  ) +
  scale_fill_manual(values = c("light blue", "lightcoral"))
county.2012v2016.swing.perc

# As number
# county.2012v2016.swing.num = ggplot(county.summary.2016.final,
#                                     aes(x = ordered.county.swing.perc, y = swing.num, fill = winner)) +
#   geom_bar(stat = "identity") + geom_bar(
#     stat = "identity",
#     aes(y = county.summary.2016.final$swing.turnout),
#     fill = "grey",
#     alpha = 0.8
#   ) +
#   scale_y_continuous(
#     name = "Swing between 2012 and 2016 by county as no. voters. Blue/red bars indicate swing as no. voters,\nbelow 0 = swing to Democrat, above 0 = swing to Republican. Grey bars show swing in turnout as voter numbers.",
#     breaks = c(seq(-60000,20000,10000))
#   ) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_x_discrete(
#     limit =  county.summary.2016.final$ordered.county.swing.perc,
#     labels = as.character(county.summary.2016.final$county),
#     name = NULL
#   ) +
#   scale_fill_manual(values = c("light blue", "lightcoral"))
# county.2012v2016.swing.num
#
# swing.votes.2016.2012.compare.plotnames = c("county.2012v2016.swing.perc",
#                                             "county.differences.years.num")
# swing.votes.2016.2012.compare = marrangeGrob(
#   grobs = mget(swing.votes.2016.2012.compare.plotnames),
#   nrow = 2,
#   ncol = 1,
#   top = NULL
# )
# swing.votes.2016.2012.compare
}


# Change in those voting 'Democrat' between 2012 and 2016 by turnout
{
  county.2012v2016.change.dem.perc.point = ggplot(county.summary.2016.final.2000ppl,
                                                  aes(x = swing.turnout.perc, y = dem.change, colour = winner)) +
    geom_smooth(alpha = 0.5, colour = "grey", method = "lm", se = F) +
    geom_point(stat = "identity") +
    scale_y_continuous(
      name = "Change in vote for Democrats 2012 - 2016 (%)"
    ) +
    scale_x_continuous(
      name = "Change in turnout 2012 - 2016 (%)"
    ) +
    geom_text(x = -8, y = 2,
              label = corr_eqn(county.summary.2016.final.2000ppl$swing.turnout.perc,
                               county.summary.2016.final.2000ppl$dem.change), parse = TRUE,
              show.legend = F, colour = "black") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_colour_manual(values = c("light blue", "lightcoral"))
  county.2012v2016.change.dem.perc.point
}

# Change in those voting 'other' between 2012 and 2016 by turnout
{
county.2012v2016.change.oth.perc.point = ggplot(county.summary.2016.final.2000ppl,
                                                aes(x = swing.turnout.perc, y = oth.change, colour = winner)) +
  geom_smooth(alpha = 0.5, colour = "grey", method = "lm", se = F) +
  geom_point(stat = "identity") +
  scale_y_continuous(
    name = "Change in vote for other 2012 - 2016 (%)"
  ) +
    scale_x_continuous(
      name = "Change in turnout 2012 - 2016 (%)"
    ) +
  geom_text(x = -8, y = 5,
            label = corr_eqn(county.summary.2016.final.2000ppl$swing.turnout.perc,
                              county.summary.2016.final.2000ppl$oth.change), parse = TRUE,
            show.legend = F, colour = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_colour_manual(values = c("light blue", "lightcoral"))
county.2012v2016.change.oth.perc.point
}

# Change in those voting dem - those between 2012 and 2016
{
  # The bigger the change from Democrat, the lower the change in other
  county.2012v2016.change.oth.dem.point = ggplot(county.summary.2016.final.2000ppl,
                                                 aes(x = dem.change, y = oth.change, colour = winner)) +
    geom_smooth(alpha = 0.5, colour = "grey", method = "lm", se = F) +
    geom_point(stat = "identity") +
    scale_y_continuous(
      name = "Change in voting other from 2012 (%)"
    ) +
    scale_x_continuous(
      name = "Change in Democrat from 2012 - 2016 (%)"
    ) +
    geom_text(x = 0, y = 5,
              label = corr_eqn(county.summary.2016.final.2000ppl$dem.change,
                               county.summary.2016.final.2000ppl$oth.change), parse = TRUE,
              show.legend = F, colour = "black") +
    theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
    scale_colour_manual(values = c("light blue", "lightcoral"))
  county.2012v2016.change.oth.dem.point
}


# County Correlation plot --------------------------------------------------------
# plot(log(county.summary.2016.final.2000ppl$total.voting.age))
# plot(county.summary.2016.final.2000ppl$total.voting.age)

## With turnout not logged
corr.table = with(county.summary.2016.final.2000ppl, data.frame(dem.change, rep.change,
                                                                oth.change,swing.turnout.perc,turnout,
                                                                use.machines.prop, county_paper_or_paperplusmachine,
                                                                dem.wins,
                                                                pct_white,ratio_white_black,
                                                                ratio_nocollege_college,medianhouseholdincome_2009.2013,
                                                                pop_sq_mile_2010
))
colnames(corr.table)[7] = "voting.method"
colnames(corr.table)[12] = "med.income"
correlationmatrix = cor(corr.table, use = "pairwise.complete.obs")
corrplot.all = corrplot(correlationmatrix, method = "number", type = "upper")


# With turnout logged
corr.table = with(county.summary.2016.final.2000ppl, data.frame(dem.change, rep.change,
                                                  oth.change,swing.turnout.perc,log(turnout),
                                                  use.machines.prop, county_paper_or_paperplusmachine,
                                                  dem.wins,
                                                  pct_white,ratio_white_black,
                                                  ratio_nocollege_college,medianhouseholdincome_2009.2013,
                                                  pop_sq_mile_2010
                                                  ))
colnames(corr.table)[7] = "voting.method"
colnames(corr.table)[12] = "med.income"
correlationmatrix = cor(corr.table, use = "pairwise.complete.obs")
corrplot.all = corrplot(correlationmatrix, method = "number", type = "upper")

require(nlme)
# Following Nate Silver's method: https://twitter.com/Nate_Cohn/status/801226924156719104/photo/1?ref_src=twsrc%5Etfw
# I found it's better to use the non-logged version of pop.sq.mile.2010
county.summary.2016.final$log.pop.sq.mile.2010 = log(county.summary.2016.final$pop_sq_mile_2010)


model.df = with(county.summary.2016.final, data.frame(county,dem.change,rep.change,oth.change,turnout.change,
                                                        county_paper_or_paperplusmachine , use.machines.prop ,
                                                        pct_white , ratio_nocollege_college , pop_sq_mile_2010 ,
                                                        medianhouseholdincome_2009.2013, turnout,total.voting.age, dem.wins,
                                                        dem.2016, dem.vote.perc, rep.vote.perc, oth.vote.perc,
                                                      tch.use.prop, inc_2015,unemp_rate,
                                                      pct_male, age_med, pct_latino, pct_white,
                                                      pct_black,pct_hs))
colnames(model.df)[6] = "papernopaper"
colnames(model.df)[7] = "scan.machines.prop"

write.csv(model.df,"modeldf.csv")
model.df = read.csv("modeldf.csv")

model.df$demwins.h = NA
model.df$demwins.h = ifelse(model.df$dem.wins > 2, "1","0")

require(car)

dem.vote.lm = lm(dem.vote.perc ~ tch.use.prop + scan.machines.prop +
                   inc_2015 + demwins.h*tch.use.prop + pct_male +
                   age_med + pct_latino + pct_black + pct_hs,
                 data = model.df, weights = log(turnout))
summary(dem.vote.lm)
plot(dem.vote.lm)
vif(dem.vote.lm)
AIC(dem.vote.lm)

dem.vote.lm = lm(dem.vote.perc ~ tch.use.prop + scan.machines.prop +
                   inc_2015 + demwins.h*tch.use.prop + pct_male +
                   age_med + pct_latino + pct_black + pct_hs,
                 data = model.df)
summary(dem.vote.lm)
vif(dem.vote.lm)
AIC(dem.vote.lm)

# dem.vote.lm = lm(dem.vote.perc ~ tch.use.prop + scan.machines.prop +
#                    pct_white + ratio_nocollege_college + pop_sq_mile_2010+
#                    medianhouseholdincome_2009.2013 + demwins.h*tch.use.prop,
#                  data = model.df)
# summary(dem.vote.lm)

dem.vote.lm = lm(dem.change ~ tch.use.prop + scan.machines.prop +
                   inc_2015 + demwins.h + pct_male +
                   age_med + pct_latino + pct_black + pct_hs,
                 data = model.df, weights = log(turnout))
summary(dem.vote.lm)
vif(dem.vote.lm)
AIC(dem.vote.lm)

dem.vote.lm = lm(dem.change~ tch.use.prop + scan.machines.prop +
                   inc_2015 + tch.use.prop + pct_male +
                   age_med + pct_latino + pct_black + pct_hs,
                 data = model.df)
summary(dem.vote.lm)
vif(dem.vote.lm)
AIC(dem.vote.lm)

# dem.vote.lm = lm(dem.change ~ tch.use.prop + scan.machines.prop +
#                        pct_white + ratio_nocollege_college + pop_sq_mile_2010+
#                    medianhouseholdincome_2009.2013 + demwins.h,
#                      data = model.df, weights = turnout)
# summary(dem.vote.lm)


rep.vote.lm = lm(rep.change ~ tch.use.prop + scan.machines.prop +
                   pct_white + ratio_nocollege_college + pop_sq_mile_2010+
                   medianhouseholdincome_2009.2013,
                 data = model.df , weights = turnout)
summary(rep.vote.lm)

turnout.vote.lm = lm(turnout.change ~ tch.use.prop + scan.machines.prop +
                   pct_white + ratio_nocollege_college + pop_sq_mile_2010 +
                     medianhouseholdincome_2009.2013 + dem.change,
                 data = model.df , weights = turnout)
summary(turnout.vote.lm)

other.vote.lm = lm(oth.change ~ tch.use.prop + scan.machines.prop +
                       pct_white + ratio_nocollege_college + pop_sq_mile_2010 +
                     medianhouseholdincome_2009.2013 + dem.change, weights= turnout,
                     data = model.df)
summary(other.vote.lm)
AIC(other.vote.lm)

other.vote.lm = lm(oth.change ~ tch.use.prop + scan.machines.prop +
                     pct_white + ratio_nocollege_college + pop_sq_mile_2010 +
                     medianhouseholdincome_2009.2013, weights= turnout,
                   data = model.df)
summary(other.vote.lm)
AIC(other.vote.lm)

plot(other.vote.lm)

#### Checking 'other vote' model, as scan machines prop was the most significant here
{
#Next, comparing residuals against each factor individually
layout.show(layout(matrix(c(1,2,3,4,5,6),3,2)))

A <- data.frame(rstandard(other.vote.lm),
                other.vote.lm$model$county_paper_or_paperplusmachine,
                other.vote.lm$model$scan.machines.prop,
                other.vote.lm$model$pct_white,
                other.vote.lm$model$ratio_nocollege_college,
                other.vote.lm$model$pop_sq_mile_2010,
                other.vote.lm$model$medianhouseholdincome_2009.2013
                )
colnames(A) <- c("resid",  "tch.use.prop","scan.machines.prop","pct_white","ratio_nocollege_college",
                 "pop_sq_mile_2010", "medianhouseholdincome_2009.2013" )

plot(A$resid ~ A$tch.use.prop, xlab = "tch.use.prop",
     ylab = "Residuals"); abline(0,0)

plot(A$resid ~ A$scan.machines.prop, xlab = "scan.machines.prop",
     ylab = "Residuals"); abline(0,0)

plot(A$resid ~ A$pct_white, xlab = "pct_white",
     ylab = "Residuals"); abline(0,0)

plot(A$resid ~ A$ratio_nocollege_college, xlab = "ratio_nocollege_college",
     ylab = "Residuals"); abline(0,0)

plot(A$resid ~ A$pop_sq_mile_2010, xlab = "pop_sq_mile_2010",
     ylab = "Residuals"); abline(0,0)

plot(A$resid ~ A$medianhouseholdincome_2009.2013, xlab = "medianhouseholdincome_2009.2013",
     ylab = "Residuals"); abline(0,0)

e2nona = A$resid[!is.na(A$resid)]
residlength = length(e2nona)
residsd = sd(e2nona, na.rm = TRUE)
residmean = mean(e2nona, na.rm = TRUE)
normed = (rnorm(10000, mean = residmean, sd = residsd))

#Following gives PROBABILITY DENSITIES instead of frequencies
hist(e2nona,xlab="Residuals", main="Model residuals vs rnorm 10000 (red line)",
     freq=FALSE, ylim = c(0,0.7), breaks = 15)
lines(density(normed), col="red")

shapiro.test(e2nona)
ks.test(e2nona,rnorm)
}
####

other.vote.perc.change.lm.log = lm(oth.change ~ swing.turnout.perc + log(turnout) + scan.machines.prop + dem.wins,data = county.summary.2016.final.2000ppl)
summary(other.vote.perc.change.lm.log)
AIC(other.vote.perc.change.lm.log)
plot(other.vote.perc.change.lm.log)
####
{
#Next, comparing residuals against each factor individually
layout.show(layout(matrix(c(1,2,3,4),2,2)))

A <- data.frame(rstandard(other.vote.perc.change.lm.log),
                other.vote.perc.change.lm.log$model$swing.turnout.perc,other.vote.perc.change.lm.log$model$"log(turnout)",
                other.vote.perc.change.lm.log$model$scan.machines.prop,
                other.vote.perc.change.lm.log$model$dem.wins
)

colnames(A) <- c("resid", "swing.turnout.perc","turnout","scan.machines.prop","dem.wins")
plot(A$resid ~ A$swing.turnout.perc, xlab = "swing.turnout.perc",
     ylab = "Residuals"); abline(0,0)

plot(A$resid ~ A$scan.machines.prop, xlab = "turnout",
     ylab = "Residuals"); abline(0,0)

plot(A$resid ~ A$scan.machines.prop, xlab = "scan.machines.prop",
     ylab = "Residuals"); abline(0,0)

plot(A$resid ~ A$scan.machines.prop, xlab = "dem.wins",
     ylab = "Residuals"); abline(0,0)

e2nona = A$resid[!is.na(A$resid)]
residlength = length(e2nona)
residsd = sd(e2nona, na.rm = TRUE)
residmean = mean(e2nona, na.rm = TRUE)
normed = (rnorm(10000, mean = residmean, sd = residsd))

#Following gives PROBABILITY DENSITIES instead of frequencies
hist(e2nona,xlab="Residuals", main="Model residuals vs rnorm 10000 (red line)",
     freq=FALSE, ylim = c(0,0.7), breaks = 15)
lines(density(normed), col="red")

shapiro.test(e2nona)
ks.test(e2nona,rnorm)
}
####

# Change in those voting 'other' between 2012 and 2016 by absolute turnout
{
  county.2012v2016.change.oth.turnout.point = ggplot(county.summary.2016.final.2000ppl,
                                                     aes(x = turnout, y = oth.change, colour = winner)) +
    geom_smooth(alpha = 0.5, colour = "grey", method = "lm", se = F) +
    geom_point(stat = "identity") +
    scale_y_continuous(
      name = "Change in vote for other 2012 - 2016 (%)"
    ) +
    scale_x_continuous(
      name = "Absolute turnout 2012 - 2016"
    ) +
    geom_text(x = 100000, y = 5,
              label = corr_eqn(county.summary.2016.final.2000ppl$turnout,
                               county.summary.2016.final.2000ppl$oth.change), parse = TRUE,
              show.legend = F, colour = "black") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_colour_manual(values = c("light blue", "lightcoral"))
  county.2012v2016.change.oth.turnout.point
}

# Change in those voting 'other' between 2012 and 2016 by log of turnout
{
  county.2012v2016.change.oth.turnout.log.point = ggplot(county.summary.2016.final.2000ppl,
                                                     aes(x = log(turnout), y = oth.change, colour = winner)) +
    geom_smooth(alpha = 0.5, colour = "grey", method = "lm", se = F) +
    geom_point(stat = "identity") +
    scale_y_continuous(
      name = "Change in vote for other 2012 - 2016 (%)"
    ) +
    scale_x_continuous(
      name = "Log of turnout 2012 - 2016"
    ) +
    geom_text(x = 8, y = 5,
              label = corr_eqn(log(county.summary.2016.final.2000ppl$turnout),
                               county.summary.2016.final.2000ppl$oth.change), parse = TRUE,
              show.legend = F, colour = "black") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
    scale_colour_manual(values = c("light blue", "lightcoral"))
  county.2012v2016.change.oth.turnout.log.point
}

other.turnout.2016.2012.compare.plotnames = c("county.2012v2016.change.oth.turnout.point",
                                              "county.2012v2016.change.oth.turnout.log.point")
other.turnout.2016.2012.compare = marrangeGrob(
  grobs = mget(other.turnout.2016.2012.compare.plotnames),
  nrow = 2,
  ncol = 1,
  top = NULL)
other.turnout.2016.2012.compare

# Looking at County data through time 2000-2016 -------------------------------------
# counties.2000.2016 = counties.2000.2016[order(counties.2000.2016$turnout), ]
# counties.2000.2016$ordered.turnout = c(1:length(counties.2000.2016$year))
county.turnout = subset(counties.2000.2016.2000ppl, year == 2016)
county.turnout.lookup = with(county.turnout, (data.frame(county, total.voting.age)))

counties.2000.2016.2000ppl$total.voting.age = NULL
counties.2000.2016.2000ppl = join(counties.2000.2016.2000ppl,county.turnout.lookup, by="county")
counties.2000.2016.2000ppl$turnout.perc = (counties.2000.2016.2000ppl$turnout / counties.2000.2016.2000ppl$total.voting.age)*100
counties.2000.2016.2000ppl$swing.turnout.perc = (counties.2000.2016.2000ppl$swing.turnout / counties.2000.2016.2000ppl$total.voting.age)*100


### Other votes by party
historical.oth.vote.perc.graph = ggplot(counties.2000.2016.2000ppl,
                                        aes(x = year, y = oth.vote.perc, colour = all.machines,
                                            group = all.machines)) +
  geom_jitter(alpha = 0.6, width = 0.2) +
  geom_smooth(size = 2) +
  scale_y_continuous(name = "Other vote by use of voting machines in county (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~mostly.dem)
historical.oth.vote.perc.graph

historical.oth.vote.perc.change.graph = ggplot(counties.2000.2016.2000ppl,
                                        aes(x = year, y = oth.change, colour = all.machines,
                                            group = all.machines)) +
  geom_jitter(alpha = 0.6, width = 0.2) +
  geom_smooth() +
  scale_y_continuous(name = "Change in other vote from previous election by county (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~mostly.dem)
historical.oth.vote.perc.change.graph

# Other numbers
historical.oth.vote.graph.hist = ggplot(counties.2000.2016.2000ppl[counties.2000.2016.2000ppl$year == 2016,],
                                   aes(x = oth.vote, colour = all.machines,
                                       group = all.machines)) +
  geom_histogram()
historical.oth.vote.graph.hist

historical.oth.vote.graph = ggplot(counties.2000.2016.2000ppl,
                                   aes(x = year, y = log(oth.vote), colour = all.machines,
                                       group = all.machines)) +
  geom_smooth() +
  scale_y_continuous(name = "Log of average numbers of other voters per county") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~mostly.dem)
historical.oth.vote.graph


counties.2000.2016.2000ppl$medturnout = with(counties.2000.2016.2000ppl, ifelse(turnout > median(turnout),
                                                                "High voter turnout county","Low voter turnout county"))

historical.oth.vote.perc.graph = ggplot(counties.2000.2016.2000ppl,
                                        aes(x = year, y = oth.vote.perc, colour = all.machines,
                                            group = all.machines)) +
  geom_jitter(alpha = 0.6, width = 0.2) +
  geom_smooth(size = 2) +
  scale_y_continuous(name = "Other vote by use of voting machines in county (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~medturnout)
historical.oth.vote.perc.graph

historical.oth.vote.perc.change.graph = ggplot(counties.2000.2016.2000ppl,
                                               aes(x = year, y = oth.change, colour = all.machines,
                                                   group = all.machines)) +
  geom_jitter(alpha = 0.6, width = 0.2) +
  geom_smooth() +
  scale_y_continuous(name = "Change in other vote from previous election by county (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~medturnout)
historical.oth.vote.perc.change.graph

# Other numbers
historical.oth.vote.graph.hist = ggplot(counties.2000.2016.2000ppl[counties.2000.2016.2000ppl$year == 2016,],
                                        aes(x = oth.vote, colour = all.machines,
                                            group = all.machines)) +
  geom_histogram()
historical.oth.vote.graph.hist

historical.oth.vote.graph = ggplot(counties.2000.2016.2000ppl,
                                   aes(x = year, y = log(oth.vote), colour = all.machines,
                                       group = all.machines)) +
  geom_smooth() +
  scale_y_continuous(name = "Log of average numbers of other voters per county") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~medturnout)
historical.oth.vote.graph


### Rep votes by party
historical.rep.vote.perc.graph = ggplot(counties.2000.2016.2000ppl,
                                        aes(x = year, y = rep.vote.perc, colour = all.machines,
                                            group = all.machines)) +
  geom_jitter(alpha = 0.6, width = 0.2) +
  geom_smooth(size = 2) +
  scale_y_continuous(name = "Republican vote by use of voting machines in county (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~mostly.dem)
historical.rep.vote.perc.graph

historical.rep.vote.perc.change.graph = ggplot(counties.2000.2016.2000ppl,
                                               aes(x = year, y = rep.change, colour = all.machines,
                                                   group = all.machines)) +
  geom_jitter(alpha = 0.6, width = 0.2) +
  geom_smooth() +
  scale_y_continuous(name = "Change in Republican vote from previous election by county (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~mostly.dem)
historical.rep.vote.perc.change.graph

# Other numbers
historical.rep.vote.graph.hist = ggplot(counties.2000.2016.2000ppl[counties.2000.2016.2000ppl$year == 2016,],
                                        aes(x = rep.vote, colour = all.machines,
                                            group = all.machines)) +
  geom_histogram()
historical.rep.vote.graph.hist

historical.rep.vote.graph = ggplot(counties.2000.2016.2000ppl,
                                   aes(x = year, y = rep.vote, colour = all.machines,
                                       group = all.machines)) +
  geom_smooth() +
  scale_y_continuous(name = "Average numbers of other voters per county") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~mostly.dem)
historical.rep.vote.graph





### Turnout votes
historical.turnout.perc.graph = ggplot(counties.2000.2016.2000ppl,
                                        aes(x = year, y = turnout.perc, colour = all.machines,
                                            group = all.machines)) +
  geom_jitter(alpha = 0.6, width = 0.2) +
  geom_smooth(size = 2) +
  scale_y_continuous(name = "Turnout vote by use of voting machines in county (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~mostly.dem)
historical.turnout.perc.graph

## Turnout in Republican-won counties with voting machines was
## much higher than in those without voting machines
historical.turnout.perc.graph = ggplot(counties.2000.2016.2000ppl,
                                       aes(x = year, y = turnout.perc, colour = all.machines,
                                           group = all.machines)) +
  geom_jitter(alpha = 0.6, width = 0.2) +
  geom_smooth(size = 2) +
  scale_y_continuous(name = "Turnout vote by use of voting machines in county (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~dem.2016)
historical.turnout.perc.graph

historical.turnout.perc.graph = ggplot(counties.2000.2016.2000ppl,
                                       aes(x = year, y = turnout.perc, colour = machine.most.used,
                                           group = machine.most.used)) +
  geom_jitter(alpha = 0.6, width = 0.2) +
  geom_smooth(size = 2) +
  scale_y_continuous(name = "Turnout vote by use of voting machines in county (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~dem.2016)
historical.turnout.perc.graph





historical.turnout.perc.change.graph = ggplot(counties.2000.2016.2000ppl,
                                               aes(x = year, y = turnout.change, colour = all.machines,
                                                   group = all.machines)) +
  geom_jitter(alpha = 0.6, width = 0.2) +
  geom_smooth() +
  scale_y_continuous(name = "Change in Turnout vote from previous election by county (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~mostly.dem)
historical.turnout.perc.change.graph

historical.turnout.perc.change.graph = ggplot(counties.2000.2016.2000ppl,
                                              aes(x = year, y = turnout.change, colour = mostly.dem,
                                                  group = mostly.dem)) +
  geom_jitter(alpha = 0.6, width = 0.2) +
  geom_smooth() +
  scale_y_continuous(name = "Change in Turnout vote from previous election by county (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))
historical.turnout.perc.change.graph

historical.turnout.perc.change.graph = ggplot(counties.2000.2016.2000ppl,
                                              aes(x = year, y = turnout.change, colour = dem.2016,
                                                  group = dem.2016)) +
  geom_jitter(alpha = 0.6, width = 0.2) +
  geom_smooth() +
  scale_y_continuous(name = "Change in Turnout vote from previous election by county (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))
historical.turnout.perc.change.graph


# Other numbers
historical.turnout.vote.graph.hist = ggplot(counties.2000.2016.2000ppl[counties.2000.2016.2000ppl$year == 2016,],
                                        aes(x = turnout, colour = all.machines,
                                            group = all.machines)) +
  geom_histogram()
historical.turnout.vote.graph.hist



historical.turnout.vote.graph = ggplot(counties.2000.2016.2000ppl,
                                   aes(x = year, y = turnout, colour = all.machines,
                                       group = all.machines)) +
  geom_smooth() +
  scale_y_continuous(name = "Average numbers of other voters per county") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~mostly.dem)
historical.turnout.vote.graph

# Removing Milwaukee and Dane counties ------------------------------------
corr.table.mildane = with(county.summary.2016.final.2000ppl.mildane, data.frame(dem.change, rep.change,
                                                                oth.change,swing.turnout.perc,log(total.voting.age),
                                                                use.machines.prop,dem.wins))
correlationmatrix.mildane = cor(corr.table.mildane, use = "pairwise.complete.obs")
corrplot.all = corrplot(correlationmatrix.mildane, method = "number", type = "upper")



# Other votes
historical.oth.vote.perc.graph = ggplot(counties.2000.2016.2000ppl.mildane,
                                        aes(x = year, y = oth.vote.perc, colour = all.machines,
                                            group = all.machines)) +
  geom_jitter(alpha = 0.6, width = 0.2) +
  geom_smooth(size = 2) +
  scale_y_continuous(name = "Other vote by use of voting machines in county (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral")) +
  facet_wrap(~mostly.dem)
historical.oth.vote.perc.graph

historical.oth.vote.perc.graph = ggplot(counties.2000.2016.2000ppl.mildane,
                                        aes(x = year, y = oth.change, colour = all.machines,
                                            group = all.machines)) +
  geom_jitter(alpha = 0.6, width = 0.2) +
  geom_smooth() +
  scale_y_continuous(name = "Change in other vote from previous election by county (%)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~mostly.dem)
historical.oth.vote.perc.graph

# Other numbers
historical.oth.vote.graph = ggplot(counties.2000.2016.2000ppl.mildane,
                                   aes(x = year, y = oth.vote, colour = all.machines,
                                       group = all.machines)) +
  geom_smooth() +
  scale_y_continuous(name = "Average numbers of other voters per county") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~mostly.dem)
historical.oth.vote.graph

# Other numbers
historical.oth.vote.graph = ggplot(counties.2000.2016.2000ppl.mildane,
                                   aes(x = year, y = oth.vote, colour = all.machines,
                                       group = county)) +
  geom_smooth() +
  scale_y_continuous(name = "Average numbers of other voters per county") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral")) +
  facet_wrap(~mostly.dem)
historical.oth.vote.graph



# Voting machines in counties ---------------------------------------------
# NOW, GRAPHING SWING BY PROPORTION OF ELECTRONIC VOTING MACHINES IN COUNTY
# Make sure to reorder before adding in new data!

# Now, let's order the counties by proportion of municipalities using voting machines
county.summary.2016.final = county.summary.2016.final[order(county.summary.2016.final$use.machines.prop), ]
county.summary.2016.final$use.machines.prop.order = c(1:length(county.summary.2016.final$use.machines.prop))

# Machines by county
county.vs.machines.bar = ggplot(
  county.summary.2016.final,
  aes(x = county.summary.2016.final$use.machines.prop.order, y = use.machines.prop * 100,
      fill = machine.most.used)
) +
  geom_bar(stat="identity") +
  scale_y_continuous(name = "Use of voting machines in each county (%)") +
  scale_x_discrete(
    limit =  county.summary.2016.final$use.machines.prop.order,
    labels = as.character(county.summary.2016.final$county),
    name = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5)) +
  scale_fill_discrete(name = "Primary machine vendor")
county.vs.machines.bar


# Swing percentage on x axis vs the % of municipalities that use voting machines in the county
county.swing.perc.vs.machines.point = ggplot(
  county.summary.2016.final,
  aes(x = swing.perc, y = use.machines.prop * 100,
      colour = machine.most.used)
) +
  geom_point() +
  scale_y_continuous(name = "Use of voting machines in each county as a percentage of total counties that use them") +
  scale_x_continuous(name = "Swing percentage from 2012 to 2016 election - negative is towards\n
                     Democrats, positive is towards republicans") +
  scale_colour_discrete(name = "Primary machine vendor")
county.swing.perc.vs.machines.point


correlation(county.summary.2016.final$swing.perc,
            county.summary.2016.final$use.machines.prop)
# cor -0.712513 : high correlation

correlation(county.summary.2016.final$turnout.perc,
            county.summary.2016.final$use.machines.prop)
# cor 0.3324792 : not highly correlated

# absolute.2016.2012.compare.plotnames = c("county.2012v2016.perc.diff.2016ord","county.differences.years.num")
# absolute.2016.2012.compare = marrangeGrob(grobs = mget(absolute.2016.2012.compare.plotnames), nrow=2, ncol=1,top=NULL)
# absolute.2016.2012.compare


# County Turnout comparisons -----------------------------------------------------
# 2016 vs 2012
# Ordering by turnout percentage, setup summarised data frames
{
  county.2016.vs.2012 = county.2016.vs.2012[order(county.2016.vs.2012$turnout.perc.allage.est), ]
  county.2016.vs.2012$ordered.county.2012.turnout = c(1:length(county.2016.vs.2012$turnout.perc.allage.est))

  county.2016.demrep.wins = group_by(county.2016.vs.2012, dem.2016)
  county.2016.demrep.wins.group = dplyr::summarise(county.2016.demrep.wins,
    turnout.perc.2016 = mean(turnout.perc.allage.est),
   turnout.perc.2012 = mean(turnout.2012.perc)
  )

  county.2016.machines.mostlydem = group_by(county.2016.vs.2012, all.machines,mostly.dem)
  county.2016.machines.mostlydem.group = dplyr::summarise(county.2016.machines.mostlydem,
                                                turnout.perc.2016 = mean(turnout.perc.allage.est),
                                                turnout.perc.2012 = mean(turnout.2012.perc)
  )

  county.2016.machines.mostlydemwins = group_by(county.2016.vs.2012, all.machines,dem.wins)
  county.2016.machines.mostlydemwins.group = dplyr::summarise(county.2016.machines.mostlydemwins,
                                                              turnout.perc.2016 = mean(turnout.perc.allage.est),
                                                              turnout.perc.2012 = mean(turnout.2012.perc)
  )

}
#  Turnout by counties mostly voting dem and those with mostly voting machines 2016 and 2012
{
  county.2012v2016.perc.diff.2016ord = ggplot(county.2016.machines.mostlydem.group,
                                              aes(x = mostly.dem, y = turnout.perc.2016,
                                                  fill = mostly.dem
                                                  )) +
    geom_bar(
      stat = "identity",
      aes(y = county.2016.machines.mostlydem.group$turnout.perc.2012),
      fill = "grey",
      alpha = 0.5
    ) +
    geom_bar(stat = "identity") +
    scale_y_continuous(
      name = "Percentage turnout in 2016 by mostly Dem voting counties and voting machines.\nGrey bars are 2012 results."
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
    facet_wrap(~all.machines) +
    scale_fill_manual(values = c("light blue", "lightcoral"))
  county.2012v2016.perc.diff.2016ord
}

# Percentage turnout by the number of times that Democrats have won the county
{
  county.2016.machines.mostlydemwins.group = county.2016.machines.mostlydemwins.group[
    order(as.integer(county.2016.machines.mostlydemwins.group$dem.wins)),]

county.2012v2016.turnout.demwins = ggplot(county.2016.machines.mostlydemwins.group,
                                            aes(x = all.machines, y = turnout.perc.2016,
                                                fill = factor(dem.wins)
                                            )) +
  geom_bar(stat = "identity") +
  geom_text(label=round(county.2016.machines.mostlydemwins.group$turnout.perc.2016,0),
            y = county.2016.machines.mostlydemwins.group$turnout.perc.2016/2, size = 6) +
  scale_y_continuous(
    name = "Percentage turnout in 2016 by no. times since 2000 Dems have won county and presence of voting machines.") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
        axis.title.x = element_blank()) +
  facet_wrap(~dem.wins) +
  scale_fill_brewer(type = "seq",guide = guide_legend(title = "Dem. wins since 2000"))
county.2012v2016.turnout.demwins
}

# Ordering by turnout percentage, percentage difference in votes
{
  county.2016.vs.2012 = county.2016.vs.2012[order(county.2016.vs.2012$turnout.perc.allage.est), ]
  county.2016.vs.2012$ordered.county.2016.turnout = c(1:length(county.2016.vs.2012$turnout.perc.allage.est))

  county.2012v2016.perc.diff.2016ord = ggplot(county.2016.vs.2012,
                                              aes(x = ordered.county.2016.turnout, y = perc.diff,
                                                  fill = all.machines)) +
    geom_bar(stat = "identity") +
    geom_bar(
      stat = "identity",
      aes(y = county.2016.vs.2012$turnout.perc.allage.est - mean(county.2016.vs.2012$turnout.perc.allage.est)),
      fill = "grey",
      alpha = 0.5, width = 0.4
    ) +
    annotate(
      "text",
      x = mean(county.2016.vs.2012$ordered.county.2016.turnout),
      y = min(county.2016.vs.2012$perc.diff),
      label = "More votes for Democrats",
      vjust = 1,
      hjust = 0.5
    ) +
    annotate(
      "text",
      x = mean(county.2016.vs.2012$ordered.county.2016.turnout),
      y = max(county.2016.vs.2012$perc.diff),
      label = "More votes for Republicans",
      vjust = 1,
      hjust = 0.5
    ) +
    scale_y_continuous(
      name = "Percentage vote difference in 2016 by county. Grey bars represent turnout - average county turnout,\nColours represent counties with mostly voting machines - negative values: Democrat win, positive: Republican win"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
    scale_x_discrete(
      limit =  county.2016.vs.2012$ordered.county.2016.turnout,
      labels = as.character(county.2016.vs.2012$county),
      name = NULL
    ) +
    # facet_wrap(~dem.wins)+
    # scale_fill_brewer(type = "seq")
    scale_fill_manual(values = c("light blue","lightcoral"))
  county.2012v2016.perc.diff.2016ord

}

# Comparisons 2000 to 2016
{
### Turnout percentage by machines and mostly dem
historical.turnout.perc.graph = ggplot(counties.2000.2016.2000ppl,
                                       aes(x = year, y = turnout.perc, colour = all.machines,
                                           group = all.machines)) +
  geom_jitter(alpha = 0.6, width = 0.2) +
  geom_smooth(size = 2) +
  scale_y_continuous(name = "Turnout vote by use of voting machines in county (%) by the winner of most elections since 2000") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~mostly.dem)
historical.turnout.perc.graph

## By Dem 2016 winner
historical.turnout.perc.graph = ggplot(counties.2000.2016.2000ppl,
                                       aes(x = year, y = turnout.perc, colour = all.machines,
                                           group = all.machines)) +
  geom_jitter(alpha = 0.6, width = 0.2) +
  geom_smooth(size = 2) +
  scale_y_continuous(name = "Turnout vote by use of voting machines in county (%) by the winner of the 2016 election") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
  scale_x_continuous(breaks = c(seq(2000,2016,4))) +
  scale_colour_manual(values = c("light blue","lightcoral"))  +
  facet_wrap(~dem.2016)
historical.turnout.perc.graph

## By Dem wins since 2000. Not enough data here to make it useful
# historical.turnout.perc.graph = ggplot(counties.2000.2016.2000ppl,
#                                        aes(x = year, y = turnout.perc, colour = all.machines,
#                                            group = all.machines)) +
#   geom_jitter(alpha = 0.6, width = 0.2) +
#   geom_smooth(size = 2) +
#   scale_y_continuous(name = "Turnout vote by by number of Democrat wins in county since 2000\n
#                      and use of voting machines in county (%)") +
#   theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
#   scale_x_continuous(breaks = c(seq(2000,2016,4))) +
#   scale_colour_brewer(type = "seq")  +
#   facet_wrap(~dem.wins)
# historical.turnout.perc.graph


# By machine type used
# historical.turnout.perc.graph = ggplot(counties.2000.2016.2000ppl,
#                                        aes(x = year, y = turnout.perc, colour = machine.most.used,
#                                            group = machine.most.used)) +
#   geom_jitter(alpha = 0.6, width = 0.2) +
#   geom_smooth(size = 2) +
#   scale_y_continuous(name = "Turnout vote by use of voting machines in county (%)") +
#   theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
#   scale_x_continuous(breaks = c(seq(2000,2016,4))) +
#   scale_colour_manual(values = c("light blue","lightcoral"))  +
#   facet_wrap(~dem.2016)
# historical.turnout.perc.graph


# Turnout change with time by mostly dem and voting machines
# historical.turnout.perc.change.graph = ggplot(counties.2000.2016.2000ppl,
#                                               aes(x = year, y = turnout.change, colour = all.machines,
#                                                   group = all.machines)) +
#   geom_jitter(alpha = 0.6, width = 0.2) +
#   geom_smooth() +
#   scale_y_continuous(name = "Change in Turnout vote from previous election by county (%)") +
#   theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
#   scale_x_continuous(breaks = c(seq(2000,2016,4))) +
#   scale_colour_manual(values = c("light blue","lightcoral"))  +
#   facet_wrap(~mostly.dem)
# historical.turnout.perc.change.graph


# Change in turnout by Dem 2016
# historical.turnout.perc.change.graph = ggplot(counties.2000.2016.2000ppl,
#                                               aes(x = year, y = turnout.change, colour = dem.2016,
#                                                   group = dem.2016)) +
#   geom_jitter(alpha = 0.6, width = 0.2) +
#   geom_smooth() +
#   scale_y_continuous(name = "Change in Turnout vote from previous election by county (%)") +
#   theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
#   scale_x_continuous(breaks = c(seq(2000,2016,4))) +
#   scale_colour_manual(values = c("light blue","lightcoral"))
# historical.turnout.perc.change.graph
}



# County turnout comparisons vote machines changed to voting method -----------------------------------------------------
# 2016 vs 2012
# Ordering by turnout percentage, setup summarised data frames
{
  county.2016.vs.2012$county_paper_or_paperplusmachine = as.factor(
    county.2016.vs.2012$county_paper_or_paperplusmachine

    counties.2000.2016.2000ppl$county_paper_or_paperplusmachine = as.factor(
      counties.2000.2016.2000ppl$county_paper_or_paperplusmachine
  )

  county.2016.vs.2012 = county.2016.vs.2012[order(county.2016.vs.2012$turnout.perc.allage.est), ]
  county.2016.vs.2012$ordered.county.2012.turnout = c(1:length(county.2016.vs.2012$turnout.perc.allage.est))

  county.2016.demrep.wins = group_by(county.2016.vs.2012, dem.2016)
  county.2016.demrep.wins.group = dplyr::summarise(county.2016.demrep.wins,
                                                   turnout.perc.2016 = mean(turnout.perc.allage.est),
                                                   turnout.perc.2012 = mean(turnout.2012.perc)
  )

  county.2016.machines.mostlydem = group_by(county.2016.vs.2012, county_paper_or_paperplusmachine,mostly.dem)
  county.2016.machines.mostlydem.group = dplyr::summarise(county.2016.machines.mostlydem,
                                                          turnout.perc.2016 = mean(turnout.perc.allage.est),
                                                          turnout.perc.2012 = mean(turnout.2012.perc)
  )

  county.2016.machines.mostlydemwins = group_by(county.2016.vs.2012, county_paper_or_paperplusmachine,dem.wins)
  county.2016.machines.mostlydemwins.group = dplyr::summarise(county.2016.machines.mostlydemwins,
                                                              turnout.perc.2016 = mean(turnout.perc.allage.est),
                                                              turnout.perc.2012 = mean(turnout.2012.perc)
  )

}
#  Turnout by counties mostly voting dem and those with mostly voting machines 2016 and 2012
{
  county.2012v2016.perc.diff.2016ord = ggplot(county.2016.machines.mostlydem.group,
                                              aes(x = mostly.dem, y = turnout.perc.2016,
                                                  fill = mostly.dem
                                              )) +
    geom_bar(
      stat = "identity",
      aes(y = county.2016.machines.mostlydem.group$turnout.perc.2012),
      fill = "grey",
      alpha = 0.5
    ) +
    geom_bar(stat = "identity") +
    scale_y_continuous(
      name = "Percentage turnout in 2016 by mostly Dem voting counties and voting machines.\nGrey bars are 2012 results."
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
    facet_wrap(~county_paper_or_paperplusmachine) +
    scale_fill_manual(values = c("light blue", "lightcoral"))
  county.2012v2016.perc.diff.2016ord
}

# Percentage turnout by the number of times that Democrats have won the county
{
  county.2016.machines.mostlydemwins.group = county.2016.machines.mostlydemwins.group[
    order(as.integer(county.2016.machines.mostlydemwins.group$dem.wins)),]

  county.2012v2016.turnout.demwins = ggplot(county.2016.machines.mostlydemwins.group,
                                            aes(x = county_paper_or_paperplusmachine, y = turnout.perc.2016,
                                                fill = factor(dem.wins)
                                            )) +
    geom_bar(stat = "identity") +
    geom_text(label=round(county.2016.machines.mostlydemwins.group$turnout.perc.2016,0),
              y = county.2016.machines.mostlydemwins.group$turnout.perc.2016/2, size = 6) +
    scale_y_continuous(
      name = "Percentage turnout in 2016 by no. times since 2000 Dems have won county and presence of voting machines.") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
          axis.title.x = element_blank()) +
    facet_wrap(~dem.wins) +
    scale_fill_brewer(type = "seq",guide = guide_legend(title = "Dem. wins since 2000"))
  county.2012v2016.turnout.demwins
}

# Ordering by turnout percentage, percentage difference in votes
{
  county.2016.vs.2012 = county.2016.vs.2012[order(county.2016.vs.2012$turnout.perc.allage.est), ]
  county.2016.vs.2012$ordered.county.2016.turnout = c(1:length(county.2016.vs.2012$turnout.perc.allage.est))

  county.2012v2016.perc.diff.2016ord = ggplot(county.2016.vs.2012,
                                              aes(x = ordered.county.2016.turnout, y = perc.diff,
                                                  fill = county_paper_or_paperplusmachine)) +
    geom_bar(stat = "identity") +
    geom_bar(
      stat = "identity",
      aes(y = county.2016.vs.2012$turnout.perc.allage.est - mean(county.2016.vs.2012$turnout.perc.allage.est)),
      fill = "grey",
      alpha = 0.5, width = 0.4
    ) +
    annotate(
      "text",
      x = mean(county.2016.vs.2012$ordered.county.2016.turnout),
      y = min(county.2016.vs.2012$perc.diff),
      label = "More votes for Democrats",
      vjust = 1,
      hjust = 0.5
    ) +
    annotate(
      "text",
      x = mean(county.2016.vs.2012$ordered.county.2016.turnout),
      y = max(county.2016.vs.2012$perc.diff),
      label = "More votes for Republicans",
      vjust = 1,
      hjust = 0.5
    ) +
    scale_y_continuous(
      name = "Percentage vote difference in 2016 by county. Grey bars represent turnout - average county turnout,\nColours represent counties with mostly voting machines - negative values: Democrat win, positive: Republican win"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank()) +
    scale_x_discrete(
      limit =  county.2016.vs.2012$ordered.county.2016.turnout,
      labels = as.character(county.2016.vs.2012$county),
      name = NULL
    ) +
    # facet_wrap(~dem.wins)+
    # scale_fill_brewer(type = "seq")
    scale_fill_manual(values = c("light blue","lightcoral"))
  county.2012v2016.perc.diff.2016ord

}

# Comparisons 2000 to 2016
{
  ### Turnout percentage by machines and mostly dem
  historical.turnout.perc.graph = ggplot(counties.2000.2016.2000ppl,
                                         aes(x = year, y = turnout.perc, colour = county_paper_or_paperplusmachine,
                                             group = county_paper_or_paperplusmachine)) +
    geom_jitter(alpha = 0.6, width = 0.2) +
    geom_smooth(size = 2) +
    scale_y_continuous(name = "Turnout vote by use of voting machines in county (%) by the winner of most elections since 2000") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
    scale_x_continuous(breaks = c(seq(2000,2016,4))) +
    scale_colour_manual(values = c("light blue","lightcoral"))  +
    facet_wrap(~mostly.dem)
  historical.turnout.perc.graph

  ## By Dem 2016 winner
  historical.turnout.perc.graph = ggplot(counties.2000.2016.2000ppl,
                                         aes(x = year, y = turnout.perc, colour = county_paper_or_paperplusmachine,
                                             group = county_paper_or_paperplusmachine)) +
    geom_jitter(alpha = 0.6, width = 0.2) +
    geom_smooth(size = 2) +
    scale_y_continuous(name = "Turnout vote by use of voting machines in county (%) by the winner of the 2016 election") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
    scale_x_continuous(breaks = c(seq(2000,2016,4))) +
    scale_colour_manual(values = c("light blue","lightcoral"))  +
    facet_wrap(~dem.2016)
  historical.turnout.perc.graph

  ## By Dem wins since 2000. Not enough data here to make it useful
  historical.turnout.perc.graph = ggplot(counties.2000.2016.2000ppl,
                                         aes(x = year, y = turnout.perc, colour = county_paper_or_paperplusmachine,
                                             group = county_paper_or_paperplusmachine)) +
    geom_jitter(alpha = 0.6, width = 0.2) +
    geom_smooth(size = 2) +
    scale_y_continuous(name = "Turnout vote by by number of Democrat wins in county since 2000\n
                       and use of voting machines in county (%)") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
    scale_x_continuous(breaks = c(seq(2000,2016,4))) +
    scale_colour_brewer(type = "seq")  +
    facet_wrap(~dem.wins)
  historical.turnout.perc.graph


  # Turnout change with time by mostly dem and voting machines
  historical.turnout.perc.change.graph = ggplot(counties.2000.2016.2000ppl,
                                                aes(x = year, y = turnout.change, colour = county_paper_or_paperplusmachine,
                                                    group = county_paper_or_paperplusmachine)) +
    geom_jitter(alpha = 0.6, width = 0.2) +
    geom_smooth() +
    scale_y_continuous(name = "Change in Turnout vote from previous election by county (%)") +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.title = element_blank()) +
    scale_x_continuous(breaks = c(seq(2000,2016,4))) +
    scale_colour_manual(values = c("light blue","lightcoral"))  +
    facet_wrap(~mostly.dem)
  historical.turnout.perc.change.graph

}
)

