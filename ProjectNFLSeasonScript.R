# GOod luck future, Sean. Hopefully the Vikings have won a super bowl the next time you read this.



#### Get Player History
getqb <- function(year) {
  url <- str_c("https://www.pro-football-reference.com/years/", year,"/passing.htm")
  h <- read_html(url) 
  pass <- html_nodes(h, ".left , .right , .center") %>% html_text
  
  num <- round(length(pass) /32)
  sum = 0
  emptydf <- matrix(0, ncol=32, nrow = num)
  
  for (i in c(1:num)){  
    for (j in c(1:32)) {
      emptydf[i, j] <- pass[32 * (i -1) + j]
    }
  }
  x <- as.data.frame(emptydf)
  idk <- x %>% filter(V8 != "" & V8 != "QBrec") %>% select(-1,-8, -30, -31)
  colnames(idk) <- c("Name", "Team", "Age", "Pos", "GS", "GP", "Com", "Att", "CompPer", "Yds", "TD", "TDPer", "Int", "IntPer", "FirstDown", "Long", "YPA", "AYPA", "YPC", "YPG", "Rating", "QBR", "Sack", "SackYds", "SackPer", "NYPA", "NAYPA","GWD")
  idk <- idk %>% mutate(Pos = "QB")
  for (i in c(5:27)){
    idk[,i]<- as.numeric(idk[,i])
  }
  idk <- idk %>% mutate(yearID = year)
  dfqb <- idk %>% filter(Att > 125) 
  dfqb1 <- dfqb %>%
    mutate(Use = Att / GS) %>%
    mutate(Durability = Att) %>%
    select(Name, Team, yearID, YPC, CompPer, IntPer, TDPer, Durability, Use, Sack, SackPer) %>%
    mutate(TDTO = TDPer - 2*IntPer) %>%
    mutate(passer = YPC * CompPer) %>%
    group_by(yearID) %>%
    mutate(TDTO = (TDTO - mean(TDTO)) / sd(TDTO))%>%
    mutate(passer = (passer - mean(passer)) / sd(passer)) %>%
    ungroup() %>%
    mutate(FloStrength = TDTO + passer) %>%
    group_by(yearID) %>%
    mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
    ungroup() %>%
    mutate(Value = FloStrength * Durability) %>%
    group_by(yearID) %>%
    mutate(Value = (Value - mean(Value)) / sd(Value))
  return(dfqb1)
}

getrb <- function(year) {
  url <- str_c("https://www.pro-football-reference.com/years/", year,"/rushing.htm")
  h <- read_html(url) 
  rush <- html_nodes(h, "td , .right") %>% html_text
  
  num <- round(length(rush) /16)
  sum = 0
  emptydf <- matrix(0, ncol=16, nrow = num)
  
  for (i in c(1:num)){  
    for (j in c(1:16)) {
      emptydf[i, j] <- rush[16 * (i -1) + j]
    }
  }
  x <- as.data.frame(emptydf)
  idk <- x %>% select(-1)
  colnames(idk) <- c("Name", "Team", "Age", "Pos", "GP", "GS", "Att", "Yds", "TD", "FirstDown","filt", "Long", "YPA", "YPG", "Fmb")
  idk<-idk%>%select(-filt)
  for (i in c(5:14)){
    idk[,i]<- as.numeric(idk[,i])
  }
  idk <- idk %>% mutate(yearID = year)
  dfrb <- idk %>% filter(Att > 72) 
  dfrb1 <- dfrb %>%
    mutate(Use = Att / GP) %>%
    mutate(Durability = Att) %>%
    mutate(TDPerc = TD / Att) %>%
    mutate(FumPerc = Fmb / Att) %>%
    mutate(TDTO = TDPerc - 3 * FumPerc) %>%
    group_by(yearID) %>%
    mutate(TDTO = (TDTO - mean(TDTO)) / sd(TDTO))%>%
    mutate(rusher = (YPA - mean(YPA)) / sd(YPA)) %>%
    ungroup() %>%
    mutate(FloStrength = TDTO + 3 * rusher) %>%
    group_by(yearID) %>%
    mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
    ungroup() %>%
    mutate(Value = FloStrength * Durability) %>%
    group_by(yearID) %>%
    mutate(Value = (Value - mean(Value)) / sd(Value))
  return(dfrb1)
}


getwr <- function(year) {
  url <- str_c("https://www.pro-football-reference.com/years/", year,"/receiving.htm")
  h <- read_html(url) 
  rec <- html_nodes(h, ".right , td") %>% html_text
  
  num <- round(length(rec) /20)
  sum = 0
  emptydf <- matrix(0, ncol=20, nrow = num)
  
  for (i in c(1:num)){  
    for (j in c(1:20)) {
      emptydf[i, j] <- rec[20 * (i -1) + j]
    }
  }
  x <- as.data.frame(emptydf)
  idk <- x %>% select(-1)
  colnames(idk) <- c("Name", "Team", "Age", "Pos", "GP", "GS", "Targets","Rec","CatchPer", "Yds", "YPR","TD", "FirstDown","filt", "Long", "YPT", "RPG","YPG", "Fmb")
  idk<-idk%>%select(-filt)
  for (i in c(5:8,10:18)){
    idk[,i]<- as.numeric(idk[,i])
  }
  idk <- idk %>% mutate(yearID = year)
  dfwr <- idk %>% filter(Targets > 31) 
  dfwr1 <- dfwr %>%
    mutate(CatchPer= Rec / Targets) %>%
    mutate(Durability = Rec) %>%
    mutate(TDPerc = TD / Rec) %>%
    mutate(FumPerc = Fmb / Rec) %>%
    mutate(TDTO = TDPerc - 3 * FumPerc) %>%
    group_by(yearID) %>%
    mutate(TDTO = (TDTO - mean(TDTO)) / sd(TDTO))%>%
    mutate(reciever = (YPR - mean(YPR)) / sd(YPR)) %>%
    mutate(hands = (Rec - mean(Rec)) / sd(Rec)) %>%
    ungroup() %>%
    mutate(FloStrength = TDTO + 3*reciever + 2 * hands) %>%
    group_by(yearID) %>%
    mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
    ungroup() %>%
    mutate(Value = (-1*min(FloStrength)+FloStrength) * Durability) %>%
    group_by(yearID) %>%
    mutate(Value = (Value - mean(Value)) / sd(Value))
  return(dfwr1)
}
getdef <- function(year) {
  url <- str_c("https://www.pro-football-reference.com/years/", year,"/defense.htm")
  h <- read_html(url) 
  def <- html_nodes(h, ".right , td") %>% html_text
  
  num <- round(length(def) /24)
  sum = 0
  emptydf <- matrix(0, ncol=24, nrow = num)
  
  for (i in c(1:num)){  
    for (j in c(1:24)) {
      emptydf[i, j] <- def[24 * (i -1) + j]
    }
  }
  x <- as.data.frame(emptydf)
  idk <- x %>% select(-1)
  colnames(idk) <- c("Name", "Team", "Age", "Pos", "GP", "GS", "Int", "IntYds", "IntTD", "IntTDLong", "PD", "FF", "Fum", "FR", "FumYds", "FumTD", "Sack", "Tck", "TckSolo", "TckAsst", "TFL", "QBHit", "Sfty")
  for (i in c(5:23)){
    idk[,i]<- as.numeric(idk[,i])
  }
  for (i in c(7:23)){
    ind <- which(is.na(idk[,i]))
    idk[,i]<- as.numeric(idk[,i])
    idk[ind,i]<- 0
  }
  idk <- idk %>% mutate(yearID = year)
  dfdef <- idk %>% filter(GP > 4 & Tck > 15) 
  dfdef1 <- dfdef %>%
    mutate(Tackling= (TckSolo + .5 * TckAsst)/ GP) %>%
    mutate(Durability = GP) %>%
    mutate(Rushing = (Sack + .4 * QBHit + .2 * TFL + 4 * FF)/ GP) %>%
    mutate(Coverage = (Int + .2 * PD) / GP) %>%
    group_by(yearID) %>%
    mutate(Tackling = (Tackling - mean(Tackling)) / sd(Tackling))%>%
    mutate(Rushing = (Rushing - mean(Rushing)) / sd(Rushing)) %>%
    mutate(Coverage = (Coverage - mean(Coverage)) / sd(Coverage)) %>%
    ungroup() %>%
    mutate(FloStrength = Tackling + Rushing + Coverage) %>%
    group_by(yearID) %>%
    mutate(FloStrength = (FloStrength - mean(FloStrength)) / sd(FloStrength)) %>%
    ungroup() %>%
    mutate(Value = (-1*min(FloStrength)+FloStrength) * Durability) %>%
    group_by(yearID) %>%
    mutate(Value = (Value - mean(Value)) / sd(Value))
  return(dfdef1)
}
getdraft<- function(year) {
  url <- str_c("https://www.pro-football-reference.com/years/", year,"/draft.htm")
  h <- read_html(url) 
  draft <- html_nodes(h, "th+ .right , td.left , th.right") %>% html_text
  
  num <- round(length(draft) /6)
  sum = 0
  emptydf <- matrix(0, ncol=6, nrow = num)
  
  for (i in c(1:num)){  
    for (j in c(1:6)) {
      emptydf[i, j] <- draft[6 * (i -1) + j]
    }
  }
  x <- as.data.frame(emptydf)
  idk <- x %>% select(-6)
  colnames(idk) <- c("Round", "Pick", "Team", "Name", "Pos")
  for (i in c(1,2)){
    idk[,i]<- as.numeric(idk[,i])
  }
  idk <- idk %>%
    select(-Round) %>%
    mutate(Name = str_remove(Name, " HOF")) %>%
    mutate(yeardraft = year)
}
dfdraft <- map_df(.x= 2006:2022, .f=getdraft) 
dfqb <- map_df(.x= 2006:2022, .f=getqb)  
ind <- which(dfqb$Name == "Matt Cassel" & dfqb$yearID == 2022)
dfqb$Team[ind] <- "DAL"
dfrb <- map_df(.x= 2006:2022, .f=getrb)  

dfwr <- map_df(.x= 2006:2022, .f=getwr)  

dfdef <- map_df(.x= 2006:2022, .f=getdef)  
write_csv(dfdef, "/Users/seanfloersch/FloStrength/NFLFloStrength/NFLPlayerPred/DefDF")
write_csv(dfqb, "/Users/seanfloersch/FloStrength/NFLFloStrength/NFLPlayerPred/QBDF")
write_csv(dfrb, "/Users/seanfloersch/FloStrength/NFLFloStrength/NFLPlayerPred/RBDF")
write_csv(dfwr, "/Users/seanfloersch/FloStrength/NFLFloStrength/NFLPlayerPred/WRDF")
write_csv(dfdraft, "/Users/seanfloersch/FloStrength/NFLFloStrength/NFLPlayerPred/draftdf")





# Prediction playerdf
PredPlayers <- function(pos){
  
  innerpredplayerknown <- function(year){
    filename <- str_c("~/FloStrength/NFLFloStrength/NFLPlayerPred/", pos, "DF")
    if (pos == "OL"){
      filename <- str_c("~/FloStrength/NFLFloStrength/NFLPlayerPred/QB", "DF")
    }
    df <- read.csv(filename, header=TRUE) %>%
      mutate(Name = str_remove(Name, "\\*")) %>%
      mutate(Name = str_remove(Name, "\\+"))
    a <- df %>%
      filter(yearID == year)
    b <- df %>%
      filter(yearID == (year-1))
    c <- df %>%
      filter(yearID == (year-2))
    idk <- left_join(a, b, by = "Name")
    idk <- left_join(idk, c, by = "Name")
    draft <- read.csv("~/FloStrength/NFLFloStrength/NFLPlayerPred/draftdf", header=TRUE) %>%
      select(-Team, -Pos)
    
    if (pos == "RB"){
      idk <- idk %>%
        mutate(Pos = "RB") %>%
        select(Name, "Team" = "Team.x", "Age" = "Age.x", Pos,"Year"= "yearID.x", GP.x, GP.y, GP, Att.x, Att.y,Att,FloStrength.x, FloStrength.y, FloStrength, Value.x, Value.y, Value) %>%
        na.omit %>%
        mutate(FSChange21 = FloStrength.y-FloStrength)%>%
        mutate(FSChange10 = FloStrength.x-FloStrength.y)
    }
    if (pos == "QB"){
      idk <- idk %>%
        mutate(Pos = "QB") %>%
        select(Name, "Team" = "Team.x",Pos,"Year"= "yearID.x", Durability.x, Durability.y, Durability,FloStrength.x, FloStrength.y, FloStrength, Value.x, Value.y, Value) %>%
        na.omit %>%
        mutate(FSChange21 = FloStrength.y-FloStrength)%>%
        mutate(FSChange10 = FloStrength.x-FloStrength.y)
    }
    if (pos == "WR"){
      idk <- idk %>%
        mutate(Pos = "WR") %>%
        select(Name, "Team" = "Team.x", "Age" = "Age.x", Pos,"Year"= "yearID.x", GP.x, GP.y, GP, FloStrength.x, FloStrength.y, FloStrength, Value.x, Value.y, Value) %>%
        na.omit %>%
        mutate(FSChange21 = FloStrength.y-FloStrength)%>%
        mutate(FSChange10 = FloStrength.x-FloStrength.y)
    }
    if (pos == "DEF"){
      idk <- idk %>%
        mutate(Pos = "DEF") %>%
        select(Name, "Team" = "Team.x", "Age" = "Age.x", Pos,"Year"= "yearID.x", GP.x, GP.y, GP, FloStrength.x, FloStrength.y, FloStrength, Value.x, Value.y, Value) %>%
        na.omit %>%
        mutate(FSChange21 = FloStrength.y-FloStrength)%>%
        mutate(FSChange10 = FloStrength.x-FloStrength.y)
    }
    return(idk)
  }
  innerpredplayeruk <- function(year){
    filename <- str_c("~/FloStrength/NFLFloStrength/NFLPlayerPred/", pos, "DF")
    if (pos == "OL"){
      filename <- str_c("~/FloStrength/NFLFloStrength/NFLPlayerPred/QB", "DF")
    }
    df <- read.csv(filename, header=TRUE) %>%
      mutate(Name = str_remove(Name, "\\*")) %>%
      mutate(Name = str_remove(Name, "\\+"))
    a <- df %>%
      filter(yearID == year)
    b <- df %>%
      filter(yearID == (year-1))
    c <- df %>%
      filter(yearID == (year-2))
    idk <- left_join(a, b, by = "Name")
    idk <- left_join(idk, c, by = "Name")
    draft <- read.csv("~/FloStrength/NFLFloStrength/NFLPlayerPred/draftdf", header=TRUE) %>%
      select(-Team, -Pos)
    
    if (pos == "RB"){
      second <- idk %>%
        mutate(Pos = "RB") %>%
        select(Name, "Team" = "Team.x", "Age" = "Age.x", Pos,"Year"= "yearID.x", GP.x, GP.y, GP, Att.x, Att.y,Att,FloStrength.x, FloStrength.y, FloStrength, Value.x, Value.y, Value) %>%
        filter(is.na(GP.y) == TRUE & is.na(GP) == TRUE) 
    }
    if (pos == "QB"){
      second <- idk %>%
        mutate(Pos = "QB") %>%
        select(Name, "Team" = "Team.x","Year"= "yearID.x", Pos, Durability.x, Durability.y, Durability,FloStrength.x, FloStrength.y, FloStrength, Value.x, Value.y, Value) %>%
        filter(is.na(Durability.y) == TRUE & is.na(Durability) == TRUE) 
    }
    if (pos == "WR"){
      second <- idk %>%
        mutate(Pos = "WR") %>%
        select(Name, "Team" = "Team.x",Pos, "Age" = "Age.x", Pos,"Year"= "yearID.x", GP.x, GP.y, GP, FloStrength.x, FloStrength.y, FloStrength, Value.x, Value.y, Value) %>%
        filter(is.na(GP.y) == TRUE & is.na(GP) == TRUE) 
    }
    if (pos == "DEF"){
      second <- idk %>%
        mutate(Pos = "DEF") %>%
        select(Name, "Team" = "Team.x",Pos, "Age" = "Age.x", Pos,"Year"= "yearID.x", GP.x, GP.y, GP, FloStrength.x, FloStrength.y, FloStrength, Value.x, Value.y, Value) %>%
        filter(is.na(GP.y) == TRUE & is.na(GP) == TRUE) 
    }
    return(second)
  }
  innerpredplayermissedlast <- function(year){
    filename <- str_c("~/FloStrength/NFLFloStrength/NFLPlayerPred/", pos, "DF")
    if (pos == "OL"){
      filename <- str_c("~/FloStrength/NFLFloStrength/NFLPlayerPred/QB", "DF")
    }
    df <- read.csv(filename, header=TRUE) %>%
      mutate(Name = str_remove(Name, "\\*")) %>%
      mutate(Name = str_remove(Name, "\\+"))
    a <- df %>%
      filter(yearID == year)
    b <- df %>%
      filter(yearID == (year-1))
    c <- df %>%
      filter(yearID == (year-2))
    idk <- left_join(a, b, by = "Name")
    idk <- left_join(idk, c, by = "Name")
    draft <- read.csv("~/FloStrength/NFLFloStrength/NFLPlayerPred/draftdf", header=TRUE) %>%
      select(-Team, -Pos)
    
    if (pos == "RB"){
      third <- idk %>%
        mutate(Pos = "RB") %>%
        select(Name, "Team" = "Team.x", "Age" = "Age.x", Pos,"Year"= "yearID.x", GP.x, GP.y, GP, Att.x, Att.y,Att,FloStrength.x, FloStrength.y, FloStrength, Value.x, Value.y, Value) %>%
        filter(is.na(GP.y) == TRUE & is.na(GP) == FALSE) 
    }
    if (pos == "QB"){
      third <- idk %>%
        mutate(Pos = "QB") %>%
        select(Name, "Team" = "Team.x",Pos,"Year"= "yearID.x", Durability.x, Durability.y, Durability,FloStrength.x, FloStrength.y, FloStrength, Value.x, Value.y, Value) %>%
        filter(is.na(Durability.y) == TRUE & is.na(Durability) == FALSE) 
    }
    if (pos == "WR"){
      third <- idk %>%
        mutate(Pos = "WR") %>%
        select(Name, "Team" = "Team.x",Pos, "Age" = "Age.x", Pos,"Year"= "yearID.x", GP.x, GP.y, GP, FloStrength.x, FloStrength.y, FloStrength, Value.x, Value.y, Value) %>%
        filter(is.na(GP.y) == TRUE & is.na(GP) == FALSE) 
    }
    if (pos == "DEF"){
      third <- idk %>%
        mutate(Pos = "DEF") %>%
        select(Name, "Team" = "Team.x",Pos, "Age" = "Age.x", Pos,"Year"= "yearID.x", GP.x, GP.y, GP, FloStrength.x, FloStrength.y, FloStrength, Value.x, Value.y, Value) %>%
        filter(is.na(GP.y) == TRUE & is.na(GP) == FALSE) 
    }
    return(third)
  }
  innerpredplayersecondyear<- function(year){
    filename <- str_c("~/FloStrength/NFLFloStrength/NFLPlayerPred/", pos, "DF")
    if (pos == "OL"){
      filename <- str_c("~/FloStrength/NFLFloStrength/NFLPlayerPred/QB", "DF")
    }
    df <- read.csv(filename, header=TRUE) %>%
      mutate(Name = str_remove(Name, "\\*")) %>%
      mutate(Name = str_remove(Name, "\\+"))
    a <- df %>%
      filter(yearID == year)
    b <- df %>%
      filter(yearID == (year-1))
    c <- df %>%
      filter(yearID == (year-2))
    idk <- left_join(a, b, by = "Name")
    idk <- left_join(idk, c, by = "Name")
    draft <- read.csv("~/FloStrength/NFLFloStrength/NFLPlayerPred/draftdf", header=TRUE) %>%
      select(-Team, -Pos)
    
    if (pos == "RB"){
      fourth <- idk %>%
        mutate(Pos = "RB") %>%
        select(Name, "Team" = "Team.x", "Age" = "Age.x", Pos,"Year"= "yearID.x", GP.x, GP.y, GP, Att.x, Att.y,Att,FloStrength.x, FloStrength.y, FloStrength, Value.x, Value.y, Value) %>%
        filter(is.na(GP.y) == FALSE & is.na(GP) == TRUE) 
    }
    if (pos == "QB"){
      fourth <- idk %>%
        mutate(Pos = "QB") %>%
        select(Name, "Team" = "Team.x",Pos,"Year"= "yearID.x", Durability.x, Durability.y, Durability,FloStrength.x, FloStrength.y, FloStrength, Value.x, Value.y, Value) %>%
        filter(is.na(Durability.y) == FALSE & is.na(Durability) == TRUE) 
    }
    if (pos == "WR"){
      fourth <- idk %>%
        mutate(Pos = "WR") %>%
        select(Name, "Team" = "Team.x",Pos, "Age" = "Age.x","Year"= "yearID.x", GP.x, GP.y, GP, FloStrength.x, FloStrength.y, FloStrength, Value.x, Value.y, Value) %>%
        filter(is.na(GP.y) == FALSE & is.na(GP) == TRUE) 
    }
    if (pos == "DEF"){
      fourth <- idk %>%
        mutate(Pos = "DEF") %>%
        select(Name, "Team" = "Team.x",Pos, "Age" = "Age.x", "Pos","Year"= "yearID.x", GP.x, GP.y, GP, FloStrength.x, FloStrength.y, FloStrength, Value.x, Value.y, Value) %>%
        filter(is.na(GP.y) == FALSE & is.na(GP) == TRUE)
    }
    return(fourth)
  }
  known <-  map_df(.x= 2008:2022, .f=innerpredplayerknown)  
  unknown <-  map_df(.x= 2008:2022, .f=innerpredplayeruk)  %>%
    mutate(FSPred = mean(FloStrength.x)) 
  missedlast <-map_df(.x= 2008:2022, .f=innerpredplayermissedlast)%>%
    mutate(FSPred = mean(FloStrength.x))
  secondyear <-map_df(.x= 2008:2022, .f=innerpredplayersecondyear)
  
  if (pos == "RB"){
    unknown <- unknown %>%
      mutate(Att.y = mean(Att.x))
    missedlast <- missedlast %>%
      mutate(Att.y = mean(Att.x))
  }
  if (pos == "QB"){
    unknown <- unknown %>%
      mutate(Durability.y = mean(Durability.x))
    missedlast <- missedlast %>%
      mutate(Durability.y = mean(Durability.x))
  }
  if (pos == "WR"){
    unknown <- unknown %>%
      mutate(GP.y = mean(GP.x))
    missedlast <- missedlast %>%
      mutate(GP.y = mean(GP.x))
  }
  if (pos == "DEF"){
    unknown <- unknown %>%
      mutate(GP.y = mean(GP.x))
    missedlast <- missedlast %>%
      mutate(GP.y = mean(GP.x))
  }
  linmod <- lm(FSChange10~FSChange21, data = known)
  known<-known %>%
    mutate(FSPred = FloStrength.y+ linmod$coefficients[1]+linmod$coefficients[2]*FSChange21) %>%
    select(-FSChange10, -FSChange21)
  linmod <- lm(FloStrength.x~FloStrength.y, data = secondyear)
  secondyear<-secondyear %>%
    mutate(FSPred = linmod$coefficients[1]+linmod$coefficients[2]*FloStrength.y)
  linmod <- lm(FloStrength.x~FloStrength, data = missedlast)
  missedlast<-missedlast %>%
    mutate(FSPred = linmod$coefficients[1]+linmod$coefficients[2]*FloStrength)
  unknown<-unknown %>%
    mutate(FSPred = mean(FloStrength.x))
  
  lessgo <- rbind(known, unknown) 
  lessgo <- rbind(lessgo, missedlast) 
  lessgo <- rbind(lessgo, secondyear) 
  return(lessgo)
}
positions <- c("DEF", "QB", "RB", "WR")
lessgo <-  map_df(.x= positions, .f=PredPlayers)
write_csv(lessgo, "/Users/seanfloersch/FloStrength/NFLFloStrength/NFLPlayerPred/nflplayerhistory")

#this season now!
get_current_rosters <- function(team) {
  url <- str_c("https://www.espn.com/nfl/team/roster/_/name/", team)
  h <- read_html(url) 
  Name <- html_nodes(h, ".Table__TD+ .Table__TD .AnchorLink") %>% html_text%>% str_trim()
  pos <- html_nodes(h, ".Table__TD:nth-child(3) .inline") %>% html_text
  roster <- data.frame(Name, pos) %>%
    mutate(Team = team)
  return(roster)
}
get_depth_chart <- function(team) {
  url <- str_c("https://www.espn.com/nfl/team/depth/_/name/", team)
  h <- read_html(url) 
  Name <- html_nodes(h, ".fw-medium .AnchorLink") %>% html_text
  pos <- html_nodes(h, ".Table--fixed-left .Table__TD") %>% html_text %>% str_trim()
  avl <- html_nodes(h, ".fw-medium .n8") %>% html_text
  roster <- data.frame(Name, pos,avl) %>%
    filter(avl != "O") %>%
    mutate(Team = team) %>%
    group_by(pos) %>%
    mutate(depth = c(1:length(Name))) %>%
    ungroup %>%
    filter(pos != "KR") %>%
    filter(pos != "PR") %>%
    filter(pos != "H") %>%
    filter(pos != "PK") %>%
    filter(pos != "FB") %>%
    select(-pos) 
  
  return(roster)
}
dfqb <- read.csv("/Users/seanfloersch/FloStrength/NFLFloStrength/NFLPlayerPred/QBDF")
dfrb <- read.csv("/Users/seanfloersch/FloStrength/NFLFloStrength/NFLPlayerPred/RBDF")
dfwr <- read.csv("/Users/seanfloersch/FloStrength/NFLFloStrength/NFLPlayerPred/WRDF")
dfdef <- read.csv("/Users/seanfloersch/FloStrength/NFLFloStrength/NFLPlayerPred/DefDF")

teams <- c("hou","cle","nyj", "mia", "buf", "ne", "ind", "jax", "ten", "pit", "bal", "cin", "den", "lv", "sea", "kc", "lac", "dal", "ari", "nyg","wsh", "phi", "min", "gb", "tb", "chi", "det", "atl", "sf", "no", "car", "lar")
roster1 <-  map_df(.x= teams, .f=get_current_rosters) 
depth <-  map_df(.x= teams, .f=get_depth_chart) 

roster <- left_join(depth, roster1, by = c("Name", "Team")) 
ind <- which(roster$Name == "Zach Wilson")
roster$pos[ind] <- "QB"
roster <- roster %>%
  filter(pos != "FB")%>%
  filter(pos != "C")%>%
  filter(pos != "G")%>%
  filter(pos != "OT")%>%
  filter(pos != "LS")%>%
  filter(pos != "P")%>%
  filter(pos != "PK")%>%
  filter(pos != "OL")
ind <- which(roster$pos == "TE")
roster$pos[ind] <- "WR"
roster$depth[ind] <- 4
ind <- which(roster$pos == "CB" |roster$pos == "S")
roster$pos[ind] <- "DEF"
ind <- which(roster$pos == "DE" |roster$pos == "DT")
roster$pos[ind] <- "DEF"
ind <- which(roster$pos == "DB" |roster$pos == "NT")
roster$pos[ind] <- "DEF"
ind <- which(roster$pos == "DL" |roster$pos == "LB")
roster$pos[ind] <- "DEF"
roster <- roster %>%
  mutate(depth = str_trim(str_c(pos, depth, by = ""))) %>%
  mutate(Name = str_trim(Name))

## UPdate years

qb21 <- dfqb %>% 
  filter(yearID == 2022) %>%
  mutate(Name = str_remove(Name, "\\*"))%>%
  mutate(Name = str_remove(Name, "\\+")) %>%
  select(Name, yearID, Team, FloStrength) %>%
  mutate(pos = "QB")
rb21 <- dfrb %>% 
  filter(yearID == 2022) %>%
  mutate(Name = str_remove(Name, "\\*"))%>%
  mutate(Name = str_remove(Name, "\\+")) %>%
  select(Name, yearID, Team, FloStrength) %>%
  mutate(pos = "RB")
wr21 <- dfwr %>% 
  filter(yearID == 2022) %>%
  mutate(Name = str_remove(Name, "\\*"))%>%
  mutate(Name = str_remove(Name, "\\+")) %>%
  select(Name, yearID, Team, FloStrength) %>%
  mutate(pos = "WR")
def21 <- dfdef %>% 
  filter(yearID == 2022) %>%
  mutate(Name = str_remove(Name, "\\*"))%>%
  mutate(Name = str_remove(Name, "\\+")) %>%
  select(Name, yearID, Team, FloStrength) %>%
  mutate(pos = "DEF")
players21 <- rbind(qb21, rb21)
players21 <- rbind(players21, wr21)
players21 <- rbind(players21, def21)%>%
  mutate(Name = str_trim(Name))
rosterfs1 <- left_join(roster, players21, by = c("Name", "pos"))

qb21 <- dfqb %>% 
  filter(yearID == 2021) %>%
  mutate(Name = str_remove(Name, "\\*"))%>%
  mutate(Name = str_remove(Name, "\\+")) %>%
  select(Name, yearID, Team, FloStrength) %>%
  mutate(pos = "QB")
rb21 <- dfrb %>% 
  filter(yearID == 2021) %>%
  mutate(Name = str_remove(Name, "\\*"))%>%
  mutate(Name = str_remove(Name, "\\+")) %>%
  select(Name, yearID, Team, FloStrength) %>%
  mutate(pos = "RB")
wr21 <- dfwr %>% 
  filter(yearID == 2021) %>%
  mutate(Name = str_remove(Name, "\\*"))%>%
  mutate(Name = str_remove(Name, "\\+")) %>%
  select(Name, yearID, Team, FloStrength) %>%
  mutate(pos = "WR")
def21 <- dfdef %>% 
  filter(yearID == 2021) %>%
  mutate(Name = str_remove(Name, "\\*"))%>%
  mutate(Name = str_remove(Name, "\\+")) %>%
  select(Name, yearID, Team, FloStrength) %>%
  mutate(pos = "DEF")
players21 <- rbind(qb21, rb21)
players21 <- rbind(players21, wr21)
players21 <- rbind(players21, def21) %>%
  mutate(Name = str_trim(Name))

rosterfs1 <- left_join(rosterfs1, players21, by = c("Name","pos")) %>%
  group_by(Name, pos) %>%
  mutate(FloStrength.y = mean(FloStrength.y)) %>%
  slice(1) %>%
  mutate(FSChange21= FloStrength.x - FloStrength.y)
# Time to predict players
predfs <- function(position1, rosterfs1, lessgo) {
  test = position1
  if (test == "WR"){
    rosterfspos <- rosterfs1 %>%
      filter(pos == "WR"| pos == "RB")
    ind <- which(rosterfspos$pos == "RB")
    rosterfspos$pos[ind] <- "WR"
    rosterfspos$depth[ind] <- "WR5"
  } else {
    rosterfspos <- rosterfs1 %>%
      filter(pos == position1)
  }
  known <- lessgo %>%
    filter(Pos == position1) %>%
    mutate(FSChange10 = FloStrength.x - FloStrength.y) %>%
    mutate(FSChange21 = FloStrength.y - FloStrength) %>%
    select(Name, FloStrength.y, FloStrength.x, FloStrength, FSChange10, FSChange21, Pos)
  linmod <- lm(FSChange10~FSChange21, data = known)
  rosterfspos1 <- rosterfspos %>%
    filter(is.na(FloStrength.x) == FALSE & is.na(FloStrength.y) == FALSE) %>%
    mutate(predFS = linmod$coefficients[1] + linmod$coefficients[2] * FSChange21 + FloStrength.x)
  unknown <- lessgo %>%
    filter(Pos == position1) %>%
    filter(is.na(FloStrength.y) == TRUE & is.na(FloStrength) == TRUE)
  middle = mean(unknown$FloStrength.x)
  rosterfspos2 <- rosterfspos %>%
    filter(is.na(FloStrength.x) == TRUE & is.na(FloStrength.y) == TRUE) %>%
    mutate(predFS = middle)
  missedlast <- lessgo %>%
    filter(Pos == position1) %>%
    filter(is.na(FloStrength.y) == TRUE & is.na(FloStrength) == FALSE)
  linmod <- lm(FloStrength.x~FloStrength, data = missedlast)
  rosterfspos3 <- rosterfspos %>%
    filter(is.na(FloStrength.x) == TRUE & is.na(FloStrength.y) == FALSE) %>%
    mutate(predFS = linmod$coefficients[1] + linmod$coefficients[2] * FloStrength.y)
  
  secondyear <- lessgo %>%
    filter(Pos == position1) %>%
    filter(is.na(FloStrength.y) == FALSE & is.na(FloStrength) == TRUE)
  linmod <- lm(FloStrength.x~FloStrength.y, data = secondyear)
  rosterfspos4 <- rosterfspos %>%
    filter(is.na(FloStrength.x) == FALSE & is.na(FloStrength.y) == TRUE) %>%
    mutate(predFS = linmod$coefficients[1] + linmod$coefficients[2] * FloStrength.x)
  playpredros <- rbind(rosterfspos1, rosterfspos2)
  playpredros <- rbind(playpredros, rosterfspos3)
  playpredros <- rbind(playpredros, rosterfspos4)
  return(playpredros)
}
predqb22 <- predfs("QB", rosterfs1, lessgo) %>%
  select(Name, Team.x, predFS,pos, depth)%>%
  mutate(weight = 1)
teamdf <- data.frame(teams) %>%
  rename("Team.x" = "teams")
predqb22 <- right_join(predqb22, teamdf, by = "Team.x")
ind <- which(is.na(predqb22$predFS) == TRUE)
predqb22$predFS[ind] = -.5
predqb22$pos[ind] = "QB"
predqb22$depth[ind] = "QB1"
predqb22$weight[ind] = 1
predqb22$Name[ind] = "Inj"

predrb22 <- predfs("RB", rosterfs1, lessgo)%>%
  select(Name, Team.x, predFS,pos, depth)%>%
  mutate(weight = 1)
predrb22 <- right_join(predrb22, teamdf, by = "Team.x")
ind <- which(is.na(predrb22$predFS) == TRUE)
predrb22$predFS[ind] = -.5
preddef22 <- predfs("DEF", rosterfs1, lessgo)%>%
  select(Name, Team.x, predFS,pos, depth)%>%
  mutate(weight = 1)
predwr22 <- predfs("WR", rosterfs1, lessgo)%>%
  select(Name, Team.x, predFS,pos, depth) %>%
  mutate(weight = 0)
ind <- which(predwr22$depth == "WR1")
predwr22$weight[ind] <- .5
ind <- which(predwr22$depth == "WR2")
predwr22$weight[ind] <- .35
ind <- which(predwr22$depth == "WR3")
predwr22$weight[ind] <- .2
ind <- which(predwr22$depth == "WR4")
predwr22$weight[ind] <- .3
ind <- which(predwr22$depth == "WR5")
predwr22$weight[ind] <- .25
predplayer22 <- rbind(predqb22, predrb22)
predplayer22 <- rbind(predplayer22, preddef22)
predplayer22 <- rbind(predplayer22, predwr22) %>%
  mutate(weightedfs = weight*predFS)
predwrteam <- predplayer22 %>%
  filter(pos == "WR") %>%
  group_by(Team.x) %>%
  mutate(wrfs = mean(weightedfs)) %>%
  slice(1)%>%
  select(Team.x, wrfs)
predqbteam <- predplayer22 %>%
  filter(pos == "QB") %>%
  group_by(Team.x) %>%
  mutate(qbfs = mean(weightedfs))%>%
  slice(1)%>%
  select(Team.x, qbfs)
predrbteam <- predplayer22 %>%
  filter(pos == "RB") %>%
  group_by(Team.x) %>%
  mutate(rbfs = mean(weightedfs))%>%
  slice(1)%>%
  select(Team.x, rbfs)
preddefteam <- predplayer22 %>%
  filter(pos == "DEF") %>%
  group_by(Team.x) %>%
  mutate(deffs= mean(weightedfs))%>%
  slice(1) %>%
  select(Team.x, deffs)
predteamdf <- left_join(predwrteam, predqbteam, by = "Team.x")
predteamdf <- left_join(predteamdf, predrbteam, by = "Team.x")
predteamdf <- left_join(predteamdf, preddefteam, by = "Team.x")%>%
  mutate(rbfs=ifelse(is.na(rbfs)==TRUE,-1.5,rbfs))
predteam <- predteamdf%>%
  mutate(Year = 2023) %>%
  group_by(Year) %>%
  mutate(qbfs = (qbfs - mean(qbfs))/ sd(qbfs)) %>%
  mutate(wrfs = (wrfs - mean(wrfs))/ sd(wrfs)) %>%
  mutate(rbfs = (rbfs - mean(rbfs))/ sd(rbfs)) %>%
  mutate(deffs  = (deffs - mean(deffs))/ sd(deffs)) %>%
  mutate(teamfs = .5+ .06406* qbfs + .02525 * deffs + .03803 * rbfs + .01832* wrfs) %>%
  mutate(predW = teamfs * 17) %>%
  rename("Team" = "Team.x") %>%
  mutate(Team = toupper(Team))
#time to predict teams
df<-read.csv("AllTimeTeams")
prevyear <- df %>%
  mutate(wins=str_extract(Record,"\\d{1,2}"))%>%
  mutate(losses=str_extract(str_extract(Record,"\\-\\d{1,2}\\-"),"\\d{1,2}"))%>%
  mutate(ties=str_remove(Record,"\\d{1,2}\\-\\d{1,2}\\-"))%>%
  mutate(Wpct = as.numeric(wins)/(as.numeric(wins)+as.numeric(losses)+as.numeric(ties)))%>%
  filter(yearID == 2022) %>%
  select("Team"=Tm,"prevWP" = Wpct,"prevFS"= FloStrength)
teams <- c("Miami Dolphins"="MIA", "Cincinnati Bengals" = "CIN", "Baltimore Ravens" = "BAL", "Philadelphia Eagles"= "PHI", 
           "Buffalo Bills"= "BUF", "Denver Broncos"= "DEN", "Atlanta Falcons"= "ATL", "New York Jets"="NYJ","New Orleans Saints" = "NO", "Seattle Seahawks"="SEA","San Francisco 49ers"="SF","Chicago Bears"="CHI", "Dallas Cowboys" = "DAL", "Indianapolis Colts" ="IND", "Minnesota Vikings"="MIN" ,"San Diego Chargers" ="SDG","New York Giants"= "NYG", "Oakland Raiders" = "LV", "Detroit Lions"="DET", "Cleveland Browns"="CLE","Houston Texans" = "HOU", "Carolina Panthers"="CAR","Tampa Bay Buccaneers"="TB" ,"Arizona Cardinals"="ARI", "St. Louis Rams" = "RAM","New England Patriots"="NE","Tennessee Titans"="TEN","Washington Commanders"= "WSH", "Pittsburgh Steelers"= "PIT" ,"Jacksonville Jaguars"="JAX","Green Bay Packers"="GB","Los Angeles Rams"= "LAR","Los Angeles Chargers"  ="LAC","Las Vegas Raiders"="LV","Washington Football Team"="WAS", "Kansas City Chiefs" = "KC")
prevyear$Team <-as.character(teams[prevyear$Team])
preddf<-left_join(predteam,prevyear, by = "Team")%>%
  arrange(prevWP)%>%
  mutate(draft=c(1:32))%>%
  mutate(prevFS=(prevFS-mean(prevFS))/sd(prevFS))%>%
  ungroup()%>%
  select(Team, "qbsc"=qbfs,"rbsc"=rbfs,"defsc"=deffs,"wrsc"=wrfs,draft,prevFS)%>%
  arrange(Team)


predteamdf <- read.csv("~/FloStrength/NFLFloStrength/NFLPlayerPred/predteamdf")
linmod <- lm(Wpct~qbsc+rbsc+defsc+wrsc+draft+prevFS, data = predteamdf)
predictions<-predict(linmod,newdata = preddf)
preddf<-preddf%>%
  mutate(fspred=predictions)
getsched <- function(weeknum){
  url <- str_c("https://www.pro-football-reference.com/years/2023/week_",weeknum, ".htm")
  h <- read_html(url) 
  away <- html_nodes(h, ".date+ tr td:nth-child(1) a") %>% html_text
  home <- html_nodes(h, ".teams tr:nth-child(3) a") %>% html_text
  sched <- data.frame(away, home) %>%
    mutate(Week = weeknum)
}
schedule22 <- map_df(.x= 1:18, .f=getsched)
teams <- c("Miami Dolphins"="MIA", "Cincinnati Bengals" = "CIN", "Baltimore Ravens" = "BAL", "Philadelphia Eagles"= "PHI", 
           "Buffalo Bills"= "BUF", "Denver Broncos"= "DEN", "Atlanta Falcons"= "ATL", "New York Jets"="NYJ","New Orleans Saints" = "NO", "Seattle Seahawks"="SEA","San Francisco 49ers"="SF","Chicago Bears"="CHI", "Dallas Cowboys" = "DAL", "Indianapolis Colts" ="IND", "Minnesota Vikings"="MIN" ,"San Diego Chargers" ="SDG","New York Giants"= "NYG", "Oakland Raiders" = "RAI", "Detroit Lions"="DET", "Cleveland Browns"="CLE","Houston Texans" = "HOU", "Carolina Panthers"="CAR","Tampa Bay Buccaneers"="TB" ,"Arizona Cardinals"="ARI", "St. Louis Rams" = "RAM","New England Patriots"="NE","Tennessee Titans"="TEN","Washington Redskins"= "WAS", "Pittsburgh Steelers"= "PIT" ,"Jacksonville Jaguars"="JAX","Green Bay Packers"="GB","Los Angeles Rams"= "LAR","Los Angeles Chargers"  ="LAC","Las Vegas Raiders"="LV","Washington Commanders"="WSH", "Kansas City Chiefs" = "KC")
schedule22$away <-as.character(teams[schedule22$away])
schedule22$home <-as.character(teams[schedule22$home])
awaypred <- preddf %>%
  select("away" = "Team","awayfs"= fspred) 
schedule22 <- left_join(schedule22,awaypred, by = "away")
homepred <- preddf %>%
  select("home" = "Team","homefs"= fspred) 
schedule22 <- left_join(schedule22,homepred, by = "home")
awaysched <- schedule22 %>%
  group_by(away) %>%
  mutate(awaysos = sum(homefs)) %>%
  slice(1) %>%
  ungroup() %>%
  select("Team" = away, awaysos)
homesched <- schedule22 %>%
  group_by(home) %>%
  mutate(homesos = sum(awayfs)) %>%
  slice(1) %>%
  ungroup() %>%
  select("Team" = home, homesos) 
sos22 <- left_join(awaysched, homesched, by = "Team") %>%
  mutate(psos = (awaysos + homesos) / 17)

linmod <- lm(Wpct~qbsc+rbsc+defsc+wrsc+draft+prevFS+psos, data = predteamdf)
preddf <- left_join(preddf, sos22, by = "Team") 
predictions<-predict(linmod,newdata = preddf)
predteam <- preddf %>%
  mutate(PredWpct=predictions)%>%
  mutate(predWins = 17 * PredWpct)
# save that shit
write_csv(predteam, "/Users/seanfloersch/FloStrength/FloStrengthFuture/NFL/NFLTeamPred2023")
write_csv(predplayer22, "/Users/seanfloersch/FloStrength/FloStrengthFuture/NFL/NFLPlayPred2023")

