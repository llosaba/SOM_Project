#Script con todas las ligas
library(devtools)
library(dplyr)
library(readr)
library(kohonen)

###Inpunts
separador <- "/"

tipo <- c("Passing", "Shooting", "Defensive",'Possession') 

equiposLiga <- c('Barcelona', 'Real Madrid', 'Sevilla', 'Real Sociedad', 'Getafe', 'Atletico Madrid', 'Valencia','Villarreal',
                 'Granada', 'Athletic club', 'Osasuna', 'Real Betis', 'Levante', 'Alaves', 'Valladolid', 'Eibar', 'Celta Vigo', 'Mallorca', 'Leganes','Espanyol')

equiposPremier <- c("Arsenal","Aston Villa","Bournemouth","Brighton","Burnley","Chelsea","Crystal Palace",
                    "Everton","Leicester City","Liverpool","Manchester City","Manchester Utd","Newcastle",
                    "Norwich","Sheffield","Southampton","Tottenham","Watford", "West Ham", "Wolverhampton")

equiposSerie <- c("Atalanta", "Bologna", "Brescia","Cagliari","Fiorentina","Genoa","Hellas Verona","Internazionale","Juventus","Lazio","Lecce","Milan","Napoli","Parma","Roma","Sampdoria","Sassuolo","SPAL","Torino","Udinese")

equiposBundesliga <- c("Augsburg","Bayern Leverkusen","Bayern Munich", "Borussia Dortmund", "Borussia", "Eintracht Frankfurt","Fortuna Dusseldorf", "Freiburg","Hertha BSC", "Hoffenheim", "Koln", "Mainz 05", "Paderborn 07", "RB Leipzig", "Schalke 04", "Union Berlin", "Werder Bremen", "Wolfsburg"  )

equiposLigue <- c("Amiens", "Angers", "Bordeaux","Brest" , "Dijon", "Lille", "Lyon", "Marseille", "Metz", "Monaco", "Montpellier", "Nantes", "Nice", "Nimes", "Paris Saint Germain", "Reims", "Rennes", "Saint Etienne", "Strasbourg", "Toulouse")

ligas <-list(equiposLiga, equiposPremier, equiposLigue, equiposBundesliga , equiposSerie)

names(ligas) <- c('La Liga', 'Premier league', 'Ligue 1', 'Bundesliga','Serie A' )





Passing <- data.frame("Player"=factor(), "Nation"=factor(), "Pos"=factor(), "Age"=integer(), "x90s"=double(),"Cmp"=integer(), "Att"=integer(), "Cmp."=double(), "TotDist"=integer(), "PrgDist"=integer(), "Cmp.1"=integer(),
                      "Att.1"=integer(), "Cmp..1"=double(), "Cmp.2"=integer(), "Att.2"=integer(),"Cmp..2"=double(),"Cmp.3"=double(), "Att.3"=double(), "Cmp..3"=double(),
                      "Ast"=double(), "xA"=double(), "A.xA"=double(), "KP"=double(), "x1.3"=integer(), "PPA"=double() , "CrsPA"=double(), "Prog"=double(), "Matches"=factor(), "team"=factor(), "league"=factor())

Shooting <- data.frame("Player"=factor(), "Nation"=factor(), "Pos"=factor(), "Age"=integer(), "x90s"=double(),"Gls"=integer(), "PK"=integer(), "PKatt"=integer(), "Sh"=integer(), "SoT"=integer(), "FK"=integer(),
                       "SoT%"=double(), "Sh/90"=double(), "SoT/90"=double(), "G/Sh"=double(),"G/SoT"=double(),"xG"=double(), "npxG"=double(), "npxG/Sh"=double(),
                       "G-xG"=double(), "np:G-xG"=double(), "Matches"=factor(), "team"=factor(), "league"=factor())

Defensive <- data.frame("Player"=factor(), "Nation"=factor(), "Pos"=factor(), "Age"=integer(), "x90s"=double(),"Tkl"=integer(), "TklW"=integer(), "Def 3rd"=integer(), "Mid 3rd"=integer(), "Att 3rd"=integer(),"Tkl"=integer(), "Att"=integer(), "Tkl%"=double(),
                        "Past"=integer(), "Press"=integer(), "Succ"=integer(), "%"=double(),"Def 3rd"=integer(), "Mid 3rd"=integer(), "Att 3rd"=integer(),"Blocks"=integer(), "Sh"=integer(), "ShSv"=integer(),
                        "Pass"=integer(),  "Int"=integer(), "Clr"=integer() , "Err"=integer(), "Matches"=factor(), "team"=factor(), "league"=factor())

Possession <- data.frame("Player"=factor(), "Nation"=factor(), "Pos"=factor(), "Age"=integer(), "x90s"=double(), 
                         'Touches'=integer(),	'DefPen'=integer(),	'Def3rd'=integer(),	'Mid3rd'=integer(),	
                         'Att3rd'=integer(),	'Att Pen'=integer(),	'Live'=integer(),	'Succ'=integer(),
                         'Att'=integer(),	'Succ%'=double(),	'#Pl'=integer(),	'Megs'=integer(),	
                         'Carries'=integer(),	'TotDist'=integer(),	'PrgDist'=integer(),	'Targ'=integer(),	
                         'Rec'=integer(),	'Rec%'=double(),	'Miscon'=integer(),	'Dispos'=integer()	,"Matches"=factor(), "team"=factor(), "league"=factor())


datTotal <- data.frame()

k=1
for(j in ligas){
  
  for (i in j){
    
    i
    
    dat1 <- read.csv(paste("BBDD", 'Passing' , names(ligas)[k], paste(i,"csv", sep="."), sep=separador), header = TRUE, fileEncoding="UTF-16LE")
    dat1 <- select(dat1,-c("Matches") )
    dat2 <- read.csv(paste("BBDD", 'Shooting' , names(ligas)[k], paste(i,"csv", sep="."), sep=separador), header = TRUE, fileEncoding="UTF-16LE")
    dat2 <- select(dat2,-(1:5) )
    dat2 <- select(dat2,-c("Matches") )
    
    dat3 <- read.csv(paste( "BBDD", 'Defensive' , names(ligas)[k], paste(i,"csv", sep="."), sep=separador), header = TRUE, fileEncoding="UTF-16LE")
    dat3 <- select(dat3,-(1:5) )
    dat3 <- select(dat3,-c("Matches") )
    dat4 <- read.csv(paste("BBDD", 'Possession' , names(ligas)[k], paste(i,"csv", sep="."), sep=separador), header = TRUE, fileEncoding="UTF-16LE")
    dat4 <- select(dat4,-(1:5) )
    dat4 <- select(dat4,-c("Matches") )
    
    dat <- cbind(dat1,dat2,dat3,dat4)
    
    dat$team <- i
    
    dat$league <- names(ligas)[k]
    datTotal <- rbind(datTotal, dat)
  }
  k=k+1
}

datTotal[is.na(datTotal)] <- 0


datTotal$Player[datTotal$Player=="Nicholas Opoku\\Nicholas-Opoku"] <-NA

datTotal <- na.omit(datTotal)





nam <- c("Player", "Nation", "Pos", "Age", "Tjugado","Cmp.Total", "Att.Total", "Cmp%.Total", "TotDist", "PrgDist", "Cmp.short",
         "Att.short", "Cmp%.short", "Cmp.medium", "Att.medium","Cmp%.medium","Cmp.long", "Att.long", "Cmp%.long",
         "Ast", "xA", "A-xA", "KP", "1/3", "PPA" , "CrsPA", "Prog", "Gls", "PK", "PKatt", "Sh", "SoT", "FK",
         "SoT%", "Sh/90", "SoT/90", "G/Sh","G/SoT","xG", "npxG", "npxG/Sh", "G-xG", "np:G-xG","Tkl", "TklW", "Def 3rd",
         "Mid 3rd", "Att 3rd",'TklD', "AttD", "TklD%",
         "Past", "Press", "PressSucc", "Press%","PressDef 3rd", "PressMid 3rd", "PressAtt 3rd","Blocks", "BlockSh", "BlocKShSv",
         "BlockPass",  "Int", "Clr" , "Err",'Touches',	'DefPen',	'Def3rd',	'Mid3rd',	
         'Att3rd',	'Att Pen',	'Live',	'Succ', 'Att',	'Succ%',	'#Pl',	'Megs',	    'Carries',	'TotDist',	'PrgDist',	'Targ',	
         'Rec',	'Rec%',	'Miscon',	'Dispos', "team", "league")

colnames(datTotal) <- nam


datTotal <- subset(datTotal, Tjugado>3)


datSOMTotal <- select(datTotal, -c("Player", "Nation",  "Pos",  "Age", "Tjugado", "Cmp.Total", "Cmp.short", "Cmp.medium", "Cmp.long",
                                   "A-xA",'PK','PKatt',"FK","Sh/90", "SoT/90", "G/Sh","G/SoT","xG", "npxG", "npxG/Sh",
                                   "G-xG", "np:G-xG","TklD%","Past","Press%","BlocKShSv",'Succ%','Megs','Rec%', "team", "league" ))


dat <- select(datSOMTotal, c( "Att.Total", "TotDist", "PrgDist", 
                              "Att.short",   "Att.medium", "Att.long",
                              "Ast",  "KP", "1/3", "PPA" , "CrsPA", "Prog","Gls", "Sh", "SoT","Tkl", "TklW", "Def 3rd", "Mid 3rd", "Att 3rd",'TklD', "AttD", 
                              "Press", "PressSucc", "PressDef 3rd", "PressMid 3rd", "PressAtt 3rd","Blocks", "BlockSh", 
                              "BlockPass",  "Int", "Clr" , "Err",'Touches',	'DefPen',	'Def3rd',	'Mid3rd',	
                              'Att3rd',	'Att Pen',	'Live',	'Succ', 'Att', '#Pl',	 'Carries',	'TotDist',	'PrgDist',	'Targ',	
                              'Rec',	'Miscon',	'Dispos'))/datTotal$"Tjugado"





datSOMTotal <- cbind(select(datSOMTotal, -c( "Att.Total", "TotDist", "PrgDist", 
                                             "Att.short",   "Att.medium", "Att.long",
                                             "Ast",  "KP", "1/3", "PPA" , "CrsPA", "Prog","Gls", "Sh", "SoT","Tkl", "TklW", "Def 3rd", "Mid 3rd", "Att 3rd",'TklD', "AttD", 
                                             "Press", "PressSucc", "PressDef 3rd", "PressMid 3rd", "PressAtt 3rd","Blocks", "BlockSh", 
                                             "BlockPass",  "Int", "Clr" , "Err",'Touches',	'DefPen',	'Def3rd',	'Mid3rd',	
                                             'Att3rd',	'Att Pen',	'Live',	'Succ', 'Att', '#Pl',	 'Carries',	'TotDist',	'PrgDist',	'Targ',	
                                             'Rec',	'Miscon',	'Dispos')), dat)

datSOMTotalscaled <- scale(datSOMTotal, center=T, scale=T)


#Para crear las tablas de datos

datPassing2 <- select(datTotal, 1:27, 86:87)

datShooting <- select(datTotal, 1:5, 28:43, 86:87)

datDefensive <- select(datTotal, 1:5, 44:65, 86:87)

datPossesion <- select(datTotal, 1:5, 66:87)



PCA <- prcomp(datSOMTotal, center = TRUE,scale. = TRUE, retx=TRUE)


varRel <- PCA$sdev^2/sum(PCA$sdev^2)

varCum <- cumsum(varRel)

varCum[length((varCum))] <- 1

