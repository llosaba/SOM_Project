library(readr)



tipo <- "Passing" ##Açò està per fer
separador <- "\\"


tipus <-c("Passing", "Shooting") ##Ara mateix no fa res


equiposLiga <- c('Barcelona', 'Real Madrid', 'Sevilla', 'Real Sociedad', 'Getafe', 'Atletico Madrid', 'Valencia','Villareal',
                 'Granada', 'Athletic club', 'Osasuna', 'Real Betis', 'Levante', 'Alaves', 'Valladolid', 'Eibar', 'Celta Vigo', 'Mallorca', 'Leganes','Espanyol')

equiposPremier <- c("Arsenal","Aston Villa","Bournemouth","Brighton","Burnley","Chelsea","Crystal Palace",
                    "Everton","Leicester City","Liverpool","Manchester City","Manchester Utd","Newcastle",
                    "Norwich","Sheffield","Southampton","Tottenham","Watford", "West Ham", "Wolves")

equiposSerie <- c("Atalanta", "Bologna", "Brescia","Cagliari","Fiorentina","Genoa","Hellas Verona","Internazionale","Juventus","Lazio","Lecce","Milan","Napoli","Parma","Roma","Sampdoria","Sassuolo","SPAL","Torino","Udinese")

equiposBundesliga <- c("Augsburg","Bayer Leverkusen","Bayern Munich", "Borussia Dortmund", "Borussia Monchengladbach", "Eintracht Frankfurt","Fortuna Dusseldorf", "Freiburg","Hertha BSC", "Hoffenheim", "Koln", "Mainz 05", "Paderborn 07", "RB Leipzig", "Schalke 04", "Union Berlin", "Werder Bremen", "Wolfsburg"  )

equiposLigue <- c("Amiens", "Angers", "Bordeaux","Brest" , "Dijon", "Lille", "Lyon", "Marseille", "Metz", "Monaco", "Montpellier", "Nantes", "Nice", "Nimes", "Paris Saint-Germain", "Reims", "Rennes", "Saint Etienne", "Strasbourg", "Toulouse")

ligas <-list(equiposLiga, equiposPremier, equiposLigue, equiposBundesliga , equiposSerie)

names(ligas) <- c('La Liga', 'Premier league', 'Ligue 1', 'Bundesliga', 'Serie A')




datCompleto <- data.frame("Player"=factor(), "Nation"=factor(), "Pos"=factor(), "Age"=integer(), "x90s"=double(),"Cmp"=integer(), "Att"=integer(), "Cmp."=double(), "TotDist"=integer(), "PrgDist"=integer(), "Cmp.1"=integer(),
                          "Att.1"=integer(), "Cmp..1"=double(), "Cmp.2"=integer(), "Att.2"=integer(),"Cmp..2"=double(),"Cmp.3"=double(), "Att.3"=double(), "Cmp..3"=double(),
                          "Ast"=double(), "xA"=double(), "A.xA"=double(), "KP"=double(), "x1.3"=integer(), "PPA"=double() , "CrsPA"=double(), "Prog"=double(), "Matches"=factor(), "team"=factor(), "league"=factor())

k=1

for(j in ligas){
  for (i in j){
    i
    dat <- read.csv(paste( "BBDD", tipo, names(ligas)[k], paste(i,"csv", sep="."), sep=separador), header = TRUE, fileEncoding="UTF-16LE")
    dat$team <- i
    dat$league <- names(ligas)[k]
    datCompleto <- rbind(datCompleto, dat)
  }
  k=k+1
}

datCompleto <- select(datCompleto, -"Matches")

datCompleto <- na.omit(datCompleto)




nam <- c("Player", "Nation", "Pos", "Age", "Tjugado","Cmp.Total", "Att.Total", "Cmp%.Total", "TotDist", "PrgDist", "Cmp.short",
         "Att.short", "Cmp%.short", "Cmp.medium", "Att.medium","Cmp%.medium","Cmp.long", "Att.long", "Cmp%.long",
         "Ast", "xA", "A-xA", "KP", "1/3", "PPA" , "CrsPA", "Prog", "team", "league")
colnames(datCompleto) <- nam

datCompleto <- subset(datCompleto, Tjugado>3)

datSOM <- select(datCompleto, -c("Player", "Nation",  "Pos",  "Age", "Tjugado", "Cmp.Total", "Cmp.short", "Cmp.medium", "Cmp.long", "A-xA", "team", "league"))


dat <- select(datSOM, c( "Att.Total", "TotDist", "PrgDist", 
                         "Att.short",   "Att.medium", "Att.long",
                         "Ast",  "KP", "1/3", "PPA" , "CrsPA", "Prog"))/datCompleto$"Tjugado"
datSOM <- cbind(select(datSOM, -c( "Att.Total", "TotDist", "PrgDist", 
                                   "Att.short",   "Att.medium", "Att.long",
                                   "Ast",  "KP", "1/3", "PPA" , "CrsPA", "Prog")), dat)

datSOMscaled <- scale(datSOM, center=T, scale=T)



