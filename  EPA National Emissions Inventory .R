url1 <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
destfile1 <- "destfile.zip"

if(!file.exists(destfile1)) {
  download.file(url1, 
                destfile = destfile1, 
                method = "curl")
  unzip(destfile1, exdir = ".")
}

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
names(url1)
names(NEI)
names(SCC)

#1
head(NEI)
tot.emissions.year <- NEI %>%  
  group_by(year) %>%
summarise(Total.Emissions = sum(Emissions, na.rm = TRUE))

tot.emissions.year
with(tot.emissions.year,
     plot(x = year, 
          y = Total.Emissions, 
          ylab = "Total Annual Emissions [Tons]", 
          xlab = "Year",
          main = "Total Annual Emissions in the US by Year",
          cex = 1,
          pch = 1,
          col = "red",
          lwd = 2))
tot.emissions.1999 <- tot.emissions.year[tot.emissions.year$year == 1999, 2]
tot.emissions.2008 <- tot.emissions.year[tot.emissions.year$year == 2008, 2]
delta.tot.emissions <- tot.emissions.2008 - tot.emissions.1999
#Decreased

#2
tot.emissions.baltimore <- NEI %>%
  subset(fips == "24510") %>%
  group_by(year) %>%
  summarise(Total.Emissions.Baltimore = sum(Emissions, 
                                            na.rm = TRUE))

with(tot.emissions.baltimore, 
     plot(x = year, 
          y = Total.Emissions.Baltimore, 
          ylab = "Total Annual Emissions [Tons]", 
          xlab = "Year",
          main = "Total Annual Emissions in Baltimore by Year",
          cex = 2,
          pch = 2,
          col = "red",
          lwd = 3))
tot.emissions.baltimore.2008 <- tot.emissions.baltimore[tot.emissions.baltimore$year == 2008, 2]
tot.emissions.baltimore.1999 <- tot.emissions.baltimore[tot.emissions.baltimore$year == 1999, 2]

delta.emissions.baltimore <- tot.emissions.baltimore.2008 - tot.emissions.baltimore.1999
#Baltimore decreased

#3
tot.emissions.type <- NEI %>% 
  subset(fips == "24510") %>%
  group_by(year, type) %>%
  summarise(Total.Emissions.Type = sum(Emissions, na.rm = TRUE))

emissions.type <- ggplot(data = tot.emissions.type, aes(year, Total.Emissions.Type))

emissions.type <- emissions.type + 
  geom_point(color = "red", 
             size = 4, 
             alpha = 1/3) + 
  facet_grid(. ~ type) +
  xlab("Year") +
  ylab("Total Emissions [Tons]") +
  ggtitle("Total Annual Emissions in Baltimore by Year")

emissions.type
#They decreased

#4
SCC.coal.comb <- SCC[grep("[Cc]oal",SCC$EI.Sector),]
NEI.sub.coal <- subset(NEI, 
                       NEI$SCC %in% SCC.coal.comb$SCC)


NEI.coal.comb <- merge(x = NEI.sub.coal, 
                       y = SCC, 
                       by.x = "SCC", 
                       by.y = "SCC")

NEI.coal.comb.tot <- NEI.coal.comb %>% 
  group_by(year) %>%
  summarise(Total.Coal.Comb = sum(Emissions, na.rm = TRUE))

NEI.coal.comb.plot <- ggplot(NEI.coal.comb.tot, aes(year, Total.Coal.Comb))

NEI.coal.comb.plot <- NEI.coal.comb.plot + 
  geom_point(color = "red", 
             size = 4, 
             alpha = 1/3) + 
  xlab("Year") +
  ylab("Total Emissions [Tons]") +
  ggtitle("Total Annual Coal Combustion Emissions in the US")

NEI.coal.comb.plot
#They decreased

#5
NEI.coal.comb.tot.2008 <- NEI.coal.comb.tot[NEI.coal.comb.tot$year == 2008, 2]
NEI.coal.comb.tot.1999 <- NEI.coal.comb.tot[NEI.coal.comb.tot$year == 1999, 2]

NEI.coal.comb.delta <- NEI.coal.comb.tot.2008 - NEI.coal.comb.tot.1999
NEI.coal.comb.delta

vehicle.scc <- SCC[grep("[Vv]eh", SCC$Short.Name), ]

emissions.motor.baltimore <- NEI %>% 
  subset(fips == "24510" & NEI$SCC %in% vehicle.scc$SCC) %>%
  merge(y = vehicle.scc, by.x = "SCC", by.y = "SCC") %>%
  group_by(year) %>%
  summarise(Vehicle.Emissions.Type = sum(Emissions, na.rm = TRUE))


emissions.motor.baltimore.plot <- ggplot(emissions.motor.baltimore, aes(year, Vehicle.Emissions.Type)) +
  geom_point(color = "red", 
             size = 4, 
             alpha = 1/3) + 
  xlab("Year") +
  ylab("Total Emissions [Tons]") +
  ggtitle("Total Annual Vehicle Emissions in Baltimore City")

emissions.motor.baltimore.plot

emissions.motor.baltimore.2008 <- emissions.motor.baltimore[emissions.motor.baltimore$year  == 2008, 2]
emissions.motor.baltimore.1999 <- emissions.motor.baltimore[emissions.motor.baltimore$year  == 1999, 2]

delta.baltimore <- emissions.motor.baltimore.2008 - emissions.motor.baltimore.1999
#They decreased

#6
vehicle.scc <- SCC[grep("[Vv]eh", SCC$Short.Name), ]

emissions.motor.la <- NEI %>% 
  subset(fips == "06037" & NEI$SCC %in% vehicle.scc$SCC) %>%
  merge(y = vehicle.scc, by.x = "SCC", by.y = "SCC") %>%
  group_by(year) %>%
  summarise(Vehicle.Emissions.Type = sum(Emissions, na.rm = TRUE))

emissions.motor.baltimore2 <- cbind(emissions.motor.baltimore, "City" = rep("Baltimore", 4))
emissions.motor.la2 <- cbind(emissions.motor.la, "City" = rep("LA", 4))

emissions.motor.comp <- rbind(emissions.motor.baltimore2, emissions.motor.la2)

emissions.motor.comp.plot <- ggplot(emissions.motor.comp, aes(year, Vehicle.Emissions.Type, col = City)) +
  geom_point(size = 4, 
             alpha = 1/3) +
  xlab("Year") +
  ylab("Total Emissions [Tons]") +
  ggtitle("Comparison of Total Annual Vehicle Emissions in Baltimore and Los Angeles")

emissions.motor.la.2008 <- emissions.motor.la[emissions.motor.la$year  == 2008, 2]
emissions.motor.la.1999 <- emissions.motor.la[emissions.motor.la$year  == 1999, 2]


delta.la <- emissions.motor.la.2008 - emissions.motor.la.1999
abs(delta.la) > abs(delta.baltimore)
emissions.motor.comp.plot
#Baltimore city 

