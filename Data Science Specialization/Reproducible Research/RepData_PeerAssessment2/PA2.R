
storm.data.ready$PROPDMGEXP = as.character(storm.data.ready$PROPDMGEXP)
storm.data.ready$PROPDMGEXP[toupper(storm.data.ready$PROPDMGEXP) == 'B'] = "9"
storm.data.ready$PROPDMGEXP[toupper(storm.data.ready$PROPDMGEXP) == 'M'] = "6"
storm.data.ready$PROPDMGEXP[toupper(storm.data.ready$PROPDMGEXP) == 'K'] = "3"
storm.data.ready$PROPDMGEXP[toupper(storm.data.ready$PROPDMGEXP) == 'H'] = "2"
storm.data.ready$PROPDMGEXP = as.numeric(storm.data.ready$PROPDMGEXP)
storm.data.ready$PROPDMGEXP[is.na(storm.data.ready$PROPDMGEXP)] = 0
storm.data.ready$PropertyDamage = storm.data.ready$PROPDMG * 10^storm.data.ready$PROPDMGEXP
summary(storm.data.ready$PropertyDamage)

storm.data.ready$CROPDMGEXP = as.character(storm.data.ready$CROPDMGEXP)
storm.data.ready$CROPDMGEXP[toupper(storm.data.ready$CROPDMGEXP) == 'B'] = "9"
storm.data.ready$CROPDMGEXP[toupper(storm.data.ready$CROPDMGEXP) == 'M'] = "6"
storm.data.ready$CROPDMGEXP[toupper(storm.data.ready$CROPDMGEXP) == 'K'] = "3"
storm.data.ready$CROPDMGEXP[toupper(storm.data.ready$CROPDMGEXP) == 'H'] = "2"
storm.data.ready$CROPDMGEXP[toupper(storm.data.ready$CROPDMGEXP) == ''] = "0"
storm.data.ready$CROPDMGEXP = as.numeric(storm.data.ready$CROPDMGEXP)
storm.data.ready$CROPDMGEXP[is.na(storm.data.ready$CROPDMGEXP)] = 0
storm.data.ready$CropDamage = storm.data.ready$CROPDMG * 10^storm.data.ready$CROPDMGEXP
summary(storm.data.ready$CropDamage)

storm.data.ready$TotalDamage = storm.data.ready$PropertyDamage + storm.data.ready$CropDamage
summary(storm.data.ready$TotalDamage)

ggplot(storm.data.ready, aes(x = TotalDamage)) + geom_histogram() + scale_x_log10() + stat_bin(binwidth = .1)


#Now, take a look at the different event types.

event_type <- storm.data.ready$EVTYPE
event_counts <- count(event_type)

storm.data.ready$EVTYPE[toupper(storm.data.ready$EVTYPE) %in%
               c("HURRICANE/TYPHOON",
                 "HURRICANE OPAL",
                 "HURRICANE OPAL/HIGH WINDS",
                 "HURRICANE EMILY",
                 "TYPHOON",
                 "HURRICANE ERIN")] = "HURRICANE"
storm.data.ready$EVTYPE[toupper(storm.data.ready$EVTYPE) %in%
               c("TSTM WIND",
                 " TSTM WIND",
                 "SEVERE THUNDERSTORM WINDS",
                 "THUNDERSTORM WINDS")] = "THUNDERSTORM WIND"
storm$EVTYPE[toupper(storm$EVTYPE) %in%
               c("HEAVY RAIN/SEVERE WEATHER",
                 "EXCESSIVE RAINFALL",
                 "UNSEASONAL RAIN",
                 "HEAVY RAINS")] = "HEAVY RAIN"
storm.data.ready$EVTYPE[toupper(storm.data.ready$EVTYPE) %in%
               c("STORM SURGE/TIDE"
               )] = "STORM SURGE"
storm.data.ready$EVTYPE[toupper(storm.data.ready$EVTYPE) %in%
               c("WILD/FOREST FIRE",
                 "WILDFIRES",
                 "WILD FIRES")] = "WILDFIRE"
storm.data.ready$EVTYPE[toupper(storm.data.ready$EVTYPE) %in%
               c("EXCESSIVE HEAT",
                 "HEAT WAVE",
                 "EXTREME HEAT",
                 "UNSEASONABLY WARM",
                 "RECORD/EXCESSIVE HEAT",
                 "RECORD HEAT")] = "HEAT"
storm.data.ready$EVTYPE[toupper(storm.data.ready$EVTYPE) %in%
               c("EXTREME COLD",
                 "FROST/FREEZE",
                 "FROST",
                 "Early Frost ",
                 "DAMAGING FREEZE",
                 "RECORD COLD",
                 "COLD/WIND CHILL",
                 "EXTREME COLD/WIND CHILL",
                 "UNSEASONABLY COLD",
                 "Unseasonable Cold",
                 "HARD FREEZE",
                 "FREEZE")] = "COLD"
storm.data.ready$EVTYPE[toupper(storm.data.ready$EVTYPE) %in%
               c("HIGH WINDS",
                 "HIGH WIND",
                 "BLOWING WIND",
                 "STRONG WINDS",
                 "STRONG WIND")] = "WIND"
storm.data.ready$EVTYPE[toupper(storm.data.ready$EVTYPE) %in%
               c("FLASH FLOODING",
                 "FLASH FLOOD/FLOOD",
                 "FLOOD/FLASH FLOOD")] = "FLASH FLOOD"
storm.data.ready$EVTYPE[toupper(storm.data.ready$EVTYPE) %in%
               c("SMALL HAIL")] = "HAIL"
storm.data.ready$EVTYPE[toupper(storm.data.ready$EVTYPE) %in%
               c("RIVER FLOODING"
               )] = "RIVER FLOOD"
storm.data.ready$EVTYPE[toupper(storm.data.ready$EVTYPE) %in%
               c("FLOODING",
                 "MAJOR FLOOD")] = "FLOOD"
storm.data.ready$EVTYPE[toupper(storm.data.ready$EVTYPE) %in%
               c("COASTAL FLOODING",
                 "COASTAL FLOODING/EROSION",
                 "COASTAL  FLOODING/EROSION",
                 "Erosion/Cstl Flood",
                 "COASTAL FLOOD")] = "COASTAL FLOOD"
storm.data.ready$EVTYPE[toupper(storm.data.ready$EVTYPE) %in%
               c("TROPICAL STORM GORDON",
                 "TROPICAL STORM JERRY")] = "TROPICAL STORM"
storm.data.ready$EVTYPE[toupper(storm.data.ready$EVTYPE) %in%
               c("DENSE FOG"
               )] = "FOG"
storm.data.ready$EVTYPE[toupper(storm.data.ready$EVTYPE) %in%
               c("RIP CURRENTS"
               )] = "RIP CURRENT"
storm.data.ready$EVTYPE[toupper(storm.data.ready$EVTYPE) %in%
               c("HEAVY SURF",
                 "HEAVY SURF/HIGH SURF")] = "HIGH SURF"
storm.data.ready$EVTYPE[toupper(storm.data.ready$EVTYPE) %in%
               c("WATERSPOUT/TORNADO"
               )] = "WATERSPOUT"
storm.data.ready$EVTYPE[toupper(storm.data.ready$EVTYPE) %in%
               c("WINTRY MIX",
                 "WINTER WEATHER MIX",
                 "WINTER WEATHER/MIX")] = "WINTER WEATHER"
storm.data.ready$EVTYPE[toupper(storm.data.ready$EVTYPE) %in%
               c("WINTER STORMS"
               )] = "WINTER STORM"
storm.data.ready$EVTYPE[toupper(storm.data.ready$EVTYPE) %in%
               c("MARINE TSTM WIND"
               )] = "MARINE THUNDERSTORM WIND"


storm.data.ready$TotalPeople = storm.data.ready$INJURIES + storm.data.ready$FATALITIES

sorted.by.impacted.people = storm.data.ready[order(storm.data.ready$TotalPeople,decreasing = TRUE),]

sorted.by.damaged = storm.data.ready[order(storm.data.ready$TotalDamage,decreasing = TRUE),]


Top.50.impacted.people <- head(sorted.by.impacted.people,50)[,c('EVTYPE','TotalPeople')]
Top.50.damaged <- head(sorted.by.impacted.people,50)[,c('EVTYPE','TotalDamage')]

people <- ggplot(data=Top.50.most.impactful, aes(x=EVTYPE, y=TotalPeople)) +
  geom_bar(stat="identity") +
  labs(x="Event Type", y="People Impacted") +
  ggtitle("People Impacted by Event Type")

damage <- ggplot(data=Top.50.damaged, aes(x=EVTYPE, y=TotalDamage)) +
  geom_bar(stat="identity") +
  labs(x="Event Type", y="People Impacted") +
  ggtitle("Damage Caused by Event Type")

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(people, damage)
