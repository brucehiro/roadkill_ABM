#setwd("D:/PhD/路殺/模擬分析")
setwd("D:/research/2025/路殺/publish/ABM_simulation")
set.seed(1022)


prop_matrix<-matrix(c(1,0,0,
                      0,0.730,0.270,
                      0.87,0.43,0),nrow=3,ncol=3)

#slope->slope = 1, vertical->vertical = 0.73 from Heish
#mean vertical: 0.43 from Chen et al., 2024
#mean slope = 0.87 from Chen et al., 2024

#total experiment time
total_t = 3*60*60 #second
#total_t = 13*60 #second

#slope velocity
slope_v = 50/43.6 #cm/s
vertical_v = 50/102.5 #cm/s
road_v = 50/43.6
#road_v = 208
slope_v_sd = 50/34.8
vertical_v_sd = 50/89.2
road_v_sd = 50/34.8
velocity_vertical_max = 50/360.9
velocity_slope_max = 50/188.6
velocity_road_max = 50/188.6

#experiment boundary
top_limit = 50
right_limit = 34000
#right_limit = 2200
left_limit = 0
bottom_limit = 0

#initial state
state_now<-matrix(c(0,0,1),nrow=3,ncol=1)
state_previous<-matrix(c(0,0,1),nrow=3,ncol=1)
state_prob<-matrix(c(0.87,0.43,0),nrow=3,ncol=1)
#hotspot<-c(1:2200)
hotspot<-c(14000:24000)

#slope vertical choice
withinSwitchRange = 20
switchProp<-c(0.24,0.76)


