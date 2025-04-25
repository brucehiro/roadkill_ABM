source("simulation.R")
source("wall_construct.R")

scenario<-list(start_wall="sloped",wall_width_start=300,intra_wall="vertical",wall_width_intra=1600)
wall_structure<-wall_construct(scenario)
#wall_structure<-matrix(c(0,-1,16,-1,19,1,35,-1,38,1,54,-1,57,1,73,-1,76,1,92,-1,
#                         95,1,111,-1,114,1,130,-1,200,1,216,-1,219,1,235,-1,238,1,
#                         254,-1,257,1,273,-1,276,1,292,-1,295,1,311,-1,314,1,
#                         330,-1,333,1,340,-1),nrow=2,ncol=30)

result = NULL
result_success_s = c()
result_success_v = c()
result_success = c()
result_on_road_time = c()
result_on_road = c()
result_sv_proportion = c()
for(j in c(1:100))
{
  for(i in c(1:1000))
  {
    if(i == 1)
    {
      result<-main_simulation(wall_structure,rk_test = FALSE,out_of_range = FALSE)
      result_data = result$data
      result_wall = result$wall
    }
    else
    {
      result_data = rbind(result_data,main_simulation(wall_structure,rk_test = FALSE,out_of_range = FALSE)$data)
    }
  }
  result_success_s[j]=dim(subset(result_data,(result_data$success=='Y' & result_data$final_state=="Slope")))[1]
  result_success_v[j]=dim(subset(result_data,(result_data$success=='Y' & result_data$final_state=="Vertical")))[1]
  result_success[j]=dim(subset(result_data,result_data$success=='Y'))[1]
  result_on_road_time[j]=mean(result_data$time_state.road)
  result_on_road[j]=dim(subset(result_data,result_data$final_state=="Road"))[1]
  result_sv_proportion[j]=(dim(subset(result_data,result_data$first_seawall=="S"))[1]/6)/(dim(subset(result_data,result_data$first_seawall=="V"))[1]/16)
}


result_wall
mean(result_success)
sd(result_success)
mean(result_success_s)
sd(result_success_s)
mean(result_success_v)
sd(result_success_v)
mean(result_on_road_time)
sd(result_on_road_time)
mean(result_on_road)
sd(result_on_road)
mean(result_sv_proportion)
sd(result_sv_proportion)