#source("global_variable.R")
source("position_check.R")
#source("rk_estimate.R")

main_simulation<-function(custom_wall_structure,rk_test = FALSE,out_of_range = FALSE)
{
  time_used = 0
  isRoadKill = 0
  time_state = list(slope=0,vertical=0,road=0)
  first_seawall= "N"
  
  #initial position
  start_point = sample(hotspot,1)
  origin_xy<-matrix(c(start_point,0),nrow=1,ncol=2)
  
  #roadkill model construction
  if(rk_test == TRUE)
  {
    model<-roadkill_model_construction(rk_data)
  }
  
  while(time_used < total_t & origin_xy[1,2] < top_limit) 
  {
    wall_near<-wall_search(origin_xy,wall_structure)
    position_now<-position_check(state_now,state_previous,state_prob,wall_near,first_seawall)
    state_now<-position_now$state_now
    state_previous<-position_now$state_previous
    first_seawall<-position_now$first_seawall
    
    corrd_result<-coordinate_check(origin_xy, state_now, state_previous,time_used,time_state,out_of_range)
    origin_xy<-corrd_result$origin_xy
    time_state<-corrd_result$time_state
    time_used = time_used+corrd_result$time_cum
    
    if(state_now[3] == 1 & rk_test == TRUE)
    {
      isRoadKill = roadkill_prediction(model,corrd_result$time_cum)$state
      if(isRoadKill == 1)
      {
        break
      }
    }
    
    if(origin_xy[1,1] == -1)
    {
      break
    }
    
    state_prob<-prop_matrix%*%position_now$state_prob
  }
  
  final_state = NULL
  if(isRoadKill == 1)
  {
    final_state = "Roadkill"
  }
  else if(state_now[1,1] == 1)
  {
    final_state = "Slope"
  }
  else if(state_now[2,1] == 1)
  {
    final_state = "Vertical"
  }
  else
  {
    final_state = "Road"
  }
  
  success = NULL
  if(origin_xy[1,2] >= top_limit)
  {
    success = 'Y'
  }
  else
  {
    success = 'N'
  }
  
  result_list<-list(data=data.frame(final_state = final_state,time_used = time_used, success = success,time_state=time_state,first_seawall = first_seawall),
                    wall=wall_structure)
  return(result_list)
}