source("move_func.R")
source("global_variable.R")
source("wall_construct.R")

position_check<-function(state_now,state_previous,state_prob,wall_near, first_seawall)
{
  sample_slope = 0
  sample_vertical = 0
  sample_road = 0
  
  if(wall_near == 1) #slope
  {
    sample_slope = sample(c(0,1),1,prob=c(1-state_prob[1,1],state_prob[1,1]))
  }
  else #vertical
  {
    sample_vertical = sample(c(0,1),1,prob=c(1-state_prob[2,1],state_prob[2,1]))
  }
  
  if(sample_slope==0 & sample_vertical==0)
  {
    sample_road = 1
  }
  
  if(sample_slope == 1 & first_seawall == "N")
  {
    first_seawall = "S"
  }
  else if(sample_vertical == 1 & first_seawall == "N")
  {
    first_seawall = "V"
  }
  
  state_previous<-state_now
  state_now<-matrix(c(sample_slope,sample_vertical,sample_road),nrow=3,ncol=1)
  state_prob<-state_now
  
  return(list(state_now=state_now,state_previous=state_previous,state_prob=state_prob, first_seawall=first_seawall))
}


coordinate_check<-function(origin_xy, state_now,state_previous,time_used,time_state,out_of_range)
{
  state_change<-state_now-state_previous #origin_xy[1] == slope; origin_xy[2] == vertical; origin_xy[3] == ground
  direction<-sample(c(-1,1),1)
  time_cum = 0
  move_result<-NULL
  velocity_vertical = rnorm(1,vertical_v,vertical_v_sd)
  velocity_slope = rnorm(1,slope_v,slope_v_sd)
  velocity_road = rnorm(1,road_v,road_v_sd)
  
  if(velocity_vertical<0)
  {
    velocity_vertical = velocity_vertical_max
  }
  if(velocity_slope<0)
  {
    velocity_slope = velocity_slope_max
  }
  if(velocity_road<0)
  {
    velocity_road = velocity_road_max
  }
  
  if(state_change[2,1]==1 & state_change[3,1]==-1)
  {
    move_result<-move_time_cal(origin_xy,velocity_vertical,"ground2wall",time_used,out_of_range)
    origin_xy<-move_result$coordinate
    time_cum<-move_result$time_cum
    time_state$vertical<-time_state$vertical+move_result$time_cum
    #print("ground2wall-v")
  }
  else if(state_change[2,1]==-1 & state_change[3,1]==1)
  {
    #print("wall2ground-v")
    move_result<-move_time_cal(origin_xy,velocity_vertical,"wall2ground",time_used,out_of_range)
    origin_xy<-move_result$coordinate
    time_cum<-move_result$time_cum
    time_state$vertical<-time_state$vertical+move_result$time_cum
    
  }
  else if(state_change[1,1]==1 & state_change[3,1]==-1)
  {
    move_result<-move_time_cal(origin_xy,velocity_slope,"ground2wall",time_used,out_of_range)
    origin_xy<-move_result$coordinate
    time_cum<-move_result$time_cum
    time_state$slope<-time_state$slope+move_result$time_cum
    #print("ground2wall-s")
  }
  else if(state_change[1,1]==-1 & state_change[3,1]==1)
  {
    move_result<-move_time_cal(origin_xy,velocity_slope,"wall2ground",time_used,out_of_range)
    origin_xy<-move_result$coordinate
    time_cum<-move_result$time_cum
    time_state$slope<-time_state$slope+move_result$time_cum
    #print("wall2ground-s")
  }
  else
  {
    if(state_now[1,1]==1 & state_change[1,1]==0)
    {
      if(velocity_slope<0)
      {
        velocity_slope = 0
      }
      move_result<-move_time_cal(origin_xy,velocity_slope,"keep_wall",time_used,out_of_range)
      origin_xy<-move_result$coordinate
      time_cum<-move_result$time_cum
      time_state$slope<-time_state$slope+move_result$time_cum
      #print("keep_wall-s")
    }
    else if(state_now[2,1]==1 & state_change[2,1]==0)
    {
      if(velocity_vertical<0)
      {
        velocity_vertical = 0
      }
      move_result<-move_time_cal(origin_xy,velocity_vertical,"keep_wall",time_used,out_of_range)
      origin_xy<-move_result$coordinate
      time_cum<-move_result$time_cum
      time_state$vertical<-time_state$vertical+move_result$time_cum
      #print("keep_wall-v")
    }
    else if(state_now[3,1]==1 & state_change[3,1]==0)
    {
      if(velocity_road<0)
      {
        velocity_road = 0
      }
      velocity_slope = velocity_road*direction
      move_result<-move_time_cal(origin_xy,velocity_road,"keep_ground",time_used,out_of_range)
      #print("keep_ground")
      origin_xy<-move_result$coordinate
      time_cum<-move_result$time_cum
      time_state$road<-time_state$road+move_result$time_cum
    }
  }
  
  return(list(origin_xy=origin_xy,time_cum=time_cum,time_state=time_state))
}
