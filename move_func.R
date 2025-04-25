#movement function
move_time_cal<-function(origin_xy,velocity,move_type,time_used,out_of_range)
{
  wall_ground_dist = 0
  time_cum = sample(c(1:total_t),1)
  if(time_cum+time_used>total_t)
  {
    time_cum = total_t-time_used
  }

  if(move_type=="keep_wall" | move_type=="ground2wall")
  {
    if(origin_xy[1,2]+velocity*time_cum > top_limit)
    {
      time_cum = 1/(velocity/(top_limit-origin_xy[1,2]))
      origin_xy[1,2] = top_limit
    }
    else if(origin_xy[1,2]+velocity*time_cum == top_limit)
    {
      time_cum = 1/(velocity/top_limit)
      origin_xy[1,2] = top_limit
    }
    else
    {
      origin_xy[1,2] = origin_xy[1,2]+velocity*time_cum
    }
  }
  else if(move_type=="keep_ground")
  {
    distance_remain = velocity*time_cum
    while(1)
    {
      if(out_of_range == TRUE)
      {
        if(origin_xy[1,1]+distance_remain > right_limit | origin_xy[1,1]+distance_remain < left_limit)
        {
          origin_xy[1,1] = -1
          break
        }
        else
        {
          origin_xy[1,1] = origin_xy[1,1]+distance_remain
          break
        }
      }
      else
      {
        if(origin_xy[1,1]+distance_remain > right_limit)
        {
          dist2rightLimit = right_limit - origin_xy[1,1]
          origin_xy[1,1] = right_limit
          distance_remain = distance_remain-dist2rightLimit
          distance_remain = distance_remain*-1
        }
        else if(origin_xy[1,1]+distance_remain < left_limit)
        {
          dist2leftLimit = origin_xy[1,1] - left_limit
          origin_xy[1,1] = left_limit
          distance_remain = distance_remain+dist2leftLimit
          distance_remain = distance_remain*-1
        }
        else
        {
          origin_xy[1] = origin_xy[1]+distance_remain
          break
        }
      }
    }
  }
  else if(move_type=="wall2ground")
  {
    #print("wall2ground")
    wall_ground_dist<-origin_xy[1,2]
    origin_xy[1,2] = 0
    time_cum = 1/(velocity/wall_ground_dist)
  }
  
  #print(time_cum)
  result<-list(coordinate=origin_xy,time_cum=time_cum)
  return(result)
}

