source("global_variable.R")

wall_construct<-function(scenario)
{

  if(scenario$start_wall == "vertical")
  {
    start_wall = -1
  }
  else
  {
    start_wall = 1
  }
  
  if(scenario$intra_wall == "vertical")
  {
    intra_wall = -1
  }
  else
  {
    intra_wall = 1
  }
  
  wall_extent = 0
  if(scenario$wall_width_intra==0 | start_wall == intra_wall)
  {
    #max_num_wall = round(right_limit/(scenario$wall_width_start+scenario$wall_width_intra),0)
    max_num_wall = round(right_limit/(scenario$wall_width_start),0)
  }
  else
  {
    max_num_wall = round(2*right_limit/(scenario$wall_width_start+scenario$wall_width_intra),0)
  }
  
  wall_structure<-matrix(nrow=2,ncol=max_num_wall+2)
  wall_structure[1,1] = 0
  wall_structure[2,1] = start_wall
  #wall_structure[1,2] = scenario$wall_width_start
  #wall_structure[2,2] = start_wall
  wall_counter = 2
  next_wall_type = start_wall
  next_wall_index = 1
  next_wall_switch = -1
  next_wall_width = c(scenario$wall_width_start,scenario$wall_width_intra)

  while(wall_extent<=right_limit)
  {
    wall_extent = wall_extent + next_wall_width[next_wall_index]
    wall_structure[1,wall_counter] = wall_extent
    wall_structure[2,wall_counter] = next_wall_type
    
    if(wall_extent>=right_limit)
    {
      break
    }
      
    if(start_wall == intra_wall | scenario$wall_width_intra == 0)
    {
      next_wall_type = next_wall_type*1
    }
    else
    {
      next_wall_type = next_wall_type*-1
      next_wall_index = next_wall_index-next_wall_switch
      next_wall_switch = next_wall_switch*-1
    }
    
    wall_counter = wall_counter+1
  }
  
  return(wall_structure)
}

slope_vertical_switch<-function(current_coorx,wall_structure,middle)
{
  shortestDist = 0
  wall_type = wall_structure[2,middle]
  if(current_coorx-wall_structure[1,middle-1] < wall_structure[1,middle]-current_coorx)
  {
    shortestDist = current_coorx-wall_structure[1,middle-1]
  }
  else
  {
    shortestDist = wall_structure[1,middle]-current_coorx
  }
  
  if(shortestDist <= withinSwitchRange & wall_structure[2,middle-1] != wall_structure[2,middle])
  {
    wall_type <- sample(c(-1,1),1,prob=switchProp)
  }
  
  return(wall_type)
}


wall_search<-function(location_xy,wall_structure)
{
  current_coorx = location_xy[1]
  left = 1
  right = dim(wall_structure)[2]
  wall_type = 0
  isFind = 0
  middle = floor((right+left)/2)
  while(left<right)
  {
    
    if(wall_structure[1,middle] == current_coorx)
    {
      wall_type = wall_structure[2,middle]
      isFind = 1
      break
    }
    else if(wall_structure[1,middle] > current_coorx)
    {
      right = middle-1
      middle = floor((right+left)/2)
    }
    else if(wall_structure[1,middle] < current_coorx)
    {
      left = middle+1
      middle = floor((right+left)/2)
    }
  }
  
  if(middle == 1)
  {
    middle = middle+1
  }
 

  if(isFind != 1)
  {
    if(current_coorx>wall_structure[1,middle] & middle < dim(wall_structure)[2])
    {
      middle = middle+1
    }
    wall_type = wall_structure[2,middle]
  }
  
  #if(unique(wall_structure[,2]))
  wall_type<-slope_vertical_switch(current_coorx,wall_structure,middle)
  
  return(wall_type)
}


#unit test
unit_test<-function(test_list,wall_structure)
{
  for(i in test_list)
  {
    result<-wall_search(c(i,0),wall_structure)
    print(wall_structure)
    print(i)
    print(result)
  }
}


