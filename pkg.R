library(plotly)

# generic_r ------------------------------------------------------------------

TRY = function(x){
  tryCatch(
    x,
    error = function(e)
      NULL
  )}

decay = function(v,decay){
  if (decay == 0) {
    return(v)
  }
  else {
    stats::filter(v, decay, method = "recursive")
  }
}

diminish = function(v,m,abs=T){
  m = as.numeric(as.character(m))
  if (m == 0) {
    return(v)
  }
  else {
    1 - base::exp(-v/m)
  }
}

lag = function(v,l,zero=T){
  
  # if lag is zero, return original vector
  if(l == 0){
    return(v)
  }
  
  # get the length of the vector
  n = length(v)
    
  # if the lag is positive
  if(l > 0){
    
    #move forward 
    
    # cut forward extremities
    v = v[1:(n-l)]
    
    # if zero is TRUE
    if(zero == T){
      
      v = c(rep(0,l),v)
      
    }else if(zero == F){
      
      v = c(rep(v[1],l),v)
      
    }
  }
  
  if(l < 0){
    
    l = l*-1
    
    v = v[(l+1):n]
    
    if(zero == T){
      
      v = c(v,rep(0,l))
      
    }
    else{
      
      v = c(v,rep(v[length(v)],l))
      
    }
  }
  
  return(v)
  
}

ma = function(v,width,align="center",fill=1){
  
  if(width == 0){
    return(v)
  }
  
  else{
    
    # fill options:
    ## 0
    ## 1 (extremes)
    
    if(fill == 0){
      v = rollmean(v,width,fill = fill,align = align)
    }
    else if(fill == 1){
      
      fill = mean(v)
      v = rollmean(v,width,fill = fill,align = align)
      
      #if right, left, center
      # if odd or even
      # if(align == "center"){}
      # if(align == "left"){}
      # if(align == "right"){}
      # test_roll_mean = function(width,fill){
      #   
      #   df = data.frame(
      #     v0 = mtcars$mpg,
      #     vcenter = rollmean(mtcars$mpg,width,fill = fill,align = "center"),
      #     vright = rollmean(mtcars$mpg,width,fill = fill,align = "right"),
      #     vleft = rollmean(mtcars$mpg,width,fill = fill,align = "left"),
      #     id = 1:32
      #   ) %>% reshape2::melt(id.vars = "id")
      #   
      #   plot_ly() %>% 
      #     add_lines(data = df,x = ~id,y = ~value,color = ~variable)
      #   
      # }
      # 
      # test_roll_mean(width = 5,fill = NA)
      # 
      
    }
  }
  
  return(v)
}
