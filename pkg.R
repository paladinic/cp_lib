library(plotly)

# CP LIB ------------------------------------------------------------------

decomping = function(coefs, cats, dep_var, data) {
  
  vars = filter(select(coefs, variable), variable != "(Intercept)")
  
  dep_var_data = data %>%
    select(date, !!sym(dep_var)) %>%
    rename(actual = !!sym(dep_var))
  
  data = data %>%
    select(vars$variable, date) %>%
    mutate("(Intercept)" = 1) %>%
    reshape2::melt(id.vars = "date")
  
  data = data %>%
    mutate(variable = as.character(variable)) %>%
    left_join(select(coefs, variable, Estimate), by = "variable")
  
  data = data %>%
    mutate(contrib = Estimate * value)
  
  # no cats for now
  cats = coefs %>%
    select(variable) %>%
    filter(variable != "(Intercept)") %>%
    mutate(cats = variable,
           calcs = "none") %>%
    mutate_all(as.character())
  
  data = data %>%
    left_join(cats, by = "variable") %>%
    mutate(calcs = as.character(calcs)) %>%
    mutate(calcs = if_else(is.na(calcs), "none", calcs)) %>%
    mutate(cats = as.character(cats)) %>%
    mutate(cats = if_else(is.na(cats), "Other", cats)) %>%
    mutate(calcs = if_else(variable == "(Intercept)", "none", calcs)) %>%
    mutate(cats = if_else(variable == "(Intercept)", "Base", cats))
  
  
  data = data %>%
    group_by(date, cats) %>%
    summarise(contrib = sum(contrib))
  
  avm = data %>%
    group_by(date) %>%
    summarise(model = sum(contrib)) %>%
    left_join(dep_var_data, by = "date") %>%
    mutate(residual = actual - model)
  
  return(list(decomp = data,
              avm = avm))
}

decomp_chart = function(decomp){
  
  decomp_data = decomp$decomp
  avm_data = decomp$avm
  

  
  plot_ly(
    data = decomp_data,
    y = ~contrib,
    x = ~date,
    color = ~cats,
    type = 'bar'
  ) %>%
    add_trace(
      data = avm_data,
      x = ~date,
      y = ~actual,
      type = 'scatter',
      mode = 'lines',
      name = "actual"
    ) %>%
    layout(font = list(color = "white"),
           yaxis = list(title = 'Contribution',
                        zerolinecolor = "white",
                        linecolor = toRGB("white")),
           xaxis = list(zerolinecolor = "white",
                        linecolor = toRGB("white")),
           barmode = 'relative')%>% 
    layout(plot_bgcolor='#2B3E50') %>% 
    layout(paper_bgcolor='#2B3E50')
  
}



avm_chart = function(decomp){
  avm_data = decomp$avm %>% 
    reshape2::melt(id.vars = "date")
  
  plot_ly(data = avm_data,type = "scatter",x = ~date,y = ~value,color = ~variable,mode = "lines")%>% 
    layout(font = list(color = "white"),
           yaxis = list(title = 'Contribution',
                        zerolinecolor = "white",
                        linecolor = toRGB("white")),
           xaxis = list(zerolinecolor = "white",
                        linecolor = toRGB("white")))%>% 
    layout(plot_bgcolor='#2B3E50') %>% 
    layout(paper_bgcolor='#2B3E50')
  
}

TRY = function(x){
  tryCatch(
    x,
    error = function(e)
      NULL
  )}

as_func = function(v, decay = 0){  if (decay == 0) {
    return(v)
  } else{
    stats::filter(v, decay, method = "recursive")
  }}
dr_func = function(v, m = max(v)){
  m = as.numeric(as.character(m))
  if(m == 0) {
    return(v)
  } else{
    1 - base::exp(-v / m)
  }
}

# example of UNIX timestamp
timestamp = 1551873597
print(timestamp)

# from time stamp to date
date = as.Date(as.POSIXct(timestamp, origin="1970-01-01"))
print(date)

# from date to timestamp 
unix_timestamp = as.integer(as.POSIXct(date))
print(unix_timestamp)
