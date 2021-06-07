


# for tables with m number of lead columns and n number of columns representing the dates surveys were done on, split the date columns into groups of num_dates_per_split, and create multiple sub-tables as the concatenation of the same lead columns and each successive group of date columns 
date_table_splitter <- function(ztable, num_lead_cols = 4, num_dates_per_split = 6) {

num.splits <- ceiling((ncol(ztable) - num_lead_cols) / num_dates_per_split)

seq.splits <- seq(1, num.splits)

col.splits <- num_lead_cols + (num_dates_per_split * seq.splits)

lead.cols <- c(1:num_lead_cols)

lead.col.width <- lead.col.width[1:num_lead_cols]

ztable1_cols <- c(lead.cols, (num_lead_cols+1):col.splits[1])
# define sub-table 1 columns

  if(ncol(ztable) <= col.splits[1]){
ztable1_cols <- c(1:ncol(ztable))
  }  
  if(ncol(ztable) > col.splits[1]){
ztable1_cols <- c(1:col.splits[1])
}

# define sub-table 2 columns
if(length(col.splits) > 1){
if(ncol(ztable) > col.splits[1]) {
  if(ncol(ztable) <= col.splits[2]){
ztable2_cols <- c(lead.cols, (col.splits[1]+1):ncol(ztable))
  }  
  if(ncol(ztable) > col.splits[2]){
ztable2_cols <- c(lead.cols, (col.splits[1]+1):col.splits[2])
}
}
}
# define sub-table 3 columns
if(length(col.splits) > 2) {
if(ncol(ztable) > col.splits[2]) {
  if(ncol(ztable) <= col.splits[3]){
ztable3_cols <- c(lead.cols, (col.splits[2]+1):ncol(ztable))
  }  
  if(ncol(ztable) > col.splits[3]){
ztable3_cols <- c(lead.cols, (col.splits[2]+1):col.splits[3])
}
}
}
# define sub-table 4 columns
if(length(col.splits) > 3) {
if(ncol(ztable) > col.splits[3]) {
  if(ncol(ztable) <= col.splits[4]){
ztable4_cols <- c(lead.cols, (col.splits[3]+1):ncol(ztable))
  }  
  if(ncol(ztable) > col.splits[4]){
ztable4_cols <- c(lead.cols, (col.splits[3]+1):col.splits[4])
}
}
}
# define sub-table 5 columns
if(length(col.splits) > 4) {
if(ncol(ztable) > col.splits[4]) {
  if(ncol(ztable) <= col.splits[5]){
ztable5_cols <- c(lead.cols, (col.splits[4]+1):ncol(ztable))
  }  
  if(ncol(ztable) > col.splits[5]){
ztable5_cols <- c(lead.cols, (col.splits[4]+1):col.splits[5])
}
} 
}

#--- make sub table 1
sub_table1 <- flextable(ztable[,ztable1_cols]) %>% 
  autofit() %>% 
  fit_to_width(max_width = 8.5) %>% 
  width(j = 1:num_lead_cols, width = lead.col.width) %>% 
  width(j = (num_lead_cols+1):length(ztable1_cols), width = date.col.width)

#--- make sub table 2
if(exists("ztable2_cols")) {
  
sub_table2 <- flextable(ztable[ztable2_cols]) %>% 
  autofit() %>% 
  fit_to_width(max_width = 8.5) %>%
  width(j = 1:num_lead_cols, width = lead.col.width) %>% 
  width(j = (num_lead_cols+1):length(ztable2_cols), width = date.col.width)
} else {
  sub_table2 <- NULL
}
#--- make sub table 3
if(exists("ztable3_cols")) {
  
sub_table3 <- flextable(ztable[ztable3_cols]) %>% 
  autofit() %>% 
  fit_to_width(max_width = 8.5) %>% 
  width(j = 1:num_lead_cols, width = lead.col.width) %>% 
  width(j = (num_lead_cols+1):length(ztable3_cols), width = date.col.width)
} else {
  sub_table3 <- NULL
}
#--- make sub table 4
if(exists("ztable4_cols")) {
  
sub_table4 <- flextable(ztable[ztable4_cols]) %>% 
  autofit() %>% 
  fit_to_width(max_width = 8.5) %>% 
  width(j = 1:num_lead_cols, width = lead.col.width) %>% 
  width(j = (num_lead_cols+1):length(ztable4_cols), width = date.col.width)
} else {
  sub_table4 <- NULL
}
#--- make sub table 5
if(exists("ztable5_cols")) {
  
sub_table5 <- flextable(ztable[ztable5_cols]) %>% 
  autofit() %>% 
  fit_to_width(max_width = 8.5) %>% 
  width(j = 1:num_lead_cols, width = lead.col.width) %>% 
  width(j = (num_lead_cols+1):length(ztable5_cols), width = date.col.width)
} else {
  sub_table5 <- NULL
}

out_sub_tables <- list(sub_table1 = sub_table1,
                       sub_table2 = sub_table2,
                       sub_table3 = sub_table3,
                       sub_table4 = sub_table4,
                       sub_table5 = sub_table5)
return(out_sub_tables)
}
