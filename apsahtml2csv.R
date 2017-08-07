apsahtml2csv = function(directory, file.name, file.ext = ".htm", verbose = TRUE){
  
  jobs = dir(directory, full.names = TRUE)[grep(file.ext, dir(directory))]
  
  for(i in 1:length(jobs)){
    
    if(verbose == TRUE){
      cat("Starting ", jobs[i], "\n")
    }
    
    write("\n", jobs[i], append=T)
    
    # read in data
    data = htmlParse(jobs[i])
    
   # job id
    xpath = '//*[(@id = "dnn_ctr4356_ViewJobBank_ViewJob_lbl_JobID")]'
    extracted_info = 
      xpathSApply(data, xpath, xmlValue)
    if(length(extracted_info) == 0)
    {
      extracted_info = 'N/A'
    }
    assign('jobid', extracted_info)
    
    # start date
    xpath = '//*[(@id = "dnn_ctr4356_ViewJobBank_ViewJob_lb_DateAvailable")]'
    extracted_info = 
      xpathSApply(data, xpath, xmlValue)
    if(length(extracted_info) == 0)
    {
      extracted_info = 'N/A'
    }
    assign('startdate', extracted_info)
    
    # deadline
    xpath = '//*[(@id = "dnn_ctr4356_ViewJobBank_ViewJob_lb_Deadline")]'
    extracted_info = 
      xpathSApply(data, xpath, xmlValue)
    if(length(extracted_info) == 0)
    {
      extracted_info = 'N/A'
    }
    assign('deadline', extracted_info)
    
    # position
    xpath = '//*[(@id = "dnn_ctr4356_ViewJobBank_ViewJob_lb_Title")]'
    extracted_info = 
      xpathSApply(data, xpath, xmlValue)
    if(length(extracted_info) == 0)
    {
      extracted_info = 'N/A'
    }
    assign('position', extracted_info)
    
    # subfield
    xpath = '//*[(@id = "dnn_ctr4356_ViewJobBank_ViewJob_lb_Subfield1")]'
    extracted_info = 
      xpathSApply(data, xpath, xmlValue)
    if(length(extracted_info) == 0)
    {
      extracted_info = 'N/A'
    }
    assign('subfield', extracted_info)
    
    # dept
    xpath = '//*[(@id = "dnn_ctr4356_ViewJobBank_ViewJob_lb_Department")]'
    extracted_info = 
      xpathSApply(data, xpath, xmlValue)
    if(length(extracted_info) == 0)
    {
      extracted_info = 'N/A'
    }
    assign('dept', extracted_info)
    
    # inst
    xpath = '//*[(@id = "dnn_ctr4356_ViewJobBank_ViewJob_lb_Company")]'
    extracted_info = 
      xpathSApply(data, xpath, xmlValue)
    if(length(extracted_info) == 0)
    {
      extracted_info = 'N/A'
    }
    assign('inst', extracted_info)
    
    # areas expertise
    xpath = '//*[(@id = "dnn_ctr4356_ViewJobBank_ViewJob_lb_Expertise1")]'
    extracted_info = 
      xpathSApply(data, xpath, xmlValue)
    if(length(extracted_info) == 0)
    {
      extracted_info = 'N/A'
    }
    assign('areas', extracted_info)
    
    # contact
    xpath = '//*[(@id = "dnn_ctr4356_ViewJobBank_ViewJob_lb_ContactName")]'
    extracted_info = 
      xpathSApply(data, xpath, xmlValue)
    if(length(extracted_info) == 0)
    {
      extracted_info = 'N/A'
    }
    assign('contact', extracted_info)
    
    # email
    xpath = '//*[(@id = "dnn_ctr4356_ViewJobBank_ViewJob_lb_Email")]'
    extracted_info = 
      xpathSApply(data, xpath, xmlValue)
    if(length(extracted_info) == 0)
    {
      extracted_info = 'N/A'
    }
    assign('email', extracted_info)
    
    # alt deadline
    xpath = '//*[(@id = "dnn_ctr4356_ViewJobBank_ViewJob_lb_Email")]'
    extracted_info = 
      xpathSApply(data, xpath, xmlValue)
    if(length(extracted_info) == 0)
    {
      extracted_info = 'N/A'
    }
    assign('email', extracted_info)
    
    
   
    df = data.frame(jobid = jobid, 
                     subfield = subfield, startdate = startdate, 
                     deadline = deadline, position = position, dept = dept,
                     institution = inst, 
                     contact = contact) %>% 
      as.data.frame()
    
    
    if(i == 1){
      myjobs = df
    }else{
      myjobs = merge(myjobs, df, all = TRUE)
    }
  }
  
  write.csv(myjobs, file = file.name, row.names = FALSE, na = "")
  
  if(verbose == TRUE){
    cat("Data stored as file `", file.name, "'.  \nThe current working directory is ", getwd(), "\n", sep="")
  }
}
