source('global.R')

root <- pr()
root

# API FILES ----
usacoe <- pr('usacoe/usacoe.R')

# Mount API files to the router ----
mounts <- root %>%
  pr_mount('/usacoe', usacoe) %>%
  pr_set_error(function(req, res, err){
    print(err)
    res$status <- 500
    list(error = "An error occurred. Please contact the administrator at contact@threadingdata.com")
  }) %>%
  pr_hook("exit", function(){
    print("Database Pool Closing...")
    pool::poolClose(conn)
  })

mounts %>%
  pr_run(host = '127.0.0.1',
         port = 8000)
