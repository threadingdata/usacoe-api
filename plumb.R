source('global.R')

root <- pr()
root

# API FILES ----
plumber <- pr('plumber.R')
usbr <-   pr('usbr/usbr.R')
cdfw <- pr('cdfw/cdfw.R')
dwr <- pr('dwr/dwr.R')
cdec <- pr('cdec/cdec.R')
noaa <- pr('noaa/noaa.R')
plotly_anim <- pr('html-widget/plotly-animation.R')

# Mount API files to the router ----
mounts <- root %>%
  pr_mount('/usbr', usbr) %>%
  pr_mount('/cdfw', cdfw) %>%
  pr_mount('/dwr', dwr) %>%
  pr_mount('/cdec', cdec) %>%
  pr_mount('/noaa', noaa) %>%
  pr_mount('/plotly_anim', plotly_anim) %>%
  pr_set_error(function(req, res, err){
    print(err)
    res$status <- 500
    list(error = "An error occurred. Please contact your administrator.")
  }) %>%
 # pr_set_api_spec(read_yaml(cf[['plumber_openapi_yaml']])) %>%
  pr_hook("postroute", function(req, value) {
    # Print stage information
    str(list(
      stage = "postroute",
      type = req$REQUEST_METHOD,
      path = req$PATH_INFO,
      value = value
    ))
    # Must return the `value` since we took one in
    value
  }) %>%
  pr_hook("postserialize", function(req, value) {
    # Print stage
    str(list(
      stage = "postserializinformatione",
      type = req$REQUEST_METHOD,
      path = req$PATH_INFO,
      value = value
    ))
    # Must return the `value` since we took one in
    value
  }) %>%
  pr_hook("exit", function(){
    print("Database Pool Closing...")
    pool::poolClose(conn)
  })
mounts

mounts %>%
  # Change to 0.0.0.0 to run from any machine
  # Can also add options
  pr_run(host = '127.0.0.1',
         port = 8000)

# pr_hook('preroute', auth-user) %>%
# pr_hook('preroute', require-auth)
# pr_filter('auth_user', auth_user) %>%
# pr_filter('require-auth', require-auth)

#%>%
  # pr_set_error(function(req, res, err){
  #   print(err)
  #   res$status <- 500
  #   list(error = "An 500 error occurred. What is ya, ignorant?")
  # }) %>%
  # pr_set_error(function(req, res, err){
  #   print(err)
  #   res$status <- 400
  #   list(error = "An 400 error occurred. What is ya, ignorant?")
  # }) %>%
  # pr_set_404(
#   function(req, res) {
#     res$status <- 404
#     res$body <- "Oops"
#     }) %>%
#pr_set_docs(docs = 'rapidoc')


#  Other Endpoints ----
#filters <- pr('filters.R')
#usbr <- pr('usbr.R')
#
# dfg <- pr('dfg.R')
# database <- pr('rds_database.R')
#
# mounts <- root %>%
#   pr_mount('/plumber', plumber) %>%
#   pr_mount('/usbr', usbr) %>%
#   pr_mount('/dwr', dwr) %>%
#   pr_mount('/dfg', dfg) %>%
#   pr_mount('/database', database)
#
#
# mounts
#
# mounts %>%
#   pr_run(host = '127.0.0.1',
#          port = 8000)
#
# #pool::poolClose(conn)
# #pr('plumber.R') %>%
# #  pr_run(host = '127.0.0.1',
# #         port = 8000)
