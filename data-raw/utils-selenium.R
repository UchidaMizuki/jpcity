
# utils-selenium ----------------------------------------------------------

ActionChains <- import("selenium.webdriver.common.action_chains")$ActionChains

selenium_driver <- function(path) {
  webdriver <- import("selenium")$webdriver
  Service <- import("selenium.webdriver.chrome.service")$Service
  ChromeDriverManager <- import("webdriver_manager.chrome")$ChromeDriverManager

  # options
  options <- webdriver$ChromeOptions()
  options$add_argument("--headless")
  prefs <- list(`download.default_directory` = here::here() |>
                  str_c(path_merger_raw,
                        sep = "/") |>
                  str_replace_all("/", r"(\\)"))
  options$add_experimental_option("prefs", prefs)

  webdriver$Chrome(service = Service(ChromeDriverManager()$install()),
                   options = options)
}

insistent_close_driver <- insistently(function(driver, path,
                                               size = 1L) {
  if (vec_size(dir_ls(path_merger_raw)) < size) {
    abort()
  } else {
    driver$close()
  }
})
