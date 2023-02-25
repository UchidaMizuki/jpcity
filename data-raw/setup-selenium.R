source("data-raw/setup.R")

# setup-selenium ----------------------------------------------------------

webdriver <- import("selenium")$webdriver
Service <- import("selenium.webdriver.chrome.service")$Service
By <- import("selenium.webdriver.common.by")$By
ActionChains <- import("selenium.webdriver.common.action_chains")$ActionChains
Select <- import("selenium.webdriver.support.select")$Select

ChromeDriverManager <- import("webdriver_manager.chrome")$ChromeDriverManager

new_driver <- function(exdir,
                       headless = TRUE) {
  options <- webdriver$ChromeOptions()
  if (headless) {
    options$add_argument("--headless=new")
  }
  # options$headless <- headless

  options$add_experimental_option("prefs",
                                  list(`download.default_directory` = path_abs(exdir) |>
                                         str_replace_all("/", "\\\\")))

  webdriver$Chrome(service = Service(ChromeDriverManager()$install()),
                   options = options)
}

select_date <- function(driver, date, year_name, month_name, day_name) {
  date_year <- driver$find_element(By$XPATH, str_glue('//select[@name="{year_name}"]'))
  date_year <- Select(date_year)
  date_year$select_by_value(as.character(year(date)))

  date_month <- driver$find_element(By$XPATH, str_glue('//select[@name="{month_name}"]'))
  date_month <- Select(date_month)
  date_month$select_by_value(as.character(month(date)))

  date_day <- driver$find_element(By$XPATH, str_glue('//select[@name="{day_name}"]'))
  date_day <- Select(date_day)
  date_day$select_by_value(as.character(day(date)))
}

click_pref <- function(driver, pref_code) {
  checkbox <- driver$find_element(By$XPATH, '//input[@name="prefecture_all"]')
  if (checkbox$is_selected()) {
    checkbox$click()
  }

  pref_boxes <- driver$find_elements(By$XPATH, '//td[@data-alias="pref_boxes"]//input')
  for (checkbox in pref_boxes) {
    if (checkbox$is_selected()) {
      checkbox$click()
    }

    pref_code_checkbox <- checkbox$get_attribute("value")
    if (pref_code_checkbox %in% pref_code) {
      checkbox$click()
    }
  }
}

click_city_category <- function(driver, city_kd = 2:7) {
  city_category_list <- driver$find_elements(By$XPATH, '//td[@data-alias="city_category_list"]//input')

  for (checkbox in city_category_list) {
    if (checkbox$is_selected()) {
      checkbox$click()
    }

    city_kd_checkbox <- checkbox$get_attribute("value")
    if (city_kd_checkbox %in% city_kd) {
      checkbox$click()
    }
  }
}

click_submit_button <- function(driver) {
  driver$find_element(By$XPATH, '//button[@value="search"]')$click()
}

click_download_button <- function(driver) {
  driver$find_element(By$XPATH, '//li[contains(@class,"js-dbview-download-button")]/button')$click()

  button <- driver$find_element(By$XPATH, '//div[contains(@class,"stat-display_selector-modal-ok")]')
  action <- ActionChains(driver)
  action$move_to_element(button)
  action$click(button)
  action$perform()
}

close_driver <- insistently(\(driver, exdir) {
  if (vec_is_empty(dir_ls(exdir))) {
    stop()
  } else {
    driver$close()
  }
},
rate = rate_backoff(max_times = 1e1),
quiet = FALSE)
