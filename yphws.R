#' YPHWS functions module 
#' 
#' The \code {PHEI_functions/yphws} contains functions used to:
#' - TBC

'.__module__.'

#' Looks for new commits in the GitLab repo for PHEI_functions. Changes can be pulled if found.
#' @param pull Set to TRUE by default. If FALSE, the function will only look for changes and update
#' @export


# Update ------------------------------------------------------------------

update <- function(pull = TRUE) {
  
  proj_url <- "@hertscc.managed.mango-solutions.com/git/hcc_phei/tools/phei_functions.git"
  
  # check status of local folder by fetching data and then checking git status
  branch_status <- system(paste0("cd ../../phei_functions \n
                                git fetch \n 
                                git status"), intern = TRUE)[2]
  
  # if up to date, the second line should say it is up to date with origin/master
  
  if (branch_status == "Your branch is up to date with 'origin/master'.") {
    
    message("You are up to date with the latest version.")
    
  } else {
    
    if (pull == T) {
      
      # fetch once again, and pull updates
      system(paste0("cd ../../phei_functions
                  git fetch https://", Sys.getenv("GIT_PH_USER"), ":", Sys.getenv("GIT_PH_PASS"), proj_url,
                  "\n git pull https://", Sys.getenv("GIT_PH_USER"), ":", Sys.getenv("GIT_PH_PASS"), proj_url, " master"))
      
      message("Updates pulled. You are up to date with the latest version.")
      
    } else {
      
      # if pull is set to FALSE, just state that updates were found without pulling. 
      message("Updates found. Please pull the latest version.")
      
    }
  }
  
}


# Common functions --------------------------------------------------------

#' Generates a dataset of summary statistics - counts, percentages, CIs
#' @param data School dataset of a given year 
#' @param by Variable in which to summarise by 
#' @export

summarystats <- function(data, by, q_coded) {
  # calculate summary metrics for all responses by specified groups
  # by should be a vector of columns to group by
  
  box::use(magrittr[`%>%`])

  t <- data %>% 
    tidyr::pivot_longer(cols = -{{ by }} ,
                 names_to = c("question"), values_to = "response") %>%
    dplyr::group_by_all() %>%
    dplyr::filter(!is.na(response)) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_at(dplyr::vars( {{ by }} , question)) %>%
    dplyr::mutate(denominator = sum(count)) %>%
    dplyr::rename(breakdown = by[2]) %>%
    dplyr::mutate(count = plyr::round_any(count, 5), # disclosure control - round to base 5
           denominator = sum(count)) %>% # redo denom with altered count
    dplyr::ungroup() %>% 
    dplyr::filter(denominator >= 5) %>% # disclosure control & CI calculation - filter out less than 5
    PHEindicatormethods::phe_proportion(., count, denominator, type='standard', confidence = 0.998) %>% 
    dplyr::mutate(question_text = q_coded$question_text[match(question,  #add friendly names
                                                       q_coded$question_coded)]) %>% 
    dplyr::select(breakdown, school, question, question_text, dplyr::everything()) #reorder columns
  
}

#' Returns dataset with differences found as new columns 
#' @param data Dataset from summarystats
#' @param by Variable to compare by 
#' @param from
#' @param to 
#' @param join
#' @export

differences <- function(data, by, from, to, join = NULL) {
  # looks for sig differences between report area and all responses
  # e.g. data, school, report_school, all_schools, schoolyear returns differences
  # between the respective year of the report school and all schools 
  # e.g. data, school, report_school, report_school returns differences between
  # each year of the report school (includes comaring year to itself)
  
  box::use(magrittr[`%>%`])
  
  data_from <- data[data[[by]] == from, ]
  data_to <- data[data[[by]] == to, ] 
  
  df <- dplyr::inner_join(data_from, data_to, by = c(join, "question", "response")) %>%
    dplyr::mutate(diff = dplyr::case_when(lowercl.x > uppercl.y ~ "significantly higher than",
                            uppercl.x < lowercl.y ~ "significantly lower than"),
           image = paste0("graphics/", diff, ".png"))
  
  return(df)
}

#' Returns summary statistics of survey data - count, percentage, 95% confidence intervals. The function will recode rating variables
#' (e.g. scoring 1 - 10) into categorical variables before this. For district level summary statistics, only districts with >= 250 responses from
#' at least 2 schools will be kept. 
#' @param data should be the yearly survey data (pinned/cleaned version)
#' @param group grouping variable you're working with
#' @param levels "All Responses", then each part of the group... e.g. "Female", "Male", "Others"
#' @param compare_to_all FALSE by default. If TRUE, it will compare the groups to ALl Responses and not to each other
#' @export

get_stats <- function(data,
                      group,
                      sch = NA,
                      q_coded) { 
  
  box::use(magrittr[`%>%`])
  box::use(dplyr[mutate, recode, select, group_by, summarise, case_when, distinct, filter, bind_rows, n, everything])
  box::use(tidyr[complete, expand, nesting])
  
  # recode scale to categories
  data <- data %>%
    mutate(life_satisfied = recode(life_satisfied, "0"="low","1"="low","2"="low","3"="low","4"="low","5"="medium","6"="medium","7"="high","8"="high","9"="very high","10"="very high"),
           life_worthwhile = recode(life_worthwhile, "0"="low","1"="low","2"="low","3"="low","4"="low","5"="medium","6"="medium","7"="high","8"="high","9"="very high","10"="very high"),
           life_happyyesterday = recode(life_happyyesterday, "0"="low","1"="low","2"="low","3"="low","4"="low","5"="medium","6"="medium","7"="high","8"="high","9"="very high","10"="very high"),
           life_satisfied_before_covid = recode(life_satisfied_before_covid, "0"="low","1"="low","2"="low","3"="low","4"="low","5"="medium","6"="medium","7"="high","8"="high","9"="very high","10"="very high"),
           alcohol_perday = recode(alcohol_perday, "0"="none","1"="1-2","2"="1-2","3"="3-4","4"="3-4","5"="5-6","6"="5-6","7"="7-9","8"="7-9","9"="7-9","10"="10+"),
           pa_60 = recode(pa_60, "0"="none","1"="1-3","2"="1-3","3"="1-3","4"="4-5","5"="4-5","6"="5-7","7"="5-7"),
           pa_30 = recode(pa_30, "0"="none","1"="1-3","2"="1-3","3"="1-3","4"="4-5","5"="4-5","6"="5-7","7"="5-7"))
  
  data_l <- list()
  
  # summary statistics for all schools. 
  data_l[["data_all_schools"]] <- data %>% 
    mutate(school = "All Schools", schyear = "All Responses") %>% 
    summarystats(c('school', 'schyear'), q_coded = q_coded)
  
  # summary statistics by group variable(s)
  for (g in 1:length(group)) {
    
    data_l[[paste0("data_by", group[g])]] <- data %>% 
      mutate(school = "All Schools") %>%  
      summarystats(c('school', group[g]), q_coded = q_coded)
    
  }
  
  # IF WE WANT TO GROUP BY DISTRICTS, WE NEED SOME ADDITIONAL PRE-PROCESSING. 
  if (any(grepl("District", group))) {
    
    responses_per_district <- data %>% 
      select(school, District) %>%  
      group_by(District) %>% 
      mutate(school_count = n()) %>% distinct() %>% 
      summarise(District, school_count, schools_per_district = n()) %>% distinct() %>% 
      mutate(comparison = case_when(school_count >= 250 & schools_per_district > 1 ~ TRUE, 
                                    TRUE ~ FALSE))
    
    data <- filter(data, District %in% responses_per_district$District[responses_per_district$comparison == T])
    
    # bind to list
    data_l[[paste0("data_by", "District")]] <- data %>% 
      mutate(school = "All Schools") %>%  
      summarystats(c('school', "District"), q_coded = q_coded)
    
  }
  
  omit_cols <- c("Ended", "Started", "UserID", "postcode", "postcode2", "sch_hidden", "school_other_hidden") #certain columns are not needed for reporting.
  
  stats <- bind_rows(data_l)
  
  # IF RUNNING SCHOOL REPORT (e.g. school = "Tring")
  if (!is.na(sch[1])) {
    
    data_l_d <- list()
    
    # summary statistics for school of interest
    data_l_d[["data_school"]] <- data %>%
      filter(school == sch) %>%
      mutate(schyear = "All Responses") %>% # report schools all years
      summarystats(c('school','schyear'), q_coded = q_coded)
    
    # summary statistics by group variable(s) for school of interest
    for (g in 1:length(group)) {
      
      data_l_d[[paste0("data_school_by", group[g])]] <- data %>% 
        filter(school == sch) %>%  
        summarystats(c('school', group[g]), q_coded = q_coded)
      
    }
    
    # additionally, if working with school data we should exclude "Not at school/other" 
    stats_d <- bind_rows(data_l_d) %>% 
      filter(breakdown != "Not at school/other")
    
    stats <- bind_rows(stats, stats_d)
    
  }
  
  stats <- stats %>%
    mutate(value = formattable::percent(value, digits = 1),
           lowercl = formattable::percent(lowercl, digits = 1),
           uppercl = formattable::percent(uppercl, digits = 1),
           lowereb = value - lowercl,
           uppereb = uppercl - value) %>% 
    complete(expand(., breakdown, school, 
                    nesting(question, response)),
             fill = list(count = 0, denominator = 0, value = 0,
                         lowercl = 0, uppercl = 0, lowereb = 0, uppereb = 0)) 
  
  return(stats)
  
}

#' Accepts output from get_stats() and outputs comparisons between groups based off of 95% confidence intervals. Additional columns are added that 
#' specify if one group is statistically higher/lower/similar to another group. This is done for every group, question, and response.
#' @param stats **must be** be the summary statistics output from get_stats() function
#' @param levels should be a vector specifying the order of the groups you are interested in. This argument also filters out the groups not present within this vector. For example, if you are interested in comparisons with sex/gender - \code {levels = c("Female", "Male", "Other")}
#' @param compare_to_all (optional) if TRUE, will only compare the groups in \code {levels} with "All Responses" in "All Schools", and not with each other. (e.g. Females compared to All, and not Females compared to Males etc.)
#' @param school (optional) can be left blank if the function is used for county level data. If using for school-level data, input the name of the school as a string.
#' @export

get_stats_diffs <- function(stats, 
                            levels, 
                            compare_to_all = F,
                            school = NA){
  
  box::use(magrittr[`%>%`])
  
  stats <- dplyr::filter(stats, breakdown %in% levels)
  
  # Check whether we are comparing one school to all schools
  if(!is.na(school)) {
    
    stats_diffs_all <- differences(stats, "school", school, "All Schools")
    
  } else {
    
    stats_diffs_all <- differences(stats, "school", "All Schools", "All Schools")
    
  }
  
  stats_diffs_all$breakdown.x <- factor(stats_diffs_all$breakdown.x, levels = levels)
  stats_diffs_all$breakdown.x <- droplevels(stats_diffs_all$breakdown.x)
  
  if(compare_to_all) {
    
    stats_diffs_all <- stats_diffs_all %>% 
      dplyr::filter(school.y == "All Schools") %>% 
      dplyr::filter(!breakdown.x == "All Responses") %>% 
      dplyr::filter(breakdown.y == "All Responses")
    
  } else {
    
    stats_diffs_all <- stats_diffs_all %>% 
      dplyr::filter(!(breakdown.x == breakdown.y & school.x == school.y)) 
    
  }
  
  return(stats_diffs_all)
  
}


# School report ---------------------------------------------------------

#' @export

create_sch_plot <- function(df, 
                            sch, 
                            sex = FALSE, 
                            plot_title, 
                            fontsize = 11, 
                            textangle = 0,
                            no_school = F){
  
  box::use(magrittr[`%>%`])
  box::use(tidyr[complete, expand, nesting])
  
  year <- "2021"
  col_db <- '#001874' #'rgba(0,24,116,1)' #dark blue
  
  # clean data so no denominator is less than 20 responses
  df <- tidyr::complete(df, tidyr::expand(df, tidyr::nesting(breakdown, school, response))) 
  
  if(sex) {
    
    data <- df %>%
      dplyr::filter(breakdown %in% c("All Responses", "Female", "Male", "Other", "Prefer not to say"))
    
    t <- table(data$breakdown[data$count < 20]) #find group that consistently has less than 20 responses
    remove <- names(t)[t == 14]
    
    data <- data[!data$breakdown %in% remove, ]
    
  } else {
    
    data <- df %>%
      dplyr::filter(breakdown %in% c("All Responses", "Year 7", "Year 8", "Year 9",
                                     "Year 10", "Year 11", "Year 12", "Year 13", "Not at school/other"))
    
    t <- table(data$breakdown[data$count < 20]) #find group that consistently has less than 20 responses
    remove <- names(t)[t == 14]
    
    data <- data[!data$breakdown %in% remove, ]
    
    g_levels <- c('Not at school/other',
                  'Year 13',
                  'Year 12',
                  'Year 11',
                  'Year 10',
                  'Year 9',
                  'Year 8',
                  'Year 7',
                  'All Responses')
    
    g_levels <- g_levels[!g_levels %in% remove]
    
    data$breakdown <- ordered(data$breakdown, levels = g_levels) 
    
  }
  
  data_all <- data %>% 
    dplyr::filter(school == "All Schools") 
  
  data_all$value[data_all$denominator < 20] <- 0
  data_all$lowereb[data_all$denominator < 20] <- 0
  data_all$uppereb[data_all$denominator < 20] <- 0
  data_all$lowercl[data_all$denominator < 20] <- 0
  data_all$uppercl[data_all$denominator < 20] <- 0
  
  p_all <- data_all %>%
    dplyr::arrange(breakdown) %>%
    plotly::plot_ly(x = ~stringr::str_wrap(response, 10), y = ~value, type = "bar", width = 900, height = 500,
                    name = ~breakdown, color = ~breakdown, colors = "viridis", 
                    legendgroup = ~breakdown,
                    error_y = list(array = ~uppereb, arrayminus = ~lowereb, 
                                   symmetric = F, color = '#000000', opacity = 0.2),
                    hovertemplate = ~paste0(value, " (", count, " responses) <br>CI: ", uppercl, " - ", lowercl, ")")) %>%
    plotly::layout(yaxis = list(title = 'Percent', tickformat = ',.0%'),
                   xaxis = list(tickangle = textangle, tickfont = list(size = fontsize), title = "All Schools"),
                   barmode = 'group')
  
  if(no_school) {
    
    p_all <- p_all %>% plotly::layout(title = list(text = paste(plot_title, "| YPHWS", year),
                                                   x = 0.05, font = list(color = col_db)))
    
    return(p_all)
    
  }
  
  data_school <- data %>% 
    dplyr::filter(school == sch)
  
  data_school$value[data_school$denominator < 20] <- 0
  data_school$lowereb[data_school$denominator < 20] <- 0
  data_school$uppereb[data_school$denominator < 20] <- 0
  data_school$lowercl[data_school$denominator < 20] <- 0
  data_school$uppercl[data_school$denominator < 20] <- 0
  
  p_sch <- data_school %>%
    dplyr::arrange(breakdown) %>%
    plotly::plot_ly(x = ~stringr::str_wrap(response, 10), y = ~value, type = "bar", width = 1100, height = 500,
                    name = ~breakdown, color = ~breakdown, colors = "viridis", 
                    legendgroup = ~breakdown, 
                    showlegend = F,
                    error_y = list(array = ~uppereb, arrayminus = ~lowereb,
                                   symmetric = F, color = '#000000', opacity = 0.2),
                    hovertemplate = ~paste0(count, ", ", value, " (CI: ", uppercl, " - ", lowercl, ")")) %>%
    plotly::layout(yaxis = list(title = 'Percent', tickformat = ',.0%'),
                   barmode = 'group', xaxis = list(title = sch),
                   xaxis = list(tickangle = textangle, tickfont = list(size = fontsize)))  
  
  plotly::subplot(p_sch, p_all, shareY = T, titleX = TRUE) %>%
    plotly::layout(title = list(text = paste(plot_title, "| YPHWS", year),
                                x = 0.05, font = list(color = col_db)),
                   images = list(source = base64enc::dataURI(file ="www/logo_phei.png"),
                                 x = 1, y = 0.99, sizex = 0.2, sizey = 0.2),
                   legend = list(y = 0.85),
                   xaxis = list(tickangle = textangle, tickfont = list(size = fontsize))) %>%
    plotly::config(displaylogo = FALSE, 
                   modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d", "autoScale2d", "toggleSpikelines")) 
  
}

#' @export
create_sch_plot_static <- function(df,
                                   sex = FALSE,
                                   plot_title,
                                   fontsize = 11,
                                   textangle = 0,
                                   no_school = F) {
  
  box::use(magrittr[`%>%`])
  box::use(ggplot2[aes, element_blank, element_rect, element_text])
  
  year <- "2021"
  if(no_school) {
    
    df <- dplyr::filter(df, school == "All Schools")
    
  }
  
  p <- df %>%
    dplyr::filter(breakdown %in% c("All Responses", "Female", "Male", "Other", "Prefer not to say")) %>%
    ggplot2::ggplot(aes(fill = breakdown, y = value)) +
    ggplot2::geom_bar(aes(x = stringr::str_wrap(response, 10)), position = "dodge",
                      stat = "identity") +
    viridis::scale_fill_viridis(discrete = T) +
    ggplot2::scale_y_continuous(labels = function(n){paste0(n * 100, "%")}) +
    ggplot2::ggtitle(paste(plot_title, "| YPHWS", year)) +
    ggplot2::geom_errorbar(aes(x = stringr::str_wrap(response, 10), ymin = lowercl, ymax = uppercl),
                           position = ggplot2::position_dodge(0.9),
                           width = 0.5, colour = "black", alpha = 0.5, size = 0.5) +
    ggplot2::xlab("") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "top",
                   panel.background = element_blank(),
                   strip.background = element_rect(fill = "#f5f5f5"),
                   legend.title = element_blank(),
                   legend.key.size = ggplot2::unit(0.3, "cm"),
                   legend.text = element_text(size = 8),
                   axis.text.x = element_text(size = 8, angle = textangle))
  
  if(no_school == F) {
    
    p <- p +
      ggplot2::facet_wrap(~school)
    
    return(p)
    
  }
  
  return(p)
  
}

#' @export

sch_tbl <- function(chk_diff_sch, 
                    q_coded, 
                    multi_response = F) {

  if (multi_response == F) {
    
    groupedby <- "breakdown"
    data <- chk_diff_sch %>% 
      dplyr::mutate(difference = dplyr::case_when(lowercl.x > uppercl.y ~ "higher",
                                                  uppercl.x < lowercl.y ~ "lower",
                                                  TRUE ~ "similar")) %>%
      dplyr::rename(count = count.x, percentage = value.x, `all schools` = value.y,
                    `comparison to all schools` = difference, breakdown = breakdown.x) %>% 
      dplyr::left_join(q_coded, by = c("question_text.x" = "question_text")) %>% 
      dplyr::select(breakdown, response, count, percentage, `all schools`, `comparison to all schools`)
    
  } else {
    
    groupedby <- c("breakdown", "question")
    data <- chk_diff_sch %>% 
      dplyr::mutate(difference = dplyr::case_when(lowercl.x > uppercl.y ~ "higher",
                                                  uppercl.x < lowercl.y ~ "lower",
                                                  TRUE ~ "similar"))
    
    if(length(unique(data$response)) <= 2) {
      
      data <- data %>% 
        dplyr::rename(count = count.x, percentage = value.x, `all schools` = value.y,
                      `comparison to all schools` = difference) %>% 
        dplyr::left_join(q_coded, by = c("question" = "question_coded")) %>% 
        dplyr::mutate(question = menu_text) %>% 
        dplyr::select(breakdown, question, response, count, percentage, `all schools`, `comparison to all schools`) %>% 
        dplyr::distinct()
      
      
    } else {
      
      data <- data %>% 
        dplyr::select(-question) %>% 
        dplyr::rename(count = count.x, percentage = value.x, `all schools` = value.y,
                      `comparison to all schools` = difference, 
                      question = question_text.x) %>% 
        dplyr::left_join(q_coded, by = c("question" = "question_text")) %>% 
        dplyr::select(breakdown, question, response, count, percentage, `all schools`, `comparison to all schools`) %>% 
        dplyr::distinct()
      
    }
    
  }
  
  if (nrow(data) == 0) {
    
    return("Data suppressed due to low amount of responses.")
    
  } else {
    
    data %>%
      reactable::reactable(groupBy = groupedby,
                           columns = list(
                             percentage = colDef(format = colFormat(percent = T, digits = 1)),
                             `all schools` = colDef(format = colFormat(percent = T, digits = 1)),
                             `comparison to all schools` = colDef(style = JS(
                               "function(rowInfo) {
                var value = rowInfo.row['comparison to all schools']
                if (value == 'similar') {
                var color = '#ffc000'} else {
                if (value == 'lower') {
                var color = '#5555e5' } else {
                var color = '#bed2ff' }
                }
                return { color: color, fontWeight: 'bold' }
                }"), minWidth = 100)
                           ))
    
  }
  
}

#' @export

create_sum_sentence <- function(dataset = chk_stats, 
                                multi = F, 
                                value_of_interest = F, 
                                full_data = stats,
                                diffs = chk_diff_sch,
                                diffs_yr = chk_diff_yr) {
  
  if (nrow(dataset[dataset$school == sch, ]) == 0) { return ("") }
  
  if (multi == F) { # run the following if it's a simple one-choice question
    
    q_binary <- F
    
    data <- dataset[dataset$school == sch, ]
    
    # generate the values used for the sentences. 
    
    df <- data[data$breakdown == "All Responses", ]
    
    most_common <- df$response[df$count == max(df$count)]
    most_v <- df$value_cens[df$count == max(df$count)]
    least_common <- df$response[df$count == min(df$count[df$count >= 0])]
    least_v <- df$value_cens[df$count == min(df$count[df$count >= 0])]
    
    if ("Female" %in% unique(data$breakdown) & "Male" %in% unique(data$breakdown)) {
      
      df <- data[data$breakdown == "Female", ]
      
      most_common[2] <- df$response[df$count == max(df$count)]
      most_v[2] <- df$value_cens[df$count == max(df$count)]
      least_common[2] <- df$response[df$count == min(df$count[df$count >= 0])]
      least_v[2] <- df$value_cens[df$count == min(df$count[df$count >= 0])]
      f_count <- df$denominator[!is.na(df$question_text)][1]
      
      df <- data[data$breakdown == "Male", ]
      
      most_common[3] <- df$response[df$count == max(df$count)]
      most_v[3] <- df$value_cens[df$count == max(df$count)]
      least_common[3] <- df$response[df$count == min(df$count[df$count >= 0])]
      least_v[3] <- df$value_cens[df$count == min(df$count[df$count >= 0])]
      m_count <- df$denominator[!is.na(df$question_text)][1]
      
      total_resp <- sum(data$count[data$breakdown %in% c("Female", "Male", "Other") & !is.na(data$question_text)])
      o_count <- total_resp - (f_count + m_count) 
      
    }
    
    # If all students responded to this question, skip the sex breakdown. Include if not. 
    
    total_resp <- sum(data$count[data$breakdown %in% c("Female", "Male", "Other") & !is.na(data$question_text)])
    max_resp <- max(full_data$denominator[full_data$school == sch])
    
    perc <- round(total_resp / max_resp * 100, 1)
    percent_cens <- perc #ifelse(perc >= 95, ">=95%", ifelse(perc <= 5, "<=5%", perc))
    
    total_resp <- ifelse(total_resp == max_resp, "every student", 
                         paste0(total_resp, " students", " (", percent_cens, "%)"))
    
    add_sex_breakdown <- ""
    
    if ("Female" %in% unique(data$breakdown) & "Male" %in% unique(data$breakdown)) {
      
      add_sex_breakdown <- ifelse(total_resp != "every student", 
                                  paste0(" Of these, ", f_count, " were female, ", m_count,
                                         " were male, and ", o_count, " either preferred not to say, or stated 'Other' for sex. "), "")
      
      # sometimes due to low response rate for each sex, NAs appear due to frequent rounding to 0. If this occurs, omit breakdown.
      if (is.na(f_count) | is.na(m_count)) { add_sex_breakdown <- ""}
      
    }
    
    # generate main sentence for All Responses
    
    if (is.na(total_resp)) { 
      
      return(paste0("Within the school, no students responded to this question."))
      
    } else {
      
      sentence <- paste0("Within the school, ", total_resp, " responded to this question." , add_sex_breakdown,
                         "<br> <br> The most common response was '", most_common[1], "', which made up ", most_v[1], 
                         " of responses and the least common response was '", least_common[1], "', with ",
                         least_v[1], " of responses.")
      
    }
    
    # generate additional sentences if male or female values for most common values differ
    if (max(data$denominator[data$breakdown == "Female" & data$school == sch])[1] != 0 &
        max(data$denominator[data$breakdown == "Male" & data$school == sch])[1] != 0 &
        "Female" %in% unique(data$breakdown) & "Male" %in% unique(data$breakdown)) {
      
      if (most_common[1] != most_common[2] | most_common[1] != most_common[3]) {
        
        if (most_common[1] != most_common[2]) {
          
          sentence <- paste0(sentence, 
                             " For female students, the most common response instead was '", most_common[2], "' (", most_v[2], ").")
          
        }
        
        if (most_common[1] != most_common[3]) {
          
          sentence <- paste0(sentence, 
                             " For male students, the most common response instead was '", most_common[3], "' (", most_v[3], ").")
          
        }
        
      }
      
    }
    
    
  } else { #run the following instead if it's a multi-check question
    
    # for these questions we summarise either one or a vector of responses of interest 
    #(for example, for diet we are interested in whether they say "on most days" for water, and "rarely" for sweets.)
    
    reps <- chk_var
    data <- dataset[dataset$school == sch, ]
    
    vector_interest <- ifelse(length(value_of_interest) > 1, T, F) # if there are multiple responses of interest, T
    q_binary <- ifelse(length(unique(dataset$response)) == 2, T, F) # if the responses is just Yes or No, T
    
    if (q_binary == T) { reps <- reps[reps %in% unique(data$question[data$count != 0 & 
                                                                       data$response %in% value_of_interest])] }
    
    t <- data %>% 
      filter(breakdown %in% c("Female", "Male", "Other"), !is.na(question_text)) %>% 
      group_by(breakdown) %>% 
      summarise(total = max(denominator))
    
    total_resp <- sum(t$total)
    max_resp <- max(full_data$denominator[full_data$school == sch])
    
    if ("Female" %in% unique(data$breakdown) & "Male" %in% unique(data$breakdown)) {
      
      f_count <- max(data$denominator[data$breakdown == "Female"], na.rm = T)
      m_count <- max(data$denominator[data$breakdown == "Male"], na.rm = T)
      
      t <- data %>% 
        filter(breakdown %in% c("Female", "Male", "Other"), !is.na(question_text)) %>% 
        group_by(breakdown) %>% 
        summarise(total = max(denominator))
      
      total_resp <- sum(t$total)
      o_count <- total_resp - (f_count + m_count)
      
    }
    
    perc <- round(total_resp / max_resp * 100, 1)
    percent_cens <- perc #ifelse(perc >= 95, ">=95%", ifelse(perc <= 5, "<=5%", perc))
    
    total_resp <- ifelse(total_resp == max_resp, "every student", 
                         paste0(total_resp, " students", " (", percent_cens, "%)"))
    
    # if not everyone has responded, add sex breakdown. 
    
    add_sex_breakdown <- ""
    
    if ("Female" %in% unique(data$breakdown) & "Male" %in% unique(data$breakdown)) {
      
      add_sex_breakdown <- ifelse(total_resp != "every student", 
                                  paste0(" Of these, ", f_count, " were female, ", m_count,
                                         " were male, and ", o_count, " either preferred not to say, or stated 'Other' for sex. "), "")
      
      # sometimes due to low response rate for each sex, NAs appear due to frequent rounding to 0. If this occurs, omit breakdown.
      if (is.na(f_count) | is.na(m_count)) { add_sex_breakdown <- ""}
      
    }
    
    
    if (is.na(total_resp)) { 
      
      return(paste0("Within the school, no students responded to this question."))
      
    } else {
      
      # Start main sentence. 
      sentence <- paste0("Within the school, ", total_resp, " responded to this question." , add_sex_breakdown, "<br> <br>")
      
    }
    
    if (vector_interest == T) { reps <- na.omit(reps[!is.na(value_of_interest)]) } # if there is NA in vector of variables of interest, remove the choice from the loop. 
    
    temp <- " "
    
    if (length(reps) > 1) { #reorder by the most to least number of responses
      
      if (q_binary == T) {
        
        t <- data %>%  
          filter(question %in% reps, response == "Yes", breakdown == "All Responses") %>% 
          arrange(desc(value)) %>% 
          distinct()
        
        reps <- t$question
        
      } 
      
      for (q in 1:length(reps)) {
        
        # generate the values used for the sentences. 
        
        df <- data[data$breakdown == "All Responses" & data$question == reps[q], ] %>% 
          left_join(q_coded, by = c("question" = "question_coded")) %>% 
          drop_na(reworded)
        
        if (vector_interest == T) {
          
          df <- filter(df, response == na.omit(value_of_interest)[q])
          
        } else {
          
          df <- filter(df, response == value_of_interest)
          
          df <- mutate(df, question_text.x = menu_text)
          
        }
        
        # generate main sentence for All Responses
        
        if (q_binary == F) {
          
          temp <- paste0(temp, "When asked about ", df$question_text.x, ", ", df$count, " students answered with '", df$response, "'. ")
          
        } else if (q_binary == T & q != length(reps) & df$count > 0) {
          
          posneg_starter <- ifelse(df$response == "Yes", paste0(df$question_text.x),
                                   paste0("not ", df$question_text.x))
          
          if (q_binary == T) { df <- filter(df, response == value_of_interest) }
          
          temp <- paste0(temp, df$count, " students selected '",
                         posneg_starter, "', ")  
          
        } else if (q_binary == T & q == length(reps) & df$count > 0) {
          
          posneg_starter <- ifelse(df$response == "Yes", df$question_text.x,
                                   paste0("not ", df$question_text.x))
          
          if (q_binary == T) { df <- filter(df, response == value_of_interest) }
          
          temp <- paste0(temp, "and ", df$count, " students selected '",
                         posneg_starter, "'. ")  
          
        }
        
      }
      
    } else if (length(reps) == 1) {
      
      df <- data[data$breakdown == "All Responses" & data$question == reps[1], ] %>% 
        left_join(q_coded, by = c("question" = "question_coded")) %>% 
        drop_na(reworded) 
      
      if (q_binary == T) { df <- filter(df, response == value_of_interest) }
      
      posneg_starter <- ifelse(df$response == "Yes", paste0(df$question_text.x),
                               paste0("not ", df$question_text.x))
      
      temp <- paste0(temp, df$count, " students selected '",
                     posneg_starter, "', ")  
      
    }
    
    sentence <- paste0(sentence, temp)
    
  }
  
  # Add a short prompt(?) sentence.
  
  sentence <- paste0(sentence, " For more detail, please see the Graph or Table tabs.")
  
  # Add significant difference count sentences if they exist.
  
  keep_these <- q_coded$question_coded[!is.na(q_coded$reworded)]
  
  df <- data
  
  if (unique(df$question) %in% keep_these) {
    
    diffs_sch <- nrow(filter(create_bullets(chk_diff_sch, "between schools")$Data, `Differences found` != "None"))
    diffs_yr <- nrow(filter(create_bullets(chk_diff_yr, "between year groups")$Data, `Differences found` != "None"))
    
  } else {
    
    return(sentence)
    
  }
  
  # if its a yes or no question the "no" row is omitted so if this is the case, omit 1 from the count. 
  if (q_binary == T) { 
    
    diffs_sch <- nrow(filter(create_bullets(chk_diff_sch, "between schools")$Data, response == "Yes"))
    diffs_yr <- nrow(filter(create_bullets(chk_diff_yr, "between year groups")$Data, `Differences found` != "None"))
    
  }
  
  if (diffs_sch > 0) {
    
    sentence <- paste0(sentence, "<br> <br>", "Of the response options given to the students there were ",
                       diffs_sch, " significant difference(s) when comparing between responses from ", sch, 
                       " and responses from all schools.")
    
    if(diffs_yr > 0) {
      
      sentence <- paste0(sentence, " Additionally, there were ", diffs_yr, 
                         " significant difference(s) when comparing between individual year groups at ", sch, ".")
      
    }
    
  } else if (diffs_yr > 0) {
    
    sentence <- paste0(sentence, "<br> <br>", "Of the response options given to the students there were ", diffs_yr, 
                       " significant difference(s) when comparing between individual year groups at ", sch, ".")
    
  } else {
    
    sentence <- paste0(sentence, "<br> <br> Students from ", sch, " were found to be statistically similar to other schools. ")
    
  }
  
  return(sentence)
  
}

# Overview report ---------------------------------------------------------

#'@export

create_sum_sentence_herts <- function(dataset = chk_stats, 
                                      multi = F, 
                                      value_of_interest = F, 
                                      full_data = stats,
                                      diffs = chk_diff_sch) {
  
  if (nrow(dataset) == 0) { return ("") }
  
  if (multi == F) { # run the following if it's a simple one-choice question
    
    q_binary <- F
    
    data <- dataset
    # generate the values used for the sentences. 
    
    df <- dataset[dataset$breakdown == "All Responses" & dataset$school == "All Schools", ]
    
    most_common <- df$response[df$count == max(df$count) & !is.na(df$question_text)]
    most_v <- df$value_cens[df$count == max(df$count) & !is.na(df$question_text)]
    least_common <- df$response[df$count == min(df$count[df$count >= 0 & !is.na(df$question_text)])]
    least_v <- df$value_cens[df$count == min(df$count[df$count >= 0 & !is.na(df$question_text)])][1]
    
    if ("Female" %in% unique(data$breakdown) & "Male" %in% unique(data$breakdown)) {
      
      df <- dataset[dataset$breakdown == "Female" & dataset$school == "All Schools", ]
      
      most_common[2] <- df$response[df$count == max(df$count) & !is.na(df$question_text)]
      most_v[2] <- df$value_cens[df$count == max(df$count) & !is.na(df$question_text)]
      least_common[2] <- df$response[df$count == min(df$count[df$count >= 0 & !is.na(df$question_text)])]
      least_v[2] <- df$value_cens[df$count == min(df$count[df$count >= 0 & !is.na(df$question_text)])]
      f_count <- df$denominator[!is.na(df$question_text) & !is.na(df$question_text)][1]
      
      df <- dataset[dataset$breakdown == "Male" & dataset$school == "All Schools", ]
      
      most_common[3] <- df$response[df$count == max(df$count) & !is.na(df$question_text)]
      most_v[3] <- df$value_cens[df$count == max(df$count) & !is.na(df$question_text)]
      least_common[3] <- df$response[df$count == min(df$count[df$count >= 0 & !is.na(df$question_text)])]
      least_v[3] <- df$value_cens[df$count == min(df$count[df$count >= 0 & !is.na(df$question_text)])]
      m_count <- df$denominator[!is.na(df$question_text)][1]
      
      total_resp <- sum(dataset$count[dataset$breakdown %in% c("Female", "Male", "Other") &
                                        !is.na(dataset$question_text) &
                                        dataset$school == "All Schools"])
      o_count <- total_resp - (f_count + m_count) 
      
    }
    
    # If all students responded to this question, skip the sex breakdown. Include if not. 
    
    total_resp <- sum(dataset$count[dataset$breakdown %in% c("Female", "Male", "Other") &
                                      !is.na(dataset$question_text) &
                                      dataset$school == "All Schools"])
    max_resp <- max(full_data$denominator)
    
    perc <- round(total_resp / max_resp * 100, 1)
    percent_cens <- perc #ifelse(perc >= 95, ">=95%", ifelse(perc <= 5, "<=5%", perc))
    
    total_resp <- ifelse(total_resp == max_resp, "every student", 
                         paste0(total_resp, " students", " (", percent_cens, "%)"))
    
    add_sex_breakdown <- ""
    
    if ("Female" %in% unique(data$breakdown) & "Male" %in% unique(data$breakdown)) {
      
      add_sex_breakdown <- ifelse(total_resp != "every student", 
                                  paste0(" Of these, ", f_count, " were female, ", m_count,
                                         " were male, and ", o_count, " stated 'Other' for sex. "), "")
      
      # sometimes due to low response rate for each sex, NAs appear due to frequent rounding to 0. If this occurs, omit breakdown.
      if (is.na(f_count) | is.na(m_count)) { add_sex_breakdown <- ""}
      
    }
    
    # generate main sentence for All Responses
    
    if (is.na(total_resp)) { 
      
      return(paste0("Within the school, no students responded to this question."))
      
    } else {
      
      sentence <- paste0("Within Hertfordshire, ", total_resp, " responded to this question." , add_sex_breakdown,
                         "<br> <br> The most common response for all respondents was '", most_common[1], "', which made up ", most_v[1], 
                         " of responses and the least common response was '", least_common[1], "', with ",
                         least_v[1], " of responses.")
      
    }
    
    # generate additional sentences if male or female values for most common values differ
    if (max(data$denominator[data$breakdown == "Female" & data$school == "All Schools"])[1] != 0 &
        max(data$denominator[data$breakdown == "Male" & data$school == "All Schools"])[1] != 0 &
        "Female" %in% unique(data$breakdown) & "Male" %in% unique(data$breakdown)) {
      
      if (most_common[1] != most_common[2]) {
        
        sentence <- paste0(sentence, 
                           " For female students, the most common response instead was '", most_common[2], "' (", most_v[2], ").")
        
      }
      
      if (most_common[1] != most_common[3]) {
        
        sentence <- paste0(sentence, 
                           " For male students, the most common response instead was '", most_common[3], "' (", most_v[3], ").")
        
      }
      
    }
    
    # Repeat same process for ethnic groups if there are at least 2 unique ethnic groups that responded. 
    
    if (sum(unique(data$breakdown[data$denominator > 0]) %in% plot_ethn_grp) > 2) {
      
      e_sentence <- NA
      
      loop_grps <- unique(data$breakdown[data$denominator > 0 & data$breakdown %in% plot_ethn_grp])
      
      spot_commons <- data %>% 
        filter(denominator > 0 , breakdown %in% loop_grps) %>% 
        group_by(breakdown) %>%
        filter(count == max(count)) %>% 
        ungroup() %>% 
        select(breakdown, response, count, value) 
      
      spot_commons$unique <- ifelse(spot_commons$response != 
                                      spot_commons$response[spot_commons$breakdown == "All Responses"], 1, 0)
      
      loop <- ifelse(sum(spot_commons$unique) > 0, T, F)
      
      # get all ethnicity groups that have a different common response. 
      loop_grps <- unique(as.character(spot_commons$breakdown[spot_commons$unique == 1]))
      
      # loop to generate sentence for each ethnic group in loop_grps
      if (length(loop_grps) > 0) {
        
        for (e in 1:length(loop_grps)) {
          
          current <- as.character(loop_grps[e])
          
          df <- dataset[dataset$breakdown == current & dataset$school == "All Schools", ] %>% 
            filter(count == max(.$count))
          
          if (current %in% c("Any other ethnic group", "Mixed")) { 
            
            current <- paste0("respondents that selected '", current, "' when asked for their ethnicity")
            
            t_sentence <- c(glue::glue(" The most common response for {current} was '{df$response}' ({df$value})."))
            
            if (length(t_sentence) > 1) { 
              
              t_sentence <- paste(c(gsub(").", ") ", t_sentence[1]), 
                                    gsub(paste0("The most common response for respondents that selected '", loop_grps[e], 
                                                "' when asked for their ethnicity was "), "", 
                                         t_sentence[2:length(t_sentence)])), collapse = "and")  
              
            }
            
          } else {
            
            current <- paste(current, "respondents")
            
            t_sentence <- c(glue::glue(" The most common response for {current} was '{df$response}' ({df$value})."))
            
            if (length(t_sentence) > 1) { 
              
              t_sentence <- paste(c(gsub(").", ") ", t_sentence[1]), 
                                    gsub(paste0("The most common response for ", loop_grps[e], " respondents was "), "", 
                                         t_sentence[2:length(t_sentence)])), collapse = "and")  
              
            }
            
          }
          
          e_sentence[e] <- t_sentence
          
        }
        
        # most_common <- spot_commons$response[spot_commons$breakdown == "All Responses"]
        
        sentence <- paste0(sentence, paste(e_sentence, collapse = " "))
        
      }
      
    }
    
    
  } else { #run the following instead if it's a multi-check question
    
    # for these questions we summarise either one or a vector of responses of interest 
    #(for example, for diet we are interested in whether they say "on most days" for water, and "rarely" for sweets.)
    
    reps <- chk_var
    data <- filter(dataset, !is.na(question_text))
    
    vector_interest <- ifelse(length(value_of_interest) > 1, T, F) # if there are multiple responses of interest, T
    q_binary <- ifelse(length(unique(dataset$response)) == 2, T, F) # if the responses is just Yes or No, T
    
    if (q_binary == T) { reps <- reps[reps %in% unique(data$question[data$count != 0 & 
                                                                       data$response %in% value_of_interest])] }
    
    t <- data %>% 
      filter(breakdown %in% c("Female", "Male", "Other"), !is.na(question_text)) %>% 
      group_by(breakdown) %>% 
      summarise(total = max(denominator))
    
    total_resp <- sum(t$total)
    max_resp <- max(full_data$denominator)
    
    if ("Female" %in% unique(data$breakdown) & "Male" %in% unique(data$breakdown)) {
      
      f_count <- max(data$denominator[data$breakdown == "Female"], na.rm = T)
      m_count <- max(data$denominator[data$breakdown == "Male"], na.rm = T)
      
      t <- data %>% 
        filter(breakdown %in% c("Female", "Male", "Other"), !is.na(question_text)) %>% 
        group_by(breakdown) %>% 
        summarise(total = max(denominator))
      
      total_resp <- sum(t$total)
      o_count <- total_resp - (f_count + m_count)
      
    }
    
    perc <- round(total_resp / max_resp * 100, 1)
    percent_cens <- perc #ifelse(perc >= 95, ">=95%", ifelse(perc <= 5, "<=5%", perc))
    
    total_resp <- ifelse(total_resp == max_resp, "every student", 
                         paste0(total_resp, " students", " (", percent_cens, "%)"))
    
    # if not everyone has responded, add sex breakdown. 
    
    add_sex_breakdown <- ""
    
    if ("Female" %in% unique(data$breakdown) & "Male" %in% unique(data$breakdown)) {
      
      add_sex_breakdown <- ifelse(total_resp != "every student", 
                                  paste0(" Of these, ", f_count, " were female, ", m_count,
                                         " were male, and ", o_count, " either preferred not to say, or stated 'Other' for sex. "), "")
      
      # sometimes due to low response rate for each sex, NAs appear due to frequent rounding to 0. If this occurs, omit breakdown.
      if (is.na(f_count) | is.na(m_count)) { add_sex_breakdown <- ""}
      
    }
    
    
    if (is.na(total_resp)) { 
      
      return(paste0("Within the school, no students responded to this question."))
      
    } else {
      
      # Start main sentence. 
      sentence <- paste0("Within Hertfordshire, ", total_resp, " responded to this question." , add_sex_breakdown, "<br> <br>")
      
    }
    
    if (vector_interest == T) { reps <- na.omit(reps[!is.na(value_of_interest)]) } # if there is NA in vector of variables of interest, remove the choice from the loop. 
    
    temp <- " "
    
    if (length(reps) > 1) { #reorder by the most to least number of responses
      
      if (q_binary == T) {
        
        t <- data %>%  
          filter(question %in% reps, response == "Yes", breakdown == "All Responses") %>% 
          arrange(desc(value)) %>% 
          distinct()
        
        reps <- t$question
        
      } 
      
      for (q in 1:length(reps)) {
        
        # generate the values used for the sentences. 
        
        df <- data[data$breakdown == "All Responses" & data$question == reps[q], ] %>% 
          left_join(q_coded, by = c("question" = "question_coded")) %>% 
          drop_na(reworded)
        
        if (vector_interest == T) {
          
          df <- filter(df, response == na.omit(value_of_interest)[q])
          
        } else {
          
          df <- filter(df, response == value_of_interest)
          
          df <- mutate(df, question_text.x = menu_text)
          
        }
        
        # generate main sentence for All Responses
        
        if (q_binary == F) {
          
          temp <- paste0(temp, "When asked about ", df$question_text.x, ", ", df$count, " students answered with '", df$response, "'. ")
          
        } else if (q_binary == T & q != length(reps) & df$count > 0) {
          
          posneg_starter <- ifelse(df$response == "Yes", paste0(df$question_text.x),
                                   paste0("not ", df$question_text.x))
          
          if (q_binary == T) { df <- filter(df, response == value_of_interest) }
          
          temp <- paste0(temp, df$count, " students selected '",
                         posneg_starter, "', ")  
          
        } else if (q_binary == T & q == length(reps) & df$count > 0) {
          
          posneg_starter <- ifelse(df$response == "Yes", df$question_text.x,
                                   paste0("not ", df$question_text.x))
          
          if (q_binary == T) { df <- filter(df, response == value_of_interest) }
          
          temp <- paste0(temp, "and ", df$count, " students selected '",
                         posneg_starter, "'. ")  
          
        }
        
      }
      
    } else if (length(reps) == 1) {
      
      df <- data[data$breakdown == "All Responses" & data$question == reps[1], ] %>% 
        left_join(q_coded, by = c("question" = "question_coded")) %>% 
        drop_na(reworded) 
      
      if (q_binary == T) { df <- filter(df, response == value_of_interest) }
      
      posneg_starter <- ifelse(df$response == "Yes", paste0(df$question_text.x),
                               paste0("not ", df$question_text.x))
      
      temp <- paste0(temp, df$count, " students selected '",
                     posneg_starter, "', ")  
      
    }
    
    sentence <- paste0(sentence, temp)
    
  }
  
  # Add a short prompt(?) sentence.
  
  sentence <- paste0(sentence, " For more detail, please see the Graph or Table tabs.")
  
  # Add significant difference count sentences if they exist.
  
  keep_these <- q_coded$question_coded[!is.na(q_coded$reworded)]
  
  df <- data
  
  if (unique(df$question) %in% keep_these) {
    
    diffs_yr <- nrow(filter(create_bullets(chk_diff_yr, "between year groups")$Data, `Differences found` != "None"))
    diffs_sex <- nrow(filter(create_bullets(chk_diff_sex, "between female and male young people")$Data, `Differences found` != "None"))
    diffs_ethn <- nrow(filter(create_bullets(chk_diff_ethn, "ethnicity")$Data, `Differences found` != "None"))
    diffs_dis <- nrow(filter(create_bullets(chk_diff_dis, "District")$Data, `Differences found` != "None"))
    
    diffs_all <- sum(diffs_yr, diffs_sex, diffs_ethn, diffs_dis)
    
    diffs_table <- c(diffs_yr, diffs_sex, diffs_ethn, diffs_dis)
    names(diffs_table) <- c("year groups", "sexes", "ethnic groups", "districts")
    loop_grp <- names(diffs_table)[diffs_table > 0]
    
  } else {
    
    return(sentence)
    
  }
  
  # if its a yes or no question the "no" row is omitted so if this is the case, omit 1 from the count. 
  # if (q_binary == T) { 
  #   
  #   #diffs_sch <- nrow(filter(create_bullets(chk_diff_sch, "between schools")$Data, response == "Yes"))
  #   diffs_yr <- nrow(filter(create_bullets(chk_diff_yr, "between year groups")$Data, `Differences found` != "None"))
  #   
  # }
  
  if (diffs_all > 0) {
    
    sentence <- paste0(sentence, "<br> <br> Overall there were ", diffs_all, " significant difference(s) found between groups. ")
    
    for (s in loop_grp) {
      
      current <- which(names(diffs_table) == s)
      sentence <- paste0(sentence, "<br> <br> There were ", diffs_table[current], 
                         " significant difference(s) found between ", names(diffs_table)[current], ". ")
      
    }
    
    sentence <- paste0(sentence, "<br><br>For more detail, please see the Comparisons tab to see the full breakdown. ")
    
  } else {
    
    sentence <- paste0(sentence, "<br> <br> No statistical differences between groups were found for this question.")
    
  }
  
  return(sentence)
  
}


