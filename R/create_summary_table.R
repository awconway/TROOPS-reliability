#' Create a summary table of participant characteristics
#' @importFrom gtsummary tbl_summary
#' @importFrom dplyr select
#' @importFrom stringr str_detect
#' @param baseline_data dataframe
#' @export
create_summary_table <- function(baseline_data) {
  baseline_data %>%
    mutate(
      `Procedure duration` = ProcEnd - ProcStart,
      Procedure = case_when(
        str_detect(Procedure, "ICD") ~
        "Implantable Cardioverter Defibrillator",
        str_detect(Procedure, "PPM") ~
        "Permanent pacemaker",
        str_detect(Procedure, "PCI") ~
        "Coronary angiography or intervention",
        str_detect(Procedure, "angiogram") ~
        "Coronary angiography or intervention",
        str_detect(Procedure, "AF ablation") ~
        "Atrial fibrillation ablation",
        str_detect(Procedure, "VT") ~
        "Ventricular tachycardia ablation",
        TRUE ~ "Electrophysiology study and radiofrequency ablation"
      ),
      `BaselineOxygen (L/min)` = case_when(
        `BaselineOxygen (L/min)` < 6 ~ "< 6",
        `BaselineOxygen (L/min)` > 6 ~ "> 6",
        TRUE ~ "6"
      ),
      Sex = ifelse(Sex == "F", "Female", "Male")
    ) %>%
    select(
      Age,
      Sex,
      `Fentanyl (mcg)`,
      `Midazolam (mg)`,
      Procedure,
      `Procedure duration`,
      `BaselineOxygen (L/min)`
    ) %>%
    tbl_summary(
      type = c(`Fentanyl (mcg)`) ~ "continuous",
      missing = "no"
    )
}