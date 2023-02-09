
# Requirement: disable the "unique ID" in input.R BEFORE running this script, otherwise it won't work properly

moved <- data.raw %>%
  select(
    id,
    FollowUpPeriod,
    starts_with("Zip"),
  ) %>%
  pivot_wider(id_cols = c(id, ZipInj, ZipDis), names_from = FollowUpPeriod, values_from = ZipF, names_prefix = "ZipF_") %>%
  group_by(id) %>%
  mutate(
    # Moved_Dis = ZipDis != ZipInj,
    # Moved_Inj = ZipF != ZipInj,
    Moved_n = sum(c(
      ZipInj != ZipDis,
      ZipDis != ZipF_1,
      ZipF_1 != ZipF_2,
      ZipF_2 != ZipF_5,
      ZipF_5 != ZipF_10
    ), na.rm = TRUE),
  ) %>%
  ungroup()

moved

write_csv(moved, "dataset/moved.csv")
