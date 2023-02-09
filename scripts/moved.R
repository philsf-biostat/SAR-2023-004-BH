

moved <- data.raw %>%
  transmute(
    id,
    FollowUpPeriod,
    across(starts_with("Zip"), as.character),
    Moved_Dis = ZipF != ZipDis,
    Moved_Inj = ZipF != ZipInj,
    )

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

# moved between injury and followup
moved %>%
  filter(Moved_Inj == TRUE) %>%
  group_by(FollowUpPeriod) %>%
  count(Moved_Inj) %>%
  ungroup()

# moved between discharge and followup
moved %>%
  filter(Moved_Dis == TRUE) %>%
  group_by(FollowUpPeriod) %>%
  count(Moved_Dis) %>%
  ungroup()

