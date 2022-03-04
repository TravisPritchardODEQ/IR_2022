AU_all <- read.xlsx('Rollups/Rollup outputs/AU_all_rollup.xlsx')


rollup_errors <- AU_all %>%
  group_by(AU_ID,Pollu_ID, wqstd_code, period, DO_Class ) %>%
  #filter(Pollu_ID == 124) %>%
  filter(n() > 1) %>%
  distinct()



#   filter(n_distinct(AU_final_status) > 1)

write.xlsx(rollup_errors, file = 'Rollups/Rollup outputs/rollup_errors.xlsx')
