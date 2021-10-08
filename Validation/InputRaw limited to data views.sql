SELECT *
  FROM [IntegratedReport].[dbo].[InputRaw]
  Where ((dbo.InputRaw.wqstd_code = 12) AND (dbo.InputRaw.Statistical_Base = '7DADM') AND (dbo.InputRaw.FishCode <> 10) AND (dbo.InputRaw.FishCode <> 11) AND (dbo.InputRaw.FishCode <> 99)) -- temperature
    or ( (chr_uid = 549) AND (MLocID IS NOT NULL) AND (wqstd_code = 15)) -- aluminum
    or ( (dbo.InputRaw.Pollu_ID = 6) AND (dbo.InputRaw.wqstd_code = 15)) -- ammonia
    or ((dbo.InputRaw.wqstd_code = 1)) -- bacteria
    or ((dbo.InputRaw.wqstd_code = 17) AND (dbo.InputRaw.Result_Unit <> 'mg/cm2')) -- chl
    or ( (chr_uid = 832) AND (MLocID IS NOT NULL) AND (wqstd_code = 15)) -- copper
    or ((dbo.InputRaw.wqstd_code = 3) AND (NOT (dbo.InputRaw.Statistical_Base = 'Minimum')) OR
                         (dbo.InputRaw.wqstd_code = 3) AND (NOT (dbo.InputRaw.Time_Basis = '30 Day')) OR
                         (dbo.InputRaw.wqstd_code = 3) AND (dbo.InputRaw.Statistical_Base IS NULL)) -- DO
    or ( (dbo.InputRaw.Pollu_ID = 25 OR
                         dbo.InputRaw.Pollu_ID = 42 OR
                         dbo.InputRaw.Pollu_ID = 101 OR
                         dbo.InputRaw.Pollu_ID = 111 OR
                         dbo.InputRaw.Pollu_ID = 130 OR
                         dbo.InputRaw.Pollu_ID = 150) AND (dbo.InputRaw.wqstd_code = 15)) -- metals      
    or ( (Pollu_ID = 123) AND (MonLocType <> 'Estuary') AND (MonLocType <> 'Ocean ') AND (MonLocType <> 'BEACH Program Site-Ocean') AND (wqstd_code = 15))    -- pentachlorophenol
    or ( (dbo.InputRaw.wqstd_code = 10) AND (dbo.InputRaw.Result_Type = 'Actual')) -- pH grab
    or (  (dbo.InputRaw.wqstd_code = 15) AND (dbo.InputRaw.Calc_Crit IS NULL OR
                         dbo.InputRaw.Calc_Crit = 0) AND (dbo.InputRaw.Sample_Fraction <> 'Suspended' OR
                         dbo.InputRaw.Sample_Fraction IS NULL) AND (dbo.InputRaw.Char_Name <> 'Azinphos-methyl oxygen analog')) -- ToxAL
    or (  (dbo.InputRaw.wqstd_code = 16) AND (dbo.InputRaw.AU_ID IS NOT NULL)) -- toxHH
    or (  (dbo.InputRaw.MonLocType = 'Facility Public Water Supply (PWS)') AND (dbo.InputRaw.Char_Name = 'Turbidity')) -- Turbidity
