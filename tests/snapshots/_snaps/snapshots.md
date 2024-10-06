# Model fits are the same

    Code
      delta
    Output
              variable     mean   median    sd   mad       q5      q95 rhat ess_bulk
       lp__            -1184.11 -1178.54 58.33 56.66 -1277.41 -1101.09 1.09       35
       t0_pop[1]           4.11     4.11  0.27  0.30     3.69     4.56 1.03      155
       t0_pop[2]           4.77     4.77  0.26  0.26     4.33     5.17 1.04       72
       t0_pop[3]           3.50     3.49  0.29  0.30     3.04     3.97 1.01      324
       tp_pop[1]           9.51     9.53  0.70  0.73     8.27    10.57 1.07       40
       tp_pop[2]          10.74    10.74  0.64  0.64     9.71    11.78 1.08       37
       tp_pop[3]           8.84     8.84  0.84  0.88     7.47    10.14 1.08       40
       ts_pop_delta[1]    52.74    52.69  2.75  2.71    48.15    57.29 1.00      494
       ts_pop_delta[2]    61.67    61.58  2.82  2.95    56.79    66.53 1.01      142
       ts_pop_delta[3]    49.86    49.73  2.65  2.68    45.61    54.34 1.01      355
       ess_tail
            103
            289
            220
            412
            340
            307
            227
            416
            370
            348
      
       # showing 10 of 10103 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)

# Population trajectories are the same

    Code
      trajectories
    Output
               t        me        lo       hi titre_type
           <int>     <num>     <num>    <num>     <char>
        1:     0  76.25839  56.51254 102.5247  Ancestral
        2:     1  94.45253  72.00175 121.8491  Ancestral
        3:     2 116.75048  90.41975 144.7452  Ancestral
        4:     3 144.43023 113.33132 180.0799  Ancestral
        5:     4 178.01885 141.05339 224.6319  Ancestral
       ---                                              
      902:   146 163.39995 128.89520 201.2052      Delta
      903:   147 162.78005 128.23676 200.4452      Delta
      904:   148 162.15565 127.58168 199.7415      Delta
      905:   149 161.54259 126.92995 199.2712      Delta
      906:   150 160.95822 126.28154 198.8112      Delta
                           infection_history
                                      <char>
        1:                   Infection naive
        2:                   Infection naive
        3:                   Infection naive
        4:                   Infection naive
        5:                   Infection naive
       ---                                  
      902: Previously infected (Pre-Omicron)
      903: Previously infected (Pre-Omicron)
      904: Previously infected (Pre-Omicron)
      905: Previously infected (Pre-Omicron)
      906: Previously infected (Pre-Omicron)

# Individual trajectories are the same

    Code
      trajectories
    Output
            calendar_date titre_type        me        lo       hi time_shift
                   <IDat>     <char>     <num>     <num>    <num>      <num>
         1:    2021-03-08  Ancestral 543.79451 433.09823 670.4975          0
         2:    2021-03-09  Ancestral 528.88463 429.80874 653.8752          0
         3:    2021-03-10  Ancestral 545.26438 444.31407 665.8502          0
         4:    2021-03-11  Ancestral 522.29561 418.67869 631.7992          0
         5:    2021-03-12  Ancestral 531.74586 423.99750 648.3266          0
        ---                                                                 
      1775:    2022-08-07      Delta  91.77050  31.07070 429.5746          0
      1776:    2022-08-08      Delta  91.18062  31.07408 424.9366          0
      1777:    2022-08-09      Delta  94.16216  31.29551 426.2925          0
      1778:    2022-08-10      Delta  90.74115  30.39902 426.8355          0
      1779:    2022-08-11      Delta  92.97980  28.91787 438.0270          0

