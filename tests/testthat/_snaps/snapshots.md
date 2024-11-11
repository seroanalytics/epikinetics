# Model fits are the same

    Code
      delta
    Output
              variable     mean   median    sd   mad       q5      q95 rhat ess_bulk
       lp__            -2109.08 -2109.36 46.47 43.82 -2188.94 -2029.83 1.04       77
       t0_pop[1]           4.26     4.26  0.28  0.30     3.81     4.74 1.00      350
       t0_pop[2]           2.53     2.52  0.31  0.33     2.02     3.03 1.01      344
       t0_pop[3]           1.47     1.48  0.32  0.35     0.94     2.02 1.00      351
       tp_pop[1]           8.66     8.65  0.62  0.60     7.70     9.66 1.01      367
       tp_pop[2]           9.14     9.13  0.65  0.63     8.12    10.20 1.00      286
       tp_pop[3]           9.38     9.36  0.70  0.72     8.32    10.56 1.00      319
       ts_pop_delta[1]    59.90    59.89  2.46  2.29    56.07    64.51 1.01      403
       ts_pop_delta[2]    48.89    48.89  3.13  3.20    43.53    54.03 1.00      329
       ts_pop_delta[3]    43.53    43.59  2.87  2.90    38.78    48.10 1.01      196
       ess_tail
            168
            317
            301
            397
            414
            286
            362
            375
            371
            262
      
       # showing 10 of 10103 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)

# Population trajectories are the same

    Code
      trajectories
    Output
           time_since_last_exp        me        lo       hi titre_type
                         <int>     <num>     <num>    <num>     <char>
        1:                   0  85.50469  61.58522 114.4462  Ancestral
        2:                   1 117.88403  89.33763 155.4831  Ancestral
        3:                   2 161.88338 125.71051 211.4578  Ancestral
        4:                   3 221.63832 177.37863 287.5669  Ancestral
        5:                   4 304.82979 242.45015 390.8075  Ancestral
       ---                                                            
      902:                 146 114.71590  82.31486 153.4804      Delta
      903:                 147 114.07370  81.57447 152.6794      Delta
      904:                 148 113.43508  80.95385 152.0921      Delta
      905:                 149 112.89102  80.55711 151.5578      Delta
      906:                 150 112.35549  80.06288 151.0253      Delta
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
            calendar_day titre_type        me        lo       hi time_shift
                  <IDat>     <char>     <num>     <num>    <num>      <num>
         1:   2021-03-08  Ancestral 1130.6519 886.46528 1515.564          0
         2:   2021-03-09  Ancestral 1111.4907 870.98406 1445.181          0
         3:   2021-03-10  Ancestral 1162.5233 910.19435 1502.363          0
         4:   2021-03-11  Ancestral 1096.5356 863.12051 1428.106          0
         5:   2021-03-12  Ancestral 1092.3590 841.37625 1455.778          0
        ---                                                                
      1775:   2022-08-07      Delta  327.5756  43.51063 3757.989          0
      1776:   2022-08-08      Delta  332.4900  44.09745 3769.825          0
      1777:   2022-08-09      Delta  328.4756  44.39750 3789.218          0
      1778:   2022-08-10      Delta  330.0316  44.69789 3862.426          0
      1779:   2022-08-11      Delta  337.2001  42.71692 3877.397          0

