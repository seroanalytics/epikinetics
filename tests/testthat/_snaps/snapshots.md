# Model fits are the same

    Code
      delta
    Output
              variable     mean   median    sd   mad       q5      q95 rhat ess_bulk
       lp__            -1174.70 -1176.14 50.78 53.93 -1251.74 -1090.28 1.08       34
       t0_pop[1]           4.13     4.13  0.28  0.27     3.66     4.55 1.02      201
       t0_pop[2]           4.80     4.83  0.26  0.27     4.37     5.22 1.02      173
       t0_pop[3]           3.52     3.51  0.28  0.27     3.09     3.99 1.02      189
       tp_pop[1]           9.52     9.53  0.65  0.66     8.54    10.56 1.01      200
       tp_pop[2]          10.72    10.74  0.63  0.59     9.68    11.70 1.02      215
       tp_pop[3]           8.91     8.91  0.73  0.75     7.71    10.11 1.00      253
       ts_pop_delta[1]    52.70    52.57  2.56  2.32    48.91    57.15 1.00      349
       ts_pop_delta[2]    61.50    61.35  2.65  2.72    57.32    65.69 1.00      327
       ts_pop_delta[3]    50.15    50.21  2.61  2.66    45.77    54.31 1.00      329
       ess_tail
            111
            340
            301
            264
            247
            199
            368
            403
            360
            331
      
       # showing 10 of 10103 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)

# Population trajectories are the same

    Code
      trajectories
    Output
               t        me        lo        hi titre_type
           <int>     <num>     <num>     <num>     <char>
        1:     0  75.60473  57.18538  98.67432  Ancestral
        2:     1  93.45106  73.95371 119.73536  Ancestral
        3:     2 115.78921  93.98541 144.80589  Ancestral
        4:     3 143.32611 116.92203 178.46907  Ancestral
        5:     4 178.27950 146.07233 220.82064  Ancestral
       ---                                               
      902:   146 162.44845 128.80162 200.22728      Delta
      903:   147 161.79766 128.30153 199.51119      Delta
      904:   148 161.15433 127.80338 198.86864      Delta
      905:   149 160.66958 127.30716 198.22817      Delta
      906:   150 159.99208 126.81288 197.58975      Delta
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
         1:   2021-03-08  Ancestral 537.64994 427.22113 657.8683          0
         2:   2021-03-09  Ancestral 522.51435 421.14547 653.5493          0
         3:   2021-03-10  Ancestral 542.12465 439.50193 668.6812          0
         4:   2021-03-11  Ancestral 522.11912 432.77731 643.5820          0
         5:   2021-03-12  Ancestral 526.98594 421.46465 637.5343          0
        ---                                                                
      1775:   2022-08-07      Delta  84.55922  30.75784 317.8574          0
      1776:   2022-08-08      Delta  86.74586  29.61217 322.3420          0
      1777:   2022-08-09      Delta  86.13082  31.87980 319.2163          0
      1778:   2022-08-10      Delta  84.68752  28.99385 312.2929          0
      1779:   2022-08-11      Delta  84.58911  30.75438 320.1563          0

