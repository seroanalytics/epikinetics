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
           time_since_last_exp       me        lo       hi titre_type
                         <int>    <num>     <num>    <num>     <char>
        1:                   0 121.1892  94.45425 154.0367      Alpha
        2:                   1 150.7446 121.39549 186.0631      Alpha
        3:                   2 188.0531 155.65597 228.6746      Alpha
        4:                   3 233.8627 196.06447 281.9184      Alpha
        5:                   4 291.1750 244.87488 347.7999      Alpha
       ---                                                           
      902:                 146 162.4485 128.80162 200.2273      Delta
      903:                 147 161.7977 128.30153 199.5112      Delta
      904:                 148 161.1543 127.80338 198.8686      Delta
      905:                 149 160.6696 127.30716 198.2282      Delta
      906:                 150 159.9921 126.81288 197.5898      Delta
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
            calendar_day titre_type         me        lo        hi time_shift
                  <IDat>     <char>      <num>     <num>     <num>      <num>
         1:   2021-03-08      Alpha 1179.41596 898.59408 1592.5041          0
         2:   2021-03-09      Alpha 1158.69205 865.99919 1558.7359          0
         3:   2021-03-10      Alpha 1208.48440 953.30901 1520.2822          0
         4:   2021-03-11      Alpha 1154.03970 900.55636 1490.7134          0
         5:   2021-03-12      Alpha 1166.98651 885.90001 1506.8642          0
        ---                                                                  
      1775:   2022-08-07      Delta   82.89897  31.27422  308.8199          0
      1776:   2022-08-08      Delta   84.04324  30.86336  312.8336          0
      1777:   2022-08-09      Delta   84.28422  30.20377  319.3140          0
      1778:   2022-08-10      Delta   86.52412  29.56398  320.8826          0
      1779:   2022-08-11      Delta   86.69664  31.20098  317.9618          0

