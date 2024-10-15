# Model fits are the same

    Code
      delta
    Output
              variable     mean   median    sd   mad       q5      q95 rhat ess_bulk
       lp__            -1195.29 -1182.41 78.03 45.74 -1340.68 -1107.94 1.07       48
       t0_pop[1]           4.80     4.79  0.24  0.24     4.45     5.21 1.04       97
       t0_pop[2]           4.12     4.11  0.25  0.27     3.75     4.57 1.02      233
       t0_pop[3]           3.52     3.52  0.26  0.27     3.10     3.92 1.02      205
       tp_pop[1]          10.72    10.73  0.60  0.60     9.81    11.68 1.01      304
       tp_pop[2]           9.55     9.55  0.64  0.65     8.61    10.57 1.01      349
       tp_pop[3]           8.88     8.87  0.77  0.80     7.68    10.09 1.02      250
       ts_pop_delta[1]    61.55    61.45  2.61  2.60    57.50    65.70 1.00      203
       ts_pop_delta[2]    52.55    52.44  2.56  2.50    48.64    56.37 1.01      304
       ts_pop_delta[3]    50.10    50.10  2.65  2.51    45.80    54.36 1.02      173
       ess_tail
             20
            351
            248
            359
            415
            412
            203
            239
            330
            341
      
       # showing 10 of 10103 rows (change via 'max_rows' argument or 'cmdstanr_max_rows' option)

# Population trajectories are the same

    Code
      trajectories
    Output
           time_since_last_exp       me       lo       hi titre_type
                         <int>    <num>    <num>    <num>     <char>
        1:                   0 120.1929  93.3773 154.1345  Ancestral
        2:                   1 150.4494 118.7096 186.3689  Ancestral
        3:                   2 187.4234 152.6142 227.3441  Ancestral
        4:                   3 233.3859 192.6505 280.5822  Ancestral
        5:                   4 291.5457 244.1629 346.3841  Ancestral
       ---                                                          
      902:                 146 160.6464 127.8557 205.7500      Delta
      903:                 147 159.9748 127.1635 204.8898      Delta
      904:                 148 159.2377 126.6307 204.0333      Delta
      905:                 149 158.5859 126.1045 203.1803      Delta
      906:                 150 157.9107 125.5806 202.4049      Delta
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
            calendar_day titre_type         me         lo        hi time_shift
                  <IDat>     <char>      <num>      <num>     <num>      <num>
         1:   2021-03-08  Ancestral 1172.55130 909.767640 1501.3053          0
         2:   2021-03-09  Ancestral 1151.63927 882.770140 1488.7275          0
         3:   2021-03-10  Ancestral 1184.73564 910.075137 1566.1617          0
         4:   2021-03-11  Ancestral 1136.15980 888.320759 1485.9868          0
         5:   2021-03-12  Ancestral 1153.10110 875.290893 1506.4791          0
        ---                                                                   
      1775:   2022-08-07      Delta   84.12329  11.738509  387.2564          0
      1776:   2022-08-08      Delta   84.26154  10.517460  382.8336          0
      1777:   2022-08-09      Delta   85.79343  10.647211  386.2053          0
      1778:   2022-08-10      Delta   85.99704   9.774407  385.3051          0
      1779:   2022-08-11      Delta   84.49904  11.370238  395.8927          0

