# Run pairwise bootstrap comparisons

    Code
      bootstrap_summary
    Output
      # A tibble: 10 x 8
         species1 species2 prop_1_gt_2 prop_1_lt_2 ci_lower ci_upper   p_value diffs 
         <chr>    <chr>          <dbl>       <dbl>    <dbl>    <dbl>     <dbl> <list>
       1 cebe     heca               1           0   1.93     3.45   1.87e-301 <dbl> 
       2 cebe     quag               1           0   0.739    0.901  1.87e-301 <dbl> 
       3 cebe     laca               1           0   1.86     3.38   1.87e-301 <dbl> 
       4 cebe     hear               1           0   0.792    1.31   1.87e-301 <dbl> 
       5 heca     quag               0           1  -2.56    -1.18   1.87e-301 <dbl> 
       6 heca     laca               0           1  -0.0767  -0.0704 1.87e-301 <dbl> 
       7 heca     hear               0           1  -2.14    -1.14   1.87e-301 <dbl> 
       8 quag     laca               1           0   1.11     2.49   1.87e-301 <dbl> 
       9 quag     hear               1           0   0.0476   0.418  1.87e-301 <dbl> 
      10 laca     hear               0           1  -2.07    -1.06   1.87e-301 <dbl> 

