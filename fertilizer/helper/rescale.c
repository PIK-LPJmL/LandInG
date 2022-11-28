/*******************************************************************************
 * Copyright (C) 2022 Potsdam Institute for Climate Impact Research (PIK),    
 * see COPYRIGHT file.
 *
 * This file is part of LandInG and licensed under GNU AGPL Version 3 or
 * later. See LICENSE file or go to http://www.gnu.org/licenses/
 * Contact: https://github.com/PIK-LPJmL/LandInG/
 ******************************************************************************/

/******************************************************************************
 * Functions to aggregate array data using sum or mean aggregation.
 * To compile rescale.c as an R library on Linux:
 * R CMD SHLIB --preclean rescale.c -lm
 * Please confirm settings for other operating systems.
 * Then use dyn.load() within R to load library
 ******************************************************************************/
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#define EQUAL(a, b) (fabs((a) - (b)) < 0.0000001)

/* Aggregation using sum */
void aggregate_array(const double *indata, /* data field */
                     const int *ncols,     /* number of columns (first dimension) in indata */
                     const int *nrows,     /* number of rows (second dimension) in indata */
                     const int *nyears,    /* number of years/levels (third dimension) in indata */
                     const int *fact_x,    /* aggregation factor in X direction (cols) */
                     const int *fact_y,    /* aggregation factor in Y direction (rows) */
                     const double *NAval,  /* NA value in data */
                     double *aggregated,   /* return variable (must be an array with dimensions ncols/fact_x, nrows/fact_y, nyears */
                     int *error,           /* error code */
                     int *verbose          /* whether to print non-error messages */
                    )
{
  
  int c, c2, r, r2, y, count;
  int nc, nr, ny, nc2, nr2, f_x, f_y;
  double aggregated_val;
  
  nc = *ncols;
  nr = *nrows;
  ny = *nyears;
  f_x = *fact_x;
  f_y = *fact_y;
  nc2 = nc/f_x;
  nr2 = nr/f_y;
  
  if (nc < f_x | nr < f_y | ny < 1) {
    printf("Error in aggregate_array: invalid dimensions %d %d %d\n",
           nc, nc, ny);
    *error = 1;
    return;
  }
  if (f_x < 2 && f_y < 2) {
    printf("Error in aggregate_array: either fact_x or fact_y must be larger than 1: %d %d\n",
           f_x, f_y);
    *error = 2;
    return;
  }
  
  if (*verbose)
    printf("Indata: %d cols, %d rows, %d years/levels, aggregated by %d in X, %d in Y, output: %d, %d, %d\n",
           nc, nr, ny, f_x, f_y, nc2, nr2, ny);
  
  for (y = 0; y < ny; y++) {
    for (r = 0; r < nr; r += f_y) {
      for (c = 0; c < nc; c += f_x) {
        aggregated_val = 0.0;
        count = 0;
        for (r2 = r; r2 < (r + f_y); r2++) {
          for (c2 = c; c2 < (c + f_x); c2++) {
            if (!EQUAL(indata[y * nr * nc + r2 * nc + c2], *NAval)) {
              aggregated_val += indata[y * nr * nc + r2 * nc + c2];
              count++;
            }
          }
        }
        if (count > 0) {
          aggregated[y * nr2 * nc2 + r/f_y * nc2 + c/f_x] = aggregated_val;
        } else {
          aggregated[y * nr2 * nc2 + r/f_y * nc2 + c/f_x] = *NAval;
        }
      }
    }
  }
}

/* Aggregation using mean */
void average_array(const double *indata, /* data field */ 
                   const int *ncols,     /* number of columns (first dimension) in indata */
                   const int *nrows,     /* number of rows (second dimension) in indata */
                   const int *nyears,    /* number of years/levels (third dimension) in indata */
                   const int *fact_x,    /* aggregation factor in X direction (cols) */
                   const int *fact_y,    /* aggregation factor in Y direction (rows) */
                   const double *NAval,  /* NA value in data */
                   double *aggregated,   /* return variable (must be an array with dimensions ncols/fact_x, nrows/fact_y, nyears */
                   int *error,           /* error code */
                   int *verbose          /* whether to print non-error messages */
                  )
{
  
  int c, c2, r, r2, y, count;
  int nc, nr, ny, nc2, nr2, f_x, f_y;
  double aggregated_val;
  
  nc = *ncols;
  nr = *nrows;
  ny = *nyears;
  f_x = *fact_x;
  f_y = *fact_y;
  nc2 = nc/f_x;
  nr2 = nr/f_y;
  
  
  if (nc < f_x | nr < f_y | ny < 1) {
    printf("Error in average_array: invalid dimensions %d %d %d\n", nc, nc, ny);
    *error = 1;
    return;
  }
  if (f_x < 2 && f_y < 2) {
    printf("Error in average_array: either fact_x or fact_y must be larger than 1: %d %d\n",
           f_x, f_y);
    *error = 2;
    return;
  }
  
  if(*verbose)
    printf("Indata: %d cols, %d rows, %d years/levels, aggregated by %d in X, %d in Y, output: %d, %d, %d\n",
           nc, nr, ny, f_x, f_y, nc2, nr2, ny);
  
  for (y = 0; y < ny; y++) {
    for (r = 0; r < nr; r += f_y) {
      for (c = 0; c < nc; c += f_x) {
        aggregated_val = 0.0;
        count = 0;
        for (r2 = r; r2 < (r + f_y); r2++) {
          for (c2 = c; c2 < (c + f_x); c2++) {
            if (!EQUAL(indata[y * nr * nc + r2 * nc + c2], *NAval)) {
              aggregated_val += indata[y * nr * nc + r2 * nc + c2];
              count++;
            }
          }
        }
        if (count > 0) {
          aggregated[y * nr2 * nc2 + r/f_y * nc2 + c/f_x] = aggregated_val/count;
        } else {
          aggregated[y * nr2 * nc2 + r/f_y * nc2 + c/f_x] = *NAval;
        }
      }
    }
  }
}
