#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <k.h>

#include <common.c>
#include <qserver.c>

static
K from_unknown_robject(SEXP object) {
     /* place holder */
     K result;
     int i, n;
     n = LENGTH(object);
     result = ktn(KS,0);
     for(i = 0; i != n; ++i){
	  char *result_i = ss("");
	  js(&result, result_i);
     }
     return result;
}

static
K from_Date_robject(SEXP object) {
     /* Typically 1970-01-01 is date origin in R */
     K result;
     int n, i;
     int object_i;
     double v;
     int offset = 10957; /* magic constant */
     n = LENGTH(object);
     result = ktn(KD,0);
     for(i = 0; i != n; ++i) {
	  v = REAL(object)[i];
	  if( ISNA(v) || ISNAN(v)) {
	       object_i = nf;
	  }
	  else 
	       object_i = (int)(v - offset);
	  ja(&result, &object_i);
     }
     return result;
}

static
K from_factor_robject(SEXP object) {
     /* factors go across as symbols. Missing factors re null symbols */
     K result;
     SEXP levels;
     int n, object_i, i;
     char* result_i;
     n = LENGTH(object);
     levels = GET_LEVELS(object);
     
     result = ktn(KS,0);
     for(i = 0; i != n; ++i){
	  object_i = INTEGER(object)[i];
	  if( object_i == NA_INTEGER) {
	       result_i = "";
	  } else {
	       result_i = (char*)CHAR(STRING_ELT(levels,object_i-1)) ;
	  }
	  js(&result, result_i); 
     }
     return result;
}

static
K from_integer_robject(SEXP object) {
     K result;
     int n, i;
     long object_i;
     n = LENGTH(object);
     result = ktn(KI,0);
     for(i = 0; i != n; ++i) {
	  object_i = INTEGER(object)[i];
	  if(object_i == NA_INTEGER )
	       object_i = ni;
	  ja(&result, &object_i);
     }
     return result;
}

static
K from_double_robject(SEXP object) {
     int n, i;
     K result;
     double object_i;
     n = LENGTH(object);
     result = ktn(KF,0);
     for(i = 0 ; i != n; ++i) {
	  object_i = REAL(object)[i];
	  /* check for nan or missing */
	  if( ISNA(object_i) || ISNAN(object_i) ) {
	       object_i = nf;
	  }
	  else if ( !R_FINITE(object_i)) {
	       if( object_i == R_PosInf )
		    object_i = wf;
	       else
		    object_i = -wf;
	  }
	  ja(&result, &object_i);
     }
     return result;
}

static
K from_char_robject(SEXP object){
     /* NOTE: null chars and zero length chars are different betwen R
      * and kdb+ */
     K result;
     /* In R_ext/Arith.h NA_STRING is a SEXP */
     if( object == NA_STRING ) {
	  result = kp(" ");
     } else {
	  result = kp((char*)CHAR(object));
     }
     return result;
}

static
K from_string_robject(SEXP object) {
     /* a mixed list of characters */
     K result;
     SEXP object_i;
     int n, i;
     n = LENGTH(object);
     result = knk(0,0);
     for(i = 0; i != n; ++i) {
	  object_i = STRING_ELT(object, i);
	  jk(&result, from_char_robject(object_i));
     }
     return result;
}

static
K from_logical_robject(SEXP object) {
     /*    Logical values are sent as 0 ( FALSE ), 1 ( TRUE ) or INT_MIN =
	   -2147483648 ( NA , but only if NAOK is true), */
     K result;
     int n, i;
     char object_i;
     n = LENGTH(object);
     result = ktn(KB,0);
     for(i = 0; i != n; ++i){
	  object_i = (char)INTEGER(object)[i];
	  ja(&result, &object_i);
     }
     return result;
}

static
K from_POSIXct_robject(SEXP object) {
     /* POSIXct to datetime */
     K result;
     int n, i;
     double object_i;
     n = LENGTH(object);
     result = ktn(KZ,0);
     for(i = 0; i != n; ++i) {
	  object_i = REAL(object)[i];
	  if( ISNA(object_i) || ISNAN(object_i))
	       object_i = nf;
	  else {
	       object_i /= 86400;
	       object_i -= 10957;
	  }
	  ja(&result, &object_i);
     }
     return result;
}

SEXP interface(SEXP object, SEXP tbl_name, SEXP conn) {
     SEXP result=R_NilValue, colNames=R_NilValue, col_i=R_NilValue;
     int nc, i, nprotect;
     K kobject=(K)0, kcol_i=(K)0, keys=(K)0, values=(K)0;
     K kreturn = (K)0;
     char qry[2000] = {'\0'};
     
     nprotect = 0;
     result = R_NilValue;
     if( !inherits(object, "data.frame"))
	  error("object must be data frame"); 
     
     // Rprintf("converting data frame to kdb+ table\n");
     /* we should sanitize columne names to make sure they are not kdb
      * keywords */

     /* add option to coy row names */
     PROTECT(colNames = GET_NAMES(object) ); ++nprotect;
     nc = LENGTH(colNames);
     
     keys = ktn(KS, 0);
     values = ktn(0, 0);
     
     for(i = 0 ; i != nc; ++i) {
	  col_i = VECTOR_ELT(object, i);
	  char *kcolname_i = ss((char*)CHAR(STRING_ELT(colNames, i)));
	  
	  /* Rprintf("extracting column %d, name %s, type %s ", */
	  /* 	      i+1, */
	  /* 	      CHAR(STRING_ELT(colNames, i)), */
	  /* 	      get_type_name(TYPEOF(col_i)) */
	  /* 	 ); */
	  
	  switch(TYPEOF(col_i)) {
	       
	  case INTSXP:     /* factors, ordered are integers */
	       if (inherits(col_i, "factor")) {
		    kcol_i = from_factor_robject(col_i);
	       } else {
		    kcol_i = from_integer_robject(col_i);
	       }
	       break;
	       
	  case REALSXP:    /* floats and POSIXct are floats, Dates are
			    * really integral types */
	       if( inherits(col_i, "Date") ) {
		    kcol_i = from_Date_robject(col_i);
	       } else if (inherits(col_i, "POSIXct")) {
		    kcol_i = from_POSIXct_robject(col_i);
	       } else {
		    kcol_i = from_double_robject(col_i);
	       }
	       break;
	       
	  case STRSXP:     /* characters are neither integer or floats */
	       kcol_i = from_string_robject(col_i);
	       break;
	       
	  case LGLSXP:
	       kcol_i = from_logical_robject(col_i);
	       break;
	       
	  default:         /* unknown or uncatered for types */
	       kcol_i = from_unknown_robject(col_i);
	  }
	  
	  /* append to keys and values*/
	  js(&keys, kcolname_i);
	  jk(&values, kcol_i);
     }
     
     UNPROTECT(nprotect);
     
     kobject = xT(xD(keys, values));
     snprintf(qry, 2000, "{`%s set x ; x}", (char*)CHAR(STRING_ELT(tbl_name,0)) ) ;
     kreturn = k( INTEGER(conn)[0], qry, kobject, (K)0);

     if (0 == kreturn) {
	  error("Error: not connected to kdb+ server\n");
     }   else if(-128 == kreturn->t) {
	  error("kdb+ returned: %s", kreturn->s);
     }
     
     r0(kreturn);
     return result;
}
