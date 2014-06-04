#include "opt_oct_hmat.h"

double oct_to_lincons_array_time = 0;
double oct_to_box_time = 0;
double sat_lincons_time = 0;

bool opt_oct_is_bottom(ap_manager_t* man, opt_oct_t* o)
{
  double *m = o->m? o->m : o->closed;
  if(m!=NULL){
  	//print_opt_hmat(m, o->dim);
  	fflush(stdout);
  }
  opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_IS_BOTTOM,0);
  if (pr->funopt->algorithm>=0){ opt_oct_cache_closure(pr,o);}
  m = o->closed ? o->closed : o->m;
  if (o->closed) {
    /* definitively non empty on Q */
    if (num_incomplete || o->intdim) { flag_incomplete; }
    return false;
  }
  else if (!o->m){
    /* definitively empty */
    return true;
  }
  else {
    /* no closure => we don't know */
    flag_algo;
    return false;
  }
}

bool opt_oct_is_top(ap_manager_t* man, opt_oct_t* o)
{
  opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_IS_TOP,0);
  int i,j;
  double* m = o->m ? o->m : o->closed;
  if (!m) return false;
  return is_top_avx_half_double(m,o->dim);
}

bool opt_oct_is_leq(ap_manager_t* man, opt_oct_t* o1, opt_oct_t* o2)
{
  opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_IS_LEQ,0);
  /*** TODO:
	Handle arg_assert
  ****/
   if((o1->dim != o2->dim) || (o1->intdim != o2->intdim))return false;
  //arg_assert(a1->dim==a2->dim && a1->intdim==a2->intdim,return false;);
  if (pr->funopt->algorithm>=0) opt_oct_cache_closure(pr,o1);
  if (!o1->closed && !o1->m) {
    /* a1 definitively empty */
    return true;
  }
  else if (!o2->closed && !o2->m) {
    /* a2 definitively empty */
    if (o1->closed) {
      /* a1 not empty on Q */
      if (num_incomplete || o1->intdim) { flag_incomplete; }
      return false;
    }
    else { flag_algo; return false; }
  }
  else {
    double *m1 = o1->closed ? o1->closed : o1->m;
    double *m2 = o2->closed ? o2->closed : o2->m;
    return is_lequal_avx_half_double(m1, m2, o1->dim);
  }
}


bool opt_oct_is_eq(ap_manager_t* man, opt_oct_t* o1, opt_oct_t* o2)
{
  opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_IS_EQ,0);
  /*** TODO:
	Handle arg_assert
  ****/
   if((o1->dim != o2->dim) || (o1->intdim != o2->intdim))return false;
  //arg_assert(a1->dim==a2->dim && a1->intdim==a2->intdim,return false;);
  if (pr->funopt->algorithm>=0) {
    opt_oct_cache_closure(pr,o1);
    opt_oct_cache_closure(pr,o2);
  }
  if (!o1->closed && !o1->m) {
    if (!o2->closed && !o2->m) {
      /* both are empty */
      return true;
    }
    else if (o2->closed) {
      /* a1 empty, e2 not empty on Q */
      if (num_incomplete || o1->intdim) { flag_incomplete; }
      return false;
    }
    else { flag_algo; return false; }
  }
  else if (!o2->closed && !o2->m) {
    if (o1->closed) {
      /* a2 empty, e1 not empty on Q */
      if (num_incomplete || o1->intdim) { flag_incomplete; }
      return false;
    }
    else { flag_algo; return false; }
  }
  else {
    double *m1 = o1->closed ? o1->closed : o1->m;
    double *m2 = o2->closed ? o2->closed : o2->m;
    return is_equal_avx_half_double(m1,m2,o1->dim);
  }
}


ap_interval_t** opt_oct_to_box(ap_manager_t* man, opt_oct_t* o)
{
  opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_TO_BOX,0);
  ap_interval_t** in = ap_interval_array_alloc(o->dim);
  size_t i;
  if (pr->funopt->algorithm>=0) {
	opt_oct_cache_closure(pr,o);
  }
  if (!o->closed && !o->m) {
    /* definitively empty */
    for (i=0;i<o->dim;i++)
      ap_interval_set_bottom(in[i]);
  }
  else {
    /* put variable bounds */
    double* m = o->closed ? o->closed : o->m;
    for (i=0;i<o->dim;i++){
      opt_interval_of_bounds(pr,in[i],
			 m[opt_matpos(2*i,2*i+1)],m[opt_matpos(2*i+1,2*i)],true);
    }
    man->result.flag_exact = false;
    if (!o->closed) flag_algo;
    else if (num_incomplete || o->intdim) flag_incomplete;
    else if (pr->conv) flag_conv;
  }
  return in;
}


ap_lincons0_array_t opt_oct_to_lincons_array(ap_manager_t* man, opt_oct_t* o)
{
  ap_lincons0_array_t ar;
  opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_TO_LINCONS_ARRAY,0);
  
  if (!o->closed && !o->m) {
    /* definitively empty */
    ar = ap_lincons0_array_make(1);
    ar.p[0] = ap_lincons0_make_unsat();
  }
  else {
    /* put non-oo constraint bounds only */
    double* m = o->closed ? o->closed : o->m;
    int i,j,n=0;
    int size = 2*(o->dim)*(o->dim + 1);
    ar = ap_lincons0_array_make(size);
    for (i=0;i<2*o->dim;i++)
      for (j=0;j<=(i|1);j++,m++) {
	if ((i==j) || (*m==INFINITY)) continue;

	ar.p[n] = opt_lincons_of_bound(pr,i,j,*m);
	n++;
      }
    ar.size = n;
   // m = o->closed ? o->closed : o->m;
     
    if (pr->conv) flag_conv;
  }
  return ar;
}


/****

SAT Constraints
****/

bool opt_oct_sat_lincons(ap_manager_t* man, opt_oct_t* o,
		     ap_lincons0_t* lincons)
{
  opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_SAT_LINCONS,
					     2*(o->dim+1));
  if (pr->funopt->algorithm>=0) opt_oct_cache_closure(pr,o);
  if (!o->closed && !o->m) {
    /* really empty */
    return true;
  }
  else {
    double * b = o->closed ? o->closed : o->m;
    size_t i, ui, uj;
    ap_constyp_t c = lincons->constyp;
    opt_uexpr u;
    bool r;

    switch (c) {

      /* skipped */
    case AP_CONS_EQMOD:
    case AP_CONS_DISEQ:
      return false;

      /* handled */
    case AP_CONS_EQ:
    case AP_CONS_SUPEQ:
    case AP_CONS_SUP:
      break;

      /* error */
    default:
      assert(0);
    }

    u = opt_oct_uexpr_of_linexpr(pr,pr->tmp,lincons->linexpr0,o->intdim,o->dim);

    switch (u.type) {

    case OPT_EMPTY:
      /* the empty set has all properties */
     {
      return true;
     }

    case OPT_ZERO:
      if ((c==AP_CONS_SUPEQ && (pr->tmp[0]<=0)) ||
	  /* [-a,b] >= 0 <=> a <= 0 */
	  (c==AP_CONS_SUP && (pr->tmp[0]<0)) ||
	  /* [-a,b] > 0 <=> a < 0 */
	  (c==AP_CONS_EQ && (pr->tmp[0]==0) && (pr->tmp[1]==0))
	  /* [-a,b] = 0 <=> a=b=0 */
	  )
       {
	return true;
	} /* always saturates */
      else {
	/* does not always saturate on Q, if closed and no conv error */
	if (num_incomplete || o->intdim) { flag_incomplete; return false; }
	else if (!o->closed) { flag_algo; return false; }
	else if (pr->conv) { flag_conv; return false; }
	return false;
      }

   case OPT_UNARY:
      if (u.coef_i==1) ui = 2*u.i; else ui = 2*u.i+1;
      //bound_mul_2(pr->tmp[0],pr->tmp[0]);
      pr->tmp[0] = 2*pr->tmp[0];
      //bound_mul_2(pr->tmp[1],pr->tmp[1]);
      pr->tmp[1] = 2*pr->tmp[1];
      //bound_badd(pr->tmp[0],b[matpos(ui,ui^1)]);
      pr->tmp[0] += b[opt_matpos(ui,ui^1)];
      //bound_badd(pr->tmp[1],b[matpos(ui^1,ui)]);
      pr->tmp[1] += b[opt_matpos(ui^1,ui)];
      if ((pr->tmp[0] <=0) &&
	  /* c_i X_i + [-a,b] >= 0 <=> -c_i X_i + a <= 0 */
	  (c!=AP_CONS_SUP || (pr->tmp[0]<0)) &&
	  /* c_i X_i + [-a,b] >  0 <=> -c_i X_i + a <  0 */
	  (c!=AP_CONS_EQ || (pr->tmp[1]<=0))
	  /* c_i X_i + [-a,b] <= 0 <=>  c_i X_i + b <= 0 */
	  ){
	return true;
	} /* always saturates */
      else {
	/* does not always saturate on Q, if closed and no conv error */
	if (num_incomplete || o->intdim) { flag_incomplete; return false; }
	else if (!o->closed) { flag_algo; return false; }
	else if (pr->conv) { flag_conv; return false; }
	return false;
      }

    case OPT_BINARY:
      if ( u.coef_i==1) ui = 2*u.i; else ui = 2*u.i+1;
      if ( u.coef_j==1) uj = 2*u.j; else uj = 2*u.j+1;
      //bound_badd(pr->tmp[0],b[matpos(uj,ui^1)]);
      pr->tmp[0] += b[opt_matpos2(uj,ui^1)];
      //bound_badd(pr->tmp[1],b[matpos(uj^1,ui)]);
      pr->tmp[1] += b[opt_matpos2(uj^1,ui)];
      if ((pr->tmp[0]<=0) &&
	  /* c_i X_i + c_j X_j + [-a,b] >= 0 <=> -c_i X_i - c_j X_j + a <= 0 */
	  (c!=AP_CONS_SUP || (pr->tmp[0]<0)) &&
	  /* c_i X_i + c_j X_j + [-a,b] >  0 <=> -c_i X_i - c_j X_j + a <  0 */
	  (c!=AP_CONS_EQ || (pr->tmp[1]<=0))
	  /* c_i X_i + c_j X_j + [-a,b] <= 0 <=>  c_i X_i + c_j X_j + b <= 0 */
	  ){
		return true;
	   }
      else {
	/* does not saturate on Q, when closed and no conv error */
	if (num_incomplete || o->intdim) { flag_incomplete; return false; }
	else if (!o->closed) { flag_algo; return false; }
	else if (pr->conv) { flag_conv; return false; }
	return false;
      }

    case OPT_OTHER:
      /* no clue */
      flag_incomplete;
      return false;

    default:
      assert(0);
      return false; /* unreachable */
    }
  }
}

bool opt_oct_sat_tcons(ap_manager_t* man, opt_oct_t* o,
		   ap_tcons0_t* cons)
{
  return ap_generic_sat_tcons(man,o,cons,NUM_AP_SCALAR,false);
}

