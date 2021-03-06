#include "opt_oct_hmat.h"


opt_oct_t* opt_oct_meet_lincons_array(ap_manager_t* man,
			      bool destructive, opt_oct_t* o,
			      ap_lincons0_array_t* array)
{
  opt_oct_internal_t* pr =
    opt_oct_init_from_manager(man,AP_FUNID_MEET_LINCONS_ARRAY,2*(o->dim+8));
  if (!o->closed && !o->m)
    /* definitively empty */
    return opt_oct_set_mat(pr,o,NULL,NULL,destructive);
  else {
    bool exact, respect_closure;
    int i;
    opt_oct_mat_t * oo = o->closed ? o->closed : o->m;
    /* can / should we try to respect closure */
    respect_closure = (oo==o->closed) && (pr->funopt->algorithm>=0);
    int size = 2*(o->dim)*(o->dim + 1);
    if (!destructive) oo = opt_hmat_copy(oo,o->dim);

    /* go */
   
    bool res = opt_hmat_add_lincons(pr,oo,o->intdim,o->dim,array,&exact,&respect_closure);
   
    if (res) {
      /* empty */
      if (!destructive) {
	opt_hmat_free(oo);
	oo = NULL;
      }
      return opt_oct_set_mat(pr,o,NULL,NULL,destructive);
    }
    else {
      /* exact if octagonal constraints & no conversion error */
      if (num_incomplete || !exact) flag_incomplete;
      else if (pr->conv) flag_conv;
      if (respect_closure) return opt_oct_set_mat(pr,o,NULL,oo,destructive);
      else return opt_oct_set_mat(pr,o,oo,NULL,destructive);
    }
  }
}


opt_oct_t* opt_oct_meet_tcons_array(ap_manager_t* man,
			    bool destructive, opt_oct_t* o,
			    ap_tcons0_array_t* array)
{
  return ap_generic_meet_intlinearize_tcons_array(man,destructive,o,array,
						  NUM_AP_SCALAR,
						  AP_LINEXPR_INTLINEAR,
						  &opt_oct_meet_lincons_array);
}

opt_oct_t* opt_oct_assign_texpr_array(ap_manager_t* man,
			      bool destructive, opt_oct_t* o,
			      ap_dim_t* tdim,
			      ap_texpr0_t** texpr,
			      int size,
			      opt_oct_t* dest)
{
  opt_oct_t *b = ap_generic_assign_texpr_array(man,destructive,o,tdim,texpr,size,dest);
  return b;
  //return ap_generic_assign_texpr_array(man,destructive,o,tdim,texpr,size,dest);
}
