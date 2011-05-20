/* THIS PROGRAM READS A SWEEP FILE */ 

/* modified 2/28/06 by David Dowell:                                                    */
/* -- added compiler option -DLONG32, which reads "int" (32 bit) type instead of "long" */

/* modified 1/15/10 by David Dowell:                                                    */
/* -- allow for different sizes of RADD and PARM        blocks:                         */
/* --    RADD either 144 or 300 bytes                                                   */
/* --    PARM either 104 or 216 bytes                                                   */

/* modified 2/3/10 by David Dowell:                                                     */
/* -- rather than terminating, return data of size 0 if the specified field is          */
/*       not found in the sweep file                                                    */
/* -- added a new function read_csfd                                                    */
/* -- added offset_fac to the calculations for RDAT                                     */

/* modified 5/20/11 by David Dowell:
/* -- directed sweepread to return if the identifier is not recognized                  */
/* -- modified read_sh_arr so that bad / missing data are set to -32768.0               */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include <ctype.h>
#include "read_dorade.h"

/**************************************************/
void sweepread_(char swp_fname[],struct vold_info *vptr,
                struct radd_info *rptr,struct celv_info *cptr,
                struct cfac_info *cfptr,struct parm_info *pptr,
                struct swib_info *sptr,struct ryib_info *ryptr,
                struct asib_info *aptr,struct rdat_info *dptr,
                char fld_name[])
{

/****************************************************/
/* THIS SUBROUTINE READS THE DESCRIPTORS FROM A     */
/* SWEEP FILE & PASSES THE VOLD, RADD, CFAC, PARM,  */
/* SWIB BACK TO THE FORTRAN PROGRAM                 */
/* swp_fname=name of sweep file                     */
/* *vptr=pointer to vold descriptor                 */
/* *rptr=pointer to radd descriptor                 */
/* *cptr=pointer to cfac descriptor                 */
/* *pptr=pointer to parm descriptor                 */
/* *sptr=pointer to swib descriptor                 */
/* *ryptr=pointer to ryib descriptor                */
/* *aptr=pointer to asib descriptor                 */
/* *dptr=pointer to rdat descriptor                 */
/* tot_gates=total number of gates                  */
/* arr=array to hold gate spacing                   */
/* *sptr=pointer of swib descriptor                 */
/****************************************************/

   FILE *fp;
   int i,len=0;
   char identifier[IDENT_LEN];
   int desc_len,parm_count=0;
   int fld_num=0;
   int match;
   int found=FALSE;
   int beam=0;

/* ADD NULL CHARACTER TO END OF STRING */
   for (i=0;i<strlen(swp_fname);i++) {
      if (isspace(swp_fname[i])) {break;}
      else {len++;}
   }
   swp_fname[len]='\0';

/* READ THE SWEEP FILE */
   /* OPEN THE SWEEP FILE */
   if ( (fp = fopen(swp_fname,"rb"))==NULL) {
      printf("Can't open %s\n",swp_fname);
   }

   while ( !feof(fp) ) {

      /* READ THE DESCRIPTOR IDENTIFIER */
      if ( (fread(identifier,sizeof(char),IDENT_LEN,fp)) != IDENT_LEN) {
         printf("sweep file read error..can't read identifier\n");
         exit(-1);
      }
      /* printf ("reading '%s'\n",identifier); */

      /* READ THE DESCRIPTOR LENGTH */
#ifdef LONG32
      desc_len=read_int(fp);
#else
      desc_len=read_long(fp);
#endif
      /* printf ("desc_len = %d\n",desc_len); */

      if ( (strncmp(identifier,"VOLD",IDENT_LEN)) == 0) {
         /* READ THE VOLUME DESCRIPTOR */
         /*printf ("reading vold\n");*/
         read_vold(fp,vptr);

      } else if ( (strncmp(identifier,"RADD",IDENT_LEN)) == 0) {
         /* READ THE RADAR DESCRIPTOR */
         /*printf ("reading radd\n");*/
         read_radd(fp,rptr,desc_len);
         if (rptr->num_param_desc > MAX_NUM_PARMS) {
            printf ("WARNING: NUMBER OF PARAMETERS ");
            printf ("GREATER THAN MAX_NUM_PARMS\n");
            printf ("NUMBER OF PARAMETERS: %d\n",rptr->num_param_desc);
            printf ("MAX_NUM_PARMS: %d\n",MAX_NUM_PARMS);
            printf ("SEE READ_DORADE.H\n");
         }

      } else if ( (strncmp(identifier,"CFAC",IDENT_LEN)) == 0) {
         /* READ THE CFAC DESCRIPTOR */
         /*printf ("reading cfac\n");*/
         read_cfac(fp,cfptr);

      } else if ( (strncmp(identifier,"PARM",IDENT_LEN)) == 0) {
         /* READ THE PARAMETER DESCRIPTOR */
         /*printf ("reading parm\n");*/
         read_parm(fp,pptr,desc_len);
         *pptr++;

      } else if ( (strncmp(identifier,"CELV",IDENT_LEN)) == 0) {
         /*printf ("reading CELV\n");*/
         /* CHECK & MAKE SURE NUMBER OF GATES DOESN'T
            EXCEED LIMIT */
         read_celv(fp,cptr,desc_len);
         if (cptr->total_gates>MAX_GATES) {
            printf ("WARNING: NUMBER OF GATES ");
            printf ("GREATER THAN MAX_GATES\n");
            printf ("SEE READ_DORADE.H\n");
            printf ("NUMBER OF GATES: %d\n",cptr->total_gates);
            printf ("MAX_GATES: %d\n",MAX_GATES);
         }

      } else if ( (strncmp(identifier,"CSFD",IDENT_LEN)) == 0) {
         /*printf ("reading CSFD\n");*/
         /*printf ("desc_len = %d\n", desc_len);*/
         read_csfd(fp,cptr,desc_len);
         if (cptr->total_gates>MAX_GATES) {
            printf ("WARNING: NUMBER OF GATES ");
            printf ("GREATER THAN MAX_GATES\n");
            printf ("SEE READ_DORADE.H\n");
            printf ("NUMBER OF GATES: %d\n",cptr->total_gates);
            printf ("MAX_GATES: %d\n",MAX_GATES);
         }

      } else if ( (strncmp(identifier,"SWIB",IDENT_LEN)) == 0) {
         /* READ THE SWEEP INFO DESCRIPTOR */
         /*printf ("reading swib\n");*/
         read_swib(fp,sptr);
         if (sptr->num_rays > MAX_BEAMS) {
            printf ("WARNING: NUMBER OF BEAMS ");
            printf ("GREATER THAN MAX_BEAMS\n");
            printf ("SEE READ_DORADE.H\n");
            printf ("NUMBER OF BEAMS: %d\n",sptr->num_rays);
            printf ("MAX_BEAMS: %d\n",MAX_BEAMS);
         }


      } else if ( (strncmp(identifier,"RYIB",IDENT_LEN)) == 0) {
         /* READ THE RAY INFO DESCRIPTOR */
         /*printf ("reading ryib\n");*/
         read_ryib(fp,ryptr);
         fld_num=0;
         beam++;
         *ryptr++;
         /* GO BACK TO FIRST PARAMETER DESCRIPTOR */
         for (i=0;i<rptr->num_param_desc;i++) {*pptr--;}
         /* DID WE FIND THE FIELD?? */
         if (beam==2 && found==FALSE) {
           printf ("%s NOT FOUND..\n",fld_name);
           /*  printf ("VALID FIELD NAMES:\n");
           for (i=0;i<rptr->num_param_desc;i++) {
             printf ("%s\n",pptr->parm_name);
             *pptr++;
           } */
           /* printf ("EXITING..\n"); */
           /* exit(-1); */
           sptr->num_rays = 0;
           cptr->total_gates = 0;
         }


      } else if ( (strncmp(identifier,"ASIB",IDENT_LEN)) == 0) {
         /* READ THE PLATFORM INFO DESCRIPTOR */
         /*printf ("reading asib\n");*/
         read_asib(fp,aptr);
         *aptr++;

      } else if ( (strncmp(identifier,"RDAT",IDENT_LEN)) == 0) {
         /* READ THE DATA DESCRIPTOR */
         /* printf ("reading rdat %s %d:\n", fld_name, desc_len); */

         match=FALSE;
         read_rdat(fp,fld_name,parm_count,desc_len,&match,
                   pptr->parm_type,beam,cptr->total_gates,
                   rptr->compress_flag,pptr->baddata_flag,
                   pptr->scale_fac, pptr->offset_fac, dptr);
         fld_num++;
         *pptr++;
         if (match==TRUE) {
            *dptr++;
            found=match;
         }

      } else if ( (strncmp(identifier,"NULL",IDENT_LEN)) == 0) {
         break;

      } else if (    ((strncmp(identifier,"SSWB",IDENT_LEN)) == 0)
                  || ((strncmp(identifier,"COMM",IDENT_LEN)) == 0)
                  || ((strncmp(identifier,"QDAT",IDENT_LEN)) == 0)
                  || ((strncmp(identifier,"XSTF",IDENT_LEN)) == 0)
                  || ((strncmp(identifier,"RKTB",IDENT_LEN)) == 0)
                  || ((strncmp(identifier,"FRAD",IDENT_LEN)) == 0)
                  || ((strncmp(identifier,"FRIB",IDENT_LEN)) == 0)
                  || ((strncmp(identifier,"LIDR",IDENT_LEN)) == 0)
                  || ((strncmp(identifier,"FLIB",IDENT_LEN)) == 0)
                  || ((strncmp(identifier,"SITU",IDENT_LEN)) == 0)
                  || ((strncmp(identifier,"ISIT",IDENT_LEN)) == 0)
                  || ((strncmp(identifier,"INDF",IDENT_LEN)) == 0)
                  || ((strncmp(identifier,"MINI",IDENT_LEN)) == 0)
                  || ((strncmp(identifier,"NDDS",IDENT_LEN)) == 0)
                  || ((strncmp(identifier,"TIME",IDENT_LEN)) == 0)
                  || ((strncmp(identifier,"SAVE",IDENT_LEN)) == 0) ) {		  
		  
#ifdef LONG32
          skip_bytes(fp,desc_len-(IDENT_LEN+sizeof(int)));
#else
          skip_bytes(fp,desc_len-(IDENT_LEN+sizeof(long)));
#endif

      } else {
		  
 	     printf ("*** warning:  unable to read entire sweep file\n");
		 break;

      } /* endif */
	   
   } /* endwhile */

   fclose(fp);

}
/**************************************************/
void read_vold(FILE *fp,struct vold_info *vptr) 
{
   
   /* READ THE VOLUME DESCRIPTOR */
   if ( fread ((char *)vptr,sizeof (struct vold_info),1,fp) !=1 ) {
      puts("ERROR READING VOLUME DESCRIPTOR\n");
      exit(-1);
   } /* endif */

}
/**************************************************/
void read_radd(FILE *fp,struct radd_info *rptr,int desc_len) 
{

   float pulse_width;
   
   /* READ THE RADAR DESCRIPTOR */

   if ( fread ((char *)rptr,sizeof (struct radd_info),1,fp) !=1 ) {
      puts("ERROR READING RADAR DESCRIPTOR\n");
      exit(-1);
   } /* endif */

   if (desc_len==300) {

      /*printf("300 byte RADD\n");*/

      skip_bytes(fp,112);

      if ( fread (&pulse_width,4,1,fp) !=1 ) {
         puts("ERROR READING PULSE_WIDTH\n");
         exit(-1);
      }
      /*printf("pulse_width = %f\n", pulse_width);*/

      skip_bytes(fp,40);      

   } /* endif */

}
/**************************************************/
void read_cfac(FILE *fp,struct cfac_info *cptr) 
{
   
   /* READ THE CFAC DESCRIPTOR */
   if ( fread ((char *)cptr,sizeof (struct cfac_info),1,fp) !=1 ) {
      puts("ERROR READING CFAC DESCRIPTOR\n");
      exit(-1);
   } /* endif */

}
/**************************************************/
void read_parm(FILE *fp,struct parm_info *pptr,int desc_len) 
{

#ifdef LONG32
   int offset_to_data;
   int number_of_cells;
#else
   long offset_to_data;
   long number_of_cells;
#endif
   float meters_to_first_cell;
   float meters_between_cells;

   /* READ THE PARAMETER DESCRIPTOR */
   if ( fread ((char *)pptr,sizeof (struct parm_info),1,fp) !=1 ) {
      puts("ERROR READING PARAMETER DESCRIPTOR\n");
      exit(-1);
   } /* endif */

   if (desc_len==216) {

      /*printf("216 byte PARM\n");*/

      skip_bytes(fp,16);

      if ( fread (&offset_to_data,4,1,fp) !=1 ) {
         puts("ERROR READING OFFSET_TO_DATA\n");
         exit(-1);
      }
      /*printf("offset_to_data = %d\n", offset_to_data);*/

      skip_bytes(fp,76);

      if ( fread (&number_of_cells,4,1,fp) !=1 ) {
         puts("ERROR READING NUMBER_OF_CELLS\n");
         exit(-1);
      }
      /*printf("number_of_cells = %d\n", number_of_cells);*/

      if ( fread (&meters_to_first_cell,4,1,fp) !=1 ) {
         puts("ERROR READING METERS_TO_FIRST_CELL\n");
         exit(-1);
      }
      /*printf("meters_to_first_cell = %f\n", meters_to_first_cell);*/

      if ( fread (&meters_between_cells,4,1,fp) !=1 ) {
         puts("ERROR READING METERS_BETWEEN_CELLS\n");
         exit(-1);
      }
      /*printf("meters_between_cells = %f\n", meters_between_cells);*/

      skip_bytes(fp,4);

   } /* endif */

    pptr->parm_name[PARM_NAME_LEN-1]='\0';
    pptr->parm_desc[PARM_DESC_LEN-1]='\0';
    pptr->parm_unit[PARM_UNIT_LEN-1]='\0';

    /* printf("field name:  %s\n",pptr->parm_name); */
    /* printf("field desc:  %s\n",pptr->parm_desc); */
    /* printf("field unit:  %s\n",pptr->parm_unit); */
    /* printf("\n");                                */
    
}
/***************************************************/
void read_celv(FILE *fp,struct celv_info *cptr,int desc_len)
{

   int skip;

   /* TOTAL GATES */
#ifdef LONG32
   cptr->total_gates=read_int(fp);
#else
   cptr->total_gates=read_long(fp);
#endif

   /* ALLOCATE THE ARRAY */
   /*
   cptr->gate_spacing=calloc(cptr->total_gates,sizeof(float));
   if (!cptr->gate_spacing) {
      printf ("Reallocation error..aborting..\n");
      exit(1);
   } 
   */

   /* GATE SPACING */
   if ( (fread(cptr->gate_spacing,sizeof(float),cptr->total_gates,fp))
   != cptr->total_gates) {
      puts("ERROR READING CELV DESCRIPTOR\n");
   }

   skip=desc_len-(sizeof(float)*cptr->total_gates+12);
   skip_bytes(fp,skip);

}
/***************************************************/
void read_csfd(FILE *fp,struct celv_info *cptr,int desc_len)
{

#ifdef LONG32
   int number_of_cell_segments;
#else
   long number_of_cell_segments;
#endif
   float meters_to_first_cell;
   float spacing;
   short segment_cell_count;
   int skip;
   int i;


#ifdef LONG32
   number_of_cell_segments = read_int(fp);
#else
   number_of_cell_segments = read_long(fp);
#endif
   /*printf("number_of_cell_segments = %d\n", number_of_cell_segments);*/
   if ( fread (&meters_to_first_cell,4,1,fp) !=1 ) {
      puts("ERROR READING METERS_TO_FIRST_CELL\n");
      exit(-1);
   }
   /*printf("meters_to_first_cell = %f\n", meters_to_first_cell);*/

   if ( number_of_cell_segments == 1 ) {

     if ( fread (&spacing,4,1,fp) !=1 ) {
        puts("ERROR READING SPACING\n");
        exit(-1);
     }
     /*printf("spacing = %f\n", spacing);*/

     skip=28;
     skip_bytes(fp,skip);

     if ( fread (&segment_cell_count,2,1,fp) !=1 ) {
        puts("ERROR READING SEGMENT_CELL_COUNT\n");
        exit(-1);
     }
     /*printf("segment_cell_count = %d\n", segment_cell_count);*/

     skip=14;
     skip_bytes(fp,skip);

     cptr->total_gates = segment_cell_count;
     for (i=0; i<segment_cell_count; i++)
       cptr->gate_spacing[i] = meters_to_first_cell + i*spacing;
   }

   else {
     printf("number_of_cell_segments > 1:  %d\n", number_of_cell_segments);
     printf("EXITING..\n");
     exit(-1);
   }

}
/**************************************************/
void read_swib(FILE *fp,struct swib_info *sptr) 
{
   
   /* READ THE SWEEP INFO DESCRIPTOR */
   if ( fread ((char *)sptr,sizeof (struct swib_info),1,fp) !=1 ) {
      puts("ERROR READING SWIB DESCRIPTOR\n");
      exit(-1);
   } /* endif */

}
/**************************************************/
void read_ryib(FILE *fp,struct ryib_info *rptr) 
{
   
   /* READ THE RAY INFO DESCRIPTOR */
   if ( fread ((char *)rptr,sizeof (struct ryib_info),1,fp) !=1 ) {
      puts("ERROR READING RYIB DESCRIPTOR\n");
      exit(-1);
   } /* endif */

}
/**************************************************/
void read_asib(FILE *fp,struct asib_info *aptr) 
{
   
   /* READ THE PLATFORM INFO DESCRIPTOR */
   if ( fread ((char *)aptr,sizeof (struct asib_info),1,fp) !=1 ) {
      puts("ERROR READING ASIB DESCRIPTOR\n");
      exit(-1);
   } /* endif */

}
/***************************************************/
#ifdef LONG32
void read_rdat(FILE *fp,char fld_name[],int fld_num,
               int desc_len,int *match,short parm_type,
	       int beam_count,int total_gates,
               short compression,int baddata_flag,
	       float scale_fac,float offset_fac,struct rdat_info *dptr)
#else
void read_rdat(FILE *fp,char fld_name[],int fld_num,
               int desc_len,int *match,short parm_type,
	       int beam_count,long total_gates,
               short compression,long baddata_flag,
	       float scale_fac,float offset_fac,struct rdat_info *dptr)
#endif
{

   /* fp=pointer to sweep file
   *  fld_name=user supplied field name
   *  fld_num=index of parameter descriptor
   *  desc_len=length of RDAT descriptor
   *  match=flag to indicate field match found
   *  parm_type=data type of parameter
   *  dptr=pointer to rdat structure
   */ 

   /* modified 11/10/10 by David Dowell */
   int t_strsize,i_strsize,datasize,arrsize;
   char tempname[PARM_NAME_LEN];
   char inputname[PARM_NAME_LEN];

   memset(dptr->parm_name,' ',PARM_NAME_LEN);
   memset(tempname,' ',PARM_NAME_LEN);
   memset(inputname,' ',PARM_NAME_LEN);

   /* READ THE PARAMETER NAEM */
   if ( (fread(tempname,sizeof(char),PARM_NAME_LEN,fp))
         != PARM_NAME_LEN)
      {printf("sweep file read error..can't read parameter name\n");}

   /* printf("field name:  %s\n",tempname); */

   /* added 11/10/10 by David Dowell */
   /* CALCULATE LENGTH OF INPUTNAME AND INITIALIZE IT */
   for (i_strsize=0;i_strsize<PARM_NAME_LEN;i_strsize++) {
       if (isspace(fld_name[i_strsize])) {break;}
   }
   strncpy(inputname,fld_name,i_strsize);

   /* CALCULATE LENGTH OF TEMPNAME */
   for (t_strsize=0;t_strsize<strlen(tempname);t_strsize++) {
       if (isspace(tempname[t_strsize])) {break;}
   }

   /* FIND THE CORRECT FIELD */
   /* modified 11/10/10 by David Dowell */
   if ( (i_strsize==t_strsize) && (strncmp(tempname,inputname,t_strsize)==0)) {

      *match=TRUE;
      /* CALCULATE SIZE OF DATA */
      strncpy(dptr->parm_name,tempname,t_strsize);
      if (parm_type==1) {datasize=sizeof(char);}
      else if (parm_type==2) {datasize=sizeof(short);}
#ifdef LONG32
      else if (parm_type==3) {datasize=sizeof(int);}
#else
      else if (parm_type==3) {datasize=sizeof(long);}
#endif
      else if (parm_type==4) {datasize=sizeof(float);}
      /* SIZE OF ARRAY */
#ifdef LONG32
      arrsize=(desc_len-(IDENT_LEN+sizeof(int)
	       +PARM_NAME_LEN))/datasize;
#else
      arrsize=(desc_len-(IDENT_LEN+sizeof(long)
	       +PARM_NAME_LEN))/datasize;
#endif

      /* READ IN THE DATA */
      if (datasize==1) {
	 read_ch_arr(fp,arrsize);
      } else if (datasize==2) {
	 read_sh_arr(fp,arrsize,beam_count,total_gates,compression,
		     baddata_flag,scale_fac,offset_fac,dptr);
      } else if (datasize==3) {
	 read_lg_arr(fp,arrsize);
      } else if (datasize==4) {
	 read_fl_arr(fp,arrsize);
      } /* endif */

   } else {
#ifdef LONG32
      skip_bytes(fp,desc_len-(IDENT_LEN+sizeof(int)+PARM_NAME_LEN));
#else
      skip_bytes(fp,desc_len-(IDENT_LEN+sizeof(long)+PARM_NAME_LEN));
#endif
   }

}
/***************************************************/
void read_ch_arr(FILE *fp,int arrsize) {
}
/***************************************************/
#ifdef LONG32
void read_sh_arr(FILE *fp,int arrsize,int beam_count,
		 int total_gates,short compression,
		 int baddata_flag,float scale_fac,float offset_fac,
                 struct rdat_info *dptr) {
#else
void read_sh_arr(FILE *fp,int arrsize,int beam_count,
		 long total_gates,short compression,
		 long baddata_flag,float scale_fac,float offset_fac,
                 struct rdat_info *dptr) {
#endif

   static short arr_com[MAX_GATES], arr_uncom[MAX_GATES];
   int i,num;
   int empty_run=0;

   if (compression==TRUE) {
      /* READ A RAY OF DATA */
      if ( (fread(arr_com,sizeof(short),arrsize,fp)) != arrsize)
         {printf("sweep file read error..can't read data\n");}

      /* UNCOMPRESS A RAY OF DATA */
      num=dd_hrd16_uncompressx(arr_com,arr_uncom,baddata_flag,
                           &empty_run,total_gates,beam_count);

   } else {
      /* READ A RAY OF DATA */
      if ( (fread(arr_uncom,sizeof(short),arrsize,fp)) != arrsize)
         {printf("sweep file read error..can't read data\n");}
   } /* endif */

   /* SCALE THE DATA */
   for (i=0;i<total_gates;i++) {
      if (arr_uncom[i] != baddata_flag) {
         dptr->data[i]=((float)arr_uncom[i] - offset_fac) / scale_fac;
      } else {

         /* printf("baddata_flag, data = %d, %f\n", baddata_flag, dptr->data[i]); */

         /* David Dowell 5/20/11                                                               */
         /* Use the following hard-coded value for bad/missing data rather than the bad data   */
         /* flag specified in the dorade sweep file.  The value below should be the same as    */
         /* sbad in opaws.inc.                                                                 */
         dptr->data[i]=-32768.0;
         /* dptr->data[i]=(float)arr_uncom[i]; */
      }
   }

}
/***************************************************/
void read_lg_arr(FILE *fp,int arrsize) {
}
/***************************************************/
void read_fl_arr(FILE *fp,int arrsize) {
}
/***************************************************/
void get_field(struct parm_info parm[],int num_desc,int *fld_num)
{

   int i,num;

   /* GET THE NAME OF THE DESIRED FIELD */
   printf ("Please choose number of the desired field:\n");

   for (i=0;i<num_desc;i++) {
      printf ("%2d.  %s\n",i+1,parm[i].parm_name);
   }

   scanf("%d",fld_num);

}
/***************************************************/
long read_long(FILE *fp)
{

   long temp;

   if ( (fread(&temp,sizeof(long),1,fp)) != 1) {
      printf("sweep file read error..\n");
   }
   return temp;

}
/***************************************************/
int read_int(FILE *fp)
{

   int temp;

   if ( (fread(&temp,sizeof(int),1,fp)) != 1) {
      printf("sweep file read error..\n");
   }
   return temp;

}
/***************************************************/
void skip_bytes(FILE *fp,int numskip)
{

/* SKIP TO THE RIGHT BYTE! */

   if (fseek(fp,numskip,SEEK_CUR)) {
      printf("Seek Error..aborting..\n");
      exit(1);
   }

}
/***************************************************/
/* Dick Oye's decompression routine */
int dd_hrd16_uncompressx( ss, dd, flag, empty_run, wmax ,beam_count)

  short *ss, *dd;
  int flag, *empty_run, wmax;
  int beam_count;
{
    /*
     * routine to unpacks actual data assuming MIT/HRD compression where:
     * ss points to the first 16-bit run-length code for the compressed data
     * dd points to the destination for the unpacked data
     * flag is the missing data flag for this dataset that is inserted
     *     to fill runs of missing data.
     * empty_run pointer into which the number of missing 16-bit words
     *    can be stored. This will be 0 if the last run contained data.
     # wmax indicate the maximum number of 16-bit words the routine can
     *    unpack. This should stop runaways.
     */
    int i, j, k, n, mark, wcount=0;

    while(*ss != 1) {           /* 1 is an end of compression flag */
        n = *ss & 0x7fff;       /* nab the 16-bit word count */
        if(wcount+n > wmax) {
            printf("Uncompress failure %d %d %d at %d\n"
                   , wcount, n, wmax, beam_count);
            mark = 0;
            break;
        }
        else {
            wcount += n;                /* keep a running tally */
        }
        if( *ss & 0x8000 ) {    /* high order bit set implies data! */
            *empty_run = 0;
            ss++;
            for(; n--;) {
                *dd++ = *ss++;
            }
        }
        else {                  /* otherwise fill with flags */
            *empty_run = n;
            ss++;
            for(; n--;) {
                *dd++ = flag;
            }
        }
    }
    return(wcount);
}


