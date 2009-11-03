#include<string.h>
#include<stdio.h>
#include <R.h>


static int calc_levdist(const char *s1, const char *s2);
void levenshtein(char ** strvec_1, char ** strvec_2,
             int * length_1, int * length_2, int * ans);

/* Interface for GNU implementation below
 * 
 * Arguments:
 * 	strvec_1, strvec_2		The character vectors to compare
 * 	length_1, length_2		Length of strvec_1 and strvec_2
 * 	ans										return vector, must hold max(length_1, length_2)
 * 												double numbers     
 */
void levenshtein(char ** strvec_1, char ** strvec_2,
             int * length_1, int * length_2, int * ans)
{
  int max_length= *length_1 > *length_2 ? *length_1 : *length_2;  
  int str_ind;
	for (str_ind=0; str_ind < max_length; str_ind++)
  {
    char * str_1=strvec_1[str_ind % *length_1];
    char * str_2=strvec_2[str_ind % *length_2];
   	int lev_dist=calc_levdist(str_1, str_2);
    ans[str_ind]=lev_dist;
// 		Rprintf("Vergleiche %s, %s\n",str_1, str_2); // Debug-Ausgabe
// 		Rprintf("Levenshtein-Distanz: %d\n",lev_dist);   
// 		Rprintf("Levenshtein-Distanz: %d\n",ans[str_ind]);   
  }
} 
// 
// int main(int argc, char ** argv)
// {
// 	if (argc==3)
// 	{
// 		printf("%d\n",calc_levdist(argv[1],argv[2]));
// 		return(0);
// 	}
// 	else
// 	{
// 		printf("Usage: levenshtein STRING_1 STRING_2");
// 		return(1);
// 	}
// }

/*
Copyright (C)  2001 Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this software; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
Boston, MA 02111-1307 USA

This software is a derivative work of PHP3 Copyright (c) 1997-2000
PHP Development Team
*/

/* $Id: levenshtein.c,v 1.2 2009-11-03 08:27:45 borg Exp $ */

//#include <stdio.h>
//#include "php.h"
//#include "internal_functions.h"
//#include "reg.h"
//#include "post.h"
//#include "php3_string.h"

static int calc_levdist(const char *s1, const char *s2) /* faster, but obfuscated */
{
	register char *p1,*p2;
	register int i,j,n;
	int l1=0,l2=0;
	char r[512];
	const char *tmp;

	/* skip equal start sequence, if any */
	while(*s1==*s2) {
		if(!*s1) break;
		s1++; s2++;
	}
	
	/* if we already used up one string, then
      the result is the length of the other */
	if(*s1=='\0') return strlen(s2);
	if(*s2=='\0') return strlen(s1);

	/* length count */
	while(*s1++) l1++;
	while(*s2++) l2++;
	
	/* cut of equal tail sequence, if any */
	while(*--s1 == *--s2) {
		l1--; l2--;		
	}
	
	/* reset pointers, adjust length */
	s1-=l1++;
	s2-=l2++;
  	
	/* possible dist to great? */
 	if(abs(l1-l2)>=255) return -1;

	/* swap if l2 longer than l1 */
	if(l1<l2) {
		tmp=s1; s1=s2; s2=tmp;
		l1 ^= l2; l2 ^= l1; l1 ^= l2;
	}

	
	/* fill initial row */
	n=(*s1!=*s2);
	for(i=0,p1=r;i<l1;i++,*p1++=n++,p1++) {/*empty*/}
	
	/* calc. rowwise */
	for(j=1;j<l2;j++) {
		/* init pointers and col#0 */
		p1 = r + !(j&1);
		p2 = r + (j&1);
		n=*p1+1;
		*p2++=n;p2++;
		s2++;
		
		/* foreach column */
		for(i=1;i<l1;i++) {
			if(*p1<n) n=*p1+(*(s1+i)!=*(s2)); /* replace cheaper than delete? */
			p1++;
			if(*++p1<n) n=*p1+1; /* insert cheaper then insert ? */
			*p2++=n++; /* update field and cost for next col's delete */
			p2++;
		}	
	}

	/* return result */
	return n-1;
}
