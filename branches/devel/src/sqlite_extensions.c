/* Prototyp zum Laden von Erweiterungen in SQLite mittels Funktion 
   load_extension() 
   Informationen dazu: http://www.sqlite.org/cvstrac/wiki?p=LoadableExtensions
   In Datei DESCRIPTION des Pakets muss "LinkingTo: RSQLite" benutzt werden
   */


#include "sqlite3ext.h"
#include <string.h>

SQLITE_EXTENSION_INIT1


double jarowinkler_core(char * str_1, char * str_2,
             double W_1, double W_2, double W_t,
             double r);



void jarowinkler_wrapper(sqlite3_context *ctx, int n_values, sqlite3_value **value)
{
	// check for NULL values, return NULL if any of the input strings is NULL
	if(sqlite3_value_type(value[0]) == SQLITE_NULL || 
  	 sqlite3_value_type(value[1]) == SQLITE_NULL)
  {
		sqlite3_result_null(ctx);
		return;
	}
  const unsigned char *str1 = sqlite3_value_text(value[0]);
  const unsigned char *str2 = sqlite3_value_text(value[1]);
//   Rprintf("String 1: %s\n", str1);
//   Rprintf("String 2: %s\n", str2);
  double result;
  result = jarowinkler_core(str1, str2, 1.0/3, 1.0/3, 1.0/3, 0.5);
//   Rprintf("Ergebnis des Stringvergleichs: %f", result);
  sqlite3_result_double(ctx, result);
}


/* SQLite invokes this routine once when it loads the extension.
** Create new functions, collating sequences, and virtual table
** modules here.  This is usually the only exported symbol in
** the shared library.
*/
int sqlite3_extension_init(
  sqlite3 *db_connection,
  char **pzErrMsg,
  const sqlite3_api_routines *pApi
){
  SQLITE_EXTENSION_INIT2(pApi)

   sqlite3_create_function(
      db_connection,
      "jarowinkler",
      2,
      SQLITE_UTF8,
      NULL,
      &jarowinkler_wrapper,
      NULL,
      NULL
      );
  return 0;
}
      