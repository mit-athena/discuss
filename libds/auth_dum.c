/*
 *
 * auth_dum () -- Authentication procedure for non-kerberos sites.
 *		  Just returns an empty string.
 *
 */
get_authenticator (service_id, checksum, authp, authl, result)
char *service_id;
int checksum;
char **authp;
int *authl;
int *result;
{
     *authl = 0;
     *authp = "";
     *result = 0;
     return;
}
