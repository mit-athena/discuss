/*
 * Discuss server internal routines
 */

#ifndef __STDC__
#define const
#define volatile
#endif

/*
 * ACL-manipulation routines
 */

#ifdef __STDC__
dsc_acl *acl_read(int fd);
dsc_acl *acl_create(void);
dsc_acl *acl_copy(dsc_acl *old);
bool acl_check(dsc_acl *list, const char *principal, const char *modes);
int acl_add_access(dsc_acl *list, const char *principal, const char *modes);
bool acl_delete_access(dsc_acl *acl, const char *principal);
bool acl_is_subset(const char *s1, const char *s2);
int acl_replace_access(dsc_acl *list, const char *principal,
		       const char *modes);
const char *acl_get_access(dsc_acl *list, const char *principal);
char *acl_canon(const char *s1, const char *s2, int *code);
bool acl_write(int fd, dsc_acl *list);
bool has_mtg_access();
bool has_trn_access();
int acl_destroy (dsc_acl *list);
char *acl_intersection (const char *s1, const char *s2);
char *acl_union (const char *s1, const char *s2);
char *acl_subtract (const char *s1, const char *s2);
#else
dsc_acl *acl_read();
dsc_acl *acl_create();
dsc_acl *acl_copy();
bool acl_check();
extern int acl_add_access();
extern bool acl_delete_access();
extern bool acl_is_subset();
extern acl_replace_access();
char *acl_get_access();
char *acl_canon();
bool acl_write();
bool has_mtg_access();
bool has_trn_access();
#endif
