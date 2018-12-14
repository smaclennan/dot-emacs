void out_init(void);
void out_file(const char *fname);
void out_sym(char type, int lineno, const char *sym);
int out_fini(void);

int do_lookup(const char *sym);
