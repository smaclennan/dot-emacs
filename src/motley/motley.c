#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <ctype.h>
#include <errno.h>
#include <assert.h>
#include <regex.h>

// If this is defined, try to ignore #if 0 blocks that are outside of bodies.
#define IGNORE_IF0

// For some reason, Dinkumware likes put brackets around functions
// that have a pointer. e.g. char *(index)(...)
#define DINKUMWARE_HACK

#ifdef DINKUMWARE_HACK
#define FUNC_RE "\\(?(_*[a-zA-Z][a-zA-Z0-9_]*)\\)? ?\\(\\)"
#else
#define FUNC_RE "(_*[a-zA-Z][a-zA-Z0-9_]*) ?\\(\\)"
#endif

// The get_line() line
#define LINE_SIZE (16 * 1024)

static int verbose;

static FILE *out;
static const char *cur_fname; // for debugging save the current filename
static int cur_line;
static int lineno;
static int sol;

static regex_t func_re;

static void out_sym(char type, int lineno, const char *sym)
{
	if (!*sym) {
		if (verbose && type != 'e' && type != 'g') // SAM DBG
			printf("%s:%d empty sym %c\n", cur_fname, lineno, type);
		return;
	}

	fprintf(out, "%s %d %c\n", sym, lineno, type);
}

static inline int __getch(FILE *fp)
{
	return getc(fp);
}

static inline void ungetch(int c, FILE *fp)
{
	ungetc(c, fp);
}

#undef getc
#define getc bogus

static inline int ishex(int c)
{
	switch (c) {
	case '0' ... '9':
	case 'a' ... 'f':
	case 'A' ... 'F':
		return 1;
	default:
		return 0;
	}
}

// Deal with \c or \nnn
static void quote(FILE *fp)
{
	int c;

again:
	c = __getch(fp);

	switch (c) {
	case '0' ... '7':
		while ((c = __getch(fp)) != EOF && (c >= '0' && c <= '7')) ;
		ungetch(c, fp); // we read one too far
		break;
	case 'x':
	case 'X':
		while ((c = __getch(fp)) != EOF && ishex(c)) ;
		ungetch(c, fp); // we read one too far
		break;
	case '\r':
		goto again;
	case '\n':
		++cur_line;
		break;
	}
}

// Skip /* */ style comments.
static void skip_comments(FILE *fp)
{
	int count = 1;
	int state = 0;
	int c;

	while (count > 0 && (c = __getch(fp)) != EOF) {
		if (c == '\n')
			++cur_line;
		switch (state) {
		case 0:
			if (c == '*')
				state = 1;
			else if (c == '/')
				state = 2;
			break;
		case 1:
			if (c == '/') {
				--count;
				state = 0;
			} else if (c != '*')
				state = 0;
			break;
		case 2:
			if (c == '*')
				++count;
			else if(c != '/')
				state = 0;
			break;
		}
	}
}

// This is the lowest level character input and the only one that can
// call getc().
//
//   - It counts the lines.
//   - It removes spaces from the starts and ends of the line.
//   - It converts all sequences of spaces to one space.
//   - It removes empty lines.
//   - It removes CRs from evil dos files.
//   - It deals with the backslash.
//   - It deals with C comments.
//   - It deals with double and single quotes.
static int _getch(FILE *fp)
{
	int c;

again:
	c = __getch(fp);
	switch (c) {
	case '\n':
		++cur_line;
		if (sol == 1)
			goto again; // empty line
		sol = 1; // start of line next time
		return c;
	case '\r':
		goto again;
	case ' ':
	case '\t':
		if (sol)
			goto again;
		while ((c = __getch(fp)) == ' ' || c == '\t' || c == '\r') ;
		ungetch(c, fp); // we read one too far
		if (c == '\n')
			goto again;
		c = ' ';
		break;
	case '\\':
		quote(fp);
		goto again;
	case '/': // possible comment
		c = __getch(fp);
		if (c == '/') {
			while ((c = __getch(fp)) != '\n' && c != EOF) ;
			ungetch('\n', fp); // deal with \n
			goto again;
		} else if (c == '*') {
			skip_comments(fp);
			goto again;
		}
		ungetch(c, fp);
		c = '/';
		break;
	case '"':
		while ((c = __getch(fp)) != '"' && c != EOF) {
			if (c == '\\') // still must deal with backquotes
				quote(fp);
		}
		goto again;
	case '\'': ;
		c = __getch(fp);
		do {
			if (c == '\\')
				quote(fp);
			c = __getch(fp);
		} while (c != '\'' && c != EOF);
		goto again;
	case '#':
		// do not clear sol
		return c;
	}

	sol = 0;
	return c;
}

static void maybe_skip_if0(int c, FILE *fp)
{
#ifdef IGNORE_IF0
	if (c != 'i' || _getch(fp) != 'f' || _getch(fp) != ' ' || _getch(fp) != '0')
		return;

	int count = 1;
	while (count > 0 && (c = _getch(fp)) != EOF)
		if (c == '#' && sol) {
			c = _getch(fp);
			if (c == 'i') {
				if (_getch(fp) == 'f')
					++count;
			} else if (c == 'e') {
				// #endif or #elif or #else
				if ((c = _getch(fp) == 'n'))
					--count;
				else if (c == 'l' && _getch(fp) == 's') {
					if (count == 1)
						// our else
						--count;
				}
			}
		}
#endif
}

static void skip_body(FILE *fp)
{
	int count = 1;
	int c;

	while (count > 0 && (c = _getch(fp)) != EOF)
		if (c == '{')
			++count;
		else if (c == '}')
			--count;
	ungetc(c, fp);
}

// This deals with things we can't do in _getch()
//   - preprocessor statements
//   - body {}
//   - brackets ()
//   - array []
static int getch(FILE *fp)
{
	static int state;
	int c, count;

again:
	c = _getch(fp);

	switch (c) {
	case '\n':
		// don't clear sol
		state = 0;
		return c;
	case '#':
		if (sol) {
			sol = 0;
			if ((c = _getch(fp)) == ' ')
				c = _getch(fp);
			if (c == 'd') {
				ungetch(c, fp);
				return '#';
			}
			maybe_skip_if0(c, fp);
			// skip to EOL
			while (_getch(fp) != EOF && sol == 0) ;
			goto again;
		}
		break;
	case 'e':
		if (sol)
			state = 1;
		break;
	case '{':
		if (state != 5)
			skip_body(fp);
		break;
#ifdef DINKUMWARE_HACK
	case '*':
		state = 6;
		break;
#endif
	case '(':
#ifdef DINKUMWARE_HACK
		if (state == 7)
			break;
#endif
		state = 0;
		count = 1;
		if ((c = _getch(fp)) == ' ') c = _getch(fp);
		if (c == '*') { // typedef (*func)
			ungetch(c, fp);
			c = '(';
			break;
		}
		ungetch(c, fp);
		while (count > 0 && (c = _getch(fp)) != EOF)
			if (c == ')') --count;
			else if (c == '(') ++count;
		ungetch(c, fp);
		c = '(';
		break;
	case '[':
		count = 1;
		while (count > 0 && (c = _getch(fp)) != EOF)
			if (c == ']') --count;
			else if (c == '[') ++count;
		ungetch(c, fp);
		c = '[';
		break;
	}

	switch (state) {
	case 1: // e
		state = 2;
		break;
	case 2:
		state = c == 'n' ? 3 : 0;
		break;
	case 3:
		state = c == 'u' ? 4 : 0;
		break;
	case 4:
		state = c == 'm' ? 5 : 0;
		break;
#ifdef DINKUMWARE_HACK
	case 6: // *
		state = 7;
		break;
	case 7:
		state = 0;
		break;
#endif
	}

	sol = 0;
	return c;
}

static inline int issym(int c)
{
	return isalnum(c) || c == '_';
}

static int get_token(char *token, FILE *fp)
{
	int c = getch(fp);

	// keep the # with the #define
	if (issym(c) || c == '#') {
		do {
			*token++ = c;
			// while we are parsing a sym it is safe to call the
			// lowest level function. This gets around problems with
			// NL and sol.
			c = __getch(fp);
		} while (issym(c));
		ungetch(c, fp);
		*token = 0;
		return 0;
	}

	return c;
}

static void strip_attributes(char *line, char **p)
{
	char *s = strstr(line, "__attribute__");
	if (!s)
		return; // 97.6% case

	char *e = s + 13;
	if (*e == ' ') ++e;
	if (*e != '(')
		return; // for example #ifndef __attribute__
	++e;

	int count = 1;
	while (count > 0 && *e) {
		if (*e == '(')
			++count;
		else if (*e == ')')
			--count;
		++e;
	}

	int len = e - s;
	memmove(s, e, strlen(s) - len + 1);

	*p -= len;
	**p = 0;
}

// Get a line. Ignore extern lines. Drop bodies.
static char *get_line(char *str, int len, FILE *fp)
{
	int c;
	char *p;
	int plen;

	memset(str, 0, len);

again:
	p = str;
	plen = len;
	c = getch(fp);
	if (c == EOF)
		return NULL;
	lineno = cur_line + 1;
	while (1) {
		if (c == '\n' || c == EOF) {
			// This is specific to work
			if (strncmp(str, "__SRCVERSION", 12) == 0)
				goto again;
			if (strncmp(str, "__END_DECLS", 11) == 0)
				goto again;
			// End of work specific

			*p = 0;
			if (*str != '#')
				// attributes on defines are generally safe
				strip_attributes(str, &p);

			// This can happen with 'code; // comment'
			if (*(p - 1) == ' ')
				*(--p) = 0;

			if (strncmp(str, "typedef", 7) == 0) {
				// This is to deal with typedef struct foo { bar } foo;
				if (*(p - 1) != ';') {
					// need more
					c = getch(fp);
					continue;
				}
			}
			if (*str == '#') {
				*p = 0;
				return str;
			}
			if (*(p - 1) == ';' || *(p - 1) == '}') {
				// good line
				*p = 0;
				if (strncmp(str, "extern", 6) == 0)
					goto again;
				return str;
			}
			if (c == EOF)
				return NULL;
			// need more - keep going
			*p++ = ' ';
		} else {
			*p++ = c;
			--plen;
			if (plen == 1) {
				fprintf(stderr, "%s:%d Line truncated\n", cur_fname, lineno);
				*p = 0;
				return str;
			}
		}
		c = getch(fp);
	}
}

/* Returns 0 if not a function, 1 if function, 2 if reference */
static int try_match_func(char *line, char *func, FILE *fp)
{
	regmatch_t match[2];
	if (regexec(&func_re, line, 2, match, 0))
		return 0;

	int len = match[1].rm_eo - match[1].rm_so;
	memcpy(func, line + match[1].rm_so, len);
	func[len] = 0;

	char *p = line + match[0].rm_eo;

	if (*p == ' ') ++p;
	if (*p == ';')
		return 2; //forward reference
	if (*p == '{')
		return 1; // func - normal case

	// Look for {. This is to remove old-school arg definitions
	while (!strchr(line, '{'))
		if (!get_line(line, LINE_SIZE, fp))
			return 1;

	return 1;
}

static void add_file(const char *fname)
{
	cur_fname = fname;
	cur_line = 0;
	lineno = 0;
	sol = 1;

	if (out == NULL) {
		out = fopen("motley.out", "w");
		if (!out) {
			perror("motley.out");
			exit(1);
		}
	}

	fprintf(out, "@%s\n", fname);
}

/* Returns where it stopped in in */
static char *get_sym(char *in, char *out)
{
	if (*in == ' ') ++in;
	while (*in == '*')
		++in;
	if (*in == ' ') ++in;

	while (issym(*in))
		*out++ = *in++;
	*out = 0;

	return in;
}

static int get_word_backwards(char *start, char *p, char *word)
{
	if (p == NULL) {
		p = strchr(start, ';');
		if (p == NULL) return 0;
		--p;
	}

	if (isspace(*p)) --p;

	if (*p == ')') {
		// pointer to function
		int count = 1;
		--p;
		while (p > start && count > 0) {
			if (*p == ')')
				++count;
			else if (*p == '(')
				--count;
			--p;
		}
		if (isspace(*p)) --p;
		if (*p == ')') --p;
		if (isspace(*p)) --p;
	} else if (*p == ']') {
		--p;
		while (p > start && *p != '[') --p;
		if (*p == '[') --p;
		if (isspace(*p)) --p;
	}

	while ((isalnum(*p) || *p == '_') && p >= start) --p;
	++p;
	while (isalnum(*p) || *p == '_')
		*word++ = *p++;
	*word = 0;
	return 1;
}

static int process_globals(char *line)
{
	char word[128];

	if (strncmp(line, "static", 6) == 0)
		for (line += 6; isspace(*line); ++line) ;

	char *p = strchr(line, ',');
	if (p) {
		process_globals(p + 1);
		*p++ = ';';
		*p = 0;
	}

	p = strchr(line, '[');
	if (p) {
		get_word_backwards(line, p - 1, word);
		out_sym('g', lineno, word);
		return 1;
	}

	p = strchr(line, '=');
	if (p)
		get_word_backwards(line, p - 1, word);
	else {
		p = strchr(line, ';');
		if (!p) return 0;
		get_word_backwards(line, p - 1, word);
	}

	out_sym('g', lineno, word);
	return 1;
}

// Of course enums have to be special.
static void handle_enum(char *line)
{
	char sym[128];

	char *p = get_sym(line + 4, sym);
	if (strchr(p, '{')) {
		// enum definition
		out_sym('e', lineno, sym);

		if (*p == ' ') ++p;
		assert(*p == '{');
		++p;

		while (1) {
			p = get_sym(p, sym);
			out_sym('e', lineno, sym);

			if (*p == ' ') ++p;
			switch (*p) {
			case ',':
				++p;
				break;
			case '=':
				for (++p; *p != ','; ++p)
					if (*p == '}')
						return;
					else if (!*p) {
						fprintf(stderr, "%s:%d: enum error\n", cur_fname, cur_line);
						return;
					}
				++p;
				break;
			case '}':
				return; // done
			default:
				fprintf(stderr, "%s:%d: enum unexpected %c\n", cur_fname, cur_line, *p);
				return;
			}
		}
	} else {
		// global variable of type enum
		get_sym(p, sym);
		out_sym('g', lineno, sym);
	}
}

static void handle_struct(char *line, char type)
{
	char sym[128], *e;

	// we already incremented past struct or union
	char *p = get_sym(line, sym);

	if ((e = strchr(p, '{'))) {
		// struct definition
		if (*sym)
			out_sym(type, lineno, sym);
		++e;
		if (*e == '}') ++e;

		get_sym(e, sym);
		if (*sym)
			out_sym('g', lineno, sym);
		return;
	}

	if (*p == ' ') ++p;
	if (*p == ';') {
		// forward declaration
		return;
	}

	process_globals(line);
}

static int process_one(const char *fname)
{
	FILE *fp = fopen(fname, "r");
	if (!fp) {
		perror(fname);
		return 1;
	}

	add_file(fname);
	if (verbose > 1) printf("%s\n", cur_fname);

	int rc;
	char line[LINE_SIZE], func[126];
	while (get_line(line, sizeof(line), fp)) {
		if (verbose > 2) printf(">> %s:%d: %s\n", cur_fname, lineno, line);
		if (*line == '#') {
			assert(strncmp(line, "#define", 7) == 0);
			get_sym(line + 7, func);
			out_sym('#', lineno, func);
		} else if (strncmp(line, "typedef", 7) == 0) {
			get_word_backwards(line, NULL, func);
			out_sym('t', lineno, func);
		} else if ((rc = try_match_func(line, func, fp))) {
			if (rc == 1)
				out_sym('$', lineno, func);
		} else if (strncmp(line, "struct", 6) == 0) {
			handle_struct(line + 6, 's');
		} else if (strncmp(line, "union", 5) == 0) {
			handle_struct(line + 5, 'u');
		} else if (strncmp(line, "enum", 4) == 0) {
			handle_enum(line);
		} else if (strncmp(line, "namespace", 9) == 0) {
			// silently ignore namespace entries
		} else if (!process_globals(line)) {
			if(verbose) printf("%s:%d Hmmm %s\n", cur_fname, lineno, line);
		}
	}

	fclose(fp);
	return 0;
}

static void dump_one(const char *fname, int level)
{
	FILE *fp = fopen(fname, "r");
	if (!fp) {
		perror(fname);
		return;
	}

	out = stdout;
	add_file(fname);

	char line[LINE_SIZE];
	int c;

	switch (level) {
	case 1: // lines
		while (get_line(line, sizeof(line), fp))
			printf("%d: %s\n", lineno, line);
		break;
	case 2: // tokens
		while ((c = get_token(line, fp)) != EOF)
			if (c == 0) // token
				printf("'%s'", line);
			else
				putchar(c);
		break;
	case 3: // chars
		while ((c = getch(fp)) != EOF)
			putchar(c);
		putchar('\n');
		break;
	default:
		fprintf(stderr, "Unsupported dump level %d\n", level);
	}

	fclose(fp);
}

static int do_lookup(const char *sym)
{
	static char *types[] = {
		"$()",
		"# define",
		"t typedef",
		"s struct",
		"u union",
		"e enum",
		"g global",
		"X unknown"
	};

	char fname[256];
	int i, rc = 1, len = strlen(sym);

	FILE *fp = fopen("motley.out", "r");
	if (!fp) {
		perror("motley.out");
		return 1;
	}

	char line[256], *e;
	while (fgets(line, sizeof(line), fp))
		if (*line == '@')
			strcpy(fname, line + 1);
		else if (strncmp(line, sym, len) == 0 && line[len] == ' ') {
			int lineno = strtol(line + len, &e, 10);
			if (lineno > 0) {
				if (*e == ' ') ++e;
				for (i = 0; i < (sizeof(types) / sizeof(char *)) - 1; ++i)
					if (*types[i] == *e)
						break;
				if ((e = strchr(fname, '\n'))) *e = 0;
				printf("%s:%d:1    %s%s\n", fname, lineno, sym, types[i] + 1);
				rc = 0;
			}
		}

	fclose(fp);
	return rc;
}

int main(int argc, char *argv[])
{
	int c, dump_only = 0, lookup = 0;

	while ((c = getopt(argc, argv, "DlLv")) != EOF)
		switch (c) {
		case 'D':
			++dump_only;
			break;
		case 'l':
		case 'L':
			lookup = 1;
			break;
		case 'v':
			++verbose;
			break;
		default:
			puts("Sorry!");
			exit(1);
		}

	if (lookup) {
		if (optind == argc) {
			fputs("Lookup what?\n", stderr);
			exit(1);
		}
		return do_lookup(argv[optind]);
	}

	// Note: (asctime)(const struct tm *t)
	int rc = regcomp(&func_re, FUNC_RE, REG_EXTENDED);
	if (rc) {
		char err[100];
		regerror(rc, &func_re, err, sizeof(err));
		fprintf(stderr, "regcomp func: %s\n", err);
		exit(1);
	}

	if (dump_only) {
		for (int arg = optind; arg < argc; ++arg)
			dump_one(argv[arg], dump_only);
		exit(0);
	}

	if (optind == argc) {
		// first check for motley.files
		FILE *fp = fopen("motley.files", "r");
		if (fp) {
			char line[1024];
			while (fgets(line, sizeof(line), fp)) {
				strtok(line, "\r\n");
				process_one(line);
			}

			fclose(fp);
		} else if (errno != ENOENT) {
			perror("motley.files");
			exit(1);
		}
	} else
		for (int arg = optind; arg < argc; ++arg)
			process_one(argv[arg]);

	rc = 0; // reset

	if (out) {
		if (ferror(out)) {
			perror("error motley.out");
			rc = 1;
		}

		if (fclose(out)) {
			perror("close motley.out");
			rc = 1;
		}
	}

	return rc;
}
