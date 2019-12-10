#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <ctype.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/file.h>

/* This deletes old entries... where old is last week or older.
 *
 * We keep a flock() lock on the file to protect ourselves from
 * claws-mail.
 */

static char *outbuf;
static int outbuf_len;

static int modified; // did we delete at least one entry


static void logit(const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	vprintf(fmt, ap);
	va_end(ap);
}

static void file_error(const char *fname, const char *what)
{
	logit("%s %s: %s\n", what, fname, strerror(errno));
}

static int weekno(int year, int month, int day)
{
	struct tm this = { 0 };

	this.tm_year = year - 1900;
	this.tm_mon = month - 1;
	this.tm_mday = day;
	this.tm_isdst = -1; // look it up

	if (mktime(&this) == (time_t)-1)
		return 0; // do not skip

	char str[4];
	strftime(str, sizeof(str), "%W", &this);
	int week = strtol(str, NULL, 10);
	return week;
}

static void add_line(char *line, int *deleting)
{
	static int saw_empty;

	if (line == NULL) {
		if (saw_empty)
			// We guarantee an empty line is 1 byte
			--outbuf_len;
		return;
	}

	if (*deleting) {
		if (*line == '\t')
			return; // continuation line, delete
		*deleting = 0;
	}

	// We try to collapse empty lines into one line
	const char *p = line;
	while (isspace(*p)) ++p;
	if (*p == 0) {
		// empty line
		if (saw_empty)
			return; // delete
		saw_empty = 1;
		strcpy(line, "\n");
	} else
		saw_empty = 0;

	int len = strlen(line);
	outbuf = realloc(outbuf, outbuf_len + len);
	if (!outbuf) {
		logit("Out of memory!");
		exit(1);
	}
	memcpy(outbuf + outbuf_len, line, len);
	outbuf_len += len;
}

static void process_buffer(FILE *fp)
{
	int month, day, year;

	time_t now = time(NULL);
	struct tm *tm = localtime(&now);
	tm->tm_year += 1900;
	tm->tm_mon++; // we want 1 based

	int this_week = weekno(tm->tm_year, tm->tm_mon, tm->tm_mday);
	int deleting = 0;

	char line[1024];
	while (fgets(line, sizeof(line), fp)) {
		if (!deleting && sscanf(line, "%d/%d/%d", &month, &day, &year) == 3) {
			int week = weekno(year, month, day);

			if ((tm->tm_year == year && this_week > week) || tm->tm_year > year) {
				deleting = 1;
				modified = 1;
				continue; // delete this line
			}
		}
		add_line(line, &deleting);
	}

	add_line(NULL, NULL);
}

int main(int argc, char *argv[])
{
	int rc = 1;

	if (argc == 1) {
		puts("I need a diary file.");
		exit(1);
	}

	char *diary = argv[1];
	FILE *fp = fopen(diary, "r+");
	if (!fp) {
		file_error(diary, "open");
		exit(1);
	}
	int fd = fileno(fp);

	if (flock(fd, LOCK_EX)) {
		file_error(diary, "flock");
		goto done;
	}

	process_buffer(fp);

	if (ferror(fp)) {
		file_error(diary, "file error");
		goto done;
	}

	if (modified) {
		if (lseek(fd, 0, SEEK_SET)) {
			file_error(diary, "lseek");
			goto done;
		}

		if (write(fd, outbuf, outbuf_len) != outbuf_len) {
			file_error(diary, "write");
			goto done;
		}

		if (ftruncate(fd, outbuf_len)) {
			file_error(diary, "ftruncate");
			goto done;
		}
	}

	rc = 0;

done:
	fclose(fp);
	return rc;
}

/*
 * Local Variables:
 * compile-command: "gcc -O2 -Wall diary-cleanup.c -o diary-cleanup"
 * End:
 */
