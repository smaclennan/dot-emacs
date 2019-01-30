#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/file.h>

/* This deletes old entries... where old is last week or older.
 *
 * We read the file into memory, edit it in memory, then write it back
 * out if necessary. This is done so we can keep a flock() lock on the
 * file and protect ourselves from claws-mail.
 */

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

static int delete_entry(char * const base, char *start, int *len)
{
	char *end = strchr(start, '\n');
	if (!end) return 0;
	++end;

	while (*end == '\t') {
		char *p = strchr(end, '\n');
		if (!p) return 0;
		end = p + 1;
	}

	if (*end == '\n') ++end;

	memmove(start, end, base + *len - end + 1);
	*len -= (end - start);

	return 1;
}

static int process_buffer(char * const buf, int len)
{
	int rc_len = len;
	int month, day, year;

	time_t now = time(NULL);
	struct tm *tm = localtime(&now);
	tm->tm_year += 1900;
	tm->tm_mon++; // we want 1 based

	int this_week = weekno(tm->tm_year, tm->tm_mon, tm->tm_mday);

	char *p = buf;
	while (p) {
		while (*p == '\n' || *p == '\r') ++p;
		if (sscanf(p, "%d/%d/%d", &month, &day, &year) == 3) {
			int week = weekno(year, month, day);

			if ((tm->tm_year == year && this_week > week) || tm->tm_year > year) {
				if (delete_entry(buf, p, &rc_len))
					continue; // we have updated p indirectly
			}
		}
		p = strchr(p, '\n');
	}

	return rc_len;
}

int main(int argc, char *argv[])
{
	int rc = 1;

	if (argc == 1) {
		puts("I need a diary file.");
		exit(1);
	}

	char *diary = argv[1];
	int fd = open(diary, O_RDWR);
	if (fd < 0) {
		file_error(diary, "open");
		exit(1);
	}

	if (flock(fd, LOCK_EX)) {
		file_error(diary, "flock");
		goto done;
	}

	struct stat sbuf;
	if (fstat(fd, &sbuf)) {
		file_error(diary, "fstat");
		goto done;
	}

	char *buf = malloc(sbuf.st_size + 1);
	if (!buf) {
		logit("Out of memory!\n");
		goto done;
	}

	if (read(fd, buf, sbuf.st_size) != sbuf.st_size) {
		file_error(diary, "read");
		goto done;
	}
	buf[sbuf.st_size] = 0;

	int len = process_buffer(buf, sbuf.st_size);
	if (len != sbuf.st_size) {
		if (lseek(fd, 0, SEEK_SET)) {
			file_error(diary, "lseek");
			goto done;
		}

		if (write(fd, buf, len) != len) {
			file_error(diary, "write");
			goto done;
		}

		if (ftruncate(fd, len)) {
			file_error(diary, "ftruncate");
			goto done;
		}
	}

	rc = 0;

done:
	close(fd);
	return rc;
}

/*
 * Local Variables:
 * compile-command: "gcc -O2 -Wall diary-cleanup.c -o diary-cleanup"
 * End:
 */
