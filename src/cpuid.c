#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <ctype.h>
#include <errno.h>

#if defined(__x86_64__) || defined(__i386__) || defined(WIN32)

/* Warning: only tested on x86_64 */

static int cpuid(uint32_t id, uint32_t *regs)
{
#ifdef WIN32
	__cpuid(regs, id);
#else
	asm volatile
		("cpuid"
		 : "=a" (regs[0]), "=b" (regs[1]), "=c" (regs[2]), "=d" (regs[3])
		 : "a" (id), "c" (0));
#endif
	return 0;
}

static char *cpu_vendor(char *vendor)
{
	uint32_t regs[4];

	cpuid(0, regs);

	memcpy(vendor, &regs[1], sizeof(uint32_t));
	memcpy(vendor + 4, &regs[3], sizeof(uint32_t));
	memcpy(vendor + 8, &regs[2], sizeof(uint32_t));
	vendor[12] = 0;

	if (strcmp(vendor, "GenuineIntel") == 0)
		strcpy(vendor, "Intel");
	else if (strstr(vendor, "AMD"))
		strcpy(vendor, "AMD");
	else if (strcmp(vendor, "CentaurHauls") == 0)
		strcpy(vendor, "VIA");

	return vendor;
}

static char *cpu_model(char *model)
{
	uint32_t regs[4];

	cpuid(0x80000000, regs);
	if (regs[0] < 0x80000004)
		return "unknown";

	cpuid(0x80000002, regs);
	memcpy(model, regs, sizeof(regs));

	cpuid(0x80000003, regs);
	memcpy(model + 16, regs, sizeof(regs));

	cpuid(0x80000004, regs);
	memcpy(model + 32, regs, sizeof(regs));

	model[48] = 0;

	int i;
	for (i = 0; isspace(model[i]); ++i) ;
	if (i)
		// Remove leading spaces
		memmove(model, model + i, 48 - i);

	return model;
}

int main(int argc, char *argv[])
{
	char model[49];
	char vendor[13];
	uint32_t regs[4];

	cpuid(1, regs);

	printf("Model Name : %s\n", cpu_model(model));
	printf("Vendor     : %s\n", cpu_vendor(vendor));
	printf("Family     : %u\n",
		   ((regs[0] >> 8) & 0xf) | ((regs[0] >> (20 - 4)) & 0xf0));
	printf("Model      : %u\n",
		   ((regs[0] >> 4) & 0xf) | ((regs[0] >> (16 - 4)) & 0xf0));
	printf("Stepping   : %u\n", regs[0] & 0xf);

	return 0;
}
#else
#error ARCH not supported
#endif

/*
 * Local Variables:
 * compile-command: "gcc -O2 -Wall cpuid.c -o cpuid"
 * End:
 */
