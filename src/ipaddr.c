/* ipaddr.c - get various IP information
 * Copyright (C) 2012-2019 Sean MacLennan
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this project; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <dirent.h>
#include <errno.h>
#include <ifaddrs.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ether.h>
#include <arpa/inet.h>
#include <net/if.h>
#include <net/route.h>
#include <netdb.h>

#define W_ADDRESS  (1 <<  0)
#define W_MASK     (1 <<  1)
#define W_SUBNET   (1 <<  2)
#define W_BITS     (1 <<  3)
#define W_GATEWAY  (1 <<  4)
#define W_GUESSED  (1 <<  5)
#define W_ALL	   (1 <<  6)
#define W_FLAGS    (1 <<  7)
#define W_SET      (1 <<  8)
#define W_QUIET    (1 <<  9)
#define W_MAC      (1 << 10)

#if defined(__linux__)
/* Returns the size of src */
size_t strlcpy(char *dst, const char *src, size_t dstlen)
{
	int srclen = strlen(src);

	if (dstlen > 0) {
		if (dstlen > srclen)
			strcpy(dst, src);
		else {
			strncpy(dst, src, dstlen - 1);
			dst[dstlen - 1] = 0;
		}
	}

	return srclen;
}

/* Returns 0 on success, < 0 for errors, and > 0 if ifname not found.
 * The gateway arg can be NULL.
 */
static int get_gateway(const char *ifname, struct in_addr *gateway)
{
	FILE *fp = fopen("/proc/net/route", "r");
	if (!fp)
		return -1;

	char line[128], iface[8];
	uint32_t dest, gw, flags;
	while (fgets(line, sizeof(line), fp))
		if (sscanf(line, "%s %x %x %x", iface, &dest, &gw, &flags) == 4 &&
			strcmp(iface, ifname) == 0 && dest == 0 && (flags & 2)) {
			fclose(fp);
			if (gateway)
				gateway->s_addr = gw;
			return 0;
		}

	fclose(fp);
	return 1;
}

static int set_gateway(const char *gw)
{
	int fd = socket(PF_INET, SOCK_DGRAM, IPPROTO_IP);
	if (fd == -1)
		return -1;

	struct rtentry rtreq;
	memset(&rtreq, 0, sizeof(rtreq));
	rtreq.rt_flags = (RTF_UP | RTF_GATEWAY);

	rtreq.rt_gateway.sa_family = AF_INET;
	rtreq.rt_genmask.sa_family = AF_INET;
	rtreq.rt_dst.sa_family = AF_INET;

	struct sockaddr_in *sa = (struct sockaddr_in*)&rtreq.rt_gateway;
	sa->sin_addr.s_addr = inet_addr(gw);

	int rc = ioctl(fd, SIOCADDRT, &rtreq);
	close(fd);
	return rc;
}

static int get_hw_addr(const char *ifname, unsigned char *hwaddr)
{
	int sock = socket(PF_INET, SOCK_DGRAM, IPPROTO_IP);
	if (sock == -1)
		return -1;

	struct ifreq ifreq;
	memset(&ifreq, 0, sizeof(ifreq));
	strlcpy(ifreq.ifr_name, ifname, IF_NAMESIZE);
	int rc = ioctl(sock, SIOCGIFHWADDR, &ifreq);
	close(sock);

	memcpy(hwaddr, ifreq.ifr_hwaddr.sa_data, ETHER_ADDR_LEN);
	return rc;
}
#else
#include <net/route.h>
#include <sys/poll.h>
#include <sys/sysctl.h>

#define RTM_ADDRS ((1 << RTAX_DST) | (1 << RTAX_NETMASK))
#define RTM_SEQ 42
#define RTM_FLAGS (RTF_STATIC | RTF_UP | RTF_GATEWAY)
#define	READ_TIMEOUT 10

struct rtmsg {
	struct rt_msghdr hdr;
	unsigned char data[512];
};

static int rtmsg_send(int s, int cmd, const char *gw)
{
	struct rtmsg rtmsg;

	memset(&rtmsg, 0, sizeof(rtmsg));
	rtmsg.hdr.rtm_type = cmd;
	rtmsg.hdr.rtm_flags = RTM_FLAGS;
	rtmsg.hdr.rtm_version = RTM_VERSION;
	rtmsg.hdr.rtm_seq = RTM_SEQ;
	rtmsg.hdr.rtm_addrs = RTM_ADDRS;

	struct sockaddr_in *sa = (struct sockaddr_in *)rtmsg.data;
	sa->sin_len = sizeof(struct sockaddr_in); // DST
	sa->sin_family = AF_INET;
	++sa;
	if (cmd != RTM_GET) {                     // GATEWAY
		rtmsg.hdr.rtm_addrs |= 1 << RTAX_GATEWAY;
		sa->sin_len = sizeof(struct sockaddr_in);
		sa->sin_family = AF_INET;
		sa->sin_addr.s_addr = inet_addr(gw);
		++sa;
	}
	sa->sin_len = sizeof(struct sockaddr_in); // NETMASK
	sa->sin_family = AF_INET;
	++sa;

	rtmsg.hdr.rtm_msglen = (uintptr_t)sa - (uintptr_t)&rtmsg;
	if (write(s, &rtmsg, rtmsg.hdr.rtm_msglen) < 0)
		return -1;

	return 0;
}

static int rtmsg_recv(int s, struct in_addr *gateway)
{
	struct rtmsg rtmsg;
	struct pollfd ufd = { .fd = s, .events = POLLIN };

	do {
		if (poll(&ufd, 1, READ_TIMEOUT * 1000) <= 0)
			return -1;

		if (read(s, (char *)&rtmsg, sizeof(rtmsg)) <= 0)
			return -1;
	} while (rtmsg.hdr.rtm_type != RTM_GET ||
			 rtmsg.hdr.rtm_seq != RTM_SEQ ||
			 rtmsg.hdr.rtm_pid != getpid());

	if (rtmsg.hdr.rtm_version != RTM_VERSION)
		return -1;
	if (rtmsg.hdr.rtm_errno)  {
		errno = rtmsg.hdr.rtm_errno;
		return -1;
	}

	unsigned char *cp = rtmsg.data;
	for (int i = 0; i < RTAX_MAX; i++)
		if (rtmsg.hdr.rtm_addrs & (1 << i)) {
			if (i == RTAX_GATEWAY) {
				*gateway = ((struct sockaddr_in *)cp)->sin_addr;
				return 0;
			}
			cp += ((struct sockaddr *)cp)->sa_len;
		}

	return 1; /* not found */
}

/* ifname ignored */
static int get_gateway(const char *ifname, struct in_addr *gateway)
{
	int s = socket(PF_ROUTE, SOCK_RAW, 0);
	if (s < 0)
		return -1;

	if (rtmsg_send(s, RTM_GET, NULL)) {
		close(s);
		return -1;
	}

	int n = rtmsg_recv(s, gateway);
	if (n) {
		close(s);
		return n;
	}

	close(s);
	return 0;
}

static int set_gateway(const char *gw)
{
	int s = socket(PF_ROUTE, SOCK_RAW, 0);
	if (s < 0)
		return -1;

	shutdown(s, SHUT_RD); /* Don't want to read back our messages */

	if (rtmsg_send(s, RTM_ADD, gw) == 0) {
		close(s);
		return 0;
	}

	if (errno == EEXIST)
		if (rtmsg_send(s, RTM_CHANGE, gw) == 0) {
			close(s);
			return 0;
		}

	close(s);
	return -1;
}

static int get_hw_addr(const char *ifname, unsigned char *hwaddr)
{
	struct ifaddrs *ifa = NULL;
	struct sockaddr_dl *sa = NULL;

	if (getifaddrs(&ifa))
		return -1;

	for (struct ifaddrs *p = ifa; p; p = p->ifa_next) {
		if (p->ifa_addr->sa_family == AF_LINK &&
			strcmp(p->ifa_name, ifname) == 0) {
			sa = (struct sockaddr_dl *)p->ifa_addr;
			if (sa->sdl_type == 1 || sa->sdl_type == 6) { // ethernet
				memcpy(hwaddr, LLADDR(sa), sa->sdl_alen);
				freeifaddrs(ifa);
				return 0;
			} else
				return -1;
		}
	}

	errno = ENOENT;
	return -1;
}
#endif

/* This is so fast, it is not worth optimizing. */
static int maskcnt(unsigned mask)
{
	unsigned count = 32;

	mask = ntohl(mask);
	while ((mask & 1) == 0) {
		mask >>= 1;
		--count;
	}

	return count;
}

/* Returns 0 on success. The args addr and/or mask and/or gw can be NULL. */
static int ip_addr(const char *ifname,
				   struct in_addr *addr, struct in_addr *mask, struct in_addr *gw)
{
	int s = socket(AF_INET, SOCK_DGRAM, 0);
	if (s < 0)
		return -1;

	struct ifreq ifr;
	strlcpy(ifr.ifr_name, ifname, IFNAMSIZ);

	if (addr) {
		if (ioctl(s, SIOCGIFADDR, &ifr) < 0)
			goto failed;
		*addr = ((struct sockaddr_in *)&ifr.ifr_addr)->sin_addr;
	}

	if (mask) {
		// We need this zero for QNX
		((struct sockaddr_in *)&ifr.ifr_addr)->sin_addr.s_addr = 0;
		if (ioctl(s, SIOCGIFNETMASK, &ifr) < 0)
			goto failed;
		*mask = ((struct sockaddr_in *)&ifr.ifr_addr)->sin_addr;
	}

	close(s);

	if (gw)
		return get_gateway(ifname, gw);

	return 0;

failed:
	close(s);
	return -1;
}

static int set_ip(const char *ifname, const char *ip, unsigned mask)
{
	int s = socket(AF_INET, SOCK_DGRAM, 0);
	if (s == -1) {
		perror("set_ip socket");
		return -1;
	}

	struct ifreq req;
	memset(&req, 0, sizeof(req));

	strlcpy(req.ifr_name, ifname, IF_NAMESIZE);

	struct sockaddr_in *in = (struct sockaddr_in *)&req.ifr_addr;
#ifndef __linux__
	in->sin_len = sizeof(struct sockaddr_in);
#endif
	in->sin_family = AF_INET;
	in->sin_addr.s_addr = inet_addr(ip);

	errno = 0;
	if (ioctl(s, SIOCSIFADDR, &req)) {
		perror("SIOCSIFADDR");
		goto failed;
	}

 	in->sin_addr.s_addr = mask;
	if (ioctl(s, SIOCSIFNETMASK, &req)) {
		perror("SIOCSIFNETMASK");
		goto failed;
	}

	req.ifr_flags = IFF_UP | IFF_RUNNING;
	if (ioctl(s, SIOCSIFFLAGS, &req)) {
		perror("SIOCSIFFLAGS");
		goto failed;
	}

	close(s);
	return 0;

failed:
	close(s);
	return -1;
}

static char *ip_flags(const char *ifname)
{
	static char flagstr[64];
	struct ifreq ifreq;

	int sock = socket(AF_INET, SOCK_DGRAM, 0);
	if (sock < 0)
		return "failed";

	memset(&ifreq, 0, sizeof(ifreq));
	strlcpy(ifreq.ifr_name, ifname, IF_NAMESIZE);
	if (ioctl(sock, SIOCGIFFLAGS, &ifreq)) {
		return "Failed";
	}

	strcpy(flagstr, (ifreq.ifr_flags & IFF_UP) ? "UP" : "DOWN");
	if (ifreq.ifr_flags & IFF_RUNNING)
		strcat(flagstr, ",RUNNING");

	return flagstr;
}

static int check_one(const char *ifname, int state, unsigned what)
{
	int n = 0;
	struct in_addr addr, mask, gw, *gw_ptr = NULL;
	char mac_str[ETHER_ADDR_LEN * 3];

	if (what & W_GATEWAY)
		/* gateway is more expensive */
		gw_ptr = &gw;

	int rc = ip_addr(ifname, &addr, &mask, gw_ptr);
	if (what & W_QUIET)
		return !!rc;

	if (what & W_MAC) {
		unsigned char mac[ETHER_ADDR_LEN];
		if (get_hw_addr(ifname, mac))
			return 1;
		for (int i = 0; i < ETHER_ADDR_LEN; ++i)
			sprintf(mac_str + (i * 3), "%02x:", mac[i]);
		mac_str[sizeof(mac_str) - 1] = 0;
	}

	if (rc) {
		if (errno == EADDRNOTAVAIL) {
			if (what & W_ALL) {
				/* not an error, they asked for down interfaces */
				if (state)
					fputs("0.0.0.0", stdout);
				else
					fputs("down", stdout);
				if (what & W_MAC)
					printf(" %s", mac_str);
				printf(" (%s)\n", ifname);
				return 0;
			} else if ((what & W_GUESSED) == 0)
				fprintf(stderr, "%s: No address\n", ifname);
		} else
			perror(ifname);
		return 1;
	}

	if (what & W_ADDRESS) {
		++n;
		printf("%s", inet_ntoa(addr));
		if (what & W_BITS)
			printf("/%d", maskcnt(mask.s_addr));
	}
	if (what & W_SUBNET) {
		addr.s_addr &= mask.s_addr;
		if (n++)
			putchar(' ');
		printf("%s", inet_ntoa(addr));
		if (what & W_BITS)
			printf("/%d", maskcnt(mask.s_addr));
	}
	if (what & W_MASK) {
		if (n++)
			putchar(' ');
		printf("%s", inet_ntoa(mask));
	}

	if (what & W_GATEWAY) {
		if (n++)
			putchar(' ');
		printf("%s", inet_ntoa(gw));
	}

	if (what & W_FLAGS) {
		if (n++)
			putchar(' ');
		printf("<%s>", ip_flags(ifname));
	}

	if (what & W_MAC) {
		if (n++)
			putchar(' ');
		fputs(mac_str, stdout);
	}

	if (n) {
		if (what & W_GUESSED)
			printf(" (%s)", ifname);
		putchar('\n');
	}

	return 0;
}

static void usage(int rc)
{
	fputs("usage: ipaddr [-abgimsqM] [interface]\n"
		  "       ipaddr -S <interface> <ip> <mask> [gateway]\n"
		  "where: -i displays IP address (default)\n"
		  "		  -f display up and running flags\n"
		  "       -g displays gateway\n"
		  "       -m displays network mask\n"
		  "       -s displays subnet\n"
		  "       -b add bits as /bits to -a and/or -s\n"
		  "       -a displays all interfaces (even down)\n"
		  "       -q quiet, return error code only\n"
		  "       -M display hardware address (mac)\n"
		  "Interface defaults to eth0.\n\n"
		  "-q returns 0 if the interface (or gw) is up and has an IP address.\n\n"
		  "Designed to be easily used in scripts. All error output to stderr.\n",
		  stderr);

	exit(rc);
}

int main(int argc, char *argv[])
{
	int c, rc = 0;
	unsigned what = 0;

	while ((c = getopt(argc, argv, "abfgmishqSM")) != EOF)
		switch (c) {
		case 'i':
			what |= W_ADDRESS;
			break;
		case 'b':
			what |= W_BITS;
			break;
		case 'f':
			what |= W_FLAGS;
			break;
		case 'g':
			what |= W_GATEWAY;
			break;
		case 'm':
			what |= W_MASK;
			break;
		case 's':
			what |= W_SUBNET;
			break;
		case 'a':
			what |= W_ALL;
			break;
		case 'h':
			usage(0);
		case 'q':
			what |= W_QUIET;
			break;
		case 'S':
			what |= W_SET;
			break;
		case 'M':
			what |= W_MAC;
			break;
		default:
			exit(2);
		}

	if (what & W_SET) {
		if ((what & ~W_SET) || argc - optind < 2)
			usage(1);
		char *ifname = argv[optind++];
		char *ip = argv[optind++];
		char *p = strchr(ip, '/');
		unsigned mask = 0;
		if (p) {
			*p++ = 0;
			unsigned bits = strtol(p, NULL, 10);
			mask = htonl(((1ul << bits) - 1) << (32 - bits));
		} else if (optind < argc) {
			mask = inet_addr(argv[optind]);
			++optind;
		} else
			usage(1);

		if (set_ip(ifname, ip, mask))
			exit(1);
		if (optind < argc) {
			if (set_gateway(argv[optind])) {
				perror("set_gateway");
				exit(1);
			}
		}
		return 0;
	}

	if ((what & ~(W_BITS | W_ALL | W_QUIET)) == 0)
		what |= W_ADDRESS;

	if (optind < argc) {
		while (optind < argc)
			rc |= check_one(argv[optind++], 0, what);
	} else {
		struct ifaddrs *ifa;

		if (getifaddrs(&ifa)) {
			perror("getifaddrs");
			exit(1);
		}

		for (struct ifaddrs *p = ifa; p; p = p->ifa_next) {
			if (p->ifa_addr->sa_family != AF_INET || (p->ifa_flags & IFF_LOOPBACK))
				continue;

			unsigned up = p->ifa_flags & IFF_UP;
			if ((what & W_ALL) || up)
				rc |= check_one(p->ifa_name, up, what | W_GUESSED);
		}
	}

	return rc;
}

/*
 * Local Variables:
 * compile-command: "gcc -O2 -Wall ipaddr.c -o ipaddr"
 * End:
 */
