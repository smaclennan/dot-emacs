#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <ctype.h>
#include <errno.h>

#if defined(__x86_64__) || defined(__i386__) || defined(WIN32)

/*
 * Defines x86 CPU feature bits
 */
#define NCAPINTS			19	   /* N 32-bit words worth of info */
#define NBUGINTS			1	   /* N 32-bit bug flags */

/* Intel-defined CPU features, CPUID level 0x00000001 (EDX), word 0 */
#define X86_FEATURE_FPU			( 0*32+ 0) /* Onboard FPU */
#define X86_FEATURE_VME			( 0*32+ 1) /* Virtual Mode Extensions */
#define X86_FEATURE_DE			( 0*32+ 2) /* Debugging Extensions */
#define X86_FEATURE_PSE			( 0*32+ 3) /* Page Size Extensions */
#define X86_FEATURE_TSC			( 0*32+ 4) /* Time Stamp Counter */
#define X86_FEATURE_MSR			( 0*32+ 5) /* Model-Specific Registers */
#define X86_FEATURE_PAE			( 0*32+ 6) /* Physical Address Extensions */
#define X86_FEATURE_MCE			( 0*32+ 7) /* Machine Check Exception */
#define X86_FEATURE_CX8			( 0*32+ 8) /* CMPXCHG8 instruction */
#define X86_FEATURE_APIC		( 0*32+ 9) /* Onboard APIC */
#define X86_FEATURE_SEP			( 0*32+11) /* SYSENTER/SYSEXIT */
#define X86_FEATURE_MTRR		( 0*32+12) /* Memory Type Range Registers */
#define X86_FEATURE_PGE			( 0*32+13) /* Page Global Enable */
#define X86_FEATURE_MCA			( 0*32+14) /* Machine Check Architecture */
#define X86_FEATURE_CMOV		( 0*32+15) /* CMOV instructions (plus FCMOVcc, FCOMI with FPU) */
#define X86_FEATURE_PAT			( 0*32+16) /* Page Attribute Table */
#define X86_FEATURE_PSE36		( 0*32+17) /* 36-bit PSEs */
#define X86_FEATURE_PN			( 0*32+18) /* Processor serial number */
#define X86_FEATURE_CLFLUSH		( 0*32+19) /* CLFLUSH instruction */
#define X86_FEATURE_DS			( 0*32+21) /* "dts" Debug Store */
#define X86_FEATURE_ACPI		( 0*32+22) /* ACPI via MSR */
#define X86_FEATURE_MMX			( 0*32+23) /* Multimedia Extensions */
#define X86_FEATURE_FXSR		( 0*32+24) /* FXSAVE/FXRSTOR, CR4.OSFXSR */
#define X86_FEATURE_XMM			( 0*32+25) /* "sse" */
#define X86_FEATURE_XMM2		( 0*32+26) /* "sse2" */
#define X86_FEATURE_SELFSNOOP	( 0*32+27) /* "ss" CPU self snoop */
#define X86_FEATURE_HT			( 0*32+28) /* Hyper-Threading */
#define X86_FEATURE_ACC			( 0*32+29) /* "tm" Automatic clock control */
#define X86_FEATURE_IA64		( 0*32+30) /* IA-64 processor */
#define X86_FEATURE_PBE			( 0*32+31) /* Pending Break Enable */

/* AMD-defined CPU features, CPUID level 0x80000001, word 1 */
/* Don't duplicate feature flags which are redundant with Intel! */
#define X86_FEATURE_SYSCALL		( 1*32+11) /* SYSCALL/SYSRET */
#define X86_FEATURE_MP			( 1*32+19) /* MP Capable */
#define X86_FEATURE_NX			( 1*32+20) /* Execute Disable */
#define X86_FEATURE_MMXEXT		( 1*32+22) /* AMD MMX extensions */
#define X86_FEATURE_FXSR_OPT		( 1*32+25) /* FXSAVE/FXRSTOR optimizations */
#define X86_FEATURE_GBPAGES		( 1*32+26) /* "pdpe1gb" GB pages */
#define X86_FEATURE_RDTSCP		( 1*32+27) /* RDTSCP */
#define X86_FEATURE_LM			( 1*32+29) /* Long Mode (x86-64, 64-bit support) */
#define X86_FEATURE_3DNOWEXT		( 1*32+30) /* AMD 3DNow extensions */
#define X86_FEATURE_3DNOW		( 1*32+31) /* 3DNow */

/* Transmeta-defined CPU features, CPUID level 0x80860001, word 2 */
#define X86_FEATURE_RECOVERY		( 2*32+ 0) /* CPU in recovery mode */
#define X86_FEATURE_LONGRUN		( 2*32+ 1) /* Longrun power control */
#define X86_FEATURE_LRTI		( 2*32+ 3) /* LongRun table interface */

/* Other features, Linux-defined mapping, word 3 */
/* This range is used for feature bits which conflict or are synthesized */
#define X86_FEATURE_CXMMX		( 3*32+ 0) /* Cyrix MMX extensions */
#define X86_FEATURE_K6_MTRR		( 3*32+ 1) /* AMD K6 nonstandard MTRRs */
#define X86_FEATURE_CYRIX_ARR		( 3*32+ 2) /* Cyrix ARRs (= MTRRs) */
#define X86_FEATURE_CENTAUR_MCR		( 3*32+ 3) /* Centaur MCRs (= MTRRs) */

/* CPU types for specific tunings: */
#define X86_FEATURE_K8			( 3*32+ 4) /* "" Opteron, Athlon64 */
#define X86_FEATURE_K7			( 3*32+ 5) /* "" Athlon */
#define X86_FEATURE_P3			( 3*32+ 6) /* "" P3 */
#define X86_FEATURE_P4			( 3*32+ 7) /* "" P4 */
#define X86_FEATURE_CONSTANT_TSC	( 3*32+ 8) /* TSC ticks at a constant rate */
#define X86_FEATURE_UP			( 3*32+ 9) /* SMP kernel running on UP */
#define X86_FEATURE_ART			( 3*32+10) /* Always running timer (ART) */
#define X86_FEATURE_ARCH_PERFMON	( 3*32+11) /* Intel Architectural PerfMon */
#define X86_FEATURE_PEBS		( 3*32+12) /* Precise-Event Based Sampling */
#define X86_FEATURE_BTS			( 3*32+13) /* Branch Trace Store */
#define X86_FEATURE_SYSCALL32		( 3*32+14) /* "" syscall in IA32 userspace */
#define X86_FEATURE_SYSENTER32		( 3*32+15) /* "" sysenter in IA32 userspace */
#define X86_FEATURE_REP_GOOD		( 3*32+16) /* REP microcode works well */
#define X86_FEATURE_MFENCE_RDTSC	( 3*32+17) /* "" MFENCE synchronizes RDTSC */
#define X86_FEATURE_LFENCE_RDTSC	( 3*32+18) /* "" LFENCE synchronizes RDTSC */
#define X86_FEATURE_ACC_POWER		( 3*32+19) /* AMD Accumulated Power Mechanism */
#define X86_FEATURE_NOPL		( 3*32+20) /* The NOPL (0F 1F) instructions */
#define X86_FEATURE_ALWAYS		( 3*32+21) /* "" Always-present feature */
#define X86_FEATURE_XTOPOLOGY		( 3*32+22) /* CPU topology enum extensions */
#define X86_FEATURE_TSC_RELIABLE	( 3*32+23) /* TSC is known to be reliable */
#define X86_FEATURE_NONSTOP_TSC		( 3*32+24) /* TSC does not stop in C states */
#define X86_FEATURE_CPUID		( 3*32+25) /* CPU has CPUID instruction itself */
#define X86_FEATURE_EXTD_APICID		( 3*32+26) /* Extended APICID (8 bits) */
#define X86_FEATURE_AMD_DCM		( 3*32+27) /* AMD multi-node processor */
#define X86_FEATURE_APERFMPERF		( 3*32+28) /* P-State hardware coordination feedback capability (APERF/MPERF MSRs) */
#define X86_FEATURE_NONSTOP_TSC_S3	( 3*32+30) /* TSC doesn't stop in S3 state */
#define X86_FEATURE_TSC_KNOWN_FREQ	( 3*32+31) /* TSC has known frequency */

/* Intel-defined CPU features, CPUID level 0x00000001 (ECX), word 4 */
#define X86_FEATURE_XMM3		( 4*32+ 0) /* "pni" SSE-3 */
#define X86_FEATURE_PCLMULQDQ		( 4*32+ 1) /* PCLMULQDQ instruction */
#define X86_FEATURE_DTES64		( 4*32+ 2) /* 64-bit Debug Store */
#define X86_FEATURE_MWAIT		( 4*32+ 3) /* "monitor" MONITOR/MWAIT support */
#define X86_FEATURE_DSCPL		( 4*32+ 4) /* "ds_cpl" CPL-qualified (filtered) Debug Store */
#define X86_FEATURE_VMX			( 4*32+ 5) /* Hardware virtualization */
#define X86_FEATURE_SMX			( 4*32+ 6) /* Safer Mode eXtensions */
#define X86_FEATURE_EST			( 4*32+ 7) /* Enhanced SpeedStep */
#define X86_FEATURE_TM2			( 4*32+ 8) /* Thermal Monitor 2 */
#define X86_FEATURE_SSSE3		( 4*32+ 9) /* Supplemental SSE-3 */
#define X86_FEATURE_CID			( 4*32+10) /* Context ID */
#define X86_FEATURE_SDBG		( 4*32+11) /* Silicon Debug */
#define X86_FEATURE_FMA			( 4*32+12) /* Fused multiply-add */
#define X86_FEATURE_CX16		( 4*32+13) /* CMPXCHG16B instruction */
#define X86_FEATURE_XTPR		( 4*32+14) /* Send Task Priority Messages */
#define X86_FEATURE_PDCM		( 4*32+15) /* Perf/Debug Capabilities MSR */
#define X86_FEATURE_PCID		( 4*32+17) /* Process Context Identifiers */
#define X86_FEATURE_DCA			( 4*32+18) /* Direct Cache Access */
#define X86_FEATURE_XMM4_1		( 4*32+19) /* "sse4_1" SSE-4.1 */
#define X86_FEATURE_XMM4_2		( 4*32+20) /* "sse4_2" SSE-4.2 */
#define X86_FEATURE_X2APIC		( 4*32+21) /* X2APIC */
#define X86_FEATURE_MOVBE		( 4*32+22) /* MOVBE instruction */
#define X86_FEATURE_POPCNT		( 4*32+23) /* POPCNT instruction */
#define X86_FEATURE_TSC_DEADLINE_TIMER	( 4*32+24) /* TSC deadline timer */
#define X86_FEATURE_AES			( 4*32+25) /* AES instructions */
#define X86_FEATURE_XSAVE		( 4*32+26) /* XSAVE/XRSTOR/XSETBV/XGETBV instructions */
#define X86_FEATURE_OSXSAVE		( 4*32+27) /* "" XSAVE instruction enabled in the OS */
#define X86_FEATURE_AVX			( 4*32+28) /* Advanced Vector Extensions */
#define X86_FEATURE_F16C		( 4*32+29) /* 16-bit FP conversions */
#define X86_FEATURE_RDRAND		( 4*32+30) /* RDRAND instruction */
#define X86_FEATURE_HYPERVISOR		( 4*32+31) /* Running on a hypervisor */

/* VIA/Cyrix/Centaur-defined CPU features, CPUID level 0xC0000001, word 5 */
#define X86_FEATURE_XSTORE		( 5*32+ 2) /* "rng" RNG present (xstore) */
#define X86_FEATURE_XSTORE_EN		( 5*32+ 3) /* "rng_en" RNG enabled */
#define X86_FEATURE_XCRYPT		( 5*32+ 6) /* "ace" on-CPU crypto (xcrypt) */
#define X86_FEATURE_XCRYPT_EN		( 5*32+ 7) /* "ace_en" on-CPU crypto enabled */
#define X86_FEATURE_ACE2		( 5*32+ 8) /* Advanced Cryptography Engine v2 */
#define X86_FEATURE_ACE2_EN		( 5*32+ 9) /* ACE v2 enabled */
#define X86_FEATURE_PHE			( 5*32+10) /* PadLock Hash Engine */
#define X86_FEATURE_PHE_EN		( 5*32+11) /* PHE enabled */
#define X86_FEATURE_PMM			( 5*32+12) /* PadLock Montgomery Multiplier */
#define X86_FEATURE_PMM_EN		( 5*32+13) /* PMM enabled */

/* More extended AMD flags: CPUID level 0x80000001, ECX, word 6 */
#define X86_FEATURE_LAHF_LM		( 6*32+ 0) /* LAHF/SAHF in long mode */
#define X86_FEATURE_CMP_LEGACY		( 6*32+ 1) /* If yes HyperThreading not valid */
#define X86_FEATURE_SVM			( 6*32+ 2) /* Secure Virtual Machine */
#define X86_FEATURE_EXTAPIC		( 6*32+ 3) /* Extended APIC space */
#define X86_FEATURE_CR8_LEGACY		( 6*32+ 4) /* CR8 in 32-bit mode */
#define X86_FEATURE_ABM			( 6*32+ 5) /* Advanced bit manipulation */
#define X86_FEATURE_SSE4A		( 6*32+ 6) /* SSE-4A */
#define X86_FEATURE_MISALIGNSSE		( 6*32+ 7) /* Misaligned SSE mode */
#define X86_FEATURE_3DNOWPREFETCH	( 6*32+ 8) /* 3DNow prefetch instructions */
#define X86_FEATURE_OSVW		( 6*32+ 9) /* OS Visible Workaround */
#define X86_FEATURE_IBS			( 6*32+10) /* Instruction Based Sampling */
#define X86_FEATURE_XOP			( 6*32+11) /* extended AVX instructions */
#define X86_FEATURE_SKINIT		( 6*32+12) /* SKINIT/STGI instructions */
#define X86_FEATURE_WDT			( 6*32+13) /* Watchdog timer */
#define X86_FEATURE_LWP			( 6*32+15) /* Light Weight Profiling */
#define X86_FEATURE_FMA4		( 6*32+16) /* 4 operands MAC instructions */
#define X86_FEATURE_TCE			( 6*32+17) /* Translation Cache Extension */
#define X86_FEATURE_NODEID_MSR		( 6*32+19) /* NodeId MSR */
#define X86_FEATURE_TBM			( 6*32+21) /* Trailing Bit Manipulations */
#define X86_FEATURE_TOPOEXT		( 6*32+22) /* Topology extensions CPUID leafs */
#define X86_FEATURE_PERFCTR_CORE	( 6*32+23) /* Core performance counter extensions */
#define X86_FEATURE_PERFCTR_NB		( 6*32+24) /* NB performance counter extensions */
#define X86_FEATURE_BPEXT		( 6*32+26) /* Data breakpoint extension */
#define X86_FEATURE_PTSC		( 6*32+27) /* Performance time-stamp counter */
#define X86_FEATURE_PERFCTR_LLC		( 6*32+28) /* Last Level Cache performance counter extensions */
#define X86_FEATURE_MWAITX		( 6*32+29) /* MWAIT extension (MONITORX/MWAITX instructions) */

/*
 * Auxiliary flags: Linux defined - For features scattered in various
 * CPUID levels like 0x6, 0xA etc, word 7.
 *
 * Reuse free bits when adding new feature flags!
 */
#define X86_FEATURE_RING3MWAIT		( 7*32+ 0) /* Ring 3 MONITOR/MWAIT instructions */
#define X86_FEATURE_CPUID_FAULT		( 7*32+ 1) /* Intel CPUID faulting */
#define X86_FEATURE_CPB			( 7*32+ 2) /* AMD Core Performance Boost */
#define X86_FEATURE_EPB			( 7*32+ 3) /* IA32_ENERGY_PERF_BIAS support */
#define X86_FEATURE_CAT_L3		( 7*32+ 4) /* Cache Allocation Technology L3 */
#define X86_FEATURE_CAT_L2		( 7*32+ 5) /* Cache Allocation Technology L2 */
#define X86_FEATURE_CDP_L3		( 7*32+ 6) /* Code and Data Prioritization L3 */
#define X86_FEATURE_INVPCID_SINGLE	( 7*32+ 7) /* Effectively INVPCID && CR4.PCIDE=1 */
#define X86_FEATURE_HW_PSTATE		( 7*32+ 8) /* AMD HW-PState */
#define X86_FEATURE_PROC_FEEDBACK	( 7*32+ 9) /* AMD ProcFeedbackInterface */
#define X86_FEATURE_SME			( 7*32+10) /* AMD Secure Memory Encryption */
#define X86_FEATURE_PTI			( 7*32+11) /* Kernel Page Table Isolation enabled */
#define X86_FEATURE_RETPOLINE		( 7*32+12) /* "" Generic Retpoline mitigation for Spectre variant 2 */
#define X86_FEATURE_RETPOLINE_AMD	( 7*32+13) /* "" AMD Retpoline mitigation for Spectre variant 2 */
#define X86_FEATURE_INTEL_PPIN		( 7*32+14) /* Intel Processor Inventory Number */
#define X86_FEATURE_CDP_L2		( 7*32+15) /* Code and Data Prioritization L2 */
#define X86_FEATURE_MSR_SPEC_CTRL	( 7*32+16) /* "" MSR SPEC_CTRL is implemented */
#define X86_FEATURE_SSBD		( 7*32+17) /* Speculative Store Bypass Disable */
#define X86_FEATURE_MBA			( 7*32+18) /* Memory Bandwidth Allocation */
#define X86_FEATURE_RSB_CTXSW		( 7*32+19) /* "" Fill RSB on context switches */
#define X86_FEATURE_SEV			( 7*32+20) /* AMD Secure Encrypted Virtualization */
#define X86_FEATURE_USE_IBPB		( 7*32+21) /* "" Indirect Branch Prediction Barrier enabled */
#define X86_FEATURE_USE_IBRS_FW		( 7*32+22) /* "" Use IBRS during runtime firmware calls */
#define X86_FEATURE_SPEC_STORE_BYPASS_DISABLE	( 7*32+23) /* "" Disable Speculative Store Bypass. */
#define X86_FEATURE_LS_CFG_SSBD		( 7*32+24)  /* "" AMD SSBD implementation via LS_CFG MSR */
#define X86_FEATURE_IBRS		( 7*32+25) /* Indirect Branch Restricted Speculation */
#define X86_FEATURE_IBPB		( 7*32+26) /* Indirect Branch Prediction Barrier */
#define X86_FEATURE_STIBP		( 7*32+27) /* Single Thread Indirect Branch Predictors */
#define X86_FEATURE_ZEN			( 7*32+28) /* "" CPU is AMD family 0x17 (Zen) */
#define X86_FEATURE_L1TF_PTEINV		( 7*32+29) /* "" L1TF workaround PTE inversion */
#define X86_FEATURE_IBRS_ENHANCED	( 7*32+30) /* Enhanced IBRS */

/* Virtualization flags: Linux defined, word 8 */
#define X86_FEATURE_TPR_SHADOW		( 8*32+ 0) /* Intel TPR Shadow */
#define X86_FEATURE_VNMI		( 8*32+ 1) /* Intel Virtual NMI */
#define X86_FEATURE_FLEXPRIORITY	( 8*32+ 2) /* Intel FlexPriority */
#define X86_FEATURE_EPT			( 8*32+ 3) /* Intel Extended Page Table */
#define X86_FEATURE_VPID		( 8*32+ 4) /* Intel Virtual Processor ID */

#define X86_FEATURE_VMMCALL		( 8*32+15) /* Prefer VMMCALL to VMCALL */
#define X86_FEATURE_XENPV		( 8*32+16) /* "" Xen paravirtual guest */
#define X86_FEATURE_EPT_AD		( 8*32+17) /* Intel Extended Page Table access-dirty bit */

/* Intel-defined CPU features, CPUID level 0x00000007:0 (EBX), word 9 */
#define X86_FEATURE_FSGSBASE		( 9*32+ 0) /* RDFSBASE, WRFSBASE, RDGSBASE, WRGSBASE instructions*/
#define X86_FEATURE_TSC_ADJUST		( 9*32+ 1) /* TSC adjustment MSR 0x3B */
#define X86_FEATURE_BMI1		( 9*32+ 3) /* 1st group bit manipulation extensions */
#define X86_FEATURE_HLE			( 9*32+ 4) /* Hardware Lock Elision */
#define X86_FEATURE_AVX2		( 9*32+ 5) /* AVX2 instructions */
#define X86_FEATURE_SMEP		( 9*32+ 7) /* Supervisor Mode Execution Protection */
#define X86_FEATURE_BMI2		( 9*32+ 8) /* 2nd group bit manipulation extensions */
#define X86_FEATURE_ERMS		( 9*32+ 9) /* Enhanced REP MOVSB/STOSB instructions */
#define X86_FEATURE_INVPCID		( 9*32+10) /* Invalidate Processor Context ID */
#define X86_FEATURE_RTM			( 9*32+11) /* Restricted Transactional Memory */
#define X86_FEATURE_CQM			( 9*32+12) /* Cache QoS Monitoring */
#define X86_FEATURE_MPX			( 9*32+14) /* Memory Protection Extension */
#define X86_FEATURE_RDT_A		( 9*32+15) /* Resource Director Technology Allocation */
#define X86_FEATURE_AVX512F		( 9*32+16) /* AVX-512 Foundation */
#define X86_FEATURE_AVX512DQ		( 9*32+17) /* AVX-512 DQ (Double/Quad granular) Instructions */
#define X86_FEATURE_RDSEED		( 9*32+18) /* RDSEED instruction */
#define X86_FEATURE_ADX			( 9*32+19) /* ADCX and ADOX instructions */
#define X86_FEATURE_SMAP		( 9*32+20) /* Supervisor Mode Access Prevention */
#define X86_FEATURE_AVX512IFMA		( 9*32+21) /* AVX-512 Integer Fused Multiply-Add instructions */
#define X86_FEATURE_CLFLUSHOPT		( 9*32+23) /* CLFLUSHOPT instruction */
#define X86_FEATURE_CLWB		( 9*32+24) /* CLWB instruction */
#define X86_FEATURE_INTEL_PT		( 9*32+25) /* Intel Processor Trace */
#define X86_FEATURE_AVX512PF		( 9*32+26) /* AVX-512 Prefetch */
#define X86_FEATURE_AVX512ER		( 9*32+27) /* AVX-512 Exponential and Reciprocal */
#define X86_FEATURE_AVX512CD		( 9*32+28) /* AVX-512 Conflict Detection */
#define X86_FEATURE_SHA_NI		( 9*32+29) /* SHA1/SHA256 Instruction Extensions */
#define X86_FEATURE_AVX512BW		( 9*32+30) /* AVX-512 BW (Byte/Word granular) Instructions */
#define X86_FEATURE_AVX512VL		( 9*32+31) /* AVX-512 VL (128/256 Vector Length) Extensions */

/* Extended state features, CPUID level 0x0000000d:1 (EAX), word 10 */
#define X86_FEATURE_XSAVEOPT		(10*32+ 0) /* XSAVEOPT instruction */
#define X86_FEATURE_XSAVEC		(10*32+ 1) /* XSAVEC instruction */
#define X86_FEATURE_XGETBV1		(10*32+ 2) /* XGETBV with ECX = 1 instruction */
#define X86_FEATURE_XSAVES		(10*32+ 3) /* XSAVES/XRSTORS instructions */

/* Intel-defined CPU QoS Sub-leaf, CPUID level 0x0000000F:0 (EDX), word 11 */
#define X86_FEATURE_CQM_LLC		(11*32+ 1) /* LLC QoS if 1 */

/* Intel-defined CPU QoS Sub-leaf, CPUID level 0x0000000F:1 (EDX), word 12 */
#define X86_FEATURE_CQM_OCCUP_LLC	(12*32+ 0) /* LLC occupancy monitoring */
#define X86_FEATURE_CQM_MBM_TOTAL	(12*32+ 1) /* LLC Total MBM monitoring */
#define X86_FEATURE_CQM_MBM_LOCAL	(12*32+ 2) /* LLC Local MBM monitoring */

/* AMD-defined CPU features, CPUID level 0x80000008 (EBX), word 13 */
#define X86_FEATURE_CLZERO		(13*32+ 0) /* CLZERO instruction */
#define X86_FEATURE_IRPERF		(13*32+ 1) /* Instructions Retired Count */
#define X86_FEATURE_XSAVEERPTR		(13*32+ 2) /* Always save/restore FP error pointers */
#define X86_FEATURE_WBNOINVD		(13*32+ 9) /* WBNOINVD instruction */
#define X86_FEATURE_AMD_IBPB		(13*32+12) /* "" Indirect Branch Prediction Barrier */
#define X86_FEATURE_AMD_IBRS		(13*32+14) /* "" Indirect Branch Restricted Speculation */
#define X86_FEATURE_AMD_STIBP		(13*32+15) /* "" Single Thread Indirect Branch Predictors */
#define X86_FEATURE_AMD_STIBP_ALWAYS_ON	(13*32+17) /* "" Single Thread Indirect Branch Predictors always-on preferred */
#define X86_FEATURE_AMD_SSBD		(13*32+24) /* "" Speculative Store Bypass Disable */
#define X86_FEATURE_VIRT_SSBD		(13*32+25) /* Virtualized Speculative Store Bypass Disable */
#define X86_FEATURE_AMD_SSB_NO		(13*32+26) /* "" Speculative Store Bypass is fixed in hardware. */

/* Thermal and Power Management Leaf, CPUID level 0x00000006 (EAX), word 14 */
#define X86_FEATURE_DTHERM		(14*32+ 0) /* Digital Thermal Sensor */
#define X86_FEATURE_IDA			(14*32+ 1) /* Intel Dynamic Acceleration */
#define X86_FEATURE_ARAT		(14*32+ 2) /* Always Running APIC Timer */
#define X86_FEATURE_PLN			(14*32+ 4) /* Intel Power Limit Notification */
#define X86_FEATURE_PTS			(14*32+ 6) /* Intel Package Thermal Status */
#define X86_FEATURE_HWP			(14*32+ 7) /* Intel Hardware P-states */
#define X86_FEATURE_HWP_NOTIFY		(14*32+ 8) /* HWP Notification */
#define X86_FEATURE_HWP_ACT_WINDOW	(14*32+ 9) /* HWP Activity Window */
#define X86_FEATURE_HWP_EPP		(14*32+10) /* HWP Energy Perf. Preference */
#define X86_FEATURE_HWP_PKG_REQ		(14*32+11) /* HWP Package Level Request */

/* AMD SVM Feature Identification, CPUID level 0x8000000a (EDX), word 15 */
#define X86_FEATURE_NPT			(15*32+ 0) /* Nested Page Table support */
#define X86_FEATURE_LBRV		(15*32+ 1) /* LBR Virtualization support */
#define X86_FEATURE_SVML		(15*32+ 2) /* "svm_lock" SVM locking MSR */
#define X86_FEATURE_NRIPS		(15*32+ 3) /* "nrip_save" SVM next_rip save */
#define X86_FEATURE_TSCRATEMSR		(15*32+ 4) /* "tsc_scale" TSC scaling support */
#define X86_FEATURE_VMCBCLEAN		(15*32+ 5) /* "vmcb_clean" VMCB clean bits support */
#define X86_FEATURE_FLUSHBYASID		(15*32+ 6) /* flush-by-ASID support */
#define X86_FEATURE_DECODEASSISTS	(15*32+ 7) /* Decode Assists support */
#define X86_FEATURE_PAUSEFILTER		(15*32+10) /* filtered pause intercept */
#define X86_FEATURE_PFTHRESHOLD		(15*32+12) /* pause filter threshold */
#define X86_FEATURE_AVIC		(15*32+13) /* Virtual Interrupt Controller */
#define X86_FEATURE_V_VMSAVE_VMLOAD	(15*32+15) /* Virtual VMSAVE VMLOAD */
#define X86_FEATURE_VGIF		(15*32+16) /* Virtual GIF */

/* Intel-defined CPU features, CPUID level 0x00000007:0 (ECX), word 16 */
#define X86_FEATURE_AVX512VBMI		(16*32+ 1) /* AVX512 Vector Bit Manipulation instructions*/
#define X86_FEATURE_UMIP		(16*32+ 2) /* User Mode Instruction Protection */
#define X86_FEATURE_PKU			(16*32+ 3) /* Protection Keys for Userspace */
#define X86_FEATURE_OSPKE		(16*32+ 4) /* OS Protection Keys Enable */
#define X86_FEATURE_AVX512_VBMI2	(16*32+ 6) /* Additional AVX512 Vector Bit Manipulation Instructions */
#define X86_FEATURE_GFNI		(16*32+ 8) /* Galois Field New Instructions */
#define X86_FEATURE_VAES		(16*32+ 9) /* Vector AES */
#define X86_FEATURE_VPCLMULQDQ		(16*32+10) /* Carry-Less Multiplication Double Quadword */
#define X86_FEATURE_AVX512_VNNI		(16*32+11) /* Vector Neural Network Instructions */
#define X86_FEATURE_AVX512_BITALG	(16*32+12) /* Support for VPOPCNT[B,W] and VPSHUF-BITQMB instructions */
#define X86_FEATURE_TME			(16*32+13) /* Intel Total Memory Encryption */
#define X86_FEATURE_AVX512_VPOPCNTDQ	(16*32+14) /* POPCNT for vectors of DW/QW */
#define X86_FEATURE_LA57		(16*32+16) /* 5-level page tables */
#define X86_FEATURE_RDPID		(16*32+22) /* RDPID instruction */
#define X86_FEATURE_CLDEMOTE		(16*32+25) /* CLDEMOTE instruction */
#define X86_FEATURE_MOVDIRI		(16*32+27) /* MOVDIRI instruction */
#define X86_FEATURE_MOVDIR64B		(16*32+28) /* MOVDIR64B instruction */

/* AMD-defined CPU features, CPUID level 0x80000007 (EBX), word 17 */
#define X86_FEATURE_OVERFLOW_RECOV	(17*32+ 0) /* MCA overflow recovery support */
#define X86_FEATURE_SUCCOR		(17*32+ 1) /* Uncorrectable error containment and recovery */
#define X86_FEATURE_SMCA		(17*32+ 3) /* Scalable MCA */

/* Intel-defined CPU features, CPUID level 0x00000007:0 (EDX), word 18 */
#define X86_FEATURE_AVX512_4VNNIW	(18*32+ 2) /* AVX-512 Neural Network Instructions */
#define X86_FEATURE_AVX512_4FMAPS	(18*32+ 3) /* AVX-512 Multiply Accumulation Single precision */
#define X86_FEATURE_TSX_FORCE_ABORT	(18*32+13) /* "" TSX_FORCE_ABORT */
#define X86_FEATURE_PCONFIG		(18*32+18) /* Intel PCONFIG */
#define X86_FEATURE_SPEC_CTRL		(18*32+26) /* "" Speculation Control (IBRS + IBPB) */
#define X86_FEATURE_INTEL_STIBP		(18*32+27) /* "" Single Thread Indirect Branch Predictors */
#define X86_FEATURE_FLUSH_L1D		(18*32+28) /* Flush L1D cache */
#define X86_FEATURE_ARCH_CAPABILITIES	(18*32+29) /* IA32_ARCH_CAPABILITIES MSR (Intel) */
#define X86_FEATURE_SPEC_CTRL_SSBD	(18*32+31) /* "" Speculative Store Bypass Disable */

const char * const x86_cap_flags[NCAPINTS*32] = {
	[X86_FEATURE_FPU]		 = "fpu",
	[X86_FEATURE_VME]		 = "vme",
	[X86_FEATURE_DE]		 = "de",
	[X86_FEATURE_PSE]		 = "pse",
	[X86_FEATURE_TSC]		 = "tsc",
	[X86_FEATURE_MSR]		 = "msr",
	[X86_FEATURE_PAE]		 = "pae",
	[X86_FEATURE_MCE]		 = "mce",
	[X86_FEATURE_CX8]		 = "cx8",
	[X86_FEATURE_APIC]		 = "apic",
	[X86_FEATURE_SEP]		 = "sep",
	[X86_FEATURE_MTRR]		 = "mtrr",
	[X86_FEATURE_PGE]		 = "pge",
	[X86_FEATURE_MCA]		 = "mca",
	[X86_FEATURE_CMOV]		 = "cmov",
	[X86_FEATURE_PAT]		 = "pat",
	[X86_FEATURE_PSE36]		 = "pse36",
	[X86_FEATURE_PN]		 = "pn",
	[X86_FEATURE_CLFLUSH]	 = "clflush",
	[X86_FEATURE_DS]		 = "dts",
	[X86_FEATURE_ACPI]		 = "acpi",
	[X86_FEATURE_MMX]		 = "mmx",
	[X86_FEATURE_FXSR]		 = "fxsr",
	[X86_FEATURE_XMM]		 = "sse",
	[X86_FEATURE_XMM2]		 = "sse2",
	[X86_FEATURE_SELFSNOOP]	 = "ss",
	[X86_FEATURE_HT]		 = "ht",
	[X86_FEATURE_ACC]		 = "tm",
	[X86_FEATURE_IA64]		 = "ia64",
	[X86_FEATURE_PBE]		 = "pbe",
	[X86_FEATURE_SYSCALL]	 = "syscall",
	[X86_FEATURE_MP]		 = "mp",
	[X86_FEATURE_NX]		 = "nx",
	[X86_FEATURE_MMXEXT]		 = "mmxext",
	[X86_FEATURE_FXSR_OPT]		 = "fxsr_opt",
	[X86_FEATURE_GBPAGES]		 = "pdpe1gb",
	[X86_FEATURE_RDTSCP]		 = "rdtscp",
	[X86_FEATURE_LM]		 = "lm",
	[X86_FEATURE_3DNOWEXT]		 = "3dnowext",
	[X86_FEATURE_3DNOW]		 = "3dnow",
	[X86_FEATURE_RECOVERY]		 = "recovery",
	[X86_FEATURE_LONGRUN]		 = "longrun",
	[X86_FEATURE_LRTI]		 = "lrti",
	[X86_FEATURE_CXMMX]		 = "cxmmx",
	[X86_FEATURE_K6_MTRR]		 = "k6_mtrr",
	[X86_FEATURE_CYRIX_ARR]		 = "cyrix_arr",
	[X86_FEATURE_CENTAUR_MCR]	 = "centaur_mcr",
	[X86_FEATURE_CONSTANT_TSC]	 = "constant_tsc",
	[X86_FEATURE_UP]		 = "up",
	[X86_FEATURE_ART]		 = "art",
	[X86_FEATURE_ARCH_PERFMON]	 = "arch_perfmon",
	[X86_FEATURE_PEBS]		 = "pebs",
	[X86_FEATURE_BTS]		 = "bts",
	[X86_FEATURE_REP_GOOD]		 = "rep_good",
	[X86_FEATURE_ACC_POWER]		 = "acc_power",
	[X86_FEATURE_NOPL]		 = "nopl",
	[X86_FEATURE_XTOPOLOGY]		 = "xtopology",
	[X86_FEATURE_TSC_RELIABLE]	 = "tsc_reliable",
	[X86_FEATURE_NONSTOP_TSC]	 = "nonstop_tsc",
	[X86_FEATURE_CPUID]		 = "cpuid",
	[X86_FEATURE_EXTD_APICID]	 = "extd_apicid",
	[X86_FEATURE_AMD_DCM]		 = "amd_dcm",
	[X86_FEATURE_APERFMPERF]	 = "aperfmperf",
	[X86_FEATURE_NONSTOP_TSC_S3]	 = "nonstop_tsc_s3",
	[X86_FEATURE_TSC_KNOWN_FREQ]	 = "tsc_known_freq",
	[X86_FEATURE_XMM3]		 = "pni",
	[X86_FEATURE_PCLMULQDQ]		 = "pclmulqdq",
	[X86_FEATURE_DTES64]		 = "dtes64",
	[X86_FEATURE_MWAIT]		 = "monitor",
	[X86_FEATURE_DSCPL]		 = "ds_cpl",
	[X86_FEATURE_VMX]		 = "vmx",
	[X86_FEATURE_SMX]		 = "smx",
	[X86_FEATURE_EST]		 = "est",
	[X86_FEATURE_TM2]		 = "tm2",
	[X86_FEATURE_SSSE3]		 = "ssse3",
	[X86_FEATURE_CID]		 = "cid",
	[X86_FEATURE_SDBG]		 = "sdbg",
	[X86_FEATURE_FMA]		 = "fma",
	[X86_FEATURE_CX16]		 = "cx16",
	[X86_FEATURE_XTPR]		 = "xtpr",
	[X86_FEATURE_PDCM]		 = "pdcm",
	[X86_FEATURE_PCID]		 = "pcid",
	[X86_FEATURE_DCA]		 = "dca",
	[X86_FEATURE_XMM4_1]		 = "sse4_1",
	[X86_FEATURE_XMM4_2]		 = "sse4_2",
	[X86_FEATURE_X2APIC]		 = "x2apic",
	[X86_FEATURE_MOVBE]		 = "movbe",
	[X86_FEATURE_POPCNT]		 = "popcnt",
	[X86_FEATURE_TSC_DEADLINE_TIMER] = "tsc_deadline_timer",
	[X86_FEATURE_AES]		 = "aes",
	[X86_FEATURE_XSAVE]		 = "xsave",
	[X86_FEATURE_AVX]		 = "avx",
	[X86_FEATURE_F16C]		 = "f16c",
	[X86_FEATURE_RDRAND]		 = "rdrand",
	[X86_FEATURE_HYPERVISOR]	 = "hypervisor",
	[X86_FEATURE_XSTORE]		 = "rng",
	[X86_FEATURE_XSTORE_EN]		 = "rng_en",
	[X86_FEATURE_XCRYPT]		 = "ace",
	[X86_FEATURE_XCRYPT_EN]		 = "ace_en",
	[X86_FEATURE_ACE2]		 = "ace2",
	[X86_FEATURE_ACE2_EN]		 = "ace2_en",
	[X86_FEATURE_PHE]		 = "phe",
	[X86_FEATURE_PHE_EN]		 = "phe_en",
	[X86_FEATURE_PMM]		 = "pmm",
	[X86_FEATURE_PMM_EN]		 = "pmm_en",
	[X86_FEATURE_LAHF_LM]		 = "lahf_lm",
	[X86_FEATURE_CMP_LEGACY]	 = "cmp_legacy",
	[X86_FEATURE_SVM]		 = "svm",
	[X86_FEATURE_EXTAPIC]		 = "extapic",
	[X86_FEATURE_CR8_LEGACY]	 = "cr8_legacy",
	[X86_FEATURE_ABM]		 = "abm",
	[X86_FEATURE_SSE4A]		 = "sse4a",
	[X86_FEATURE_MISALIGNSSE]	 = "misalignsse",
	[X86_FEATURE_3DNOWPREFETCH]	 = "3dnowprefetch",
	[X86_FEATURE_OSVW]		 = "osvw",
	[X86_FEATURE_IBS]		 = "ibs",
	[X86_FEATURE_XOP]		 = "xop",
	[X86_FEATURE_SKINIT]		 = "skinit",
	[X86_FEATURE_WDT]		 = "wdt",
	[X86_FEATURE_LWP]		 = "lwp",
	[X86_FEATURE_FMA4]		 = "fma4",
	[X86_FEATURE_TCE]		 = "tce",
	[X86_FEATURE_NODEID_MSR]	 = "nodeid_msr",
	[X86_FEATURE_TBM]		 = "tbm",
	[X86_FEATURE_TOPOEXT]		 = "topoext",
	[X86_FEATURE_PERFCTR_CORE]	 = "perfctr_core",
	[X86_FEATURE_PERFCTR_NB]	 = "perfctr_nb",
	[X86_FEATURE_BPEXT]		 = "bpext",
	[X86_FEATURE_PTSC]		 = "ptsc",
	[X86_FEATURE_PERFCTR_LLC]	 = "perfctr_llc",
	[X86_FEATURE_MWAITX]		 = "mwaitx",
	[X86_FEATURE_RING3MWAIT]	 = "ring3mwait",
	[X86_FEATURE_CPUID_FAULT]	 = "cpuid_fault",
	[X86_FEATURE_CPB]		 = "cpb",
	[X86_FEATURE_EPB]		 = "epb",
	[X86_FEATURE_CAT_L3]		 = "cat_l3",
	[X86_FEATURE_CAT_L2]		 = "cat_l2",
	[X86_FEATURE_CDP_L3]		 = "cdp_l3",
	[X86_FEATURE_INVPCID_SINGLE]	 = "invpcid_single",
	[X86_FEATURE_HW_PSTATE]		 = "hw_pstate",
	[X86_FEATURE_PROC_FEEDBACK]	 = "proc_feedback",
	[X86_FEATURE_SME]		 = "sme",
	[X86_FEATURE_PTI]		 = "pti",
	[X86_FEATURE_INTEL_PPIN]	 = "intel_ppin",
	[X86_FEATURE_CDP_L2]		 = "cdp_l2",
	[X86_FEATURE_SSBD]		 = "ssbd",
	[X86_FEATURE_MBA]		 = "mba",
	[X86_FEATURE_SEV]		 = "sev",
	[X86_FEATURE_IBRS]		 = "ibrs",
	[X86_FEATURE_IBPB]		 = "ibpb",
	[X86_FEATURE_STIBP]		 = "stibp",
	[X86_FEATURE_IBRS_ENHANCED]	 = "ibrs_enhanced",
	[X86_FEATURE_TPR_SHADOW]	 = "tpr_shadow",
	[X86_FEATURE_VNMI]		 = "vnmi",
	[X86_FEATURE_FLEXPRIORITY]	 = "flexpriority",
	[X86_FEATURE_EPT]		 = "ept",
	[X86_FEATURE_VPID]		 = "vpid",
	[X86_FEATURE_VMMCALL]		 = "vmmcall",
	[X86_FEATURE_EPT_AD]		 = "ept_ad",
	[X86_FEATURE_FSGSBASE]		 = "fsgsbase",
	[X86_FEATURE_TSC_ADJUST]	 = "tsc_adjust",
	[X86_FEATURE_BMI1]		 = "bmi1",
	[X86_FEATURE_HLE]		 = "hle",
	[X86_FEATURE_AVX2]		 = "avx2",
	[X86_FEATURE_SMEP]		 = "smep",
	[X86_FEATURE_BMI2]		 = "bmi2",
	[X86_FEATURE_ERMS]		 = "erms",
	[X86_FEATURE_INVPCID]		 = "invpcid",
	[X86_FEATURE_RTM]		 = "rtm",
	[X86_FEATURE_CQM]		 = "cqm",
	[X86_FEATURE_MPX]		 = "mpx",
	[X86_FEATURE_RDT_A]		 = "rdt_a",
	[X86_FEATURE_AVX512F]		 = "avx512f",
	[X86_FEATURE_AVX512DQ]		 = "avx512dq",
	[X86_FEATURE_RDSEED]		 = "rdseed",
	[X86_FEATURE_ADX]		 = "adx",
	[X86_FEATURE_SMAP]		 = "smap",
	[X86_FEATURE_AVX512IFMA]	 = "avx512ifma",
	[X86_FEATURE_CLFLUSHOPT]	 = "clflushopt",
	[X86_FEATURE_CLWB]		 = "clwb",
	[X86_FEATURE_INTEL_PT]		 = "intel_pt",
	[X86_FEATURE_AVX512PF]		 = "avx512pf",
	[X86_FEATURE_AVX512ER]		 = "avx512er",
	[X86_FEATURE_AVX512CD]		 = "avx512cd",
	[X86_FEATURE_SHA_NI]		 = "sha_ni",
	[X86_FEATURE_AVX512BW]		 = "avx512bw",
	[X86_FEATURE_AVX512VL]		 = "avx512vl",
	[X86_FEATURE_XSAVEOPT]		 = "xsaveopt",
	[X86_FEATURE_XSAVEC]		 = "xsavec",
	[X86_FEATURE_XGETBV1]		 = "xgetbv1",
	[X86_FEATURE_XSAVES]		 = "xsaves",
	[X86_FEATURE_CQM_LLC]		 = "cqm_llc",
	[X86_FEATURE_CQM_OCCUP_LLC]	 = "cqm_occup_llc",
	[X86_FEATURE_CQM_MBM_TOTAL]	 = "cqm_mbm_total",
	[X86_FEATURE_CQM_MBM_LOCAL]	 = "cqm_mbm_local",
	[X86_FEATURE_CLZERO]		 = "clzero",
	[X86_FEATURE_IRPERF]		 = "irperf",
	[X86_FEATURE_XSAVEERPTR]	 = "xsaveerptr",
	[X86_FEATURE_WBNOINVD]		 = "wbnoinvd",
	[X86_FEATURE_VIRT_SSBD]		 = "virt_ssbd",
	[X86_FEATURE_DTHERM]		 = "dtherm",
	[X86_FEATURE_IDA]		 = "ida",
	[X86_FEATURE_ARAT]		 = "arat",
	[X86_FEATURE_PLN]		 = "pln",
	[X86_FEATURE_PTS]		 = "pts",
	[X86_FEATURE_HWP]		 = "hwp",
	[X86_FEATURE_HWP_NOTIFY]	 = "hwp_notify",
	[X86_FEATURE_HWP_ACT_WINDOW]	 = "hwp_act_window",
	[X86_FEATURE_HWP_EPP]		 = "hwp_epp",
	[X86_FEATURE_HWP_PKG_REQ]	 = "hwp_pkg_req",
	[X86_FEATURE_NPT]		 = "npt",
	[X86_FEATURE_LBRV]		 = "lbrv",
	[X86_FEATURE_SVML]		 = "svm_lock",
	[X86_FEATURE_NRIPS]		 = "nrip_save",
	[X86_FEATURE_TSCRATEMSR]	 = "tsc_scale",
	[X86_FEATURE_VMCBCLEAN]		 = "vmcb_clean",
	[X86_FEATURE_FLUSHBYASID]	 = "flushbyasid",
	[X86_FEATURE_DECODEASSISTS]	 = "decodeassists",
	[X86_FEATURE_PAUSEFILTER]	 = "pausefilter",
	[X86_FEATURE_PFTHRESHOLD]	 = "pfthreshold",
	[X86_FEATURE_AVIC]		 = "avic",
	[X86_FEATURE_V_VMSAVE_VMLOAD]	 = "v_vmsave_vmload",
	[X86_FEATURE_VGIF]		 = "vgif",
	[X86_FEATURE_AVX512VBMI]	 = "avx512vbmi",
	[X86_FEATURE_UMIP]		 = "umip",
	[X86_FEATURE_PKU]		 = "pku",
	[X86_FEATURE_OSPKE]		 = "ospke",
	[X86_FEATURE_AVX512_VBMI2]	 = "avx512_vbmi2",
	[X86_FEATURE_GFNI]		 = "gfni",
	[X86_FEATURE_VAES]		 = "vaes",
	[X86_FEATURE_VPCLMULQDQ]	 = "vpclmulqdq",
	[X86_FEATURE_AVX512_VNNI]	 = "avx512_vnni",
	[X86_FEATURE_AVX512_BITALG]	 = "avx512_bitalg",
	[X86_FEATURE_TME]		 = "tme",
	[X86_FEATURE_AVX512_VPOPCNTDQ]	 = "avx512_vpopcntdq",
	[X86_FEATURE_LA57]		 = "la57",
	[X86_FEATURE_RDPID]		 = "rdpid",
	[X86_FEATURE_CLDEMOTE]		 = "cldemote",
	[X86_FEATURE_MOVDIRI]		 = "movdiri",
	[X86_FEATURE_MOVDIR64B]		 = "movdir64b",
	[X86_FEATURE_OVERFLOW_RECOV]	 = "overflow_recov",
	[X86_FEATURE_SUCCOR]		 = "succor",
	[X86_FEATURE_SMCA]		 = "smca",
	[X86_FEATURE_AVX512_4VNNIW]	 = "avx512_4vnniw",
	[X86_FEATURE_AVX512_4FMAPS]	 = "avx512_4fmaps",
	[X86_FEATURE_PCONFIG]		 = "pconfig",
	[X86_FEATURE_FLUSH_L1D]		 = "flush_l1d",
	[X86_FEATURE_ARCH_CAPABILITIES]	 = "arch_capabilities",
};

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

static int cpuid1(uint32_t id, uint32_t *regs)
{
#ifdef WIN32
	// How to specify ecx?
	memset(regs, 0, sizeof(regs));
#else
	asm volatile
		("cpuid"
		 : "=a" (regs[0]), "=b" (regs[1]), "=c" (regs[2]), "=d" (regs[3])
		 : "a" (id), "c" (1));
#endif
	return 0;
}

static uint32_t max_reg; // value from 0x80000000

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

	if (max_reg < 0x80000004)
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

static void add_flags(void)
{
	uint32_t caps[NCAPINTS];
	uint32_t regs[4];
	int i, bit;

	memset(caps, 0, sizeof(caps));

	// caps[2] transmeta
	// caps[3] Linux
	// caps[5] via
	// caps[7] aux (lookup?)
	// caps[8] Linux

	cpuid(1, regs);
	caps[0] = regs[3];
	caps[4] = regs[2];

	cpuid(6, regs);
	caps[14] = regs[0];

	cpuid(7, regs);
	caps[9] = regs[1];
	caps[16] = regs[2];
	caps[18] = regs[3];

	cpuid1(0xd, regs);
	caps[10] = regs[0];

	cpuid(0xf, regs);
	caps[11] = regs[3];

	cpuid1(0xf, regs);
	caps[12] = regs[3];

	if (max_reg >= 0x80000001) {
		cpuid(0x80000001, regs);
		caps[1] = regs[3];
		caps[6] = regs[2];
	}

	if(max_reg >= 0x80000008) {
		cpuid(0x80000008, regs);
		caps[13] = regs[2];
	}

	if (max_reg >= 0x8000000a) {
		cpuid(0x8000000a, regs);
		caps[15] = regs[3];
	}

	if (max_reg >= 0x80000007) {
		cpuid(0x80000007, regs);
		caps[17] = regs[1];
	}

	printf("Flags      :");

	for (i = 0; i < NCAPINTS; ++i)
		for (bit = 0; bit < 32; ++bit)
			if (caps[i] & (1 << bit))
				if (x86_cap_flags[bit + (i * 32)])
					printf(" %s", x86_cap_flags[bit + (i * 32)]);

	putchar('\n');
}

int main(int argc, char *argv[])
{
	char model[49];
	char vendor[13];
	uint32_t regs[4];

	if (argc > 1 && strcmp(argv[1], "-g") == 0) {
		cpuid(1, regs);
		return !(regs[2] & 0x80000000);
	}

	cpuid(0x80000000, regs);
	max_reg = regs[0];

	cpuid(1, regs);

	printf("Model Name : %s\n", cpu_model(model));
	printf("Vendor     : %s\n", cpu_vendor(vendor));
	printf("Family     : %u\n",
		   ((regs[0] >> 8) & 0xf) | ((regs[0] >> (20 - 4)) & 0xf0));
	printf("Model      : %u\n",
		   ((regs[0] >> 4) & 0xf) | ((regs[0] >> (16 - 4)) & 0xf0));
	printf("Stepping   : %u\n", regs[0] & 0xf);

	add_flags();

	return 0;
}
#elif defined(__QNX__) && (defined(__aarch64__) || defined(__arm__))
int main(int argc, char *argv[])
{
	FILE *pfp = popen("pidin info", "r");
	if (!pfp) {
		perror("pidin");
		exit(1);
	}

	char line[128];
	while (fgets(line, sizeof(line), pfp)) {
		if (strncmp(line, "Processor1:", 11) == 0) {
			char *model;
			strtol(line + 11, &model, 16);
			while (*model == ' ') ++model;
			printf("Model Name : %s", model);
		} else if (strncmp(line, "CPU:", 4) == 0) {
			char *p = line + 4;
			while (*p == ' ') ++p;
			fputs("Vendor     : ", stdout);
			while (*p != ' ') {
				putchar(*p);
				++p;
			}
			putchar('\n');
		}
	}

	pclose(pfp);

	puts("Family     : 0");
	puts("Model      : 0");
	puts("Stepping   : 0");
	puts("Flags      : ");

	return 0;
}

#else
int main(int argc, char *argv[])
{
	fprintf(stderr, "Not supported.");
	exit(1);
}
#endif

/*
 * compile-command: "gcc -O2 -Wall cpuid.c -o cpuid"
 */

/*
 * Local Variables:
 * compile-command: "ntoaarch64-gcc -O2 -Wall cpuid.c -o cpuid"
 * End:
 */
