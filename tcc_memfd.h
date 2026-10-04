/* Linux executable storage for libtcc.  Keep the allocator separate so its
   failure paths can be exercised without compiling or executing generated code. */
#ifndef TCC_MEMFD_H
#define TCC_MEMFD_H

#include <errno.h>
#include <limits.h>
#include <stddef.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <unistd.h>

/* These flags are Linux ABI constants, including on older build sysroots. */
#ifndef MFD_CLOEXEC
#define MFD_CLOEXEC 0x0001U
#endif
#ifndef MFD_EXEC
#define MFD_EXEC 0x0010U
#endif

#ifndef TCC_MEMFD_CREATE
static int tcc_memfd_create(unsigned flags)
{
#ifdef SYS_memfd_create
    /* Avoid requiring the glibc 2.27 memfd_create symbol. */
    return syscall(SYS_memfd_create, "tcc-jit", flags);
#else
    errno = ENOSYS;
    return -1;
#endif
}
#define TCC_MEMFD_CREATE tcc_memfd_create
#endif
#ifndef TCC_MEMFD_TRUNCATE
#define TCC_MEMFD_TRUNCATE ftruncate
#endif
#ifndef TCC_MEMFD_MAP
#define TCC_MEMFD_MAP mmap
#endif

/* On failure, preserve errno and describe the failed operation.  Never use a
   filesystem fallback: callers can run their interpreter instead. */
static void *tcc_memfd_alloc(int size, long page_size, const char **operation)
{
    int fd = -1, saved_errno;
    void *base = MAP_FAILED;
    size_t total;

    *operation = "size validation";
    if (size <= 0 || size > INT_MAX / 2 || page_size <= 0 ||
        (page_size & (page_size - 1)) || size % page_size) {
        errno = EINVAL;
        return MAP_FAILED;
    }
    total = (size_t)size * 2;
    *operation = "memfd_create";
    fd = TCC_MEMFD_CREATE(MFD_CLOEXEC | MFD_EXEC);
    /* Linux before 6.3 does not understand MFD_EXEC.  Do not retry a genuine
       policy denial, nor try to weaken the policy on a newer kernel. */
    if (fd < 0 && errno == EINVAL)
        fd = TCC_MEMFD_CREATE(MFD_CLOEXEC);
    if (fd < 0)
        goto fail;
    *operation = "ftruncate";
    if (TCC_MEMFD_TRUNCATE(fd, size) < 0)
        goto fail;
    *operation = "address reservation";
    base = TCC_MEMFD_MAP(NULL, total, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (base == MAP_FAILED)
        goto fail;
    /* MAP_FIXED only replaces parts of our own successful reservation. */
    *operation = "executable mapping";
    if (TCC_MEMFD_MAP(base, size, PROT_READ | PROT_EXEC,
                     MAP_SHARED | MAP_FIXED, fd, 0) == MAP_FAILED)
        goto fail;
    *operation = "writable mapping";
    if (TCC_MEMFD_MAP((char *)base + size, size, PROT_READ | PROT_WRITE,
                     MAP_SHARED | MAP_FIXED, fd, 0) == MAP_FAILED)
        goto fail;
    close(fd);
    return base;

fail:
    saved_errno = errno;
    if (base != MAP_FAILED)
        munmap(base, total);
    if (fd >= 0)
        close(fd);
    errno = saved_errno;
    return MAP_FAILED;
}

#endif
