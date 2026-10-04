#define _GNU_SOURCE
#undef NDEBUG
#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <unistd.h>
#ifdef __APPLE__
#include <mach/mach.h>
#include <mach/mach_vm.h>
#endif

static int fail_stage, map_calls, create_calls, old_kernel;
static int test_create(unsigned flags)
{
    ++create_calls;
    if (fail_stage == 1) {
        errno = EPERM;
        return -1;
    }
    if (fail_stage == 6) {
        errno = ENOSYS;
        return -1;
    }
    if (old_kernel && (flags & 0x0010U)) {
        errno = EINVAL;
        return -1;
    }
#ifdef __linux__
    return syscall(SYS_memfd_create, "tcc-test", flags);
#else
    /* Exercise the same alias/cleanup code on macOS. This test-only backing
       file is not a production fallback, nor a test of the Linux syscall. */
    char path[1024];
    const char *root = getenv("TMPDIR");
    int fd;
    assert(root);
    snprintf(path, sizeof(path), "%s/tcc-memfd-test-XXXXXX", root);
    fd = mkstemp(path);
    if (fd >= 0)
        unlink(path);
    return fd;
#endif
}

static int test_truncate(int fd, off_t size)
{
    if (fail_stage == 2) {
        errno = ENOSPC;
        return -1;
    }
    return ftruncate(fd, size);
}

static void *test_map(void *addr, size_t len, int prot, int flags, int fd, off_t off)
{
    assert((prot & (PROT_WRITE | PROT_EXEC)) != (PROT_WRITE | PROT_EXEC));
    ++map_calls;
    if (fail_stage == map_calls + 2) {
        errno = EACCES;
        return MAP_FAILED;
    }
#ifdef __APPLE__
    /* macOS denies shared executable file mappings. This harness only checks
       alias contents and rollback; native generated-code execution is covered
       separately by runtime_storage. Linux tests the actual RX protections. */
    prot &= ~PROT_EXEC;
#endif
    return mmap(addr, len, prot, flags, fd, off);
}

#define TCC_MEMFD_CREATE test_create
#define TCC_MEMFD_TRUNCATE test_truncate
#define TCC_MEMFD_MAP test_map
#include "tcc_memfd.h"

static int count_fds(void)
{
#ifdef __linux__
    DIR *dir = opendir("/proc/self/fd");
    struct dirent *entry;
    int count = 0;
    assert(dir);
    while ((entry = readdir(dir)))
        if (entry->d_name[0] != '.')
            ++count;
    closedir(dir);
    return count;
#else
    int fd, count = 0;
    for (fd = 0; fd < 1024; ++fd)
        if (fcntl(fd, F_GETFD) >= 0)
            ++count;
    return count;
#endif
}

static int count_maps(void)
{
#ifdef __linux__
    FILE *file = fopen("/proc/self/maps", "r");
    char line[1024];
    int count = 0;
    assert(file);
    while (fgets(line, sizeof(line), file))
        ++count;
    fclose(file);
    return count;
#else
    mach_vm_address_t address = 0;
    mach_vm_size_t size;
    vm_region_basic_info_data_64_t info;
    mach_msg_type_number_t length;
    mach_port_t object;
    int count = 0;
    for (;;) {
        length = VM_REGION_BASIC_INFO_COUNT_64;
        if (mach_vm_region(mach_task_self(), &address, &size, VM_REGION_BASIC_INFO_64,
                           (vm_region_info_t)&info, &length, &object) != KERN_SUCCESS)
            break;
        if (object != MACH_PORT_NULL)
            mach_port_deallocate(mach_task_self(), object);
        ++count;
        address += size;
    }
    return count;
#endif
}

int main(void)
{
    const char *operation;
    long page = sysconf(_SC_PAGESIZE);
    int stage, iteration, fds, maps;
    void *ptr;
    /* Warm up libc's stdio allocations before measuring cleanup. */
    count_fds();
    count_maps();
    fds = count_fds();
    maps = count_maps();
    for (stage = 0; stage <= 5; ++stage) {
        for (iteration = 0; iteration < 20; ++iteration) {
            fail_stage = stage;
            map_calls = create_calls = 0;
            ptr = tcc_memfd_alloc(page, page, &operation);
            if (!stage) {
                if (ptr == MAP_FAILED)
                    fprintf(stderr, "%s: %s\n", operation, strerror(errno));
                assert(ptr != MAP_FAILED);
                strcpy((char *)ptr + page, "shared aliases");
                assert(strcmp(ptr, "shared aliases") == 0);
                assert(munmap(ptr, page * 2) == 0);
            } else {
                assert(ptr == MAP_FAILED);
                assert(errno == (stage == 1 ? EPERM : stage == 2 ? ENOSPC : EACCES));
                if (stage == 1)
                    assert(create_calls == 1); /* Do not retry a policy denial. */
            }
            assert(count_fds() == fds);
            assert(count_maps() == maps);
        }
    }
    fail_stage = 0;
    old_kernel = 1;
    create_calls = map_calls = 0;
    ptr = tcc_memfd_alloc(page, page, &operation);
    assert(ptr != MAP_FAILED && create_calls == 2);
    munmap(ptr, page * 2);
    old_kernel = 0;
    fail_stage = 6;
    create_calls = 0;
    assert(tcc_memfd_alloc(page, page, &operation) == MAP_FAILED);
    assert(errno == ENOSYS && create_calls == 1);
    assert(tcc_memfd_alloc(0, page, &operation) == MAP_FAILED);
    assert(tcc_memfd_alloc(INT_MAX, page, &operation) == MAP_FAILED);
    assert(tcc_memfd_alloc(page, 3, &operation) == MAP_FAILED);
    assert(count_fds() == fds && count_maps() == maps);
    return 0;
}
