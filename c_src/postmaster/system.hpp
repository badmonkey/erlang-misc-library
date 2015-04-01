#ifndef POSTMASTER_SYSTEM_
#define POSTMASTER_SYSTEM_

#include "eixx/eterm.hpp"
#include <cerrno>


namespace post
{


inline const char* errno_to_symbol(int err)
{
    switch(errno)
    {
    case EACCES:        return "access_denied";
    case EBADF:         return "bad_fd";
    case EFAULT:        return "addr_fault";
    case EINVAL:        return "invalid_arg";
    case ENAMETOOLONG:  return "name_too_long";
    case ENOENT:        return "does_not_exist";
    case ENOMEM:        return "out_of_memory";
    case ENOSPC:        return "no_space_left";
    }
    return "unknown";
} // errno_to_symbol()


inline eixx::eterm make_error(int err)
{
    return eixx::tuple::make( eixx::am_error, eixx::atom(errno_to_symbol(err)) );
} // make_error()


} // namespace post


#endif // POSTMASTER_SYSTEM_s