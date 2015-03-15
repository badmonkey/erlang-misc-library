#ifndef FSWATCHER_INOTIFY_
#define FSWATCHER_INOTIFY_

#include <functional>
#include <vector>
#include <iostream>
#include <boost/array.hpp>
#include <cerrno>
#include <sys/inotify.h>
#include <map>
#include "postmaster/system.hpp"


namespace asio = boost::asio;
namespace posix = boost::asio::posix;


class inotify
{
public:
    typedef std::function<void(const inotify_event&)>  event_handler_type;
    typedef std::map<std::string, int>  map_type;
    
    enum event
    {
          access = IN_ACCESS
        , attrib = IN_ATTRIB
        , close_write = IN_CLOSE_WRITE
        , close_nowrite = IN_CLOSE_NOWRITE
        , create = IN_CREATE
        , deleted = IN_DELETE
        , delete_self = IN_DELETE_SELF
        , modify = IN_MODIFY
        , move_self = IN_MOVE_SELF
        , moved_from = IN_MOVED_FROM
        , moved_to = IN_MOVED_TO
        , open = IN_OPEN
        , all = IN_ALL_EVENTS
    };
    
    enum add_flags
    {
          dont_follow = IN_DONT_FOLLOW
        , ignore_unlinked = IN_EXCL_UNLINK
        , mask_add = IN_MASK_ADD
        , dir_only = IN_ONLYDIR
    };
    
    enum event_flags
    {
          ignored = IN_IGNORED
        , is_dir = IN_ISDIR
        , overflow = IN_Q_OVERFLOW
    };
    
    
    
    inotify(asio::io_service&  io_service, event_handler_type  handler)
    : fd_( init_fd() )
    , master_(io_service, fd_)
    , handler_(handler)
    {
    } // inotify()
    
    
    int add_target(const std::string& path, int mask)
    {
        int wd = ::inotify_add_watch(fd_, path.c_str(), mask);
        if ( wd == -1 )
            throw post::make_error(errno);
        return wd;
    } // add_target()
    
    
    void remove_target(int watch)
    {
        int e = ::inotify_rm_watch(fd_, watch);
        if ( e == -1 )
            throw post::make_error(errno);
    } // remove_target()
    

    
protected:

    static int init_fd()
    {
        int fd = ::inotify_init1( IN_NONBLOCK );
        if ( fd == -1 )
            throw post::make_error(errno);
        return fd;
    }  // init_fd()
    
    
    void async_monitor()
    {
        master_.async_read_some( boost::asio::buffer(buffer_)
                               , boost::bind( &inotify::handle_monitor
                                            , this
                                            , asio::placeholders::error
                                            , asio::placeholders::bytes_transferred) );

    } // async_monitor()
    
    
    void handle_monitor( boost::system::error_code ec
                       , std::size_t  bytes_transferred)
    {
        if( !ec )
        {
            buffer_str_.append(buffer_.data(), buffer_.data() + bytes_transferred);

            while( buffer_str_.size() >= sizeof(inotify_event) )
            {
                const inotify_event& iev = *( reinterpret_cast<const inotify_event *>( buffer_str_.data() ) );
                
                master_.get_io_service().post(
                            boost::bind( &inotify::process_notify
                                       , this
                                       , iev) );

                buffer_str_.erase( 0, sizeof(inotify_event) + iev.len );
            }
            
            async_monitor();
        }
        else
        {
            //badness
        }
             
        async_monitor();
    } // handle_monitor()
                
    
    void process_notify(const inotify_event& evt)
    {
        try
        {
            handler_(evt);
        }
        catch(...)
        {
            // reply with error or crash?
        }
    } // process_notify()
    
    
    
private:
    constexpr static size_t BUFFER_SIZE = 4096;
    
    static_assert( BUFFER_SIZE >= ( sizeof(struct inotify_event) + NAME_MAX + 1 ), "Buffer too small to hold an inotify_event record" );
    
    int                             fd_;
    posix::stream_descriptor        master_;
    boost::array<char, BUFFER_SIZE> buffer_;
    std::string                     buffer_str_;
    event_handler_type              handler_;
    map_type                        map_;
    
    
}; // class inotify


#endif // FSWATCHER_INOTIFY_