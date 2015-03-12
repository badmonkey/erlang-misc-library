#ifndef POSTMASTER_POSTMASTER_
#define POSTMASTER_POSTMASTER_

#include "eixx/eterm.hpp"
#include <functional>
#include <vector>
#include <iostream>

namespace post
{

namespace asio = boost::asio;
namespace posix = boost::asio::posix;

using boost::bind;



class master
{
public:
    typedef std::function<eixx::eterm (const eixx::eterm&)>  dispatch_type;
    typedef unsigned char  byte;
    
    
    master(asio::io_service&  io_service, dispatch_type  disp)
    : input_(io_service, ::dup(STDIN_FILENO))
    , output_(io_service, ::dup(STDOUT_FILENO))
    , dispatch_(disp)
    {
        start_read_header();
    } // master()
    
    
    void start_read_header()
    {
        size_buf_[0] = size_buf_[1] = 0;
        
        asio::async_read( input_
                        , asio::buffer(size_buf_)
                        , bind( &master::handle_read_header
                              , this
                              , boost::asio::placeholders::error
                              , boost::asio::placeholders::bytes_transferred) );
    } // start_read_header()
    
    
    void handle_read_header( const boost::system::error_code& error
                           , std::size_t bytes_transferred)
    {
        if ( error )
        {
            close();
            return;
        }
        
        int len = (size_buf_[0] << 8) | size_buf_[1];
        
        buffer_.resize(len);
        
        asio::async_read( input_
                        , asio::buffer(buffer_)
                        , bind( &master::handle_read_body
                              , this
                              , boost::asio::placeholders::error
                              , boost::asio::placeholders::bytes_transferred) );

    } // handle_read_header()
    
    
    void handle_read_body( const boost::system::error_code& error
                         , std::size_t bytes_transferred)
    {
        if ( !error )
        {
            try
            {
                eixx::eterm  msg(buffer_.data(), buffer_.size());
                
                input_.get_io_service().post(
                            bind( &master::process_msg
                                , this
                                , msg) );
                                
                return;
            }
            catch(...)
            {
            }
        }
        
            // we only reach here if something bad happened
        close();
    } // handle_read_body()
    
    
    void process_msg(const eixx::eterm& msg)
    {
        try
        {
            eixx::eterm reply = dispatch_(msg);
        
            // async_write
        }
        catch(...)
        {
            // reply with error or crash?
        }
        
        start_read_header();
    } // process_msg()
    
    
    void close()
    {
        input_.close();
        output_.close();
    } // close()

    
private:
    posix::stream_descriptor    input_;
    posix::stream_descriptor    output_;
    dispatch_type               dispatch_;
    byte                        size_buf_[2];
    std::vector<char>           buffer_;
    
}; // class master


} // namespace post

#endif // POSTMASTER_POSTMASTER_
