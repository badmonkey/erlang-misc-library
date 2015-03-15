#ifndef POSTMASTER_POSTMASTER_
#define POSTMASTER_POSTMASTER_

#include "eixx/eterm.hpp"
#include <boost/asio.hpp>
#include <functional>
#include <vector>
#include <iostream>
#include "system.hpp"


namespace post
{

namespace asio = boost::asio;
namespace posix = boost::asio::posix;

using boost::bind;


constexpr size_t HEADER_SIZE = 2;


class master
{
public:
    typedef std::function<void (post::master&, const eixx::eterm&)>  dispatch_type;
    typedef unsigned char  byte;
    
    
    master(asio::io_service&  io_service, dispatch_type  disp)
    : input_(io_service, ::dup(STDIN_FILENO))
    , output_(io_service, ::dup(STDOUT_FILENO))
    , dispatch_(disp)
    {
        start_read_header();
    } // master()
    
    
    void dispatch_to(dispatch_type f)
    {
        dispatch_ = f;
    } // dispatch_to()
    
    
    void close()
    {
        input_.close();
        output_.close();
    } // close()
    
    
    void send_to_erlang(const eixx::eterm& msg)
    {
        size_t buflen = msg.encode_size(HEADER_SIZE);
        char* data = new char[buflen];
        
        msg.encode(data, buflen, HEADER_SIZE);
        
        asio::async_write( output_
                         , asio::buffer(data, buflen)
                         , bind( &master::handle_write
                               , this
                               , boost::asio::placeholders::error
                               , data) );
    } // send_to_erlang()
    
    
protected:
    
    void start_read_header()
    {
        size_buf_[0] = size_buf_[1] = 0;
        
        asio::async_read( input_
                        , asio::buffer(size_buf_)
                        , bind( &master::handle_read_header
                              , this
                              , asio::placeholders::error) );
    } // start_read_header()
    
    
    void handle_read_header(const boost::system::error_code& error)
    {
        if ( error )
        {
            close();
            return;
        }
        
        assert(HEADER_SIZE == 2);
        int len = (size_buf_[0] << 8) | size_buf_[1];
        
        buffer_.resize(len);
        
        asio::async_read( input_
                        , asio::buffer(buffer_)
                        , bind( &master::handle_read_body
                              , this
                              , asio::placeholders::error) );

    } // handle_read_header()
    
    
    void handle_read_body(const boost::system::error_code& error)
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
                                
                start_read_header();
                return;
            }
            catch(...)
            {
                // error
            }
        }
        else
        {
            // an error
        }
        
            // we only reach here if something bad happened
        close();
    } // handle_read_body()
    
    
    void process_msg(const eixx::eterm& msg)
    {
        try
        {
            dispatch_(*this, msg);
        }
        catch(const eixx::eterm& et)
        {
            send_to_erlang(et);
        }
        catch(...)
        {
            // reply with error or crash?
            //post::make_error(0);
        }
    } // process_msg()

    
    void handle_write( const boost::system::error_code& error, char* data)
    {
        delete[] data;
    } // handle_write()
    
    
    
private:
    posix::stream_descriptor    input_;
    posix::stream_descriptor    output_;
    dispatch_type               dispatch_;
    byte                        size_buf_[HEADER_SIZE];
    std::vector<char>           buffer_;
    
}; // class master


} // namespace post

#endif // POSTMASTER_POSTMASTER_
