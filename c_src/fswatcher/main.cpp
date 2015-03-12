
#include "eixx/alloc_std.hpp"
#include "eixx/eixx.hpp"
#include "boost/asio.hpp"
#include "postmaster/postmaster.hpp"


eixx::eterm handle_msg(const eixx::eterm& in)
{
    return in;
} // handle_msg()


int main()
{
    try
    {
        boost::asio::io_service  io_service;

        post::master  master(io_service, &handle_msg);
        
        // other initialization

        io_service.run();
    }
    catch( std::exception& e )
    {
    }
    
        // We only get here if something went wrong
    return -1;
} // main()
