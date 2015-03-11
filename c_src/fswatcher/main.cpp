
#include "eixx/alloc_std.hpp"
#include "eixx/eixx.hpp"
#include "boost/asio.hpp"
#include "postmaster/postmaster.hpp"



int main()
{
    try
    {

        boost::asio::io_service  io_service;

        //posix_chat_client c(io_service, iterator);

        io_service.run();
    }
    catch( std::exception& e )
    {
        std::cerr << "Exception: " << e.what() << "\n";

    }
    return 0;
} // main()
