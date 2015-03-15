
#include "eixx/alloc_std.hpp"
#include "eixx/eixx.hpp"
#include "boost/asio.hpp"
#include "postmaster/postmaster.hpp"
#include "inotify.hpp"


void handle_msg(post::master& master, const eixx::eterm& in);
void handle_inotify(const inotify_event& evt);


///// -------------------------------------------------------------------- /////


asio::io_service&  get_io_service()
{
    static asio::io_service  singleService;
    return singleService;
} // get_io_service()


post::master&  get_master()
{
    static post::master  singleMaster( get_io_service(), &handle_msg );
    return singleMaster;
} // get_master()


inotify&  get_inotify()
{
    static inotify  singleInotify( get_io_service(), &handle_inotify );
    return singleInotify;
} // get_inotify()


///// -------------------------------------------------------------------- /////


void handle_msg(post::master& master, const eixx::eterm& in)
{
} // handle_msg()


///// -------------------------------------------------------------------- /////


void handle_inotify(const inotify_event& evt)
{
} // handle_inotify()


///// -------------------------------------------------------------------- /////


int main()
{
    // if we can start the postmaster there's not point trying the rest
    try { get_master(); } catch(...) { ::exit(-1); }
    
    
    try
    {
        get_inotify();
        
        // other initialization
        get_master().send_to_erlang( eixx::atom("ready") );

        get_io_service().run();
    }
    catch(const eixx::eterm& et)
    {
    }
    catch(...)
    {
    }
    
        // We only get here if something went wrong
    ::exit(-1);
} // main()
