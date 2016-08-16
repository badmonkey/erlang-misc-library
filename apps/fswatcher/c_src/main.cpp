
#include "eixx/alloc_std.hpp"
#include "eixx/eixx.hpp"
#include "boost/asio.hpp"
#include "postmaster/postmaster.hpp"
#include "postmaster/postoffice.hpp"
#include "inotify.hpp"


void handle_msg(post::master& master, const eixx::eterm& in);
void handle_inotify(const inotify_event& evt);


///// -------------------------------------------------------------------- /////


asio::io_service&  get_io_service()
{
    static asio::io_service  singleService;
    return singleService;
} // get_io_service()


post::office&  get_master()
{
    static post::office  singleMaster( get_io_service() );
    return singleMaster;
} // get_master()


inotify&  get_inotify()
{
    static inotify  singleInotify( get_io_service(), &handle_inotify );
    return singleInotify;
} // get_inotify()


///// -------------------------------------------------------------------- /////


bool handle_add_msg( const eixx::eterm& a_pattern
                   , const eixx::varbind& a_varbind
                   , long a_opaque)
{
    get_inotify().add_target("/etc/", inotify::event::all | inotify::add_flags::dir_only);
    return true;
} // handle_add_msg()


bool handle_remove_msg( const eixx::eterm& a_pattern
                      , const eixx::varbind& a_varbind
                      , long a_opaque)
{
    return true;
} // handle_remove_msg()


///// -------------------------------------------------------------------- /////


void handle_inotify(const inotify_event& evt)
{
    get_master().send_to_erlang( eixx::am_ok );
} // handle_inotify()


///// -------------------------------------------------------------------- /////


int main()
{
        // if we can't start the postmaster there's not point trying the rest
    try { get_master(); } catch(...) { ::exit(-1); }
    
    
    try
    {
        get_inotify();
        
        get_master()
            .register_message("{add, Path :: string(), Mask :: int()}", &handle_add_msg)
            .register_message("{remove, Watch :: int()}", &handle_remove_msg)
            .send_to_erlang( eixx::atom("ready") );

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
