
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


eixx::eterm_pattern_matcher& get_matcher()
{
    static eixx::eterm_pattern_matcher  singleMatcher;
    return singleMatcher;
} // get_matcher()


///// -------------------------------------------------------------------- /////


void handle_msg( post::master& master
               , const eixx::eterm& in)
{
    if ( !get_matcher().match(in) )
    {
        master.send_to_erlang( eixx::tuple::make( eixx::am_error, eixx::atom("unknown_msg") ) );
    }
} // handle_msg()


bool handle_add_msg( const eixx::eterm& a_pattern
                   , const eixx::varbind& a_varbind
                   , long a_opaque)
{
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
} // handle_inotify()


///// -------------------------------------------------------------------- /////


void add_pattern( const char* pat
                , eixx::eterm_pattern_matcher::pattern_functor_t f
                , long user)
{
    get_matcher().push_back(eixx::eterm::format(pat), f, user);
} // add_pattern()


int main()
{
        // if we can't start the postmaster there's not point trying the rest
    try { get_master(); } catch(...) { ::exit(-1); }
    
    
    try
    {
        get_inotify();
        
        add_pattern("{add, Path :: string(), Mask :: int()}", &handle_add_msg, 1);
        add_pattern("{remove, Watch :: int()}", &handle_remove_msg, 2);
        
        
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
