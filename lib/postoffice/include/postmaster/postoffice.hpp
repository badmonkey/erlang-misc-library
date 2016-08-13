#ifndef POSTMASTER_POSTOFFICE_
#define POSTMASTER_POSTOFFICE_

#include "postmaster.hpp"


namespace post
{
    
    
class office : public master
{
public:
    typedef eixx::eterm_pattern_matcher::pattern_functor_t  dispatch_type;
    
    
        office(asio::io_service&  io_service)
    : master(io_service, boost::bind(&office::handle_msg, this, _1, _2) )
    {
    } // office
    
    
    office& register_message(const char* pat, dispatch_type f, long user = 0)
    {
        matcher_.push_back(eixx::eterm::format(pat), f, user);
        return *this;
    } // register_message()
    
    
protected:
    
    void handle_msg( post::master& master
                   , const eixx::eterm& in)
    {
        if ( !matcher_.match(in) )
        {
            send_error_to_erlang( eixx::atom("unknown_msg") );
        }
    } // handle()
    
    
private:
    eixx::eterm_pattern_matcher  matcher_;
    
}; // class office


} // namespace post


#endif // POSTMASTER_POSTOFFICE_