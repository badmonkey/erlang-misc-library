//----------------------------------------------------------------------------
/// \file  alloc_pool.hpp
//----------------------------------------------------------------------------
/// \brief Custom memory allocator using a pool of objects.
//----------------------------------------------------------------------------
// Copyright (c) 2010 Serge Aleynikov <saleyn@gmail.com>
// Created: 2010-08-10
//----------------------------------------------------------------------------
/*
***** BEGIN LICENSE BLOCK *****

This file is part of the eixx (Erlang C++ Interface) Library.

Copyright (C) 2010 Serge Aleynikov <saleyn@gmail.com>

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

***** END LICENSE BLOCK *****
*/
#ifndef _EIXX_ALLOC_POOL_HPP_
#define _EIXX_ALLOC_POOL_HPP_

#include <eixx/namespace.hpp>  // definition of EIXX_NAMESPACE
#include <boost/pool/pool_alloc.hpp>

#define EIXX_USE_ALLOCATOR

namespace EIXX_NAMESPACE {

typedef boost::fast_pool_allocator<char> allocator_t;
//typedef boost::pool_allocator<char> allocator_t;

} // namespace EIXX_NAMESPACE

#endif // _EIXX_ALLOC_POOL_HPP_

