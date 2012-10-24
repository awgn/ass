// 
//  Copyright (c) 2011 Bonelli Nicola <bonelli@antifork.org>
// 
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
// 
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
// 
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
// 
//  ass: C++11 code ass'istant
//

#ifndef __ASS_HPP__
#define __ASS_HPP__ 

#include <cxxabi.h>

#include <ccomplex>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <cassert>
#include <cmath>
#include <cctype>
#include <cerrno>
#include <climits>
#include <ctime>
#include <cfenv>
#include <cfloat>
#include <cstdint>
#include <cinttypes>
#include <clocale>
#include <csetjmp>
#include <csignal>
#include <cstdarg>
#include <cstddef>
#include <cstdbool>
#include <cwchar>
#include <cwctype>

#include <ios>
#include <iostream>
#include <fstream>
#include <sstream>
#include <streambuf>
#include <iomanip>
#include <exception>
#include <stdexcept>
#include <typeinfo>
#include <memory>
#include <limits>
#include <numeric>
#include <utility>
#include <string>
#include <locale>
#include <new>
#include <system_error>

#include <algorithm>
#include <functional>
#include <iterator>

#include <vector>
#include <list>
#include <map>
#include <set>
#include <deque>
#include <queue>
#include <stack>
#include <bitset>
#include <forward_list>

#include <initializer_list>

#include <chrono>
#include <ratio>

#include <complex>
#include <array>
#include <valarray>
#include <tuple>
#include <type_traits>
#include <random>
#include <regex>

#include <unordered_set>
#include <unordered_map>

//////////////////////////////////////////////////////// additional type_traits...

namespace ass 
{
    namespace traits {

    // For use in __is_convertible_simple.
    struct __sfinae_types
    {
      typedef char __one;
      typedef struct { char __arr[2]; } __two;
    };

    // has member type helper (using SFINAE... Vandevoorde/Josuttis)
    #define __has_member_type_helper(abc) \
    template <typename T>   \
    class __has_ ## abc ## _helper : public __sfinae_types   \
    {   \
        template <typename C> static __one test(typename std::remove_reference<typename C::abc>::type *);  \
        template <typename C> static __two test(...);   \
    \
    public: \
        enum { value = sizeof(test<T>(0)) == sizeof(__one) };   \
    }

    __has_member_type_helper(value_type);
    __has_member_type_helper(pointer);
    __has_member_type_helper(const_pointer);
    __has_member_type_helper(reference);
    __has_member_type_helper(const_reference);
    __has_member_type_helper(iterator);
    __has_member_type_helper(const_iterator);
    __has_member_type_helper(reverse_iterator);
    __has_member_type_helper(const_reverse_iterator);
    __has_member_type_helper(size_type);
    __has_member_type_helper(difference_type);

    template <typename T>
    struct has_value_type : public std::integral_constant<bool, __has_value_type_helper<T>::value>
    {};

    template <typename T>
    struct has_pointer : public std::integral_constant<bool, __has_pointer_helper<T>::value>
    {};

    template <typename T>
    struct has_const_pointer : public std::integral_constant<bool, __has_const_pointer_helper<T>::value>
    {};
    
    template <typename T>
    struct has_reference : public std::integral_constant<bool, __has_reference_helper<T>::value>
    {};

    template <typename T>
    struct has_const_reference : public std::integral_constant<bool, __has_const_reference_helper<T>::value>
    {};

    template <typename T>
    struct has_iterator : public std::integral_constant<bool, __has_iterator_helper<T>::value>
    {};

    template <typename T>
    struct has_const_iterator : public std::integral_constant<bool, __has_const_iterator_helper<T>::value>
    {};

    template <typename T>
    struct has_reverse_iterator : public std::integral_constant<bool, __has_reverse_iterator_helper<T>::value>
    {};

    template <typename T>
    struct has_const_reverse_iterator : public std::integral_constant<bool, __has_const_reverse_iterator_helper<T>::value>
    {};
    
    template <typename T>
    struct has_size_type : public std::integral_constant<bool, __has_size_type_helper<T>::value>
    {};
    
    template <typename T>
    struct has_difference_type : public std::integral_constant<bool, __has_difference_type_helper<T>::value>
    {};
    
    template <typename T>
    struct is_container : public std::integral_constant<bool, __has_value_type_helper<T>::value && 
                                                              __has_reference_helper<T>::value &&  
                                                              __has_const_reference_helper<T>::value &&  
                                                              __has_iterator_helper<T>::value && 
                                                              __has_const_iterator_helper<T>::value && 
                                                              __has_pointer_helper<T>::value &&  
                                                              __has_const_pointer_helper<T>::value &&  
                                                              __has_size_type_helper<T>::value &&  
                                                              __has_difference_type_helper<T>::value 
                                                               >
    {};

    // is_tuple 

    template <typename T>
    struct is_tuple : public std::integral_constant<bool, false>
    {};

    template <typename ...Ti>
    struct is_tuple<std::tuple<Ti...>> : public std::integral_constant<bool, true>
    {};

    // is_pair

    template <typename T>
    struct is_pair : public std::integral_constant<bool, false>
    {};

    template <typename T, typename U>
    struct is_pair<std::pair<T,U>> : public std::integral_constant<bool, true>
    {};

    // duration traits
    template <typename T>
    struct _duration_traits;
        template <> struct _duration_traits<std::chrono::nanoseconds> { static constexpr const char  *str = "_ns"; };
        template <> struct _duration_traits<std::chrono::microseconds> { static constexpr const char *str = "_us"; };
        template <> struct _duration_traits<std::chrono::milliseconds> { static constexpr const char *str = "_ms"; };
        template <> struct _duration_traits<std::chrono::seconds> { static constexpr const char *str = "_s"; };
        template <> struct _duration_traits<std::chrono::minutes> { static constexpr const char *str = "_m"; };
        template <> struct _duration_traits<std::chrono::hours> { static constexpr const char *str = "_h"; };

    } // namespace traits

} // namespace ass 


////////////////////////////////////////////////////////////// type utils 

namespace ass {

    static std::string
    cxa_demangle(const char *name)
    {
        int status;
        std::unique_ptr<char, void(*)(void *)> ret(abi::__cxa_demangle(name,0,0, &status), ::free);
        if (status < 0) {
            return std::string("?");
        }
        return std::string(ret.get());
    }

    template <bool is_ref, typename Tp>
    std::string type_name(Tp &&x)
    {
        typedef decltype(std::forward<Tp>(x)) decl_type;
        
        auto name = cxa_demangle(typeid(Tp).name());
        if (std::is_const<
             typename std::remove_reference<decl_type>::type>::value)
            name.append(" const");
        if (std::is_volatile<
             typename std::remove_reference<decl_type>::type>::value)
            name.append(" volatile");
        if (is_ref &&
            std::is_lvalue_reference<decl_type>::value)
            name.append("&");
        else if (std::is_rvalue_reference<decl_type>::value)
            name.append("&&");
        return name;
    }
    
} // namespace ass


///////////////// libmore show:


inline namespace ass_show {

    // forward declarations:
    //
    
    inline std::string 
    show(const char *v, const char *n = nullptr);
    
    inline std::string 
    show(std::string const &s, const char *n = nullptr);

    template <typename T> 
    inline 
    typename std::enable_if<std::is_arithmetic<T>::value, std::string>::type 
    show(T const &value, const char * n = nullptr);

    template <typename T>
    inline  
    typename std::enable_if<std::is_pointer<T>::value, std::string>::type 
    show(T const &p, const char *n = nullptr);

    template <typename U, typename V>
    inline std::string
    show(std::pair<U,V> const &r, const char * n = nullptr);

    template <typename T, std::size_t N>
    inline std::string
    show(std::array<T,N> const &a, const char * n = nullptr);

    template <typename ...Ts>
    inline std::string
    show(std::tuple<Ts...> const &t, const char * n = nullptr);

    template <typename Rep, typename Period>
    inline std::string
    show(std::chrono::duration<Rep, Period> const &dur, const char *n = nullptr);
    
    template <typename Clock, typename Dur>
    inline std::string
    show(std::chrono::time_point<Clock, Dur> const &r, const char *n = nullptr);

    template <typename T>
    inline typename std::enable_if<
        (!std::is_pointer<T>::value) && (
        (ass::traits::is_container<T>::value && !std::is_same<typename std::string,T>::value) ||
        (std::rank<T>::value > 0 && !std::is_same<char, typename std::remove_cv<typename std::remove_all_extents<T>::type>::type>::value)),
    std::string>::type 
    show(const T &v, const char * n = nullptr);
    
    namespace details {

        template <typename T>
        inline std::string 
        header(const char *n)
        {
            return n == nullptr ? std::string() : 
                    *n == '\0' ? ass::cxa_demangle(typeid(T).name()) + " " :
                     std::string(n) + " ";
        }

        // show_on policy 
        //

        template <typename T, int N>
        struct show_on
        {
            static inline
            void apply(std::string &out, const T &tupl, const char *n)
            {
                out += show(std::get< std::tuple_size<T>::value - N>(tupl), nullptr) + " ";
                show_on<T,N-1>::apply(out,tupl, n);
            }
        }; 
        template <typename T>
        struct show_on<T, 0>
        {
            static inline
            void apply(std::string&, const T &, const char *)
            {}
        };

        template <typename T>
        struct _duration_traits;
            template <> struct _duration_traits<std::chrono::nanoseconds>  { static constexpr const char *str = "_ns"; };
            template <> struct _duration_traits<std::chrono::microseconds> { static constexpr const char *str = "_us"; };
            template <> struct _duration_traits<std::chrono::milliseconds> { static constexpr const char *str = "_ms"; };
            template <> struct _duration_traits<std::chrono::seconds>      { static constexpr const char *str = "_s"; };
            template <> struct _duration_traits<std::chrono::minutes>      { static constexpr const char *str = "_m"; };
            template <> struct _duration_traits<std::chrono::hours>        { static constexpr const char *str = "_h"; };

    }

    ///////////////////////////////////////
    // show for const char *
    //
    
    inline std::string
    show(const char *v, const char *n)
    {
        return details::header<const char *>(n) + "\"" + std::string(v) + "\"";
    }
    
    ///////////////////////////////////////
    // show for std::string
    //
    
    inline std::string
    show(std::string const &s, const char *n)
    {
        return details::header<std::string>(n) + "\"" + s + "\"";
    }

    ///////////////////////////////////////
    // show for arithmetic types..
    //
    
    template <typename T>
    inline typename std::enable_if<std::is_arithmetic<T>::value, std::string>::type
    show(T const &value, const char * n)
    {
        return details::header<T>(n) + std::to_string(value);
    }

    ///////////////////////////////////////
    // show for pointers *
    //
    
    template <typename T> 
    inline typename std::enable_if<std::is_pointer<T>::value, std::string>::type
    show(T const &p, const char *n)
    {
        std::ostringstream o;
        o << static_cast<void *>(p);
        return details::header<T>(n) + o.str();
    }

    //////////////////////////
    // show for pair...

    template <typename U, typename V>
    inline std::string
    show(const std::pair<U,V> &r, const char * n)
    {
        return details::header<std::pair<U,V>>(n) + 
                "(" + show(r.first) + 
                "," + show(r.second) + ")";
    }

    ///////////////////////////
    // show for array...

    template <typename T, std::size_t N>
    inline std::string
    show(std::array<T,N> const &a, const char * n)
    {
        std::string out("[ ");
        details::show_on<std::array<T,N>, N>::apply(out,a, n ? "" : nullptr);
        return details::header<std::array<T,N>>(n) + out + "]";
    }

    ////////////////////////////////////////////////////////
    // show for tuple... 

    template <typename ...Ts>
    inline std::string
    show(std::tuple<Ts...> const &t, const char * n)
    {
        std::string out("{ ");
        details::show_on<std::tuple<Ts...>, sizeof...(Ts)>::apply(out,t, n ? "" : nullptr);
        return details::header<std::tuple<Ts...>>(n) + out + "}";
    }                                              

    ////////////////////////////////////////////////////////
    // show for chrono types... 

    template <typename Rep, typename Period>
    inline std::string
    show(std::chrono::duration<Rep, Period> const &dur, const char *n)
    {
        std::string out(std::to_string(dur.count()));
        return details::header<std::chrono::duration<Rep,Period>>(n) + out + std::string(details::_duration_traits<std::chrono::duration<Rep,Period>>::str);
    }

    template <typename Clock, typename Dur>
    inline std::string
    show(std::chrono::time_point<Clock, Dur> const &r, const char *n)
    {    
        return details::header<std::chrono::time_point<Clock,Dur>>(n) + show(r.time_since_epoch());
    }

    ///////////////////////////////////////
    // show for generic containers...
    //

    template <typename T>
    inline typename std::enable_if<
        (!std::is_pointer<T>::value) && (
        (ass::traits::is_container<T>::value && !std::is_same<typename std::string,T>::value) ||
        (std::rank<T>::value > 0 && !std::is_same<char, typename std::remove_cv<typename std::remove_all_extents<T>::type>::type>::value)),
    std::string>::type 
    show(const T &v, const char * n)
    {
        std::string s("{ ");
        for(auto & e : v)
        {
            s += show(e) + " ";
        }
        return details::header<T>(n) + s + "}";
    };

} // namespace ass_show


namespace std 
{
    ////////////////////////////////////////////
    // operator<< for types that can be shown...

    template <typename CharT, typename Traits, typename T>
    inline std::basic_ostream<CharT, Traits> &
    operator<< (std::basic_ostream<CharT, Traits> &out, const T &value)
    {
        return out << show(value);
    }

} // namespace std


////////////////////////////////////////////////////////////// simple Oracle class


struct O
{
    template <typename T, typename CharT, typename Traits>
    static void print(std::basic_ostream<CharT,Traits> &out, T elem)
    {
         static std::string last_token;
 
         std::ostringstream ss; ss << elem;
         std::string token = ss.str();
 
         if ( last_token != token ) {
             out << token; last_token = std::move(token);
         }
         else {
             out << '.';
         }
    }

    template <typename T>
    static void print_type(std::ostringstream &out, T&& arg)
    {
        out << ass::type_name<std::is_reference<T>::value>(std::forward<T>(arg));
    }
    template <typename T, typename ...Ti>
    static void print_type(std::ostringstream &out, T&& arg, Ti&&...args)
    {
        out << ass::type_name<std::is_reference<T>::value>(std::forward<T>(arg)) << ',';
        print_type(out, std::forward<Ti>(args)...);
    }

    std::intptr_t value;
    
    O() : 
    value(reinterpret_cast<std::intptr_t>(this)) 
    { 
        print(std::cout," O()"); 
    } 
   
    O(O &other) :
    value(other.value) 
    { 
        print(std::cout," O(O&)"); 
    } 

    O(const O &other) :
    value(other.value) 
    { 
        print(std::cout," O(const O&)"); 
    } 

    O &operator=(const O &other) 
    { 
        value = other.value;
        print(std::cout," op=(const O&)"); return *this; 
    } 

    ~O() 
    { 
        print(std::cout," ~O()"); 
    }

    O(O &&other) :
    value(other.value)    
    { 
        other.value = 0xdeadbeef;
        print(std::cout," O(O&&)"); 
    } 

    O &operator=(O &&other) 
    { 
        value = other.value;
        other.value = 0xdeadbeef;        
        print(std::cout," op=(O&&)"); 
        return *this; 
    } 
    
    template <typename ...Ti> 
    explicit O(Ti&& ...arg) :
    value(reinterpret_cast<std::intptr_t>(this)) 
    {
        std::ostringstream ss; ss << " O(";
        print_type(ss, std::forward<Ti>(arg)...);
        ss << ")";
        print(std::cout,ss.str().c_str()); 
    } 

    void swap(O &rhs) 
    { 
        std::swap(value, rhs.value);
        print(std::cout," swap(O,O)"); 
    }

    bool operator<(const O &rhs) const
    {
         print(std::cout," <");
         return value < rhs.value;
    }
    bool operator>=(const O &rhs) const
    {
         print(std::cout," >=");
         return !(*this < rhs);   
    }
    bool operator>(const O &rhs) const
    {
         print(std::cout," >");
         return rhs < *this;
    }
    bool operator<=(const O &rhs) const
    {
         print(std::cout," <=");
         return !(rhs < *this);   
    }

    bool operator==(const O &rhs) const
    {
         print(std::cout," ==");
         return value == rhs.value;
    }
    bool operator!=(const O &rhs) const
    {
         print(std::cout," !=");
         return !(*this == rhs);
    }
};  

template <typename CharT, typename Traits>
typename std::basic_ostream<CharT, Traits> &
operator<<(std::basic_ostream<CharT,Traits> &out, const O & rhs)
{
    std::ostringstream ss; ss << " O@" << (void *)rhs.value;
    O::print(out, ss.str().c_str());
    return out;
}

////////////////////////////////////////////////////////////// R(): Ranges ala Haskell 

// The following implementation mimics the std::initializer_list, and can be constructed
// by the user. We all know that what follows is barely legal and is not guaranteed to work 
// by the standard (fake and standard initializer list have indeed a different layout).
// Although, it works with gcc and clang, and no better way to do it is possibile (Nicola).

namespace ass {

    template<class _E>
    class initializer_list
    {
    public:
      typedef _E 		    value_type;
      typedef const _E& 	reference;
      typedef const _E& 	const_reference;
      typedef size_t 		size_type;
      typedef const _E* 	iterator;
      typedef const _E* 	const_iterator;

    private:
      iterator			_M_array;
      size_type			_M_len;

    public:
      // User can call this public constructor.
      initializer_list(const_iterator __a, size_type __l)
      : _M_array(__a), _M_len(__l) { }

      initializer_list() noexcept
      : _M_array(0), _M_len(0) { }

      // Number of elements.
      size_type
      size() const noexcept { return _M_len; }

      // First element.
      const_iterator
      begin() const noexcept { return _M_array; }

      // One past the last element.
      const_iterator
      end() const noexcept { return begin() + size(); }
    };
        
    static_assert(sizeof(initializer_list<int>) == sizeof(std::initializer_list<int>), "ass::initializer_list<_E>");
}

template <typename Tp = int>
std::initializer_list<Tp> 
R(int a0, int a1, int b)
{                                 
    int step = a1 - a0;
    int size = (b - a0 + (step < 0 ? -1 : 1))/step; 
    size = (size > 0 ? size : 0);
    
    Tp * leak = nullptr;   // leak! a per-thread static should be a viable option...
    if (size) {
        leak = static_cast<Tp *>(realloc(leak, size * sizeof(Tp)));
        for(int n = 0; (step > 0  ? a0 <= b : a0 >= b); a0 += step, n++)
            new (leak+n) Tp(a0);  
    }
    ass::initializer_list<Tp> ret(leak,size);
    return reinterpret_cast<std::initializer_list<Tp> &>(ret);
}

template <typename Tp = int>
std::initializer_list<Tp> 
R(int a, int b)
{
    return R<Tp>(a,a+1,b);
}

////////////////////////////////////////////////////////////// P(): generic variadic print

template <typename T>
void P(T &&arg)
{
    std::cout << std::forward<T>(arg);
}
template <typename T, typename ...Ti>
void P(T &&arg, Ti&&... args)
{
    std::cout << arg << ' ';
    P(std::forward<Ti>(args)...);
}

////////////////////////////////////////////////////////////// _(): build pairs and tuples 

template <typename T1, typename T2>
std::pair<T1,T2> _(T1 &&arg1, T2 &&arg2)
{
    return std::make_pair(std::forward<T1>(arg1),
                          std::forward<T2>(arg2));
}
template <typename ... T>
std::tuple<T...> _(T&& ...arg)
{
    return std::make_tuple(std::forward<T>(arg) ...);
}

////////////////////////////////////////////////////////////// _T(): return the type of an expression
//

#define _T(x)   ass::type_name<is_reference<decltype(x)>::value>(x)


using namespace std;
using namespace std::chrono;
using namespace std::placeholders;

#endif /* __ASS_HPP__ */
