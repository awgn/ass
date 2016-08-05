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
#include <ciso646>

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

#include <thread>
#include <mutex>
#include <condition_variable>
#include <future>
#include <atomic>

#if __cplusplus >= 201300L

#include <experimental/optional>
#include <experimental/string_view>

#endif


#ifndef ASS_PASTE
#define ASS_PASTE(a,b)      a ## b
#define ASS_XPASTE(a,b)     ASS_PASTE(a,b)
#endif

namespace ass
{
    namespace traits {

    // For use in __is_convertible_simple.
    struct __sfinae_types
    {
      typedef char __one;
      typedef struct { char __arr[2]; } __two;
    };

    // is_class_or_union (using SFINAE... Vandevoorde/Josuttis)
    template <typename T>
    class __is_class_or_union_helper : public __sfinae_types
    {
        template <typename C> static __one test(int C::*);
        template <typename C> static __two test(...);

    public:
        enum { value = sizeof(test<T>(0)) == sizeof(__one) };
    };

    template <typename T>
    struct is_class_or_union : public std::integral_constant<bool, __is_class_or_union_helper<T>::value>
    {};

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
    __has_member_type_helper(key_type);
    __has_member_type_helper(mapped_type);
    __has_member_type_helper(container_type);

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

    template <typename t>
    struct has_key_type : public std::integral_constant<bool, __has_key_type_helper<t>::value>
    {};

    template <typename t>
    struct has_mapped_type : public std::integral_constant<bool, __has_mapped_type_helper<t>::value>
    {};

    template <typename t>
    struct has_container_type : public std::integral_constant<bool, __has_container_type_helper<t>::value>
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


    // is_container

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


    // has_insertion_operator: operator<<()

    template <typename T>
    class has_insertion_operator : public __sfinae_types
    {
        template <typename C> static __one test(typename std::remove_reference<decltype((std::cout << std::declval<C>()))>::type *);
        template <typename C> static __two test(...);
    public:
        enum { value = sizeof(test<T>(0)) == sizeof(__one) };
    };

    // has_extraction_operator: operator>>()

    template <typename T>
    class has_extraction_operator : public __sfinae_types
    {
        template <typename C> static __one test(typename std::remove_reference<decltype((std::cin >> std::declval<C &>()))>::type *);
        template <typename C> static __two test(...);
    public:
        enum { value = sizeof(test<T>(0)) == sizeof(__one) };
    };

    } // namespace traits


    // utilities
    //

    inline std::string
    demangle(const char * name)
    {
        int status;
        std::unique_ptr<char, void(*)(void *)> ret(abi::__cxa_demangle(name,0,0, &status), ::free);
        if (status < 0) {
            return std::string(1,'?');
        }
        return std::string(ret.get());
    }

    // xray pointer
    //

    template <typename Tp>
    struct xray_base
    {
        typedef typename std::remove_reference<Tp>::type T;

        xray_base()
        : v1{}
        , v2{}
        { }

        xray_base(std::unique_ptr<T> a1, std::unique_ptr<T> a2)
        : v1(std::move(a1))
        , v2(std::move(a2))
        { }

        std::unique_ptr<T> v1;
        std::unique_ptr<T> v2;

        static std::pair<void *, void *>
        get_memory()
        {
            auto p1 = malloc(sizeof(Tp));
            auto p2 = malloc(sizeof(Tp));

            auto fill = [](void * start, size_t len, size_t init)
            {
                auto p = static_cast<char *>(start);
                for(auto n = init; n < init + len ; ++n)
                    *p++ = n;
            };

            fill(p1, sizeof(Tp), 0);
            fill(p2, sizeof(Tp), sizeof(Tp));

            return std::make_pair(p1, p2);
        }
    };


    template <typename Tp>
    struct xray_ptr : xray_base<Tp>
    {
        typedef typename std::remove_reference<Tp>::type T;

        xray_ptr()
        : xray_base<Tp>{}
        { }

        xray_ptr(std::unique_ptr<T> a1, std::unique_ptr<T> a2)
        : xray_base<Tp>(std::move(a1), std::move(a2))
        { }

        static xray_ptr<T>
        make_default()
        {
            void *p1, *p2; std::tie(p1,p2) = xray_base<Tp>::get_memory();

            new (p1) typename std::remove_cv<T>::type;
            new (p2) typename std::remove_cv<T>::type;

            return { std::unique_ptr<T>(reinterpret_cast<T *>(p1)),
                        std::unique_ptr<T>(reinterpret_cast<T *>(p2)) };
        }

        template <typename ...Ts>
        static xray_ptr<T>
        make_value(Ts && ... args)
        {
            void *p1, *p2; std::tie(p1,p2) = xray_base<Tp>::get_memory();

            new (p1) T (std::forward<Ts>(args)...);
            new (p2) T (std::forward<Ts>(args)...);

            return { std::unique_ptr<T>(reinterpret_cast<T *>(p1)),
                        std::unique_ptr<T>(reinterpret_cast<T *>(p2)) };
        }

        template <typename ...Ts>
        static xray_ptr<T>
        make_uniform(Ts && ... args)
        {
            void *p1, *p2; std::tie(p1,p2) = xray_base<Tp>::get_memory();

            new (p1) T { std::forward<Ts>(args)... };
            new (p2) T { std::forward<Ts>(args)... };

            return { std::unique_ptr<T>(reinterpret_cast<T *>(p1)),
                        std::unique_ptr<T>(reinterpret_cast<T *>(p2)) };
        }

    };

    template <>
    struct xray_ptr<void> : xray_base<int>
    {
        static xray_ptr<int> make_default() { return xray_ptr<int>{}; }

        template <typename ...Ts>
        static xray_ptr<int> make_value(Ts && ... ) { return xray_ptr<int>{}; }

        template <typename ...Ts>
        static xray_ptr<int> make_uniform(Ts && ... ) { return xray_ptr<int>{}; }
    };


    template <typename R, typename ...Tx>
    struct xray_ptr<R(Tx...)> : xray_base<int>
    {
        static xray_ptr<int> make_default() { return xray_ptr<int>{}; }
        template <typename ...Ts>
        static xray_ptr<int> make_value(Ts && ... ) { return xray_ptr<int>{}; }
        template <typename ...Ts>
        static xray_ptr<int> make_uniform(Ts && ... ) { return xray_ptr<int>{}; }
    };


    template<typename Tp>
    inline std::string
    show(xray_ptr<Tp> const& value)
    {
        std::string ret;

        if (!value.v1 || !value.v2)
            return ret;

        ret.reserve(8);
        auto p1 = reinterpret_cast<char const *>(value.v1.get());
        auto p2 = reinterpret_cast<char const *>(value.v2.get());

        for(size_t n = 0; n < sizeof(Tp); ++n)
        {
            if (p1[n] == p2[n])
                ret += (p1[n] == 0 ? '0' : 'X');
            else
                ret += (p1[n] == static_cast<char>(n) && p2[n] == static_cast<char>(n + sizeof(Tp)) ? '_' : '?');
        }

        return ret;
    }

    // manipulators
    //

    template <typename T>
    struct _hex
    {
        T value;
    };

    template <typename T>
    _hex<T> hex(T const &value)
    {
        return _hex<T>{value};
    }

    template <typename T>
    struct _oct
    {
        T value;
    };

    template <typename T>
    _oct<T> oct(T const &value)
    {
        return _oct<T>{value};
    }

    template <typename T>
    struct _bin
    {
        T value;
    };

    template <typename T>
    _bin<T> bin(T const &value)
    {
        return _bin<T>{value};
    }

    // forward declarations:
    //

    inline std::string
    show(char c);

    inline std::string
    show(bool);

    inline std::string
    show(const char *v);

    inline std::string
    show(std::string const &s);

    // numeric like...

    template <typename T>
    inline typename std::enable_if<std::is_arithmetic<T>::value || (std::is_enum<T>::value && std::is_convertible<T,int>::value), std::string>::type
    show(T const &value);

    // manipulators...

    template <typename T>
    inline typename std::enable_if<std::is_integral<T>::value, std::string>::type
    show(_hex<T> const &value);

    template <typename T>
    inline typename std::enable_if<std::is_integral<T>::value, std::string>::type
    show(_oct<T> const &value);

    template <typename T>
    inline typename std::enable_if<std::is_integral<T>::value, std::string>::type
    show(_bin<T> const &value);

    // pointers...

    template <typename T>
    inline std::string
    show(T const *p);

    template <typename T>
    inline std::string
    show(std::unique_ptr<T> const &);

    template <typename T>
    inline std::string
    show(std::shared_ptr<T> const &);

#if __cplusplus >= 201300L
    template <typename T>
    inline std::string
    show(std::experimental::optional<T> const &);

    template <typename T>
    inline std::string
    show(std::experimental::string_view const &);
#endif

    // pair<>

    template <typename U, typename V>
    inline std::string
    show(std::pair<U,V> const &r);

    // array...

    template <typename T, std::size_t N>
    inline std::string
    show(std::array<T,N> const &a);

    // tuple<>

    template <typename ...Ts>
    inline std::string
    show(std::tuple<Ts...> const &t);

    // chrono types

    template <typename Rep, typename Period>
    inline std::string
    show(std::chrono::duration<Rep, Period> const &dur);

    template <typename Clock, typename Dur>
    inline std::string
    show(std::chrono::time_point<Clock, Dur> const &r);

    // initializer list

    template <typename T>
    inline std::string
    show(std::initializer_list<T> const &);

    // integral_constant

    template <typename Tp, Tp Value>
    inline std::string
    show(std::integral_constant<Tp,Value>);

    template <bool Value>
    inline std::string
    show(std::integral_constant<bool,Value>);

    // containers

    template <typename T>
    inline typename std::enable_if<
    (!std::is_pointer<T>::value) && (
        (ass::traits::is_container<T>::value && !std::is_same<typename std::string,T>::value) ||
        (std::rank<T>::value > 0 && !std::is_same<char, typename std::remove_cv<typename std::remove_all_extents<T>::type>::type>::value)),
    std::string>::type
    show(const T &v);

    namespace details
    {
        // show_on policy
        //

        template <typename T, int N>
        struct show_on
        {
            static inline
            void apply(std::string &out, const T &tupl)
            {
                out += show(std::get< std::tuple_size<T>::value - N>(tupl)) + ' ';
                show_on<T,N-1>::apply(out,tupl);
            }
        };
        template <typename T>
        struct show_on<T, 0>
        {
            static inline
            void apply(std::string&, const T &)
            {}
        };

        // generic_show_on...
        //

        template <typename T, typename Tp, int N>
        struct generic_show_on
        {
            static inline
            void apply(std::string &out, const T &tupl, const Tp &value)
            {
                auto p = std::get< std::tuple_size<T>::value - N>(tupl);

                out += p.first + " = " +  show (std::bind(p.second, value)());
                if (N > 1)
                    out += ", ";
                generic_show_on<T, Tp, N-1>::apply(out,tupl, value);
            }
        };
        template <typename T, typename Tp>
        struct generic_show_on<T, Tp, 0>
        {
            static inline
            void apply(std::string&, const T &, const Tp &)
            {}
        };

        template <typename T>
        struct duration_traits;
        template <> struct duration_traits<std::chrono::nanoseconds>  { static constexpr const char *str = "_ns"; };
        template <> struct duration_traits<std::chrono::microseconds> { static constexpr const char *str = "_us"; };
        template <> struct duration_traits<std::chrono::milliseconds> { static constexpr const char *str = "_ms"; };
        template <> struct duration_traits<std::chrono::seconds>      { static constexpr const char *str = "_s"; };
        template <> struct duration_traits<std::chrono::minutes>      { static constexpr const char *str = "_m"; };
        template <> struct duration_traits<std::chrono::hours>        { static constexpr const char *str = "_h"; };

    } // namespace details


    ///////////////////////////////////////
    // show with additional header/type:
    //

    template <typename Tp>
    inline std::string
    show(Tp &&type, const char *n)
    {
        auto hdr = n == nullptr ? "" :
                   n[0] == '\0' ? ass::demangle(typeid(Tp).name()) : n;

        return std::move(hdr) + ' ' + show(std::forward<Tp>(type));
    }

    ///////////////////////////////////////
    // show for char

    inline std::string
    show(char c)
    {
        return std::string(1, c);
    }

    ///////////////////////////////////////
    // show for bool

    inline std::string
    show(bool v)
    {
        return v ? "true" : "false";
    }

    ///////////////////////////////////////
    // show for const char *

    inline std::string
    show(const char *v)
    {
        if (v != nullptr)
            return '"' + std::string(v) + '"';
        else
            return "\"nullptr\"";
    }

    ///////////////////////////////////////
    // show for std::string

    inline std::string
    show(std::string const &s)
    {
        return '"' + s + '"';
    }

    ///////////////////////////////////////
    // show for arithmetic types..

    template <typename T>
    inline typename std::enable_if<std::is_arithmetic<T>::value || (std::is_enum<T>::value && std::is_convertible<T,int>::value), std::string>::type
    show(T const &value)
    {
        return std::to_string(value);
    }

    /////////////////////////////////////////////
    // show for arithmetic types as hex values...

    template <typename T>
    inline typename std::enable_if<std::is_integral<T>::value, std::string>::type
    show(_hex<T> const &value)
    {
        std::ostringstream out;
        out << std::hex;

        if (std::is_same<T, uint8_t>::value)
            out << static_cast<uint32_t>(value.value);
        else
            out << value.value;

        return out.str();
    }

    /////////////////////////////////////////////
    // show for arithmetic types as oct values...

    template <typename T>
    inline typename std::enable_if<std::is_integral<T>::value, std::string>::type
    show(_oct<T> const &value)
    {
        std::ostringstream out;
        out << std::oct << value.value;

        if (std::is_same<T, uint8_t>::value)
            out << static_cast<uint32_t>(value.value);
        else
            out << value.value;

        return out.str();
    }

    /////////////////////////////////////////////
    // show for arithmetic types as bin values...

    template <typename T>
    inline typename std::enable_if<std::is_integral<T>::value, std::string>::type
    show(_bin<T> const &value)
    {
        std::ostringstream out;

        std::function<void(T)> binary = [&] (T value)
        {
            T rem;

            if(value <= 1) {
                out << value;
                return;
            }

            rem = value % 2;
            binary(value >> 1);

            out << rem;
        };

        binary(value.value);
        return out.str();
    }

    ///////////////////////////////////////
    // show for pointers *

    template <typename T>
    inline std::string
    show(T const *p)
    {
        std::ostringstream out;
        out << static_cast<const void *>(p);
        return out.str();
    }

    ///////////////////////////////////////
    // show for unique_ptr

    template <typename T>
    inline std::string
    show(std::unique_ptr<T> const &p)
    {
        std::ostringstream out;
        out << static_cast<const void *>(p.get()) << "_up";
        return out.str();
    }

    ///////////////////////////////////////
    // show for shared_ptr

    template <typename T>
    inline std::string
    show(std::shared_ptr<T> const &p)
    {
        std::ostringstream out;
        out << static_cast<const void *>(p.get()) << "_sp" << p.use_count();
        return out.str();
    }

#if __cplusplus >= 201300L
    ///////////////////////////////////////
    // show for optional

    template <typename T>
    inline std::string
    show(std::experimental::optional<T> const &p)
    {
        std::ostringstream out;
        out << '(';
        if (static_cast<bool>(p))
            out << *p;
        out << ')';
        return out.str();
    }

    ///////////////////////////////////////
    // show for string_view

    template <typename T>
    inline std::string
    show(std::experimental::string_view p)
    {
        return p.to_string();
    }
#endif

    //////////////////////////
    // show for pair...

    template <typename U, typename V>
    inline std::string
    show(const std::pair<U,V> &r)
    {
        return  '(' + show(r.first) + ' ' + show(r.second) + ')';
    }

    ///////////////////////////
    // show for array...

    template <typename T, std::size_t N>
    inline std::string
    show(std::array<T,N> const &a)
    {
        std::string out("[");
        details::show_on<std::array<T,N>, N>::apply(out,a);
        return std::move(out) + ']';
    }

    ////////////////////////////////////////////////////////
    // show for tuple...

    template <typename ...Ts>
    inline std::string
    show(std::tuple<Ts...> const &t)
    {
        std::string out("( ");
        details::show_on<std::tuple<Ts...>, sizeof...(Ts)>::apply(out,t);
        return std::move(out) + ')';
    }

    ////////////////////////////////////////////////////////
    // show for chrono types...

    template <typename Rep, typename Period>
    inline std::string
    show(std::chrono::duration<Rep, Period> const &dur)
    {
        std::string out(std::to_string(dur.count()));
        return std::move(out) + details::duration_traits<std::chrono::duration<Rep,Period>>::str;
    }

    template <typename Clock, typename Dur>
    inline std::string
    show(std::chrono::time_point<Clock, Dur> const &r)
    {
        return show(r.time_since_epoch());
    }

    // initializer list

    template <typename T>
    inline std::string
    show(std::initializer_list<T> const &init)
    {
        std::string out("{ ");
        for(auto const & e : init)
        {
            out += show(e) + ' ';
        }
        return std::move(out) + '}';
    }

    // integral_constant

    template <typename Tp, Tp Value>
    inline std::string
    show(std::integral_constant<Tp,Value>)
    {
        std::string out(std::to_string(Value));

        return out + "_" + ass::demangle(typeid(Tp).name());
    }

    template <bool Value>
    inline std::string
    show(std::integral_constant<bool,Value>)
    {
        std::string out(Value ? "true" : "false");

        return std::string(Value ? "true" : "false") + "_type";
    }

    ///////////////////////////////////////
    // show for generic containers...

    template <typename T>
    inline typename std::enable_if<
    (!std::is_pointer<T>::value) && (
        (ass::traits::is_container<T>::value && !std::is_same<typename std::string,T>::value) ||
        (std::rank<T>::value > 0 && !std::is_same<char, typename std::remove_cv<typename std::remove_all_extents<T>::type>::type>::value)),
    std::string>::type
    show(const T &v)
    {
        std::string out("[ ");
        for(auto const & e : v)
        {
            out += show(e) + ' ';
        }
        return std::move(out) + ']';
    }

    //////////////////////////////////////////
    // generic_show for user defined types...

    template <typename Tp, typename ...Ps>
    struct generic_show
    {
        template <typename ...Ts>
        generic_show(Ts && ...args)
        : data_(std::forward<Ts>(args)...)
        {}

        std::string
        operator()(Tp const &value)
        {
            auto out = ass::demangle(typeid(Tp).name()) + "{";

            details::generic_show_on<std::tuple<Ps...>, Tp, sizeof...(Ps)>::apply(out, data_, value);

            return out + "}";
        }

        std::tuple<Ps...> data_;
    };

    template <typename Tp, typename ...Ts>
    generic_show<Tp, Ts...>
    make_generic_show(Ts && ... args)
    {
        return generic_show<Tp, Ts...>(std::forward<Ts>(args)...);
    }


} // namespace ass_inline


namespace std
{
    ////////////////////////////////////////////
    // operator<< for types that can be shown...

    template <typename CharT, typename Traits, typename T>
    inline typename
    std::enable_if<!::ass::traits::has_insertion_operator<T>::value,
    std::basic_ostream<CharT, Traits>>::type &
    operator<< (std::basic_ostream<CharT, Traits> &out, const T &value)
    {
        return out << ass::show(value);
    }

} // namespace std


////////////////////////////////////////////////////////////// type utils

namespace ass {

    template <typename Tp>
    std::string add_cv_qualifier(std::string name)
    {
        if (std::is_volatile<Tp>::value)
            name += " volatile";
        if (std::is_const<Tp>::value)
            name += " const";
        return name;
    }

    template <typename Tp>
    std::string add_ref_qualifier(std::string name)
    {
        if (std::is_lvalue_reference<Tp>::value)
            name += "&";
        else if (std::is_rvalue_reference<Tp>::value)
            name += "&&";
        return name;
    }

    template <typename Tp>
    std::string type_name()
    {
        typedef typename std::remove_reference<Tp>::type T;

        auto name = add_cv_qualifier<Tp>(ass::demangle(typeid(T).name()));

        if (std::is_reference<Tp>::value)
        {
            name = add_cv_qualifier<T>(std::move(name));
            name = add_ref_qualifier<Tp>(std::move(name));
        }

        return name;
    }

    template <typename Tp>
    std::string
    type_of(Tp &&)
    {
        return type_name<Tp &&>();
    }

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
            out << type_of<T>(std::forward<T>(arg));
        }
        template <typename T, typename ...Ti>
        static void print_type(std::ostringstream &out, T&& arg, Ti&&...args)
        {
            out << type_of<T>(std::forward<T>(arg)) << ',';
            print_type(out, std::forward<Ti>(args)...);
        }

        std::intptr_t value;

        O() :
        value(reinterpret_cast<std::intptr_t>(this))
        {
            print(std::cout,"O() ");
        }

        O(O &other) :
        value(other.value)
        {
            print(std::cout,"O(O&) ");
        }

        O(const O &other) :
        value(other.value)
        {
            print(std::cout,"O(const O&) ");
        }

        O &operator=(const O &other)
        {
            value = other.value;
            print(std::cout,"op=(const O&) "); return *this;
        }

        ~O()
        {
            print(std::cout,"~O() ");
        }

        O(O &&other) noexcept
        : value(other.value)
        {
            other.value = 0xdeadbeef;
            print(std::cout,"O(O&&) ");
        }

        O &operator=(O &&other) noexcept
        {
            value = other.value;
            other.value = 0xdeadbeef;
            print(std::cout,"op=(O&&) ");
            return *this;
        }

        template <typename ...Ti>
        explicit O(Ti&& ...arg)
        : value(reinterpret_cast<std::intptr_t>(this))
        {
            std::ostringstream ss; ss << "O(";
            print_type(ss, std::forward<Ti>(arg)...);
            ss << ")";
            print(std::cout,ss.str().c_str());
        }

        void swap(O &rhs) noexcept
        {
            std::swap(value, rhs.value);
            print(std::cout,"swap(O,O) ");
        }

        bool operator<(const O &rhs) const
        {
            print(std::cout,"< ");
            return value < rhs.value;
        }
        bool operator>=(const O &rhs) const
        {
            print(std::cout,">= ");
            return !(*this < rhs);
        }
        bool operator>(const O &rhs) const
        {
            print(std::cout,"> ");
            return rhs < *this;
        }
        bool operator<=(const O &rhs) const
        {
            print(std::cout,"<= ");
            return !(rhs < *this);
        }

        bool operator==(const O &rhs) const
        {
            print(std::cout,"== ");
            return value == rhs.value;
        }
        bool operator!=(const O &rhs) const
        {
            print(std::cout,"!= ");
            return !(*this == rhs);
        }
    };

    inline std::string
    show(const O &that)
    {
        return "O@" + show(hex(that.value));
    }

    template <typename CharT, typename Traits>
    typename std::basic_ostream<CharT, Traits> &
    operator<<(std::basic_ostream<CharT,Traits> &out, const O & that)
    {
        std::ostringstream ss; ss << "O@" << (void *)that.value << ' ';
        O::print(out, ss.str().c_str());
        return out;
    }

    template <typename CharT, typename Traits>
    typename std::basic_istream<CharT, Traits> &
    operator>>(std::basic_istream<CharT,Traits>& in, O& that)
    {
        std::ostringstream ss; ss << "->O@" << (void *)that.value << ' ';
        O::print(std::cout, ss.str().c_str());
        return in;
    }

#if __cplusplus >= 201103L

    /// user defined literal... (waiting C++14!)

    inline std::basic_string<char>
    operator "" _s(const char* str, size_t len)
    {
        return std::basic_string<char>{str, len};
    }

    constexpr
    std::chrono::hours operator"" _h(unsigned long long n)
    {
        return std::chrono::hours{n};
    }

    constexpr
    std::chrono::minutes operator"" _min(unsigned long long n)
    {
        return std::chrono::minutes{n};
    }

    constexpr
    std::chrono::seconds operator"" _s(unsigned long long n)
    {
        return std::chrono::seconds{n};
    }

    constexpr
    std::chrono::milliseconds operator"" _ms(unsigned long long n)
    {
        return std::chrono::milliseconds{n};
    }

    constexpr
    std::chrono::microseconds operator"" _us(unsigned long long n)
    {
        return std::chrono::microseconds{n};
    }

    constexpr
    std::chrono::nanoseconds operator"" _ns(unsigned long long n)
    {
        return std::chrono::nanoseconds{n};
    }

#endif

} // namespace ass_inline


namespace std
{
    template <>
    inline void swap<ass::O>(ass::O & lhs, ass::O & rhs) noexcept
    {
        lhs.swap(rhs);
    }
}


////////////////////////////////////////////////////////////// R(): Ranges ala Haskell

// The following implementation mimics the std::initializer_list, only it and can be constructed
// by the user. This is *not* guaranteed to work by the standard (my initializer_list and the standard
// initializer list have indeed different layouts).

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


    template <typename Tp = int>
    std::initializer_list<Tp>
    R(Tp a0, Tp a1, Tp b)
    {
        auto step = a1 - a0;
        if (step == 0)
            throw std::runtime_error("Range error: step == 0");

        auto size  = (b - a0 + step) / step;

        size = (size > 0 ? size : 0);

        Tp * leak = nullptr;   // leak! a per-thread static should be a viable option...
        if (size) {
            leak = static_cast<Tp *>(realloc(leak, size * sizeof(Tp)));
            for(int n = 0; n < size && (step > 0  ? a0 <= b : a0 >= b); a0 += step, n++)
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
        std::cout << show(arg);
    }

    template <typename T, typename ...Ts>
    void P(T &&arg, Ts&&... args)
    {
        std::cout << show(arg) << ' ';
        P(std::forward<Ts>(args)...);
    }

    ////////////////////////////////////////////////////////////// _(): build pairs and tuples

    template <typename T1, typename T2>
    auto _(T1 &&arg1, T2 &&arg2)
    -> decltype(std::make_pair(std::forward<T1>(arg1), std::forward<T2>(arg2)))
    {
        return std::make_pair(std::forward<T1>(arg1), std::forward<T2>(arg2));
    }

    template <typename ... Ts>
    auto _(Ts&& ...args)
    -> decltype(std::make_tuple(std::forward<Ts>(args)...))
    {
        return std::make_tuple(std::forward<Ts>(args)...);
    }

    ////////////////////////////////////////////////////////////// S(): stringfy arg with show()

    template <typename T>
    std::string
    S(const T & arg)
    {
        return ass::show (arg);
    }

#define SHOW(v) # v " = " + S(v)

    ////////////////////////////////////////////////////////////// T<>(): get a demangled type name

    template <typename Tp>
    std::string
    T()
    {
        return type_name<Tp>();
    }

    ////////////////////////////////////////////////////////////// X<Type>::make_default(), X<Type>::make_value(..) etc.

#if (__clang__) || ((__GNUC__ == 4) && (__GNUC_MINOR__ > 6))
    template <typename Tp>
    using X = ass::xray_ptr<Tp>;
#endif

    ////////////////////////////////////////////////////////////// type_info_<Type>(): dump info about the given type

    namespace vt100
    {
        namespace
        {
            const char * const BOLD  = "\E[1m";
            const char * const RESET = "\E[0m";
            const char * const RED   = "\E[31m";
            const char * const BLUE  = "\E[1;34m";
        }
    }

#define ASS_TRAIT_INFO(Tp,trait)   do { \
    if (std::trait<Tp>::value) \
        std::cout << vt100::BOLD << # trait << vt100::RESET << ": " << std::trait<Tp>::value << std::endl; \
    else \
        std::cout << # trait ": " << std::trait<Tp>::value << std::endl; \
    } while (0)

    template <typename Tp>
    void type_info_()
    {
        std::cout << "type name: " << type_name<Tp>() << std::boolalpha << std::endl;

        ASS_TRAIT_INFO(Tp, is_void            );

#if !((__GNUC__ == 4) && (__GNUC_MINOR__ <= 8))
        ASS_TRAIT_INFO(Tp, is_null_pointer    );
#endif

        ASS_TRAIT_INFO(Tp, is_integral        );
        ASS_TRAIT_INFO(Tp, is_floating_point  );
        ASS_TRAIT_INFO(Tp, is_array           );
        ASS_TRAIT_INFO(Tp, is_enum            );
        ASS_TRAIT_INFO(Tp, is_union           );
        ASS_TRAIT_INFO(Tp, is_class           );
        ASS_TRAIT_INFO(Tp, is_function        );
        ASS_TRAIT_INFO(Tp, is_pointer         );
        ASS_TRAIT_INFO(Tp, is_lvalue_reference);
        ASS_TRAIT_INFO(Tp, is_rvalue_reference);
        ASS_TRAIT_INFO(Tp, is_member_object_pointer);
        ASS_TRAIT_INFO(Tp, is_member_function_pointer);

        ASS_TRAIT_INFO(Tp, is_fundamental       );
        ASS_TRAIT_INFO(Tp, is_arithmetic        );
        ASS_TRAIT_INFO(Tp, is_scalar            );
        ASS_TRAIT_INFO(Tp, is_object            );
        ASS_TRAIT_INFO(Tp, is_compound          );
        ASS_TRAIT_INFO(Tp, is_reference         );
        ASS_TRAIT_INFO(Tp, is_member_pointer    );


        ASS_TRAIT_INFO(Tp, is_const           );
        ASS_TRAIT_INFO(Tp, is_volatile        );
        ASS_TRAIT_INFO(Tp, is_trivial         );
        ASS_TRAIT_INFO(Tp, is_standard_layout );
        ASS_TRAIT_INFO(Tp, is_pod             );
        ASS_TRAIT_INFO(Tp, is_literal_type    );
        ASS_TRAIT_INFO(Tp, is_empty           );
        ASS_TRAIT_INFO(Tp, is_polymorphic     );

#if __cplusplus >= 201300L
        ASS_TRAIT_INFO(Tp, is_final           );
#endif
        ASS_TRAIT_INFO(Tp, is_abstract        );
        ASS_TRAIT_INFO(Tp, is_signed          );
        ASS_TRAIT_INFO(Tp, is_unsigned        );

        ASS_TRAIT_INFO(Tp, is_constructible         );
        ASS_TRAIT_INFO(Tp, is_nothrow_constructible );

#if (__GNUC__ == 4) && (__GNUC_MINOR__ == 6)
        ASS_TRAIT_INFO(Tp, has_trivial_default_constructor);
        ASS_TRAIT_INFO(Tp, has_trivial_copy_constructor   );
        ASS_TRAIT_INFO(Tp, has_trivial_copy_assign        );
        ASS_TRAIT_INFO(Tp, has_trivial_destructor         );
        ASS_TRAIT_INFO(Tp, has_nothrow_default_constructor);
        ASS_TRAIT_INFO(Tp, has_nothrow_copy_constructor   );
        ASS_TRAIT_INFO(Tp, has_nothrow_copy_assign        );

#elif (__GNUC__ == 4) && (__GNUC_MINOR__ == 7)
        ASS_TRAIT_INFO(Tp, has_trivial_default_constructor);
        ASS_TRAIT_INFO(Tp, has_trivial_copy_constructor   );
        ASS_TRAIT_INFO(Tp, has_trivial_copy_assign        );
        ASS_TRAIT_INFO(Tp, has_trivial_destructor         );
#endif

#if (__clang__) && defined(_LIBCPP_VERSION)
        ASS_TRAIT_INFO(Tp, is_trivially_copyable              );
        ASS_TRAIT_INFO(Tp, is_trivially_constructible         );
        ASS_TRAIT_INFO(Tp, is_trivially_default_constructible );
        ASS_TRAIT_INFO(Tp, is_trivially_copy_constructible    );
        ASS_TRAIT_INFO(Tp, is_trivially_move_constructible    );
        ASS_TRAIT_INFO(Tp, is_trivially_copy_assignable       );
        ASS_TRAIT_INFO(Tp, is_trivially_move_assignable       );

#endif

#if (__clang__) || ((__GNUC__ == 4) && (__GNUC_MINOR__ > 6))
        ASS_TRAIT_INFO(Tp, is_default_constructible         );
        ASS_TRAIT_INFO(Tp, is_nothrow_default_constructible );
        ASS_TRAIT_INFO(Tp, is_copy_constructible            );
        ASS_TRAIT_INFO(Tp, is_nothrow_copy_constructible    );
        ASS_TRAIT_INFO(Tp, is_move_constructible            );
        ASS_TRAIT_INFO(Tp, is_nothrow_move_constructible    );
        ASS_TRAIT_INFO(Tp, is_copy_assignable               );
        ASS_TRAIT_INFO(Tp, is_nothrow_copy_assignable       );
        ASS_TRAIT_INFO(Tp, is_nothrow_move_assignable       );
        ASS_TRAIT_INFO(Tp, is_destructible                  );
        ASS_TRAIT_INFO(Tp, is_move_assignable               );
#endif

#if (__clang__) || ((__GNUC__ == 4) && (__GNUC_MINOR__ > 7))
        ASS_TRAIT_INFO(Tp, is_trivially_destructible        );
        ASS_TRAIT_INFO(Tp, is_nothrow_destructible          );
#endif

        ASS_TRAIT_INFO(Tp, has_virtual_destructor           );

        std::cout << "sizeof   : " << sizeof(Tp) << std::endl;
        std::cout << "alignment: " << std::alignment_of<Tp>::value << std::endl;
        std::cout << "default  : {" << show( ass::xray_ptr<Tp>::make_default() ) << '}' << std::endl;
        std::cout << "uniform  : {" << show( ass::xray_ptr<Tp>::make_uniform() ) << '}' << std::endl;
        std::cout << "value    : {" << show( ass::xray_ptr<Tp>::make_value() )   << '}' << std::endl;
    }

    #define ASS_DUMP_MACRO(m)   std::cout << "    " << #m << " -> " << m << std::endl

    void
    compiler_info_()
    {
    #if defined(__cplusplus)
       ASS_DUMP_MACRO(__cplusplus);
    #endif
    #if defined(__GNUC__)
       ASS_DUMP_MACRO(__GNUC__);
    #endif
    #if defined(__GNUC_MINOR__)
       ASS_DUMP_MACRO(__GNUC_MINOR__);
    #endif
    #if defined(__GNUC_PATCHLEVEL__)
       ASS_DUMP_MACRO(__GNUC_PATCHLEVEL__);
    #endif
    #if defined(__VERSION__)
       ASS_DUMP_MACRO(__VERSION__);
    #endif
    #if defined(__BYTE_ORDER__)
       ASS_DUMP_MACRO(__BYTE_ORDER__);
    #endif
    #if defined(__LP64__)
       ASS_DUMP_MACRO(__LP64__);
    #endif
    #if defined(__clang__)
       ASS_DUMP_MACRO(__clang__);
    #endif
    #if defined(__clang_major__)
       ASS_DUMP_MACRO(__clang_major__);
    #endif
    #if defined(__clang_minor__)
       ASS_DUMP_MACRO(__clang_minor__);
    #endif
    #if defined(__clang_patchlevel__)
       ASS_DUMP_MACRO(__clang_patchlevel__);
    #endif
    #if defined(__clang_version__)
       ASS_DUMP_MACRO(__clang_version__);
    #endif
    }

} // namespace ass_inline


namespace ass {

    ////////////////////////////////////////////////////////////// interactive: run a command line

    struct eval
    {
        template <typename Fun>
        eval(Fun fun)
        {
            cmdline<decltype(fun())>::run(fun);
        }

        template <typename T>
        struct cmdline
        {
            template <typename F>
            static void run(F f)
            { auto ret = f(); std::cout << std::boolalpha << ret << std::endl; }
        };

    };

    template <>
    struct eval::cmdline<void>
    {
        template <typename F>
        static void run(F f)
        { f(); std::cout << "(void)"; }
    };


} // namespace ass

using namespace std;
using namespace std::chrono;
#if __cplusplus >= 201300L
using namespace std::experimental;
#endif
using namespace std::placeholders;
using namespace ass;

#endif /* __ASS_HPP__ */
