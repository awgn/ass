/*
 *  Copyright (c) 2011 Bonelli Nicola <bonelli@antifork.org>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *
 */

#include <ass.hpp>

#define RUNME   "/tmp/runme"

struct line
{ 
    std::string str;
};

template <typename CharT, typename Traits>
typename std::basic_istream<CharT, Traits> &
operator>>(std::basic_istream<CharT,Traits> &in, line& other)
{
    std::getline(in, other.str);
    other.str.append(1, '\n');
    return in;
}
    
int
main(int argc, char *argv[])
{
    std::vector<std::pair<std::string,int>> translation_unit = { make_pair("#include <ass.hpp>\n",0) };
    std::vector<std::pair<std::string,int>> main_ = { make_pair("int main(int argc, char *argv[]) { cout << boolalpha;\n",0) };

    bool state = true;

    // create the source code...
    //
    int n = 1;
    std::for_each(std::istream_iterator<line>(std::cin),
                  std::istream_iterator<line>(),
                  [&](const line &l) {

        if (l.str.find_first_of("...") == 0) {
            state = !state; n++;
            return;
        }        

        bool pp = false;
        for(auto c : l.str)
        {
            if (isspace(c))
                continue;
            if (c == '#')
                pp = true;
            break;    
        }

        auto & where = (state && !pp ? main_ : translation_unit);
        where.push_back( make_pair(std::move(l.str),n++) );
    });

    main_.push_back(make_pair("}",0));
    std::move(main_.begin(), main_.end(), std::back_inserter(translation_unit));

    std::ofstream cpp(RUNME ".cpp");
    if (!cpp)
        throw std::runtime_error("ass: ofstream");
    
    std::for_each(translation_unit.begin(), translation_unit.end(), [&](const std::pair<std::string, int> &l) {
                        cpp << "#line " << l.second << '\n' << l.first;
                  });
    cpp.close();

    // compile the test...
    //

    char path[PATH_MAX];
    if (!getcwd(path, PATH_MAX))
        throw std::runtime_error("ass: getcwd");

    std::string compile("/usr/bin/g++ -std=c++0x -Wall -Wextra -Wno-unused-parameter "
                    "-D_GLIBXX_DEBUG " RUNME ".cpp -I/usr/local/include/ -o " RUNME);

    compile.append(" -I").append(path);

    auto argx = argv + 1;
    for(; argx != (argv+argc); ++argx)
    {
        if (!std::string("--").compare(*argx))
        {
            argx++; break;
        }
        compile.append(1, ' ').append(*argx);
    }

    int status = system(compile.c_str());

    if( WEXITSTATUS(status))
        return EXIT_FAILURE;
    
    // run it...
    //

    std::cout << "---\n";
    std::ostringstream runme; runme << RUNME << ' ';
    std::copy(argx, argv + argc, std::ostream_iterator<const char *>(runme, " "));

    int rc = system(runme.str().c_str());
    return WIFEXITED(rc) ? WEXITSTATUS(rc) : EXIT_FAILURE;
}
 
