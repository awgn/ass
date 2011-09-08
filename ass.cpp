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

#include <ass>

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
    std::vector<std::string> translation_unit = { "#include <ass>\n" };
    std::vector<std::string> body = { "int main(int argc, char *argv[]) { cout << boolalpha;\n" };

    bool state = true;

    std::for_each(std::istream_iterator<line>(std::cin),
                  std::istream_iterator<line>(),
                  [&](const line &l) {

        if (l.str.find_first_of("...") == 0) {
            state = !state;
            return;
        }        
        auto & where = (state ? body : translation_unit);
        where.push_back(std::move(l.str));
    });

    body.push_back("}");
    std::move(body.begin(), body.end(), std::back_inserter(translation_unit));

    std::ofstream cpp("/tmp/runme.cpp");
    std::copy(translation_unit.begin(), translation_unit.end(), std::ostream_iterator<std::string>(cpp));
    cpp.close();

    int status = system("g++ -std=c++0x -O0 -g2 -Wall -Weffc++ -Wextra -Wno-unused-parameter -pthread "
                        "-D_GLIBCXX_DEBUG /tmp/runme.cpp -o /tmp/runme");
    if( WEXITSTATUS(status) == 0)
    {
        std::cout << "running..." << std::endl;
        std::ostringstream runme; runme << "/tmp/runme ";
        std::copy(argv + 1, argv + argc, std::ostream_iterator<const char *>(runme, " "));
        return system(runme.str().c_str());
    }
    else {
        std::cout << "...aborted!" << std::endl;
        return 1; 
    }
}
 
