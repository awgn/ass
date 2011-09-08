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
    std::vector<std::string> body = { "int main(int argc, char *argv[]) {\n" };

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

    std::ofstream cpp("/tmp/ass.cpp");
    std::copy(translation_unit.begin(), translation_unit.end(), std::ostream_iterator<std::string>(cpp));
    cpp.close();

    int status = system("g++ -std=c++0x -O0 -g2 -Wall -Weffc++ -Wextra -Wno-unused-parameter -pthread "
                        "-D_GLIBCXX_DEBUG /tmp/ass.cpp -o /tmp/runme");
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
 
