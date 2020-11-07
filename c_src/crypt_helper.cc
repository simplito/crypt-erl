#include <crypt.h>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

bool getargs(vector<string>& args, size_t n) {
    args.resize(n);
    for(size_t i = 0; i < n; ++i) {
        if (!getline(cin, args[i]))
            return false;
    }
    return true;
}

int main(int argc, char* argv[]) {
    string command;
    vector<string> args;

    while(getline(cin, command)) {
        if (command == "gensalt") {
            if (!getargs(args, 2))
                return 1;
            int rounds = atoi(args[1].c_str());
            char* result = crypt_gensalt(args[0].c_str(), rounds, NULL, 0);
            cout << (result ? result : "") << endl;
        } else if (command == "crypt") {
            if (!getargs(args, 2))
                return 1;
            char* result = crypt(args[0].c_str(), args[1].c_str());
            cout << (result ? result : "") << endl;
        } else {
            return 2;
        }
    }
    return 0;
}
