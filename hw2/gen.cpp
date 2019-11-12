#include <iostream>
#include <set>
#include <unordered_map>
#include <random>

using namespace std;

using container = unordered_map<int, char>;




int genRand() {
    static random_device dev;
    static mt19937 rng(dev());
    static uniform_int_distribution<mt19937::result_type> dist(1, 1000);
    return dist(rng);
}

string genVar(container&);

string genExpr(container&);

string genAppl(container& names) {
    int i = genRand() % 10;
    int j = genRand() % 10;
    string s = "(";
    if (i <= 2) {
        s += genVar(names);
    } else {
        s += genExpr(names);
    }
    s += " ";
    if (j <= 2) {
        s += genVar(names);
    } else {
        s += genExpr(names);
    }
    return s + ")";
}

char genName() {
    return (char) ('a' + (genRand() % 26));
}

string genLambda(container& names) {
    string s = "(\\";
    char randomName = genName();
    int index = names.size();
    names[index] = randomName;
    s += randomName;
    s += ".";
    s += genExpr(names);
    names.erase(index);
    return s + ")";
}

string genVar(container& names) {
    int r = genRand() % 10;
    string emp;
    if (r < 7 && !names.empty()) {
        int i = genRand() % names.size();
        emp += names[i];
    } else {
        char c = genName();
        emp += c;
    }
    return emp;
}

string genExpr(container& names) {
    int i = genRand() % 10;
    string s;
    if (i < 4) {
        s = genAppl(names);
    } else if (i < 8) {
        s = genLambda(names);
    } else {
        s = genVar(names);
    }
    return s;
}

int main() {
    container vals;
    string s = genExpr(vals);
    cout << s << endl;
    return 0;
}