#include <iostream>
#include <fstream>
#include <vector>
#include <set>
#include <cmath>
#include <map>
#include <functional>
#include <cassert>
#include <Windows.h>
#include <iomanip>

using namespace std;

typedef long double ld;
typedef function<ld (const vector<ld >& )> multi_var_func;

template <typename T>
T sqr(T val) {
    return val * val;
}


const ld F = 1.61803398874989484;

long long iterations_count = 0;

bool collect_intervals = false;
vector<pair<ld, ld>> intervals;

vector<vector<ld>> points;

ostream& operator << (ostream& out, const vector<ld> &v) {
    for (auto val : v) {
        out << setprecision(10) << val << " ";
    }
    return out;
}

ld dichotomy_method(function<ld (ld)> f, ld lim_1, ld lim_2, ld eps) {
    assert(lim_1 <= lim_2);
    if (collect_intervals) {
        intervals.emplace_back(lim_1, lim_2);
    }
    while (abs(lim_2 - lim_1) > eps) {
        ld p1 = (lim_2 + lim_1) / 2 - eps / 4;
        ld p2 = (lim_2 + lim_1) / 2 + eps / 4;

        ld p1_val = f(p1);
        ld p2_val = f(p2);

        if (p1_val < p2_val) {
            lim_2 = p2;
        } else {
            lim_1 = p1;
        }
        iterations_count++;
        if (collect_intervals) {
            intervals.emplace_back(lim_1, lim_2);
        }
    }
    return (lim_1 + lim_2) / 2;
}

ld golden_ratio_method(function<ld (ld)> f, ld lim_1, ld lim_2, ld eps) {
    assert(lim_1 <= lim_2);

    if (collect_intervals) {
        intervals.emplace_back(lim_1, lim_2);
    }

    ld p1_val = INFINITE;
    ld p2_val = INFINITE;

    while (abs(lim_2 - lim_1) > eps) {
        ld p1 = lim_2 - (lim_2 - lim_1) / F;
        ld p2 = lim_1 + (lim_2 - lim_1) / F;

        if (p1_val == INFINITE) {
            p1_val = f(p1);
        }
        if (p2_val == INFINITE) {
            p2_val = f(p2);
        }

        if (p1_val < p2_val) {
            lim_2 = p2;
            p2_val = p1_val;
            p1_val = INFINITE;
        } else {
            lim_1 = p1;
            p1_val = p2_val;
            p2_val = INFINITE;
        }
        iterations_count++;
        if (collect_intervals) {
            intervals.emplace_back(lim_1, lim_2);
        }
    }
    return (lim_1 + lim_2) / 2;
}

ld fibonacci_method(function<ld (ld)> f, ld lim_1, ld lim_2, ld eps) {
    assert(lim_1 <= lim_2);

    if (collect_intervals) {
        intervals.emplace_back(lim_1, lim_2);
    }

    ld f1 = 0, f2 = 1;
    int n = 1;

    while (f2 <= (lim_2 - lim_1) / eps) {
        ld f3 = f1 + f2;
        f1 = f2;
        f2 = f3;
        n++;
    }

    ld p1 = lim_1 + (f2 - f1) / f2 * (lim_2 - lim_1);
    ld p2 = lim_1 +        f1 / f2 * (lim_2 - lim_1);

    ld p1_val = f(p1);
    ld p2_val = f(p2);

    for (int k = 1; k <= n - 2; k++) {
        ld f0 = f2 - f1;
        f2 = f1;
        f1 = f0;

        if (p1_val > p2_val) {
            lim_1 = p1;
            p1 = p2;
            p2 = lim_1 + f1 / f2 * (lim_2 - lim_1);
            p1_val = p2_val;
            p2_val = f(p2);
        } else {
            lim_2 = p2;
            p2 = p1;
            p1 = lim_1 + (f2 - f1) / f2 * (lim_2 - lim_1);
            p2_val = p1_val;
            p1_val = f(p1);
        }
        iterations_count++;
        if (collect_intervals) {
            intervals.emplace_back(lim_1, lim_2);
        }
    }

    p2 = p1 + eps / 4;
    p2_val = f(p2);

    if (p1_val < p2_val) {
        lim_2 = p2;
    } else {
        lim_1 = p1;
    }
    if (collect_intervals) {
        intervals.emplace_back(lim_1, lim_2);
    }

    return (lim_1 + lim_2) / 2;
}

vector<ld> simple_gradient_method(
    vector<ld> p,
    multi_var_func f,
    vector<multi_var_func> f_grad,
    function<ld ((function<ld (ld)> f, ld lim_1, ld lim_2, ld eps))> one_dimensional_search_method,
    ld eps) {
        points.push_back(p);
        int n_step = 10000;
        while (n_step > 0) {
            n_step--;
            vector<ld> dir;
            for (auto f_g : f_grad) {
                dir.push_back(-f_g(p));
            }

            auto one_dim_task = [&dir, &p, &f] (ld c) {
                vector<ld> p2;
                for (int i = 0; i < (int) dir.size(); i++) {
                    p2.push_back(p[i] + dir[i] * c);
                }
                ld val = f(p2);

//                cout << p2 << "   " << c << "    " << val << "\n";

                return val;
            };
            ld c = one_dimensional_search_method(one_dim_task, -10, 10, eps);

            vector<ld> p2;
            ld grad_norm = 0;
            for (int i = 0; i < (int) p.size(); i++) {
                p2.push_back(p[i] + dir[i] * c);
                grad_norm += abs(dir[i]);
            }
            swap(p, p2);

//            cout << "grad_norm : " << grad_norm << "\n";

            if (grad_norm < eps) {
                cout << 10000 - n_step << "\n";
                return p;
            }
            {
                ld p_norm = 0;
                for (ld val : p) {
                    p_norm = max(p_norm, abs(val));
                }
                if (p_norm > 100000) {
                    cout << 10000 - n_step << "\n";
                    return p;
                }
            }
            cout << p << "  " << f(p) << "\n";
            points.push_back(p);
        }
    cout << 10000 - n_step << "\n";
    return p;
}

long long calc_counter = 0;

void test_1(function<ld (ld)> f, string function_name, ld lim_1, ld lim_2) {
    vector<pair<function<ld ((function<ld (ld)> f, ld lim_1, ld lim_2, ld eps))>, string>> one_dimention_search_methods = {
        {dichotomy_method, "dichotomy_method"},
        {golden_ratio_method, "golden_ratio_method"},
        {fibonacci_method, "fibonacci_method"}
    };

    collect_intervals = true;

    { // Зависимость a и b от шага
        ld eps = 1e-9;

        for (auto p : one_dimention_search_methods) {
            intervals.clear();
            p.first(f, lim_1, lim_2, eps);

            std::ostringstream sstream;
            sstream << setprecision(11) << eps;

            string filename = "task_1/" + p.second + " " + function_name + " " + sstream.str() + ".txt";

            ofstream out(filename);
            for (int i = 0; (int) i < intervals.size(); i++) {
                out << setprecision(11) << i << " " << intervals[i].first << " " << intervals[i].second << "\n";
            }
        }
    }
    { // Зависимость (b - a) от шага
        ld eps = 1e-9;

        for (auto p : one_dimention_search_methods) {
            intervals.clear();
            p.first(f, lim_1, lim_2, eps);

            std::ostringstream sstream;
            sstream << setprecision(11) << eps;

            string filename = "task_1/interval_length " + p.second + " " + function_name + " " + sstream.str() + ".txt";

            ofstream out(filename);
            for (int i = 0; i < intervals.size(); i++) {
                out << setprecision(11) << i << " " << intervals[i].second - intervals[i].first << "\n";
            }
        }
    }
    { // Количество шагов от eps по каждому методу.
        string filename = "task_1/n_steps_by_eps " + one_dimention_search_methods[0].second
                    + " " + one_dimention_search_methods[1].second
                    + " " + one_dimention_search_methods[2].second
                    + " " + function_name + ".txt";
        ofstream out(filename);

        for (ld eps = (lim_2 - lim_1) / 4; eps >= 1e-9; eps /= 2) {
            out << setprecision(11) << eps << " ";
            for (auto p : one_dimention_search_methods) {
                intervals.clear();
                p.first(f, lim_1, lim_2, eps);
                out << intervals.size() - 1 << " ";
            }
            out << "\n";
        }
    }
    { // Количество вычислений функции от eps по каждому методу.
        string filename = "task_1/n_evaluations_by_eps " + one_dimention_search_methods[0].second
                    + " " + one_dimention_search_methods[1].second
                    + " " + one_dimention_search_methods[2].second
                    + " " + function_name + ".txt";
        ofstream out(filename);

        for (ld eps = (lim_2 - lim_1) / 4; eps >= 1e-9; eps /= 2) {
            out << setprecision(11) << eps << " ";
            for (auto p : one_dimention_search_methods) {
                intervals.clear();
                calc_counter = 0;
                p.first(f, lim_1, lim_2, eps);
                out << calc_counter << " ";
            }
            out << "\n";
        }
    }

    collect_intervals = false;
}

void test_2(multi_var_func f,
            vector<multi_var_func> f_grad,
            string function_name,
            ld lim_1,
            ld lim_2) {

    vector<vector<ld>> start_points = {{1, 1}, {1, 0}, {0.2, 1}, {-1, 1}};

    for (int i = 0; i < start_points.size(); i++) {
        points.clear();

        simple_gradient_method(start_points[i], f, f_grad, golden_ratio_method, 1e-9);

        ofstream out("task_2/" + function_name + " " + to_string(i) + ".txt");
        for (auto p : points) {
            for (auto c : p) {
                out << setprecision(10) << c << " ";
            }
            out << "\n";
        }
    }
}

int main() {
    #ifdef Vlad_kv
        freopen("input.txt", "r", stdin);
//        freopen("output.txt", "w", stdout);
    #endif // Vlad_kv

    {
        function<ld (ld)> f = [&calc_counter] (ld val) {
            calc_counter++;
            return val * val + 3 * val + 1;
        };
        test_1(f, "xx+3x+1", -100, 100);
    }

    {
        multi_var_func f = [] (const vector<ld>& coordinates) {
            return sqr(coordinates[0]) + sqr(coordinates[1]);
        };
        vector<multi_var_func> f_grad = {
            [] (const vector<ld>& coordinates) {
                return 2 * coordinates[0];
            },[] (const vector<ld>& coordinates) {
                return 2 * coordinates[1];
            }};
        test_2(f, f_grad, "x^2 + y^2", -100, 100);
    }

    {
        multi_var_func f = [] (const vector<ld>& coordinates) {
            return sqr(coordinates[0]) + 10 * sqr(coordinates[1]);
        };
        vector<multi_var_func> f_grad = {
            [] (const vector<ld>& coordinates) {
                return 2 * coordinates[0];
            },[] (const vector<ld>& coordinates) {
                return 20 * coordinates[1];
            }};
        test_2(f, f_grad, "x^2 + 10y^2", -100, 100);
    }
    {
        multi_var_func f = [] (const vector<ld>& coordinates) {
            return sqr(coordinates[0]) + 10 * sqr(coordinates[1])
                   + coordinates[0] * coordinates[1];
        };
        vector<multi_var_func> f_grad = {
            [] (const vector<ld>& coordinates) {
                return 2 * coordinates[0] + coordinates[1];
            },[] (const vector<ld>& coordinates) {
                return 20 * coordinates[1] + coordinates[0];
            }};
        test_2(f, f_grad, "x^2 + 10y^2 + xy", -100, 100);
    }


    return 0;
}
