#include <bits/stdc++.h>
using namespace std;
#define DB(v) cerr << endl <<  #v << ' ' << v << endl;
#define sz(v) int(v.size())
#define For(i, a, b) for(int i = a; i <= b; ++i)

unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
  std::default_random_engine generator (seed);

int n, K, B; double selection, delta, v, mutation_proba, sigma, migrate_proba;
int generation = 1;
double eps = 10e-6;

map <double, vector <int> > trace_individum;

struct individual {
    double type;
    individual(double ty = 0) {
        type = ty;
    }
    bool operator < (const individual & other) const{
        /*if(abs(type - other.type) < eps)
            return true;*/
        return (type < other.type);
    }
    bool operator == (const individual & other) {
        return abs(type - other.type) < eps;
    }
};

double theta(int number_island) {
    return delta * number_island + v * generation;
}

double survive (individual ind, int number_island) {
    double diff = ind.type - theta(number_island);
    double cur = -selection * diff * diff;
    return exp(cur);
}

void add (map <individual, int> & map_add, individual add_ind, int how_many) {
    map_add[add_ind] += how_many;
    return;
}

struct island {
    map <individual, int> inhabitants;
    int number_island;
    island(map <individual, int> inh = {}, int numb = 0) {
        inhabitants = inh;
        number_island = numb;
    }

    void add_island(individual ind, int how_many) {
        add(inhabitants, ind, how_many);
    }

    void fill_island() {
        inhabitants[individual(0.0)] = K;
    }
    int number_of_inhabitants() {
        return sz(inhabitants);
    }
    void print(){
        cout << endl << "Island number " << number_island << endl;
        for(auto to: inhabitants) {
            cout << "(" << to.first.type /*type of indiv*/ << ", " << to.second /*number*/ << ") ";
        }
        cout << endl;
    }
     void print_debug(){
        cerr << endl << "Island number " << number_island << endl;
        for(auto to: inhabitants) {
            cerr << "(" << to.first.type /*type of indiv*/ << ", " << to.second /*number*/ <<") ";
        }
        cerr << endl;
    }

    double calc_medium() {

    }
};
vector <island> islands;
vector <island> migrated;

void add_offspring(individual add_individual, map <individual, int> & inhabitants) {
    poisson_distribution <int> pd(B);
    normal_distribution <double> nd(0, sigma * sigma);
    int number = pd(generator);
    for(int i = 0;i < number; ++i) {
        individual offspring = add_individual;
        double trial_mutation = ((double) rand() / (RAND_MAX));
        if(trial_mutation <= mutation_proba) {
            double mutation = nd(generator);
            offspring.type += mutation;
        }
        add(inhabitants, offspring, 1);
    }
    return;
}

void reproduce (int number_island) { //current round of reproduction
    map <individual, int> new_inhabitants;
    for(auto to: islands[number_island].inhabitants) {
        individual ind = to.first;
        for(int j = 0;j < to.second; ++j) {
            double trial = ((double) rand() / (RAND_MAX));
            if(trial < survive(ind, number_island)) {
                add_offspring(ind, new_inhabitants);
            }
        }
    }
    islands[number_island].inhabitants = new_inhabitants; //new generation replaces the old one
}

void migrate (int number_island) {
    map <individual, int> staying_inhabitants;
    for(auto to: islands[number_island].inhabitants) {
        individual ind = to.first;
        for(int j = 0;j < to.second; ++j) {
            double trial = ((double) rand() / (RAND_MAX)); // decides if the current individual migrates
            if(trial < migrate_proba && n > 1) { // if does
                if(number_island == 0)
                    migrated[number_island + 1].add_island(ind, 1);
                else if(number_island == n - 1)
                    migrated[number_island - 1].add_island(ind, 1);
                else {
                    double where_trial = ((double) rand() / (RAND_MAX));
                    if(where_trial <= 0.5) {
                        migrated[number_island - 1].add_island(ind, 1);
                    }
                    else {
                        migrated[number_island + 1].add_island(ind, 1);
                    }
                }
            }
            else { //if stays
                add(staying_inhabitants, ind, 1);
            }
        }
    }
    islands[number_island].inhabitants = staying_inhabitants;
    return;
}

void migrate() {
    migrated.resize(n);
    for(int i = 0;i < n; ++i)
        migrated[i] = island({}, i);
    for(int i = 0;i < n; ++i)
        migrate(i);
    for(int i = 0;i < n; ++i)
        for(auto to: migrated[i].inhabitants) {
            islands[i].add_island(to.first, to.second);
        }
    return;
}

void pick_randomly(int number_island) {
    vector <individual> result = {};
    for(auto to: islands[number_island].inhabitants) {
        individual ind = to.first;
        for(int i = 0;i < to.second; ++i)
            result.push_back(ind);
    }
    random_shuffle(result.begin(), result.end());

    islands[number_island].inhabitants.clear();
    for(int i = 0;i < min(K, sz(result)); ++i)
        islands[number_island].add_island(result[i], 1);
    return;
}

void print_islands();

void proceed () {
    for(int i = 0;i < sz(islands); ++i) {
        reproduce(i);
    }
    migrate();

    for(int i = 0;i < sz(islands); ++i) {
        pick_randomly(i);
    }
    generation++;
    return;
}

void initialize_islands() {
    islands.resize(n);
    for(int i = 0;i < n; ++i) {
        islands[i] = island({}, i);
    }
    islands[0].fill_island();
    return;
}

void print_islands() {
    cout << endl << "Generation number " << ' ' << generation << endl;
    for(int i = 0;i < n; ++i)
        islands[i].print();

    return;
}

void print_table() {
    trace_individum.clear();
    for(int i = 0;i < n; ++i) {
        for(auto to: islands[i].inhabitants) {
            individual ind = to.first;
            if(trace_individum[ind.type].empty())
                trace_individum[ind.type].assign(n, 0);
            trace_individum[ind.type][i] += to.second;
        }
    }

    cout << "printing table for generation " << generation << endl;
    for(auto to: trace_individum) {
        double cur_type = to.first;
        cout << fixed << setprecision(4) << cur_type << "    ";
        for(int i = 0;i < n; ++i) {
            cout << trace_individum[cur_type][i] << ' ';
        }
        cout << endl;
    }
}

void solve() {
    int N = 15000; //number of generations to model
    for(int i = 0;i < N; ++i) {
        proceed();
        if(i % 100 == 0) { //we print all_islands every 100 generations
            DB(generation);
            //print_islands();
            print_table();
        }
    }
}

void debug() {
    int N = 50;
    for(int i = 0;i < N; ++i) {
        proceed();
        DB(generation);
        print_islands();
    }
}

int main()
{
    freopen("input.txt", "r", stdin);
    freopen("output.txt", "w", stdout);

    string text;
    cin >> n >> text >> K >> text >> B >> text;
    cin >> selection >> text >> mutation_proba >> text >> migrate_proba >> text;
    cin >> delta >> text >> v >> text >> sigma >> text;

    initialize_islands();
    //debug();
    solve();
    return 0;
}
