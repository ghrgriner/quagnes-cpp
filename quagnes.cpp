//  A program for solving Agnes solitaire
//   Copyright (C) 2019, 2024 Ray Griner (rgriner_fwd@outlook.com)
//
//   This program is free software: you can redistribute it and/or modify
//   it under the terms of the GNU General Public License as published by
//   the Free Software Foundation, either version 3 of the License, or
//   (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//   GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program.  If not, see <https://www.gnu.org/licenses/>.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// File: quagnes.cpp
// Date: 2019
// Author: Ray Griner
// Purpose: main entry point, parse command line args and create and play Agnes
//   object
// Changes: (changes before 20240405 not logged)
// [20240405] - (1) Add rep_id to output file. Change input directory from
//    decks/dN to decks/oN and make first file deck1.txt instead of deck0.txt.
//    (2) Rename file to quagnes.cpp. (3) Change -a parameter to -k parameter
//    and do not output skipped simulations.
//------------------------------------------------------------------------------

#include <unistd.h>

#include <string>
#include <iostream>
#include <cmath>

#include "Agnes.h"

#define DECK_FILENAME_LEN 255

using std::cout;
using std::string;

void print_usage();

void print_usage() {
    std::cerr << "Usage: ./quagnes -n reps [-e empty] [-u] [-s] [-i] [-z] ";
    std::cerr << "[-t int] [-p] [-f] [-m int] [-k int]\n";
    std::cerr << "    -n reps (int): number of simulations\n";
    std::cerr << "    -e empty: pass to move_to_empty_pile parameter\n";
    std::cerr << "    -u: set move_same_suit=true\n";
    std::cerr << "    -s: set split_runs=false\n";
    std::cerr << "    -t int: set track_threshold parameter\n";
    std::cerr << "    -p: set print_states=true\n";
    std::cerr << "    -f: set face_up=true\n";
    std::cerr << "    -z: set maximize_score=true\n";
    std::cerr << "    -i: set print_states=true\n";
    std::cerr << "    -k n_to_skip (int): skip first n_to_skip simulations\n";
    std::cerr << "    -m int: set max_states parameter\n";
}

int main(int argc, char *argv[]) {
    int n_reps = 0;
    char deck_filename_cp[DECK_FILENAME_LEN];
    quagnes::AgnesOptions agnes_options;
    int n_to_skip = 0;

    while (1) {
        switch(getopt(argc, argv, "n:d:e:usft:pm:k:z")) {
        case 'n':
            n_reps = atoi(optarg);
            continue;

        case 'k':
            n_to_skip = atoi(optarg);
            continue;

        case 'e':
            agnes_options.move_to_empty_pile = string(optarg);
            continue;

        case 'u':
            agnes_options.move_same_suit = true;
            continue;

        case 's':
            agnes_options.split_runs = false;
            continue;

        case 'f':
            agnes_options.face_up = true;
            continue;

        case 'p':
            agnes_options.print_states = true;
            continue;

        case 'm':
            //max_states = atoi(optarg);
            agnes_options.max_states = std::stoull(string(optarg));
            continue;

        case 'z':
            agnes_options.maximize_score = true;
            continue;

        case 't':
            agnes_options.track_threshold = atoi(optarg);
            continue;

        case '?':
        case 'h':
        default :
            print_usage();
            return 1;
            break;

        case -1:
            break;
        }

        break;
    }
    if (optind != argc) {
        print_usage();
        return 1;
    }

    cout << "rep_id,rc,n_states_checked,n_deal,n_move_to_foundation,";
    cout << "n_move_card_in_tableau,n_no_move_possible,max_score,max_depth,";
    cout << "current_depth" << std::endl;

    for (int rep=n_to_skip; rep<n_reps; ++rep) {
        //cout << "Rep: " << rep << endl;
        snprintf(deck_filename_cp, DECK_FILENAME_LEN-1,
             "decks/o%d/deck%d.txt", static_cast<int>(rep/1000), rep+1);
        agnes_options.deck_filename=string(deck_filename_cp);

        quagnes::Agnes new_game(agnes_options);

        int rc = new_game.Play();
        if (rep >= n_to_skip) {
            cout << (rep+1) << ",";
            cout << rc << ",";
            cout << new_game.n_states_checked() << ",";
            cout << new_game.n_deal() << ",";
            cout << new_game.n_move_to_foundation() << ",";
            cout << new_game.n_move_card_in_tableau() << ",";
            cout << new_game.n_no_move_possible() << ",";
            cout << new_game.max_score() << ",";
            cout << new_game.max_depth() << ",";
            cout << new_game.current_depth() << std::endl;
        }
    }
  return 0;
}
