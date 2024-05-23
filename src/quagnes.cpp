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
//  decks/dN to decks/oN and make first file deck1.txt instead of deck0.txt.
//  (2) Rename file to quagnes.cpp. (3) Change -a parameter to -k parameter
//  and do not output skipped simulations.
// [20240413] - Fix print_usage function. '-h' option was giving an error rather
// than printing help message, lengthen other messages, and remove one or two
// incorrect options from the message.
// [20240426] - (1) Remove '-r' option from print_usage(), as this was never
//  implemented. (2) Add '-y' option to print variables useful for estimating
//  memory utilization. (3) Standardidze leading white-space to 2 from 4.
// [20240522] (1) Add `-r` and `-b` options for random number generator (RNG)
// [20240523] Bug fix to previous. When rep<n_to_skip, create the `Agnes`
//  object and run it with one state so that the RNG is advanced the same way
//  as if the initial reps weren't skipped. (But the output records for these
//  skipped reps are still not printed.)
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
  std::cerr << "Usage: ./quagnes -n reps [-k n_to_skip] [-e empty] [-u] [-s]";
  std::cerr << "[-t threshold] [-p] [-f] [-z] [-m n_states] [-r random_seed] [-b burn_in] [-h]\n";
  std::cerr << "    -n reps: execute the first reps simulations (except those skipped by the -k parameter)\n";
  std::cerr << "    -k n_to_skip (int): skip first n_to_skip simulations\n";
  std::cerr << "    -e empty: define what can be moved to empty tableau piles: {'none', 'high run', 'any run', 'high 1', 'any 1'}\n";
  std::cerr << "    -u: move runs by suit instead of by color\n";
  std::cerr << "    -s: do not allow movable runs to be split\n";
  std::cerr << "    -t threshold: track losing states in a set until the number of cards in the stock is less than threshold (default=0)\n";
  std::cerr << "    -p: print state as each move is performed or undone\n";
  std::cerr << "    -f: deal all tableau cards face-up instead of just the last card in each column\n";
  std::cerr << "    -z: maximize score by disabling identification of losing games without playing moves (used when '-e none' is specified)\n";
  std::cerr << "    -m n_states: stop processing game after n_states states\n";
  std::cerr << "    -r random_seed: single seed to pass to std::mt19937 RNG for shuffling deck (default=0 [read shuffled deck from file])\n";
  std::cerr << "    -b burn_in: burn-in period for std::mt19937 RNG (default=0)\n";
  std::cerr << "    -y: print information useful for estimating memory utilization to standard output\n";
  std::cerr << "    -h: print this help message\n";
}

int main(int argc, char *argv[]) {
  int n_reps = 0;
  char deck_filename_cp[DECK_FILENAME_LEN];
  string rerun_filename;
  quagnes::AgnesOptions agnes_options;
  int n_to_skip = 0;
  quagnes::StatesType input_max_states = 0;

  while (1) {
    switch(getopt(argc, argv, "n:d:e:usft:pm:b:r:k:yz")) {
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

    case 'b':
      agnes_options.burn_in = std::stoull(string(optarg));
      continue;

    case 'r':
      agnes_options.random_seed = std::stoul(string(optarg));
      continue;

    case 'h':
      print_usage();
      return 1;
      continue;

    case 'p':
      agnes_options.print_states = true;
      continue;

    case 'm':
      input_max_states = std::stoull(string(optarg));
      continue;

    case 'y':
      agnes_options.print_memory = true;
      continue;

    case 'z':
      agnes_options.maximize_score = true;
      continue;

    case 't':
      agnes_options.track_threshold = atoi(optarg);
      continue;

    case '?':
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
  cout << "current_depth";

  if (agnes_options.print_memory) {
    cout << ",n_losing_states";
    cout << ",cum_length";
    for (int i=0; i<quagnes::kNSymbol; i++) {
      cout << ",csc0_" << i;
    }
    for (int i=0; i<quagnes::kNSymbol; i++) {
      cout << ",csc1_" << i;
    }
  }

  cout << std::endl;

  for (int rep=0; rep<n_reps; ++rep) {
    // Even if it is a rep that we want to skip, we run at least one
    // state because if deck is shuffled by a RNG, we want to advance
    // the RNG in the same way as if no skip was done.
    if (rep < n_to_skip) {
        if (agnes_options.random_seed) agnes_options.max_states = 1;
        else continue;
    }
    else agnes_options.max_states = input_max_states;

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
      cout << new_game.current_depth();
      if (agnes_options.print_memory) {
        cout << "," << new_game.n_losing_states();
        cout << "," << new_game.cum_length();
        for (uint64_t val : new_game.cum_symbol_count0()) {
          cout << "," << val;
        }
        for (uint64_t val : new_game.cum_symbol_count1()) {
          cout << "," << val;
        }
      }
      cout << std::endl;
    }
  }
  return 0;
}
