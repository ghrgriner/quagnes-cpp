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
// File: Agnes.cpp
// Date: 2019
// Author: Ray Griner
// Purpose: Agnes class and related functions
// Changes:
// 20240411RG: (1) Add call to new SetLastCards function after initializing deck
// (2) Pass enum_to_empty_pile parameter to all functions that do or undo moves
// as it is needed by AgnesState.set_sort_order. (3) Add assert to check that
// a string is in the check_loops set before we erase it.
// [20240114RG]: New `Agnes.max_states_guard_` attribute to guard against
// overflow. Is set to kNStatesMax when max_states_=0.
// (1) Change C-style define constants to const variables. (2) Mark other
// functions as const as appropriate.
// [20230420] (1a) Change hash_ attribute to compstr_ and UpdateHash to
//   UpdateCompStr since there is no information loss when we compress.
//   (1b) pass face_up parameter to UpdateCompStr so that we don't waste memory
//   on the pile-markers of the hidden piles when we know they are empty.
//   (2) Add enum_to_empty_pile parameter to UpdateCompStr
//------------------------------------------------------------------------------

#include <iostream>
#include <fstream>
#include <random>
#include <cassert>
#include <algorithm>

#include "Agnes.h"

using std::cout;
using std::cerr;
using std::set;
using std::stack;
using std::array;
using std::string;

namespace quagnes {

//------------------------------------------------------------------------------
// PUBLIC FUNCTIONS
//------------------------------------------------------------------------------
Agnes::Agnes (const AgnesOptions &agnes_options)
    : deck_filename_(agnes_options.deck_filename),
      move_to_empty_pile_(agnes_options.move_to_empty_pile),
      move_same_suit_(agnes_options.move_same_suit),
      split_runs_(agnes_options.split_runs),
      track_threshold_(agnes_options.track_threshold),
      face_up_(agnes_options.face_up),
      maximize_score_(agnes_options.maximize_score),
      print_states_(agnes_options.print_states),
      max_states_(agnes_options.max_states),
      n_states_checked_(1) ,
      n_deal_(1),
      n_move_card_in_tableau_(0),
      n_move_to_foundation_(0),
      n_no_move_possible_(0),
      max_depth_(0),
      curr_state_(),
      max_score_(1),
      score_(1),
      current_depth_(0),
      initial_deck_(),
      deck_(),
      check_loops_(),
      losing_states_(),
      // under some combinations of rules, it's not possible to infinitely
      // loop in the tableau, so we don't need to check for loops
      check_for_loops_(agnes_options.split_runs
                       || (enum_to_empty_pile_!= EmptyRule::None)),
      moves_left_()
{
  if (print_states_) {
    cout << "Start Agnes()\n";
  }

  // Create enum based on move_to_empty_pile
  // Also, create `split_empty_stock` variable that indicates whether same-suit
  // runs should be split even when the stock is empty.
  if (move_to_empty_pile_ == "none") {
    enum_to_empty_pile_ = EmptyRule::None;
    split_empty_stock_ = false;
  }
  else if (move_to_empty_pile_ == "any 1") {
    enum_to_empty_pile_ = EmptyRule::Any1;
    split_empty_stock_ = true;
  }
  else if (move_to_empty_pile_ == "any run") {
    enum_to_empty_pile_ = EmptyRule::AnyRun;
    split_empty_stock_ = false;
  }
  else if (move_to_empty_pile_ == "high 1") {
    enum_to_empty_pile_ = EmptyRule::High1;
    split_empty_stock_ = true;
  }
  else if (move_to_empty_pile_ == "high run") {
    enum_to_empty_pile_ = EmptyRule::HighRun;
    split_empty_stock_ = false;
  } else {
      throw std::invalid_argument("move_to_empty_pile must be 'none', 'any 1', 'any run', 'high 1', 'high run'");
  }

  InitializeDeckFromFile(deck_filename_);
  if (!max_states_) max_states_guard_ = kNStatesMax;
  else max_states_guard_ = max_states_;
}

//void Agnes::initialize_deck(string seed)
//{
//  for (int rank=0; rank<kNRank; ++rank) {
//    for (int suit=0; suit<kNSuit; ++suit) {
//      initial_deck_[rank*4+suit].rank = rank;
//      initial_deck_[rank*4+suit].suit = suit;
//    }
//  }
//  std::seed_seq(str.begin(), str.end())
//  std::mt19937 g(12345);
//  std::shuffle(initial_deck_.begin(), initial_deck_.end(), g);
//  //for(auto it=initial_deck_.begin();
//  //     it != initial_deck_.end(); ++it) {
//  //    if (it != initial_deck_.begin()) { cout << ", "; }
//  //    cout << "(" << it->rank << ", " << it->suit << ")";
//  //}
//  //std::cout << '\n';
//}

void Agnes::InitializeDeckFromFile(const string &deck_filename) {
  std::string line;
  std::ifstream infile(deck_filename);
  if (infile) {
    int line_ctr = 0;
    while (std::getline(infile, line))
    {
      initial_deck_[line_ctr].rank = std::stoi(line.substr(1,2), NULL);
      initial_deck_[line_ctr].suit = std::stoi(line.substr(5,1), NULL);
      ++line_ctr;
    }
  }
  else {
    throw FileNotFoundError(deck_filename);
  }
}

int Agnes::Play() {
  int i, j;
  LastMoveInfo empty_lmi;
  array<LastMoveInfo, kNPile> last_move_info;

  // reset the indexing of all cards so base card has rank 0
  for (i=0; i<kNCard; ++i) {
    deck_[i].rank = ((initial_deck_[i].rank - initial_deck_[0].rank)
             % kNRank);
    if (deck_[i].rank < 0) deck_[i].rank += kNRank;
    deck_[i].suit=initial_deck_[i].suit;
  }
  curr_state_.SetLastCards(deck_);

  curr_state_.PlayBaseCard(deck_[0]);
  score_ = 1;
  for (i=0; i<kNPile; ++i) {
    for (j=i; j<kNPile; ++j) {
      curr_state_.DealOntoPile(j, deck_, (face_up_ or j==i));
    }
  }

  const SetNMovableOpts snm_opts (move_same_suit_, split_runs_,
                  split_empty_stock_);

  for (i=0; i<kNPile; ++i) {
    curr_state_.set_n_movable(i, snm_opts);
  }

  all_lmi_.push(last_move_info);
  curr_state_.set_valid_moves(enum_to_empty_pile_, move_same_suit_,
                              split_empty_stock_, track_threshold_,
                              all_lmi_.top());
  all_valid_moves_.push(curr_state_.valid_moves());
  //curr_state_.set_sort_order(enum_to_empty_pile_);
  int done = 0;
  if (print_states_) {
    curr_state_.Print();
    PrintLastMoveInfo();
  }
  if (enum_to_empty_pile_ == EmptyRule::None && !maximize_score_
    && curr_state_.IsAnyPileBlocked()) {
    return 2;
  }
  else {
    while (!done) {
      done = PerformMove();
      if (n_states_checked_ % 5000000 == 0) { SummarizeState(); }
      if (print_states_) {
        curr_state_.Print();
        PrintLastMoveInfo();
      }
    }
    current_depth_ = curr_state_.depth();
    return done;
  }
}

void Agnes::SummarizeState() const {
  std::cerr << "Processed " << n_states_checked_ << "|";
  std::cerr << all_valid_moves_.size();
  std::cerr << "|";
  for (const auto& elem  : moves_left_) {
    std::cerr << elem << "|";
  }
  std::cerr << std::endl;
}

//-------------------
// Accessor functions
//-------------------

StatesType Agnes::n_states_checked() const {
  return n_states_checked_;
}

StatesType Agnes::n_deal() const {
  return n_deal_;
}

StatesType Agnes::n_move_card_in_tableau() const {
  return n_move_card_in_tableau_;
}

StatesType Agnes::n_move_to_foundation() const {
  return n_move_to_foundation_;
}

StatesType Agnes::n_no_move_possible() const {
  return n_no_move_possible_;
}

int Agnes::max_depth() const {
  return max_depth_;
}

string Agnes::move_to_empty_pile() const {
  return move_to_empty_pile_;
}

bool Agnes::move_same_suit() const {
  return move_same_suit_;
}

bool Agnes::print_states() const {
  return print_states_;
}

bool Agnes::split_runs() const {
  return split_runs_;
}

bool Agnes::maximize_score() const {
  return maximize_score_;
}

bool Agnes::face_up() const {
  return face_up_;
}

int Agnes::track_threshold() const {
  return track_threshold_;
}

int Agnes::current_depth() const {
  return current_depth_;
}

int Agnes::max_score() const {
  return max_score_;
}

//------------------------------------------------------------------------------
// Private functions
//------------------------------------------------------------------------------
void Agnes::UndoMove(const bool &no_print) {
  if (print_states_ && !no_print) {
    cout << "Undo move" << "\n";
  }

  const Move &curr_move = moves_.top();
  const SetNMovableOpts snm_opts (move_same_suit_, split_runs_,
                                  split_empty_stock_);

  moves_.pop();
  all_valid_moves_.pop();
  all_lmi_.pop();

  if (check_for_loops_) {
    if (!curr_state_.is_loop()) {
      auto it = check_loops_.find(curr_state_.compstr());
      assert(it != check_loops_.end());
      check_loops_.erase(it);
    }
  }
  curr_state_.set_is_loop(false);
  curr_state_.set_is_loser(false);

  if (curr_move.movetype == Moves::ToFoundation) {
    --score_;
    curr_state_.UndoMoveToFoundation(curr_move, snm_opts, enum_to_empty_pile_);
  }
  else if (curr_move.movetype == Moves::Deal) {
    curr_state_.UndoDeal(snm_opts, enum_to_empty_pile_);
  }
  else if (curr_move.movetype == Moves::InTableau) {
    curr_state_.UndoTableauMove(curr_move, snm_opts, enum_to_empty_pile_);
  }

  curr_state_.UpdateCompStr(face_up_, enum_to_empty_pile_);

  if (moves_.size()) {
    curr_state_.set_curr_move(Move(moves_.top()));
  } else {
    curr_state_.set_curr_move(Move());
  }
  curr_state_.set_valid_moves(all_valid_moves_.top());
}

int Agnes::PerformMove()
{
  bool any_pile_blocked = false;
  const SetNMovableOpts snm_opts (move_same_suit_, split_runs_,
                                  split_empty_stock_);

  ++n_states_checked_;

  if (curr_state_.depth() > max_depth_) {
    max_depth_ = curr_state_.depth();
  }

  if (n_states_checked_ >= max_states_guard_) {
    return 3;
  }

  // Check the score and whether it's greater than the max score
  std::vector<Move> &valid_moves = all_valid_moves_.top();

  int n_valid_moves = valid_moves.size();

  // Lost the game!
  if (curr_state_.depth()==0 && n_valid_moves==0) {
    ++n_no_move_possible_;
    return 2;
  }
  else if (n_valid_moves==0) {
    //if (curr_state_.n_stock_left() >= track_threshold_) {
    if (curr_state_.n_stock_left() >= track_threshold_
        && !curr_state_.is_loser()) {
      losing_states_.insert(curr_state_.compstr());
    }
    ++n_no_move_possible_;
    UndoMove(false);
    return 0;
  }
  else {
    const Move curr_move = Move(valid_moves.back());
    curr_state_.set_curr_move(curr_move);
    // For last_move_info, duplicate the top element and return a reference
    all_lmi_.push(all_lmi_.top());
    array<LastMoveInfo, kNPile>& last_move_info = all_lmi_.top();

    valid_moves.pop_back();
    if (all_valid_moves_.size() < kNToTrack) {
      moves_left_[all_valid_moves_.size()] = valid_moves.size();
    }

    if (curr_move.movetype == Moves::ToFoundation) {
      ++n_move_to_foundation_;
      ++score_;
      curr_state_.MoveToFoundation(curr_move, last_move_info, snm_opts,
                                   enum_to_empty_pile_);
      if (score_ > max_score_) max_score_ = score_;
    }
    else if (curr_move.movetype == Moves::Deal) {
      ++n_deal_;
      curr_state_.DealMove(deck_, last_move_info, snm_opts, enum_to_empty_pile_);
      if (enum_to_empty_pile_ == EmptyRule::None && !maximize_score_) {
        any_pile_blocked = curr_state_.IsAnyPileBlocked();
      }
    }
    else if (curr_move.movetype == Moves::InTableau) {
      ++n_move_card_in_tableau_;
      curr_state_.TableauMove(curr_move, last_move_info, snm_opts,
                              enum_to_empty_pile_);
    }

    curr_state_.UpdateCompStr(face_up_, enum_to_empty_pile_);
    curr_state_.ClearValidMoves();

    curr_state_.set_is_loop(false);
    curr_state_.set_is_loser(false);
    if (check_for_loops_) {
      auto ret = check_loops_.insert(curr_state_.compstr());
      if (!ret.second) curr_state_.set_is_loop(true);
    }

    // Checking if already know state is losing
    if (curr_state_.is_loop()) {
      if (print_states_) {
        cout << "New state is a loop, so setting valid_moves to empty\n";
      }
    }
    else if (curr_state_.n_stock_left() >= track_threshold_
        && losing_states_.find(curr_state_.compstr()) != losing_states_.end()) {
      curr_state_.set_is_loser(true);
      curr_state_.ClearValidMoves();
      if (print_states_) {
        cout << "Already checked the new state, so setting valid_moves";
        cout << " to empty\n";
      }
    }
    else if (any_pile_blocked) {
      if (print_states_) {
        cout << "New state is a block, so setting valid_moves to empty\n";
      }
    }
    else {
      curr_state_.set_valid_moves(enum_to_empty_pile_, move_same_suit_,
                                  split_empty_stock_, track_threshold_,
                                  all_lmi_.top());
    }

    if (score_ == kNCard) {
      // Won the game
      std::vector<Move> no_moves;
      moves_.push(curr_move);
      all_valid_moves_.push(no_moves);
      return 1;
    }
    else if (curr_state_.valid_moves().size()==0) {
      all_valid_moves_.push(curr_state_.valid_moves());
      moves_.push(curr_move);
    }
    else {
      all_valid_moves_.push(curr_state_.valid_moves());
      moves_.push(curr_move);
    }
    return 0;
  }
}

void Agnes::PrintDeck() const {
  cout << "[";
  for (auto it=initial_deck_.begin(); it != initial_deck_.end(); ++it) {
    if (it!=initial_deck_.begin()) {
      cout << ", ";
    }
    cout << "(" << it->rank << ", " << it-> suit << ")";
  }
  cout << "]" << "\n";
}

void Agnes::PrintLastMoveInfo() const {
   const array<LastMoveInfo, kNPile>& last_move_info = all_lmi_.top();

   cout << "Last move info:[";
   for (auto it=last_move_info.begin(); it != last_move_info.end(); ++it) {
    if (it!=last_move_info.begin()) {
      cout << ", ";
    }
    cout << it->depth << "-" << it->n_moved << "-";
    if (it->moved_to) { cout << "T-"; }
    else { cout << "F-"; }
    if (it->can_move_to_found) { cout << "T"; }
    else { cout << "F"; }
   }
   cout << "]" << "\n";
}

} // namespace quagnes
